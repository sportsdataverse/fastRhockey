#' Calculate expected goals (xG) for play-by-play data
#'
#' @description Adds an `xg` column to a play-by-play data frame using the
#'   fastRhockey xG models (XGBoost).
#'
#'   The models were trained on NHL PBP data from 2010-2024 using the same
#'   methodology as the hockeyR xG models. Three models are used:
#'   * **5v5 model** — Even-strength situations
#'   * **Special teams model** — Power play, short-handed, and other
#'     non-5v5 situations (adds skater count features)
#'   * **Penalty shot model** — A constant xG value for penalty shot attempts
#'
#'   Models are loaded into the package namespace on package load via
#'   \code{.onLoad()} (see `zzz.R`).
#'
#' @param pbp A play-by-play data frame produced by
#'   \code{\link{nhl_game_pbp}} or \code{\link{nhl_game_feed}}.
#'   Must contain columns needed by \code{helper_nhl_prepare_xg_data()}: `event_type`,
#'   `secondary_type`, `period_type`, `period`, `game_seconds`, `x`, `y`,
#'   `x_fixed`, `event_team_abbr`, `home_abbr`, `away_abbr`, `home_skaters`,
#'   `away_skaters`, `shot_distance`, `shot_angle`, `empty_net`,
#'   `strength_state`, `event_id`, `season`.
#'
#' @return The original `pbp` data frame with an `xg` column appended.
#'   `xg` is NA for non-shot events.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   pbp <- nhl_game_pbp(2024020001, include_shifts = TRUE)
#'   pbp <- helper_nhl_calculate_xg(pbp)
#'   # View xG for shots
#'   pbp[!is.na(pbp$xg), c("event_type", "shot_distance", "shot_angle", "xg")]
#' }
helper_nhl_calculate_xg <- function(pbp) {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop(
            "Package 'xgboost' is required for xG calculations. ",
            "Install it with: install.packages('xgboost')",
            call. = FALSE
        )
    }

    # Verify models are loaded
    if (is.null(.xg_env$xg_model_5v5) || is.null(.xg_env$xg_model_st)) {
        stop(
            "xG models are not loaded. ",
            "They should load automatically on package load via .onLoad(). ",
            "Try: library(fastRhockey)",
            call. = FALSE
        )
    }

    # Prepare features from PBP
    model_data <- helper_nhl_prepare_xg_data(pbp)

    if (nrow(model_data) == 0) {
        pbp$xg <- NA_real_
        return(pbp)
    }

    # --- 5v5 predictions ---
    data_5v5 <- model_data %>%
        dplyr::filter(strength_state == "5v5")

    if (nrow(data_5v5) > 0) {
        feats_5v5 <- data_5v5 %>%
            dplyr::select(dplyr::all_of(.xg_env$xg_feature_names_5v5)) %>%
            as.matrix()
        dmat_5v5 <- xgboost::xgb.DMatrix(data = feats_5v5)
        data_5v5$xg <- stats::predict(.xg_env$xg_model_5v5, dmat_5v5)
    } else {
        data_5v5$xg <- numeric(0)
    }

    # --- Special teams predictions ---
    data_st <- model_data %>%
        dplyr::filter(strength_state != "5v5")

    if (nrow(data_st) > 0) {
        feats_st <- data_st %>%
            dplyr::select(dplyr::all_of(.xg_env$xg_feature_names_st)) %>%
            as.matrix()
        dmat_st <- xgboost::xgb.DMatrix(data = feats_st)
        data_st$xg <- stats::predict(.xg_env$xg_model_st, dmat_st)
    } else {
        data_st$xg <- numeric(0)
    }

    # Combine predictions
    xg_results <- dplyr::bind_rows(
        data_5v5 %>% dplyr::select(event_id, xg),
        data_st %>% dplyr::select(event_id, xg)
    )

    # Join xG back to original PBP
    pbp <- pbp %>%
        dplyr::left_join(xg_results, by = "event_id")

    # Override penalty shot xG with constant model value
    if ("secondary_type" %in% names(pbp)) {
        penalty_shot_mask <- !is.na(pbp$secondary_type) &
            pbp$secondary_type == "Penalty Shot"
        pbp$xg[penalty_shot_mask] <- .xg_env$xg_model_ps
    }

    # Ensure event order is preserved
    if ("event_idx" %in% names(pbp)) {
        pbp <- pbp %>% dplyr::arrange(event_idx)
    } else if ("event_id" %in% names(pbp)) {
        pbp <- pbp %>% dplyr::arrange(event_id)
    }

    return(pbp)
}


#' Prepare PBP data for xG model predictions
#'
#' @description Helper function to prepare fastRhockey PBP data for expected
#'   goals (xG) calculations.
#'
#'   Adapted from the hockeyR `helper_nhl_prepare_xg_data()` function. The feature
#'   engineering logic (lag events, era dummies, shot metrics, one-hot
#'   encoding) is identical to the hockeyR xG training pipeline so that the
#'   same xgboost models can be used for inference.
#'
#' @param x A play-by-play data frame produced by
#'   \code{\link{nhl_game_pbp}} or
#'   \code{\link{nhl_game_feed}}.
#'
#' @return A tibble with model features plus identifiers
#'   (`season`, `game_id`, `event_id`, `strength_state`).
#'
#' @details
#' The function:
#' 1. Filters out shootouts, penalty shots, and shift-change events.
#' 2. Computes lag features (last event type/team, time since last, distance
#'    from last, event zones).
#' 3. Adds era dummies for the four training eras (2011-13, 2014-18,
#'    2019-21, 2022+).
#' 4. Computes tactical booleans: `rebound`, `rush`, `cross_ice_event`.
#' 5. One-hot encodes `shot_type` and `last_event_type`.
#' 6. Pads any missing model features to zero so the xgboost DMatrix
#'    dimensions match the trained models.
#'
#' @keywords internal
helper_nhl_prepare_xg_data <- function(x) {
    # Column compatibility: nhl_game_feed uses slightly different names
    # Map them to the names the hockeyR xG model expects
    if (
        !("event_team_abbr" %in% names(x)) &&
            "event_owner_team_abbr" %in% names(x)
    ) {
        x$event_team_abbr <- x$event_owner_team_abbr
    }
    if (!("home_abbr" %in% names(x)) && "home_team_abbr" %in% names(x)) {
        x$home_abbr <- x$home_team_abbr
    }
    if (!("away_abbr" %in% names(x)) && "away_team_abbr" %in% names(x)) {
        x$away_abbr <- x$away_team_abbr
    }

    model_df <- x %>%
        # --- Normalize secondary_type across NHL API eras ---
        # Old API (2010-2022): Title Case ("Wrist Shot", "Snap Shot", etc.)
        # New API (2023+): lowercase abbreviated ("wrist", "snap", etc.)
        # Must match the canonical values used when training the xG models.
        dplyr::mutate(
            secondary_type = dplyr::case_when(
                is.na(secondary_type) ~ NA_character_,
                tolower(secondary_type) == "wrist" ~ "Wrist Shot",
                tolower(secondary_type) == "wrist shot" ~ "Wrist Shot",
                tolower(secondary_type) == "snap" ~ "Snap Shot",
                tolower(secondary_type) == "snap shot" ~ "Snap Shot",
                tolower(secondary_type) == "slap" ~ "Slap Shot",
                tolower(secondary_type) == "slap shot" ~ "Slap Shot",
                tolower(secondary_type) == "backhand" ~ "Backhand",
                tolower(secondary_type) == "deflected" ~ "Deflected",
                tolower(secondary_type) == "tip-in" ~ "Tip-In",
                tolower(secondary_type) == "wrap-around" ~ "Wrap-around",
                tolower(secondary_type) == "bat" ~ "Batted",
                tolower(secondary_type) == "batted" ~ "Batted",
                tolower(secondary_type) == "poke" ~ "Poke",
                tolower(secondary_type) == "between-legs" ~ "Between Legs",
                tolower(secondary_type) == "between legs" ~ "Between Legs",
                tolower(secondary_type) == "cradle" ~ "Cradle",
                tolower(secondary_type) == "penalty shot" ~ "Penalty Shot",
                TRUE ~ secondary_type
            )
        ) %>%
        # filter out shootouts
        dplyr::filter(period_type != "SHOOTOUT") %>%
        # remove penalty shots
        dplyr::filter(
            secondary_type != "Penalty Shot" | is.na(secondary_type)
        ) %>%
        # remove shift change events (excluded from model)
        dplyr::filter(event_type != "CHANGE") %>%
        # add lag feature variables
        dplyr::group_by(game_id, period) %>%
        dplyr::mutate(
            last_event_type = dplyr::lag(event_type),
            last_event_team = dplyr::lag(event_team_abbr),
            time_since_last = game_seconds - dplyr::lag(game_seconds),
            last_x = dplyr::lag(x),
            last_y = dplyr::lag(y),
            distance_from_last = round(
                sqrt(((y - last_y)^2) + ((x - last_x)^2)),
                1
            ),
            event_zone = dplyr::case_when(
                x >= -25 & x <= 25 ~ "NZ",
                (x_fixed < -25 & event_team_abbr == home_abbr) |
                    (x_fixed > 25 & event_team_abbr == away_abbr) ~ "DZ",
                (x_fixed > 25 & event_team_abbr == home_abbr) |
                    (x_fixed < -25 & event_team_abbr == away_abbr) ~ "OZ"
            ),
            last_event_zone = dplyr::lag(event_zone)
        ) %>%
        dplyr::ungroup() %>%
        # keep only unblocked shots (SHOT, MISSED_SHOT, GOAL)
        dplyr::filter(event_type %in% c("SHOT", "MISSED_SHOT", "GOAL")) %>%
        # filter to valid last-event types (removes oddball events)
        dplyr::filter(
            last_event_type %in%
                c(
                    "FACEOFF",
                    "GIVEAWAY",
                    "TAKEAWAY",
                    "BLOCKED_SHOT",
                    "HIT",
                    "MISSED_SHOT",
                    "SHOT",
                    "STOP",
                    "PENALTY",
                    "GOAL"
                )
        ) %>%
        # era dummies (must match training eras exactly)
        dplyr::mutate(
            era_2011_2013 = ifelse(
                season %in% c("20102011", "20112012", "20122013"),
                1,
                0
            ),
            era_2014_2018 = ifelse(
                season %in%
                    c(
                        "20132014",
                        "20142015",
                        "20152016",
                        "20162017",
                        "20172018"
                    ),
                1,
                0
            ),
            era_2019_2021 = ifelse(
                season %in% c("20182019", "20192020", "20202021"),
                1,
                0
            ),
            era_2022_2024 = ifelse(
                season %in% c("20212022", "20222023", "20232024"),
                1,
                0
            ),
            era_2025_on = ifelse(as.numeric(season) > 20232024, 1, 0),
            # ST model features
            event_team_skaters = ifelse(
                event_team_abbr == home_abbr,
                home_skaters,
                away_skaters
            ),
            opponent_team_skaters = ifelse(
                event_team_abbr == home_abbr,
                away_skaters,
                home_skaters
            ),
            total_skaters_on = event_team_skaters + opponent_team_skaters,
            event_team_advantage = event_team_skaters - opponent_team_skaters,
            # 5v5 model features
            rebound = ifelse(
                last_event_type %in%
                    c("SHOT", "MISSED_SHOT", "GOAL") &
                    time_since_last <= 2,
                1,
                0
            ),
            rush = ifelse(
                last_event_zone %in% c("NZ", "DZ") & time_since_last <= 4,
                1,
                0
            ),
            cross_ice_event = ifelse(
                last_event_zone == "OZ" &
                    ((last_y > 3 & y < -3) | (last_y < -3 & y > 3)) &
                    time_since_last <= 2,
                1,
                0
            ),
            # fix missing empty net vars
            empty_net = ifelse(
                is.na(empty_net) | empty_net == FALSE,
                FALSE,
                TRUE
            ),
            shot_type = secondary_type,
            goal = ifelse(event_type == "GOAL", 1, 0)
        ) %>%
        dplyr::select(
            season,
            game_id,
            event_id,
            strength_state,
            shot_distance,
            shot_angle,
            empty_net,
            last_event_type,
            last_event_team,
            time_since_last,
            last_x,
            last_y,
            distance_from_last,
            event_zone,
            last_event_zone,
            era_2011_2013,
            era_2014_2018,
            era_2019_2021,
            era_2022_2024,
            era_2025_on,
            event_team_skaters,
            opponent_team_skaters,
            total_skaters_on,
            event_team_advantage,
            rebound,
            rush,
            cross_ice_event,
            shot_type,
            goal
        ) %>%
        # one-hot encode categorical variables
        dplyr::mutate(type_value = 1L, last_value = 1L) %>%
        tidyr::pivot_wider(
            names_from = shot_type,
            values_from = type_value,
            values_fill = 0L,
            values_fn = max
        ) %>%
        tidyr::pivot_wider(
            names_from = last_event_type,
            values_from = last_value,
            values_fill = 0L,
            values_fn = max,
            names_prefix = "last_"
        ) %>%
        janitor::clean_names() %>%
        dplyr::select(
            -last_event_team,
            -event_zone,
            -last_event_zone,
            -event_team_skaters,
            -opponent_team_skaters
        )

    # Drop NA column created from missing shot_type values
    if ("na" %in% names(model_df)) {
        model_df <- dplyr::select(model_df, -na)
    }

    # Pad any missing features to zero (ensures DMatrix dimensions match)
    missing_feats <- dplyr::tibble(
        feature = .xg_env$xg_feature_names_5v5
    ) %>%
        dplyr::filter(!(feature %in% names(model_df))) %>%
        dplyr::mutate(val = 0L) %>%
        tidyr::pivot_wider(names_from = feature, values_from = val)

    if (length(missing_feats) > 0L) {
        model_df <- dplyr::bind_cols(model_df, missing_feats)
    }

    return(model_df)
}
