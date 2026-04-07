#' @title **NHL Game Play-by-Play**
#' @description Returns detailed play-by-play data for a given NHL game,
#' including event players, coordinates, on-ice skaters, strength states,
#' shot distance/angle, and shift integration.
#' Uses the new NHL API at api-web.nhle.com.
#'
#' @param game_id Game unique ID (e.g. 2024020001)
#' @param include_shifts Logical; whether to integrate shift data for on-ice
#' player tracking. Default TRUE.
#' @param raw Logical; if TRUE, return the unprocessed API response as a list
#' instead of the parsed data frame. Default FALSE.
#' @return A data frame with one row per event (default), or the raw API
#' response list when \code{raw = TRUE}.
#' @keywords NHL Game PBP Play-by-Play
#' @import rvest
#' @importFrom rlang .data
#' @importFrom lubridate ms period_to_seconds
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows arrange
#' left_join group_by ungroup summarize case_when coalesce lag lead row_number
#' across all_of
#' @importFrom tidyr unnest_wider fill pivot_wider
#' @importFrom stringr str_detect str_pad
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_pbp(game_id = 2024020001))
#'   try(nhl_game_pbp(game_id = 2024020001, raw = TRUE))
#' }
nhl_game_pbp <- function(game_id, include_shifts = TRUE, raw = FALSE) {
    if (raw) {
        return(.get_raw_pbp(game_id))
    }
    .get_pbp_data(game_id = game_id, include_shifts = include_shifts)
}

#' @title **NHL Game Feed**
#' @description Returns a named list with play-by-play data plus game metadata
#' for a given NHL game. Uses the new NHL API at api-web.nhle.com.
#'
#' @param game_id Game unique ID (e.g. 2024020001)
#' @param include_shifts Logical; whether to integrate shift data for on-ice
#' player tracking. Default TRUE.
#' @param raw Logical; if TRUE, return the unprocessed API response as a list
#' instead of parsed/enriched data. Default FALSE.
#' @return A named list with elements: pbp (play-by-play data frame),
#' game_info (game metadata), rosters (player roster data frame).
#' When \code{raw = TRUE}, returns the raw JSON response as a nested list.
#' @keywords NHL Game Feed
#' @export
#' @examples
#' \donttest{
#'   try(nhl_game_feed(game_id = 2024020001))
#'   try(nhl_game_feed(game_id = 2024020001, raw = TRUE))
#' }
nhl_game_feed <- function(game_id, include_shifts = TRUE, raw = FALSE) {
    # Fetch raw API data
    pbp_url <- paste0(
        "https://api-web.nhle.com/v1/gamecenter/",
        game_id,
        "/play-by-play"
    )
    res <- httr::RETRY("GET", pbp_url)
    check_status(res)
    resp <- httr::content(res, as = "text", encoding = "UTF-8")
    raw_data <- jsonlite::fromJSON(
        resp, simplifyVector = TRUE, flatten = TRUE
    )

    if (raw) {
        return(raw_data)
    }

    # Extract game info
    game_info <- dplyr::tibble(
        game_id = as.integer(game_id),
        season = raw_data$season %||% NA_integer_,
        game_type = .game_type_label(raw_data$gameType %||% NA_integer_),
        game_date = raw_data$gameDate %||% NA_character_,
        venue = raw_data$venue$default %||% NA_character_,
        home_team_abbr = raw_data$homeTeam$abbrev %||% NA_character_,
        away_team_abbr = raw_data$awayTeam$abbrev %||% NA_character_,
        home_score = raw_data$homeTeam$score %||% NA_integer_,
        away_score = raw_data$awayTeam$score %||% NA_integer_,
        game_state = raw_data$gameState %||% NA_character_
    )

    # Extract rosters
    rosters <- .parse_game_rosters(raw_data)

    # Build PBP
    pbp <- .build_pbp(raw_data, rosters, game_id, include_shifts)

    result <- list(
        pbp = pbp,
        game_info = game_info,
        rosters = rosters
    )

    return(result)
}


#' Fetch raw play-by-play API response without processing
#' @keywords internal
.get_raw_pbp <- function(game_id) {
    pbp_url <- paste0(
        "https://api-web.nhle.com/v1/gamecenter/",
        game_id,
        "/play-by-play"
    )
    res <- httr::RETRY("GET", pbp_url)
    check_status(res)
    resp <- httr::content(res, as = "text", encoding = "UTF-8")
    jsonlite::fromJSON(resp, simplifyVector = TRUE, flatten = TRUE)
}


# ---- Internal helpers ----

#' Last-observation-carried-forward fill (base R, no zoo dependency)
#' @param x A vector (numeric, integer, or character).
#' @return The vector with NAs replaced by the most recent non-NA value.
#' @keywords internal
.nafill_locf <- function(x) {
    if (length(x) == 0L) {
        return(x)
    }
    non_na <- !is.na(x)
    idx <- cummax(seq_along(x) * non_na)
    idx[idx == 0L] <- NA_integer_
    x[idx]
}

#' Build the full PBP data from raw API response
#' @keywords internal
.build_pbp <- function(raw, rosters, game_id, include_shifts) {
    plays <- raw$plays
    if (is.null(plays) || length(plays) == 0) {
        message(glue::glue(
            "{Sys.time()}: No play data available for game {game_id}"
        ))
        return(dplyr::tibble())
    }

    # Core game metadata
    season <- as.character(raw$season %||% NA)
    home_abbr <- raw$homeTeam$abbrev %||% NA_character_
    away_abbr <- raw$awayTeam$abbrev %||% NA_character_
    home_id <- raw$homeTeam$id %||% NA_integer_
    away_id <- raw$awayTeam$id %||% NA_integer_

    # Determine season type from game_id
    game_id_str <- as.character(game_id)
    season_type_code <- as.integer(substr(game_id_str, 5, 6))
    season_type <- dplyr::case_when(
        season_type_code == 1 ~ "PR",
        season_type_code == 2 ~ "R",
        season_type_code == 3 ~ "P",
        season_type_code == 4 ~ "A",
        TRUE ~ "R"
    )

    # ----- Parse plays into a data frame -----
    pbp <- .parse_plays(
        plays,
        home_abbr,
        away_abbr,
        home_id,
        away_id,
        game_id,
        season,
        season_type
    )

    if (nrow(pbp) == 0) {
        return(dplyr::tibble())
    }

    # ----- Coordinate fixing using homeTeamDefendingSide -----
    pbp <- .fix_coordinates(pbp, raw, home_abbr, away_abbr)

    # ----- Shot distance and angle -----
    pbp <- .add_shot_metrics(pbp, home_abbr)

    # ----- Shift integration -----
    if (include_shifts && nrow(pbp) > 0) {
        pbp <- tryCatch(
            .integrate_shifts(
                pbp,
                rosters,
                game_id,
                home_abbr,
                away_abbr,
                home_id,
                away_id,
                season_type
            ),
            error = function(e) {
                message(glue::glue(
                    "{Sys.time()}: Could not integrate shifts for game {game_id}: {e$message}"
                ))
                pbp
            }
        )
    }

    # ----- Score fill for CHANGE events -----
    if ("home_score" %in% names(pbp) && nrow(pbp) > 0) {
        pbp$home_score[1] <- ifelse(
            is.na(pbp$home_score[1]),
            0L,
            pbp$home_score[1]
        )
        pbp$away_score[1] <- ifelse(
            is.na(pbp$away_score[1]),
            0L,
            pbp$away_score[1]
        )
        pbp$home_score <- .nafill_locf(pbp$home_score)
        pbp$away_score <- .nafill_locf(pbp$away_score)
    }

    # ----- Period type -----
    pbp <- pbp %>%
        dplyr::mutate(
            period_type = dplyr::case_when(
                period < 4 ~ "REGULAR",
                season_type %in% c("R", "PR") & period == 4 ~ "OVERTIME",
                season_type %in% c("R", "PR") & period == 5 ~ "SHOOTOUT",
                season_type == "P" & period > 3 ~ "OVERTIME",
                TRUE ~ "REGULAR"
            )
        )

    # ----- Join event player names (must happen before descriptions) -----
    pbp <- .join_event_player_names(pbp, rosters)

    # ----- Event index and description -----
    pbp <- .add_descriptions(pbp, home_abbr, away_abbr)

    # Add season and team columns needed by xG and downstream analysis
    pbp$season <- season
    pbp$season_type <- season_type
    pbp$home_abbr <- home_abbr
    pbp$away_abbr <- away_abbr

    # Cleanup and final column order
    pbp <- .finalize_columns(pbp)

    pbp <- make_fastRhockey_data(
        pbp,
        "NHL Game PBP from NHL.com",
        Sys.time()
    )

    return(pbp)
}


#' Parse plays array from API response into a data frame
#' @keywords internal
.parse_plays <- function(
    plays,
    home_abbr,
    away_abbr,
    home_id,
    away_id,
    game_id,
    season,
    season_type
) {
    # plays is a data frame from jsonlite (flattened)
    pbp <- dplyr::as_tibble(plays)

    if (nrow(pbp) == 0) {
        return(dplyr::tibble())
    }

    # Normalize event type: typeDescKey → uppercase with underscores
    pbp <- pbp %>%
        dplyr::mutate(
            event_type = toupper(gsub("-", "_", typeDescKey)),
            event_type = dplyr::case_when(
                event_type == "SHOT_ON_GOAL" ~ "SHOT",
                event_type == "STOPPAGE" ~ "STOP",
                TRUE ~ event_type
            ),
            event = dplyr::case_when(
                event_type == "SHOT" ~ "Shot",
                event_type == "GOAL" ~ "Goal",
                event_type == "MISSED_SHOT" ~ "Missed Shot",
                event_type == "BLOCKED_SHOT" ~ "Blocked Shot",
                event_type == "HIT" ~ "Hit",
                event_type == "FACEOFF" ~ "Faceoff",
                event_type == "GIVEAWAY" ~ "Giveaway",
                event_type == "TAKEAWAY" ~ "Takeaway",
                event_type == "PENALTY" ~ "Penalty",
                event_type == "STOP" ~ "Stoppage",
                event_type == "PERIOD_START" ~ "Period Start",
                event_type == "PERIOD_END" ~ "Period End",
                event_type == "GAME_END" ~ "Game End",
                event_type == "CHANGE" ~ "Change",
                event_type == "DELAYED_PENALTY" ~ "Delayed Penalty",
                event_type == "GAME_SCHEDULED" ~ "Game Scheduled",
                TRUE ~ event_type
            )
        )

    # ----- Extract time fields -----
    # timeInPeriod is like "12:34", timeRemaining similar
    pbp <- pbp %>%
        dplyr::mutate(
            period = as.integer(periodDescriptor.number),
            period_time = timeInPeriod %||% NA_character_,
            period_time_remaining = timeRemaining %||% NA_character_
        )

    # Parse period_time to seconds
    pbp <- pbp %>%
        dplyr::mutate(
            period_seconds = .time_to_seconds(period_time),
            period_seconds_remaining = .time_to_seconds(period_time_remaining),
            game_seconds = period_seconds + (1200L * (period - 1L)),
            game_seconds_remaining = dplyr::case_when(
                period <= 3 ~ (3L - period) * 1200L + period_seconds_remaining,
                TRUE ~ period_seconds_remaining
            )
        )

    # ----- Extract coordinates -----
    if ("details.xCoord" %in% names(pbp)) {
        pbp$x <- as.numeric(pbp$`details.xCoord`)
        pbp$y <- as.numeric(pbp$`details.yCoord`)
    } else {
        pbp$x <- NA_real_
        pbp$y <- NA_real_
    }

    # ----- Score columns -----
    if ("homeTeamDefendingSide" %in% names(pbp)) {
        # Present in newer API responses
    }

    # Extract scores - handle nested or flat formats
    pbp <- .extract_scores(pbp)

    # ----- Event owner team -----
    if ("details.eventOwnerTeamId" %in% names(pbp)) {
        pbp$event_owner_team_id <- as.integer(pbp$`details.eventOwnerTeamId`)
    } else {
        pbp$event_owner_team_id <- NA_integer_
    }

    # Map event owner to abbreviations
    pbp <- pbp %>%
        dplyr::mutate(
            event_team_abbr = dplyr::case_when(
                event_owner_team_id == home_id ~ home_abbr,
                event_owner_team_id == away_id ~ away_abbr,
                TRUE ~ NA_character_
            ),
            event_team_type = dplyr::case_when(
                event_team_abbr == home_abbr ~ "home",
                event_team_abbr == away_abbr ~ "away",
                TRUE ~ NA_character_
            )
        )

    # ----- Swap blocked shot team (blocker's team, not shooter's) -----
    pbp <- pbp %>%
        dplyr::mutate(
            event_team_abbr = dplyr::case_when(
                event_type == "BLOCKED_SHOT" &
                    event_team_abbr == home_abbr ~ away_abbr,
                event_type == "BLOCKED_SHOT" &
                    event_team_abbr == away_abbr ~ home_abbr,
                TRUE ~ event_team_abbr
            ),
            event_owner_team_id = dplyr::case_when(
                event_type == "BLOCKED_SHOT" &
                    event_owner_team_id == home_id ~ away_id,
                event_type == "BLOCKED_SHOT" &
                    event_owner_team_id == away_id ~ home_id,
                TRUE ~ event_owner_team_id
            ),
            event_team_type = dplyr::case_when(
                event_team_abbr == home_abbr ~ "home",
                event_team_abbr == away_abbr ~ "away",
                TRUE ~ NA_character_
            )
        )

    # ----- Event players via coalesce -----
    pbp <- .extract_event_players(pbp)

    # ----- Situation code parsing -----
    pbp <- .parse_situation_code(pbp)

    # ----- Penalty info -----
    pbp <- .extract_penalty_info(pbp)

    # ----- Secondary type (shot type, penalty type) -----
    pbp <- .extract_secondary_type(pbp)

    # ----- Empty net / extra attacker -----
    pbp <- .extract_empty_net(pbp)

    # Game ID

    pbp$game_id <- as.integer(game_id)

    return(pbp)
}


#' Extract scores from plays data
#' @keywords internal
.extract_scores <- function(pbp) {
    # The new API has scores in details.homeScore / details.awayScore for GOAL events
    # and possibly in other fields
    if ("details.homeScore" %in% names(pbp)) {
        pbp$home_score <- as.integer(pbp$`details.homeScore`)
        pbp$away_score <- as.integer(pbp$`details.awayScore`)
    } else {
        pbp$home_score <- NA_integer_
        pbp$away_score <- NA_integer_
    }
    return(pbp)
}


#' Extract event players using coalesce pattern (adapted from hockeyR)
#' @keywords internal
.extract_event_players <- function(pbp) {
    # Player ID columns from the new API (details.*)
    p1_cols <- c(
        "details.scoringPlayerId",
        "details.shootingPlayerId",
        "details.hittingPlayerId",
        "details.winningPlayerId",
        "details.committedByPlayerId",
        "details.playerId"
    )

    p2_cols <- c(
        "details.assist1PlayerId",
        "details.blockingPlayerId",
        "details.goalieInNetId",
        "details.hitteePlayerId",
        "details.losingPlayerId",
        "details.drawnByPlayerId"
    )

    p3_cols <- c(
        "details.assist2PlayerId",
        "details.servedByPlayerId"
    )

    # Pad missing columns with NA
    for (col in c(p1_cols, p2_cols, p3_cols)) {
        if (!col %in% names(pbp)) {
            pbp[[col]] <- NA_integer_
        }
    }

    # Coalesce player IDs
    pbp$event_player_1_id <- .coalesce_int(pbp, p1_cols)
    pbp$event_player_2_id <- .coalesce_int(pbp, p2_cols)
    pbp$event_player_3_id <- .coalesce_int(pbp, p3_cols)

    # Player 4 (second assist for goals) — same as assist2 when goalie is P4
    if ("details.goalieInNetId" %in% names(pbp)) {
        pbp$event_player_4_id <- as.integer(pbp$`details.goalieInNetId`)
    } else {
        pbp$event_player_4_id <- NA_integer_
    }

    # Event player types
    pbp <- pbp %>%
        dplyr::mutate(
            event_player_1_type = dplyr::case_when(
                event_type == "GOAL" ~ "Scorer",
                event_type %in% c("SHOT", "MISSED_SHOT") ~ "Shooter",
                event_type == "HIT" ~ "Hitter",
                event_type == "FACEOFF" ~ "Winner",
                event_type == "PENALTY" ~ "PenaltyOn",
                event_type == "BLOCKED_SHOT" ~ "Shooter",
                event_type %in% c("GIVEAWAY", "TAKEAWAY") ~ "PlayerID",
                TRUE ~ NA_character_
            ),
            event_player_2_type = dplyr::case_when(
                event_type == "GOAL" ~ "Assist",
                event_type %in% c("SHOT", "MISSED_SHOT") ~ "Goalie",
                event_type == "HIT" ~ "Hittee",
                event_type == "FACEOFF" ~ "Loser",
                event_type == "PENALTY" ~ "DrewBy",
                event_type == "BLOCKED_SHOT" ~ "Blocker",
                TRUE ~ NA_character_
            ),
            event_player_3_type = dplyr::case_when(
                event_type == "GOAL" & !is.na(event_player_3_id) ~ "Assist",
                event_type == "PENALTY" &
                    !is.na(event_player_3_id) ~ "ServedBy",
                TRUE ~ NA_character_
            ),
            event_player_4_type = dplyr::case_when(
                event_type == "GOAL" & !is.na(event_player_4_id) ~ "Goalie",
                TRUE ~ NA_character_
            )
        )

    # Determine event goalie ID
    pbp <- pbp %>%
        dplyr::mutate(
            event_goalie_id = dplyr::case_when(
                event_player_2_type == "Goalie" ~ event_player_2_id,
                event_player_3_type == "Goalie" ~ event_player_3_id,
                event_player_4_type == "Goalie" ~ event_player_4_id,
                TRUE ~ NA_integer_
            )
        )

    return(pbp)
}


#' Parse situationCode into skater counts
#' @keywords internal
.parse_situation_code <- function(pbp) {
    if ("situationCode" %in% names(pbp)) {
        sc <- as.character(pbp$situationCode)
        pbp <- pbp %>%
            dplyr::mutate(
                away_goalie_in = as.integer(substr(sc, 1, 1)),
                away_skaters = as.integer(substr(sc, 2, 2)),
                home_skaters = as.integer(substr(sc, 3, 3)),
                home_goalie_in = as.integer(substr(sc, 4, 4))
            )
    } else {
        pbp$away_goalie_in <- NA_integer_
        pbp$away_skaters <- NA_integer_
        pbp$home_skaters <- NA_integer_
        pbp$home_goalie_in <- NA_integer_
    }
    return(pbp)
}


#' Extract penalty info
#' @keywords internal
.extract_penalty_info <- function(pbp) {
    if ("details.typeCode" %in% names(pbp)) {
        pbp$typeCode <- pbp$`details.typeCode`
    } else {
        pbp$typeCode <- NA_character_
    }

    if ("details.duration" %in% names(pbp)) {
        pbp$penalty_minutes <- as.integer(pbp$`details.duration`)
    } else {
        pbp$penalty_minutes <- NA_integer_
    }

    pbp <- pbp %>%
        dplyr::mutate(
            penalty_severity = dplyr::case_when(
                typeCode %in% c("MIN", "BEN") ~ "Minor",
                typeCode == "MAJ" ~ "Major",
                typeCode == "MIS" ~ "Misconduct",
                typeCode == "MAT" ~ "Match",
                typeCode == "GM" ~ "Game Misconduct",
                TRUE ~ NA_character_
            )
        )

    return(pbp)
}


#' Extract secondary type (shot type / penalty type)
#' @keywords internal
.extract_secondary_type <- function(pbp) {
    if ("details.shotType" %in% names(pbp)) {
        pbp$secondary_type <- pbp$`details.shotType`
    } else {
        pbp$secondary_type <- NA_character_
    }

    # For penalties, the descKey has the penalty type
    if ("details.descKey" %in% names(pbp)) {
        pbp <- pbp %>%
            dplyr::mutate(
                secondary_type = dplyr::case_when(
                    event_type == "PENALTY" ~ details.descKey,
                    TRUE ~ secondary_type
                )
            )
    }

    # Reason for stoppages
    if ("details.reason" %in% names(pbp)) {
        pbp$reason <- pbp$`details.reason`
    } else {
        pbp$reason <- NA_character_
    }

    if ("details.secondaryReason" %in% names(pbp)) {
        pbp$secondaryReason <- pbp$`details.secondaryReason`
    } else {
        pbp$secondaryReason <- NA_character_
    }

    return(pbp)
}


#' Extract empty net info from situation code
#' @keywords internal
.extract_empty_net <- function(pbp) {
    pbp <- pbp %>%
        dplyr::mutate(
            empty_net = dplyr::case_when(
                event_type %in%
                    c("GOAL", "SHOT", "MISSED_SHOT") &
                    event_team_type == "home" &
                    away_goalie_in == 0 ~ TRUE,
                event_type %in%
                    c("GOAL", "SHOT", "MISSED_SHOT") &
                    event_team_type == "away" &
                    home_goalie_in == 0 ~ TRUE,
                TRUE ~ FALSE
            ),
            extra_attacker = dplyr::case_when(
                event_team_type == "home" & home_goalie_in == 0 ~ TRUE,
                event_team_type == "away" & away_goalie_in == 0 ~ TRUE,
                TRUE ~ FALSE
            )
        )
    return(pbp)
}


#' Fix coordinates so home team always shoots right
#' @keywords internal
.fix_coordinates <- function(pbp, raw, home_abbr, away_abbr) {
    # Use homeTeamDefendingSide from API if available
    if ("homeTeamDefendingSide" %in% names(pbp)) {
        pbp <- pbp %>%
            dplyr::mutate(
                x_fixed = dplyr::case_when(
                    is.na(x) ~ NA_real_,
                    homeTeamDefendingSide == "left" &
                        event_team_abbr == home_abbr ~ abs(x),
                    homeTeamDefendingSide == "left" &
                        event_team_abbr == away_abbr ~ -abs(x),
                    homeTeamDefendingSide == "right" &
                        event_team_abbr == home_abbr ~ abs(x),
                    homeTeamDefendingSide == "right" &
                        event_team_abbr == away_abbr ~ -abs(x),
                    TRUE ~ x
                ),
                y_fixed = dplyr::case_when(
                    is.na(y) ~ NA_real_,
                    homeTeamDefendingSide == "left" &
                        event_team_abbr == home_abbr &
                        x < 0 ~ -y,
                    homeTeamDefendingSide == "left" &
                        event_team_abbr == away_abbr &
                        x > 0 ~ -y,
                    homeTeamDefendingSide == "right" &
                        event_team_abbr == home_abbr &
                        x > 0 ~ -y,
                    homeTeamDefendingSide == "right" &
                        event_team_abbr == away_abbr &
                        x < 0 ~ -y,
                    TRUE ~ y
                )
            )
    } else {
        # Fallback: use median-based approach
        fenwick_events <- c("SHOT", "MISSED_SHOT", "GOAL")
        pbp <- pbp %>%
            dplyr::group_by(event_team_abbr, period, game_id) %>%
            dplyr::mutate(
                med_x = stats::median(
                    x[event_type %in% fenwick_events],
                    na.rm = TRUE
                )
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                x_fixed = dplyr::case_when(
                    event_team_abbr == home_abbr & med_x > 0 ~ x,
                    event_team_abbr == home_abbr & med_x < 0 ~ 0 - x,
                    event_team_abbr == away_abbr & med_x > 0 ~ 0 - x,
                    event_team_abbr == away_abbr & med_x < 0 ~ x,
                    TRUE ~ x
                ),
                y_fixed = dplyr::case_when(
                    event_team_abbr == home_abbr & med_x > 0 ~ y,
                    event_team_abbr == home_abbr & med_x < 0 ~ 0 - y,
                    event_team_abbr == away_abbr & med_x > 0 ~ 0 - y,
                    event_team_abbr == away_abbr & med_x < 0 ~ y,
                    TRUE ~ y
                )
            ) %>%
            dplyr::select(-med_x)
    }

    return(pbp)
}


#' Add shot distance and angle
#' @keywords internal
.add_shot_metrics <- function(pbp, home_abbr) {
    fenwick_events <- c("SHOT", "MISSED_SHOT", "GOAL", "BLOCKED_SHOT")

    pbp <- pbp %>%
        dplyr::mutate(
            shot_distance = dplyr::case_when(
                event_type %in% fenwick_events & event_team_abbr == home_abbr ~
                    round(abs(sqrt((x_fixed - 89)^2 + (y_fixed)^2)), 1),
                event_type %in% fenwick_events & event_team_abbr != home_abbr ~
                    round(abs(sqrt((x_fixed - (-89))^2 + (y_fixed)^2)), 1),
                TRUE ~ NA_real_
            ),
            shot_angle = dplyr::case_when(
                event_type %in% fenwick_events & event_team_abbr == home_abbr ~
                    round(
                        abs(atan((0 - y_fixed) / (89 - x_fixed)) * (180 / pi)),
                        1
                    ),
                event_type %in% fenwick_events & event_team_abbr != home_abbr ~
                    round(
                        abs(atan((0 - y_fixed) / (-89 - x_fixed)) * (180 / pi)),
                        1
                    ),
                TRUE ~ NA_real_
            ),
            # Fix behind-the-net angles
            shot_angle = dplyr::case_when(
                event_type %in%
                    fenwick_events &
                    event_team_abbr == home_abbr &
                    x_fixed > 89 ~ 180 - shot_angle,
                event_type %in%
                    fenwick_events &
                    event_team_abbr != home_abbr &
                    x_fixed < -89 ~ 180 - shot_angle,
                TRUE ~ shot_angle
            )
        )

    return(pbp)
}


#' Integrate shift data into PBP for on-ice player tracking
#' @keywords internal
.integrate_shifts <- function(
    pbp,
    rosters,
    game_id,
    home_abbr,
    away_abbr,
    home_id,
    away_id,
    season_type
) {
    # Fetch shifts via the still-working shifts API
    shifts_raw <- tryCatch(
        nhl_game_shifts(game_id = game_id),
        error = function(e) NULL
    )

    if (is.null(shifts_raw) || nrow(shifts_raw) == 0) {
        return(pbp)
    }

    # Convert shifts into CHANGE events with players_on / players_off / ids_on / ids_off
    # The nhl_game_shifts function already returns this format
    shifts <- shifts_raw

    # Ensure required columns
    needed_shift_cols <- c(
        "event_type",
        "event",
        "period",
        "period_time",
        "period_seconds",
        "game_seconds",
        "event_team",
        "num_on",
        "players_on",
        "ids_on",
        "num_off",
        "players_off",
        "ids_off"
    )

    missing_cols <- setdiff(needed_shift_cols, names(shifts))
    if (length(missing_cols) > 0) {
        return(pbp)
    }

    # Pad PBP with shift-specific columns if missing
    if (!"num_on" %in% names(pbp)) {
        pbp$num_on <- NA_integer_
    }
    if (!"players_on" %in% names(pbp)) {
        pbp$players_on <- NA_character_
    }
    if (!"ids_on" %in% names(pbp)) {
        pbp$ids_on <- NA_character_
    }
    if (!"num_off" %in% names(pbp)) {
        pbp$num_off <- NA_integer_
    }
    if (!"players_off" %in% names(pbp)) {
        pbp$players_off <- NA_character_
    }
    if (!"ids_off" %in% names(pbp)) {
        pbp$ids_off <- NA_character_
    }

    # Pad shifts with PBP-specific columns if missing
    pbp_only_cols <- setdiff(names(pbp), names(shifts))
    for (col in pbp_only_cols) {
        shifts[[col]] <- NA
    }

    # Ensure matching columns
    shifts <- shifts[, names(pbp), drop = FALSE]

    # Bind PBP + shifts and sort by time
    pbp_full <- dplyr::bind_rows(pbp, shifts)

    # Priority sorting (adapted from hockeyR)
    pbp_full <- pbp_full %>%
        dplyr::mutate(
            priority = dplyr::case_when(
                event_type %in%
                    c(
                        "SHOT",
                        "MISSED_SHOT",
                        "BLOCKED_SHOT",
                        "HIT",
                        "GIVEAWAY",
                        "TAKEAWAY"
                    ) ~ 1L,
                event_type == "GOAL" ~ 2L,
                event_type == "STOP" ~ 3L,
                event_type == "PENALTY" ~ 4L,
                event_type == "CHANGE" ~ 5L,
                event_type == "PERIOD_END" ~ 6L,
                event_type == "GAME_END" ~ 7L,
                event_type == "FACEOFF" ~ 8L,
                TRUE ~ 9L
            )
        ) %>%
        dplyr::arrange(period, game_seconds, priority)

    # ----- Build on-ice player matrix -----
    pbp_full <- .build_onice_matrix(pbp_full, rosters, home_abbr, away_abbr)

    # ----- Strength states -----
    pbp_full <- .add_strength_states(pbp_full, home_abbr, away_abbr)

    # Remove priority column
    pbp_full <- pbp_full %>% dplyr::select(-priority)

    return(pbp_full)
}


#' Build on-ice player matrix using cumsum approach
#' @keywords internal
.build_onice_matrix <- function(pbp, rosters, home_abbr, away_abbr) {
    # Identify home and away players from shifts
    home_roster <- rosters %>%
        dplyr::filter(team_abbr == home_abbr)
    away_roster <- rosters %>%
        dplyr::filter(team_abbr == away_abbr)

    # For each player, detect on/off via cumsum of str_detect in ids_on/ids_off
    home_players <- unique(home_roster$player_id)
    away_players <- unique(away_roster$player_id)

    # Initialize empty matrix
    n <- nrow(pbp)

    # Ensure ids_on and ids_off are character
    pbp$ids_on <- as.character(pbp$ids_on)
    pbp$ids_off <- as.character(pbp$ids_off)
    pbp$ids_on[is.na(pbp$ids_on)] <- ""
    pbp$ids_off[is.na(pbp$ids_off)] <- ""

    # Helper to compute on-ice status for a set of players
    .compute_onice <- function(player_ids, ids_on_vec, ids_off_vec) {
        onice <- matrix(
            0L,
            nrow = length(ids_on_vec),
            ncol = length(player_ids)
        )
        colnames(onice) <- as.character(player_ids)
        for (i in seq_along(player_ids)) {
            pid <- as.character(player_ids[i])
            on_detected <- as.integer(grepl(pid, ids_on_vec, fixed = TRUE))
            off_detected <- as.integer(grepl(pid, ids_off_vec, fixed = TRUE))
            onice[, i] <- cumsum(on_detected) - cumsum(off_detected)
        }
        # Clamp to 0/1
        onice[onice < 0] <- 0L
        onice[onice > 1] <- 1L
        return(onice)
    }

    home_onice <- .compute_onice(home_players, pbp$ids_on, pbp$ids_off)
    away_onice <- .compute_onice(away_players, pbp$ids_on, pbp$ids_off)

    # Identify goalies from roster
    home_goalies <- home_roster$player_id[home_roster$position_code == "G"]
    away_goalies <- away_roster$player_id[away_roster$position_code == "G"]

    home_goalie_cols <- as.character(home_goalies)
    away_goalie_cols <- as.character(away_goalies)

    # Extract on-ice player ids per row (up to 7 skaters)
    .get_onice_players <- function(onice_mat, goalie_ids) {
        goalie_id_strs <- as.character(goalie_ids)
        skater_cols <- setdiff(colnames(onice_mat), goalie_id_strs)

        n <- nrow(onice_mat)
        # Up to 7 on-ice skaters
        on_mat <- matrix(NA_integer_, nrow = n, ncol = 7)
        goalie_vec <- rep(NA_integer_, n)

        for (row_i in seq_len(n)) {
            # Determine which players are on ice
            on_ice_all <- colnames(onice_mat)[onice_mat[row_i, ] == 1]

            # Separate goalies from skaters
            on_ice_goalies <- intersect(on_ice_all, goalie_id_strs)
            on_ice_skaters <- setdiff(on_ice_all, goalie_id_strs)

            if (length(on_ice_goalies) > 0) {
                goalie_vec[row_i] <- as.integer(on_ice_goalies[1])
            }

            k <- min(length(on_ice_skaters), 7)
            if (k > 0) {
                on_mat[row_i, seq_len(k)] <- as.integer(on_ice_skaters[seq_len(
                    k
                )])
            }
        }

        list(skaters = on_mat, goalie = goalie_vec)
    }

    home_result <- .get_onice_players(home_onice, home_goalies)
    away_result <- .get_onice_players(away_onice, away_goalies)

    # Add to PBP
    for (i in 1:7) {
        pbp[[paste0("home_on_", i, "_id")]] <- home_result$skaters[, i]
        pbp[[paste0("away_on_", i, "_id")]] <- away_result$skaters[, i]
    }
    pbp$home_goalie_id <- home_result$goalie
    pbp$away_goalie_id <- away_result$goalie

    # Join player names from roster
    for (prefix in c("home", "away")) {
        roster_ref <- if (prefix == "home") rosters else rosters

        for (i in 1:7) {
            id_col <- paste0(prefix, "_on_", i, "_id")
            name_col <- paste0(prefix, "_on_", i)
            pbp <- pbp %>%
                dplyr::left_join(
                    dplyr::select(
                        roster_ref,
                        !!rlang::sym(name_col) := full_name,
                        !!rlang::sym(id_col) := player_id
                    ),
                    by = id_col
                )
        }

        goalie_id_col <- paste0(prefix, "_goalie_id")
        goalie_name_col <- paste0(prefix, "_goalie")
        pbp <- pbp %>%
            dplyr::left_join(
                dplyr::select(
                    roster_ref,
                    !!rlang::sym(goalie_name_col) := full_name,
                    !!rlang::sym(goalie_id_col) := player_id
                ),
                by = goalie_id_col
            )
    }

    return(pbp)
}


#' Join event player names from roster
#' @keywords internal
.join_event_player_names <- function(pbp, rosters) {
    roster_lookup <- rosters %>%
        dplyr::select(player_id, player_name = full_name) %>%
        dplyr::distinct()

    for (p in c(1, 2, 3)) {
        id_col <- paste0("event_player_", p, "_id")
        name_col <- paste0("event_player_", p, "_name")
        if (id_col %in% names(pbp)) {
            pbp <- pbp %>%
                dplyr::left_join(
                    dplyr::rename(
                        roster_lookup,
                        !!rlang::sym(name_col) := player_name,
                        !!rlang::sym(id_col) := player_id
                    ),
                    by = id_col
                )
        }
    }

    # Event goalie name
    if ("event_goalie_id" %in% names(pbp)) {
        pbp <- pbp %>%
            dplyr::left_join(
                dplyr::rename(
                    roster_lookup,
                    event_goalie_name = player_name,
                    event_goalie_id = player_id
                ),
                by = "event_goalie_id"
            )
    }

    return(pbp)
}


#' Add strength state columns
#' @keywords internal
.add_strength_states <- function(pbp, home_abbr, away_abbr) {
    non_plays <- c(
        "PERIOD_START",
        "PERIOD_END",
        "GAME_END",
        "GAME_SCHEDULED",
        "CHANGE"
    )

    pbp <- pbp %>%
        tidyr::fill(c(home_skaters, away_skaters), .direction = "updown") %>%
        dplyr::mutate(
            strength_state = dplyr::case_when(
                event_team_abbr == home_abbr ~ glue::glue(
                    "{home_skaters}v{away_skaters}"
                ),
                event_team_abbr == away_abbr ~ glue::glue(
                    "{away_skaters}v{home_skaters}"
                ),
                TRUE ~ glue::glue("{home_skaters}v{away_skaters}")
            ),
            strength_code = dplyr::case_when(
                home_skaters == away_skaters ~ "EV",
                (home_skaters < away_skaters & event_team_abbr == home_abbr) |
                    (away_skaters < home_skaters &
                        event_team_abbr == away_abbr) ~ "SH",
                (home_skaters < away_skaters & event_team_abbr == away_abbr) |
                    (away_skaters < home_skaters &
                        event_team_abbr == home_abbr) ~ "PP"
            ),
            strength_code = ifelse(
                event_type %in%
                    non_plays |
                    dplyr::lead(event_type) %in% non_plays |
                    dplyr::lag(event_type) %in% non_plays,
                NA_character_,
                strength_code
            ),
            strength = dplyr::case_when(
                strength_code == "EV" ~ "Even",
                strength_code == "SH" ~ "Shorthanded",
                strength_code == "PP" ~ "Power Play"
            )
        )

    return(pbp)
}


#' Add event descriptions
#' @keywords internal
.add_descriptions <- function(pbp, home_abbr, away_abbr) {
    # Ensure columns used in descriptions exist (may be absent without shifts)
    for (col in c("players_on", "players_off", "reason")) {
        if (!col %in% names(pbp)) pbp[[col]] <- NA_character_
    }
    pbp <- pbp %>%
        dplyr::mutate(
            event_idx = dplyr::row_number() - 1L,
            event_id = event_idx,
            description = dplyr::case_when(
                event_type == "PERIOD_START" ~ glue::glue(
                    "Start of Period {period}"
                ),
                event_type == "PERIOD_END" ~ glue::glue(
                    "End of Period {period}"
                ),
                event_type == "GAME_END" ~ "Game End",
                event_type == "FACEOFF" ~ glue::glue(
                    "{event_player_1_name} faceoff won against {event_player_2_name}"
                ),
                event_type == "BLOCKED_SHOT" ~ glue::glue(
                    "{event_player_1_name} shot blocked by {event_player_2_name}"
                ),
                event_type == "CHANGE" ~ glue::glue(
                    "ON: {players_on}; OFF: {players_off}"
                ),
                event_type == "GIVEAWAY" ~ glue::glue(
                    "Giveaway by {event_player_1_name}"
                ),
                event_type == "TAKEAWAY" ~ glue::glue(
                    "Takeaway by {event_player_1_name}"
                ),
                event_type == "HIT" ~ glue::glue(
                    "{event_player_1_name} hit {event_player_2_name}"
                ),
                event_type == "MISSED_SHOT" ~ glue::glue(
                    "{event_player_1_name} shot missed wide of net"
                ),
                event_type == "PENALTY" ~ glue::glue(
                    "{event_player_1_name} {secondary_type}"
                ),
                event_type == "SHOT" ~ glue::glue(
                    "{event_player_1_name} shot on goal saved by {event_goalie_name}"
                ),
                event_type == "STOP" & !is.na(reason) ~ glue::glue(
                    "Stoppage in play ({reason})"
                ),
                event_type == "STOP" ~ "Stoppage in play",
                event_type == "GOAL" & !is.na(event_player_3_id) ~ glue::glue(
                    "{event_player_1_name} {secondary_type}, assists: {event_player_2_name}, {event_player_3_name}"
                ),
                event_type == "GOAL" &
                    !is.na(event_player_2_id) &
                    event_player_2_type == "Assist" ~ glue::glue(
                    "{event_player_1_name} {secondary_type}, assists: {event_player_2_name}"
                ),
                event_type == "GOAL" ~ glue::glue(
                    "{event_player_1_name} {secondary_type}, unassisted"
                ),
                TRUE ~ NA_character_
            )
        )

    return(pbp)
}


#' Finalize column ordering
#' @keywords internal
.finalize_columns <- function(pbp) {
    # Define preferred column order
    preferred <- c(
        "event_type",
        "event",
        "secondary_type",
        "event_team_abbr",
        "event_team_type",
        "description",
        "period",
        "period_type",
        "period_time",
        "period_seconds",
        "period_seconds_remaining",
        "period_time_remaining",
        "game_seconds",
        "game_seconds_remaining",
        "home_score",
        "away_score",
        "event_player_1_name",
        "event_player_1_type",
        "event_player_1_id",
        "event_player_2_name",
        "event_player_2_type",
        "event_player_2_id",
        "event_player_3_name",
        "event_player_3_type",
        "event_player_3_id",
        "event_goalie_name",
        "event_goalie_id",
        "penalty_severity",
        "penalty_minutes",
        "strength_state",
        "strength_code",
        "strength",
        "empty_net",
        "extra_attacker",
        "x",
        "y",
        "x_fixed",
        "y_fixed",
        "shot_distance",
        "shot_angle",
        "home_skaters",
        "away_skaters",
        "home_on_1",
        "home_on_2",
        "home_on_3",
        "home_on_4",
        "home_on_5",
        "home_on_6",
        "home_on_7",
        "away_on_1",
        "away_on_2",
        "away_on_3",
        "away_on_4",
        "away_on_5",
        "away_on_6",
        "away_on_7",
        "home_goalie",
        "away_goalie",
        "num_on",
        "players_on",
        "num_off",
        "players_off",
        "game_id",
        "season",
        "season_type",
        "home_abbr",
        "away_abbr",
        "event_idx",
        "event_id"
    )

    # Select available preferred columns first, then the rest
    available <- intersect(preferred, names(pbp))
    remaining <- setdiff(names(pbp), preferred)

    # Remove raw API columns that have been processed
    raw_api_cols <- grep(
        "^(details\\.|periodDescriptor\\.|typeDescKey|typeCode$|situationCode$|sortOrder$|eventId$|homeTeamDefendingSide$|timeInPeriod$|timeRemaining$|event_owner_team_id$|event_player_4)",
        remaining,
        value = TRUE
    )
    remaining <- setdiff(remaining, raw_api_cols)

    pbp <- pbp[, c(available, remaining), drop = FALSE]

    return(pbp)
}


# ---- Utility helpers ----

#' Parse game rosters from the raw API response
#' @keywords internal
.parse_game_rosters <- function(raw) {
    # The PBP API includes rosterSpots array
    roster_spots <- raw$rosterSpots
    if (is.null(roster_spots) || length(roster_spots) == 0) {
        return(dplyr::tibble(
            player_id = integer(),
            full_name = character(),
            first_name = character(),
            last_name = character(),
            team_abbr = character(),
            position_code = character(),
            sweater_number = integer()
        ))
    }

    roster <- dplyr::as_tibble(roster_spots)

    # Handle nested name fields
    if ("firstName.default" %in% names(roster)) {
        roster$first_name <- roster$`firstName.default`
    } else if (is.list(roster$firstName)) {
        roster$first_name <- sapply(roster$firstName, function(x) {
            x$default %||% NA_character_
        })
    } else {
        roster$first_name <- as.character(roster$firstName)
    }

    if ("lastName.default" %in% names(roster)) {
        roster$last_name <- roster$`lastName.default`
    } else if (is.list(roster$lastName)) {
        roster$last_name <- sapply(roster$lastName, function(x) {
            x$default %||% NA_character_
        })
    } else {
        roster$last_name <- as.character(roster$lastName)
    }

    # Determine which team each player is on
    home_id <- raw$homeTeam$id
    away_id <- raw$awayTeam$id
    home_abbr <- raw$homeTeam$abbrev
    away_abbr <- raw$awayTeam$abbrev

    roster <- roster %>%
        dplyr::mutate(
            player_id = as.integer(playerId),
            full_name = paste(first_name, last_name),
            team_id = as.integer(teamId),
            team_abbr = dplyr::case_when(
                team_id == home_id ~ home_abbr,
                team_id == away_id ~ away_abbr,
                TRUE ~ NA_character_
            ),
            position_code = as.character(positionCode),
            sweater_number = as.integer(sweaterNumber)
        ) %>%
        dplyr::select(
            player_id,
            full_name,
            first_name,
            last_name,
            team_abbr,
            team_id,
            position_code,
            sweater_number
        )

    return(roster)
}


#' Convert time string "MM:SS" to seconds
#' @keywords internal
.time_to_seconds <- function(time_str) {
    ifelse(
        is.na(time_str) | time_str == "",
        NA_real_,
        {
            parts <- strsplit(as.character(time_str), ":")
            sapply(parts, function(p) {
                if (length(p) == 2) {
                    as.numeric(p[1]) * 60 + as.numeric(p[2])
                } else {
                    NA_real_
                }
            })
        }
    )
}


#' Coalesce multiple integer columns
#' @keywords internal
.coalesce_int <- function(df, col_names) {
    vals <- lapply(col_names, function(cn) as.integer(df[[cn]]))
    do.call(dplyr::coalesce, vals)
}


#' Map game type code to label
#' @keywords internal
.game_type_label <- function(code) {
    dplyr::case_when(
        code == 1 ~ "PR",
        code == 2 ~ "R",
        code == 3 ~ "P",
        code == 4 ~ "A",
        TRUE ~ NA_character_
    )
}


#' Get PBP data (internal worker)
#' @keywords internal
.get_pbp_data <- function(game_id, include_shifts = TRUE) {
    feed <- nhl_game_feed(
        game_id = game_id,
        include_shifts = include_shifts
    )
    return(feed$pbp)
}
