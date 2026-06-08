# espn_nhl_events_detail.R
# ESPN NHL: core-v2 game-event detail wrappers (Tier 5b)
# Endpoints: game_plays, game_odds, game_probabilities, game_situation,
#            game_officials, game_broadcasts, game_predictor, game_powerindex,
#            game_leaders, game_scoringplays
# All use the core_v2 host under:
#   events/{event_id}/competitions/{cid}/...
#
# Live verification notes (2026-06-08, event_id="401688263"):
#   plays          -> OK  (244 plays, items df with type/period/clock/coordinate/strength)
#   odds           -> OK  (2 providers, items df with provider/spread/overUnder/teamOdds)
#   probabilities  -> 404 (sparse for NHL; graceful empty)
#   situation      -> OK  (1-row singular: lastPlay $ref, powerPlay, emptyNet)
#   officials      -> OK  (4 officials, items df with name/position)
#   broadcasts     -> OK  (3 broadcasts, items df with type/station/media/market)
#   predictor      -> 404 (sparse for NHL; graceful empty)
#   powerindex     -> 404 (sparse for NHL; graceful empty)
#   leaders        -> 404 (sparse for NHL; graceful empty)
#   scoringplays   -> 404 (no dedicated endpoint for NHL; implemented as filtered game_plays)
#
# cid == event_id confirmed for NHL (single competition per event).


# ===========================================================================
# 1. Game Plays (core-v2 play-by-play collection)
# ===========================================================================

#' Internal: ESPN hockey game plays collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_plays <- function(league = "nhl", event_id,
                                    cid = event_id, limit = 300, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/plays")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No plays returned for event {event_id}.")
        return(result)
      }

      n <- nrow(items)

      # Helper extractors for nested data.frames
      .dcol <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          df_col[[col]]
        else
          rep(NA_character_, n)
      }
      .dcol_num <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          as.numeric(df_col[[col]])
        else
          rep(NA_real_, n)
      }
      .dcol_int <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          as.integer(df_col[[col]])
        else
          rep(NA_integer_, n)
      }

      tbl <- data.frame(
        event_id          = as.character(event_id),
        cid               = as.character(cid),
        id                = as.character(items$id                %||% NA_character_),
        sequence_number   = as.integer( items$sequenceNumber     %||% NA_integer_),
        type_id           = as.character(.dcol(items$type, "id")),
        type_text         = as.character(.dcol(items$type, "text")),
        type_abbreviation = as.character(.dcol(items$type, "abbreviation")),
        text              = as.character(items$text              %||% NA_character_),
        short_text        = as.character(items$shortText         %||% NA_character_),
        period_number     = as.integer( .dcol_int(items$period, "number")),
        period_display    = as.character(.dcol(items$period, "displayValue")),
        clock_value       = as.numeric( .dcol_num(items$clock, "value")),
        clock_display     = as.character(.dcol(items$clock, "displayValue")),
        away_score        = as.integer( items$awayScore          %||% NA_integer_),
        home_score        = as.integer( items$homeScore          %||% NA_integer_),
        scoring_play      = as.logical( items$scoringPlay        %||% NA),
        score_value       = as.integer( items$scoreValue         %||% NA_integer_),
        priority          = as.logical( items$priority           %||% NA),
        shooting_play     = as.logical( items$shootingPlay       %||% NA),
        is_penalty        = as.logical( items$isPenalty          %||% NA),
        wallclock         = as.character(items$wallclock         %||% NA_character_),
        coordinate_x      = as.numeric( .dcol_num(items$coordinate, "x")),
        coordinate_y      = as.numeric( .dcol_num(items$coordinate, "y")),
        strength_id       = as.character(.dcol(items$strength, "id")),
        strength_text     = as.character(.dcol(items$strength, "text")),
        team_ref          = if (is.data.frame(items$team) && "$ref" %in% colnames(items$team))
                              as.character(items$team[["$ref"]]) else rep(NA_character_, n),
        play_ref          = as.character(items[["$ref"]]         %||% NA_character_),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Plays data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game plays for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game plays",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_plays
NULL
#' @title **Get ESPN NHL Game Plays (core-v2 Play-by-Play)**
#' @rdname espn_nhl_game_plays
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id` (for NHL the single
#'   competition always has the same id as its parent event).
#' @param limit Maximum number of plays to return. Defaults to `300`. A typical
#'   NHL game has 200-260 plays.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per play:
#'
#'    |col_name          |types     |description                                           |
#'    |:-----------------|:---------|:-----------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                      |
#'    |cid               |character |Competition id (echoed from arg).                     |
#'    |id                |character |ESPN play identifier.                                 |
#'    |sequence_number   |integer   |Play sequence number within the game.                 |
#'    |type_id           |character |Play type id.                                         |
#'    |type_text         |character |Play type label (e.g. "Goal", "Shot", "Face Off").    |
#'    |type_abbreviation |character |Play type abbreviation.                               |
#'    |text              |character |Full play description text.                           |
#'    |short_text        |character |Short play description text.                          |
#'    |period_number     |integer   |Period number (1-3 regulation, 4+ OT).                |
#'    |period_display    |character |Period display string (e.g. "1st").                   |
#'    |clock_value       |numeric   |Clock value in seconds.                               |
#'    |clock_display     |character |Clock display string (e.g. "19:34").                  |
#'    |away_score        |integer   |Away team score after this play.                      |
#'    |home_score        |integer   |Home team score after this play.                      |
#'    |scoring_play      |logical   |Whether this play resulted in a goal.                 |
#'    |score_value       |integer   |Point value of the scoring play (1 for a goal).       |
#'    |priority          |logical   |Whether this is a priority play for display.          |
#'    |shooting_play     |logical   |Whether this is a shot on goal.                       |
#'    |is_penalty        |logical   |Whether this is a penalty play.                       |
#'    |wallclock         |character |Wall-clock UTC timestamp of the play.                 |
#'    |coordinate_x      |numeric   |X coordinate on the ice surface.                      |
#'    |coordinate_y      |numeric   |Y coordinate on the ice surface.                      |
#'    |strength_id       |character |Strength situation id (e.g. even strength, PP, SH).  |
#'    |strength_text     |character |Strength situation description.                       |
#'    |team_ref          |character |`$ref` URL for the team associated with the play.     |
#'    |play_ref          |character |`$ref` URL for this play object.                      |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_plays(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_plays <- function(event_id, cid = event_id, limit = 300, ...) {
  .espn_hockey_game_plays(league = "nhl", event_id = event_id,
                          cid = cid, limit = limit, ...)
}


# ===========================================================================
# 2. Game Odds
# ===========================================================================

#' Internal: ESPN hockey game odds collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_odds <- function(league = "nhl", event_id,
                                   cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/odds")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No odds returned for event {event_id}.")
        return(result)
      }

      n <- nrow(items)

      .dcol <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          df_col[[col]]
        else
          rep(NA_character_, n)
      }
      # Extract a scalar numeric from a potentially nested df column
      .dcol_num_scalar <- function(df_col, col) {
        if (!is.data.frame(df_col) || !col %in% colnames(df_col))
          return(rep(NA_real_, n))
        v <- df_col[[col]]
        # v might itself be a data.frame (deeply nested); extract $value if so
        if (is.data.frame(v) && "value" %in% colnames(v)) v <- v$value
        if (is.list(v)) v <- vapply(v, function(x) {
          if (is.null(x)) NA_real_ else as.numeric(x[[1L]])
        }, numeric(1))
        as.numeric(v)
      }
      .dcol_lgl <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          as.logical(df_col[[col]])
        else
          rep(NA, n)
      }
      # Safe scalar numeric from items column (handles list columns)
      .safe_num <- function(x) {
        if (is.null(x)) return(rep(NA_real_, n))
        if (is.list(x)) return(vapply(x, function(v) {
          if (is.null(v)) NA_real_ else as.numeric(v[[1L]])
        }, numeric(1)))
        as.numeric(x)
      }
      # Nested: items$open$over is itself a df; get $value from it
      .open_val <- function(open_col, sub) {
        if (!is.data.frame(open_col) || !sub %in% colnames(open_col))
          return(rep(NA_real_, n))
        v <- open_col[[sub]]
        if (is.data.frame(v) && "value" %in% colnames(v)) return(as.numeric(v$value))
        if (is.list(v)) return(vapply(v, function(x) {
          if (is.null(x)) NA_real_ else as.numeric(x[[1L]])
        }, numeric(1)))
        as.numeric(v)
      }
      # Team odds: moneyLine is a nested df under open/close/current
      .team_moneyline <- function(team_odds_col) {
        if (!is.data.frame(team_odds_col) || !"moneyLine" %in% colnames(team_odds_col))
          return(rep(NA_integer_, n))
        v <- team_odds_col$moneyLine
        if (is.data.frame(v) && "american" %in% colnames(v))
          return(suppressWarnings(as.integer(v$american)))
        if (is.list(v))
          return(vapply(v, function(x) {
            if (is.null(x)) NA_integer_ else suppressWarnings(as.integer(x[[1L]]))
          }, integer(1)))
        suppressWarnings(as.integer(v))
      }

      tbl <- data.frame(
        event_id             = as.character(event_id),
        cid                  = as.character(cid),
        provider_id          = as.character(.dcol(items$provider, "id")),
        provider_name        = as.character(.dcol(items$provider, "name")),
        provider_priority    = as.integer(.safe_num(.dcol(items$provider, "priority"))),
        details              = as.character(items$details         %||% NA_character_),
        over_under           = .safe_num(items$overUnder),
        spread               = .safe_num(items$spread),
        over_odds            = .safe_num(items$overOdds),
        under_odds           = .safe_num(items$underOdds),
        moneyline_winner     = as.logical( items$moneylineWinner  %||% NA),
        spread_winner        = as.logical( items$spreadWinner     %||% NA),
        away_team_favorite   = as.logical( .dcol_lgl(items$awayTeamOdds, "favorite")),
        away_team_moneyline  = .team_moneyline(items$awayTeamOdds),
        home_team_favorite   = as.logical( .dcol_lgl(items$homeTeamOdds, "favorite")),
        home_team_moneyline  = .team_moneyline(items$homeTeamOdds),
        open_over            = .open_val(items$open, "over"),
        open_under           = .open_val(items$open, "under"),
        open_total_american  = if (is.data.frame(items$open) && "total" %in% colnames(items$open)) {
          tot <- items$open$total
          if (is.data.frame(tot) && "alternateDisplayValue" %in% colnames(tot))
            as.character(tot$alternateDisplayValue)
          else
            rep(NA_character_, n)
        } else rep(NA_character_, n),
        odds_ref             = as.character(items[["$ref"]]       %||% NA_character_),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Odds data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game odds for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game odds",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_odds
NULL
#' @title **Get ESPN NHL Game Odds (core-v2)**
#' @rdname espn_nhl_game_odds
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per odds provider (typically
#'   2 rows per game — consensus + one sportsbook):
#'
#'    |col_name              |types     |description                                             |
#'    |:---------------------|:---------|:-------------------------------------------------------|
#'    |event_id              |character |ESPN event id (echoed from arg).                        |
#'    |cid                   |character |Competition id (echoed from arg).                       |
#'    |provider_id           |character |Odds provider identifier.                               |
#'    |provider_name         |character |Odds provider name.                                     |
#'    |provider_priority     |integer   |Provider display priority.                              |
#'    |details               |character |Odds detail string (e.g. "DET -185").                   |
#'    |over_under            |numeric   |Over/under total line.                                  |
#'    |spread                |numeric   |Point spread.                                           |
#'    |over_odds             |numeric   |Over moneyline odds (American format).                  |
#'    |under_odds            |numeric   |Under moneyline odds (American format).                 |
#'    |moneyline_winner      |logical   |Whether the moneyline winner was determined.            |
#'    |spread_winner         |logical   |Whether the spread winner was determined.               |
#'    |away_team_favorite    |logical   |Whether the away team was the favorite.                 |
#'    |away_team_moneyline   |integer   |Away team moneyline (American format).                  |
#'    |home_team_favorite    |logical   |Whether the home team was the favorite.                 |
#'    |home_team_moneyline   |integer   |Home team moneyline (American format).                  |
#'    |open_over             |numeric   |Opening over decimal odds value.                        |
#'    |open_under            |numeric   |Opening under decimal odds value.                       |
#'    |open_total_american   |character |Opening total line in American format (e.g. "5.5").     |
#'    |odds_ref              |character |`$ref` URL for this odds object.                        |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_odds(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_odds <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_odds(league = "nhl", event_id = event_id, cid = cid, ...)
}


# ===========================================================================
# 3. Game Probabilities (SPARSE for NHL — 404 on tested games)
# ===========================================================================

#' Internal: ESPN hockey game win-probabilities collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_probabilities <- function(league = "nhl", event_id,
                                            cid = event_id, limit = 300, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/probabilities")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No win-probability data returned for event {event_id} (sparse endpoint for NHL).")
        return(result)
      }

      n <- nrow(items)

      .dcol <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          df_col[[col]]
        else
          rep(NA_character_, n)
      }

      tbl <- data.frame(
        event_id             = as.character(event_id),
        cid                  = as.character(cid),
        sequence_number      = as.integer( items$sequenceNumber      %||% NA_integer_),
        home_win_percentage  = as.numeric( items$homeWinPercentage   %||% NA_real_),
        away_win_percentage  = as.numeric( items$awayWinPercentage   %||% NA_real_),
        tie_percentage       = as.numeric( items$tiePercentage       %||% NA_real_),
        home_toss_up_change  = as.numeric( items$homeTossUpChange    %||% NA_real_),
        away_toss_up_change  = as.numeric( items$awayTossUpChange    %||% NA_real_),
        play_ref             = if ("$ref" %in% colnames(items))
                                 as.character(items[["$ref"]]) else rep(NA_character_, n),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Win Probabilities data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game probabilities for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game probabilities",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_probabilities
NULL
#' @title **Get ESPN NHL Game Win Probabilities (core-v2)**
#' @rdname espn_nhl_game_probabilities
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param limit Maximum number of probability entries to return. Defaults to `300`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per play probability entry. **Note:**
#'   This endpoint is sparse for NHL — most games return an empty tibble with a
#'   warning. When populated, columns include:
#'
#'    |col_name            |types     |description                                           |
#'    |:-------------------|:---------|:-----------------------------------------------------|
#'    |event_id            |character |ESPN event id (echoed from arg).                      |
#'    |cid                 |character |Competition id (echoed from arg).                     |
#'    |sequence_number     |integer   |Play sequence number this probability is tied to.     |
#'    |home_win_percentage |numeric   |Home team win probability (0-1).                      |
#'    |away_win_percentage |numeric   |Away team win probability (0-1).                      |
#'    |tie_percentage      |numeric   |Tie probability (0-1; typically 0 for NHL).           |
#'    |home_toss_up_change |numeric   |Change in home team win probability.                  |
#'    |away_toss_up_change |numeric   |Change in away team win probability.                  |
#'    |play_ref            |character |`$ref` URL for the associated play object.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_probabilities(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_probabilities <- function(event_id, cid = event_id,
                                        limit = 300, ...) {
  .espn_hockey_game_probabilities(league = "nhl", event_id = event_id,
                                  cid = cid, limit = limit, ...)
}


# ===========================================================================
# 4. Game Situation (current/final game state — singular)
# ===========================================================================

#' Internal: ESPN hockey game situation (league-generic, core-v2)
#' @noRd
.espn_hockey_game_situation <- function(league = "nhl", event_id,
                                        cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/situation")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      # Singular object: {$ref, lastPlay: {$ref}, powerPlay, emptyNet}
      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No situation data returned for event {event_id}.")
        return(result)
      }

      last_play_ref <- NA_character_
      if (is.list(raw$lastPlay) && !is.null(raw$lastPlay[["$ref"]])) {
        last_play_ref <- as.character(raw$lastPlay[["$ref"]])
      }

      row <- list(
        event_id      = as.character(event_id),
        cid           = as.character(cid),
        power_play    = as.logical(raw$powerPlay %||% NA),
        empty_net     = as.logical(raw$emptyNet  %||% NA),
        last_play_ref = last_play_ref,
        situation_ref = as.character(raw[["$ref"]] %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Situation data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game situation for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game situation",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_situation
NULL
#' @title **Get ESPN NHL Game Situation (core-v2)**
#' @rdname espn_nhl_game_situation
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with the game state at the moment
#'   of the last tracked play:
#'
#'    |col_name      |types     |description                                             |
#'    |:-------------|:---------|:-------------------------------------------------------|
#'    |event_id      |character |ESPN event id (echoed from arg).                        |
#'    |cid           |character |Competition id (echoed from arg).                       |
#'    |power_play    |logical   |Whether a power play was in effect.                     |
#'    |empty_net     |logical   |Whether the net was empty.                              |
#'    |last_play_ref |character |`$ref` URL for the last tracked play object.            |
#'    |situation_ref |character |`$ref` URL for this situation object.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_situation(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_situation <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_situation(league = "nhl", event_id = event_id,
                              cid = cid, ...)
}


# ===========================================================================
# 5. Game Officials
# ===========================================================================

#' Internal: ESPN hockey game officials collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_officials <- function(league = "nhl", event_id,
                                        cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/officials")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No officials returned for event {event_id}.")
        return(result)
      }

      n <- nrow(items)

      .dcol <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          df_col[[col]]
        else
          rep(NA_character_, n)
      }

      tbl <- data.frame(
        event_id      = as.character(event_id),
        cid           = as.character(cid),
        official_id   = as.character(items$id          %||% NA_character_),
        first_name    = as.character(items$firstName   %||% NA_character_),
        last_name     = as.character(items$lastName    %||% NA_character_),
        full_name     = as.character(items$fullName    %||% NA_character_),
        display_name  = as.character(items$displayName %||% NA_character_),
        order         = as.integer( items$order        %||% NA_integer_),
        position_id   = as.character(.dcol(items$position, "id")),
        position_name = as.character(.dcol(items$position, "name")),
        position_display = as.character(.dcol(items$position, "displayName")),
        official_ref  = as.character(items[["$ref"]]   %||% NA_character_),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Officials data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game officials for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game officials",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_officials
NULL
#' @title **Get ESPN NHL Game Officials (core-v2)**
#' @rdname espn_nhl_game_officials
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per official (typically 4 —
#'   2 referees + 2 linesmen):
#'
#'    |col_name         |types     |description                                             |
#'    |:----------------|:---------|:-------------------------------------------------------|
#'    |event_id         |character |ESPN event id (echoed from arg).                        |
#'    |cid              |character |Competition id (echoed from arg).                       |
#'    |official_id      |character |ESPN official identifier.                               |
#'    |first_name       |character |Official first name.                                    |
#'    |last_name        |character |Official last name.                                     |
#'    |full_name        |character |Official full name.                                     |
#'    |display_name     |character |Official display name.                                  |
#'    |order            |integer   |Display order within officials list.                    |
#'    |position_id      |character |Official position identifier.                           |
#'    |position_name    |character |Official position name (e.g. "Referee", "Linesman").    |
#'    |position_display |character |Official position display name.                         |
#'    |official_ref     |character |`$ref` URL for this official object.                    |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_officials(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_officials <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_officials(league = "nhl", event_id = event_id,
                              cid = cid, ...)
}


# ===========================================================================
# 6. Game Broadcasts
# ===========================================================================

#' Internal: ESPN hockey game broadcasts collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_broadcasts <- function(league = "nhl", event_id,
                                         cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/broadcasts")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No broadcasts returned for event {event_id}.")
        return(result)
      }

      n <- nrow(items)

      .dcol <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          df_col[[col]]
        else
          rep(NA_character_, n)
      }
      .dcol_int <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          as.integer(df_col[[col]])
        else
          rep(NA_integer_, n)
      }

      tbl <- data.frame(
        event_id        = as.character(event_id),
        cid             = as.character(cid),
        type_id         = as.character(.dcol(items$type, "id")),
        type_short_name = as.character(.dcol(items$type, "shortName")),
        type_long_name  = as.character(.dcol(items$type, "longName")),
        type_slug       = as.character(.dcol(items$type, "slug")),
        channel         = as.integer( items$channel    %||% NA_integer_),
        station         = as.character(items$station   %||% NA_character_),
        slug            = as.character(items$slug      %||% NA_character_),
        priority        = as.integer( items$priority   %||% NA_integer_),
        lang            = as.character(items$lang      %||% NA_character_),
        region          = as.character(items$region    %||% NA_character_),
        partnered       = as.logical( items$partnered  %||% NA),
        market_id       = as.character(.dcol(items$market, "id")),
        market_type     = as.character(.dcol(items$market, "type")),
        media_id        = as.character(.dcol(items$media, "id")),
        media_name      = as.character(.dcol(items$media, "name")),
        media_short_name = as.character(.dcol(items$media, "shortName")),
        media_call_letters = as.character(.dcol(items$media, "callLetters")),
        media_slug      = as.character(.dcol(items$media, "slug")),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Broadcasts data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game broadcasts for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game broadcasts",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_broadcasts
NULL
#' @title **Get ESPN NHL Game Broadcasts (core-v2)**
#' @rdname espn_nhl_game_broadcasts
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per broadcast (regional TV +
#'   national, typically 2-4 rows):
#'
#'    |col_name          |types     |description                                             |
#'    |:-----------------|:---------|:-------------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                        |
#'    |cid               |character |Competition id (echoed from arg).                       |
#'    |type_id           |character |Broadcast type id.                                      |
#'    |type_short_name   |character |Broadcast type short name (e.g. "TV").                  |
#'    |type_long_name    |character |Broadcast type long name (e.g. "Television").           |
#'    |type_slug         |character |Broadcast type slug.                                    |
#'    |channel           |integer   |Channel number identifier.                              |
#'    |station           |character |Station full name (e.g. "FanDuel Sports Network Detroit"). |
#'    |slug              |character |Station slug.                                           |
#'    |priority          |integer   |Broadcast priority order.                               |
#'    |lang              |character |Broadcast language (e.g. "en").                         |
#'    |region            |character |Broadcast region (e.g. "us").                           |
#'    |partnered         |logical   |Whether this is a partnered broadcast.                  |
#'    |market_id         |character |Market identifier.                                      |
#'    |market_type       |character |Market type.                                            |
#'    |media_id          |character |Media outlet identifier.                                |
#'    |media_name        |character |Media outlet full name.                                 |
#'    |media_short_name  |character |Media outlet short name.                                |
#'    |media_call_letters |character |Media outlet call letters.                              |
#'    |media_slug        |character |Media outlet slug.                                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_broadcasts(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_broadcasts <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_broadcasts(league = "nhl", event_id = event_id,
                               cid = cid, ...)
}


# ===========================================================================
# 7. Game Predictor (SPARSE for NHL — 404 on tested games)
# ===========================================================================

#' Internal: ESPN hockey game predictor (league-generic, core-v2)
#' @noRd
.espn_hockey_game_predictor <- function(league = "nhl", event_id,
                                        cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/predictor")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No predictor data returned for event {event_id} (sparse endpoint for NHL).")
        return(result)
      }

      # Flatten all scalar fields into a one-row tibble
      scalars <- Filter(function(x) length(x) == 1 && !is.list(x), raw)
      if (length(scalars) == 0) scalars <- list(predictor_ref = as.character(raw[["$ref"]] %||% NA_character_))

      row <- c(
        list(event_id = as.character(event_id), cid = as.character(cid)),
        lapply(scalars, as.character)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Predictor data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game predictor for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game predictor",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_predictor
NULL
#' @title **Get ESPN NHL Game Predictor (core-v2)**
#' @rdname espn_nhl_game_predictor
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with ESPN Predictor data. **Note:**
#'   This endpoint is sparse for NHL — most games return an empty tibble with a
#'   warning. When populated, scalar fields from the predictor object are returned
#'   as character columns alongside `event_id` and `cid`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_predictor(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_predictor <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_predictor(league = "nhl", event_id = event_id,
                              cid = cid, ...)
}


# ===========================================================================
# 8. Game Power Index (SPARSE for NHL — 404 on tested games)
# ===========================================================================

#' Internal: ESPN hockey game power index (league-generic, core-v2)
#' @noRd
.espn_hockey_game_powerindex <- function(league = "nhl", event_id,
                                         cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/powerindex")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No power index data returned for event {event_id} (sparse endpoint for NHL).")
        return(result)
      }

      items <- raw$items
      if (!is.null(items) && (is.data.frame(items) && nrow(items) > 0)) {
        n <- nrow(items)
        tbl <- data.frame(
          event_id  = as.character(event_id),
          cid       = as.character(cid),
          pi_ref    = as.character(items[["$ref"]] %||% NA_character_),
          stringsAsFactors = FALSE
        )
        result <- tbl %>%
          janitor::clean_names() %>%
          make_fastRhockey_data(
            paste0(toupper(league), " Game Power Index data from ESPN core-v2"),
            Sys.time()
          )
      } else {
        scalars <- Filter(function(x) length(x) == 1 && !is.list(x), raw)
        if (length(scalars) == 0)
          scalars <- list(powerindex_ref = as.character(raw[["$ref"]] %||% NA_character_))
        row <- c(
          list(event_id = as.character(event_id), cid = as.character(cid)),
          lapply(scalars, as.character)
        )
        result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
          janitor::clean_names() %>%
          make_fastRhockey_data(
            paste0(toupper(league), " Game Power Index data from ESPN core-v2"),
            Sys.time()
          )
      }
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game power index for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game power index",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_powerindex
NULL
#' @title **Get ESPN NHL Game Power Index (core-v2)**
#' @rdname espn_nhl_game_powerindex
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with ESPN Power Index data. **Note:**
#'   This endpoint is sparse for NHL — most games return an empty tibble with a
#'   warning. When populated, columns contain power-index values alongside
#'   `event_id` and `cid`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_powerindex(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_powerindex <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_powerindex(league = "nhl", event_id = event_id,
                               cid = cid, ...)
}


# ===========================================================================
# 9. Game Leaders (SPARSE for NHL — 404 on tested games)
# ===========================================================================

#' Internal: ESPN hockey game leaders collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_leaders <- function(league = "nhl", event_id,
                                      cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/leaders")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No game leaders returned for event {event_id} (sparse endpoint for NHL).")
        return(result)
      }

      n <- nrow(items)

      .dcol <- function(df_col, col) {
        if (is.data.frame(df_col) && col %in% colnames(df_col))
          df_col[[col]]
        else
          rep(NA_character_, n)
      }

      tbl <- data.frame(
        event_id        = as.character(event_id),
        cid             = as.character(cid),
        category_name   = as.character(.dcol(items$category, "name")),
        category_display = as.character(.dcol(items$category, "displayName")),
        category_abbr   = as.character(.dcol(items$category, "abbreviation")),
        value            = as.numeric(items$value            %||% NA_real_),
        display_value    = as.character(items$displayValue   %||% NA_character_),
        athlete_ref      = if (is.data.frame(items$athlete) && "$ref" %in% colnames(items$athlete))
                             as.character(items$athlete[["$ref"]]) else rep(NA_character_, n),
        team_ref         = if (is.data.frame(items$team) && "$ref" %in% colnames(items$team))
                             as.character(items$team[["$ref"]]) else rep(NA_character_, n),
        leader_ref       = as.character(items[["$ref"]]      %||% NA_character_),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Leaders data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game leaders for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game leaders",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_leaders
NULL
#' @title **Get ESPN NHL Game Leaders (core-v2)**
#' @rdname espn_nhl_game_leaders
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (category, leader) pair.
#'   **Note:** This endpoint is sparse for NHL — most games return an empty tibble
#'   with a warning. When populated:
#'
#'    |col_name          |types     |description                                             |
#'    |:-----------------|:---------|:-------------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                        |
#'    |cid               |character |Competition id (echoed from arg).                       |
#'    |category_name     |character |Stat category name.                                     |
#'    |category_display  |character |Stat category display name.                             |
#'    |category_abbr     |character |Stat category abbreviation.                             |
#'    |value             |numeric   |Leader stat value.                                      |
#'    |display_value     |character |Formatted stat value.                                   |
#'    |athlete_ref       |character |`$ref` URL for the leader athlete.                      |
#'    |team_ref          |character |`$ref` URL for the leader's team.                       |
#'    |leader_ref        |character |`$ref` URL for this leader entry.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_leaders(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_leaders <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_leaders(league = "nhl", event_id = event_id, cid = cid, ...)
}


# ===========================================================================
# 10. Game Scoring Plays
# NOTE: No dedicated /scoringplays endpoint exists for NHL in core-v2
#       (returns 404 on tested games). This function calls game_plays() and
#       filters to plays where scoring_play == TRUE.
# ===========================================================================

#' Internal: ESPN hockey game scoring plays (league-generic, core-v2)
#' @noRd
.espn_hockey_game_scoringplays <- function(league = "nhl", event_id,
                                           cid = event_id, limit = 300, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      # First try the dedicated scoringplays endpoint
      path <- paste0("events/", event_id, "/competitions/", cid, "/scoringplays")
      raw <- tryCatch(
        .espn_hockey_request("core_v2", path = path,
                             league = league,
                             simplifyVector = TRUE, ...),
        error = function(e) NULL
      )

      if (!is.null(raw) && !is.null(raw$items) &&
          ((is.data.frame(raw$items) && nrow(raw$items) > 0) ||
           (is.list(raw$items) && length(raw$items) > 0))) {
        # Dedicated endpoint returned data — parse as a $ref collection
        items  <- raw$items
        n      <- if (is.data.frame(items)) nrow(items) else length(items)
        refs   <- if (is.data.frame(items) && "$ref" %in% colnames(items))
                    as.character(items[["$ref"]]) else rep(NA_character_, n)
        ids    <- vapply(refs, .espn_ref_id, character(1), USE.NAMES = FALSE)

        tbl <- data.frame(
          event_id   = as.character(event_id),
          cid        = as.character(cid),
          play_id    = ids,
          play_ref   = refs,
          stringsAsFactors = FALSE
        )
        result <- tbl %>%
          janitor::clean_names() %>%
          make_fastRhockey_data(
            paste0(toupper(league), " Game Scoring Plays data from ESPN core-v2"),
            Sys.time()
          )
      } else {
        # Fall back: fetch all plays and filter to scoring_play == TRUE
        all_plays <- .espn_hockey_game_plays(
          league = league, event_id = event_id, cid = cid, limit = limit, ...)

        if (is.data.frame(all_plays) && nrow(all_plays) > 0 &&
            "scoring_play" %in% colnames(all_plays)) {
          result <- all_plays[!is.na(all_plays$scoring_play) &
                                all_plays$scoring_play == TRUE, ]
          if (nrow(result) == 0) {
            cli::cli_alert_warning(
              "{Sys.time()}: No scoring plays found for event {event_id}.")
          }
        } else {
          cli::cli_alert_warning(
            "{Sys.time()}: No plays data available to filter for scoring plays (event {event_id}).")
        }
      }
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} scoring plays for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game scoring plays",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_scoringplays
NULL
#' @title **Get ESPN NHL Game Scoring Plays (core-v2)**
#' @rdname espn_nhl_game_scoringplays
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param limit Maximum number of plays to fetch for filtering. Defaults to `300`.
#'   Only used when the dedicated `scoringplays` endpoint is unavailable and the
#'   function falls back to filtering `game_plays()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per scoring play (goal).
#'   **Implementation note:** The dedicated `/scoringplays` core-v2 endpoint
#'   returns 404 for NHL; this function falls back to fetching all plays via
#'   `espn_nhl_game_plays()` and filtering to rows where `scoring_play == TRUE`.
#'   Columns are the same as `espn_nhl_game_plays()`:
#'
#'    |col_name          |types     |description                                           |
#'    |:-----------------|:---------|:-----------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                      |
#'    |cid               |character |Competition id (echoed from arg).                     |
#'    |id                |character |ESPN play identifier.                                 |
#'    |sequence_number   |integer   |Play sequence number within the game.                 |
#'    |type_id           |character |Play type id.                                         |
#'    |type_text         |character |Play type label (always "Goal" for scoring plays).    |
#'    |text              |character |Full play description text.                           |
#'    |period_number     |integer   |Period number of the goal.                            |
#'    |clock_display     |character |Clock time of the goal.                               |
#'    |away_score        |integer   |Away team score after the goal.                       |
#'    |home_score        |integer   |Home team score after the goal.                       |
#'    |scoring_play      |logical   |Always `TRUE`.                                        |
#'    |score_value       |integer   |Always 1 for NHL goals.                               |
#'    |coordinate_x      |numeric   |X coordinate of the shot.                             |
#'    |coordinate_y      |numeric   |Y coordinate of the shot.                             |
#'    |strength_text     |character |Strength situation (e.g. "Even Strength", "Power Play"). |
#'    |team_ref          |character |`$ref` URL for the scoring team.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_scoringplays(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_scoringplays <- function(event_id, cid = event_id,
                                       limit = 300, ...) {
  .espn_hockey_game_scoringplays(league = "nhl", event_id = event_id,
                                 cid = cid, limit = limit, ...)
}
