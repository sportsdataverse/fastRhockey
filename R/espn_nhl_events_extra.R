# espn_nhl_events_extra.R
# ESPN NHL: core-v2 game long-tail wrappers (Group E, Tier 5c)
# Endpoints: game_play, game_play_personnel, game_status, game_team_leaders,
#            game_team_record, game_official_detail, game_propbets
# All use the core_v2 host under:
#   events/{event_id}/competitions/{cid}/...
#
# Live verification notes (2026-06-08, event_id="401688263"):
#   game_play           -> OK  (1-row; scalar fields + nested type/period/clock)
#   game_play_personnel -> OK  (graceful: playPersonnel has competitor/$ref + empty entries)
#   game_status         -> OK  (1-row; clock, period, type: id/name/state/completed)
#   game_team_leaders   -> OK  (categories list -> long tibble; 6 cats, ~1 leader each)
#   game_team_record    -> 404 (sparse for NHL game-level; graceful empty)
#   game_official_detail -> OK  (1-row; id/firstName/lastName/fullName/position)
#   game_propbets       -> 404 (sparse for NHL; graceful empty)
#
# cid == event_id confirmed for NHL.


# ===========================================================================
# 1. Game Play (singular play detail)
# ===========================================================================

#' Internal: ESPN hockey singular play detail (league-generic, core-v2)
#' @noRd
.espn_hockey_game_play <- function(league = "nhl", event_id,
                                   cid = event_id, play_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/plays/", play_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No play data returned for event {event_id} play {play_id}.")
        return(result)
      }

      # Nested extractors
      .schar <- function(x) as.character(x %||% NA_character_)
      .sint  <- function(x) as.integer( x %||% NA_integer_)
      .slgl  <- function(x) as.logical( x %||% NA)
      .snum  <- function(x) as.numeric( x %||% NA_real_)

      row <- list(
        event_id          = .schar(event_id),
        cid               = .schar(cid),
        play_id           = .schar(play_id),
        id                = .schar(raw$id),
        sequence_number   = .sint(raw$sequenceNumber),
        type_id           = .schar(raw$type$id),
        type_text         = .schar(raw$type$text),
        type_abbreviation = .schar(raw$type$abbreviation),
        text              = .schar(raw$text),
        short_text        = .schar(raw$shortText),
        away_score        = .sint(raw$awayScore),
        home_score        = .sint(raw$homeScore),
        period_number     = .sint(raw$period$number),
        period_display    = .schar(raw$period$displayValue),
        clock_value       = .snum(raw$clock$value),
        clock_display     = .schar(raw$clock$displayValue),
        scoring_play      = .slgl(raw$scoringPlay),
        score_value       = .sint(raw$scoreValue),
        priority          = .slgl(raw$priority),
        shooting_play     = .slgl(raw$shootingPlay),
        is_penalty        = .slgl(raw$isPenalty),
        wallclock         = .schar(raw$wallclock),
        play_ref          = .schar(raw[["$ref"]])
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Play data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} play data for event {event_id} / play {play_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game play",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_play
NULL
#' @title **Get ESPN NHL Game Play Detail (core-v2)**
#' @rdname espn_nhl_game_play
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id` (for NHL the single
#'   competition always has the same id as its parent event).
#' @param play_id ESPN play identifier. Use `espn_nhl_game_plays(event_id)$id`
#'   to find valid play ids for a game.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with play detail:
#'
#'    |col_name          |types     |description                                           |
#'    |:-----------------|:---------|:-----------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                      |
#'    |cid               |character |Competition id (echoed from arg).                     |
#'    |play_id           |character |ESPN play id (echoed from arg).                       |
#'    |id                |character |ESPN play identifier.                                 |
#'    |sequence_number   |integer   |Play sequence number within the game.                 |
#'    |type_id           |character |Play type id.                                         |
#'    |type_text         |character |Play type label (e.g. "Goal", "Shot").                |
#'    |type_abbreviation |character |Play type abbreviation.                               |
#'    |text              |character |Full play description text.                           |
#'    |short_text        |character |Short play description text.                          |
#'    |away_score        |integer   |Away team score after this play.                      |
#'    |home_score        |integer   |Home team score after this play.                      |
#'    |period_number     |integer   |Period number (1-3 regulation, 4+ OT).                |
#'    |period_display    |character |Period display string (e.g. "1st").                   |
#'    |clock_value       |numeric   |Clock value in seconds.                               |
#'    |clock_display     |character |Clock display string (e.g. "19:34").                  |
#'    |scoring_play      |logical   |Whether this play resulted in a goal.                 |
#'    |score_value       |integer   |Point value of the scoring play.                      |
#'    |priority          |logical   |Whether this is a priority play for display.          |
#'    |shooting_play     |logical   |Whether this is a shot on goal.                       |
#'    |is_penalty        |logical   |Whether this is a penalty play.                       |
#'    |wallclock         |character |Wall-clock UTC timestamp of the play.                 |
#'    |play_ref          |character |`$ref` URL for this play object.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     pls <- espn_nhl_game_plays(event_id = eid)
#'     espn_nhl_game_play(event_id = eid, play_id = pls$id[1])
#'   })
#' }
espn_nhl_game_play <- function(event_id, cid = event_id, play_id, ...) {
  .espn_hockey_game_play(league = "nhl", event_id = event_id,
                         cid = cid, play_id = play_id, ...)
}


# ===========================================================================
# 2. Game Play Personnel (personnel on a specific play)
# ===========================================================================

#' Internal: ESPN hockey play personnel collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_play_personnel <- function(league = "nhl", event_id,
                                             cid = event_id, play_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/plays/", play_id, "/personnel")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      pp <- raw$playPersonnel
      if (is.null(pp) || length(pp) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No personnel data for event {event_id} play {play_id} (sparse endpoint).")
        return(result)
      }

      rows <- list()
      for (comp_block in pp) {
        comp_ref <- comp_block$competitor[["$ref"]] %||% NA_character_
        comp_id  <- .espn_ref_id(comp_ref)
        entries  <- comp_block$entries
        if (is.null(entries) || length(entries) == 0) {
          # Competitor block present but no athlete entries — emit one ref-only row
          rows[[length(rows) + 1L]] <- list(
            event_id      = as.character(event_id),
            cid           = as.character(cid),
            play_id       = as.character(play_id),
            competitor_id = as.character(comp_id),
            competitor_ref = as.character(comp_ref),
            athlete_ref   = NA_character_,
            athlete_id    = NA_character_
          )
        } else {
          for (entry in entries) {
            ath_ref <- if (is.list(entry$athlete)) entry$athlete[["$ref"]] %||% NA_character_
                       else NA_character_
            rows[[length(rows) + 1L]] <- list(
              event_id       = as.character(event_id),
              cid            = as.character(cid),
              play_id        = as.character(play_id),
              competitor_id  = as.character(comp_id),
              competitor_ref = as.character(comp_ref),
              athlete_ref    = as.character(ath_ref),
              athlete_id     = as.character(.espn_ref_id(ath_ref))
            )
          }
        }
      }

      if (length(rows) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: Empty personnel for event {event_id} play {play_id}.")
        return(result)
      }

      result <- dplyr::bind_rows(lapply(rows, as.data.frame, stringsAsFactors = FALSE)) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Play Personnel data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} play personnel for event {event_id} / play {play_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game play personnel",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_play_personnel
NULL
#' @title **Get ESPN NHL Game Play Personnel (core-v2)**
#' @rdname espn_nhl_game_play_personnel
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param play_id ESPN play identifier.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (competitor, athlete)
#'   pair on the play. For NHL this endpoint is typically sparse: the competitor
#'   block is present but the `entries` array is empty, yielding one row per
#'   team with `NA` athlete fields. Returns an empty `data.frame()` if the
#'   endpoint is not available.
#'
#'    |col_name       |types     |description                                               |
#'    |:--------------|:---------|:---------------------------------------------------------|
#'    |event_id       |character |ESPN event id (echoed from arg).                          |
#'    |cid            |character |Competition id (echoed from arg).                         |
#'    |play_id        |character |ESPN play id (echoed from arg).                           |
#'    |competitor_id  |character |ESPN team/competitor id for this personnel block.         |
#'    |competitor_ref |character |`$ref` URL for the competitor object.                     |
#'    |athlete_ref    |character |`$ref` URL for the athlete (NA if entries are empty).     |
#'    |athlete_id     |character |Athlete id parsed from the `$ref` (NA if entries empty).  |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     pls <- espn_nhl_game_plays(event_id = eid)
#'     espn_nhl_game_play_personnel(event_id = eid, play_id = pls$id[1])
#'   })
#' }
espn_nhl_game_play_personnel <- function(event_id, cid = event_id, play_id, ...) {
  .espn_hockey_game_play_personnel(league = "nhl", event_id = event_id,
                                   cid = cid, play_id = play_id, ...)
}


# ===========================================================================
# 3. Game Status (singular competition status)
# ===========================================================================

#' Internal: ESPN hockey competition status (league-generic, core-v2)
#' @noRd
.espn_hockey_game_status <- function(league = "nhl", event_id,
                                     cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/status")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No status data returned for event {event_id}.")
        return(result)
      }

      row <- list(
        event_id          = as.character(event_id),
        cid               = as.character(cid),
        clock             = as.numeric( raw$clock          %||% NA_real_),
        display_clock     = as.character(raw$displayClock  %||% NA_character_),
        period            = as.integer( raw$period         %||% NA_integer_),
        type_id           = as.character(raw$type$id       %||% NA_character_),
        type_name         = as.character(raw$type$name     %||% NA_character_),
        type_state        = as.character(raw$type$state    %||% NA_character_),
        type_completed    = as.logical( raw$type$completed %||% NA),
        type_description  = as.character(raw$type$description  %||% NA_character_),
        type_detail       = as.character(raw$type$detail        %||% NA_character_),
        type_short_detail = as.character(raw$type$shortDetail   %||% NA_character_),
        status_ref        = as.character(raw[["$ref"]]    %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Status data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game status for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game status",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_status
NULL
#' @title **Get ESPN NHL Game Status (core-v2)**
#' @rdname espn_nhl_game_status
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with game status detail:
#'
#'    |col_name          |types     |description                                             |
#'    |:-----------------|:---------|:-------------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                        |
#'    |cid               |character |Competition id (echoed from arg).                       |
#'    |clock             |numeric   |Game clock value in seconds.                            |
#'    |display_clock     |character |Game clock display string (e.g. "0:00").                |
#'    |period            |integer   |Current or final period number.                         |
#'    |type_id           |character |Status type identifier.                                 |
#'    |type_name         |character |Status type name (e.g. "STATUS_FINAL").                 |
#'    |type_state        |character |Status state (e.g. "pre", "in", "post").                |
#'    |type_completed    |logical   |Whether the game is complete.                           |
#'    |type_description  |character |Status description (e.g. "Final").                      |
#'    |type_detail       |character |Status detail string.                                   |
#'    |type_short_detail |character |Short status detail string.                             |
#'    |status_ref        |character |`$ref` URL for this status object.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_status(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_status <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_status(league = "nhl", event_id = event_id,
                            cid = cid, ...)
}


# ===========================================================================
# 4. Game Team Leaders (categories long tibble)
# ===========================================================================

#' Internal: ESPN hockey game team leaders (league-generic, core-v2)
#' @noRd
.espn_hockey_game_team_leaders <- function(league = "nhl", event_id,
                                           cid = event_id, team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/competitors/", team_id, "/leaders")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      cats <- raw$categories
      if (is.null(cats) || length(cats) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No leaders returned for event {event_id} / team {team_id}.")
        return(result)
      }

      rows <- list()
      for (cat in cats) {
        cat_name  <- as.character(cat$name         %||% NA_character_)
        cat_disp  <- as.character(cat$displayName  %||% NA_character_)
        cat_short <- as.character(cat$shortDisplayName %||% NA_character_)
        cat_abbr  <- as.character(cat$abbreviation %||% NA_character_)

        leaders <- cat$leaders
        if (is.null(leaders) || length(leaders) == 0) {
          rows[[length(rows) + 1L]] <- list(
            event_id          = as.character(event_id),
            cid               = as.character(cid),
            team_id           = as.character(team_id),
            category_name     = cat_name,
            category_display  = cat_disp,
            category_short    = cat_short,
            category_abbr     = cat_abbr,
            display_value     = NA_character_,
            value             = NA_real_,
            athlete_ref       = NA_character_,
            athlete_id        = NA_character_,
            team_ref          = NA_character_
          )
        } else {
          for (ldr in leaders) {
            ath_ref  <- if (is.list(ldr$athlete)) ldr$athlete[["$ref"]] %||% NA_character_
                        else NA_character_
            team_ref <- if (is.list(ldr$team)) ldr$team[["$ref"]] %||% NA_character_
                        else NA_character_
            rows[[length(rows) + 1L]] <- list(
              event_id         = as.character(event_id),
              cid              = as.character(cid),
              team_id          = as.character(team_id),
              category_name    = cat_name,
              category_display = cat_disp,
              category_short   = cat_short,
              category_abbr    = cat_abbr,
              display_value    = as.character(ldr$displayValue %||% NA_character_),
              value            = as.numeric( ldr$value         %||% NA_real_),
              athlete_ref      = as.character(ath_ref),
              athlete_id       = as.character(.espn_ref_id(ath_ref)),
              team_ref         = as.character(team_ref)
            )
          }
        }
      }

      if (length(rows) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: Empty leaders for event {event_id} / team {team_id}.")
        return(result)
      }

      result <- dplyr::bind_rows(lapply(rows, as.data.frame, stringsAsFactors = FALSE)) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Team Leaders data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game leaders for event {event_id} / team {team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game team leaders",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_team_leaders
NULL
#' @title **Get ESPN NHL Game Team Leaders (core-v2)**
#' @rdname espn_nhl_game_team_leaders
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier. Use `espn_nhl_game_teams(event_id)$team_id`
#'   to find valid team ids for a game.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (category, leader).
#'   For a typical completed NHL game there are 6 stat categories (goals, assists,
#'   points, plusMinus, penaltyMinutes, saves) with one leader each, yielding
#'   ~6 rows per team:
#'
#'    |col_name         |types     |description                                             |
#'    |:----------------|:---------|:-------------------------------------------------------|
#'    |event_id         |character |ESPN event id (echoed from arg).                        |
#'    |cid              |character |Competition id (echoed from arg).                       |
#'    |team_id          |character |ESPN team id (echoed from arg).                         |
#'    |category_name    |character |Stat category name (e.g. "goals").                      |
#'    |category_display |character |Stat category display name.                             |
#'    |category_short   |character |Stat category short display name.                       |
#'    |category_abbr    |character |Stat category abbreviation.                             |
#'    |display_value    |character |Leader stat display value (e.g. "2").                   |
#'    |value            |numeric   |Leader stat numeric value.                              |
#'    |athlete_ref      |character |`$ref` URL for the leading athlete.                     |
#'    |athlete_id       |character |Athlete id parsed from the `$ref`.                      |
#'    |team_ref         |character |`$ref` URL for the athlete's team.                      |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_nhl_game_teams(event_id = eid)
#'     espn_nhl_game_team_leaders(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_game_team_leaders <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_leaders(league = "nhl", event_id = event_id,
                                 cid = cid, team_id = team_id, ...)
}


# ===========================================================================
# 5. Game Team Record (SPARSE for NHL — 404 on tested games)
# ===========================================================================

#' Internal: ESPN hockey game team record (league-generic, core-v2)
#' @noRd
.espn_hockey_game_team_record <- function(league = "nhl", event_id,
                                          cid = event_id, team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/competitors/", team_id, "/record")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No record data for event {event_id} / team {team_id} (sparse endpoint for NHL).")
        return(result)
      }

      items <- raw$items
      if (is.null(items) || length(items) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No record items for event {event_id} / team {team_id}.")
        return(result)
      }

      rows <- lapply(items, function(item) {
        list(
          event_id      = as.character(event_id),
          cid           = as.character(cid),
          team_id       = as.character(team_id),
          name          = as.character(item$name           %||% NA_character_),
          abbreviation  = as.character(item$abbreviation   %||% NA_character_),
          type          = as.character(item$type           %||% NA_character_),
          summary       = as.character(item$summary        %||% NA_character_),
          display_value = as.character(item$displayValue   %||% NA_character_),
          value         = as.numeric( item$value           %||% NA_real_),
          record_ref    = as.character(item[["$ref"]]      %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, as.data.frame, stringsAsFactors = FALSE)) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Team Record data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game record for event {event_id} / team {team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game team record",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_team_record
NULL
#' @title **Get ESPN NHL Game Team Record (core-v2)**
#' @rdname espn_nhl_game_team_record
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per record type (e.g.
#'   overall, home, road). **Note:** this endpoint returns 404 for most NHL
#'   games tested; an empty `data.frame()` is returned in that case.
#'
#'    |col_name      |types     |description                                              |
#'    |:-------------|:---------|:--------------------------------------------------------|
#'    |event_id      |character |ESPN event id (echoed from arg).                         |
#'    |cid           |character |Competition id (echoed from arg).                        |
#'    |team_id       |character |ESPN team id (echoed from arg).                          |
#'    |name          |character |Record type name (e.g. "overall", "home", "road").       |
#'    |abbreviation  |character |Record type abbreviation.                                |
#'    |type          |character |Record type slug.                                        |
#'    |summary       |character |Record summary string (e.g. "25-15-10").                 |
#'    |display_value |character |Record display value.                                    |
#'    |value         |numeric   |Record numeric value (e.g. win percentage).              |
#'    |record_ref    |character |`$ref` URL for this record item.                         |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_nhl_game_teams(event_id = eid)
#'     espn_nhl_game_team_record(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_game_team_record <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_record(league = "nhl", event_id = event_id,
                                cid = cid, team_id = team_id, ...)
}


# ===========================================================================
# 6. Game Official Detail (singular official within a competition)
# ===========================================================================

#' Internal: ESPN hockey singular official detail (league-generic, core-v2)
#' @noRd
.espn_hockey_game_official_detail <- function(league = "nhl", event_id,
                                              cid = event_id, official_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/officials/", official_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No official detail for event {event_id} / official {official_id}.")
        return(result)
      }

      row <- list(
        event_id         = as.character(event_id),
        cid              = as.character(cid),
        official_id      = as.character(official_id),
        id               = as.character(raw$id          %||% NA_character_),
        first_name       = as.character(raw$firstName   %||% NA_character_),
        last_name        = as.character(raw$lastName    %||% NA_character_),
        full_name        = as.character(raw$fullName    %||% NA_character_),
        display_name     = as.character(raw$displayName %||% NA_character_),
        order            = as.integer( raw$order        %||% NA_integer_),
        position_id      = as.character(raw$position$id          %||% NA_character_),
        position_name    = as.character(raw$position$name        %||% NA_character_),
        position_display = as.character(raw$position$displayName %||% NA_character_),
        official_ref     = as.character(raw[["$ref"]]   %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Official Detail data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} official detail for event {event_id} / official {official_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game official detail",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_official_detail
NULL
#' @title **Get ESPN NHL Game Official Detail (core-v2)**
#' @rdname espn_nhl_game_official_detail
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param official_id ESPN official identifier. Use `espn_nhl_game_officials(event_id)$official_id`
#'   to find valid official ids for a game.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with official detail:
#'
#'    |col_name         |types     |description                                             |
#'    |:----------------|:---------|:-------------------------------------------------------|
#'    |event_id         |character |ESPN event id (echoed from arg).                        |
#'    |cid              |character |Competition id (echoed from arg).                       |
#'    |official_id      |character |ESPN official id (echoed from arg).                     |
#'    |id               |character |ESPN official identifier.                               |
#'    |first_name       |character |Official first name.                                    |
#'    |last_name        |character |Official last name.                                     |
#'    |full_name        |character |Official full name.                                     |
#'    |display_name     |character |Official display name.                                  |
#'    |order            |integer   |Display order within officials list.                    |
#'    |position_id      |character |Official position identifier.                           |
#'    |position_name    |character |Official position name (e.g. "Linesman", "Referee").    |
#'    |position_display |character |Official position display name.                         |
#'    |official_ref     |character |`$ref` URL for this official object.                    |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb   <- espn_nhl_scoreboard(dates = "20250110")
#'     eid  <- sb$game_id[1]
#'     offs <- espn_nhl_game_officials(event_id = eid)
#'     espn_nhl_game_official_detail(event_id = eid, official_id = offs$official_id[1])
#'   })
#' }
espn_nhl_game_official_detail <- function(event_id, cid = event_id,
                                          official_id, ...) {
  .espn_hockey_game_official_detail(league = "nhl", event_id = event_id,
                                    cid = cid, official_id = official_id, ...)
}


# ===========================================================================
# 7. Game Propbets (SPARSE for NHL — 404 on tested games)
# ===========================================================================

#' Internal: ESPN hockey game prop bets collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_propbets <- function(league = "nhl", event_id,
                                       cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/propbets")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      items <- raw$items
      if (is.null(items) || length(items) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No propbets for event {event_id} (sparse endpoint for NHL).")
        return(result)
      }

      rows <- lapply(items, function(item) {
        list(
          event_id      = as.character(event_id),
          cid           = as.character(cid),
          id            = as.character(item$id            %||% NA_character_),
          name          = as.character(item$name          %||% NA_character_),
          description   = as.character(item$description   %||% NA_character_),
          display_value = as.character(item$displayValue  %||% NA_character_),
          value         = as.numeric( item$value          %||% NA_real_),
          propbet_ref   = as.character(item[["$ref"]]     %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, as.data.frame, stringsAsFactors = FALSE)) |>
        janitor::clean_names() |>
        make_fastRhockey_data(
          paste0(toupper(league), " Game Propbets data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} propbets for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game propbets",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_propbets
NULL
#' @title **Get ESPN NHL Game Prop Bets (core-v2)**
#' @rdname espn_nhl_game_propbets
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per prop bet. **Note:** this
#'   endpoint returns 404 for most NHL games; an empty `data.frame()` is returned
#'   in that case. When data is available (e.g. playoff games):
#'
#'    |col_name      |types     |description                                              |
#'    |:-------------|:---------|:--------------------------------------------------------|
#'    |event_id      |character |ESPN event id (echoed from arg).                         |
#'    |cid           |character |Competition id (echoed from arg).                        |
#'    |id            |character |Prop bet identifier.                                     |
#'    |name          |character |Prop bet name.                                           |
#'    |description   |character |Prop bet description.                                    |
#'    |display_value |character |Prop bet display value.                                  |
#'    |value         |numeric   |Prop bet numeric value.                                  |
#'    |propbet_ref   |character |`$ref` URL for this prop bet item.                       |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_propbets(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_propbets <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_propbets(league = "nhl", event_id = event_id,
                              cid = cid, ...)
}
