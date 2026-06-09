# WHL thin public wrappers over the league-generic HockeyTech core.
# Each function delegates to the corresponding .hockeytech_*("whl", ...) core
# and applies make_fastRhockey_data() for data-frame returns.

# ---------------------------------------------------------------------------
# Overview alias
# ---------------------------------------------------------------------------

#' @name whl
#' @title **WHL (Western Hockey League) Endpoint Overview**
#' @description Thin exported wrappers around the HockeyTech feeds for the WHL.
#' @keywords WHL
#' @family WHL Functions
NULL

# ---------------------------------------------------------------------------
# Season helpers
# ---------------------------------------------------------------------------

#' @title **WHL Season IDs**
#' @description All seasons for the WHL with end-year and game-type labels,
#'   from the HockeyTech modulekit/seasons feed.
#' @return A `fastRhockey_data` data frame with columns: season_id, season_name,
#'   season_short, career, playoff, start_date, end_date, season_yr,
#'   game_type_label.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_season_id()) }
whl_season_id <- function() {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_season_id_df("whl")
      out <- make_fastRhockey_data(out, "WHL Season IDs from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL season IDs unavailable! {e}")
  )
  out
}

#' @title **Most Recent WHL Season**
#' @description Returns the most-recent WHL season as an end-year integer
#'   (e.g. 2025L).
#' @return Integer end-year.
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(most_recent_whl_season()) }
most_recent_whl_season <- function() {
  out <- 2026L
  tryCatch(
    expr = { out <- .hockeytech_most_recent_season("whl") },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL most-recent season unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Schedule
# ---------------------------------------------------------------------------

#' @title **WHL Schedule**
#' @description WHL schedule from the HockeyTech feed (one row per game).
#' @param season End-year season (e.g. 2025); optional.
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per game.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_schedule()) }
whl_schedule <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_schedule("whl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "WHL Schedule from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL schedule unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Play-by-play
# ---------------------------------------------------------------------------

#' @title **WHL Play-by-Play**
#' @description WHL play-by-play for a single game (one row per event, fully
#'   enriched with coordinate transforms, on-ice players, Corsi geometry).
#' @param game_id Numeric WHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per play-by-play event.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_pbp(game_id = 27225)) }
whl_pbp <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_pbp("whl", game_id = game_id)
      out <- make_fastRhockey_data(out, "WHL Play-by-Play from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL PBP for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Standings
# ---------------------------------------------------------------------------

#' @title **WHL Standings**
#' @description WHL standings from the HockeyTech feed (one row per team).
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_standings()) }
whl_standings <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_standings("whl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "WHL Standings from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL standings unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Teams
# ---------------------------------------------------------------------------

#' @title **WHL Teams**
#' @description WHL teams for a given season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_teams()) }
whl_teams <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_teams("whl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "WHL Teams from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL teams unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Team roster
# ---------------------------------------------------------------------------

#' @title **WHL Team Roster**
#' @description WHL roster for a given team and season from the HockeyTech feed.
#' @param team_id Numeric or character WHL team identifier.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_team_roster(team_id = 1)) }
whl_team_roster <- function(team_id, season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_team_roster("whl", team_id = team_id, season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "WHL Team Roster from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL team roster for team_id {team_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player stats
# ---------------------------------------------------------------------------

#' @title **WHL Player Stats**
#' @description WHL player season stats across all seasons.
#' @param player_id Numeric or character WHL player identifier.
#' @return A `fastRhockey_data` data frame, one row per season-stat entry.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_player_stats(player_id = 1)) }
whl_player_stats <- function(player_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_stats("whl", player_id = player_id)
      out <- make_fastRhockey_data(out, "WHL Player Stats from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL player stats for player_id {player_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Leaders
# ---------------------------------------------------------------------------

#' @title **WHL Statistical Leaders**
#' @description WHL statistical leaders for a season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player entry.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_leaders()) }
whl_leaders <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_leaders("whl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "WHL Leaders from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL leaders unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game summary
# ---------------------------------------------------------------------------

#' @title **WHL Game Summary**
#' @description WHL game summary -- named list of data frames
#'   (details, scoring, penalties, shots_by_period, three_stars).
#' @param game_id Numeric WHL game identifier.
#' @return A named list of data frames.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_game_summary(game_id = 27225)) }
whl_game_summary <- function(game_id) {
  out <- NULL
  tryCatch(
    expr = {
      out <- .hockeytech_game_summary("whl", game_id = game_id)
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL game summary for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game shifts
# ---------------------------------------------------------------------------

#' @title **WHL Game Shifts**
#' @description All player shift stints for a WHL game (one row per stint).
#' @param game_id Numeric WHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per shift stint.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_game_shifts(game_id = 27225)) }
whl_game_shifts <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_shifts("whl", game_id = game_id)
      out <- make_fastRhockey_data(out, "WHL Game Shifts from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL game shifts for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player TOI
# ---------------------------------------------------------------------------

#' @title **WHL Player Time On Ice**
#' @description Per-player time-on-ice totals for a WHL game.
#' @param game_id Numeric WHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_player_toi(game_id = 27225)) }
whl_player_toi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_toi("whl", game_id = game_id)
      out <- make_fastRhockey_data(out, "WHL Player TOI from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL player TOI for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game Corsi
# ---------------------------------------------------------------------------

#' @title **WHL Game Corsi/Fenwick (player-level on-ice)**
#' @description Player-level on-ice Corsi and Fenwick for a WHL game.
#' @param game_id Numeric WHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family WHL Functions
#' @examples
#' \donttest{ try(whl_game_corsi(game_id = 27225)) }
whl_game_corsi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_corsi("whl", game_id = game_id)
      out <- make_fastRhockey_data(out, "WHL Game Corsi from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: WHL game Corsi for game_id {game_id} unavailable! {e}")
  )
  out
}
