# QMJHL thin public wrappers over the league-generic HockeyTech core.
# Each function delegates to the corresponding .hockeytech_*("qmjhl", ...) core
# and applies make_fastRhockey_data() for data-frame returns.

# ---------------------------------------------------------------------------
# Overview alias
# ---------------------------------------------------------------------------

#' @name qmjhl
#' @title **QMJHL (Quebec Maritimes Junior Hockey League) Endpoint Overview**
#' @description Thin exported wrappers around the HockeyTech feeds for the QMJHL.
#' @keywords QMJHL
#' @family QMJHL Functions
NULL

# ---------------------------------------------------------------------------
# Season helpers
# ---------------------------------------------------------------------------

#' @title **QMJHL Season IDs**
#' @description All seasons for the QMJHL with end-year and game-type labels,
#'   from the HockeyTech modulekit/seasons feed.
#' @return A `fastRhockey_data` data frame with columns: season_id, season_name,
#'   season_short, career, playoff, start_date, end_date, season_yr,
#'   game_type_label.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_season_id()) }
qmjhl_season_id <- function() {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_season_id_df("qmjhl")
      out <- make_fastRhockey_data(out, "QMJHL Season IDs from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL season IDs unavailable! {e}")
  )
  out
}

#' @title **Most Recent QMJHL Season**
#' @description Returns the most-recent QMJHL season as an end-year integer
#'   (e.g. 2025L).
#' @return Integer end-year.
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(most_recent_qmjhl_season()) }
most_recent_qmjhl_season <- function() {
  out <- 2026L
  tryCatch(
    expr = { out <- .hockeytech_most_recent_season("qmjhl") },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL most-recent season unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Schedule
# ---------------------------------------------------------------------------

#' @title **QMJHL Schedule**
#' @description QMJHL schedule from the HockeyTech feed (one row per game).
#' @param season End-year season (e.g. 2025); optional.
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per game.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_schedule()) }
qmjhl_schedule <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_schedule("qmjhl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "QMJHL Schedule from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL schedule unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Play-by-play
# ---------------------------------------------------------------------------

#' @title **QMJHL Play-by-Play**
#' @description QMJHL play-by-play for a single game (one row per event, fully
#'   enriched with coordinate transforms, on-ice players, Corsi geometry).
#' @param game_id Numeric QMJHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per play-by-play event.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_pbp(game_id = 27225)) }
qmjhl_pbp <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_pbp("qmjhl", game_id = game_id)
      out <- make_fastRhockey_data(out, "QMJHL Play-by-Play from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL PBP for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Standings
# ---------------------------------------------------------------------------

#' @title **QMJHL Standings**
#' @description QMJHL standings from the HockeyTech feed (one row per team).
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_standings()) }
qmjhl_standings <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_standings("qmjhl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "QMJHL Standings from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL standings unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Teams
# ---------------------------------------------------------------------------

#' @title **QMJHL Teams**
#' @description QMJHL teams for a given season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_teams()) }
qmjhl_teams <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_teams("qmjhl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "QMJHL Teams from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL teams unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Team roster
# ---------------------------------------------------------------------------

#' @title **QMJHL Team Roster**
#' @description QMJHL roster for a given team and season from the HockeyTech feed.
#' @param team_id Numeric or character QMJHL team identifier.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_team_roster(team_id = 1)) }
qmjhl_team_roster <- function(team_id, season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_team_roster("qmjhl", team_id = team_id, season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "QMJHL Team Roster from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL team roster for team_id {team_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player stats
# ---------------------------------------------------------------------------

#' @title **QMJHL Player Stats**
#' @description QMJHL player season stats across all seasons.
#' @param player_id Numeric or character QMJHL player identifier.
#' @return A `fastRhockey_data` data frame, one row per season-stat entry.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_player_stats(player_id = 1)) }
qmjhl_player_stats <- function(player_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_stats("qmjhl", player_id = player_id)
      out <- make_fastRhockey_data(out, "QMJHL Player Stats from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL player stats for player_id {player_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Leaders
# ---------------------------------------------------------------------------

#' @title **QMJHL Statistical Leaders**
#' @description QMJHL statistical leaders for a season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player entry.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_leaders()) }
qmjhl_leaders <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_leaders("qmjhl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "QMJHL Leaders from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL leaders unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game summary
# ---------------------------------------------------------------------------

#' @title **QMJHL Game Summary**
#' @description QMJHL game summary -- named list of data frames
#'   (details, scoring, penalties, shots_by_period, three_stars).
#' @param game_id Numeric QMJHL game identifier.
#' @return A named list of data frames.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_game_summary(game_id = 27225)) }
qmjhl_game_summary <- function(game_id) {
  out <- NULL
  tryCatch(
    expr = {
      out <- .hockeytech_game_summary("qmjhl", game_id = game_id)
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL game summary for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game shifts
# ---------------------------------------------------------------------------

#' @title **QMJHL Game Shifts**
#' @description All player shift stints for a QMJHL game (one row per stint).
#' @param game_id Numeric QMJHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per shift stint.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_game_shifts(game_id = 27225)) }
qmjhl_game_shifts <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_shifts("qmjhl", game_id = game_id)
      out <- make_fastRhockey_data(out, "QMJHL Game Shifts from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL game shifts for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player TOI
# ---------------------------------------------------------------------------

#' @title **QMJHL Player Time On Ice**
#' @description Per-player time-on-ice totals for a QMJHL game.
#' @param game_id Numeric QMJHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_player_toi(game_id = 27225)) }
qmjhl_player_toi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_toi("qmjhl", game_id = game_id)
      out <- make_fastRhockey_data(out, "QMJHL Player TOI from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL player TOI for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game Corsi
# ---------------------------------------------------------------------------

#' @title **QMJHL Game Corsi/Fenwick (player-level on-ice)**
#' @description Player-level on-ice Corsi and Fenwick for a QMJHL game.
#' @param game_id Numeric QMJHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family QMJHL Functions
#' @examples
#' \donttest{ try(qmjhl_game_corsi(game_id = 27225)) }
qmjhl_game_corsi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_corsi("qmjhl", game_id = game_id)
      out <- make_fastRhockey_data(out, "QMJHL Game Corsi from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: QMJHL game Corsi for game_id {game_id} unavailable! {e}")
  )
  out
}
