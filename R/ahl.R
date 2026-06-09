# AHL thin public wrappers over the league-generic HockeyTech core.
# Each function delegates to the corresponding .hockeytech_*("ahl", ...) core
# and applies make_fastRhockey_data() for data-frame returns.

# ---------------------------------------------------------------------------
# Overview alias
# ---------------------------------------------------------------------------

#' @name ahl
#' @title **AHL (American Hockey League) Endpoint Overview**
#' @description Thin exported wrappers around the HockeyTech feeds for the AHL.
#' @keywords AHL
#' @family AHL Functions
NULL

# ---------------------------------------------------------------------------
# Season helpers
# ---------------------------------------------------------------------------

#' @title **AHL Season IDs**
#' @description All seasons for the AHL with end-year and game-type labels,
#'   from the HockeyTech modulekit/seasons feed.
#' @return A `fastRhockey_data` data frame with columns: season_id, season_name,
#'   season_short, career, playoff, start_date, end_date, season_yr,
#'   game_type_label.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_season_id()) }
ahl_season_id <- function() {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_season_id_df("ahl")
      out <- make_fastRhockey_data(out, "AHL Season IDs from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL season IDs unavailable! {e}")
  )
  out
}

#' @title **Most Recent AHL Season**
#' @description Returns the most-recent AHL season as an end-year integer
#'   (e.g. 2025L).
#' @return Integer end-year.
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(most_recent_ahl_season()) }
most_recent_ahl_season <- function() {
  out <- 2026L
  tryCatch(
    expr = { out <- .hockeytech_most_recent_season("ahl") },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL most-recent season unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Schedule
# ---------------------------------------------------------------------------

#' @title **AHL Schedule**
#' @description AHL schedule from the HockeyTech feed (one row per game).
#' @param season End-year season (e.g. 2025); optional.
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per game.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_schedule()) }
ahl_schedule <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_schedule("ahl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "AHL Schedule from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL schedule unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Play-by-play
# ---------------------------------------------------------------------------

#' @title **AHL Play-by-Play**
#' @description AHL play-by-play for a single game (one row per event, fully
#'   enriched with coordinate transforms, on-ice players, Corsi geometry).
#' @param game_id Numeric AHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per play-by-play event.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_pbp(game_id = 1000093924)) }
ahl_pbp <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_pbp("ahl", game_id = game_id)
      out <- make_fastRhockey_data(out, "AHL Play-by-Play from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL PBP for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Standings
# ---------------------------------------------------------------------------

#' @title **AHL Standings**
#' @description AHL standings from the HockeyTech feed (one row per team).
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_standings()) }
ahl_standings <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_standings("ahl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "AHL Standings from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL standings unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Teams
# ---------------------------------------------------------------------------

#' @title **AHL Teams**
#' @description AHL teams for a given season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_teams()) }
ahl_teams <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_teams("ahl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "AHL Teams from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL teams unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Team roster
# ---------------------------------------------------------------------------

#' @title **AHL Team Roster**
#' @description AHL roster for a given team and season from the HockeyTech feed.
#' @param team_id Numeric or character AHL team identifier.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_team_roster(team_id = 341)) }
ahl_team_roster <- function(team_id, season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_team_roster("ahl", team_id = team_id, season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "AHL Team Roster from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL team roster for team_id {team_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player stats
# ---------------------------------------------------------------------------

#' @title **AHL Player Stats**
#' @description AHL player season stats across all seasons.
#' @param player_id Numeric or character AHL player identifier.
#' @return A `fastRhockey_data` data frame, one row per season-stat entry.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_player_stats(player_id = 10781)) }
ahl_player_stats <- function(player_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_stats("ahl", player_id = player_id)
      out <- make_fastRhockey_data(out, "AHL Player Stats from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL player stats for player_id {player_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Leaders
# ---------------------------------------------------------------------------

#' @title **AHL Statistical Leaders**
#' @description AHL statistical leaders for a season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player entry.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_leaders()) }
ahl_leaders <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_leaders("ahl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "AHL Leaders from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL leaders unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game summary
# ---------------------------------------------------------------------------

#' @title **AHL Game Summary**
#' @description AHL game summary -- named list of data frames
#'   (details, scoring, penalties, shots_by_period, three_stars).
#' @param game_id Numeric AHL game identifier.
#' @return A named list of data frames.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_game_summary(game_id = 1000093924)) }
ahl_game_summary <- function(game_id) {
  out <- NULL
  tryCatch(
    expr = {
      out <- .hockeytech_game_summary("ahl", game_id = game_id)
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL game summary for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game shifts
# ---------------------------------------------------------------------------

#' @title **AHL Game Shifts**
#' @description All player shift stints for an AHL game (one row per stint).
#' @param game_id Numeric AHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per shift stint.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_game_shifts(game_id = 1000093924)) }
ahl_game_shifts <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_shifts("ahl", game_id = game_id)
      out <- make_fastRhockey_data(out, "AHL Game Shifts from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL game shifts for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player TOI
# ---------------------------------------------------------------------------

#' @title **AHL Player Time On Ice**
#' @description Per-player time-on-ice totals for an AHL game.
#' @param game_id Numeric AHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_player_toi(game_id = 1000093924)) }
ahl_player_toi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_toi("ahl", game_id = game_id)
      out <- make_fastRhockey_data(out, "AHL Player TOI from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL player TOI for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game Corsi
# ---------------------------------------------------------------------------

#' @title **AHL Game Corsi/Fenwick (player-level on-ice)**
#' @description Player-level on-ice Corsi and Fenwick for an AHL game.
#' @param game_id Numeric AHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family AHL Functions
#' @examples
#' \donttest{ try(ahl_game_corsi(game_id = 1000093924)) }
ahl_game_corsi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_corsi("ahl", game_id = game_id)
      out <- make_fastRhockey_data(out, "AHL Game Corsi from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: AHL game Corsi for game_id {game_id} unavailable! {e}")
  )
  out
}
