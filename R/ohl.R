# OHL thin public wrappers over the league-generic HockeyTech core.
# Each function delegates to the corresponding .hockeytech_*("ohl", ...) core
# and applies make_fastRhockey_data() for data-frame returns.

# ---------------------------------------------------------------------------
# Overview alias
# ---------------------------------------------------------------------------

#' @name ohl
#' @title **OHL (Ontario Hockey League) Endpoint Overview**
#' @description Thin exported wrappers around the HockeyTech feeds for the OHL.
#' @keywords OHL
#' @family OHL Functions
NULL

# ---------------------------------------------------------------------------
# Season helpers
# ---------------------------------------------------------------------------

#' @title **OHL Season IDs**
#' @description All seasons for the OHL with end-year and game-type labels,
#'   from the HockeyTech modulekit/seasons feed.
#' @return A `fastRhockey_data` data frame with columns: season_id, season_name,
#'   season_short, career, playoff, start_date, end_date, season_yr,
#'   game_type_label.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_season_id()) }
ohl_season_id <- function() {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_season_id_df("ohl")
      out <- make_fastRhockey_data(out, "OHL Season IDs from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL season IDs unavailable! {e}")
  )
  out
}

#' @title **Most Recent OHL Season**
#' @description Returns the most-recent OHL season as an end-year integer
#'   (e.g. 2025L).
#' @return Integer end-year.
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(most_recent_ohl_season()) }
most_recent_ohl_season <- function() {
  out <- 2026L
  tryCatch(
    expr = { out <- .hockeytech_most_recent_season("ohl") },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL most-recent season unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Schedule
# ---------------------------------------------------------------------------

#' @title **OHL Schedule**
#' @description OHL schedule from the HockeyTech feed (one row per game).
#' @param season End-year season (e.g. 2025); optional.
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per game.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_schedule()) }
ohl_schedule <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_schedule("ohl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "OHL Schedule from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL schedule unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Play-by-play
# ---------------------------------------------------------------------------

#' @title **OHL Play-by-Play**
#' @description OHL play-by-play for a single game (one row per event, fully
#'   enriched with coordinate transforms, on-ice players, Corsi geometry).
#' @param game_id Numeric OHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per play-by-play event.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_pbp(game_id = 27225)) }
ohl_pbp <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_pbp("ohl", game_id = game_id)
      out <- make_fastRhockey_data(out, "OHL Play-by-Play from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL PBP for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Standings
# ---------------------------------------------------------------------------

#' @title **OHL Standings**
#' @description OHL standings from the HockeyTech feed (one row per team).
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_standings()) }
ohl_standings <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_standings("ohl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "OHL Standings from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL standings unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Teams
# ---------------------------------------------------------------------------

#' @title **OHL Teams**
#' @description OHL teams for a given season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per team.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_teams()) }
ohl_teams <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_teams("ohl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "OHL Teams from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL teams unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Team roster
# ---------------------------------------------------------------------------

#' @title **OHL Team Roster**
#' @description OHL roster for a given team and season from the HockeyTech feed.
#' @param team_id Numeric or character OHL team identifier.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_team_roster(team_id = 1)) }
ohl_team_roster <- function(team_id, season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_team_roster("ohl", team_id = team_id, season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "OHL Team Roster from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL team roster for team_id {team_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player stats
# ---------------------------------------------------------------------------

#' @title **OHL Player Stats**
#' @description OHL player season stats across all seasons.
#' @param player_id Numeric or character OHL player identifier.
#' @return A `fastRhockey_data` data frame, one row per season-stat entry.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_player_stats(player_id = 1)) }
ohl_player_stats <- function(player_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_stats("ohl", player_id = player_id)
      out <- make_fastRhockey_data(out, "OHL Player Stats from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL player stats for player_id {player_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Leaders
# ---------------------------------------------------------------------------

#' @title **OHL Statistical Leaders**
#' @description OHL statistical leaders for a season from the HockeyTech feed.
#' @param season End-year season (e.g. 2025); optional (defaults to most-recent).
#' @param season_id Explicit HockeyTech season id; optional.
#' @return A `fastRhockey_data` data frame, one row per player entry.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_leaders()) }
ohl_leaders <- function(season = NULL, season_id = NULL) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_leaders("ohl", season = season, season_id = season_id)
      out <- make_fastRhockey_data(out, "OHL Leaders from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL leaders unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game summary
# ---------------------------------------------------------------------------

#' @title **OHL Game Summary**
#' @description OHL game summary -- named list of data frames
#'   (details, scoring, penalties, shots_by_period, three_stars).
#' @param game_id Numeric OHL game identifier.
#' @return A named list of data frames.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_game_summary(game_id = 27225)) }
ohl_game_summary <- function(game_id) {
  out <- NULL
  tryCatch(
    expr = {
      out <- .hockeytech_game_summary("ohl", game_id = game_id)
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL game summary for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game shifts
# ---------------------------------------------------------------------------

#' @title **OHL Game Shifts**
#' @description All player shift stints for an OHL game (one row per stint).
#' @param game_id Numeric OHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per shift stint.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_game_shifts(game_id = 27225)) }
ohl_game_shifts <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_shifts("ohl", game_id = game_id)
      out <- make_fastRhockey_data(out, "OHL Game Shifts from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL game shifts for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Player TOI
# ---------------------------------------------------------------------------

#' @title **OHL Player Time On Ice**
#' @description Per-player time-on-ice totals for an OHL game.
#' @param game_id Numeric OHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_player_toi(game_id = 27225)) }
ohl_player_toi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_player_toi("ohl", game_id = game_id)
      out <- make_fastRhockey_data(out, "OHL Player TOI from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL player TOI for game_id {game_id} unavailable! {e}")
  )
  out
}

# ---------------------------------------------------------------------------
# Game Corsi
# ---------------------------------------------------------------------------

#' @title **OHL Game Corsi/Fenwick (player-level on-ice)**
#' @description Player-level on-ice Corsi and Fenwick for an OHL game.
#' @param game_id Numeric OHL game identifier.
#' @return A `fastRhockey_data` data frame, one row per player.
#' @import dplyr
#' @export
#' @family OHL Functions
#' @examples
#' \donttest{ try(ohl_game_corsi(game_id = 27225)) }
ohl_game_corsi <- function(game_id) {
  out <- data.frame()
  tryCatch(
    expr = {
      out <- .hockeytech_game_corsi("ohl", game_id = game_id)
      out <- make_fastRhockey_data(out, "OHL Game Corsi from HockeyTech", Sys.time())
    },
    error = function(e) cli::cli_alert_danger("{Sys.time()}: OHL game Corsi for game_id {game_id} unavailable! {e}")
  )
  out
}
