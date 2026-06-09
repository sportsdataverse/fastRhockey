# League-generic HockeyTech core functions.
#
# Mirrors Python sportsdataverse/hockeytech/_family.py::build_family().
# All functions are internal (not exported). Per-league exported wrappers
# (e.g. pwhl_schedule, ahl_schedule) come in B3.1b.
#
# Each .hockeytech_<stem>(league, ...) function takes a league key as its first
# argument and delegates to:
#   - .hockeytech_api(.hockeytech_url(...)) for network access
#   - .parse_hockeytech_*() for parsing
#   - hockeytech_enrich_pbp() for PBP enrichment
#   - hockeytech_player_toi() / hockeytech_corsi_fenwick_on_ice() for analytics


# ---------------------------------------------------------------------------
# Season helpers
# ---------------------------------------------------------------------------

#' @keywords internal
#' All seasons for a league, with end-year and game-type labels.
#'
#' Mirrors Python build_family()::_season_id(). Calls the modulekit/seasons
#' endpoint and parses via .parse_hockeytech_seasons().
#'
#' @param league HockeyTech league key, e.g. "pwhl", "ahl", "ohl".
#' @return A data.frame with columns: season_id, season_name, season_short,
#'   career, playoff, start_date, end_date, season_yr, game_type_label.
#' @noRd
.hockeytech_season_id_df <- function(league) {
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "modulekit", "seasons", list())),
    error = function(e) list()
  )
  .parse_hockeytech_seasons(payload)
}


#' @keywords internal
#' Most-recent season as an end-year integer.
#'
#' Mirrors Python build_family()::_most_recent_season(). Returns the maximum
#' season_yr in the seasons frame, or 2026L as a fallback when no data is
#' available.
#'
#' @param league HockeyTech league key.
#' @return Integer end-year (e.g. 2025L), or 2026L on failure.
#' @noRd
.hockeytech_most_recent_season <- function(league) {
  df <- tryCatch(.hockeytech_season_id_df(league), error = function(e) data.frame())
  if (is.data.frame(df) && nrow(df) > 0 && "season_yr" %in% names(df)) {
    v <- max(df$season_yr, na.rm = TRUE)
    if (is.finite(v)) return(as.integer(v))
  }
  2026L
}


# ---------------------------------------------------------------------------
# Schedule
# ---------------------------------------------------------------------------

#' @keywords internal
#' Schedule for a league -- one row per game.
#'
#' Mirrors Python build_family()::_schedule(). Uses the modulekit/scorebar
#' endpoint with numberofdaysback/ahead = 10000 (full-season dump). If season
#' or season_id is provided the endpoint is filtered server-side.
#'
#' @param league    HockeyTech league key.
#' @param season    Optional end-year integer (e.g. 2025L).
#' @param season_id Optional explicit numeric season_id (short-circuits lookup).
#' @return A data.frame, one row per game.
#' @noRd
.hockeytech_schedule <- function(league, season = NULL, season_id = NULL) {
  cfg <- .hockeytech_leagues()[[league]]
  params <- list(
    numberofdaysback  = 10000,
    numberofdaysahead = 10000,
    limit             = 10000,
    league_id         = cfg$league_id
  )
  if (!is.null(season) || !is.null(season_id)) {
    params[["season_id"]] <- .hockeytech_season_id(
      league, season = season, season_id = season_id
    )
  }
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "modulekit", "scorebar", params)),
    error = function(e) list()
  )
  .parse_hockeytech_schedule(payload)
}


# ---------------------------------------------------------------------------
# PBP (enriched)
# ---------------------------------------------------------------------------

#' @keywords internal
#' Play-by-play for a single game -- one row per event, fully enriched.
#'
#' Mirrors Python build_family()::_pbp(). Fetches and parses the
#' statviewfeed/gameCenterPlayByPlay payload, then enriches via
#' hockeytech_enrich_pbp() (game-meta, coordinate transforms, clock columns,
#' PP/SH back-fill, shot geometry, on-ice players).
#'
#' @param league  HockeyTech league key.
#' @param game_id Numeric game identifier.
#' @return A data.frame, one row per play-by-play event.
#' @noRd
.hockeytech_pbp <- function(league, game_id) {
  cfg <- .hockeytech_leagues()[[league]]
  pbp_payload <- tryCatch(
    .hockeytech_api(
      .hockeytech_url(league, "statviewfeed", "gameCenterPlayByPlay",
                      list(game_id = game_id, league_id = ""))
    ),
    error = function(e) list()
  )
  df <- .parse_hockeytech_pbp(pbp_payload, pbp_style = cfg$pbp_style, game_id = game_id)

  meta_payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "gc", "gamesummary", list(game_id = game_id))),
    error = function(e) NULL
  )
  shifts_payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "modulekit", "gameshifts", list(game_id = game_id))),
    error = function(e) NULL
  )

  hockeytech_enrich_pbp(df, league, game_id,
                         meta_payload   = meta_payload,
                         shifts_payload = shifts_payload)
}


# ---------------------------------------------------------------------------
# Standings
# ---------------------------------------------------------------------------

#' @keywords internal
#' Standings for a league -- one row per team.
#'
#' Mirrors Python build_family()::_standings(). Uses the statviewfeed/teams
#' endpoint with groupTeamsBy="division" and context="overall".
#'
#' @param league    HockeyTech league key.
#' @param season    Optional end-year integer (defaults to most-recent season).
#' @param season_id Optional explicit numeric season_id.
#' @return A data.frame, one row per team.
#' @noRd
.hockeytech_standings <- function(league, season = NULL, season_id = NULL) {
  cfg <- .hockeytech_leagues()[[league]]
  effective_season <- season %||% .hockeytech_most_recent_season(league)
  sid <- .hockeytech_season_id(league, season = effective_season, season_id = season_id)
  params <- list(
    groupTeamsBy = "division",
    context      = "overall",
    special      = "false",
    league_id    = cfg$league_id,
    sort         = "points",
    season       = sid
  )
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "statviewfeed", "teams", params)),
    error = function(e) list()
  )
  .parse_hockeytech_standings(payload)
}


# ---------------------------------------------------------------------------
# Teams
# ---------------------------------------------------------------------------

#' @keywords internal
#' Teams for a league in a given season.
#'
#' Mirrors Python build_family()::_teams(). Uses modulekit/teamsbyseason.
#'
#' @param league    HockeyTech league key.
#' @param season    Optional end-year integer (defaults to most-recent season).
#' @param season_id Optional explicit numeric season_id.
#' @return A data.frame, one row per team.
#' @noRd
.hockeytech_teams <- function(league, season = NULL, season_id = NULL) {
  effective_season <- season %||% .hockeytech_most_recent_season(league)
  sid <- .hockeytech_season_id(league, season = effective_season, season_id = season_id)
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "modulekit", "teamsbyseason", list(season = sid))),
    error = function(e) list()
  )
  .parse_hockeytech_teams(payload)
}


# ---------------------------------------------------------------------------
# Team roster
# ---------------------------------------------------------------------------

#' @keywords internal
#' Team roster for a given team and season.
#'
#' Mirrors Python build_family()::_team_roster(). Uses modulekit/roster.
#'
#' @param league    HockeyTech league key.
#' @param team_id   Numeric or character team identifier.
#' @param season    Optional end-year integer (defaults to most-recent season).
#' @param season_id Optional explicit numeric season_id.
#' @return A data.frame, one row per player.
#' @noRd
.hockeytech_team_roster <- function(league, team_id, season = NULL, season_id = NULL) {
  effective_season <- season %||% .hockeytech_most_recent_season(league)
  sid <- .hockeytech_season_id(league, season = effective_season, season_id = season_id)
  payload <- tryCatch(
    .hockeytech_api(
      .hockeytech_url(league, "modulekit", "roster",
                      list(team_id = team_id, season_id = sid))
    ),
    error = function(e) list()
  )
  .parse_hockeytech_roster(payload)
}


# ---------------------------------------------------------------------------
# Player stats
# ---------------------------------------------------------------------------

#' @keywords internal
#' Player season stats across all seasons.
#'
#' Mirrors Python build_family()::_player_stats(). Uses modulekit/player with
#' category="seasonstats".
#'
#' @param league    HockeyTech league key.
#' @param player_id Numeric or character player identifier.
#' @return A data.frame with one row per season-stat entry.
#' @noRd
.hockeytech_player_stats <- function(league, player_id) {
  payload <- tryCatch(
    .hockeytech_api(
      .hockeytech_url(league, "modulekit", "player",
                      list(player_id = player_id, category = "seasonstats"))
    ),
    error = function(e) list()
  )
  .parse_hockeytech_player_stats(payload)
}


# ---------------------------------------------------------------------------
# Leaders
# ---------------------------------------------------------------------------

#' @keywords internal
#' Statistical leaders for a league in a given season.
#'
#' Mirrors Python build_family()::_leaders(). Uses the statviewfeed/
#' leadersExtended endpoint with playerTypes="skaters" and
#' skaterStatTypes="points,goals".
#'
#' @param league    HockeyTech league key.
#' @param season    Optional end-year integer (defaults to most-recent season).
#' @param season_id Optional explicit numeric season_id.
#' @return A data.frame, one row per player entry.
#' @noRd
.hockeytech_leaders <- function(league, season = NULL, season_id = NULL) {
  effective_season <- season %||% .hockeytech_most_recent_season(league)
  sid <- .hockeytech_season_id(league, season = effective_season, season_id = season_id)
  params <- list(
    season_id       = sid,
    team_id         = 0,
    playerTypes     = "skaters",
    skaterStatTypes = "points,goals",
    activeOnly      = 0
  )
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "statviewfeed", "leadersExtended", params)),
    error = function(e) list()
  )
  .parse_hockeytech_leaders(payload)
}


# ---------------------------------------------------------------------------
# Game summary
# ---------------------------------------------------------------------------

#' @keywords internal
#' Game summary -- named list of data.frames (game/goals/penalties/shots_by_period/three_stars).
#'
#' Mirrors Python build_family()::_game_summary(). Uses gc/gamesummary.
#'
#' @param league  HockeyTech league key.
#' @param game_id Numeric game identifier.
#' @return A named list of data.frames.
#' @noRd
.hockeytech_game_summary <- function(league, game_id) {
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "gc", "gamesummary", list(game_id = game_id))),
    error = function(e) list()
  )
  .parse_hockeytech_game_summary(payload, game_id = game_id)
}


# ---------------------------------------------------------------------------
# Game shifts
# ---------------------------------------------------------------------------

#' @keywords internal
#' Parsed shift stints for a single game.
#'
#' Mirrors Python build_family()::_game_shifts(). Uses modulekit/gameshifts.
#'
#' @param league  HockeyTech league key.
#' @param game_id Numeric game identifier.
#' @return A data.frame, one row per shift stint.
#' @noRd
.hockeytech_game_shifts <- function(league, game_id) {
  payload <- tryCatch(
    .hockeytech_api(.hockeytech_url(league, "modulekit", "gameshifts", list(game_id = game_id))),
    error = function(e) list()
  )
  .parse_hockeytech_shifts(payload, game_id = game_id)
}


# ---------------------------------------------------------------------------
# Player TOI
# ---------------------------------------------------------------------------

#' @keywords internal
#' Per-player time-on-ice totals for a single game.
#'
#' Mirrors Python build_family()::_player_toi(). Calls .hockeytech_game_shifts()
#' then passes the result to hockeytech_player_toi().
#'
#' @param league  HockeyTech league key.
#' @param game_id Numeric game identifier.
#' @return A data.frame with one row per player: player_id, first_name,
#'   last_name, toi_seconds, num_shifts, avg_shift_s.
#' @noRd
.hockeytech_player_toi <- function(league, game_id) {
  shifts <- .hockeytech_game_shifts(league, game_id)
  hockeytech_player_toi(shifts)
}


# ---------------------------------------------------------------------------
# Game Corsi
# ---------------------------------------------------------------------------

#' @keywords internal
#' Player-level on-ice Corsi and Fenwick for a single game.
#'
#' Mirrors Python build_family()::_game_corsi(). Fetches enriched PBP via
#' .hockeytech_pbp(), computes Corsi/Fenwick via hockeytech_corsi_fenwick_on_ice(),
#' joins with TOI from .hockeytech_player_toi(), and adds corsi_for_per60.
#'
#' @param league  HockeyTech league key.
#' @param game_id Numeric game identifier.
#' @return A data.frame, one row per player.
#' @noRd
.hockeytech_game_corsi <- function(league, game_id) {
  pbp   <- .hockeytech_pbp(league, game_id)
  corsi <- hockeytech_corsi_fenwick_on_ice(pbp)

  toi <- .hockeytech_player_toi(league, game_id)

  # Join corsi with toi_seconds for per60 computation
  if (nrow(corsi) > 0 && nrow(toi) > 0) {
    toi_sel <- toi[, c("player_id", "toi_seconds"), drop = FALSE]
    toi_sel$player_id <- as.character(toi_sel$player_id)
    corsi$player_id   <- as.character(corsi$player_id)
    out <- merge(corsi, toi_sel, by = "player_id", all.x = TRUE)
    out$corsi_for_per60 <- ifelse(
      !is.na(out$toi_seconds) & out$toi_seconds > 0,
      hockeytech_per60(out$corsi_for, out$toi_seconds),
      NA_real_
    )
    return(out)
  }

  corsi$corsi_for_per60 <- NA_real_
  corsi
}
