# espn_nhl_events.R
# ESPN NHL: core-v2 event + competitor detail wrappers (Tier 5a)
# Endpoints: game, game_competition, game_teams, game_team, game_team_roster,
#            game_team_linescores, game_team_statistics
# All use the core_v2 host:
#   https://sports.core.api.espn.com/v2/sports/hockey/leagues/{league}/events/...
#
# Live verification notes (2026-06-08, event_id="401688263"):
#   game               -> OK  (1-row tibble; season/seasonType/league via $ref)
#   game_competition   -> OK  (1-row tibble; status is a $ref-only sub-object)
#   game_teams         -> OK  (2 rows; items df has inline fields + nested refs)
#   game_team          -> OK  (1-row singular competitor object)
#   game_team_roster   -> OK  (23 rows; entries df: playerId/jersey/athlete/position)
#   game_team_linescores -> OK (3 rows / 3 periods; items has value/displayValue/period)
#   game_team_statistics -> OK (wide 1-row tibble; splits.categories[].stats[] flattened)
#
# cid == event_id confirmed for NHL: competitions[1]$id == event_id for every
# tested game. Default `cid = event_id` is safe.


# ===========================================================================
# Internal helper: ref_of
# ===========================================================================

#' @noRd
.ref_of_evt <- function(x) {
  if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
  else NA_character_
}


# ===========================================================================
# 1. Game (singular event metadata)
# ===========================================================================

#' Internal: ESPN hockey event metadata (league-generic, core-v2)
#' @noRd
.espn_hockey_game <- function(league = "nhl", event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      # competitions is an inline data.frame inside the event (not a $ref collection)
      comps_df  <- raw$competitions
      comp_id   <- NA_character_
      comp_ref  <- NA_character_
      if (is.data.frame(comps_df) && nrow(comps_df) > 0) {
        comp_id  <- as.character(comps_df$id[1]       %||% NA_character_)
        comp_ref <- as.character(comps_df[["$ref"]][1] %||% NA_character_)
      }

      venues_df  <- raw$venues
      venue_ref  <- NA_character_
      if (is.data.frame(venues_df) && "$ref" %in% colnames(venues_df) &&
          nrow(venues_df) > 0) {
        venue_ref <- as.character(venues_df[["$ref"]][1] %||% NA_character_)
      }

      row <- list(
        event_id          = as.character(event_id),
        id                = as.character(raw$id          %||% NA_character_),
        uid               = as.character(raw$uid         %||% NA_character_),
        date              = as.character(raw$date        %||% NA_character_),
        name              = as.character(raw$name        %||% NA_character_),
        short_name        = as.character(raw$shortName   %||% NA_character_),
        time_valid        = as.logical( raw$timeValid    %||% NA),
        competition_id    = comp_id,
        competition_ref   = comp_ref,
        venue_ref         = venue_ref,
        season_ref        = .ref_of_evt(raw$season),
        season_type_ref   = .ref_of_evt(raw$seasonType),
        league_ref        = .ref_of_evt(raw$league),
        event_ref         = as.character(raw[["$ref"]]   %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game data for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game data",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game
NULL
#' @title **Get ESPN NHL Game Metadata (core-v2)**
#' @rdname espn_nhl_game
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id`
#'   to find valid event ids.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with event metadata:
#'
#'    |col_name          |types     |description                                           |
#'    |:-----------------|:---------|:-----------------------------------------------------|
#'    |event_id          |character |ESPN event id (echoed from arg).                      |
#'    |id                |character |ESPN event identifier.                                |
#'    |uid               |character |Event uid string.                                     |
#'    |date              |character |Game date/time (ISO 8601 UTC).                        |
#'    |name              |character |Full game name (e.g. "Chicago Blackhawks at Detroit Red Wings"). |
#'    |short_name        |character |Short name (e.g. "CHI \@ DET").                       |
#'    |time_valid        |logical   |Whether the start time is confirmed.                  |
#'    |competition_id    |character |Id of the primary competition (equals `event_id` for NHL). |
#'    |competition_ref   |character |`$ref` URL for the primary competition object.        |
#'    |venue_ref         |character |`$ref` URL for the venue.                             |
#'    |season_ref        |character |`$ref` URL for the season.                            |
#'    |season_type_ref   |character |`$ref` URL for the season type.                       |
#'    |league_ref        |character |`$ref` URL for the league.                            |
#'    |event_ref         |character |`$ref` URL for this event object.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game <- function(event_id, ...) {
  .espn_hockey_game(league = "nhl", event_id = event_id, ...)
}


# ===========================================================================
# 2. Game Competition (singular competition detail)
# ===========================================================================

#' Internal: ESPN hockey competition detail (league-generic, core-v2)
#' @noRd
.espn_hockey_game_competition <- function(league = "nhl", event_id,
                                          cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      # venue: nested list with $ref
      venue_ref <- NA_character_
      if (is.list(raw$venue) && !is.null(raw$venue[["$ref"]])) {
        venue_ref <- as.character(raw$venue[["$ref"]])
      } else if (is.data.frame(raw$venue) && "$ref" %in% colnames(raw$venue)) {
        venue_ref <- as.character(raw$venue[["$ref"]][1])
      }

      # competitors is an inline data.frame with all competitor rows; grab the
      # collection-level $ref from row 1 only (or build it from the path).
      competitors_ref <- NA_character_
      if (is.data.frame(raw$competitors) && "$ref" %in% colnames(raw$competitors) &&
          nrow(raw$competitors) > 0) {
        competitors_ref <- as.character(raw$competitors[["$ref"]][1])
      } else {
        competitors_ref <- .ref_of_evt(raw$competitors)
      }

      row <- list(
        event_id                  = as.character(event_id),
        cid                       = as.character(cid),
        id                        = as.character(raw$id                       %||% NA_character_),
        guid                      = as.character(raw$guid                     %||% NA_character_),
        uid                       = as.character(raw$uid                      %||% NA_character_),
        date                      = as.character(raw$date                     %||% NA_character_),
        attendance                = as.integer( raw$attendance                %||% NA_integer_),
        neutral_site              = as.logical( raw$neutralSite               %||% NA),
        time_valid                = as.logical( raw$timeValid                 %||% NA),
        boxscore_available        = as.logical( raw$boxscoreAvailable         %||% NA),
        play_by_play_available    = as.logical( raw$playByPlayAvailable       %||% NA),
        linescore_available       = as.logical(
          !is.null(raw$linescoreSource) && is.character(raw$linescoreSource) &&
          nzchar(raw$linescoreSource)),
        on_watch_espn             = as.logical( raw$onWatchESPN               %||% NA),
        recent                    = as.logical( raw$recent                    %||% NA),
        venue_ref                 = venue_ref,
        competitors_ref           = competitors_ref,
        status_ref                = .ref_of_evt(raw$status),
        odds_ref                  = .ref_of_evt(raw$odds),
        officials_ref             = .ref_of_evt(raw$officials),
        competition_ref           = as.character(raw[["$ref"]]                %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Competition data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} competition data for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game competition",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_competition
NULL
#' @title **Get ESPN NHL Game Competition Detail (core-v2)**
#' @rdname espn_nhl_game_competition
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id` (for NHL the single
#'   competition always has the same id as its parent event).
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with competition detail:
#'
#'    |col_name               |types     |description                                             |
#'    |:----------------------|:---------|:-------------------------------------------------------|
#'    |event_id               |character |ESPN event id (echoed from arg).                        |
#'    |cid                    |character |Competition id (echoed from arg).                       |
#'    |id                     |character |ESPN competition identifier.                            |
#'    |guid                   |character |Competition global unique identifier.                   |
#'    |uid                    |character |Competition uid string.                                 |
#'    |date                   |character |Start date/time (ISO 8601 UTC).                         |
#'    |attendance             |integer   |Announced attendance.                                   |
#'    |neutral_site           |logical   |Whether the game was played at a neutral venue.         |
#'    |time_valid             |logical   |Whether the start time is confirmed.                    |
#'    |boxscore_available     |logical   |Whether boxscore data is available.                     |
#'    |play_by_play_available |logical   |Whether play-by-play data is available.                 |
#'    |linescore_available    |logical   |Whether linescore data is available.                    |
#'    |on_watch_espn          |logical   |Whether the game is/was on WatchESPN.                   |
#'    |recent                 |logical   |Whether the game is recent.                             |
#'    |venue_ref              |character |`$ref` URL for the venue.                               |
#'    |competitors_ref        |character |`$ref` URL for the competitors collection.              |
#'    |status_ref             |character |`$ref` URL for the competition status object.           |
#'    |odds_ref               |character |`$ref` URL for the odds collection.                     |
#'    |officials_ref          |character |`$ref` URL for the officials collection.                |
#'    |competition_ref        |character |`$ref` URL for this competition object.                 |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_competition(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_competition <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_competition(league = "nhl", event_id = event_id,
                                cid = cid, ...)
}


# ===========================================================================
# 3. Game Teams (competitors collection — 2 rows)
# ===========================================================================

#' Internal: ESPN hockey game competitors collection (league-generic, core-v2)
#' @noRd
.espn_hockey_game_teams <- function(league = "nhl", event_id,
                                    cid = event_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid, "/competitors")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No competitors returned for event {event_id}.")
        return(result)
      }

      # items is an inline data.frame with columns:
      # $ref, id, uid, type, order, homeAway, winner, team, score, linescores,
      # roster, statistics, leaders, record, probables, curatedRank
      n <- nrow(items)

      .safe_char <- function(x, i) {
        v <- x[[i]]
        if (is.null(v)) return(NA_character_)
        as.character(v)
      }
      .safe_lgl <- function(x, i) {
        v <- x[[i]]
        if (is.null(v)) return(NA)
        as.logical(v)
      }
      .safe_int <- function(x, i) {
        v <- x[[i]]
        if (is.null(v)) return(NA_integer_)
        as.integer(v)
      }
      .ref_col <- function(df_col) {
        if (is.data.frame(df_col) && "$ref" %in% colnames(df_col)) {
          as.character(df_col[["$ref"]])
        } else {
          rep(NA_character_, n)
        }
      }

      rows <- lapply(seq_len(n), function(i) {
        list(
          event_id      = as.character(event_id),
          cid           = as.character(cid),
          team_id       = .safe_char(items, "id")[i],
          uid           = .safe_char(items, "uid")[i],
          type          = .safe_char(items, "type")[i],
          order         = .safe_int(items, "order")[i],
          home_away     = .safe_char(items, "homeAway")[i],
          winner        = .safe_lgl(items, "winner")[i],
          team_ref      = .ref_col(items$team)[i],
          score_ref     = .ref_col(items$score)[i],
          linescores_ref = .ref_col(items$linescores)[i],
          roster_ref    = .ref_col(items$roster)[i],
          statistics_ref = .ref_col(items$statistics)[i],
          leaders_ref   = .ref_col(items$leaders)[i],
          record_ref    = .ref_col(items$record)[i],
          competitor_ref = .safe_char(items, "$ref")[i]
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Teams data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game teams data for event {event_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game teams data",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_teams
NULL
#' @title **Get ESPN NHL Game Teams (Competitors Collection, core-v2)**
#' @rdname espn_nhl_game_teams
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per competitor (2 rows for
#'   a regular NHL game):
#'
#'    |col_name        |types     |description                                              |
#'    |:---------------|:---------|:--------------------------------------------------------|
#'    |event_id        |character |ESPN event id (echoed from arg).                         |
#'    |cid             |character |Competition id (echoed from arg).                        |
#'    |team_id         |character |ESPN competitor/team id.                                 |
#'    |uid             |character |Competitor uid string.                                   |
#'    |type            |character |Competitor type (e.g. "team").                           |
#'    |order           |integer   |Competitor display order.                                |
#'    |home_away       |character |`"home"` or `"away"`.                                    |
#'    |winner          |logical   |Whether this competitor won the game.                    |
#'    |team_ref        |character |`$ref` URL for the team-in-season object.                |
#'    |score_ref       |character |`$ref` URL for the score object.                         |
#'    |linescores_ref  |character |`$ref` URL for the per-period linescores.                |
#'    |roster_ref      |character |`$ref` URL for the game roster.                          |
#'    |statistics_ref  |character |`$ref` URL for the team game statistics.                 |
#'    |leaders_ref     |character |`$ref` URL for the team game leaders.                    |
#'    |record_ref      |character |`$ref` URL for the team record at the time of the game.  |
#'    |competitor_ref  |character |`$ref` URL for this competitor object.                   |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_nhl_scoreboard(dates = "20250110")
#'     espn_nhl_game_teams(event_id = sb$game_id[1])
#'   })
#' }
espn_nhl_game_teams <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_teams(league = "nhl", event_id = event_id, cid = cid, ...)
}


# ===========================================================================
# 4. Game Team (singular competitor detail)
# ===========================================================================

#' Internal: ESPN hockey single competitor detail (league-generic, core-v2)
#' @noRd
.espn_hockey_game_team <- function(league = "nhl", event_id, cid = event_id,
                                   team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/competitors/", team_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      row <- list(
        event_id       = as.character(event_id),
        cid            = as.character(cid),
        team_id        = as.character(team_id),
        id             = as.character(raw$id          %||% NA_character_),
        uid            = as.character(raw$uid         %||% NA_character_),
        type           = as.character(raw$type        %||% NA_character_),
        order          = as.integer( raw$order        %||% NA_integer_),
        home_away      = as.character(raw$homeAway    %||% NA_character_),
        winner         = as.logical( raw$winner       %||% NA),
        team_ref       = .ref_of_evt(raw$team),
        score_ref      = .ref_of_evt(raw$score),
        linescores_ref = .ref_of_evt(raw$linescores),
        roster_ref     = .ref_of_evt(raw$roster),
        statistics_ref = .ref_of_evt(raw$statistics),
        leaders_ref    = .ref_of_evt(raw$leaders),
        record_ref     = .ref_of_evt(raw$record),
        competitor_ref = as.character(raw[["$ref"]]   %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Team data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game team data for event {event_id} / team {team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game team data",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_team
NULL
#' @title **Get ESPN NHL Game Team (Competitor Detail, core-v2)**
#' @rdname espn_nhl_game_team
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier. Use `espn_nhl_game_teams()`
#'   to discover valid team ids for a game.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with competitor detail:
#'
#'    |col_name        |types     |description                                              |
#'    |:---------------|:---------|:--------------------------------------------------------|
#'    |event_id        |character |ESPN event id (echoed from arg).                         |
#'    |cid             |character |Competition id (echoed from arg).                        |
#'    |team_id         |character |ESPN team id (echoed from arg).                          |
#'    |id              |character |ESPN competitor identifier.                              |
#'    |uid             |character |Competitor uid string.                                   |
#'    |type            |character |Competitor type (e.g. "team").                           |
#'    |order           |integer   |Competitor display order.                                |
#'    |home_away       |character |`"home"` or `"away"`.                                    |
#'    |winner          |logical   |Whether this competitor won the game.                    |
#'    |team_ref        |character |`$ref` URL for the team-in-season object.                |
#'    |score_ref       |character |`$ref` URL for the score object.                         |
#'    |linescores_ref  |character |`$ref` URL for the per-period linescores.                |
#'    |roster_ref      |character |`$ref` URL for the game roster.                          |
#'    |statistics_ref  |character |`$ref` URL for the team game statistics.                 |
#'    |leaders_ref     |character |`$ref` URL for the team game leaders.                    |
#'    |record_ref      |character |`$ref` URL for the team record at the time of the game.  |
#'    |competitor_ref  |character |`$ref` URL for this competitor object.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_nhl_game_teams(event_id = eid)
#'     espn_nhl_game_team(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_game_team <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team(league = "nhl", event_id = event_id, cid = cid,
                         team_id = team_id, ...)
}


# ===========================================================================
# 5. Game Team Roster (entries collection)
# ===========================================================================

#' Internal: ESPN hockey game team roster (league-generic, core-v2)
#' @noRd
.espn_hockey_game_team_roster <- function(league = "nhl", event_id,
                                          cid = event_id, team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/competitors/", team_id, "/roster")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      entries <- raw$entries
      if (is.null(entries) || (is.data.frame(entries) && nrow(entries) == 0) ||
          (is.list(entries) && length(entries) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No roster entries returned for event {event_id} / team {team_id}.")
        return(result)
      }

      # entries df columns: playerId, jersey, athlete ($ref col), position ($ref col),
      #   statistics ($ref col), displayName, scratched, scratchReason
      n <- nrow(entries)

      athlete_ref  <- rep(NA_character_, n)
      position_ref <- rep(NA_character_, n)
      stats_ref    <- rep(NA_character_, n)
      if (is.data.frame(entries$athlete) && "$ref" %in% colnames(entries$athlete)) {
        athlete_ref <- as.character(entries$athlete[["$ref"]])
      }
      if (is.data.frame(entries$position) && "$ref" %in% colnames(entries$position)) {
        position_ref <- as.character(entries$position[["$ref"]])
      }
      if (is.data.frame(entries$statistics) && "$ref" %in% colnames(entries$statistics)) {
        stats_ref <- as.character(entries$statistics[["$ref"]])
      }

      athlete_id <- vapply(athlete_ref, .espn_ref_id, character(1),
                           USE.NAMES = FALSE)
      position_id <- vapply(position_ref, .espn_ref_id, character(1),
                            USE.NAMES = FALSE)

      tbl <- data.frame(
        event_id        = as.character(event_id),
        cid             = as.character(cid),
        team_id         = as.character(team_id),
        player_id       = as.character(entries$playerId   %||% NA_character_),
        jersey          = as.character(entries$jersey     %||% NA_character_),
        display_name    = as.character(entries$displayName %||% NA_character_),
        scratched       = as.logical( entries$scratched   %||% NA),
        scratch_reason  = as.character(entries$scratchReason %||% NA_character_),
        athlete_id      = athlete_id,
        athlete_ref     = athlete_ref,
        position_id     = position_id,
        position_ref    = position_ref,
        statistics_ref  = stats_ref,
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Team Roster data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game team roster for event {event_id} / team {team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game team roster",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_team_roster
NULL
#' @title **Get ESPN NHL Game Team Roster (core-v2)**
#' @rdname espn_nhl_game_team_roster
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per rostered player (typically
#'   ~23 skaters + goalies per team):
#'
#'    |col_name       |types     |description                                               |
#'    |:--------------|:---------|:---------------------------------------------------------|
#'    |event_id       |character |ESPN event id (echoed from arg).                          |
#'    |cid            |character |Competition id (echoed from arg).                         |
#'    |team_id        |character |ESPN team id (echoed from arg).                           |
#'    |player_id      |character |ESPN player/athlete identifier.                           |
#'    |jersey         |character |Player jersey number.                                     |
#'    |display_name   |character |Player display name.                                      |
#'    |scratched      |logical   |Whether the player was a healthy scratch.                 |
#'    |scratch_reason |character |Reason for scratch (if applicable).                       |
#'    |athlete_id     |character |Athlete id parsed from the `$ref` URL.                    |
#'    |athlete_ref    |character |`$ref` URL for the athlete-in-season object.              |
#'    |position_id    |character |Position id parsed from the `$ref` URL.                   |
#'    |position_ref   |character |`$ref` URL for the player's position.                     |
#'    |statistics_ref |character |`$ref` URL for the player's game statistics.              |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_nhl_game_teams(event_id = eid)
#'     espn_nhl_game_team_roster(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_game_team_roster <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_roster(league = "nhl", event_id = event_id, cid = cid,
                                team_id = team_id, ...)
}


# ===========================================================================
# 6. Game Team Linescores (per-period scores)
# ===========================================================================

#' Internal: ESPN hockey game team linescores (league-generic, core-v2)
#' @noRd
.espn_hockey_game_team_linescores <- function(league = "nhl", event_id,
                                              cid = event_id, team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/competitors/", team_id, "/linescores")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No linescore data for event {event_id} / team {team_id}.")
        return(result)
      }

      # items columns: $ref, value, displayValue, source (nested df), period
      n <- nrow(items)

      source_id   <- rep(NA_character_, n)
      source_desc <- rep(NA_character_, n)
      if (is.data.frame(items$source)) {
        if ("id" %in% colnames(items$source))
          source_id <- as.character(items$source$id)
        if ("description" %in% colnames(items$source))
          source_desc <- as.character(items$source$description)
      }

      tbl <- data.frame(
        event_id      = as.character(event_id),
        cid           = as.character(cid),
        team_id       = as.character(team_id),
        period        = as.integer( items$period        %||% NA_integer_),
        value         = as.numeric( items$value         %||% NA_real_),
        display_value = as.character(items$displayValue %||% NA_character_),
        source_id     = source_id,
        source_desc   = source_desc,
        linescore_ref = as.character(items[["$ref"]]    %||% NA_character_),
        stringsAsFactors = FALSE
      )

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Team Linescores data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} linescores for event {event_id} / team {team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game team linescores",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_team_linescores
NULL
#' @title **Get ESPN NHL Game Team Linescores (core-v2)**
#' @rdname espn_nhl_game_team_linescores
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per period (3 rows for a
#'   regulation game, up to 4+ in overtime):
#'
#'    |col_name      |types     |description                                             |
#'    |:-------------|:---------|:-------------------------------------------------------|
#'    |event_id      |character |ESPN event id (echoed from arg).                        |
#'    |cid           |character |Competition id (echoed from arg).                       |
#'    |team_id       |character |ESPN team id (echoed from arg).                         |
#'    |period        |integer   |Period number (1 = 1st, 2 = 2nd, 3 = 3rd, 4+ = OT).    |
#'    |value         |numeric   |Goals scored in this period.                            |
#'    |display_value |character |Display string for goals scored.                        |
#'    |source_id     |character |Data source identifier.                                 |
#'    |source_desc   |character |Data source description (e.g. "feed").                  |
#'    |linescore_ref |character |`$ref` URL for this period linescore object.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_nhl_game_teams(event_id = eid)
#'     espn_nhl_game_team_linescores(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_game_team_linescores <- function(event_id, cid = event_id,
                                          team_id, ...) {
  .espn_hockey_game_team_linescores(league = "nhl", event_id = event_id,
                                    cid = cid, team_id = team_id, ...)
}


# ===========================================================================
# 7. Game Team Statistics (wide flat tibble from splits.categories[].stats[])
# ===========================================================================

#' Internal: ESPN hockey game team statistics (league-generic, core-v2)
#' @noRd
.espn_hockey_game_team_statistics <- function(league = "nhl", event_id,
                                              cid = event_id, team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("events/", event_id, "/competitions/", cid,
                     "/competitors/", team_id, "/statistics")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      splits <- raw$splits
      if (is.null(splits)) {
        cli::cli_alert_warning("{Sys.time()}: No statistics splits returned for event {event_id} / team {team_id}.")
        return(result)
      }

      cats <- splits$categories
      if (is.null(cats) || (is.data.frame(cats) && nrow(cats) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No stat categories returned for event {event_id} / team {team_id}.")
        return(result)
      }

      # Flatten each category's stats[] into name=value pairs, then bind wide
      wide <- list(
        event_id = as.character(event_id),
        cid      = as.character(cid),
        team_id  = as.character(team_id)
      )

      for (i in seq_len(nrow(cats))) {
        cat_row  <- cats[i, , drop = FALSE]
        cat_abbr <- as.character(cat_row$abbreviation %||% paste0("cat", i))
        st <- if (is.list(cat_row$stats) && length(cat_row$stats) > 0)
          cat_row$stats[[1]] else NULL
        if (is.null(st) || !is.data.frame(st) || nrow(st) == 0) next

        for (j in seq_len(nrow(st))) {
          stat_name <- as.character(st$name[j] %||% paste0("stat", j))
          col_key   <- janitor::make_clean_names(
            paste0(cat_abbr, "_", stat_name))
          wide[[col_key]] <- as.character(st$displayValue[j] %||% NA_character_)
          wide[[paste0(col_key, "_value")]] <- as.numeric(st$value[j] %||% NA_real_)
        }
      }

      result <- as.data.frame(wide, stringsAsFactors = FALSE) %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Game Team Statistics data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} game team statistics for event {event_id} / team {team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} game team statistics",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_game_team_statistics
NULL
#' @title **Get ESPN NHL Game Team Statistics (core-v2)**
#' @rdname espn_nhl_game_team_statistics
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @return A wide one-row `fastRhockey_data` tibble. The first three columns are
#'   always present; the remaining stat columns are named `<category>_<stat>`
#'   (display value) and `<category>_<stat>_value` (numeric value) for each stat
#'   in each category. For a typical completed NHL game there are 5 stat
#'   categories (defensive, general, offensive, specialTeams, goalie) producing
#'   ~30 column pairs:
#'
#'    |col_name                 |types     |description                                     |
#'    |:------------------------|:---------|:-----------------------------------------------|
#'    |event_id                 |character |ESPN event id (echoed from arg).                |
#'    |cid                      |character |Competition id (echoed from arg).               |
#'    |team_id                  |character |ESPN team id (echoed from arg).                 |
#'    |category_stat            |character |Display value for a stat within a category (e.g. offensive_goals).      |
#'    |category_stat_value      |numeric   |Numeric value for a stat within a category (e.g. offensive_goals_value).|
#'
#' @importFrom janitor make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_nhl_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_nhl_game_teams(event_id = eid)
#'     espn_nhl_game_team_statistics(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_game_team_statistics <- function(event_id, cid = event_id,
                                          team_id, ...) {
  .espn_hockey_game_team_statistics(league = "nhl", event_id = event_id,
                                    cid = cid, team_id = team_id, ...)
}
