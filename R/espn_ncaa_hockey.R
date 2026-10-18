# espn_ncaa_hockey.R
# ESPN men's (mch) and women's (wch) college hockey: espn_mch_*() / espn_wch_*().
#
# Mirrors sdv-py sportsdataverse/hockey/{mch,wch}/{mch,wch}_espn_ext.py. The wrappers are
# thin public shims over the league-generic ESPN hockey helpers already used by
# espn_nhl_*() (R/espn_nhl_*.R): same hosts, same retry/transport, same
# make_fastRhockey_data() return class. Only the league slug differs:
#   mch = "mens-college-hockey", wch = "womens-college-hockey".
# Seasons are END years (2026 = 2025-26), as in sdv-py and the NHL helpers.
# The shim blocks below are generated from the espn_nhl_* shims; the hand-written
# internals at the end cover the routes sdv-py has and espn_nhl_* does not.

# ===========================================================================
# Shared internals for the routes sdv-py has and espn_nhl_*() does not
# (league-generic; the public espn_mch_*() / espn_wch_*() shims fix the slug).
# ===========================================================================

#' @noRd
.ncaa_hockey_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.list(x)) NA_character_ else as.character(x[[1]])
}
#' @noRd
.ncaa_hockey_int <- function(x) {
  if (is.null(x) || length(x) == 0 || is.list(x)) NA_integer_ else suppressWarnings(as.integer(x[[1]]))
}
#' @noRd
.ncaa_hockey_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.list(x)) NA_real_ else suppressWarnings(as.numeric(x[[1]]))
}

#' Column schema of the Site v2 rankings frame (mirrors sdv-py `_RANKINGS_SCHEMA`).
#' @noRd
.ncaa_hockey_rankings_schema <- function() {
  c(poll_id = "integer", poll_name = "character", poll_short_name = "character",
    poll_type = "character", season = "integer", season_type = "integer",
    week = "integer", week_display = "character", poll_date = "character",
    ranked = "logical", team_id = "character", rank = "integer",
    previous_rank = "integer", points = "numeric", first_place_votes = "integer",
    trend = "character", record_summary = "character", team_uid = "character",
    team_location = "character", team_name = "character", team_nickname = "character",
    team_abbreviation = "character", team_color = "character", team_logo = "character",
    last_updated = "character")
}

#' Internal: ESPN hockey Site v2 rankings (league-generic)
#'
#' One row per (poll, team): the ranked teams (`ranked = TRUE`) and the teams
#' receiving votes (`ranked = FALSE`). Mirrors sdv-py `parse_rankings()`.
#' @noRd
.espn_hockey_rankings <- function(league, ...) {
  .args <- .capture_args()
  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "rankings", league = league,
                                  simplifyVector = FALSE, ...)
      rows <- list()
      for (poll in raw[["rankings"]] %||% list()) {
        if (!is.list(poll)) next
        season <- poll[["season"]] %||% list()
        occ    <- poll[["occurrence"]] %||% list()
        head <- list(
          poll_id         = .ncaa_hockey_int(poll[["id"]]),
          poll_name       = .ncaa_hockey_chr(poll[["name"]]),
          poll_short_name = .ncaa_hockey_chr(poll[["shortName"]]),
          poll_type       = .ncaa_hockey_chr(poll[["type"]]),
          season          = .ncaa_hockey_int(season[["year"]]),
          season_type     = .ncaa_hockey_int((season[["type"]] %||% list())[["type"]]),
          week            = .ncaa_hockey_int(occ[["value"]]),
          week_display    = .ncaa_hockey_chr(occ[["displayValue"]]),
          poll_date       = .ncaa_hockey_chr(poll[["date"]])
        )
        for (ranked in c(TRUE, FALSE)) {
          for (entry in poll[[if (ranked) "ranks" else "others"]] %||% list()) {
            if (!is.list(entry)) next
            team  <- entry[["team"]] %||% list()
            color <- .ncaa_hockey_chr(team[["color"]])
            rows[[length(rows) + 1L]] <- c(head, list(
              ranked            = ranked,
              team_id           = .ncaa_hockey_chr(team[["id"]]),
              rank              = if (ranked) .ncaa_hockey_int(entry[["current"]]) else NA_integer_,
              previous_rank     = .ncaa_hockey_int(entry[["previous"]]),
              points            = .ncaa_hockey_num(entry[["points"]]),
              first_place_votes = .ncaa_hockey_int(entry[["firstPlaceVotes"]]),
              trend             = .ncaa_hockey_chr(entry[["trend"]]),
              record_summary    = .ncaa_hockey_chr(entry[["recordSummary"]]),
              team_uid          = .ncaa_hockey_chr(team[["uid"]]),
              team_location     = .ncaa_hockey_chr(team[["location"]]),
              team_name         = .ncaa_hockey_chr(team[["name"]]),
              team_nickname     = .ncaa_hockey_chr(team[["nickname"]]),
              team_abbreviation = .ncaa_hockey_chr(team[["abbreviation"]]),
              # ESPN ships the literal string "NULL" for a team with no colour.
              team_color        = if (identical(color, "NULL")) NA_character_ else color,
              team_logo         = .ncaa_hockey_chr(team[["logo"]]),
              last_updated      = .ncaa_hockey_chr(entry[["lastUpdated"]])
            ))
          }
        }
      }
      tbl <- if (length(rows) > 0) {
        dplyr::bind_rows(rows)
      } else {
        .empty_tibble(.ncaa_hockey_rankings_schema())
      }
      result <- tbl[, names(.ncaa_hockey_rankings_schema()), drop = FALSE] %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Rankings data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} rankings available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} rankings",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' Internal: a core-v2 collection (`items`) as a tibble (league-generic)
#'
#' `$ref`-only items become `ref` + `<id_col>` (the trailing id of the ref);
#' inline items are flattened and snake-cased. `echo` adds constant columns.
#' @noRd
.espn_hockey_items <- function(league, path, id_col = "id", label = "items",
                               query = list(), echo = list(), ...) {
  .args <- .capture_args()
  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = path, query = query,
                                  league = league, simplifyVector = TRUE, ...)
      items <- raw[["items"]]
      # A bare pointer (e.g. core-v2 league `standings` answers {"$ref": ...}).
      if (is.null(items) && !is.null(raw[["$ref"]])) {
        items <- data.frame(`$ref` = as.character(raw[["$ref"]]), check.names = FALSE)
      }
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }
      tbl <- if (is.data.frame(items) && identical(colnames(items), "$ref")) {
        .espn_core_collection(items, id_col = id_col)
      } else if (is.data.frame(items)) {
        jsonlite::flatten(items)
      } else {
        stop("Unexpected items format", call. = FALSE)
      }
      for (nm in names(echo)) tbl[[nm]] <- echo[[nm]]
      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " ", label, " data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} {label} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} {label}",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' Internal: ESPN hockey power index tables (league-generic)
#'
#' `fpi`: the fitt-v3 season table (one row per team, one column per FPI field,
#' labels zipped from the top-level `categories` metadata). `weekly`: the
#' core-v2 weekly snapshot (`predictives` + `efficiencies`). Mirror sdv-py
#' `parse_fpi()` / `parse_weekly_powerindex()`; zero rows when unpublished.
#' @noRd
.espn_hockey_powerindex <- function(league, kind = c("fpi", "weekly"),
                                    path = NULL, query = list(), ...) {
  kind <- match.arg(kind)
  .args <- .capture_args()
  result <- data.frame()

  tryCatch(
    expr = {
      raw <- if (kind == "fpi") {
        url  <- paste0("https://site.web.api.espn.com/apis/fitt/v3/sports/hockey/",
                       league, "/powerindex")
        resp <- .retry_request(url, params = query, ...)
        check_status(resp)
        .resp_json(resp, simplifyVector = FALSE)
      } else {
        .espn_hockey_request("core_v2", path = path, query = query,
                             league = league, simplifyVector = FALSE, ...)
      }
      rows <- list()
      if (kind == "fpi") {
        cats <- lapply(raw[["categories"]] %||% list(), function(cc) unlist(cc[["names"]] %||% list()))
        names(cats) <- vapply(raw[["categories"]] %||% list(),
                              function(cc) cc[["name"]] %||% "", character(1))
        for (entry in raw[["teams"]] %||% list()) {
          tm  <- entry[["team"]] %||% list()
          row <- list(
            team_id = .ncaa_hockey_chr(tm[["id"]]), team_uid = .ncaa_hockey_chr(tm[["uid"]]),
            team_abbreviation = .ncaa_hockey_chr(tm[["abbreviation"]]),
            team_display_name = .ncaa_hockey_chr(tm[["displayName"]]),
            team_short_display_name = .ncaa_hockey_chr(tm[["shortDisplayName"]]),
            team_nickname = .ncaa_hockey_chr(tm[["nickname"]]),
            season = .ncaa_hockey_int(raw[["requestedSeason"]])
          )
          for (cc in entry[["categories"]] %||% list()) {
            nms  <- unlist(cc[["names"]] %||% list())
            if (length(nms) == 0) nms <- cats[[cc[["name"]] %||% ""]]
            vals <- cc[["values"]] %||% list()
            for (i in seq_len(min(length(nms), length(vals)))) {
              key <- janitor::make_clean_names(nms[[i]])
              if (key %in% names(row)) key <- paste0(key, "_", janitor::make_clean_names(cc[["name"]] %||% "x"))
              row[[key]] <- if (is.null(vals[[i]])) NA else vals[[i]]
            }
          }
          rows[[length(rows) + 1L]] <- row
        }
      } else {
        for (it in raw[["items"]] %||% list()) {
          ref <- (it[["team"]] %||% list())[["$ref"]] %||% ""
          tid <- regmatches(ref, regexpr("(?<=/teams/)[0-9]+", ref, perl = TRUE))
          row <- list(
            team_id = if (length(tid)) as.integer(tid) else NA_integer_,
            season = .ncaa_hockey_int(it[["season"]]),
            last_updated = .ncaa_hockey_chr(it[["lastUpdated"]]),
            run_date_time_key = .ncaa_hockey_chr(it[["runDateTimeKey"]])
          )
          for (block in c("predictives", "efficiencies")) {
            for (f in it[[block]] %||% list()) {
              key <- janitor::make_clean_names(f[["name"]] %||% "")
              if (!nzchar(key)) next
              if (key %in% names(row)) key <- paste0(key, "_", substr(block, 1, 3))
              row[[key]] <- if (is.null(f[["value"]])) NA else f[["value"]]
            }
          }
          rows[[length(rows) + 1L]] <- row
        }
      }
      if (length(rows) == 0) return(result)
      result <- dplyr::bind_rows(lapply(rows, tibble::as_tibble_row)) %>%
        make_fastRhockey_data(
          paste0(toupper(league), if (kind == "fpi") " Power Index" else " Weekly Power Index",
                 " data from ESPN"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} power index available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} power index",
        args = .args)
    },
    finally = {}
  )
  return(result)
}


# ===========================================================================
# mch: ESPN men's college hockey (NCAA)
# ===========================================================================

#' @name espn_mch_scoreboard
NULL
#' @title **Get ESPN MCH Scoreboard**
#' @rdname espn_mch_scoreboard
#' @author Saiem Gilani
#' @param dates Optional date or date-range string in `"YYYYMMDD"` or
#'   `"YYYYMMDD-YYYYMMDD"` format. When `NULL` (default) the current day
#'   is used.
#' @param week Optional week number (ESPN `week` query).
#' @param season_type Optional season phase (1 = pre, 2 = regular, 3 = post); sent as `seasontype`.
#' @param groups Optional conference/group id filter.
#' @param limit Optional maximum number of events (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_scoreboard()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per game:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | game_id | character | ESPN event identifier. |
#'    | uid | character | ESPN event uid. |
#'    | date | character | Game date-time (ISO 8601). |
#'    | name | character | Full game name. |
#'    | short_name | character | Short game name. |
#'    | season_year | integer | Season end year. |
#'    | season_type | integer | Season type code (1=pre, 2=regular, 3=post). |
#'    | season_slug | character | Season type slug. |
#'    | status_type_id | character | Status type identifier. |
#'    | status_type_name | character | Status type name. |
#'    | status_type_state | character | Status state (pre/in/post). |
#'    | status_type_completed | logical | Whether the game is complete. |
#'    | status_type_description | character | Status description. |
#'    | status_type_detail | character | Status detail text. |
#'    | status_type_short_detail | character | Short status detail. |
#'    | status_clock | numeric | Game clock in seconds. |
#'    | status_display_clock | character | Display clock string. |
#'    | status_period | integer | Current period. |
#'    | neutral_site | logical | Whether the game is at a neutral site. |
#'    | conference_competition | character | Whether it is a conference competition. |
#'    | attendance | integer | Game attendance. |
#'    | venue_id | character | ESPN venue identifier. |
#'    | venue_full_name | character | Venue full name. |
#'    | venue_city | character | Venue city. |
#'    | venue_state | character | Venue state. |
#'    | venue_indoor | logical | Whether the venue is indoors. |
#'    | broadcast | character | Broadcast network(s). |
#'    | note | character | Game note or headline. |
#'    | home_id | character | Home team ESPN identifier. |
#'    | home_name | character | Home team name. |
#'    | home_abbreviation | character | Home team abbreviation. |
#'    | home_display_name | character | Home team display name. |
#'    | home_location | character | Home team city/location. |
#'    | home_color | character | Home team primary color hex. |
#'    | home_alternate_color | character | Home team alternate color hex. |
#'    | home_logo | character | Home team logo URL. |
#'    | home_score | character | Home team score. |
#'    | home_winner | logical | Whether the home team won. |
#'    | home_rank | integer | Home team rank (if ranked). |
#'    | away_id | character | Away team ESPN identifier. |
#'    | away_name | character | Away team name. |
#'    | away_abbreviation | character | Away team abbreviation. |
#'    | away_display_name | character | Away team display name. |
#'    | away_location | character | Away team city/location. |
#'    | away_color | character | Away team primary color hex. |
#'    | away_alternate_color | character | Away team alternate color hex. |
#'    | away_logo | character | Away team logo URL. |
#'    | away_score | character | Away team score. |
#'    | away_winner | logical | Whether the away team won. |
#'    | away_rank | integer | Away team rank (if ranked). |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_scoreboard(dates = "20250110"))
#' }
espn_mch_scoreboard <- function(dates = NULL, week = NULL, season_type = NULL, groups = NULL, limit = NULL, ...) {
  .espn_hockey_scoreboard(league = "mens-college-hockey", dates = dates, week = week, season_type = season_type, groups = groups, limit = limit, ...)
}

#' @name espn_mch_summary
NULL
#' @title **Get ESPN MCH Raw Game Summary**
#' @rdname espn_mch_summary
#' @author Saiem Gilani
#' @param event_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_summary()` (columns inherited); a collection may be empty.
#' @return A nested R list representing the full parsed ESPN summary JSON.
#'   **This function does NOT return a tibble.** It is intended for power
#'   users who need fields not exposed by pbp,
#'   team_box, or player_box.
#'
#'   Top-level keys typically include:
#'   `boxscore`, `format`, `gameInfo`, `leaders`, `seasonseries`,
#'   `broadcasts`, `pickcenter`, `odds`, `plays`, `header`, `standings`,
#'   and others.
#'
#'   - `$boxscore$teams` — list of 2 team stat objects.
#'   - `$boxscore$players` — list of 2 player stat group objects.
#'   - `$plays` — list of play objects.
#'   - `$header$season` — season year and type.
#'   - `$header$competitions[[1]]$competitors` — home/away team + score.
#'
#'   Shape:
#'
#'    |col_name|type|description                                                   |
#'    |:-------|:---|:-------------------------------------------------------------|
#'    |(list)  |list|Named list, not a data frame. Top-level keys are listed above.|
#'
#' @importFrom jsonlite fromJSON
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   s <- try(espn_mch_summary(event_id = "401717648"))
#'   if (!inherits(s, "try-error")) {
#'     names(s)
#'     length(s$plays)
#'   }
#' }
espn_mch_summary <- function(event_id, ...) {
  .espn_hockey_summary(league = "mens-college-hockey", game_id = event_id, ...)
}

#' @name espn_mch_pbp
NULL
#' @title **Get ESPN MCH Play-by-Play**
#' @rdname espn_mch_pbp
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`. Parsed from the site-v2 `summary` payload by the same
#'   helper as `espn_nhl_pbp()`; columns verified against a captured mch summary body.
#' @return A `fastRhockey_data` tibble, one row per play:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | id | character | ESPN play identifier. |
#'    | sequence_number | character | Play sequence number. |
#'    | type_id | character | Play type identifier. |
#'    | type_text | character | Play type display text. |
#'    | text | character | Play description text. |
#'    | away_score | integer | Away team score at time of play. |
#'    | home_score | integer | Home team score at time of play. |
#'    | scoring_play | logical | Whether the play resulted in a goal. |
#'    | score_value | integer | Goal value (1 for regulation/OT goals). |
#'    | shooting_play | logical | Whether the play was a shot attempt. |
#'    | wallclock | character | Wall-clock timestamp (ISO 8601). |
#'    | period_number | integer | Period number (1-3 regulation, 4+ OT/SO). |
#'    | period_display_value | character | Period display label (e.g. "1st Period"). |
#'    | clock_display_value | character | Game clock display string (e.g. "14:32"). |
#'    | team_id | character | ESPN team identifier for the acting team. |
#'    | coordinate_x | numeric | X coordinate of play location. |
#'    | coordinate_y | numeric | Y coordinate of play location. |
#'    | strength_id | character | Strength situation identifier. |
#'    | strength_text | character | Strength situation text (e.g. "Even Strength"). |
#'    | shot_info_id | character | Shot type identifier. |
#'    | shot_info_text | character | Shot type text (e.g. "Wrist Shot"). |
#'    | athlete_id_1 | character | Primary participant athlete identifier. |
#'    | athlete_id_2 | character | Secondary participant athlete identifier. |
#'    | athlete_id_3 | character | Tertiary participant athlete identifier. |
#'    | home_team_id | character | Home team ESPN identifier. |
#'    | home_team_name | character | Home team name. |
#'    | home_team_location | character | Home team city/location. |
#'    | home_team_abbrev | character | Home team abbreviation. |
#'    | home_team_display_name | character | Home team display name. |
#'    | home_team_color | character | Home team primary color hex. |
#'    | home_team_alternate_color | character | Home team alternate color hex. |
#'    | home_team_logo | character | Home team logo URL. |
#'    | home_team_score | character | Home team final score. |
#'    | home_team_winner | logical | Whether the home team won. |
#'    | away_team_id | character | Away team ESPN identifier. |
#'    | away_team_name | character | Away team name. |
#'    | away_team_location | character | Away team city/location. |
#'    | away_team_abbrev | character | Away team abbreviation. |
#'    | away_team_display_name | character | Away team display name. |
#'    | away_team_color | character | Away team primary color hex. |
#'    | away_team_alternate_color | character | Away team alternate color hex. |
#'    | away_team_logo | character | Away team logo URL. |
#'    | away_team_score | character | Away team final score. |
#'    | away_team_winner | logical | Whether the away team won. |
#'    | game_id | character | ESPN event identifier (echoed from arg). |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type code. |
#'    | game_date | character | Game date-time (ISO 8601). |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_pbp(game_id = "401717648"))
#' }
espn_mch_pbp <- function(game_id, ...) {
  .espn_hockey_pbp(league = "mens-college-hockey", game_id = game_id, ...)
}

#' @name espn_mch_team_box
NULL
#' @title **Get ESPN MCH Team Box Score**
#' @rdname espn_mch_team_box
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`. Parsed from the site-v2 `summary` payload by the same
#'   helper as `espn_nhl_team_box()`; columns verified against a captured mch summary body.
#' @return A `fastRhockey_data` tibble, one row per team (2 rows):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | team_id | character | ESPN team identifier. |
#'    | team_uid | character | ESPN team uid. |
#'    | team_slug | character | Team URL slug. |
#'    | team_location | character | Team city/location. |
#'    | team_name | character | Team name (mascot). |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_display_name | character | Team display name. |
#'    | team_short_display_name | character | Team short display name. |
#'    | team_color | character | Team primary color hex. |
#'    | team_alternate_color | character | Team alternate color hex. |
#'    | team_logo | character | Team logo URL. |
#'    | home_away | character | "home" or "away". |
#'    | game_id | character | ESPN event identifier (echoed from arg). |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type code. |
#'    | game_date | character | Game date-time (ISO 8601). |
#'    | blocked_shots | character | Total blocked shots. |
#'    | hits | character | Total hits. |
#'    | takeaways | character | Total takeaways. |
#'    | shots_total | character | Total shots on goal. |
#'    | power_play_goals | character | Power play goals. |
#'    | power_play_opportunities | character | Power play opportunities. |
#'    | power_play_pct | character | Power play percentage. |
#'    | short_handed_goals | character | Short-handed goals. |
#'    | shootout_goals | character | Shootout goals. |
#'    | faceoffs_won | character | Faceoffs won. |
#'    | faceoff_percent | character | Faceoff win percentage. |
#'    | giveaways | character | Giveaways. |
#'    | penalties | character | Penalty count. |
#'    | penalty_minutes | character | Penalty minutes. |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_box(game_id = "401717648"))
#' }
espn_mch_team_box <- function(game_id, ...) {
  .espn_hockey_team_box(league = "mens-college-hockey", game_id = game_id, ...)
}

#' @name espn_mch_player_box
NULL
#' @title **Get ESPN MCH Player Box Score**
#' @rdname espn_mch_player_box
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`. Parsed from the site-v2 `summary` payload by the same
#'   helper as `espn_nhl_player_box()`; columns verified against a captured mch summary body.
#' @return A `fastRhockey_data` tibble, one row per player appearance (skaters and goalies bound; columns not kept for a position group are `NA`):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | athlete_id | character | ESPN athlete identifier. |
#'    | athlete_display_name | character | Athlete full display name. |
#'    | athlete_short_name | character | Athlete short name. |
#'    | athlete_last_name | character | Athlete last name. |
#'    | athlete_jersey | character | Jersey number. |
#'    | athlete_headshot_href | character | Headshot image URL. |
#'    | athlete_position_name | character | Position name. |
#'    | athlete_position_abbreviation | character | Position abbreviation. |
#'    | athlete_active | logical | Whether the athlete was active. |
#'    | athlete_scratched | logical | Whether the athlete was scratched. |
#'    | stat_group | character | Stat group name ("forwards","defenses","goalies"). |
#'    | team_id | character | ESPN team identifier. |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_display_name | character | Team display name. |
#'    | home_away | character | "home" or "away". |
#'    | team_score | character | Team final score. |
#'    | team_winner | logical | Whether the team won. |
#'    | opponent_team_id | character | Opponent team identifier. |
#'    | opponent_team_abbreviation | character | Opponent team abbreviation. |
#'    | game_id | character | ESPN event identifier (echoed from arg). |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type code. |
#'    | game_date | character | Game date-time (ISO 8601). |
#'    | bs | character | Blocked shots (skaters). |
#'    | ht | character | Hits (skaters). |
#'    | tk | character | Takeaways (skaters). |
#'    | plus_minus | character | Plus/minus rating (skaters). |
#'    | toi | character | Time on ice (skaters, MM:SS). |
#'    | pptoi | character | Power play time on ice (skaters, MM:SS). |
#'    | shtoi | character | Short-handed time on ice (skaters, MM:SS). |
#'    | estoi | character | Even-strength time on ice (skaters, MM:SS). |
#'    | shft | character | Total shifts (skaters). |
#'    | g | character | Goals (skaters). |
#'    | ytdg | character | Year-to-date goals (skaters/goalies). |
#'    | a | character | Assists (skaters). |
#'    | s | character | Shots (skaters). |
#'    | sm | character | Missed shots (skaters). |
#'    | sog | character | Shots on goal (skaters). |
#'    | fw | character | Faceoffs won (skaters). |
#'    | fl | character | Faceoffs lost (skaters). |
#'    | fo_percent | character | Faceoff percentage (skaters). |
#'    | gv | character | Giveaways (skaters). |
#'    | pn | character | Penalties taken (skaters). |
#'    | pim | character | Penalty minutes (skaters/goalies). |
#'    | ga | character | Goals against (goalies). |
#'    | sa | character | Shots against (goalies). |
#'    | sos | character | Shootout saves (goalies). |
#'    | sosa | character | Shootout shots against (goalies). |
#'    | sv | character | Saves (goalies). |
#'    | sv_percent | character | Save percentage (goalies). |
#'    | essv | character | Even-strength saves (goalies). |
#'    | ppsv | character | Power play saves (goalies). |
#'    | shsv | character | Short-handed saves (goalies). |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_box(game_id = "401717648"))
#' }
espn_mch_player_box <- function(game_id, ...) {
  .espn_hockey_player_box(league = "mens-college-hockey", game_id = game_id, ...)
}

#' @name espn_mch_calendar
NULL
#' @title **Get ESPN MCH Calendar**
#' @rdname espn_mch_calendar
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_calendar()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per game date:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | date | character | Game date (ISO 8601 datetime string). |
#'    | calendar_type | character | Calendar type (e.g. `"day"`). |
#'    | calendar_start_date | character | Calendar season start date. |
#'    | calendar_end_date | character | Calendar season end date. |
#'    | calendar_is_whitelist | logical | Whether dates are a whitelist of game days. |
#'
#' @importFrom dplyr mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_calendar())
#' }
espn_mch_calendar <- function(...) {
  .espn_hockey_calendar(league = "mens-college-hockey", ...)
}

#' @name espn_mch_news
NULL
#' @title **Get ESPN MCH News**
#' @rdname espn_mch_news
#' @author Saiem Gilani
#' @param limit Maximum number of articles to return (default `50`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_news()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per article:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | article_id | character | ESPN article identifier. |
#'    | now_id | character | ESPN Now identifier. |
#'    | content_key | character | Content management key. |
#'    | type | character | Article type (e.g. "Story", "Media"). |
#'    | headline | character | Article headline. |
#'    | description | character | Article description/excerpt. |
#'    | last_modified | character | ISO 8601 last-modified datetime. |
#'    | published | character | ISO 8601 published datetime. |
#'    | byline | character | Author byline. |
#'    | premium | logical | Whether the article is premium content. |
#'    | image_href | character | URL of the lead image. |
#'    | web_href | character | URL of the article on ESPN.com. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_news(limit = 10))
#' }
espn_mch_news <- function(limit = 50, ...) {
  .espn_hockey_news(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_injuries
NULL
#' @title **Get ESPN MCH Injuries**
#' @rdname espn_mch_injuries
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_injuries()`; ESPN answered an empty collection for every captured MCH body (2026-10), so the columns below are the NHL helper's and unverified for this league.
#' @return A `fastRhockey_data` tibble with one row per injured player:
#'
#'    |col_name               |types     |description                                    |
#'    |:----------------------|:---------|:----------------------------------------------|
#'    |team_id                |character |ESPN team identifier.                          |
#'    |team_display_name      |character |Team display name.                             |
#'    |player_id              |character |ESPN player identifier.                        |
#'    |player_display_name    |character |Player display name.                           |
#'    |player_short_name      |character |Player short name.                             |
#'    |player_position        |character |Player position abbreviation.                  |
#'    |injury_id              |character |ESPN injury identifier.                        |
#'    |injury_status          |character |Injury status (e.g. "Questionable", "Out").    |
#'    |injury_date            |character |Injury date (ISO 8601).                        |
#'    |injury_short_comment   |character |Short injury comment.                          |
#'    |injury_long_comment    |character |Long injury comment.                           |
#'    |injury_type            |character |Injury type name.                              |
#'    |injury_abbreviation    |character |Injury type abbreviation.                      |
#'    |detail_side            |character |Injured side (e.g. "Left", "Right").           |
#'    |detail_fantasy_status  |character |Fantasy relevance status.                      |
#'    |detail_return_date     |character |Projected return date.                         |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_injuries())
#' }
espn_mch_injuries <- function(...) {
  .espn_hockey_injuries(league = "mens-college-hockey", ...)
}

#' @name espn_mch_transactions
NULL
#' @title **Get ESPN MCH Transactions**
#' @rdname espn_mch_transactions
#' @author Saiem Gilani
#' @param limit Optional maximum number of transactions (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_transactions()`; ESPN answered an empty collection for every captured MCH body (2026-10), so the columns below are the NHL helper's and unverified for this league.
#' @return A `fastRhockey_data` tibble with one row per transaction:
#'
#'    |col_name           |types     |description                                         |
#'    |:------------------|:---------|:---------------------------------------------------|
#'    |date               |character |Transaction date (ISO 8601).                        |
#'    |description        |character |Transaction description.                            |
#'    |team_id            |character |ESPN team identifier.                               |
#'    |team_location      |character |Team city/location.                                 |
#'    |team_name          |character |Team mascot/name.                                   |
#'    |team_abbreviation  |character |Team abbreviation.                                  |
#'    |team_display_name  |character |Team display name.                                  |
#'    |team_color         |character |Team primary color hex.                             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_transactions())
#' }
espn_mch_transactions <- function(limit = NULL, ...) {
  .espn_hockey_transactions(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_conferences
NULL
#' @title **Get ESPN MCH Conferences**
#' @rdname espn_mch_conferences
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_conferences()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per conference/division
#'   combination (4 rows for MCH: 2 conferences x 2 divisions each):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | conference_name | character | Conference name (e.g. "Eastern Conference"). |
#'    | conference_abbreviation | character | Conference abbreviation. |
#'    | division_name | character | Division name (e.g. "Atlantic Division"). |
#'    | division_abbreviation | character | Division abbreviation. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_conferences())
#' }
espn_mch_conferences <- function(...) {
  .espn_hockey_conferences(league = "mens-college-hockey", ...)
}

#' @name espn_mch_statistics_league
NULL
#' @title **Get ESPN MCH League Statistics**
#' @rdname espn_mch_statistics_league
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_statistics_league()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (stat category, leader).
#'   The ESPN MCH statistics endpoint returns a sparse set of top-of-league
#'   stat leaders organized by category. If the endpoint returns no categories,
#'   an empty tibble is returned gracefully:
#'
#'    |col_name              |types     |description                                     |
#'    |:---------------------|:---------|:-----------------------------------------------|
#'    |category_name         |character |Stat category name (e.g. "goals", "assists").   |
#'    |category_abbreviation |character |Category abbreviation.                          |
#'    |display_value         |character |Leader stat display value.                      |
#'    |value                 |numeric   |Leader stat numeric value.                      |
#'    |athlete_id            |character |ESPN athlete identifier.                        |
#'    |athlete_display_name  |character |Athlete display name.                           |
#'    |team_id               |character |ESPN team identifier.                           |
#'    |team_abbreviation     |character |Team abbreviation.                              |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_statistics_league())
#' }
espn_mch_statistics_league <- function(...) {
  .espn_hockey_statistics_league(league = "mens-college-hockey", ...)
}

#' @name espn_mch_draft
NULL
#' @title **Get ESPN MCH Draft**
#' @rdname espn_mch_draft
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_draft()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with draft data, or an empty tibble
#'   when the endpoint is unavailable. **Note**: The MCH draft endpoint
#'   (`site/v2/sports/hockey/nhl/draft`) currently returns HTTP 500 for MCH —
#'   an empty tibble is returned gracefully:
#'
#'    |col_name |types     |description                                           |
#'    |:--------|:---------|:-----------------------------------------------------|
#'    |id       |character |Draft entry identifier (if returned).                 |
#'    |name     |character |Draft entry name (if returned).                       |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN returns HTTP 500 for this endpoint)
#'   try(espn_mch_draft())
#' }
espn_mch_draft <- function(...) {
  .espn_hockey_draft(league = "mens-college-hockey", ...)
}

#' @name espn_mch_teams_site
NULL
#' @title **Get ESPN MCH Teams (site_v2) -- alias for `espn_mch_teams()`**
#' @rdname espn_mch_teams_site
#' @author Saiem Gilani
#' @param limit Optional maximum number of teams (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_teams_site()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per MCH team. This is
#'   identical to the output of teams.
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | abbreviation | character | Team abbreviation. |
#'    | display_name | character | Team display name. |
#'    | espn_team_id | integer | ESPN team identifier. |
#'    | team | character | Team location/city name. |
#'    | logo | character | Light-background logo URL. |
#'    | logo_dark | character | Dark-background logo URL. |
#'    | logos_href_3 | character | Logo variant 3 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_4 | character | Logo variant 4 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_5 | character | Logo variant 5 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_6 | character | Logo variant 6 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_7 | character | Logo variant 7 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_8 | character | Logo variant 8 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_9 | character | Logo variant 9 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_10 | character | Logo variant 10 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_11 | character | Logo variant 11 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_12 | character | Logo variant 12 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_13 | character | Logo variant 13 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_14 | character | Logo variant 14 URL (ESPN sends a varying number of logos per team). |
#'    | mascot | character | Team mascot/nickname. |
#'    | nickname | character | Team nickname. |
#'    | short_name | character | Short team display name. |
#'
#' @importFrom dplyr group_by ungroup select rename
#' @importFrom tidyr unnest_wider
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @seealso teams which this function wraps.
#' @examples
#' \donttest{
#'   try(espn_mch_teams_site())
#' }
espn_mch_teams_site <- function(limit = NULL, ...) {
  .espn_hockey_teams(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_team
NULL
#' @title **Get ESPN MCH Team Detail**
#' @rdname espn_mch_team
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | team_id | character | ESPN team identifier (echoed from arg). |
#'    | id | character | ESPN team identifier. |
#'    | uid | character | ESPN team uid. |
#'    | slug | character | URL slug. |
#'    | location | character | Team city/location. |
#'    | name | character | Team mascot name. |
#'    | nickname | character | Team nickname. |
#'    | abbreviation | character | Team abbreviation. |
#'    | display_name | character | Full display name. |
#'    | short_display_name | character | Short display name. |
#'    | color | character | Primary color hex. |
#'    | alternate_color | character | Alternate color hex. |
#'    | is_active | logical | Whether the team is active. |
#'    | logo | character | Primary logo URL. |
#'    | logo_dark | character | Dark logo URL. |
#'    | stand_summary | character | Standing summary string. |
#'    | franchise_ref | character | Franchise resource ref URL. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team(team_id = "104"))
#' }
espn_mch_team <- function(team_id, ...) {
  .espn_hockey_team(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_team_roster
NULL
#' @title **Get ESPN MCH Team Roster**
#' @rdname espn_mch_team_roster
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Pittsburgh Penguins).
#' @param limit Optional maximum number of athletes (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_roster()`; ESPN answered an empty collection for every captured MCH body (2026-10), so the columns below are the NHL helper's and unverified for this league.
#' @return A `fastRhockey_data` tibble with one row per player:
#'
#'    |col_name                      |types     |description                         |
#'    |:-----------------------------|:---------|:-----------------------------------|
#'    |position_group                |character |Position group name (e.g. Centers). |
#'    |id                            |character |ESPN player identifier.             |
#'    |uid                           |character |ESPN player uid.                    |
#'    |guid                          |character |Player global unique identifier.    |
#'    |alternate_id                  |character |Alternate player identifier.        |
#'    |alternate_ids_sdr             |character |Alternate ids sdr.                  |
#'    |first_name                    |character |First name.                         |
#'    |last_name                     |character |Last name.                          |
#'    |full_name                     |character |Full name.                          |
#'    |display_name                  |character |Display name.                       |
#'    |short_name                    |character |Short name.                         |
#'    |weight                        |numeric   |Weight (lbs).                       |
#'    |display_weight                |character |Display weight string.              |
#'    |height                        |numeric   |Height (inches).                    |
#'    |display_height                |character |Display height string.              |
#'    |age                           |integer   |Player age.                         |
#'    |date_of_birth                 |character |Date of birth.                      |
#'    |slug                          |character |URL slug.                           |
#'    |jersey                        |character |Jersey number.                      |
#'    |headshot_href                 |character |Headshot image URL.                 |
#'    |headshot_alt                  |character |Headshot alt text.                  |
#'    |hand_type                     |character |Shooting/catching hand type.        |
#'    |hand_abbreviation             |character |Hand abbreviation.                  |
#'    |hand_display_value            |character |Hand display value.                 |
#'    |position_id                   |character |Position identifier.                |
#'    |position_name                 |character |Position name.                      |
#'    |position_display_name         |character |Position display name.              |
#'    |position_abbreviation         |character |Position abbreviation.              |
#'    |position_leaf                 |logical   |Whether position is a leaf node.    |
#'    |position_parent_id            |character |Parent position identifier.         |
#'    |position_parent_name          |character |Parent position name.               |
#'    |position_parent_display_name  |character |Parent position display name.       |
#'    |position_parent_abbreviation  |character |Parent position abbreviation.       |
#'    |position_parent_leaf          |logical   |Whether parent position is leaf.    |
#'    |experience_years              |integer   |Experience years.                   |
#'    |debut_year                    |numeric   |Debut year.                         |
#'    |status_id                     |character |Status identifier.                  |
#'    |status_name                   |character |Status name.                        |
#'    |status_type                   |character |Status type.                        |
#'    |status_abbreviation           |character |Status abbreviation.                |
#'    |birth_place_city              |character |Birth place city.                   |
#'    |birth_place_state             |character |Birth place state.                  |
#'    |birth_place_country           |character |Birth place country.                |
#'    |birth_place_display_text      |character |Birth place display text.           |
#'    |birth_country_abbreviation    |character |Birth country abbreviation.         |
#'    |college_id                    |character |College identifier.                 |
#'    |college_guid                  |character |College guid.                       |
#'    |college_mascot                |character |College mascot.                     |
#'    |college_name                  |character |College name.                       |
#'    |college_short_name            |character |College short name.                 |
#'    |college_abbrev                |character |College abbreviation.               |
#'    |college_logos                 |character |College logo URLs (pipe-delimited). |
#'    |team_id                       |character |ESPN team identifier (from arg).    |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_roster(team_id = "104"))
#' }
espn_mch_team_roster <- function(team_id, limit = NULL, ...) {
  .espn_hockey_team_roster(league = "mens-college-hockey", team_id = team_id, limit = limit, ...)
}

#' @name espn_mch_team_schedule
NULL
#' @title **Get ESPN MCH Team Schedule (site_v2) -- alias for `espn_mch_schedule()`**
#' @rdname espn_mch_team_schedule
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Pittsburgh Penguins).
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_schedule()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per game. Identical to
#'   schedule.
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | game_id | character | ESPN event identifier. |
#'    | date | character | Game date-time (ISO 8601). |
#'    | name | character | Full game name. |
#'    | short_name | character | Short game name. |
#'    | season_year | integer | Season end year. |
#'    | season_type | integer | Season type code. |
#'    | time_valid | logical | Whether the game time is confirmed. |
#'    | status_type_id | character | Status type identifier. |
#'    | status_type_name | character | Status type name. |
#'    | status_type_state | character | Status state (pre/in/post). |
#'    | status_type_completed | logical | Whether the game is complete. |
#'    | neutral_site | logical | Whether the game is at a neutral site. |
#'    | attendance | integer | Game attendance. |
#'    | venue_id | character | Venue identifier. |
#'    | venue_full_name | character | Venue full name. |
#'    | venue_city | character | Venue city. |
#'    | venue_state | character | Venue state. |
#'    | broadcast | character | Broadcast network(s). |
#'    | home_id | character | Home team ESPN identifier. |
#'    | home_name | character | Home team display name. |
#'    | home_abbreviation | character | Home team abbreviation. |
#'    | home_location | character | Home team city. |
#'    | home_logo | character | Home team logo URL. |
#'    | home_score | character | Home team score. |
#'    | home_winner | logical | Whether the home team won. |
#'    | away_id | character | Away team ESPN identifier. |
#'    | away_name | character | Away team display name. |
#'    | away_abbreviation | character | Away team abbreviation. |
#'    | away_location | character | Away team city. |
#'    | away_logo | character | Away team logo URL. |
#'    | away_score | character | Away team score. |
#'    | away_winner | logical | Whether the away team won. |
#'    | team_id | character | ESPN team identifier (echoed from arg). |
#'    | season | integer | Season end-year (echoed from arg). |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @seealso schedule which this function wraps.
#' @examples
#' \donttest{
#'   try(espn_mch_team_schedule(team_id = "104", season = 2025))
#' }
espn_mch_team_schedule <- function(team_id,
                                    season = most_recent_nhl_season(),
                                    ...) {
  .espn_hockey_schedule(league = "mens-college-hockey", team_id = team_id,
                         season = season, ...)
}

#' @name espn_mch_team_record
NULL
#' @title **Get ESPN MCH Team Record**
#' @rdname espn_mch_team_record
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_record()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per record type
#'   (overall/home/away) when data is available. The `/record` endpoint
#'   currently returns an empty response for MCH; an empty tibble is returned
#'   with a cli warning. Columns when populated include `team_id`,
#'   `description`, `type`, `summary`, and one numeric column per stat.
#'
#'   Columns when populated:
#'
#'    |col_name   |type     |description                                                                                     |
#'    |:----------|:--------|:-----------------------------------------------------------------------------------------------|
#'    |team_id    |character|ESPN team identifier (echoed from the argument).                                                |
#'    |description|character|Record description.                                                                             |
#'    |type       |character|Record type (overall, home, away).                                                              |
#'    |summary    |character|Record summary string, e.g. `'10-5-1'`.                                                         |
#'    |<stat>     |numeric  |One column per ESPN stat on the record, named by `janitor::make_clean_names()` of the stat name.|
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_record(team_id = "104"))
#' }
espn_mch_team_record <- function(team_id, ...) {
  .espn_hockey_team_record(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_team_depthcharts
NULL
#' @title **Get ESPN MCH Team Depth Charts**
#' @rdname espn_mch_team_depthcharts
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_depthcharts()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row containing team and season
#'   metadata. NHL depth-slot player assignments are not served at this
#'   endpoint; the response contains team/season context only:
#'
#'    |col_name         |types     |description                             |
#'    |:----------------|:---------|:---------------------------------------|
#'    |team_id          |character |ESPN team identifier (echoed from arg). |
#'    |timestamp        |character |Response timestamp (ISO 8601).          |
#'    |status           |character |Response status string.                 |
#'    |season_year      |integer   |Season year.                            |
#'    |season_type      |integer   |Season type code.                       |
#'    |season_name      |character |Season type name.                       |
#'    |team_espn_id     |character |ESPN team identifier.                   |
#'    |abbreviation     |character |Team abbreviation.                      |
#'    |location         |character |Team city/location.                     |
#'    |name             |character |Team mascot name.                       |
#'    |display_name     |character |Full display name.                      |
#'    |color            |character |Primary color hex.                      |
#'    |logo             |character |Team logo URL.                          |
#'    |record_summary   |character |Record summary string.                  |
#'    |season_summary   |character |Season summary string.                  |
#'    |standing_summary |character |Standing summary string.                |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_depthcharts(team_id = "104"))
#' }
espn_mch_team_depthcharts <- function(team_id, ...) {
  .espn_hockey_team_depthcharts(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_team_injuries
NULL
#' @title **Get ESPN MCH Team Injuries**
#' @rdname espn_mch_team_injuries
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_injuries()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per injury entry when data
#'   is available. The `/injuries` endpoint currently returns an empty response
#'   for MCH; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `athlete_id`, `athlete_display_name`,
#'   `athlete_position`, `injury_type`, `injury_status`, `injury_date`, and
#'   `injury_description`.
#'
#'   Columns when populated:
#'
#'    |col_name            |type     |description                                     |
#'    |:-------------------|:--------|:-----------------------------------------------|
#'    |team_id             |character|ESPN team identifier (echoed from the argument).|
#'    |athlete_id          |character|ESPN athlete identifier.                        |
#'    |athlete_display_name|character|Athlete display name.                           |
#'    |athlete_position    |character|Athlete position.                               |
#'    |injury_type         |character|Injury type.                                    |
#'    |injury_status       |character|Injury status.                                  |
#'    |injury_date         |character|Injury date.                                    |
#'    |injury_description  |character|Long comment or description.                    |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_injuries(team_id = "104"))
#' }
espn_mch_team_injuries <- function(team_id, ...) {
  .espn_hockey_team_injuries(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_team_transactions
NULL
#' @title **Get ESPN MCH Team Transactions**
#' @rdname espn_mch_team_transactions
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_transactions()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per transaction when data
#'   is available. The `/transactions` endpoint currently returns an empty
#'   response for MCH; an empty tibble is returned with a cli warning. Columns
#'   when populated include `team_id`, `id`, `date`, `description`, and `type`.
#'
#'   Columns when populated:
#'
#'    |col_name   |type     |description                                     |
#'    |:----------|:--------|:-----------------------------------------------|
#'    |team_id    |character|ESPN team identifier (echoed from the argument).|
#'    |id         |character|Transaction identifier.                         |
#'    |date       |character|Transaction date.                               |
#'    |description|character|Transaction description.                        |
#'    |type       |character|Transaction type.                               |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_transactions(team_id = "104"))
#' }
espn_mch_team_transactions <- function(team_id, ...) {
  .espn_hockey_team_transactions(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_team_history
NULL
#' @title **Get ESPN MCH Team History**
#' @rdname espn_mch_team_history
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_history()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per season entry when data
#'   is available. The `/history` endpoint currently returns an empty response
#'   for MCH; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `season`, `wins`, `losses`, `ties`,
#'   `ot_losses`, `points`, `summary`, and `description`.
#'
#'   Columns when populated:
#'
#'    |col_name   |type     |description                                     |
#'    |:----------|:--------|:-----------------------------------------------|
#'    |team_id    |character|ESPN team identifier (echoed from the argument).|
#'    |season     |integer  |Season year.                                    |
#'    |wins       |integer  |Wins.                                           |
#'    |losses     |integer  |Losses.                                         |
#'    |ties       |integer  |Ties.                                           |
#'    |ot_losses  |integer  |Overtime losses.                                |
#'    |points     |integer  |Points.                                         |
#'    |summary    |character|Record summary string.                          |
#'    |description|character|Season description.                             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_history(team_id = "104"))
#' }
espn_mch_team_history <- function(team_id, ...) {
  .espn_hockey_team_history(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_team_news
NULL
#' @title **Get ESPN MCH Team News**
#' @rdname espn_mch_team_news
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param limit Maximum number of articles to return. Default `50`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_news()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per news article when data
#'   is available. The `/news` endpoint currently returns an empty response
#'   for MCH; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `id`, `headline`, `description`, `type`,
#'   `published`, `last_modified`, `premium`, and `url`.
#'
#'   Columns when populated:
#'
#'    |col_name     |type     |description                                     |
#'    |:------------|:--------|:-----------------------------------------------|
#'    |team_id      |character|ESPN team identifier (echoed from the argument).|
#'    |id           |character|Article identifier.                             |
#'    |headline     |character|Article headline.                               |
#'    |description  |character|Article description.                            |
#'    |type         |character|Article type.                                   |
#'    |published    |character|Publication timestamp.                          |
#'    |last_modified|character|Last-modified timestamp.                        |
#'    |premium      |logical  |Whether the article is premium content.         |
#'    |url          |character|Web URL of the article.                         |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_news(team_id = "104", limit = 10))
#' }
espn_mch_team_news <- function(team_id, limit = 50, ...) {
  .espn_hockey_team_news(league = "mens-college-hockey", team_id = team_id, limit = limit, ...)
}

#' @name espn_mch_team_leaders
NULL
#' @title **Get ESPN MCH Team Leaders**
#' @rdname espn_mch_team_leaders
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination when data is available. The `/leaders` endpoint currently
#'   returns an empty response for MCH; an empty tibble is returned with a cli
#'   warning. Columns when populated include `team_id`, `category_name`,
#'   `category_abbr`, `rank`, `value`, `display_value`, `athlete_id`, and
#'   `athlete_full_name`.
#'
#'   Columns when populated:
#'
#'    |col_name         |type     |description                                     |
#'    |:----------------|:--------|:-----------------------------------------------|
#'    |team_id          |character|ESPN team identifier (echoed from the argument).|
#'    |category_name    |character|Leader category name.                           |
#'    |category_abbr    |character|Leader category abbreviation.                   |
#'    |rank             |integer  |Rank within the category.                       |
#'    |value            |numeric  |Leader stat value.                              |
#'    |display_value    |character|Formatted stat value.                           |
#'    |athlete_id       |character|ESPN athlete identifier.                        |
#'    |athlete_full_name|character|Athlete full name.                              |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_leaders(team_id = "104"))
#' }
espn_mch_team_leaders <- function(team_id, ...) {
  .espn_hockey_team_leaders(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_player_info
NULL
#' @title **Get ESPN MCH Player Info**
#' @rdname espn_mch_player_info
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_info()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with athlete metadata from the
#'   `site_v2` athletes endpoint, or an empty `data.frame()` when ESPN returns
#'   HTTP 404. As of 2026-06-08, ESPN's `site_v2` NHL `athletes/{id}` endpoint
#'   returns 404; use [espn_mch_player_core()] for athlete metadata instead.
#'
#'    |col_name          |types     |description                                           |
#'    |:-----------------|:---------|:-----------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete identifier (echoed from arg).            |
#'    |athlete_espn_id   |character |ESPN athlete id from payload.                         |
#'    |uid               |character |Athlete uid string.                                   |
#'    |guid              |character |Athlete global unique identifier.                     |
#'    |display_name      |character |Player display name.                                  |
#'    |short_name        |character |Player short name.                                    |
#'    |full_name         |character |Player full name.                                     |
#'    |first_name        |character |First name.                                           |
#'    |last_name         |character |Last name.                                            |
#'    |jersey            |character |Jersey number.                                        |
#'    |slug              |character |URL slug.                                             |
#'    |position          |character |Position abbreviation.                                |
#'    |position_name     |character |Position full name.                                   |
#'    |age               |integer   |Current age.                                          |
#'    |date_of_birth     |character |Date of birth (ISO 8601).                             |
#'    |debut_year        |integer   |Year of NHL debut.                                    |
#'    |weight            |character |Formatted weight string.                              |
#'    |height            |character |Formatted height string.                              |
#'    |team_id           |character |ESPN team identifier.                                 |
#'    |team_display_name |character |Team display name.                                    |
#'    |team_abbreviation |character |Team abbreviation.                                    |
#'    |headshot_href     |character |Player headshot image URL.                            |
#'    |active            |logical   |Whether athlete is currently active.                  |
#'    |status            |character |Athlete status description.                           |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_info(athlete_id = "5136656"))
#' }
espn_mch_player_info <- function(athlete_id, ...) {
  .espn_hockey_player_info(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_bio
NULL
#' @title **Get ESPN MCH Player Bio**
#' @rdname espn_mch_player_bio
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_bio()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with athlete bio fields, or an
#'   empty `data.frame()` when ESPN returns HTTP 404. As of 2026-06-08,
#'   ESPN's `site_v2` NHL `athletes/{id}/bio` endpoint returns 404; use
#'   [espn_mch_player_core()] for athlete metadata in the meantime.
#'
#'    |col_name          |types     |description                                        |
#'    |:-----------------|:---------|:--------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete identifier (echoed from arg).         |
#'    |athlete_espn_id   |character |ESPN athlete id from payload.                      |
#'    |display_name      |character |Player display name.                               |
#'    |short_name        |character |Player short name.                                 |
#'    |full_name         |character |Player full name.                                  |
#'    |first_name        |character |First name.                                        |
#'    |last_name         |character |Last name.                                         |
#'    |jersey            |character |Jersey number.                                     |
#'    |position          |character |Position abbreviation.                             |
#'    |age               |integer   |Current age.                                       |
#'    |date_of_birth     |character |Date of birth (ISO 8601).                          |
#'    |birth_city        |character |Birth city.                                        |
#'    |birth_state       |character |Birth state/province.                              |
#'    |birth_country     |character |Birth country.                                     |
#'    |weight            |character |Formatted weight string.                           |
#'    |height            |character |Formatted height string.                           |
#'    |team_id           |character |ESPN team identifier.                              |
#'    |team_display_name |character |Team display name.                                 |
#'    |team_abbreviation |character |Team abbreviation.                                 |
#'    |slug              |character |Player URL slug.                                   |
#'    |debut_year        |integer   |Year of NHL debut.                                 |
#'    |active            |logical   |Whether athlete is currently active.               |
#'    |description       |character |Biographical description, if available.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_bio(athlete_id = "5136656"))
#' }
espn_mch_player_bio <- function(athlete_id, ...) {
  .espn_hockey_player_bio(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_news
NULL
#' @title **Get ESPN MCH Player News**
#' @rdname espn_mch_player_news
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param limit Maximum number of articles to return (default `50`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_news()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per news article, or an
#'   empty `data.frame()` when no articles are available:
#'
#'    |col_name          |types     |description                                          |
#'    |:-----------------|:---------|:----------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete identifier (echoed from arg).           |
#'    |article_id        |character |Article identifier.                                  |
#'    |type              |character |Article type (e.g. "HeadlineNews").                 |
#'    |headline          |character |Article headline.                                    |
#'    |description       |character |Article description / lede.                          |
#'    |published         |character |Publication datetime (ISO 8601).                     |
#'    |last_modified     |character |Last-modified datetime (ISO 8601).                   |
#'    |premium           |logical   |Whether article is premium/paywalled.                |
#'    |source            |character |News source.                                         |
#'    |byline            |character |Author byline.                                       |
#'    |article_url       |character |Web URL for the full article.                        |
#'    |image_url         |character |Lead image URL.                                      |
#'    |category_league   |character |League category label from article metadata.         |
#'    |category_team     |character |Team category label from article metadata.           |
#'    |category_athlete  |character |Athlete category label from article metadata.        |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_news(athlete_id = "5136656", limit = 10))
#' }
espn_mch_player_news <- function(athlete_id, limit = 50, ...) {
  .espn_hockey_player_news(league = "mens-college-hockey", athlete_id = athlete_id,
                            limit = limit, ...)
}

#' @name espn_mch_standings
NULL
#' @title **Get ESPN MCH Standings**
#' @rdname espn_mch_standings
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param group Optional conference/group id filter.
#' @param standings_type Optional standings variant sent as `type` (e.g. `"by-division"`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_standings()`; columns verified against a captured MCH body One numeric column per ESPN stat in the body, so the set follows the feed (the MCH body sends `tiegames`, the WCH body `ties`).
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | group_name | character | Group name (conference / division). |
#'    | group_abbreviation | character | Group abbreviation. |
#'    | team_id | character | ESPN team identifier. |
#'    | team_name | character | Team name. |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_display_name | character | Team display name. |
#'    | team_location | character | Team location (city). |
#'    | team_logo | character | Team logo URL. |
#'    | ot_losses | numeric | Overtime losses. |
#'    | ot_wins | numeric | Overtime wins. |
#'    | avg_points_against | numeric | Average goals against per game. |
#'    | avg_points_for | numeric | Average goals for per game. |
#'    | differential | numeric | Goal differential. |
#'    | games_behind | numeric | Games behind the group leader. |
#'    | games_played | numeric | Games played. |
#'    | losses | numeric | Regulation losses. |
#'    | playoff_seed | numeric | Current playoff seed. |
#'    | point_differential | numeric | Point (goal) differential. |
#'    | points | numeric | Points. |
#'    | points_against | numeric | Goals/points allowed. |
#'    | points_for | numeric | Goals/points scored. |
#'    | streak | numeric | Current streak value. |
#'    | tiegames | numeric | Tie games (sent in the mch body). |
#'    | win_percent | numeric | Win percentage. |
#'    | wins | numeric | Wins. |
#'    | overall | numeric | Overall record stat. |
#'    | season | integer | Season end-year (echoed from arg). |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_standings(season = 2025))
#' }
espn_mch_standings <- function(season = most_recent_nhl_season(), group = NULL, standings_type = NULL, ...) {
  .espn_hockey_standings(league = "mens-college-hockey", season = season, group = group, standings_type = standings_type, ...)
}

#' @name espn_mch_player_overview
NULL
#' @title **Get ESPN MCH Player Overview**
#' @rdname espn_mch_player_overview
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_overview()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per stats split
#'   (Regular Season / Career):
#'
#'    |col_name                     |types     |description                                       |
#'    |:----------------------------|:---------|:-------------------------------------------------|
#'    |athlete_id                   |character |ESPN athlete identifier (echoed from arg).        |
#'    |athlete_espn_id              |character |ESPN athlete identifier (from payload).           |
#'    |athlete_display_name         |character |Player display name.                              |
#'    |athlete_short_name           |character |Player short name.                                |
#'    |athlete_jersey               |character |Jersey number.                                    |
#'    |athlete_position             |character |Position abbreviation.                            |
#'    |athlete_team_id              |character |ESPN team identifier.                             |
#'    |athlete_team_abbreviation    |character |Team abbreviation.                                |
#'    |split_display_name           |character |Split label (e.g. Regular Season, Career).        |
#'    |games                        |character |Games played.                                     |
#'    |goals                        |character |Goals.                                            |
#'    |assists                      |character |Assists.                                          |
#'    |points                       |character |Points.                                           |
#'    |plus_minus                   |character |Plus/minus.                                       |
#'    |penalty_minutes              |character |Penalty minutes.                                  |
#'    |shots_total                  |character |Shots on goal.                                    |
#'    |power_play_goals             |character |Power-play goals.                                 |
#'    |power_play_assists           |character |Power-play assists.                               |
#'    |short_handed_goals           |character |Short-handed goals.                               |
#'    |short_handed_assists         |character |Short-handed assists.                             |
#'    |game_winning_goals           |character |Game-winning goals.                               |
#'    |time_on_ice_per_game         |character |Average time on ice per game.                     |
#'    |production                   |character |Production metric.                                |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_overview(athlete_id = "5136656"))
#' }
espn_mch_player_overview <- function(athlete_id, ...) {
  .espn_hockey_player_overview(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_stats
NULL
#' @title **Get ESPN MCH Player Stats (web-v3)**
#' @rdname espn_mch_player_stats
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`. When `NULL` the current season is used.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_stats_v3()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per season entry plus
#'   a career-total row per stat category:
#'
#'    |col_name             |types     |description                                  |
#'    |:--------------------|:---------|:--------------------------------------------|
#'    |athlete_id           |character |ESPN athlete identifier (echoed from arg).   |
#'    |season               |integer   |Requested season year (echoed from arg).     |
#'    |season_year          |integer   |Season end-year for this row.                |
#'    |category             |character |Stat category name (e.g. center).           |
#'    |category_display     |character |Stat category display name.                  |
#'    |position             |character |Position for this season row.                |
#'    |team_id              |character |ESPN team identifier.                        |
#'    |team_slug            |character |Team slug.                                   |
#'    |team_display_name    |character |Team display name.                           |
#'    |team_abbreviation    |character |Team abbreviation.                           |
#'    |games                |character |Games played.                                |
#'    |goals                |character |Goals.                                       |
#'    |assists              |character |Assists.                                     |
#'    |points               |character |Points.                                      |
#'    |plus_minus           |character |Plus/minus.                                  |
#'    |penalty_minutes      |character |Penalty minutes.                             |
#'    |shootout_goals       |character |Shootout goals.                              |
#'    |shooting_pct         |character |Shooting percentage.                         |
#'    |power_play_goals     |character |Power-play goals.                            |
#'    |power_play_assists   |character |Power-play assists.                          |
#'    |short_handed_goals   |character |Short-handed goals.                          |
#'    |short_handed_assists |character |Short-handed assists.                        |
#'    |game_winning_goals   |character |Game-winning goals.                          |
#'    |time_on_ice_per_game |character |Average time on ice per game.                |
#'    |production           |character |Production metric.                           |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_stats(athlete_id = "5136656", season = 2025))
#' }
espn_mch_player_stats <- function(athlete_id,
                                      season = most_recent_nhl_season(),
                                      ...) {
  .espn_hockey_player_stats_v3(league     = "mens-college-hockey",
                                athlete_id = athlete_id,
                                season     = season,
                                ...)
}

#' @name espn_mch_player_gamelog
NULL
#' @title **Get ESPN MCH Player Gamelog**
#' @rdname espn_mch_player_gamelog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param season Season end-year (e.g. `2025`). When `NULL` ESPN returns the
#'   current season. Defaults to `NULL`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_gamelog()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per game, or an empty
#'   `data.frame()` when no data is available (e.g. if the endpoint returns
#'   HTTP 404 in a future ESPN schema change). If the endpoint becomes
#'   unavailable, use [espn_mch_player_statisticslog()] from the core-v2
#'   host as a reliable substitute.
#'
#'    |col_name              |types     |description                                          |
#'    |:---------------------|:---------|:----------------------------------------------------|
#'    |athlete_id            |character |ESPN athlete identifier (echoed from arg).           |
#'    |event_id              |character |ESPN event identifier.                               |
#'    |season_type_display   |character |Season type label (e.g. "2025-26 Regular Season").   |
#'    |category_display      |character |Stat category display name.                          |
#'    |category_type         |character |Stat category type.                                  |
#'    |category_split_type   |character |Stat category split type.                            |
#'    |game_date             |character |Game date-time (ISO 8601).                           |
#'    |at_vs                 |character |"at" or "vs" home/away indicator.                   |
#'    |score                 |character |Final score string.                                  |
#'    |home_team_id          |character |ESPN home team identifier.                           |
#'    |away_team_id          |character |ESPN away team identifier.                           |
#'    |home_team_score       |character |Home team final score.                               |
#'    |away_team_score       |character |Away team final score.                               |
#'    |game_result           |character |Game result for the player's team (W/L/OT).          |
#'    |opponent_id           |character |ESPN opponent team identifier.                       |
#'    |opponent_name         |character |Opponent team display name.                          |
#'    |opponent_abbr         |character |Opponent team abbreviation.                          |
#'    |team_id               |character |Player's team ESPN identifier.                       |
#'    |team_abbr             |character |Player's team abbreviation.                          |
#'    |league_name           |character |League name.                                         |
#'    |league_abbreviation   |character |League abbreviation.                                 |
#'    |goals                 |character |Goals scored.                                        |
#'    |assists               |character |Assists.                                             |
#'    |points                |character |Points (G+A).                                        |
#'    |plus_minus            |character |Plus/minus.                                          |
#'    |penalty_minutes       |character |Penalty minutes.                                     |
#'    |shots_total           |character |Shots on goal.                                       |
#'    |shooting_pct          |character |Shooting percentage.                                 |
#'    |power_play_goals      |character |Power-play goals.                                    |
#'    |power_play_assists    |character |Power-play assists.                                  |
#'    |short_handed_goals    |character |Short-handed goals.                                  |
#'    |short_handed_assists  |character |Short-handed assists.                                |
#'    |game_winning_goals    |character |Game-winning goals.                                  |
#'    |time_on_ice_per_game  |character |Average time on ice.                                 |
#'    |production            |character |Production metric.                                   |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_gamelog(athlete_id = "5136656"))
#' }
espn_mch_player_gamelog <- function(athlete_id, season = NULL, ...) {
  .espn_hockey_player_gamelog(league     = "mens-college-hockey",
                               athlete_id = athlete_id,
                               season     = season, ...)
}

#' @name espn_mch_player_splits
NULL
#' @title **Get ESPN MCH Player Splits**
#' @rdname espn_mch_player_splits
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_splits()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per situational split:
#'
#'    |col_name                  |types     |description                                    |
#'    |:-------------------------|:---------|:----------------------------------------------|
#'    |athlete_id                |character |ESPN athlete identifier (echoed from arg).     |
#'    |season                    |integer   |Season end-year (echoed from arg).             |
#'    |split_category            |character |Split category name (e.g. split, location).   |
#'    |split_category_display    |character |Split category display name.                   |
#'    |split_display_name        |character |Individual split label (e.g. All Splits).      |
#'    |split_abbreviation        |character |Split abbreviation.                            |
#'    |games                     |character |Games played.                                  |
#'    |goals                     |character |Goals.                                         |
#'    |assists                   |character |Assists.                                       |
#'    |points                    |character |Points.                                        |
#'    |plus_minus                |character |Plus/minus.                                    |
#'    |penalty_minutes           |character |Penalty minutes.                               |
#'    |shots_total               |character |Shots on goal.                                 |
#'    |faceoff_percent           |character |Faceoff percentage.                            |
#'    |power_play_goals          |character |Power-play goals.                              |
#'    |power_play_assists        |character |Power-play assists.                            |
#'    |short_handed_goals        |character |Short-handed goals.                            |
#'    |short_handed_assists      |character |Short-handed assists.                          |
#'    |game_winning_goals        |character |Game-winning goals.                            |
#'    |time_on_ice_per_game      |character |Average time on ice per game.                  |
#'    |production                |character |Production metric.                             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_splits(athlete_id = "5136656", season = 2025))
#' }
espn_mch_player_splits <- function(athlete_id,
                                    season = most_recent_nhl_season(),
                                    ...) {
  .espn_hockey_player_splits(league     = "mens-college-hockey",
                              athlete_id = athlete_id,
                              season     = season,
                              ...)
}

#' @name espn_mch_leaders
NULL
#' @title **Get ESPN MCH League Leaders**
#' @rdname espn_mch_leaders
#' @author Saiem Gilani
#' @param category Stat category to display. For MCH one of `"offensive"`,
#'   `"defensive"`, `"general"`, `"penalties"`, or `NULL` for all
#'   categories. Defaults to `"offensive"`.
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`. **ESPN's MCH `statistics/byathlete` endpoint
#'   currently ignores this parameter and always returns the current/most-recent
#'   season.** The argument is accepted for forward-compatibility but does not
#'   filter upstream. The returned `season` and `season_type` columns reflect
#'   the season ESPN actually served, not this value.
#' @param season_type Season type code (1 = pre, 2 = regular, 3 = post).
#'   Defaults to `2`. **ESPN's MCH `statistics/byathlete` endpoint currently
#'   ignores this parameter.** The returned `season_type` column reflects the
#'   type ESPN actually served.
#' @param limit Athletes per page (default `50`).
#' @param page Page number (default `1`).
#' @param sort Optional ESPN `sort` query string.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per athlete:
#'
#'    |col_name                    |types     |description                                       |
#'    |:---------------------------|:---------|:-------------------------------------------------|
#'    |season                      |integer   |Season end-year ESPN actually served (from `currentSeason$year`). |
#'    |season_type                 |integer   |Season type id ESPN actually served (from `currentSeason$type$id`). |
#'    |requested_season_year       |integer   |Season year from `requestedSeason` (mirrors `season`). |
#'    |page                        |integer   |Current page.                                     |
#'    |pagination_count            |integer   |Total athlete count.                              |
#'    |pagination_limit            |integer   |Athletes per page.                                |
#'    |pagination_pages            |integer   |Total page count.                                 |
#'    |league_id                   |character |ESPN league identifier.                           |
#'    |league_name                 |character |League name.                                      |
#'    |league_abbreviation         |character |League abbreviation.                              |
#'    |athlete_id                  |character |ESPN athlete identifier.                          |
#'    |athlete_display_name        |character |Player display name.                              |
#'    |athlete_short_name          |character |Player short name.                                |
#'    |athlete_slug                |character |Player URL slug.                                  |
#'    |athlete_position            |character |Position abbreviation.                            |
#'    |athlete_jersey              |character |Jersey number.                                    |
#'    |athlete_team_id             |character |ESPN team identifier.                             |
#'    |athlete_team_name           |character |Team name.                                        |
#'    |athlete_team_short_name     |character |Team short name.                                  |
#'    |athlete_team_logo           |character |Team logo URL.                                    |
#'    |headshot_href               |character |Player headshot URL.                              |
#'    |debut_year                  |integer   |Debut year.                                       |
#'    |age                         |integer   |Player age.                                       |
#'    |offensive_goals             |character |Goals (offensive category).                       |
#'    |offensive_assists           |character |Assists.                                          |
#'    |offensive_points            |character |Points.                                           |
#'    |offensive_power_play_goals  |character |Power-play goals.                                 |
#'    |offensive_shots_total       |character |Shots on goal.                                    |
#'    |offensive_shooting_pct      |character |Shooting percentage.                              |
#'    |offensive_game_winning_goals|character |Game-winning goals.                               |
#'    |offensive_rank_points       |character |Rank by points.                                   |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_leaders(category = "offensive", limit = 25))
#' }
espn_mch_leaders <- function(category    = "offensive",
                              season      = most_recent_nhl_season(),
                              season_type = 2L,
                              limit       = 50L,
                              page        = 1L,
                              sort = NULL,
                              ...) {
  .espn_hockey_leaders(league      = "mens-college-hockey",
                        category    = category,
                        season      = season,
                        season_type = season_type,
                        limit       = limit,
                        page        = page,
                        sort = sort,
                        ...)
}

#' @name espn_mch_league_root
NULL
#' @title **Get ESPN MCH League Root (core-v2)**
#' @rdname espn_mch_league_root
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_league_root()`; columns verified against a captured MCH body.
#' @return A one-row `fastRhockey_data` tibble with league metadata:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | id | character | ESPN league identifier (e.g. "90" for MCH). |
#'    | guid | character | League global unique identifier. |
#'    | uid | character | League uid string. |
#'    | name | character | Full league name. |
#'    | display_name | character | League display name. |
#'    | abbreviation | character | League abbreviation (e.g. "MCH"). |
#'    | short_name | character | League short name. |
#'    | slug | character | League slug (e.g. "nhl"). |
#'    | is_tournament | logical | Whether the league is a tournament. |
#'    | gender | character | League gender designation. |
#'    | season_ref | character | `$ref` URL for the current season object. |
#'    | seasons_ref | character | `$ref` URL for the seasons collection. |
#'    | groups_ref | character | `$ref` URL for the groups collection. |
#'    | franchises_ref | character | `$ref` URL for the franchises collection. |
#'    | teams_ref | character | `$ref` URL for the teams collection. |
#'    | events_ref | character | `$ref` URL for the events collection. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_league_root())
#' }
espn_mch_league_root <- function(...) {
  .espn_hockey_league_root(league = "mens-college-hockey", ...)
}

#' @name espn_mch_season_pointer
NULL
#' @title **Get ESPN MCH Current Season Pointer (core-v2)**
#' @rdname espn_mch_season_pointer
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_pointer()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with the current season metadata:
#'
#'    |col_name           |types     |description                                      |
#'    |:------------------|:---------|:------------------------------------------------|
#'    |season             |integer   |Current season year (e.g. 2026).                 |
#'    |start_date         |character |Season start date (ISO 8601).                    |
#'    |end_date           |character |Season end date (ISO 8601).                      |
#'    |display_name       |character |Season display name (e.g. "2025-26").            |
#'    |current_type_id    |character |Current season-type identifier.                  |
#'    |current_type       |integer   |Current season-type code (1=pre,2=reg,3=post).   |
#'    |current_type_name  |character |Current season-type name.                        |
#'    |current_type_slug  |character |Current season-type slug.                        |
#'    |season_ref         |character |`$ref` URL for the full season object.           |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_pointer())
#' }
espn_mch_season_pointer <- function(...) {
  .espn_hockey_season_pointer(league = "mens-college-hockey", ...)
}

#' @name espn_mch_seasons
NULL
#' @title **Get ESPN MCH Seasons List (core-v2)**
#' @rdname espn_mch_seasons
#' @author Saiem Gilani
#' @param limit Maximum number of seasons to return (default `200`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_seasons()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per season:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | ref | character | `$ref` URL for the season object. |
#'    | season | integer | Season year parsed from the `$ref` URL. |
#'    | count | integer | Total number of seasons in the collection. |
#'    | page_count | integer | Total number of pages in the collection. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_seasons(limit = 10))
#' }
espn_mch_seasons <- function(limit = 200, ...) {
  .espn_hockey_seasons(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_season_info
NULL
#' @title **Get ESPN MCH Season Info (core-v2)**
#' @rdname espn_mch_season_info
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_info()`; columns verified against a captured MCH body.
#' @return A one-row `fastRhockey_data` tibble with season metadata:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | season | integer | Season year (echoed from arg). |
#'    | start_date | character | Season start date (ISO 8601). |
#'    | end_date | character | Season end date (ISO 8601). |
#'    | display_name | character | Season display name (e.g. "2025-26"). |
#'    | season_ref | character | `$ref` URL for this season. |
#'    | types_ref | character | `$ref` URL for the season-types collection. |
#'    | type_ref | character | `$ref` URL for the current season type. |
#'    | rankings_ref | character | `$ref` URL for rankings. |
#'    | coaches_ref | character | `$ref` URL for coaches. |
#'    | athletes_ref | character | `$ref` URL for athletes. |
#'    | awards_ref | character | `$ref` URL for awards. |
#'    | futures_ref | character | `$ref` URL for futures. |
#'    | leaders_ref | character | `$ref` URL for leaders. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_info(season = 2026))
#' }
espn_mch_season_info <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_info(league = "mens-college-hockey", season = season, ...)
}

#' @name espn_mch_season_types
NULL
#' @title **Get ESPN MCH Season Types (core-v2)**
#' @rdname espn_mch_season_types
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_types()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per season type
#'   (pre-season, regular, post-season, off-season):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | ref | character | `$ref` URL for the season-type object. |
#'    | season_type_id | integer | Season type id (1=pre, 2=reg, 3=post, 4=off). |
#'    | season | integer | Season year (echoed from arg). |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_types(season = 2026))
#' }
espn_mch_season_types <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_types(league = "mens-college-hockey", season = season, ...)
}

#' @name espn_mch_season_type
NULL
#' @title **Get ESPN MCH Season Type (core-v2)**
#' @rdname espn_mch_season_type
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_type()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with season-type metadata:
#'
#'    |col_name         |types     |description                                         |
#'    |:----------------|:---------|:---------------------------------------------------|
#'    |season           |integer   |Season year (echoed from arg).                      |
#'    |season_type      |integer   |Season type code (echoed from arg).                 |
#'    |id               |character |Season type identifier.                             |
#'    |type             |integer   |Season type numeric code.                           |
#'    |name             |character |Season type name (e.g. "Regular Season").           |
#'    |abbreviation     |character |Season type abbreviation (e.g. "reg").              |
#'    |slug             |character |Season type slug (e.g. "regular-season").           |
#'    |year             |integer   |Season year from the API.                           |
#'    |start_date       |character |Season type start date (ISO 8601).                  |
#'    |end_date         |character |Season type end date (ISO 8601).                    |
#'    |has_groups       |logical   |Whether this season type has groups.                |
#'    |has_standings    |logical   |Whether this season type has standings.             |
#'    |has_legs         |logical   |Whether this season type has legs.                  |
#'    |groups_ref       |character |`$ref` URL for the groups collection.               |
#'    |weeks_ref        |character |`$ref` URL for the weeks collection.                |
#'    |corrections_ref  |character |`$ref` URL for corrections.                         |
#'    |leaders_ref      |character |`$ref` URL for leaders.                             |
#'    |type_ref         |character |`$ref` URL for this season-type object.             |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_type(season = 2026, season_type = 2))
#' }
espn_mch_season_type <- function(season = most_recent_nhl_season(),
                                  season_type = 2,
                                  ...) {
  .espn_hockey_season_type(league = "mens-college-hockey", season = season,
                            season_type = season_type, ...)
}

#' @name espn_mch_season_group
NULL
#' @title **Get ESPN MCH Season Group (core-v2)**
#' @rdname espn_mch_season_group
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Use
#'   `espn_mch_season_groups()` to discover valid ids for a season/type.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_group()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with group metadata:
#'
#'    |col_name       |types     |description                                         |
#'    |:--------------|:---------|:---------------------------------------------------|
#'    |season         |integer   |Season year (echoed from arg).                      |
#'    |season_type    |integer   |Season type code (echoed from arg).                 |
#'    |group_id       |integer   |Group id (echoed from arg).                         |
#'    |id             |character |ESPN group identifier.                              |
#'    |uid            |character |Group uid string.                                   |
#'    |name           |character |Group name (e.g. "Eastern Conference").             |
#'    |abbreviation   |character |Group abbreviation (e.g. "East").                   |
#'    |slug           |character |Group slug.                                         |
#'    |is_conference  |logical   |Whether this group is a conference (vs. division).  |
#'    |group_ref      |character |`$ref` URL for this group object.                   |
#'    |season_ref     |character |`$ref` URL for the group's season.                  |
#'    |parent_ref     |character |`$ref` URL for the parent group (if any).           |
#'    |children_ref   |character |`$ref` URL for child groups (divisions).            |
#'    |standings_ref  |character |`$ref` URL for group standings.                     |
#'    |teams_ref      |character |`$ref` URL for the teams in this group.             |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_group(season = 2026, season_type = 2, group_id = 7))
#' }
espn_mch_season_group <- function(season = most_recent_nhl_season(),
                                   season_type = 2,
                                   group_id,
                                   ...) {
  .espn_hockey_season_group(league = "mens-college-hockey", season = season,
                             season_type = season_type, group_id = group_id, ...)
}

#' @name espn_mch_season_groups
NULL
#' @title **Get ESPN MCH Season Groups (core-v2)**
#' @rdname espn_mch_season_groups
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_groups()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per group
#'   (conference or division):
#'
#'    |col_name     |types     |description                                         |
#'    |:------------|:---------|:---------------------------------------------------|
#'    |ref          |character |`$ref` URL for the group object.                    |
#'    |group_id     |integer   |Group id parsed from the `$ref` URL.               |
#'    |season       |integer   |Season year (echoed from arg).                      |
#'    |season_type  |integer   |Season type code (echoed from arg).                 |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_groups(season = 2026, season_type = 2))
#' }
espn_mch_season_groups <- function(season = most_recent_nhl_season(),
                                    season_type = 2,
                                    ...) {
  .espn_hockey_season_groups(league = "mens-college-hockey", season = season,
                              season_type = season_type, ...)
}

#' @name espn_mch_season_group_teams
NULL
#' @title **Get ESPN MCH Season Group Teams (core-v2)**
#' @rdname espn_mch_season_group_teams
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Use
#'   `espn_mch_season_groups()` to discover valid ids for a season/type.
#' @param limit Maximum number of teams to return (default `500`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_group_teams()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name     |types     |description                                         |
#'    |:------------|:---------|:---------------------------------------------------|
#'    |ref          |character |`$ref` URL for the team-season object.              |
#'    |team_id      |character |ESPN team id parsed from the `$ref` URL.            |
#'    |season       |integer   |Season year (echoed from arg).                      |
#'    |season_type  |integer   |Season type code (echoed from arg).                 |
#'    |group_id     |integer   |Group id (echoed from arg).                         |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     grps <- espn_mch_season_groups(season = 2026, season_type = 2)
#'     espn_mch_season_group_teams(season = 2026, season_type = 2,
#'                                  group_id = grps$group_id[1])
#'   })
#' }
espn_mch_season_group_teams <- function(season = most_recent_nhl_season(),
                                         season_type = 2,
                                         group_id,
                                         limit = 500,
                                         ...) {
  .espn_hockey_season_group_teams(league = "mens-college-hockey", season = season,
                                   season_type = season_type,
                                   group_id = group_id, limit = limit, ...)
}

#' @name espn_mch_season_group_children
NULL
#' @title **Get ESPN MCH Season Group Children (core-v2)**
#' @rdname espn_mch_season_group_children
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Pass a
#'   conference-level `group_id` from `espn_mch_season_groups()` to retrieve
#'   its child divisions.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_group_children()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per child group (division
#'   under a conference). Conference-level groups typically have 2 division
#'   children each:
#'
#'    |col_name        |types     |description                                          |
#'    |:---------------|:---------|:----------------------------------------------------|
#'    |ref             |character |`$ref` URL for the child group object.               |
#'    |child_group_id  |character |Child group id parsed from the `$ref` URL.           |
#'    |season          |integer   |Season year (echoed from arg).                       |
#'    |season_type     |integer   |Season type code (echoed from arg).                  |
#'    |group_id        |integer   |Parent group id (echoed from arg).                   |
#'    |count           |integer   |Total children in the collection.                    |
#'    |page_count      |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     grps <- espn_mch_season_groups(season = 2026, season_type = 2)
#'     espn_mch_season_group_children(season = 2026, season_type = 2,
#'                                     group_id = grps$group_id[1])
#'   })
#' }
espn_mch_season_group_children <- function(season      = most_recent_nhl_season(),
                                            season_type = 2,
                                            group_id,
                                            ...) {
  .espn_hockey_season_group_children(league = "mens-college-hockey", season = season,
                                      season_type = season_type,
                                      group_id = group_id, ...)
}

#' @name espn_mch_season_type_leaders
NULL
#' @title **Get ESPN MCH Season Type Leaders (core-v2)**
#' @rdname espn_mch_season_type_leaders
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_type_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination. For a typical MCH regular season there are 9 categories
#'   with 25 leaders each (225 rows):
#'
#'    |col_name                     |types     |description                                       |
#'    |:----------------------------|:---------|:-------------------------------------------------|
#'    |category_name                |character |Stat category name (e.g. "goals", "assists").     |
#'    |category_display_name        |character |Category display name.                            |
#'    |category_short_display_name  |character |Category short display name.                      |
#'    |category_abbreviation        |character |Category abbreviation.                            |
#'    |display_value                |character |Leader stat display value.                        |
#'    |value                        |numeric   |Leader stat numeric value.                        |
#'    |rel                          |character |Relation type (e.g. "athlete").                   |
#'    |athlete_ref                  |character |`$ref` URL for the athlete.                       |
#'    |team_ref                     |character |`$ref` URL for the athlete's team.                |
#'    |statistics_ref               |character |`$ref` URL for the athlete's season statistics.   |
#'    |season                       |integer   |Season year (echoed from arg).                    |
#'    |season_type                  |integer   |Season type code (echoed from arg).               |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_type_leaders(season = 2026, season_type = 2))
#' }
espn_mch_season_type_leaders <- function(season      = most_recent_nhl_season(),
                                          season_type = 2,
                                          ...) {
  .espn_hockey_season_type_leaders(league = "mens-college-hockey", season = season,
                                    season_type = season_type, ...)
}

#' @name espn_mch_season_type_corrections
NULL
#' @title **Get ESPN MCH Season Type Corrections (core-v2)**
#' @rdname espn_mch_season_type_corrections
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_type_corrections()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per correction entry, or
#'   an empty tibble when no corrections are recorded. **Note**: ESPN does not
#'   publish score corrections for MCH — this endpoint returns empty for all
#'   tested seasons:
#'
#'    |col_name        |types     |description                                          |
#'    |:---------------|:---------|:----------------------------------------------------|
#'    |ref             |character |`$ref` URL for the correction object.                |
#'    |correction_id   |character |Correction id parsed from the `$ref` URL.            |
#'    |season          |integer   |Season year (echoed from arg).                       |
#'    |season_type     |integer   |Season type code (echoed from arg).                  |
#'    |count           |integer   |Total corrections in the collection.                 |
#'    |page_count      |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not publish corrections for hockey)
#'   try(espn_mch_season_type_corrections(season = 2026, season_type = 2))
#' }
espn_mch_season_type_corrections <- function(season      = most_recent_nhl_season(),
                                              season_type = 2,
                                              ...) {
  .espn_hockey_season_type_corrections(league = "mens-college-hockey", season = season,
                                        season_type = season_type, ...)
}

#' @name espn_mch_season_weeks
NULL
#' @title **Get ESPN MCH Season Weeks (core-v2)**
#' @rdname espn_mch_season_weeks
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_weeks()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per week, or an empty
#'   tibble when the endpoint returns no items. **Note**: The MCH does not use
#'   weekly scheduling in the ESPN core-v2 API — this endpoint consistently
#'   returns `count=0` for MCH. An empty tibble is returned gracefully:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the week object.                      |
#'    |week         |integer   |Week number parsed from the `$ref` URL.              |
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |count        |integer   |Total weeks in the collection (0 for MCH).           |
#'    |page_count   |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not populate weeks for hockey)
#'   try(espn_mch_season_weeks(season = 2026, season_type = 2))
#' }
espn_mch_season_weeks <- function(season      = most_recent_nhl_season(),
                                   season_type = 2,
                                   ...) {
  .espn_hockey_season_weeks(league = "mens-college-hockey", season = season,
                             season_type = season_type, ...)
}

#' @name espn_mch_season_week
NULL
#' @title **Get ESPN MCH Season Week (core-v2)**
#' @rdname espn_mch_season_week
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param week Week number (integer). Derive valid week numbers from
#'   `espn_mch_season_weeks()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_week()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with week metadata, or an empty
#'   tibble when the endpoint is unavailable. **Note**: NHL does not use weeks
#'   in ESPN core-v2 — HTTP 404 is the expected response:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |week         |integer   |Week number (echoed from arg).                       |
#'    |number       |integer   |Week number as returned by the API.                  |
#'    |start_date   |character |Week start date (ISO 8601).                          |
#'    |end_date     |character |Week end date (ISO 8601).                            |
#'    |text         |character |Week display text.                                   |
#'    |week_ref     |character |`$ref` URL for this week object.                     |
#'    |events_ref   |character |`$ref` URL for the events in this week.              |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
#'   try(espn_mch_season_week(season = 2026, season_type = 2, week = 1))
#' }
espn_mch_season_week <- function(season      = most_recent_nhl_season(),
                                  season_type = 2,
                                  week,
                                  ...) {
  .espn_hockey_season_week(league = "mens-college-hockey", season = season,
                            season_type = season_type, week = week, ...)
}

#' @name espn_mch_season_week_games
NULL
#' @title **Get ESPN MCH Season Week Games (core-v2)**
#' @rdname espn_mch_season_week_games
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param week Week number (integer). Derive valid week numbers from
#'   `espn_mch_season_weeks()`.
#' @param limit Maximum number of events to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_week_games()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per game (event) in the
#'   week, or an empty tibble when the endpoint is unavailable. **Note**: NHL
#'   does not use weeks in ESPN core-v2 — HTTP 404 is expected:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the event object.                     |
#'    |event_id     |character |ESPN event id parsed from the `$ref` URL.            |
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |week         |integer   |Week number (echoed from arg).                       |
#'    |count        |integer   |Total events in the collection.                      |
#'    |page_count   |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
#'   try(espn_mch_season_week_games(season = 2026, season_type = 2, week = 1))
#' }
espn_mch_season_week_games <- function(season      = most_recent_nhl_season(),
                                        season_type = 2,
                                        week,
                                        limit       = 100,
                                        ...) {
  .espn_hockey_season_week_games(league = "mens-college-hockey", season = season,
                                  season_type = season_type, week = week,
                                  limit = limit, ...)
}

#' @name espn_mch_season_teams
NULL
#' @title **Get ESPN MCH Season Teams (core-v2)**
#' @rdname espn_mch_season_teams
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of teams to return per page (default `100`).
#'   The MCH has 32 active teams; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_teams()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per team in the season:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the team-in-season object.           |
#'    |team_id     |character |ESPN team id parsed from the `$ref` URL.            |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |count       |integer   |Total teams in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_teams(season = 2026))
#' }
espn_mch_season_teams <- function(season = most_recent_nhl_season(),
                                   limit  = 100,
                                   ...) {
  .espn_hockey_season_teams(league = "mens-college-hockey", season = season,
                             limit = limit, ...)
}

#' @name espn_mch_season_team
NULL
#' @title **Get ESPN MCH Season Team (core-v2)**
#' @rdname espn_mch_season_team
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param team_id ESPN team identifier (character or numeric). Use
#'   `espn_mch_season_teams()` to discover valid team ids for a season.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_team()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with team-in-season metadata:
#'
#'    |col_name             |types     |description                                          |
#'    |:--------------------|:---------|:----------------------------------------------------|
#'    |season               |integer   |Season year (echoed from arg).                       |
#'    |team_id              |character |ESPN team id (echoed from arg).                      |
#'    |id                   |character |ESPN team identifier.                                |
#'    |guid                 |character |Team global unique identifier.                       |
#'    |uid                  |character |Team uid string.                                     |
#'    |slug                 |character |Team URL slug.                                       |
#'    |location             |character |Team city/location.                                  |
#'    |name                 |character |Team mascot/name.                                    |
#'    |nickname             |character |Team nickname.                                       |
#'    |abbreviation         |character |Team abbreviation.                                   |
#'    |display_name         |character |Team display name.                                   |
#'    |short_display_name   |character |Team short display name.                             |
#'    |color                |character |Team primary color hex.                              |
#'    |alternate_color      |character |Team alternate color hex.                            |
#'    |is_active            |logical   |Whether the team is currently active.                |
#'    |is_all_star          |logical   |Whether this is an all-star team.                    |
#'    |team_ref             |character |`$ref` URL for this team-in-season object.           |
#'    |record_ref           |character |`$ref` URL for the team record.                      |
#'    |athletes_ref         |character |`$ref` URL for the team athletes.                    |
#'    |venue_ref            |character |`$ref` URL for the team venue.                       |
#'    |groups_ref           |character |`$ref` URL for the team groups.                      |
#'    |statistics_ref       |character |`$ref` URL for the team statistics.                  |
#'    |leaders_ref          |character |`$ref` URL for the team leaders.                     |
#'    |injuries_ref         |character |`$ref` URL for the team injuries.                    |
#'    |awards_ref           |character |`$ref` URL for the team awards.                      |
#'    |franchise_ref        |character |`$ref` URL for the team franchise.                   |
#'    |events_ref           |character |`$ref` URL for the team events.                      |
#'    |transactions_ref     |character |`$ref` URL for the team transactions.                |
#'    |coaches_ref          |character |`$ref` URL for the team coaches.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     tms <- espn_mch_season_teams(season = 2026)
#'     espn_mch_season_team(season = 2026, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_season_team <- function(season  = most_recent_nhl_season(),
                                  team_id,
                                  ...) {
  .espn_hockey_season_team(league = "mens-college-hockey", season = season,
                            team_id = team_id, ...)
}

#' @name espn_mch_season_players
NULL
#' @title **Get ESPN MCH Season Players (core-v2)**
#' @rdname espn_mch_season_players
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Number of players per page (default `100`). The MCH season
#'   player list exceeds 11,000 entries across ~2,300 pages; use `page` to
#'   iterate.
#' @param page Page number (1-indexed, default `1`). Check the returned
#'   `page_count` column to determine how many pages are available.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_players()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per player reference for
#'   the requested page. This function hits the same endpoint as
#'   `espn_mch_season_athletes()` (`seasons/{season}/athletes`) but uses the
#'   `player_id` column name for consistency with the `sdv-py`
#'   `espn_mch_season_players` naming convention:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete-in-season object.         |
#'    |player_id   |character |ESPN player id parsed from the `$ref` URL.           |
#'    |season      |integer   |Season year (echoed from arg).                       |
#'    |page        |integer   |Page number (echoed from arg).                       |
#'    |count       |integer   |Total players in the collection.                     |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_players(season = 2026, limit = 10, page = 1))
#' }
espn_mch_season_players <- function(season = most_recent_nhl_season(),
                                     limit  = 100,
                                     page   = 1,
                                     ...) {
  .espn_hockey_season_players(league = "mens-college-hockey", season = season,
                               limit = limit, page = page, ...)
}

#' @name espn_mch_season_coaches
NULL
#' @title **Get ESPN MCH Season Coaches (core-v2)**
#' @rdname espn_mch_season_coaches
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of coaches to return (default `100`). The MCH
#'   typically lists ~30 head coaches per season.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_coaches()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per coach:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the coach-in-season object.          |
#'    |coach_id    |character |ESPN coach id parsed from the `$ref` URL.           |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |count       |integer   |Total coaches in the collection.                    |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_coaches(season = 2026))
#' }
espn_mch_season_coaches <- function(season = most_recent_nhl_season(),
                                     limit  = 100,
                                     ...) {
  .espn_hockey_season_coaches(league = "mens-college-hockey", season = season,
                               limit = limit, ...)
}

#' @name espn_mch_season_draft
NULL
#' @title **Get ESPN MCH Season Draft (core-v2)**
#' @rdname espn_mch_season_draft
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_draft()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with draft round references, or an
#'   empty tibble when the endpoint is unavailable (HTTP 404 is normal for
#'   NHL — the draft sub-tree is not served by this core-v2 path):
#'
#'    |col_name  |types     |description                                           |
#'    |:---------|:---------|:-----------------------------------------------------|
#'    |ref       |character |`$ref` URL for the draft round object (if returned).  |
#'    |draft_id  |character |Draft/round id parsed from the `$ref` URL.            |
#'    |season    |integer   |Season year (echoed from arg).                        |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try(espn_mch_season_draft(season = 2025))
#' }
espn_mch_season_draft <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_draft(league = "mens-college-hockey", season = season, ...)
}

#' @name espn_mch_season_draft_round_picks
NULL
#' @title **Get ESPN MCH Season Draft Round Picks (core-v2)**
#' @rdname espn_mch_season_draft_round_picks
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param round_num Draft round number (default `1`). Used to build the path
#'   `seasons/{season}/draft/rounds/{round_num}/picks`.
#' @param limit Maximum picks to return per page (default `100`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_draft_round_picks()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per draft pick, or an
#'   empty tibble when the endpoint is unavailable. The confirmed path is
#'   `seasons/{season}/draft/rounds/{round_num}/picks`; for NHL this returns
#'   HTTP 404 — the draft data is not served by the core-v2 sub-tree:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the draft pick object.               |
#'    |pick_id     |character |Pick id parsed from the `$ref` URL.                 |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |round_num   |integer   |Round number (echoed from arg).                     |
#'    |count       |integer   |Total picks in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try(espn_mch_season_draft_round_picks(season = 2025, round_num = 1))
#' }
espn_mch_season_draft_round_picks <- function(season    = most_recent_nhl_season(),
                                               round_num = 1,
                                               limit     = 100,
                                               ...) {
  .espn_hockey_season_draft_round_picks(league = "mens-college-hockey", season = season,
                                         round_num = round_num, limit = limit, ...)
}

#' @name espn_mch_season_futures
NULL
#' @title **Get ESPN MCH Season Futures (core-v2)**
#' @rdname espn_mch_season_futures
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_futures()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per futures market per
#'   betting provider. For a typical MCH season there are ~12 markets across
#'   1-2 providers:
#'
#'    |col_name             |types     |description                                          |
#'    |:--------------------|:---------|:----------------------------------------------------|
#'    |future_ref           |character |`$ref` URL for the futures market object.            |
#'    |future_id            |character |ESPN futures market identifier.                      |
#'    |future_name          |character |Futures market name (e.g. "MCH - Stanley Cup - Winner"). |
#'    |future_type          |character |Futures market type (e.g. "winLeague").              |
#'    |future_display_name  |character |Futures market display name.                         |
#'    |provider_id          |character |Betting provider identifier.                         |
#'    |provider_name        |character |Betting provider name (e.g. "ESPN BET").             |
#'    |provider_active      |logical   |Whether the provider is active.                      |
#'    |provider_priority    |integer   |Provider display priority.                           |
#'    |season               |integer   |Season year (echoed from arg).                       |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_futures(season = 2026))
#' }
espn_mch_season_futures <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_futures(league = "mens-college-hockey", season = season, ...)
}

#' @name espn_mch_season_freeagents
NULL
#' @title **Get ESPN MCH Season Free Agents (core-v2)**
#' @rdname espn_mch_season_freeagents
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of free agents to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_freeagents()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per free-agent athlete
#'   reference. **Note**: The MCH free-agents endpoint (`seasons/{season}/freeagents`)
#'   currently returns `count=0` for all tested seasons — the endpoint exists
#'   but ESPN does not populate it for MCH. An empty tibble is returned
#'   gracefully in that case:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete-in-season object.        |
#'    |athlete_id  |character |ESPN athlete id parsed from the `$ref` URL.         |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |count       |integer   |Total free agents in the collection (typically 0).  |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not populate this endpoint)
#'   try(espn_mch_season_freeagents(season = 2026))
#' }
espn_mch_season_freeagents <- function(season = most_recent_nhl_season(),
                                        limit  = 100,
                                        ...) {
  .espn_hockey_season_freeagents(league = "mens-college-hockey", season = season,
                                  limit = limit, ...)
}

#' @name espn_mch_season_powerindex
NULL
#' @title **Get ESPN MCH Season Power Index (core-v2)**
#' @rdname espn_mch_season_powerindex
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param team_id Optional ESPN team identifier (character or numeric). When
#'   provided, fetches power-index data for a single team. Defaults to `NULL`
#'   (collection for all teams).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_powerindex()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per power-index entry, or
#'   an empty tibble when no data is available. **Note**: ESPN does not publish
#'   power-index data for MCH — this endpoint consistently returns empty
#'   results for hockey:
#'
#'    |col_name        |types     |description                                      |
#'    |:---------------|:---------|:------------------------------------------------|
#'    |ref             |character |`$ref` URL for the power-index object.           |
#'    |powerindex_id   |character |Power-index id parsed from the `$ref` URL.       |
#'    |season          |integer   |Season year (echoed from arg).                   |
#'    |team_id         |character |Team id (echoed from arg, if supplied).          |
#'    |count           |integer   |Total entries in the collection.                 |
#'    |page_count      |integer   |Total pages in the collection.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not publish powerindex for hockey)
#'   try(espn_mch_season_powerindex(season = 2026))
#' }
espn_mch_season_powerindex <- function(season  = most_recent_nhl_season(),
                                        team_id = NULL,
                                        ...) {
  .espn_hockey_season_powerindex(league = "mens-college-hockey", season = season,
                                  team_id = team_id, ...)
}

#' @name espn_mch_season_powerindex_leaders
NULL
#' @title **Get ESPN MCH Season Power Index Leaders (core-v2)**
#' @rdname espn_mch_season_powerindex_leaders
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_powerindex_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per power-index leader, or
#'   an empty tibble when no data is available. **Note**: ESPN does not publish
#'   power-index data for MCH:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the power-index leader object.        |
#'    |leader_id   |character |Leader id parsed from the `$ref` URL.                |
#'    |season      |integer   |Season year (echoed from arg).                       |
#'    |count       |integer   |Total entries in the collection.                     |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not publish powerindex for hockey)
#'   try(espn_mch_season_powerindex_leaders(season = 2026))
#' }
espn_mch_season_powerindex_leaders <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_powerindex_leaders(league = "mens-college-hockey", season = season, ...)
}

#' @name espn_mch_season_awards
NULL
#' @title **Get ESPN MCH Season Awards (core-v2)**
#' @rdname espn_mch_season_awards
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Optional maximum number of awards (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_awards()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per award reference, or an
#'   empty tibble when no awards are available for the season. Awards are
#'   typically populated after the regular season ends:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the award object.                     |
#'    |award_id    |character |ESPN award id parsed from the `$ref` URL.            |
#'    |season      |integer   |Season year (echoed from arg).                       |
#'    |count       |integer   |Total awards in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_awards(season = 2025))
#' }
espn_mch_season_awards <- function(season = most_recent_nhl_season(), limit = NULL, ...) {
  .espn_hockey_season_awards(league = "mens-college-hockey", season = season, limit = limit, ...)
}

#' @name espn_mch_players_index
NULL
#' @title **Get ESPN MCH Players Index**
#' @rdname espn_mch_players_index
#' @author Saiem Gilani
#' @description Returns a paginated index of MCH athletes from ESPN. This is a
#'   site_v2-named parity alias for athletes_index; both wrap the
#'   same `core_v2 /athletes` backing endpoint. Prefer
#'   athletes_index for the established function with the same
#'   interface.
#' @param active Filter by active status. `TRUE` (default) = active players
#'   only; `FALSE` = inactive only; `NULL` = no filter.
#' @param limit Number of athletes per page (default `100`; max ~200).
#' @param page Page number (default `1`). Use the `page_count` column in the
#'   result to determine the total number of pages.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_players_index()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per athlete in the
#'   requested page:
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete object.                    |
#'    |athlete_id  |character |ESPN athlete id parsed from the `$ref` URL.           |
#'    |active      |logical   |The `active` filter value echoed from arg.            |
#'    |page        |integer   |The `page` value echoed from arg.                     |
#'    |count       |integer   |Total athlete count matching the filter.              |
#'    |page_count  |integer   |Total number of pages.                                |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @seealso athletes_index for the established alias.
#' @examples
#' \donttest{
#'   try(espn_mch_players_index(active = TRUE, limit = 10, page = 1))
#' }
espn_mch_players_index <- function(active = TRUE,
                                    limit  = 100,
                                    page   = 1,
                                    ...) {
  .espn_hockey_players_index(league = "mens-college-hockey",
                              active = active,
                              limit  = limit,
                              page   = page,
                              ...)
}

#' @name espn_mch_player_core
NULL
#' @title **Get ESPN MCH Player Core (core-v2)**
#' @rdname espn_mch_player_core
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_core()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with athlete metadata:
#'
#'    |col_name           |types     |description                                           |
#'    |:------------------|:---------|:-----------------------------------------------------|
#'    |athlete_id         |character |ESPN athlete id (echoed from arg).                    |
#'    |uid                |character |Athlete uid string.                                   |
#'    |guid               |character |Athlete global unique identifier.                     |
#'    |type               |character |Sport type (e.g. "hockey").                           |
#'    |first_name         |character |First name.                                           |
#'    |last_name          |character |Last name.                                            |
#'    |full_name          |character |Full display name.                                    |
#'    |display_name       |character |Display name.                                         |
#'    |short_name         |character |Short name (e.g. "P. Laine").                         |
#'    |weight             |numeric   |Weight in pounds.                                     |
#'    |display_weight     |character |Formatted weight string.                              |
#'    |height             |numeric   |Height in inches.                                     |
#'    |display_height     |character |Formatted height string.                              |
#'    |age                |integer   |Current age.                                          |
#'    |date_of_birth      |character |Date of birth (ISO 8601).                             |
#'    |debut_year         |integer   |Year of MCH debut.                                    |
#'    |birth_city         |character |Birth city.                                           |
#'    |birth_country      |character |Birth country abbreviation.                           |
#'    |slug               |character |URL slug.                                             |
#'    |jersey             |character |Jersey number.                                        |
#'    |hand               |character |Handedness (e.g. "L" or "R").                         |
#'    |active             |logical   |Whether athlete is currently active.                  |
#'    |position_ref       |character |`$ref` URL for the position object.                   |
#'    |team_ref           |character |`$ref` URL for the current team object.               |
#'    |statistics_ref     |character |`$ref` URL for career statistics.                     |
#'    |contracts_ref      |character |`$ref` URL for contracts.                             |
#'    |statisticslog_ref  |character |`$ref` URL for the statistics log.                    |
#'    |athlete_ref        |character |`$ref` URL for this athlete object.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_core(athlete_id = "5136656"))
#' }
espn_mch_player_core <- function(athlete_id, ...) {
  .espn_hockey_player_core(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_career_stats
NULL
#' @title **Get ESPN MCH Player Career Stats (core-v2)**
#' @rdname espn_mch_player_career_stats
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5136656"`). Obtain via [espn_mch_team_roster()].
#' @param stat_type Optional integer stat-type path segment (default `NULL`,
#'   which uses the base `/statistics` path). ESPN redirects the base path to
#'   `/statistics/0` (All Splits). Pass `0` explicitly for identical behaviour.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_career_stats()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with career aggregate statistics
#'   flattened wide across all stat categories. Columns vary by player type
#'   (skater vs goalie). Core columns include:
#'
#'    |col_name                    |types     |description                                          |
#'    |:---------------------------|:---------|:----------------------------------------------------|
#'    |athlete_id                  |character |ESPN athlete identifier (echoed from arg).           |
#'    |split_id                    |character |Split identifier (e.g. "0" = All Splits).            |
#'    |split_name                  |character |Split name (e.g. "All Splits").                      |
#'    |split_type                  |character |Split type (e.g. "total").                           |
#'    |split_abbr                  |character |Split abbreviation (e.g. "TOTAL").                   |
#'    |offensive_goals             |numeric   |Career goals.                                        |
#'    |offensive_assists           |numeric   |Career assists.                                      |
#'    |offensive_points            |numeric   |Career points.                                       |
#'    |general_games               |numeric   |Career games played.                                 |
#'    |penalties_penalty_minutes   |numeric   |Career penalty minutes.                              |
#'    |...                         |numeric   |Additional stat columns per category.                |
#'
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_career_stats(athlete_id = "5136656"))
#' }
espn_mch_player_career_stats <- function(athlete_id, stat_type = NULL, ...) {
  .espn_hockey_player_career_stats(league     = "mens-college-hockey",
                                    athlete_id = athlete_id,
                                    stat_type  = stat_type, ...)
}

#' @name espn_mch_player_statisticslog
NULL
#' @title **Get ESPN MCH Player Statistics Log (core-v2)**
#' @rdname espn_mch_player_statisticslog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_statisticslog()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per season-entry.
#'   This endpoint is the MCH game-log substitute (web-v3 gamelog 404s for MCH).
#'   Each row carries `$ref` URLs to follow for per-season totals and team splits.
#'
#'    |col_name          |types     |description                                          |
#'    |:-----------------|:---------|:----------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete id (echoed from arg).                   |
#'    |season            |integer   |Season year parsed from the season `$ref`.           |
#'    |season_ref        |character |`$ref` URL for the season object.                    |
#'    |total_stats_ref   |character |`$ref` URL for season totals statistics.             |
#'    |team_stats_ref    |character |`$ref` URL for team-level statistics.                |
#'    |team_ref          |character |`$ref` URL for the team object.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_statisticslog(athlete_id = "5136656"))
#' }
espn_mch_player_statisticslog <- function(athlete_id, ...) {
  .espn_hockey_player_statisticslog(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_eventlog
NULL
#' @title **Get ESPN MCH Player Event Log (core-v2)**
#' @rdname espn_mch_player_eventlog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_eventlog()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per event (game) in the
#'   current season. Use `statistics_ref` to retrieve per-game statistics.
#'
#'    |col_name         |types     |description                                           |
#'    |:----------------|:---------|:-----------------------------------------------------|
#'    |athlete_id       |character |ESPN athlete id (echoed from arg).                    |
#'    |event_id         |character |ESPN event (game) id.                                 |
#'    |team_id          |character |Team id the athlete played for in this event.         |
#'    |played           |logical   |Whether the athlete played in the event.              |
#'    |event_ref        |character |`$ref` URL for the event object.                      |
#'    |competition_ref  |character |`$ref` URL for the competition object.                |
#'    |statistics_ref   |character |`$ref` URL for per-game athlete statistics.           |
#'    |count            |integer   |Total event count for this athlete.                   |
#'    |page_count       |integer   |Total page count for this endpoint.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_eventlog(athlete_id = "5136656"))
#' }
espn_mch_player_eventlog <- function(athlete_id, ...) {
  .espn_hockey_player_eventlog(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_contracts
NULL
#' @title **Get ESPN MCH Player Contracts (core-v2)**
#' @rdname espn_mch_player_contracts
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_contracts()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per contract, or an empty
#'   data frame if no contract data is served. As of 2026-06-08, ESPN core-v2
#'   returns `count=0` for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#'
#'    |col_name     |types     |description                                           |
#'    |:------------|:---------|:-----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the contract object.                   |
#'    |contract_id  |character |Contract id parsed from the `$ref` URL.               |
#'    |athlete_id   |character |ESPN athlete id (echoed from arg).                    |
#'    |count        |integer   |Total contract count (0 in practice for NHL).         |
#'    |page_count   |integer   |Total page count.                                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_contracts(athlete_id = "5136656"))
#' }
espn_mch_player_contracts <- function(athlete_id, ...) {
  .espn_hockey_player_contracts(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_awards
NULL
#' @title **Get ESPN MCH Player Awards (core-v2)**
#' @rdname espn_mch_player_awards
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_awards()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per award, or an empty
#'   data frame if the athlete has no award entries. Each `$ref` follows to
#'   a season-award object (e.g. Hart Trophy, Conn Smythe, etc.).
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the award object.                      |
#'    |award_id    |character |Award id parsed from the `$ref` URL.                  |
#'    |athlete_id  |character |ESPN athlete id (echoed from arg).                    |
#'    |count       |integer   |Total award count.                                    |
#'    |page_count  |integer   |Total page count.                                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_awards(athlete_id = "5136656"))
#' }
espn_mch_player_awards <- function(athlete_id, ...) {
  .espn_hockey_player_awards(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_seasons
NULL
#' @title **Get ESPN MCH Player Seasons (core-v2)**
#' @rdname espn_mch_player_seasons
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_seasons()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/seasons` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint is not served for the NHL).
#'   For season-level data use `espn_mch_player_statisticslog()`.
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_seasons(athlete_id = "5136656"))
#' }
espn_mch_player_seasons <- function(athlete_id, ...) {
  .espn_hockey_player_seasons(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_records
NULL
#' @title **Get ESPN MCH Player Records (core-v2)**
#' @rdname espn_mch_player_records
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_records()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per record, or an empty
#'   data frame if no records are served. As of 2026-06-08, ESPN core-v2
#'   returns `count=0` for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the record object.                     |
#'    |record_id   |character |Record id parsed from the `$ref` URL.                 |
#'    |athlete_id  |character |ESPN athlete id (echoed from arg).                    |
#'    |count       |integer   |Total record count (0 in practice for NHL).           |
#'    |page_count  |integer   |Total page count.                                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_records(athlete_id = "5136656"))
#' }
espn_mch_player_records <- function(athlete_id, ...) {
  .espn_hockey_player_records(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_injuries
NULL
#' @title **Get ESPN MCH Player Injuries (core-v2)**
#' @rdname espn_mch_player_injuries
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_injuries()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/injuries` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   Use `espn_mch_injuries()` (site-v2) for league-level injury data.
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_injuries(athlete_id = "5136656"))
#' }
espn_mch_player_injuries <- function(athlete_id, ...) {
  .espn_hockey_player_injuries(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_notes
NULL
#' @title **Get ESPN MCH Player Notes (core-v2)**
#' @rdname espn_mch_player_notes
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_notes()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/notes` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_notes(athlete_id = "5136656"))
#' }
espn_mch_player_notes <- function(athlete_id, ...) {
  .espn_hockey_player_notes(league = "mens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_mch_player_vs_player
NULL
#' @title **Get ESPN MCH Player vs Player (core-v2)**
#' @rdname espn_mch_player_vs_player
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param opp_id ESPN opponent athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_vs_player()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{id}/vsathlete/{opp_id}` returns HTTP 404 for all queried NHL
#'   player pairs (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_player_vs_player(athlete_id = "5136656", opponent_id = "5136656"))
#' }
espn_mch_player_vs_player <- function(athlete_id, opp_id, ...) {
  .espn_hockey_player_vs_player(league = "mens-college-hockey", athlete_id = athlete_id,
                                 opponent_id = opp_id, ...)
}

#' @name espn_mch_games
NULL
#' @title **Get ESPN MCH Games (core-v2)**
#' @rdname espn_mch_games
#' @author Saiem Gilani
#' @param dates Optional date string `"YYYYMMDD"` or range `"YYYYMMDD-YYYYMMDD"`.
#'   When `NULL` (default) the most recent games are returned.
#' @param limit Maximum number of events per page (default `500`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_games()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per game event reference:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the event object.                     |
#'    |event_id    |character |ESPN event id parsed from the `$ref` URL.            |
#'    |count       |integer   |Total events in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'    |dates       |character |Dates filter (echoed from arg, if supplied).         |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_games(limit = 10))
#' }
espn_mch_games <- function(dates = NULL, limit = 500, ...) {
  .espn_hockey_games(league = "mens-college-hockey", dates = dates, limit = limit, ...)
}

#' @name espn_mch_game
NULL
#' @title **Get ESPN MCH Game Metadata (core-v2)**
#' @rdname espn_mch_game
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game()` (columns inherited); a collection may be empty.
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
#'    |competition_id    |character |Id of the primary competition (equals `event_id` for MCH). |
#'    |competition_ref   |character |`$ref` URL for the primary competition object.        |
#'    |venue_ref         |character |`$ref` URL for the venue.                             |
#'    |season_ref        |character |`$ref` URL for the season.                            |
#'    |season_type_ref   |character |`$ref` URL for the season type.                       |
#'    |league_ref        |character |`$ref` URL for the league.                            |
#'    |event_ref         |character |`$ref` URL for this event object.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game <- function(event_id, ...) {
  .espn_hockey_game(league = "mens-college-hockey", event_id = event_id, ...)
}

#' @name espn_mch_game_competition
NULL
#' @title **Get ESPN MCH Game Competition Detail (core-v2)**
#' @rdname espn_mch_game_competition
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id` (for MCH the single
#'   competition always has the same id as its parent event).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_competition()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_competition(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_competition <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_competition(league = "mens-college-hockey", event_id = event_id,
                                cid = cid, ...)
}

#' @name espn_mch_game_teams
NULL
#' @title **Get ESPN MCH Game Teams (Competitors Collection, core-v2)**
#' @rdname espn_mch_game_teams
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_teams()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per competitor (2 rows for
#'   a regular MCH game):
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_teams(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_teams <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_teams(league = "mens-college-hockey", event_id = event_id, cid = cid, ...)
}

#' @name espn_mch_game_team
NULL
#' @title **Get ESPN MCH Game Team (Competitor Detail, core-v2)**
#' @rdname espn_mch_game_team
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier. Use `espn_mch_game_teams()`
#'   to discover valid team ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_mch_game_teams(event_id = eid)
#'     espn_mch_game_team(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_game_team <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team(league = "mens-college-hockey", event_id = event_id, cid = cid,
                         team_id = team_id, ...)
}

#' @name espn_mch_game_team_roster
NULL
#' @title **Get ESPN MCH Game Team Roster (core-v2)**
#' @rdname espn_mch_game_team_roster
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_roster()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_mch_game_teams(event_id = eid)
#'     espn_mch_game_team_roster(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_game_team_roster <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_roster(league = "mens-college-hockey", event_id = event_id, cid = cid,
                                team_id = team_id, ...)
}

#' @name espn_mch_game_team_linescores
NULL
#' @title **Get ESPN MCH Game Team Linescores (core-v2)**
#' @rdname espn_mch_game_team_linescores
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_linescores()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_mch_game_teams(event_id = eid)
#'     espn_mch_game_team_linescores(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_game_team_linescores <- function(event_id, cid = event_id,
                                          team_id, ...) {
  .espn_hockey_game_team_linescores(league = "mens-college-hockey", event_id = event_id,
                                    cid = cid, team_id = team_id, ...)
}

#' @name espn_mch_game_team_statistics
NULL
#' @title **Get ESPN MCH Game Team Statistics (core-v2)**
#' @rdname espn_mch_game_team_statistics
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_statistics()` (columns inherited); a collection may be empty.
#' @return A wide one-row `fastRhockey_data` tibble. The first three columns are
#'   always present; the remaining stat columns are named `<category>_<stat>`
#'   (display value) and `<category>_<stat>_value` (numeric value) for each stat
#'   in each category. For a typical completed MCH game there are 5 stat
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_mch_game_teams(event_id = eid)
#'     espn_mch_game_team_statistics(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_game_team_statistics <- function(event_id, cid = event_id,
                                          team_id, ...) {
  .espn_hockey_game_team_statistics(league = "mens-college-hockey", event_id = event_id,
                                    cid = cid, team_id = team_id, ...)
}

#' @name espn_mch_game_team_record
NULL
#' @title **Get ESPN MCH Game Team Record (core-v2)**
#' @rdname espn_mch_game_team_record
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_record()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per record type (e.g.
#'   overall, home, road). **Note:** this endpoint returns 404 for most MCH
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_mch_game_teams(event_id = eid)
#'     espn_mch_game_team_record(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_game_team_record <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_record(league = "mens-college-hockey", event_id = event_id,
                                cid = cid, team_id = team_id, ...)
}

#' @name espn_mch_game_team_leaders
NULL
#' @title **Get ESPN MCH Game Team Leaders (core-v2)**
#' @rdname espn_mch_game_team_leaders
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier. Use `espn_mch_game_teams(event_id)$team_id`
#'   to find valid team ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader).
#'   For a typical completed MCH game there are 6 stat categories (goals, assists,
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_mch_game_teams(event_id = eid)
#'     espn_mch_game_team_leaders(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_mch_game_team_leaders <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_leaders(league = "mens-college-hockey", event_id = event_id,
                                 cid = cid, team_id = team_id, ...)
}

#' @name espn_mch_game_odds
NULL
#' @title **Get ESPN MCH Game Odds (core-v2)**
#' @rdname espn_mch_game_odds
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_odds()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_odds(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_odds <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_odds(league = "mens-college-hockey", event_id = event_id, cid = cid, ...)
}

#' @name espn_mch_game_probabilities
NULL
#' @title **Get ESPN MCH Game Win Probabilities (core-v2)**
#' @rdname espn_mch_game_probabilities
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param limit Maximum number of probability entries to return. Defaults to `300`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_probabilities()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per play probability entry. **Note:**
#'   This endpoint is sparse for MCH — most games return an empty tibble with a
#'   warning. When populated, columns include:
#'
#'    |col_name            |types     |description                                           |
#'    |:-------------------|:---------|:-----------------------------------------------------|
#'    |event_id            |character |ESPN event id (echoed from arg).                      |
#'    |cid                 |character |Competition id (echoed from arg).                     |
#'    |sequence_number     |integer   |Play sequence number this probability is tied to.     |
#'    |home_win_percentage |numeric   |Home team win probability (0-1).                      |
#'    |away_win_percentage |numeric   |Away team win probability (0-1).                      |
#'    |tie_percentage      |numeric   |Tie probability (0-1; typically 0 for MCH).           |
#'    |home_toss_up_change |numeric   |Change in home team win probability.                  |
#'    |away_toss_up_change |numeric   |Change in away team win probability.                  |
#'    |play_ref            |character |`$ref` URL for the associated play object.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_probabilities(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_probabilities <- function(event_id, cid = event_id,
                                        limit = 300, ...) {
  .espn_hockey_game_probabilities(league = "mens-college-hockey", event_id = event_id,
                                  cid = cid, limit = limit, ...)
}

#' @name espn_mch_game_plays
NULL
#' @title **Get ESPN MCH Game Plays (core-v2 Play-by-Play)**
#' @rdname espn_mch_game_plays
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id` (for MCH the single
#'   competition always has the same id as its parent event).
#' @param limit Maximum number of plays to return. Defaults to `1000`. A typical
#'   MCH game has 200-260 plays.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_plays()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per play:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | event_id | character | ESPN event id (echoed from arg). |
#'    | cid | character | Competition id (echoed from arg). |
#'    | id | character | ESPN play identifier. |
#'    | sequence_number | integer | Play sequence number within the game. |
#'    | type_id | character | Play type id. |
#'    | type_text | character | Play type label (e.g. "Goal", "Shot", "Face Off"). |
#'    | type_abbreviation | character | Play type abbreviation. |
#'    | text | character | Full play description text. |
#'    | short_text | character | Short play description text. |
#'    | period_number | integer | Period number (1-3 regulation, 4+ OT). |
#'    | period_display | character | Period display string (e.g. "1st"). |
#'    | clock_value | numeric | Clock value in seconds. |
#'    | clock_display | character | Clock display string (e.g. "19:34"). |
#'    | away_score | integer | Away team score after this play. |
#'    | home_score | integer | Home team score after this play. |
#'    | scoring_play | logical | Whether this play resulted in a goal. |
#'    | score_value | integer | Point value of the scoring play (1 for a goal). |
#'    | priority | logical | Whether this is a priority play for display. |
#'    | shooting_play | logical | Whether this is a shot on goal. |
#'    | is_penalty | logical | Whether this is a penalty play. |
#'    | wallclock | character | Wall-clock UTC timestamp of the play. |
#'    | coordinate_x | numeric | X coordinate on the ice surface. |
#'    | coordinate_y | numeric | Y coordinate on the ice surface. |
#'    | strength_id | character | Strength situation id (e.g. even strength, PP, SH). |
#'    | strength_text | character | Strength situation description. |
#'    | team_ref | character | `$ref` URL for the team associated with the play. |
#'    | play_ref | character | `$ref` URL for this play object. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_plays(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_plays <- function(event_id, cid = event_id, limit = 1000, ...) {
  .espn_hockey_game_plays(league = "mens-college-hockey", event_id = event_id,
                          cid = cid, limit = limit, ...)
}

#' @name espn_mch_game_play
NULL
#' @title **Get ESPN MCH Game Play Detail (core-v2)**
#' @rdname espn_mch_game_play
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id` (for MCH the single
#'   competition always has the same id as its parent event).
#' @param play_id ESPN play identifier. Use `espn_mch_game_plays(event_id)$id`
#'   to find valid play ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_play()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     pls <- espn_mch_game_plays(event_id = eid)
#'     espn_mch_game_play(event_id = eid, play_id = pls$id[1])
#'   })
#' }
espn_mch_game_play <- function(event_id, cid = event_id, play_id, ...) {
  .espn_hockey_game_play(league = "mens-college-hockey", event_id = event_id,
                         cid = cid, play_id = play_id, ...)
}

#' @name espn_mch_game_play_personnel
NULL
#' @title **Get ESPN MCH Game Play Personnel (core-v2)**
#' @rdname espn_mch_game_play_personnel
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param play_id ESPN play identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_play_personnel()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (competitor, athlete)
#'   pair on the play. For MCH this endpoint is typically sparse: the competitor
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_mch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     pls <- espn_mch_game_plays(event_id = eid)
#'     espn_mch_game_play_personnel(event_id = eid, play_id = pls$id[1])
#'   })
#' }
espn_mch_game_play_personnel <- function(event_id, cid = event_id, play_id, ...) {
  .espn_hockey_game_play_personnel(league = "mens-college-hockey", event_id = event_id,
                                   cid = cid, play_id = play_id, ...)
}

#' @name espn_mch_game_situation
NULL
#' @title **Get ESPN MCH Game Situation (core-v2)**
#' @rdname espn_mch_game_situation
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_situation()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_situation(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_situation <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_situation(league = "mens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_mch_game_status
NULL
#' @title **Get ESPN MCH Game Status (core-v2)**
#' @rdname espn_mch_game_status
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_status()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_status(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_status <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_status(league = "mens-college-hockey", event_id = event_id,
                            cid = cid, ...)
}

#' @name espn_mch_game_officials
NULL
#' @title **Get ESPN MCH Game Officials (core-v2)**
#' @rdname espn_mch_game_officials
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_officials()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_officials(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_officials <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_officials(league = "mens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_mch_game_broadcasts
NULL
#' @title **Get ESPN MCH Game Broadcasts (core-v2)**
#' @rdname espn_mch_game_broadcasts
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_broadcasts()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_broadcasts(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_broadcasts <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_broadcasts(league = "mens-college-hockey", event_id = event_id,
                               cid = cid, ...)
}

#' @name espn_mch_game_predictor
NULL
#' @title **Get ESPN MCH Game Predictor (core-v2)**
#' @rdname espn_mch_game_predictor
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_predictor()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with ESPN Predictor data. **Note:**
#'   This endpoint is sparse for MCH — most games return an empty tibble with a
#'   warning. When populated, scalar fields from the predictor object are returned
#'   as character columns alongside `event_id` and `cid`.
#'
#'   Columns when populated:
#'
#'    |col_name|type     |description                                                                                                                                                    |
#'    |:-------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------|
#'    |event_id|character|ESPN event identifier (echoed from the argument).                                                                                                              |
#'    |cid     |character|Competition identifier (echoed from the argument).                                                                                                             |
#'    |<field> |character|One column per scalar field of the ESPN predictor object, cleaned by `janitor::clean_names()`; `predictor_ref` (the `$ref` URL) when the object has no scalars.|
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_predictor(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_predictor <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_predictor(league = "mens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_mch_game_powerindex
NULL
#' @title **Get ESPN MCH Game Power Index (core-v2)**
#' @rdname espn_mch_game_powerindex
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_powerindex()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with ESPN Power Index data. **Note:**
#'   This endpoint is sparse for MCH — most games return an empty tibble with a
#'   warning. When populated, columns contain power-index values alongside
#'   `event_id` and `cid`.
#'
#'   Columns when populated:
#'
#'    |col_name|type     |description                                                                                                                                                 |
#'    |:-------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|
#'    |event_id|character|ESPN event identifier (echoed from the argument).                                                                                                           |
#'    |cid     |character|Competition identifier (echoed from the argument).                                                                                                          |
#'    |pi_ref  |character|`$ref` URL of the power-index item (list responses).                                                                                                        |
#'    |<field> |character|One column per scalar field of the ESPN power-index object (object responses), cleaned by `janitor::clean_names()`; `powerindex_ref` when it has no scalars.|
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_powerindex(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_powerindex <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_powerindex(league = "mens-college-hockey", event_id = event_id,
                               cid = cid, ...)
}

#' @name espn_mch_game_propbets
NULL
#' @title **Get ESPN MCH Game Prop Bets (core-v2)**
#' @rdname espn_mch_game_propbets
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_propbets()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per prop bet. **Note:** this
#'   endpoint returns 404 for most MCH games; an empty `data.frame()` is returned
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_propbets(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_propbets <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_propbets(league = "mens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_mch_game_leaders
NULL
#' @title **Get ESPN MCH Game Leaders (core-v2)**
#' @rdname espn_mch_game_leaders
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader) pair.
#'   **Note:** This endpoint is sparse for MCH — most games return an empty tibble
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_leaders(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_leaders <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_leaders(league = "mens-college-hockey", event_id = event_id, cid = cid, ...)
}

#' @name espn_mch_game_scoringplays
NULL
#' @title **Get ESPN MCH Game Scoring Plays (core-v2)**
#' @rdname espn_mch_game_scoringplays
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param limit Maximum number of plays to fetch for filtering. Defaults to `300`.
#'   Only used when the dedicated `scoringplays` endpoint is unavailable and the
#'   function falls back to filtering `game_plays()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_scoringplays()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per scoring play (goal).
#'   **Implementation note:** The dedicated `/scoringplays` core-v2 endpoint
#'   returns 404 for MCH; this function falls back to fetching all plays via
#'   `espn_mch_game_plays()` and filtering to rows where `scoring_play == TRUE`.
#'   Columns are the same as `espn_mch_game_plays()`:
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
#'    |score_value       |integer   |Always 1 for MCH goals.                               |
#'    |coordinate_x      |numeric   |X coordinate of the shot.                             |
#'    |coordinate_y      |numeric   |Y coordinate of the shot.                             |
#'    |strength_text     |character |Strength situation (e.g. "Even Strength", "Power Play"). |
#'    |team_ref          |character |`$ref` URL for the scoring team.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_mch_scoreboard(dates = "20250110")
#'     espn_mch_game_scoringplays(event_id = sb$game_id[1])
#'   })
#' }
espn_mch_game_scoringplays <- function(event_id, cid = event_id,
                                       limit = 300, ...) {
  .espn_hockey_game_scoringplays(league = "mens-college-hockey", event_id = event_id,
                                 cid = cid, limit = limit, ...)
}

#' @name espn_mch_game_official_detail
NULL
#' @title **Get ESPN MCH Game Official Detail (core-v2)**
#' @rdname espn_mch_game_official_detail
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param official_id ESPN official identifier. Use `espn_mch_game_officials(event_id)$official_id`
#'   to find valid official ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_official_detail()` (columns inherited); a collection may be empty.
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
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb   <- espn_mch_scoreboard(dates = "20250110")
#'     eid  <- sb$game_id[1]
#'     offs <- espn_mch_game_officials(event_id = eid)
#'     espn_mch_game_official_detail(event_id = eid, official_id = offs$official_id[1])
#'   })
#' }
espn_mch_game_official_detail <- function(event_id, cid = event_id,
                                          official_id, ...) {
  .espn_hockey_game_official_detail(league = "mens-college-hockey", event_id = event_id,
                                    cid = cid, official_id = official_id, ...)
}

#' @name espn_mch_teams_core
NULL
#' @title **Get ESPN MCH Teams Core (core-v2 Collection)**
#' @rdname espn_mch_teams_core
#' @author Saiem Gilani
#' @param limit Maximum number of teams to return (default `1000`; the MCH has
#'   32 active teams so the default returns all).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_teams_core()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name   |types     |description                                         |
#'    |:----------|:---------|:---------------------------------------------------|
#'    |ref        |character |Core-v2 `$ref` URL for the team resource.           |
#'    |team_id    |character |Team identifier (trailing path segment of ref URL). |
#'    |count      |integer   |Total number of teams in the collection.            |
#'    |page_count |integer   |Number of pages in the paginated response.          |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_teams_core(limit = 100))
#' }
espn_mch_teams_core <- function(limit = 1000, ...) {
  .espn_hockey_teams_core(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_team_core
NULL
#' @title **Get ESPN MCH Team Core (core-v2)**
#' @rdname espn_mch_team_core
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"104"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_core()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row containing enriched
#'   team metadata from the core-v2 endpoint, including `$ref` links to
#'   sub-resources:
#'
#'    |col_name            |types     |description                               |
#'    |:-------------------|:---------|:-----------------------------------------|
#'    |team_id             |character |ESPN team identifier (echoed from arg).   |
#'    |id                  |character |ESPN team identifier.                     |
#'    |guid                |character |Team global unique identifier.            |
#'    |uid                 |character |Team uid.                                 |
#'    |alternate_id_sdr    |character |SDR alternate identifier.                 |
#'    |slug                |character |URL slug.                                 |
#'    |location            |character |Team city/location.                       |
#'    |name                |character |Team mascot name.                         |
#'    |nickname            |character |Team nickname.                            |
#'    |abbreviation        |character |Team abbreviation.                        |
#'    |display_name        |character |Full display name.                        |
#'    |short_display_name  |character |Short display name.                       |
#'    |color               |character |Primary color hex.                        |
#'    |alternate_color     |character |Alternate color hex.                      |
#'    |is_active           |logical   |Whether the team is active.               |
#'    |is_all_star         |logical   |Whether the team is an all-star team.     |
#'    |logo                |character |Primary logo URL.                         |
#'    |record_ref          |character |Ref URL for team record resource.         |
#'    |athletes_ref        |character |Ref URL for team athletes resource.       |
#'    |venue_ref           |character |Ref URL for team venue resource.          |
#'    |groups_ref          |character |Ref URL for team groups resource.         |
#'    |statistics_ref      |character |Ref URL for team statistics resource.     |
#'    |leaders_ref         |character |Ref URL for team leaders resource.        |
#'    |injuries_ref        |character |Ref URL for team injuries resource.       |
#'    |awards_ref          |character |Ref URL for team awards resource.         |
#'    |franchise_ref       |character |Ref URL for franchise resource.           |
#'    |events_ref          |character |Ref URL for team events resource.         |
#'    |transactions_ref    |character |Ref URL for team transactions resource.   |
#'    |coaches_ref         |character |Ref URL for team coaches resource.        |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_team_core(team_id = "104"))
#' }
espn_mch_team_core <- function(team_id, ...) {
  .espn_hockey_team_core(league = "mens-college-hockey", team_id = team_id, ...)
}

#' @name espn_mch_venues
NULL
#' @title **Get ESPN MCH Venues (core-v2)**
#' @rdname espn_mch_venues
#' @author Saiem Gilani
#' @param limit Maximum number of venues to return (default `1000`). The MCH
#'   venue catalog contains ~180 entries; `200` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_venues()`; columns verified against a captured MCH body.
#' @return A `fastRhockey_data` tibble with one row per venue reference:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | ref | character | `$ref` URL for the venue object. |
#'    | venue_id | character | ESPN venue id parsed from the `$ref` URL. |
#'    | count | integer | Total venues in the collection. |
#'    | page_count | integer | Total pages in the collection. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_venues())
#' }
espn_mch_venues <- function(limit = 1000, ...) {
  .espn_hockey_venues(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_venue
NULL
#' @title **Get ESPN MCH Venue (core-v2)**
#' @rdname espn_mch_venue
#' @author Saiem Gilani
#' @param venue_id ESPN venue identifier (character or numeric). Use
#'   `espn_mch_venues()` to discover valid venue ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_venue()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with venue detail:
#'
#'    |col_name   |types     |description                                        |
#'    |:----------|:---------|:--------------------------------------------------|
#'    |venue_id   |character |ESPN venue id (echoed from arg).                   |
#'    |id         |character |ESPN venue identifier from response.               |
#'    |guid       |character |Venue global unique identifier (if present).       |
#'    |full_name  |character |Venue full name.                                   |
#'    |city       |character |Venue city.                                        |
#'    |state      |character |Venue state/province abbreviation.                 |
#'    |country    |character |Venue country.                                     |
#'    |grass      |logical   |Whether the surface is grass.                      |
#'    |indoor     |logical   |Whether the venue is indoors.                      |
#'    |capacity   |integer   |Seating capacity (if returned; often `NA` for MCH).|
#'    |venue_ref  |character |`$ref` URL for this venue object.                  |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     vns <- espn_mch_venues()
#'     espn_mch_venue(venue_id = vns$venue_id[1])
#'   })
#' }
espn_mch_venue <- function(venue_id, ...) {
  .espn_hockey_venue(league = "mens-college-hockey", venue_id = venue_id, ...)
}

#' @name espn_mch_franchises
NULL
#' @title **Get ESPN MCH Franchises (core-v2)**
#' @rdname espn_mch_franchises
#' @author Saiem Gilani
#' @param limit Maximum number of franchises to return (default `200`). The
#'   MCH has 32 active franchises; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_franchises()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per franchise reference:
#'
#'    |col_name      |types     |description                                        |
#'    |:-------------|:---------|:--------------------------------------------------|
#'    |ref           |character |`$ref` URL for the franchise object.               |
#'    |franchise_id  |character |ESPN franchise id parsed from the `$ref` URL.      |
#'    |count         |integer   |Total franchises in the collection.                |
#'    |page_count    |integer   |Total pages in the collection.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_franchises())
#' }
espn_mch_franchises <- function(limit = 200, ...) {
  .espn_hockey_franchises(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_franchise
NULL
#' @title **Get ESPN MCH Franchise (core-v2)**
#' @rdname espn_mch_franchise
#' @author Saiem Gilani
#' @param franchise_id ESPN franchise identifier (character or numeric). Use
#'   `espn_mch_franchises()` to discover valid franchise ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_franchise()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with franchise detail:
#'
#'    |col_name            |types     |description                                         |
#'    |:-------------------|:---------|:---------------------------------------------------|
#'    |franchise_id        |character |ESPN franchise id (echoed from arg).                |
#'    |id                  |character |ESPN franchise identifier from response.            |
#'    |uid                 |character |Franchise uid string.                               |
#'    |slug                |character |Franchise URL slug.                                 |
#'    |location            |character |Franchise city/location.                            |
#'    |name                |character |Franchise mascot name.                              |
#'    |nickname            |character |Franchise nickname.                                 |
#'    |abbreviation        |character |Franchise abbreviation.                             |
#'    |display_name        |character |Franchise display name.                             |
#'    |short_display_name  |character |Franchise short display name.                       |
#'    |color               |character |Franchise primary color hex.                        |
#'    |is_active           |logical   |Whether the franchise is currently active.          |
#'    |venue_ref           |character |`$ref` URL for the franchise's current venue.       |
#'    |team_ref            |character |`$ref` URL for the current season team object.      |
#'    |franchise_ref       |character |`$ref` URL for this franchise object.               |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     fcs <- espn_mch_franchises()
#'     espn_mch_franchise(franchise_id = fcs$franchise_id[1])
#'   })
#' }
espn_mch_franchise <- function(franchise_id, ...) {
  .espn_hockey_franchise(league = "mens-college-hockey", franchise_id = franchise_id, ...)
}

#' @name espn_mch_coach
NULL
#' @title **Get ESPN MCH Coach (core-v2)**
#' @rdname espn_mch_coach
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_mch_coaches()` to discover valid coach ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_coach()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with coach detail. Note that
#'   `careerRecords` and `coachSeasons` are sparse (empty arrays) for many MCH
#'   coaches — see `espn_mch_coach_record()` for season-scoped records:
#'
#'    |col_name       |types     |description                                        |
#'    |:--------------|:---------|:--------------------------------------------------|
#'    |coach_id       |character |ESPN coach id (echoed from arg).                   |
#'    |id             |character |ESPN coach identifier from response.               |
#'    |uid            |character |Coach uid string.                                  |
#'    |first_name     |character |Coach first name.                                  |
#'    |last_name      |character |Coach last name.                                   |
#'    |date_of_birth  |character |Coach date of birth (ISO 8601).                    |
#'    |birth_city     |character |Coach birth city.                                  |
#'    |birth_country  |character |Coach birth country.                               |
#'    |experience     |integer   |Years of coaching experience (if returned).        |
#'    |coach_ref      |character |`$ref` URL for this coach object.                  |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     ccs <- espn_mch_coaches(season = 2026)
#'     espn_mch_coach(coach_id = ccs$coach_id[1])
#'   })
#' }
espn_mch_coach <- function(coach_id, ...) {
  .espn_hockey_coach(league = "mens-college-hockey", coach_id = coach_id, ...)
}

#' @name espn_mch_coach_record
NULL
#' @title **Get ESPN MCH Coach Record (core-v2)**
#' @rdname espn_mch_coach_record
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_mch_coaches()` to discover valid coach ids.
#' @param record_type Record type path segment (`coaches/{id}/record/{record_type}`); default `0`, as sdv-py. `NULL` drops the segment.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_coach_record()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with coach career record data, or an
#'   empty tibble when the endpoint is unavailable. **Note**: The coach record
#'   path (`coaches/{id}/record`) returns HTTP 404 for NHL — this endpoint is
#'   not populated by ESPN for the NHL league. An empty tibble is returned
#'   gracefully with a warning:
#'
#'    |col_name   |types     |description                                            |
#'    |:----------|:---------|:------------------------------------------------------|
#'    |coach_id   |character |ESPN coach id (echoed from arg).                       |
#'    |...        |varies    |Additional record fields if returned (sparse for NHL). |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try({
#'     ccs <- espn_mch_coaches(season = 2026)
#'     espn_mch_coach_record(coach_id = ccs$coach_id[1])
#'   })
#' }
espn_mch_coach_record <- function(coach_id, record_type = 0, ...) {
  .espn_hockey_coach_record(league = "mens-college-hockey", coach_id = coach_id, record_type = record_type, ...)
}

#' @name espn_mch_coach_season
NULL
#' @title **Get ESPN MCH Coach Season (core-v2)**
#' @rdname espn_mch_coach_season
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_mch_season_coaches()` to find valid IDs.
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_coach_season()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per coach:
#'
#'    |col_name       |types     |description                                            |
#'    |:--------------|:---------|:------------------------------------------------------|
#'    |coach_id       |character |ESPN coach identifier (echoed from arg).               |
#'    |season         |integer   |Season year (echoed from arg).                         |
#'    |espn_id        |character |ESPN internal coach id.                                |
#'    |uid            |character |ESPN unique uid.                                       |
#'    |first_name     |character |Coach first name.                                      |
#'    |last_name      |character |Coach last name.                                       |
#'    |date_of_birth  |character |Date of birth (ISO 8601).                              |
#'    |birth_city     |character |Birth city.                                            |
#'    |birth_country  |character |Birth country.                                         |
#'    |person_ref     |character |`$ref` URL for the coach person object on core-v2.    |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_coach_season(coach_id = "900", season = 2026))
#' }
espn_mch_coach_season <- function(coach_id,
                                   season = most_recent_nhl_season(),
                                   ...) {
  .espn_hockey_coach_season(league   = "mens-college-hockey",
                              coach_id = coach_id,
                              season   = season,
                              coach_first = TRUE,
                              ...)
}

#' @name espn_mch_positions
NULL
#' @title **Get ESPN MCH Positions (core-v2)**
#' @rdname espn_mch_positions
#' @author Saiem Gilani
#' @param limit Maximum number of positions to return (default `200`). The
#'   MCH position catalog contains ~28 entries (positions + sub-positions).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_positions()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per position reference:
#'
#'    |col_name     |types     |description                                        |
#'    |:------------|:---------|:--------------------------------------------------|
#'    |ref          |character |`$ref` URL for the position object.                |
#'    |position_id  |character |ESPN position id parsed from the `$ref` URL.       |
#'    |count        |integer   |Total positions in the collection.                 |
#'    |page_count   |integer   |Total pages in the collection.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_positions())
#' }
espn_mch_positions <- function(limit = 200, ...) {
  .espn_hockey_positions(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_position
NULL
#' @title **Get ESPN MCH Position (core-v2)**
#' @rdname espn_mch_position
#' @author Saiem Gilani
#' @param position_id ESPN position identifier (character or numeric). Use
#'   `espn_mch_positions()` to discover valid position ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_position()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with position detail:
#'
#'    |col_name      |types     |description                                        |
#'    |:-------------|:---------|:--------------------------------------------------|
#'    |position_id   |character |ESPN position id (echoed from arg).                |
#'    |id            |character |ESPN position identifier from response.            |
#'    |name          |character |Position name (e.g. "Center").                     |
#'    |display_name  |character |Position display name.                             |
#'    |abbreviation  |character |Position abbreviation (e.g. "C").                  |
#'    |leaf          |logical   |Whether this is a leaf (non-parent) position.      |
#'    |parent_ref    |character |`$ref` URL for the parent position (if any).       |
#'    |position_ref  |character |`$ref` URL for this position object.               |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try({
#'     pos <- espn_mch_positions()
#'     espn_mch_position(position_id = pos$position_id[1])
#'   })
#' }
espn_mch_position <- function(position_id, ...) {
  .espn_hockey_position(league = "mens-college-hockey", position_id = position_id, ...)
}

#' @name espn_mch_tournaments
NULL
#' @title **Get ESPN MCH Tournaments (core-v2)**
#' @rdname espn_mch_tournaments
#' @author Saiem Gilani
#' @param limit Maximum number of tournaments to return (default `200`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_tournaments()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per tournament reference.
#'   The MCH tournaments endpoint is sparsely populated (typically 1 entry):
#'
#'    |col_name       |types     |description                                          |
#'    |:--------------|:---------|:----------------------------------------------------|
#'    |ref            |character |`$ref` URL for the tournament object.                |
#'    |tournament_id  |character |ESPN tournament id parsed from the `$ref` URL.       |
#'    |count          |integer   |Total tournaments in the collection.                 |
#'    |page_count     |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_tournaments())
#' }
espn_mch_tournaments <- function(limit = 200, ...) {
  .espn_hockey_tournaments(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_awards
NULL
#' @title **Get ESPN MCH Awards (core-v2)**
#' @rdname espn_mch_awards
#' @author Saiem Gilani
#' @param limit Maximum number of awards to return per page (default `200`).
#'   The MCH has 34 awards; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_awards()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per award:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the award object.                     |
#'    |award_id    |character |ESPN award id parsed from the `$ref` URL.            |
#'    |count       |integer   |Total awards in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_awards())
#' }
espn_mch_awards <- function(limit = 200, ...) {
  .espn_hockey_awards(league = "mens-college-hockey", limit = limit, ...)
}

#' @name espn_mch_award
NULL
#' @title **Get ESPN MCH Award (core-v2)**
#' @rdname espn_mch_award
#' @author Saiem Gilani
#' @param award_id ESPN award identifier (character or numeric). Use
#'   `espn_mch_awards()$award_id` to discover valid award ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_award()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with award metadata, or an
#'   empty tibble when the endpoint is unavailable. **Note**: The individual
#'   award endpoint (`core_v2/awards/{award_id}`) currently returns HTTP 500
#'   for all tested MCH award ids — an empty tibble is returned gracefully:
#'
#'    |col_name      |types     |description                                         |
#'    |:-------------|:---------|:---------------------------------------------------|
#'    |award_id      |character |Award id (echoed from arg).                         |
#'    |id            |character |ESPN award identifier (if returned).                |
#'    |name          |character |Award name (if returned).                           |
#'    |display_name  |character |Award display name (if returned).                   |
#'    |description   |character |Award description (if returned).                    |
#'    |award_ref     |character |`$ref` URL for the award object (if returned).      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN returns HTTP 500 for this endpoint)
#'   try({
#'     awards <- espn_mch_awards()
#'     if (nrow(awards) > 0) espn_mch_award(award_id = awards$award_id[1])
#'   })
#' }
espn_mch_award <- function(award_id, ...) {
  .espn_hockey_award(league = "mens-college-hockey", award_id = award_id, ...)
}

#' @name espn_mch_leaders_core
NULL
#' @title **Get ESPN MCH League Leaders (core-v2)**
#' @rdname espn_mch_leaders_core
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_leaders_core()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination. For a typical MCH season there are 7 stat categories with
#'   25 leaders each (175 rows). For season-specific / season-type leaders
#'   use [espn_mch_season_type_leaders()].
#'
#'    |col_name                     |types     |description                                       |
#'    |:----------------------------|:---------|:-------------------------------------------------|
#'    |category_name                |character |Stat category type (e.g. "offensive").            |
#'    |category_display_name        |character |Category display name (e.g. "Goals").             |
#'    |category_short_display_name  |character |Category short display name.                      |
#'    |category_abbreviation        |character |Category abbreviation.                            |
#'    |display_value                |character |Leader stat display value.                        |
#'    |value                        |numeric   |Leader stat numeric value.                        |
#'    |rel                          |character |Relation type (e.g. "athlete").                   |
#'    |athlete_ref                  |character |`$ref` URL for the athlete on core-v2.            |
#'    |statistics_ref               |character |`$ref` URL for the athlete statistics.            |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_leaders_core())
#' }
espn_mch_leaders_core <- function(...) {
  .espn_hockey_leaders_core(league = "mens-college-hockey", ...)
}

#' @name espn_mch_league_notes
NULL
#' @title **Get ESPN MCH League Notes (core-v2)**
#' @rdname espn_mch_league_notes
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_league_notes()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per league note reference,
#'   or an empty tibble when the endpoint is unavailable. **Note**: The MCH
#'   league notes endpoint (`core_v2/notes`) currently returns `count=0` —
#'   an empty tibble is returned gracefully:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the note object.                      |
#'    |note_id     |character |ESPN note id parsed from the `$ref` URL.             |
#'    |count       |integer   |Total notes in the collection (typically 0).         |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not populate this endpoint)
#'   try(espn_mch_league_notes())
#' }
espn_mch_league_notes <- function(...) {
  .espn_hockey_league_notes(league = "mens-college-hockey", ...)
}

#' @name espn_mch_talentpicks
NULL
#' @title **Get ESPN MCH Talent Picks (core-v2)**
#' @rdname espn_mch_talentpicks
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `mens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_talentpicks()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per talent pick reference,
#'   or an empty tibble when the endpoint is unavailable. **Note**: The MCH
#'   talent picks endpoint (`core_v2/talentpicks`) currently returns `count=0`
#'   — an empty tibble is returned gracefully:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the talent pick object.               |
#'    |pick_id     |character |ESPN pick id parsed from the `$ref` URL.             |
#'    |count       |integer   |Total talent picks in the collection (typically 0).  |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for MCH (ESPN does not populate this endpoint)
#'   try(espn_mch_talentpicks())
#' }
espn_mch_talentpicks <- function(...) {
  .espn_hockey_talentpicks(league = "mens-college-hockey", ...)
}

#' @name espn_mch_rankings
NULL
#' @title **Get ESPN MCH Rankings (Site v2 polls)**
#' @rdname espn_mch_rankings
#' @author Saiem Gilani
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details ESPN men's college hockey polls (USCHO), league slug `mens-college-hockey`. `rank` is `NA` on
#'   the vote-receiving rows (`ranked = FALSE`); `previous_rank` keeps ESPN's `0`
#'   for a team unranked in the previous poll. Mirrors sdv-py `espn_mch_rankings()`.
#' @return A `fastRhockey_data` tibble with one row per (poll, team), always with
#'   the same 25 columns; zero rows (with those columns) when no poll is published:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | poll_id | integer | ESPN poll identifier. |
#'    | poll_name | character | Poll name. |
#'    | poll_short_name | character | Poll short name. |
#'    | poll_type | character | Poll type code. |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type (2 = regular season, 3 = postseason). |
#'    | week | integer | Poll week within the season type. |
#'    | week_display | character | Poll week display string. |
#'    | poll_date | character | Poll date (ISO 8601). |
#'    | ranked | logical | `TRUE` for a ranked team, `FALSE` for a team receiving votes. |
#'    | team_id | character | ESPN team identifier. |
#'    | rank | integer | Current rank (`NA` when not ranked). |
#'    | previous_rank | integer | Previous poll rank (`0` = unranked). |
#'    | points | numeric | Poll points. |
#'    | first_place_votes | integer | First-place votes (`NA` when not published). |
#'    | trend | character | Rank trend indicator. |
#'    | record_summary | character | Team record summary. |
#'    | team_uid | character | ESPN team UID. |
#'    | team_location | character | Team location. |
#'    | team_name | character | Team name. |
#'    | team_nickname | character | Team nickname. |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_color | character | Team colour hex (`NA` when ESPN ships `"NULL"`). |
#'    | team_logo | character | Team logo URL. |
#'    | last_updated | character | Entry last-updated timestamp. |
#'
#' @importFrom dplyr bind_rows
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_rankings())
#' }
espn_mch_rankings <- function(...) {
  .espn_hockey_rankings(league = "mens-college-hockey", ...)
}

#' @name espn_mch_fpi
NULL
#' @title **Get ESPN MCH Power Index (fitt-v3 FPI)**
#' @rdname espn_mch_fpi
#' @author Saiem Gilani
#' @param season Season (4-digit END year) whose table to return; `NULL` (the
#'   default) returns the current season.
#' @param limit Page size. `NULL` (the default) leaves ESPN's default.
#' @param page Page number. `NULL` (the default) leaves ESPN's default.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. One row per team with a column per FPI field,
#'   snake-cased from the payload's `categories` labels (zipped by position and
#'   length-guarded); a zero-row `data.frame` when ESPN publishes no table for
#'   men's college hockey. Mirrors sdv-py `espn_mch_fpi()`.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name                |types     |description                                  |
#'    |:-----------------------|:---------|:--------------------------------------------|
#'    |team_id                 |character |ESPN team identifier.                        |
#'    |team_uid                |character |ESPN team UID.                               |
#'    |team_abbreviation       |character |Team abbreviation.                           |
#'    |team_display_name       |character |Team display name.                           |
#'    |team_short_display_name |character |Team short display name.                     |
#'    |team_nickname           |character |Team nickname.                               |
#'    |season                  |integer   |Requested season (END year).                 |
#'    |...                     |varies    |One column per FPI field in the payload.     |
#'
#' @importFrom dplyr bind_rows
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_fpi(season = 2026))
#' }
espn_mch_fpi <- function(season = NULL, limit = NULL, page = NULL, ...) {
  query <- list()
  if (!is.null(season)) query[["season"]] <- season
  if (!is.null(limit))  query[["limit"]]  <- limit
  if (!is.null(page))   query[["page"]]   <- page
  .espn_hockey_powerindex(league = "mens-college-hockey", kind = "fpi", query = query, ...)
}

#' @name espn_mch_season_week_powerindex
NULL
#' @title **Get ESPN MCH Weekly Power Index (core-v2)**
#' @rdname espn_mch_season_week_powerindex
#' @author Saiem Gilani
#' @param season Season END year (e.g. `2026`).
#' @param season_type Season type (1 = preseason, 2 = regular season, 3 = postseason).
#' @param week Week number within the season type.
#' @param limit Page size; `NULL` (the default) leaves ESPN's default.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. One row per team, `predictives` and
#'   `efficiencies` flattened to one column per field (a name colliding across
#'   the two blocks is suffixed). A zero-row `data.frame` when unpublished.
#'   Mirrors sdv-py `espn_mch_season_week_powerindex()`.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name          |types     |description                                  |
#'    |:-----------------|:---------|:--------------------------------------------|
#'    |team_id           |integer   |ESPN team identifier (parsed from the team `$ref`). |
#'    |season            |integer   |Season END year.                             |
#'    |last_updated      |character |When ESPN computed the snapshot.             |
#'    |run_date_time_key |character |ESPN run key for the snapshot.               |
#'    |...               |varies    |One column per predictive / efficiency field.|
#'
#' @importFrom dplyr bind_rows
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_week_powerindex(season = 2026, season_type = 2, week = 3))
#' }
espn_mch_season_week_powerindex <- function(season, season_type, week, limit = NULL, ...) {
  query <- list()
  if (!is.null(limit)) query[["limit"]] <- limit
  .espn_hockey_powerindex(
    league = "mens-college-hockey", kind = "weekly",
    path = paste0("seasons/", season, "/types/", season_type, "/weeks/", week, "/powerindex"),
    query = query, ...)
}

#' @name espn_mch_season_week_rankings
NULL
#' @title **Get ESPN MCH Weekly Rankings (core-v2)**
#' @rdname espn_mch_season_week_rankings
#' @author Saiem Gilani
#' @param season Season END year (e.g. `2026`).
#' @param season_type Season type (1 = preseason, 2 = regular season, 3 = postseason).
#' @param week Week number within the season type.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. Core-v2 `weeks/{week}/rankings` collection;
#'   `$ref`-only items become `ref` + `ranking_id`, inline items are flattened.
#'   Mirrors sdv-py `espn_mch_season_week_rankings()`.
#' @return A `fastRhockey_data` tibble with one row per ranking item:
#'
#'    |col_name     |types     |description                                  |
#'    |:------------|:---------|:--------------------------------------------|
#'    |ref          |character |`$ref` URL of the ranking object.            |
#'    |ranking_id  |character |Trailing id of the ranking `$ref`.           |
#'    |season       |integer   |Season END year (echoed from arg).           |
#'    |season_type  |integer   |Season type (echoed from arg).               |
#'    |week         |integer   |Week (echoed from arg).                      |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_week_rankings(season = 2026, season_type = 2, week = 3))
#' }
espn_mch_season_week_rankings <- function(season, season_type, week, ...) {
  .espn_hockey_items(
    league = "mens-college-hockey",
    path = paste0("seasons/", season, "/types/", season_type, "/weeks/", week, "/rankings"),
    id_col = "ranking_id", label = "Week Rankings",
    echo = list(season = as.integer(season), season_type = as.integer(season_type),
                week = as.integer(week)), ...)
}

#' @name espn_mch_season_recruits
NULL
#' @title **Get ESPN MCH Season Recruits (core-v2)**
#' @rdname espn_mch_season_recruits
#' @author Saiem Gilani
#' @param season Season END year (e.g. `2026`).
#' @param limit Maximum number of items per page. Defaults to `1000`.
#' @param page Page number. Defaults to `1`.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. Core-v2 `seasons/{season}/recruits`;
#'   `$ref`-only items become `ref` + `recruit_id`, inline items are flattened.
#'   Mirrors sdv-py `espn_mch_season_recruits()`; ESPN may publish nothing for men's college hockey.
#' @return A `fastRhockey_data` tibble with one row per recruit:
#'
#'    |col_name   |types     |description                                  |
#'    |:----------|:---------|:--------------------------------------------|
#'    |ref        |character |`$ref` URL of the recruit object.            |
#'    |recruit_id |character |Trailing id of the recruit `$ref`.           |
#'    |season     |integer   |Season END year (echoed from arg).           |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_season_recruits(season = 2026))
#' }
espn_mch_season_recruits <- function(season, limit = 1000, page = 1, ...) {
  .espn_hockey_items(
    league = "mens-college-hockey", path = paste0("seasons/", season, "/recruits"),
    id_col = "recruit_id", label = "Season Recruits",
    query = list(limit = limit, page = page),
    echo = list(season = as.integer(season)), ...)
}

#' @name espn_mch_recruiting_years
NULL
#' @title **Get ESPN MCH Recruiting Years (core-v2)**
#' @rdname espn_mch_recruiting_years
#' @author Saiem Gilani
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. Core-v2 `recruiting` collection (one item per
#'   recruiting class year). Mirrors sdv-py `espn_mch_recruiting_years()`.
#' @return A `fastRhockey_data` tibble with one row per recruiting year:
#'
#'    |col_name |types     |description                                  |
#'    |:--------|:---------|:--------------------------------------------|
#'    |ref      |character |`$ref` URL of the recruiting-year object.    |
#'    |year     |character |Trailing id of the `$ref` (the class year).  |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_recruiting_years())
#' }
espn_mch_recruiting_years <- function(...) {
  .espn_hockey_items(league = "mens-college-hockey", path = "recruiting", id_col = "year",
                     label = "Recruiting Years", ...)
}

#' @name espn_mch_recruiting_players
NULL
#' @title **Get ESPN MCH Recruiting Players (core-v2)**
#' @rdname espn_mch_recruiting_players
#' @author Saiem Gilani
#' @param year Recruiting class year.
#' @param limit Maximum number of items per page. Defaults to `1000`.
#' @param page Page number. Defaults to `1`.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. Core-v2 `recruiting/{year}/athletes`; inline
#'   athlete objects are flattened and snake-cased, `$ref`-only items become `ref` +
#'   `recruit_id`. Mirrors sdv-py `espn_mch_recruiting_players()`.
#' @return A `fastRhockey_data` tibble with one row per recruit:
#'
#'    |col_name   |types     |description                                  |
#'    |:----------|:---------|:--------------------------------------------|
#'    |ref        |character |`$ref` URL of the recruit (when ref-only).   |
#'    |recruit_id |character |Trailing id of the `$ref` (when ref-only).   |
#'    |year       |integer   |Recruiting class year (echoed from arg).     |
#'    |...        |varies    |Flattened athlete fields when ESPN inlines them. |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_recruiting_players(year = 2026))
#' }
espn_mch_recruiting_players <- function(year, limit = 1000, page = 1, ...) {
  .espn_hockey_items(
    league = "mens-college-hockey", path = paste0("recruiting/", year, "/athletes"),
    id_col = "recruit_id", label = "Recruiting Players",
    query = list(limit = limit, page = page),
    echo = list(year = as.integer(year)), ...)
}

#' @name espn_mch_recruiting_rankings
NULL
#' @title **Get ESPN MCH Recruiting Rankings (core-v2)**
#' @rdname espn_mch_recruiting_rankings
#' @author Saiem Gilani
#' @param year Recruiting class year.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. Core-v2 `recruiting/{year}/rankings`;
#'   `$ref`-only items become `ref` + `ranking_id`. Mirrors sdv-py
#'   `espn_mch_recruiting_rankings()`.
#' @return A `fastRhockey_data` tibble with one row per ranking set:
#'
#'    |col_name   |types     |description                                  |
#'    |:----------|:---------|:--------------------------------------------|
#'    |ref        |character |`$ref` URL of the ranking set.               |
#'    |ranking_id |character |Trailing id of the `$ref`.                   |
#'    |year       |integer   |Recruiting class year (echoed from arg).     |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_recruiting_rankings(year = 2026))
#' }
espn_mch_recruiting_rankings <- function(year, ...) {
  .espn_hockey_items(
    league = "mens-college-hockey", path = paste0("recruiting/", year, "/rankings"),
    id_col = "ranking_id", label = "Recruiting Rankings",
    echo = list(year = as.integer(year)), ...)
}

#' @name espn_mch_standings_core
NULL
#' @title **Get ESPN MCH Standings (core-v2)**
#' @rdname espn_mch_standings_core
#' @author Saiem Gilani
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `mens-college-hockey`. Core-v2 league-level `standings`; ESPN answers
#'   with a bare `$ref` pointer to the current season's group standings, returned
#'   as one `ref` row. Mirrors sdv-py `espn_mch_standings_core()`, which takes
#'   no arguments; use `espn_mch_standings()` for the per-team table.
#' @return A `fastRhockey_data` tibble with one row per standings item:
#'
#'    |col_name     |types     |description                                  |
#'    |:------------|:---------|:--------------------------------------------|
#'    |ref          |character |`$ref` URL of the standings object.          |
#'    |standings_id |character |Trailing id of the `$ref`.                   |
#'
#' @export
#' @family ESPN MCH Functions
#' @examples
#' \donttest{
#'   try(espn_mch_standings_core())
#' }
espn_mch_standings_core <- function(...) {
  .espn_hockey_items(league = "mens-college-hockey", path = "standings", id_col = "standings_id",
                     label = "Standings", ...)
}

# ===========================================================================
# wch: ESPN women's college hockey (NCAA)
# ===========================================================================

#' @name espn_wch_scoreboard
NULL
#' @title **Get ESPN WCH Scoreboard**
#' @rdname espn_wch_scoreboard
#' @author Saiem Gilani
#' @param dates Optional date or date-range string in `"YYYYMMDD"` or
#'   `"YYYYMMDD-YYYYMMDD"` format. When `NULL` (default) the current day
#'   is used.
#' @param week Optional week number (ESPN `week` query).
#' @param season_type Optional season phase (1 = pre, 2 = regular, 3 = post); sent as `seasontype`.
#' @param groups Optional conference/group id filter.
#' @param limit Optional maximum number of events (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_scoreboard()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per game:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | game_id | character | ESPN event identifier. |
#'    | uid | character | ESPN event uid. |
#'    | date | character | Game date-time (ISO 8601). |
#'    | name | character | Full game name. |
#'    | short_name | character | Short game name. |
#'    | season_year | integer | Season end year. |
#'    | season_type | integer | Season type code (1=pre, 2=regular, 3=post). |
#'    | season_slug | character | Season type slug. |
#'    | status_type_id | character | Status type identifier. |
#'    | status_type_name | character | Status type name. |
#'    | status_type_state | character | Status state (pre/in/post). |
#'    | status_type_completed | logical | Whether the game is complete. |
#'    | status_type_description | character | Status description. |
#'    | status_type_detail | character | Status detail text. |
#'    | status_type_short_detail | character | Short status detail. |
#'    | status_clock | numeric | Game clock in seconds. |
#'    | status_display_clock | character | Display clock string. |
#'    | status_period | integer | Current period. |
#'    | neutral_site | logical | Whether the game is at a neutral site. |
#'    | conference_competition | character | Whether it is a conference competition. |
#'    | attendance | integer | Game attendance. |
#'    | venue_id | character | ESPN venue identifier. |
#'    | venue_full_name | character | Venue full name. |
#'    | venue_city | character | Venue city. |
#'    | venue_state | character | Venue state. |
#'    | venue_indoor | logical | Whether the venue is indoors. |
#'    | broadcast | character | Broadcast network(s). |
#'    | note | character | Game note or headline. |
#'    | home_id | character | Home team ESPN identifier. |
#'    | home_name | character | Home team name. |
#'    | home_abbreviation | character | Home team abbreviation. |
#'    | home_display_name | character | Home team display name. |
#'    | home_location | character | Home team city/location. |
#'    | home_color | character | Home team primary color hex. |
#'    | home_alternate_color | character | Home team alternate color hex. |
#'    | home_logo | character | Home team logo URL. |
#'    | home_score | character | Home team score. |
#'    | home_winner | logical | Whether the home team won. |
#'    | home_rank | integer | Home team rank (if ranked). |
#'    | away_id | character | Away team ESPN identifier. |
#'    | away_name | character | Away team name. |
#'    | away_abbreviation | character | Away team abbreviation. |
#'    | away_display_name | character | Away team display name. |
#'    | away_location | character | Away team city/location. |
#'    | away_color | character | Away team primary color hex. |
#'    | away_alternate_color | character | Away team alternate color hex. |
#'    | away_logo | character | Away team logo URL. |
#'    | away_score | character | Away team score. |
#'    | away_winner | logical | Whether the away team won. |
#'    | away_rank | integer | Away team rank (if ranked). |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_scoreboard(dates = "20250110"))
#' }
espn_wch_scoreboard <- function(dates = NULL, week = NULL, season_type = NULL, groups = NULL, limit = NULL, ...) {
  .espn_hockey_scoreboard(league = "womens-college-hockey", dates = dates, week = week, season_type = season_type, groups = groups, limit = limit, ...)
}

#' @name espn_wch_summary
NULL
#' @title **Get ESPN WCH Raw Game Summary**
#' @rdname espn_wch_summary
#' @author Saiem Gilani
#' @param event_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_summary()` (columns inherited); a collection may be empty.
#' @return A nested R list representing the full parsed ESPN summary JSON.
#'   **This function does NOT return a tibble.** It is intended for power
#'   users who need fields not exposed by pbp,
#'   team_box, or player_box.
#'
#'   Top-level keys typically include:
#'   `boxscore`, `format`, `gameInfo`, `leaders`, `seasonseries`,
#'   `broadcasts`, `pickcenter`, `odds`, `plays`, `header`, `standings`,
#'   and others.
#'
#'   - `$boxscore$teams` — list of 2 team stat objects.
#'   - `$boxscore$players` — list of 2 player stat group objects.
#'   - `$plays` — list of play objects.
#'   - `$header$season` — season year and type.
#'   - `$header$competitions[[1]]$competitors` — home/away team + score.
#'
#'   Shape:
#'
#'    |col_name|type|description                                                   |
#'    |:-------|:---|:-------------------------------------------------------------|
#'    |(list)  |list|Named list, not a data frame. Top-level keys are listed above.|
#'
#' @importFrom jsonlite fromJSON
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   s <- try(espn_wch_summary(event_id = "401762970"))
#'   if (!inherits(s, "try-error")) {
#'     names(s)
#'     length(s$plays)
#'   }
#' }
espn_wch_summary <- function(event_id, ...) {
  .espn_hockey_summary(league = "womens-college-hockey", game_id = event_id, ...)
}

#' @name espn_wch_pbp
NULL
#' @title **Get ESPN WCH Play-by-Play**
#' @rdname espn_wch_pbp
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`. Parsed from the site-v2 `summary` payload by the same
#'   helper as `espn_nhl_pbp()`; columns verified against a captured wch summary body.
#' @return A `fastRhockey_data` tibble, one row per play:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | id | character | ESPN play identifier. |
#'    | sequence_number | character | Play sequence number. |
#'    | type_id | character | Play type identifier. |
#'    | type_text | character | Play type display text. |
#'    | text | character | Play description text. |
#'    | away_score | integer | Away team score at time of play. |
#'    | home_score | integer | Home team score at time of play. |
#'    | scoring_play | logical | Whether the play resulted in a goal. |
#'    | score_value | integer | Goal value (1 for regulation/OT goals). |
#'    | shooting_play | logical | Whether the play was a shot attempt. |
#'    | wallclock | character | Wall-clock timestamp (ISO 8601). |
#'    | period_number | integer | Period number (1-3 regulation, 4+ OT/SO). |
#'    | period_display_value | character | Period display label (e.g. "1st Period"). |
#'    | clock_display_value | character | Game clock display string (e.g. "14:32"). |
#'    | team_id | character | ESPN team identifier for the acting team. |
#'    | coordinate_x | numeric | X coordinate of play location. |
#'    | coordinate_y | numeric | Y coordinate of play location. |
#'    | strength_id | character | Strength situation identifier. |
#'    | strength_text | character | Strength situation text (e.g. "Even Strength"). |
#'    | shot_info_id | character | Shot type identifier. |
#'    | shot_info_text | character | Shot type text (e.g. "Wrist Shot"). |
#'    | athlete_id_1 | character | Primary participant athlete identifier. |
#'    | athlete_id_2 | character | Secondary participant athlete identifier. |
#'    | athlete_id_3 | character | Tertiary participant athlete identifier. |
#'    | home_team_id | character | Home team ESPN identifier. |
#'    | home_team_name | character | Home team name. |
#'    | home_team_location | character | Home team city/location. |
#'    | home_team_abbrev | character | Home team abbreviation. |
#'    | home_team_display_name | character | Home team display name. |
#'    | home_team_color | character | Home team primary color hex. |
#'    | home_team_alternate_color | character | Home team alternate color hex. |
#'    | home_team_logo | character | Home team logo URL. |
#'    | home_team_score | character | Home team final score. |
#'    | home_team_winner | logical | Whether the home team won. |
#'    | away_team_id | character | Away team ESPN identifier. |
#'    | away_team_name | character | Away team name. |
#'    | away_team_location | character | Away team city/location. |
#'    | away_team_abbrev | character | Away team abbreviation. |
#'    | away_team_display_name | character | Away team display name. |
#'    | away_team_color | character | Away team primary color hex. |
#'    | away_team_alternate_color | character | Away team alternate color hex. |
#'    | away_team_logo | character | Away team logo URL. |
#'    | away_team_score | character | Away team final score. |
#'    | away_team_winner | logical | Whether the away team won. |
#'    | game_id | character | ESPN event identifier (echoed from arg). |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type code. |
#'    | game_date | character | Game date-time (ISO 8601). |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_pbp(game_id = "401762970"))
#' }
espn_wch_pbp <- function(game_id, ...) {
  .espn_hockey_pbp(league = "womens-college-hockey", game_id = game_id, ...)
}

#' @name espn_wch_team_box
NULL
#' @title **Get ESPN WCH Team Box Score**
#' @rdname espn_wch_team_box
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`. Parsed from the site-v2 `summary` payload by the same
#'   helper as `espn_nhl_team_box()`; columns verified against a captured wch summary body.
#' @return A `fastRhockey_data` tibble, one row per team (2 rows):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | team_id | character | ESPN team identifier. |
#'    | team_uid | character | ESPN team uid. |
#'    | team_slug | character | Team URL slug. |
#'    | team_location | character | Team city/location. |
#'    | team_name | character | Team name (mascot). |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_display_name | character | Team display name. |
#'    | team_short_display_name | character | Team short display name. |
#'    | team_color | character | Team primary color hex. |
#'    | team_alternate_color | character | Team alternate color hex. |
#'    | team_logo | character | Team logo URL. |
#'    | home_away | character | "home" or "away". |
#'    | game_id | character | ESPN event identifier (echoed from arg). |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type code. |
#'    | game_date | character | Game date-time (ISO 8601). |
#'    | blocked_shots | character | Total blocked shots. |
#'    | hits | character | Total hits. |
#'    | takeaways | character | Total takeaways. |
#'    | shots_total | character | Total shots on goal. |
#'    | power_play_goals | character | Power play goals. |
#'    | power_play_opportunities | character | Power play opportunities. |
#'    | power_play_pct | character | Power play percentage. |
#'    | short_handed_goals | character | Short-handed goals. |
#'    | shootout_goals | character | Shootout goals. |
#'    | faceoffs_won | character | Faceoffs won. |
#'    | faceoff_percent | character | Faceoff win percentage. |
#'    | giveaways | character | Giveaways. |
#'    | penalties | character | Penalty count. |
#'    | penalty_minutes | character | Penalty minutes. |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_box(game_id = "401762970"))
#' }
espn_wch_team_box <- function(game_id, ...) {
  .espn_hockey_team_box(league = "womens-college-hockey", game_id = game_id, ...)
}

#' @name espn_wch_player_box
NULL
#' @title **Get ESPN WCH Player Box Score**
#' @rdname espn_wch_player_box
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`. Parsed from the site-v2 `summary` payload by the same
#'   helper as `espn_nhl_player_box()`; columns verified against a captured wch summary body.
#' @return A `fastRhockey_data` tibble, one row per player appearance (skaters and goalies bound; columns not kept for a position group are `NA`):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | athlete_id | character | ESPN athlete identifier. |
#'    | athlete_display_name | character | Athlete full display name. |
#'    | athlete_short_name | character | Athlete short name. |
#'    | athlete_last_name | character | Athlete last name. |
#'    | athlete_jersey | character | Jersey number. |
#'    | athlete_headshot_href | character | Headshot image URL. |
#'    | athlete_position_name | character | Position name. |
#'    | athlete_position_abbreviation | character | Position abbreviation. |
#'    | athlete_active | logical | Whether the athlete was active. |
#'    | athlete_scratched | logical | Whether the athlete was scratched. |
#'    | stat_group | character | Stat group name ("forwards","defenses","goalies"). |
#'    | team_id | character | ESPN team identifier. |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_display_name | character | Team display name. |
#'    | home_away | character | "home" or "away". |
#'    | team_score | character | Team final score. |
#'    | team_winner | logical | Whether the team won. |
#'    | opponent_team_id | character | Opponent team identifier. |
#'    | opponent_team_abbreviation | character | Opponent team abbreviation. |
#'    | game_id | character | ESPN event identifier (echoed from arg). |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type code. |
#'    | game_date | character | Game date-time (ISO 8601). |
#'    | bs | character | Blocked shots (skaters). |
#'    | ht | character | Hits (skaters). |
#'    | tk | character | Takeaways (skaters). |
#'    | plus_minus | character | Plus/minus rating (skaters). |
#'    | toi | character | Time on ice (skaters, MM:SS). |
#'    | pptoi | character | Power play time on ice (skaters, MM:SS). |
#'    | shtoi | character | Short-handed time on ice (skaters, MM:SS). |
#'    | estoi | character | Even-strength time on ice (skaters, MM:SS). |
#'    | shft | character | Total shifts (skaters). |
#'    | g | character | Goals (skaters). |
#'    | ytdg | character | Year-to-date goals (skaters/goalies). |
#'    | a | character | Assists (skaters). |
#'    | s | character | Shots (skaters). |
#'    | sm | character | Missed shots (skaters). |
#'    | sog | character | Shots on goal (skaters). |
#'    | fw | character | Faceoffs won (skaters). |
#'    | fl | character | Faceoffs lost (skaters). |
#'    | fo_percent | character | Faceoff percentage (skaters). |
#'    | gv | character | Giveaways (skaters). |
#'    | pn | character | Penalties taken (skaters). |
#'    | pim | character | Penalty minutes (skaters/goalies). |
#'    | ga | character | Goals against (goalies). |
#'    | sa | character | Shots against (goalies). |
#'    | sos | character | Shootout saves (goalies). |
#'    | sosa | character | Shootout shots against (goalies). |
#'    | sv | character | Saves (goalies). |
#'    | sv_percent | character | Save percentage (goalies). |
#'    | essv | character | Even-strength saves (goalies). |
#'    | ppsv | character | Power play saves (goalies). |
#'    | shsv | character | Short-handed saves (goalies). |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_box(game_id = "401762970"))
#' }
espn_wch_player_box <- function(game_id, ...) {
  .espn_hockey_player_box(league = "womens-college-hockey", game_id = game_id, ...)
}

#' @name espn_wch_calendar
NULL
#' @title **Get ESPN WCH Calendar**
#' @rdname espn_wch_calendar
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_calendar()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per game date:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | date | character | Game date (ISO 8601 datetime string). |
#'    | calendar_type | character | Calendar type (e.g. `"day"`). |
#'    | calendar_start_date | character | Calendar season start date. |
#'    | calendar_end_date | character | Calendar season end date. |
#'    | calendar_is_whitelist | logical | Whether dates are a whitelist of game days. |
#'
#' @importFrom dplyr mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_calendar())
#' }
espn_wch_calendar <- function(...) {
  .espn_hockey_calendar(league = "womens-college-hockey", ...)
}

#' @name espn_wch_news
NULL
#' @title **Get ESPN WCH News**
#' @rdname espn_wch_news
#' @author Saiem Gilani
#' @param limit Maximum number of articles to return (default `50`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_news()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per article:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | article_id | character | ESPN article identifier. |
#'    | now_id | character | ESPN Now identifier. |
#'    | content_key | character | Content management key. |
#'    | type | character | Article type (e.g. "Story", "Media"). |
#'    | headline | character | Article headline. |
#'    | description | character | Article description/excerpt. |
#'    | last_modified | character | ISO 8601 last-modified datetime. |
#'    | published | character | ISO 8601 published datetime. |
#'    | byline | character | Author byline. |
#'    | premium | logical | Whether the article is premium content. |
#'    | image_href | character | URL of the lead image. |
#'    | web_href | character | URL of the article on ESPN.com. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_news(limit = 10))
#' }
espn_wch_news <- function(limit = 50, ...) {
  .espn_hockey_news(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_injuries
NULL
#' @title **Get ESPN WCH Injuries**
#' @rdname espn_wch_injuries
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_injuries()`; ESPN answered an empty collection for every captured MCH body (2026-10), so the columns below are the NHL helper's and unverified for this league.
#' @return A `fastRhockey_data` tibble with one row per injured player:
#'
#'    |col_name               |types     |description                                    |
#'    |:----------------------|:---------|:----------------------------------------------|
#'    |team_id                |character |ESPN team identifier.                          |
#'    |team_display_name      |character |Team display name.                             |
#'    |player_id              |character |ESPN player identifier.                        |
#'    |player_display_name    |character |Player display name.                           |
#'    |player_short_name      |character |Player short name.                             |
#'    |player_position        |character |Player position abbreviation.                  |
#'    |injury_id              |character |ESPN injury identifier.                        |
#'    |injury_status          |character |Injury status (e.g. "Questionable", "Out").    |
#'    |injury_date            |character |Injury date (ISO 8601).                        |
#'    |injury_short_comment   |character |Short injury comment.                          |
#'    |injury_long_comment    |character |Long injury comment.                           |
#'    |injury_type            |character |Injury type name.                              |
#'    |injury_abbreviation    |character |Injury type abbreviation.                      |
#'    |detail_side            |character |Injured side (e.g. "Left", "Right").           |
#'    |detail_fantasy_status  |character |Fantasy relevance status.                      |
#'    |detail_return_date     |character |Projected return date.                         |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_injuries())
#' }
espn_wch_injuries <- function(...) {
  .espn_hockey_injuries(league = "womens-college-hockey", ...)
}

#' @name espn_wch_transactions
NULL
#' @title **Get ESPN WCH Transactions**
#' @rdname espn_wch_transactions
#' @author Saiem Gilani
#' @param limit Optional maximum number of transactions (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_transactions()`; ESPN answered an empty collection for every captured MCH body (2026-10), so the columns below are the NHL helper's and unverified for this league.
#' @return A `fastRhockey_data` tibble with one row per transaction:
#'
#'    |col_name           |types     |description                                         |
#'    |:------------------|:---------|:---------------------------------------------------|
#'    |date               |character |Transaction date (ISO 8601).                        |
#'    |description        |character |Transaction description.                            |
#'    |team_id            |character |ESPN team identifier.                               |
#'    |team_location      |character |Team city/location.                                 |
#'    |team_name          |character |Team mascot/name.                                   |
#'    |team_abbreviation  |character |Team abbreviation.                                  |
#'    |team_display_name  |character |Team display name.                                  |
#'    |team_color         |character |Team primary color hex.                             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_transactions())
#' }
espn_wch_transactions <- function(limit = NULL, ...) {
  .espn_hockey_transactions(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_conferences
NULL
#' @title **Get ESPN WCH Conferences**
#' @rdname espn_wch_conferences
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_conferences()`; columns verified against a captured MCH body (same helper; no WCH body captured).
#' @return A `fastRhockey_data` tibble with one row per conference/division
#'   combination (4 rows for WCH: 2 conferences x 2 divisions each):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | conference_name | character | Conference name (e.g. "Eastern Conference"). |
#'    | conference_abbreviation | character | Conference abbreviation. |
#'    | division_name | character | Division name (e.g. "Atlantic Division"). |
#'    | division_abbreviation | character | Division abbreviation. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_conferences())
#' }
espn_wch_conferences <- function(...) {
  .espn_hockey_conferences(league = "womens-college-hockey", ...)
}

#' @name espn_wch_statistics_league
NULL
#' @title **Get ESPN WCH League Statistics**
#' @rdname espn_wch_statistics_league
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_statistics_league()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (stat category, leader).
#'   The ESPN WCH statistics endpoint returns a sparse set of top-of-league
#'   stat leaders organized by category. If the endpoint returns no categories,
#'   an empty tibble is returned gracefully:
#'
#'    |col_name              |types     |description                                     |
#'    |:---------------------|:---------|:-----------------------------------------------|
#'    |category_name         |character |Stat category name (e.g. "goals", "assists").   |
#'    |category_abbreviation |character |Category abbreviation.                          |
#'    |display_value         |character |Leader stat display value.                      |
#'    |value                 |numeric   |Leader stat numeric value.                      |
#'    |athlete_id            |character |ESPN athlete identifier.                        |
#'    |athlete_display_name  |character |Athlete display name.                           |
#'    |team_id               |character |ESPN team identifier.                           |
#'    |team_abbreviation     |character |Team abbreviation.                              |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_statistics_league())
#' }
espn_wch_statistics_league <- function(...) {
  .espn_hockey_statistics_league(league = "womens-college-hockey", ...)
}

#' @name espn_wch_draft
NULL
#' @title **Get ESPN WCH Draft**
#' @rdname espn_wch_draft
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_draft()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with draft data, or an empty tibble
#'   when the endpoint is unavailable. **Note**: The WCH draft endpoint
#'   (`site/v2/sports/hockey/nhl/draft`) currently returns HTTP 500 for WCH —
#'   an empty tibble is returned gracefully:
#'
#'    |col_name |types     |description                                           |
#'    |:--------|:---------|:-----------------------------------------------------|
#'    |id       |character |Draft entry identifier (if returned).                 |
#'    |name     |character |Draft entry name (if returned).                       |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN returns HTTP 500 for this endpoint)
#'   try(espn_wch_draft())
#' }
espn_wch_draft <- function(...) {
  .espn_hockey_draft(league = "womens-college-hockey", ...)
}

#' @name espn_wch_teams_site
NULL
#' @title **Get ESPN WCH Teams (site_v2) -- alias for `espn_wch_teams()`**
#' @rdname espn_wch_teams_site
#' @author Saiem Gilani
#' @param limit Optional maximum number of teams (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_teams_site()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per WCH team. This is
#'   identical to the output of teams.
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | abbreviation | character | Team abbreviation. |
#'    | display_name | character | Team display name. |
#'    | espn_team_id | integer | ESPN team identifier. |
#'    | team | character | Team location/city name. |
#'    | logo | character | Light-background logo URL. |
#'    | logo_dark | character | Dark-background logo URL. |
#'    | logos_href_3 | character | Logo variant 3 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_4 | character | Logo variant 4 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_5 | character | Logo variant 5 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_6 | character | Logo variant 6 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_7 | character | Logo variant 7 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_8 | character | Logo variant 8 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_9 | character | Logo variant 9 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_10 | character | Logo variant 10 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_11 | character | Logo variant 11 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_12 | character | Logo variant 12 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_13 | character | Logo variant 13 URL (ESPN sends a varying number of logos per team). |
#'    | logos_href_14 | character | Logo variant 14 URL (ESPN sends a varying number of logos per team). |
#'    | mascot | character | Team mascot/nickname. |
#'    | nickname | character | Team nickname. |
#'    | short_name | character | Short team display name. |
#'
#' @importFrom dplyr group_by ungroup select rename
#' @importFrom tidyr unnest_wider
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @seealso teams which this function wraps.
#' @examples
#' \donttest{
#'   try(espn_wch_teams_site())
#' }
espn_wch_teams_site <- function(limit = NULL, ...) {
  .espn_hockey_teams(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_team
NULL
#' @title **Get ESPN WCH Team Detail**
#' @rdname espn_wch_team
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | team_id | character | ESPN team identifier (echoed from arg). |
#'    | id | character | ESPN team identifier. |
#'    | uid | character | ESPN team uid. |
#'    | slug | character | URL slug. |
#'    | location | character | Team city/location. |
#'    | name | character | Team mascot name. |
#'    | nickname | character | Team nickname. |
#'    | abbreviation | character | Team abbreviation. |
#'    | display_name | character | Full display name. |
#'    | short_display_name | character | Short display name. |
#'    | color | character | Primary color hex. |
#'    | alternate_color | character | Alternate color hex. |
#'    | is_active | logical | Whether the team is active. |
#'    | logo | character | Primary logo URL. |
#'    | logo_dark | character | Dark logo URL. |
#'    | stand_summary | character | Standing summary string. |
#'    | franchise_ref | character | Franchise resource ref URL. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team(team_id = "2137"))
#' }
espn_wch_team <- function(team_id, ...) {
  .espn_hockey_team(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_team_roster
NULL
#' @title **Get ESPN WCH Team Roster**
#' @rdname espn_wch_team_roster
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Pittsburgh Penguins).
#' @param limit Optional maximum number of athletes (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_roster()`; ESPN answered an empty collection for every captured WCH body (2026-10), so the columns below are the NHL helper's and unverified for this league.
#' @return A `fastRhockey_data` tibble with one row per player:
#'
#'    |col_name                      |types     |description                         |
#'    |:-----------------------------|:---------|:-----------------------------------|
#'    |position_group                |character |Position group name (e.g. Centers). |
#'    |id                            |character |ESPN player identifier.             |
#'    |uid                           |character |ESPN player uid.                    |
#'    |guid                          |character |Player global unique identifier.    |
#'    |alternate_id                  |character |Alternate player identifier.        |
#'    |alternate_ids_sdr             |character |Alternate ids sdr.                  |
#'    |first_name                    |character |First name.                         |
#'    |last_name                     |character |Last name.                          |
#'    |full_name                     |character |Full name.                          |
#'    |display_name                  |character |Display name.                       |
#'    |short_name                    |character |Short name.                         |
#'    |weight                        |numeric   |Weight (lbs).                       |
#'    |display_weight                |character |Display weight string.              |
#'    |height                        |numeric   |Height (inches).                    |
#'    |display_height                |character |Display height string.              |
#'    |age                           |integer   |Player age.                         |
#'    |date_of_birth                 |character |Date of birth.                      |
#'    |slug                          |character |URL slug.                           |
#'    |jersey                        |character |Jersey number.                      |
#'    |headshot_href                 |character |Headshot image URL.                 |
#'    |headshot_alt                  |character |Headshot alt text.                  |
#'    |hand_type                     |character |Shooting/catching hand type.        |
#'    |hand_abbreviation             |character |Hand abbreviation.                  |
#'    |hand_display_value            |character |Hand display value.                 |
#'    |position_id                   |character |Position identifier.                |
#'    |position_name                 |character |Position name.                      |
#'    |position_display_name         |character |Position display name.              |
#'    |position_abbreviation         |character |Position abbreviation.              |
#'    |position_leaf                 |logical   |Whether position is a leaf node.    |
#'    |position_parent_id            |character |Parent position identifier.         |
#'    |position_parent_name          |character |Parent position name.               |
#'    |position_parent_display_name  |character |Parent position display name.       |
#'    |position_parent_abbreviation  |character |Parent position abbreviation.       |
#'    |position_parent_leaf          |logical   |Whether parent position is leaf.    |
#'    |experience_years              |integer   |Experience years.                   |
#'    |debut_year                    |numeric   |Debut year.                         |
#'    |status_id                     |character |Status identifier.                  |
#'    |status_name                   |character |Status name.                        |
#'    |status_type                   |character |Status type.                        |
#'    |status_abbreviation           |character |Status abbreviation.                |
#'    |birth_place_city              |character |Birth place city.                   |
#'    |birth_place_state             |character |Birth place state.                  |
#'    |birth_place_country           |character |Birth place country.                |
#'    |birth_place_display_text      |character |Birth place display text.           |
#'    |birth_country_abbreviation    |character |Birth country abbreviation.         |
#'    |college_id                    |character |College identifier.                 |
#'    |college_guid                  |character |College guid.                       |
#'    |college_mascot                |character |College mascot.                     |
#'    |college_name                  |character |College name.                       |
#'    |college_short_name            |character |College short name.                 |
#'    |college_abbrev                |character |College abbreviation.               |
#'    |college_logos                 |character |College logo URLs (pipe-delimited). |
#'    |team_id                       |character |ESPN team identifier (from arg).    |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_roster(team_id = "2137"))
#' }
espn_wch_team_roster <- function(team_id, limit = NULL, ...) {
  .espn_hockey_team_roster(league = "womens-college-hockey", team_id = team_id, limit = limit, ...)
}

#' @name espn_wch_team_schedule
NULL
#' @title **Get ESPN WCH Team Schedule (site_v2) -- alias for `espn_wch_schedule()`**
#' @rdname espn_wch_team_schedule
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Pittsburgh Penguins).
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_schedule()`; columns verified against a captured MCH body (same helper; no WCH body captured).
#' @return A `fastRhockey_data` tibble with one row per game. Identical to
#'   schedule.
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | game_id | character | ESPN event identifier. |
#'    | date | character | Game date-time (ISO 8601). |
#'    | name | character | Full game name. |
#'    | short_name | character | Short game name. |
#'    | season_year | integer | Season end year. |
#'    | season_type | integer | Season type code. |
#'    | time_valid | logical | Whether the game time is confirmed. |
#'    | status_type_id | character | Status type identifier. |
#'    | status_type_name | character | Status type name. |
#'    | status_type_state | character | Status state (pre/in/post). |
#'    | status_type_completed | logical | Whether the game is complete. |
#'    | neutral_site | logical | Whether the game is at a neutral site. |
#'    | attendance | integer | Game attendance. |
#'    | venue_id | character | Venue identifier. |
#'    | venue_full_name | character | Venue full name. |
#'    | venue_city | character | Venue city. |
#'    | venue_state | character | Venue state. |
#'    | broadcast | character | Broadcast network(s). |
#'    | home_id | character | Home team ESPN identifier. |
#'    | home_name | character | Home team display name. |
#'    | home_abbreviation | character | Home team abbreviation. |
#'    | home_location | character | Home team city. |
#'    | home_logo | character | Home team logo URL. |
#'    | home_score | character | Home team score. |
#'    | home_winner | logical | Whether the home team won. |
#'    | away_id | character | Away team ESPN identifier. |
#'    | away_name | character | Away team display name. |
#'    | away_abbreviation | character | Away team abbreviation. |
#'    | away_location | character | Away team city. |
#'    | away_logo | character | Away team logo URL. |
#'    | away_score | character | Away team score. |
#'    | away_winner | logical | Whether the away team won. |
#'    | team_id | character | ESPN team identifier (echoed from arg). |
#'    | season | integer | Season end-year (echoed from arg). |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @seealso schedule which this function wraps.
#' @examples
#' \donttest{
#'   try(espn_wch_team_schedule(team_id = "2137", season = 2025))
#' }
espn_wch_team_schedule <- function(team_id,
                                    season = most_recent_nhl_season(),
                                    ...) {
  .espn_hockey_schedule(league = "womens-college-hockey", team_id = team_id,
                         season = season, ...)
}

#' @name espn_wch_team_record
NULL
#' @title **Get ESPN WCH Team Record**
#' @rdname espn_wch_team_record
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_record()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per record type
#'   (overall/home/away) when data is available. The `/record` endpoint
#'   currently returns an empty response for WCH; an empty tibble is returned
#'   with a cli warning. Columns when populated include `team_id`,
#'   `description`, `type`, `summary`, and one numeric column per stat.
#'
#'   Columns when populated:
#'
#'    |col_name   |type     |description                                                                                     |
#'    |:----------|:--------|:-----------------------------------------------------------------------------------------------|
#'    |team_id    |character|ESPN team identifier (echoed from the argument).                                                |
#'    |description|character|Record description.                                                                             |
#'    |type       |character|Record type (overall, home, away).                                                              |
#'    |summary    |character|Record summary string, e.g. `'10-5-1'`.                                                         |
#'    |<stat>     |numeric  |One column per ESPN stat on the record, named by `janitor::make_clean_names()` of the stat name.|
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_record(team_id = "2137"))
#' }
espn_wch_team_record <- function(team_id, ...) {
  .espn_hockey_team_record(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_team_depthcharts
NULL
#' @title **Get ESPN WCH Team Depth Charts**
#' @rdname espn_wch_team_depthcharts
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_depthcharts()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row containing team and season
#'   metadata. NHL depth-slot player assignments are not served at this
#'   endpoint; the response contains team/season context only:
#'
#'    |col_name         |types     |description                             |
#'    |:----------------|:---------|:---------------------------------------|
#'    |team_id          |character |ESPN team identifier (echoed from arg). |
#'    |timestamp        |character |Response timestamp (ISO 8601).          |
#'    |status           |character |Response status string.                 |
#'    |season_year      |integer   |Season year.                            |
#'    |season_type      |integer   |Season type code.                       |
#'    |season_name      |character |Season type name.                       |
#'    |team_espn_id     |character |ESPN team identifier.                   |
#'    |abbreviation     |character |Team abbreviation.                      |
#'    |location         |character |Team city/location.                     |
#'    |name             |character |Team mascot name.                       |
#'    |display_name     |character |Full display name.                      |
#'    |color            |character |Primary color hex.                      |
#'    |logo             |character |Team logo URL.                          |
#'    |record_summary   |character |Record summary string.                  |
#'    |season_summary   |character |Season summary string.                  |
#'    |standing_summary |character |Standing summary string.                |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_depthcharts(team_id = "2137"))
#' }
espn_wch_team_depthcharts <- function(team_id, ...) {
  .espn_hockey_team_depthcharts(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_team_injuries
NULL
#' @title **Get ESPN WCH Team Injuries**
#' @rdname espn_wch_team_injuries
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_injuries()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per injury entry when data
#'   is available. The `/injuries` endpoint currently returns an empty response
#'   for WCH; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `athlete_id`, `athlete_display_name`,
#'   `athlete_position`, `injury_type`, `injury_status`, `injury_date`, and
#'   `injury_description`.
#'
#'   Columns when populated:
#'
#'    |col_name            |type     |description                                     |
#'    |:-------------------|:--------|:-----------------------------------------------|
#'    |team_id             |character|ESPN team identifier (echoed from the argument).|
#'    |athlete_id          |character|ESPN athlete identifier.                        |
#'    |athlete_display_name|character|Athlete display name.                           |
#'    |athlete_position    |character|Athlete position.                               |
#'    |injury_type         |character|Injury type.                                    |
#'    |injury_status       |character|Injury status.                                  |
#'    |injury_date         |character|Injury date.                                    |
#'    |injury_description  |character|Long comment or description.                    |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_injuries(team_id = "2137"))
#' }
espn_wch_team_injuries <- function(team_id, ...) {
  .espn_hockey_team_injuries(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_team_transactions
NULL
#' @title **Get ESPN WCH Team Transactions**
#' @rdname espn_wch_team_transactions
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_transactions()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per transaction when data
#'   is available. The `/transactions` endpoint currently returns an empty
#'   response for WCH; an empty tibble is returned with a cli warning. Columns
#'   when populated include `team_id`, `id`, `date`, `description`, and `type`.
#'
#'   Columns when populated:
#'
#'    |col_name   |type     |description                                     |
#'    |:----------|:--------|:-----------------------------------------------|
#'    |team_id    |character|ESPN team identifier (echoed from the argument).|
#'    |id         |character|Transaction identifier.                         |
#'    |date       |character|Transaction date.                               |
#'    |description|character|Transaction description.                        |
#'    |type       |character|Transaction type.                               |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_transactions(team_id = "2137"))
#' }
espn_wch_team_transactions <- function(team_id, ...) {
  .espn_hockey_team_transactions(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_team_history
NULL
#' @title **Get ESPN WCH Team History**
#' @rdname espn_wch_team_history
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_history()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per season entry when data
#'   is available. The `/history` endpoint currently returns an empty response
#'   for WCH; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `season`, `wins`, `losses`, `ties`,
#'   `ot_losses`, `points`, `summary`, and `description`.
#'
#'   Columns when populated:
#'
#'    |col_name   |type     |description                                     |
#'    |:----------|:--------|:-----------------------------------------------|
#'    |team_id    |character|ESPN team identifier (echoed from the argument).|
#'    |season     |integer  |Season year.                                    |
#'    |wins       |integer  |Wins.                                           |
#'    |losses     |integer  |Losses.                                         |
#'    |ties       |integer  |Ties.                                           |
#'    |ot_losses  |integer  |Overtime losses.                                |
#'    |points     |integer  |Points.                                         |
#'    |summary    |character|Record summary string.                          |
#'    |description|character|Season description.                             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_history(team_id = "2137"))
#' }
espn_wch_team_history <- function(team_id, ...) {
  .espn_hockey_team_history(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_team_news
NULL
#' @title **Get ESPN WCH Team News**
#' @rdname espn_wch_team_news
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param limit Maximum number of articles to return. Default `50`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_news()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per news article when data
#'   is available. The `/news` endpoint currently returns an empty response
#'   for WCH; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `id`, `headline`, `description`, `type`,
#'   `published`, `last_modified`, `premium`, and `url`.
#'
#'   Columns when populated:
#'
#'    |col_name     |type     |description                                     |
#'    |:------------|:--------|:-----------------------------------------------|
#'    |team_id      |character|ESPN team identifier (echoed from the argument).|
#'    |id           |character|Article identifier.                             |
#'    |headline     |character|Article headline.                               |
#'    |description  |character|Article description.                            |
#'    |type         |character|Article type.                                   |
#'    |published    |character|Publication timestamp.                          |
#'    |last_modified|character|Last-modified timestamp.                        |
#'    |premium      |logical  |Whether the article is premium content.         |
#'    |url          |character|Web URL of the article.                         |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_news(team_id = "2137", limit = 10))
#' }
espn_wch_team_news <- function(team_id, limit = 50, ...) {
  .espn_hockey_team_news(league = "womens-college-hockey", team_id = team_id, limit = limit, ...)
}

#' @name espn_wch_team_leaders
NULL
#' @title **Get ESPN WCH Team Leaders**
#' @rdname espn_wch_team_leaders
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination when data is available. The `/leaders` endpoint currently
#'   returns an empty response for WCH; an empty tibble is returned with a cli
#'   warning. Columns when populated include `team_id`, `category_name`,
#'   `category_abbr`, `rank`, `value`, `display_value`, `athlete_id`, and
#'   `athlete_full_name`.
#'
#'   Columns when populated:
#'
#'    |col_name         |type     |description                                     |
#'    |:----------------|:--------|:-----------------------------------------------|
#'    |team_id          |character|ESPN team identifier (echoed from the argument).|
#'    |category_name    |character|Leader category name.                           |
#'    |category_abbr    |character|Leader category abbreviation.                   |
#'    |rank             |integer  |Rank within the category.                       |
#'    |value            |numeric  |Leader stat value.                              |
#'    |display_value    |character|Formatted stat value.                           |
#'    |athlete_id       |character|ESPN athlete identifier.                        |
#'    |athlete_full_name|character|Athlete full name.                              |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_leaders(team_id = "2137"))
#' }
espn_wch_team_leaders <- function(team_id, ...) {
  .espn_hockey_team_leaders(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_player_info
NULL
#' @title **Get ESPN WCH Player Info**
#' @rdname espn_wch_player_info
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_info()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with athlete metadata from the
#'   `site_v2` athletes endpoint, or an empty `data.frame()` when ESPN returns
#'   HTTP 404. As of 2026-06-08, ESPN's `site_v2` NHL `athletes/{id}` endpoint
#'   returns 404; use [espn_wch_player_core()] for athlete metadata instead.
#'
#'    |col_name          |types     |description                                           |
#'    |:-----------------|:---------|:-----------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete identifier (echoed from arg).            |
#'    |athlete_espn_id   |character |ESPN athlete id from payload.                         |
#'    |uid               |character |Athlete uid string.                                   |
#'    |guid              |character |Athlete global unique identifier.                     |
#'    |display_name      |character |Player display name.                                  |
#'    |short_name        |character |Player short name.                                    |
#'    |full_name         |character |Player full name.                                     |
#'    |first_name        |character |First name.                                           |
#'    |last_name         |character |Last name.                                            |
#'    |jersey            |character |Jersey number.                                        |
#'    |slug              |character |URL slug.                                             |
#'    |position          |character |Position abbreviation.                                |
#'    |position_name     |character |Position full name.                                   |
#'    |age               |integer   |Current age.                                          |
#'    |date_of_birth     |character |Date of birth (ISO 8601).                             |
#'    |debut_year        |integer   |Year of NHL debut.                                    |
#'    |weight            |character |Formatted weight string.                              |
#'    |height            |character |Formatted height string.                              |
#'    |team_id           |character |ESPN team identifier.                                 |
#'    |team_display_name |character |Team display name.                                    |
#'    |team_abbreviation |character |Team abbreviation.                                    |
#'    |headshot_href     |character |Player headshot image URL.                            |
#'    |active            |logical   |Whether athlete is currently active.                  |
#'    |status            |character |Athlete status description.                           |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_info(athlete_id = "5263060"))
#' }
espn_wch_player_info <- function(athlete_id, ...) {
  .espn_hockey_player_info(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_bio
NULL
#' @title **Get ESPN WCH Player Bio**
#' @rdname espn_wch_player_bio
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_bio()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with athlete bio fields, or an
#'   empty `data.frame()` when ESPN returns HTTP 404. As of 2026-06-08,
#'   ESPN's `site_v2` NHL `athletes/{id}/bio` endpoint returns 404; use
#'   [espn_wch_player_core()] for athlete metadata in the meantime.
#'
#'    |col_name          |types     |description                                        |
#'    |:-----------------|:---------|:--------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete identifier (echoed from arg).         |
#'    |athlete_espn_id   |character |ESPN athlete id from payload.                      |
#'    |display_name      |character |Player display name.                               |
#'    |short_name        |character |Player short name.                                 |
#'    |full_name         |character |Player full name.                                  |
#'    |first_name        |character |First name.                                        |
#'    |last_name         |character |Last name.                                         |
#'    |jersey            |character |Jersey number.                                     |
#'    |position          |character |Position abbreviation.                             |
#'    |age               |integer   |Current age.                                       |
#'    |date_of_birth     |character |Date of birth (ISO 8601).                          |
#'    |birth_city        |character |Birth city.                                        |
#'    |birth_state       |character |Birth state/province.                              |
#'    |birth_country     |character |Birth country.                                     |
#'    |weight            |character |Formatted weight string.                           |
#'    |height            |character |Formatted height string.                           |
#'    |team_id           |character |ESPN team identifier.                              |
#'    |team_display_name |character |Team display name.                                 |
#'    |team_abbreviation |character |Team abbreviation.                                 |
#'    |slug              |character |Player URL slug.                                   |
#'    |debut_year        |integer   |Year of NHL debut.                                 |
#'    |active            |logical   |Whether athlete is currently active.               |
#'    |description       |character |Biographical description, if available.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_bio(athlete_id = "5263060"))
#' }
espn_wch_player_bio <- function(athlete_id, ...) {
  .espn_hockey_player_bio(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_news
NULL
#' @title **Get ESPN WCH Player News**
#' @rdname espn_wch_player_news
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param limit Maximum number of articles to return (default `50`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_news()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per news article, or an
#'   empty `data.frame()` when no articles are available:
#'
#'    |col_name          |types     |description                                          |
#'    |:-----------------|:---------|:----------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete identifier (echoed from arg).           |
#'    |article_id        |character |Article identifier.                                  |
#'    |type              |character |Article type (e.g. "HeadlineNews").                 |
#'    |headline          |character |Article headline.                                    |
#'    |description       |character |Article description / lede.                          |
#'    |published         |character |Publication datetime (ISO 8601).                     |
#'    |last_modified     |character |Last-modified datetime (ISO 8601).                   |
#'    |premium           |logical   |Whether article is premium/paywalled.                |
#'    |source            |character |News source.                                         |
#'    |byline            |character |Author byline.                                       |
#'    |article_url       |character |Web URL for the full article.                        |
#'    |image_url         |character |Lead image URL.                                      |
#'    |category_league   |character |League category label from article metadata.         |
#'    |category_team     |character |Team category label from article metadata.           |
#'    |category_athlete  |character |Athlete category label from article metadata.        |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_news(athlete_id = "5263060", limit = 10))
#' }
espn_wch_player_news <- function(athlete_id, limit = 50, ...) {
  .espn_hockey_player_news(league = "womens-college-hockey", athlete_id = athlete_id,
                            limit = limit, ...)
}

#' @name espn_wch_standings
NULL
#' @title **Get ESPN WCH Standings**
#' @rdname espn_wch_standings
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param group Optional conference/group id filter.
#' @param standings_type Optional standings variant sent as `type` (e.g. `"by-division"`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_standings()`; columns verified against a captured WCH body One numeric column per ESPN stat in the body, so the set follows the feed (the MCH body sends `tiegames`, the WCH body `ties`).
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | group_name | character | Group name (conference / division). |
#'    | group_abbreviation | character | Group abbreviation. |
#'    | team_id | character | ESPN team identifier. |
#'    | team_name | character | Team name. |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_display_name | character | Team display name. |
#'    | team_location | character | Team location (city). |
#'    | team_logo | character | Team logo URL. |
#'    | ot_losses | numeric | Overtime losses. |
#'    | ot_wins | numeric | Overtime wins. |
#'    | avg_points_against | numeric | Average goals against per game. |
#'    | avg_points_for | numeric | Average goals for per game. |
#'    | differential | numeric | Goal differential. |
#'    | games_behind | numeric | Games behind the group leader. |
#'    | games_played | numeric | Games played. |
#'    | losses | numeric | Regulation losses. |
#'    | playoff_seed | numeric | Current playoff seed. |
#'    | point_differential | numeric | Point (goal) differential. |
#'    | points | numeric | Points. |
#'    | points_against | numeric | Goals/points allowed. |
#'    | points_for | numeric | Goals/points scored. |
#'    | streak | numeric | Current streak value. |
#'    | ties | numeric | Ties (sent in the wch body). |
#'    | win_percent | numeric | Win percentage. |
#'    | wins | numeric | Wins. |
#'    | overall | numeric | Overall record stat. |
#'    | season | integer | Season end-year (echoed from arg). |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_standings(season = 2025))
#' }
espn_wch_standings <- function(season = most_recent_nhl_season(), group = NULL, standings_type = NULL, ...) {
  .espn_hockey_standings(league = "womens-college-hockey", season = season, group = group, standings_type = standings_type, ...)
}

#' @name espn_wch_player_overview
NULL
#' @title **Get ESPN WCH Player Overview**
#' @rdname espn_wch_player_overview
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_overview()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per stats split
#'   (Regular Season / Career):
#'
#'    |col_name                     |types     |description                                       |
#'    |:----------------------------|:---------|:-------------------------------------------------|
#'    |athlete_id                   |character |ESPN athlete identifier (echoed from arg).        |
#'    |athlete_espn_id              |character |ESPN athlete identifier (from payload).           |
#'    |athlete_display_name         |character |Player display name.                              |
#'    |athlete_short_name           |character |Player short name.                                |
#'    |athlete_jersey               |character |Jersey number.                                    |
#'    |athlete_position             |character |Position abbreviation.                            |
#'    |athlete_team_id              |character |ESPN team identifier.                             |
#'    |athlete_team_abbreviation    |character |Team abbreviation.                                |
#'    |split_display_name           |character |Split label (e.g. Regular Season, Career).        |
#'    |games                        |character |Games played.                                     |
#'    |goals                        |character |Goals.                                            |
#'    |assists                      |character |Assists.                                          |
#'    |points                       |character |Points.                                           |
#'    |plus_minus                   |character |Plus/minus.                                       |
#'    |penalty_minutes              |character |Penalty minutes.                                  |
#'    |shots_total                  |character |Shots on goal.                                    |
#'    |power_play_goals             |character |Power-play goals.                                 |
#'    |power_play_assists           |character |Power-play assists.                               |
#'    |short_handed_goals           |character |Short-handed goals.                               |
#'    |short_handed_assists         |character |Short-handed assists.                             |
#'    |game_winning_goals           |character |Game-winning goals.                               |
#'    |time_on_ice_per_game         |character |Average time on ice per game.                     |
#'    |production                   |character |Production metric.                                |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_overview(athlete_id = "5263060"))
#' }
espn_wch_player_overview <- function(athlete_id, ...) {
  .espn_hockey_player_overview(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_stats
NULL
#' @title **Get ESPN WCH Player Stats (web-v3)**
#' @rdname espn_wch_player_stats
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`. When `NULL` the current season is used.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_stats_v3()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per season entry plus
#'   a career-total row per stat category:
#'
#'    |col_name             |types     |description                                  |
#'    |:--------------------|:---------|:--------------------------------------------|
#'    |athlete_id           |character |ESPN athlete identifier (echoed from arg).   |
#'    |season               |integer   |Requested season year (echoed from arg).     |
#'    |season_year          |integer   |Season end-year for this row.                |
#'    |category             |character |Stat category name (e.g. center).           |
#'    |category_display     |character |Stat category display name.                  |
#'    |position             |character |Position for this season row.                |
#'    |team_id              |character |ESPN team identifier.                        |
#'    |team_slug            |character |Team slug.                                   |
#'    |team_display_name    |character |Team display name.                           |
#'    |team_abbreviation    |character |Team abbreviation.                           |
#'    |games                |character |Games played.                                |
#'    |goals                |character |Goals.                                       |
#'    |assists              |character |Assists.                                     |
#'    |points               |character |Points.                                      |
#'    |plus_minus           |character |Plus/minus.                                  |
#'    |penalty_minutes      |character |Penalty minutes.                             |
#'    |shootout_goals       |character |Shootout goals.                              |
#'    |shooting_pct         |character |Shooting percentage.                         |
#'    |power_play_goals     |character |Power-play goals.                            |
#'    |power_play_assists   |character |Power-play assists.                          |
#'    |short_handed_goals   |character |Short-handed goals.                          |
#'    |short_handed_assists |character |Short-handed assists.                        |
#'    |game_winning_goals   |character |Game-winning goals.                          |
#'    |time_on_ice_per_game |character |Average time on ice per game.                |
#'    |production           |character |Production metric.                           |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_stats(athlete_id = "5263060", season = 2025))
#' }
espn_wch_player_stats <- function(athlete_id,
                                      season = most_recent_nhl_season(),
                                      ...) {
  .espn_hockey_player_stats_v3(league     = "womens-college-hockey",
                                athlete_id = athlete_id,
                                season     = season,
                                ...)
}

#' @name espn_wch_player_gamelog
NULL
#' @title **Get ESPN WCH Player Gamelog**
#' @rdname espn_wch_player_gamelog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param season Season end-year (e.g. `2025`). When `NULL` ESPN returns the
#'   current season. Defaults to `NULL`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_gamelog()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per game, or an empty
#'   `data.frame()` when no data is available (e.g. if the endpoint returns
#'   HTTP 404 in a future ESPN schema change). If the endpoint becomes
#'   unavailable, use [espn_wch_player_statisticslog()] from the core-v2
#'   host as a reliable substitute.
#'
#'    |col_name              |types     |description                                          |
#'    |:---------------------|:---------|:----------------------------------------------------|
#'    |athlete_id            |character |ESPN athlete identifier (echoed from arg).           |
#'    |event_id              |character |ESPN event identifier.                               |
#'    |season_type_display   |character |Season type label (e.g. "2025-26 Regular Season").   |
#'    |category_display      |character |Stat category display name.                          |
#'    |category_type         |character |Stat category type.                                  |
#'    |category_split_type   |character |Stat category split type.                            |
#'    |game_date             |character |Game date-time (ISO 8601).                           |
#'    |at_vs                 |character |"at" or "vs" home/away indicator.                   |
#'    |score                 |character |Final score string.                                  |
#'    |home_team_id          |character |ESPN home team identifier.                           |
#'    |away_team_id          |character |ESPN away team identifier.                           |
#'    |home_team_score       |character |Home team final score.                               |
#'    |away_team_score       |character |Away team final score.                               |
#'    |game_result           |character |Game result for the player's team (W/L/OT).          |
#'    |opponent_id           |character |ESPN opponent team identifier.                       |
#'    |opponent_name         |character |Opponent team display name.                          |
#'    |opponent_abbr         |character |Opponent team abbreviation.                          |
#'    |team_id               |character |Player's team ESPN identifier.                       |
#'    |team_abbr             |character |Player's team abbreviation.                          |
#'    |league_name           |character |League name.                                         |
#'    |league_abbreviation   |character |League abbreviation.                                 |
#'    |goals                 |character |Goals scored.                                        |
#'    |assists               |character |Assists.                                             |
#'    |points                |character |Points (G+A).                                        |
#'    |plus_minus            |character |Plus/minus.                                          |
#'    |penalty_minutes       |character |Penalty minutes.                                     |
#'    |shots_total           |character |Shots on goal.                                       |
#'    |shooting_pct          |character |Shooting percentage.                                 |
#'    |power_play_goals      |character |Power-play goals.                                    |
#'    |power_play_assists    |character |Power-play assists.                                  |
#'    |short_handed_goals    |character |Short-handed goals.                                  |
#'    |short_handed_assists  |character |Short-handed assists.                                |
#'    |game_winning_goals    |character |Game-winning goals.                                  |
#'    |time_on_ice_per_game  |character |Average time on ice.                                 |
#'    |production            |character |Production metric.                                   |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_gamelog(athlete_id = "5263060"))
#' }
espn_wch_player_gamelog <- function(athlete_id, season = NULL, ...) {
  .espn_hockey_player_gamelog(league     = "womens-college-hockey",
                               athlete_id = athlete_id,
                               season     = season, ...)
}

#' @name espn_wch_player_splits
NULL
#' @title **Get ESPN WCH Player Splits**
#' @rdname espn_wch_player_splits
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_splits()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per situational split:
#'
#'    |col_name                  |types     |description                                    |
#'    |:-------------------------|:---------|:----------------------------------------------|
#'    |athlete_id                |character |ESPN athlete identifier (echoed from arg).     |
#'    |season                    |integer   |Season end-year (echoed from arg).             |
#'    |split_category            |character |Split category name (e.g. split, location).   |
#'    |split_category_display    |character |Split category display name.                   |
#'    |split_display_name        |character |Individual split label (e.g. All Splits).      |
#'    |split_abbreviation        |character |Split abbreviation.                            |
#'    |games                     |character |Games played.                                  |
#'    |goals                     |character |Goals.                                         |
#'    |assists                   |character |Assists.                                       |
#'    |points                    |character |Points.                                        |
#'    |plus_minus                |character |Plus/minus.                                    |
#'    |penalty_minutes           |character |Penalty minutes.                               |
#'    |shots_total               |character |Shots on goal.                                 |
#'    |faceoff_percent           |character |Faceoff percentage.                            |
#'    |power_play_goals          |character |Power-play goals.                              |
#'    |power_play_assists        |character |Power-play assists.                            |
#'    |short_handed_goals        |character |Short-handed goals.                            |
#'    |short_handed_assists      |character |Short-handed assists.                          |
#'    |game_winning_goals        |character |Game-winning goals.                            |
#'    |time_on_ice_per_game      |character |Average time on ice per game.                  |
#'    |production                |character |Production metric.                             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_splits(athlete_id = "5263060", season = 2025))
#' }
espn_wch_player_splits <- function(athlete_id,
                                    season = most_recent_nhl_season(),
                                    ...) {
  .espn_hockey_player_splits(league     = "womens-college-hockey",
                              athlete_id = athlete_id,
                              season     = season,
                              ...)
}

#' @name espn_wch_leaders
NULL
#' @title **Get ESPN WCH League Leaders**
#' @rdname espn_wch_leaders
#' @author Saiem Gilani
#' @param category Stat category to display. For WCH one of `"offensive"`,
#'   `"defensive"`, `"general"`, `"penalties"`, or `NULL` for all
#'   categories. Defaults to `"offensive"`.
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`. **ESPN's WCH `statistics/byathlete` endpoint
#'   currently ignores this parameter and always returns the current/most-recent
#'   season.** The argument is accepted for forward-compatibility but does not
#'   filter upstream. The returned `season` and `season_type` columns reflect
#'   the season ESPN actually served, not this value.
#' @param season_type Season type code (1 = pre, 2 = regular, 3 = post).
#'   Defaults to `2`. **ESPN's WCH `statistics/byathlete` endpoint currently
#'   ignores this parameter.** The returned `season_type` column reflects the
#'   type ESPN actually served.
#' @param limit Athletes per page (default `50`).
#' @param page Page number (default `1`).
#' @param sort Optional ESPN `sort` query string.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per athlete:
#'
#'    |col_name                    |types     |description                                       |
#'    |:---------------------------|:---------|:-------------------------------------------------|
#'    |season                      |integer   |Season end-year ESPN actually served (from `currentSeason$year`). |
#'    |season_type                 |integer   |Season type id ESPN actually served (from `currentSeason$type$id`). |
#'    |requested_season_year       |integer   |Season year from `requestedSeason` (mirrors `season`). |
#'    |page                        |integer   |Current page.                                     |
#'    |pagination_count            |integer   |Total athlete count.                              |
#'    |pagination_limit            |integer   |Athletes per page.                                |
#'    |pagination_pages            |integer   |Total page count.                                 |
#'    |league_id                   |character |ESPN league identifier.                           |
#'    |league_name                 |character |League name.                                      |
#'    |league_abbreviation         |character |League abbreviation.                              |
#'    |athlete_id                  |character |ESPN athlete identifier.                          |
#'    |athlete_display_name        |character |Player display name.                              |
#'    |athlete_short_name          |character |Player short name.                                |
#'    |athlete_slug                |character |Player URL slug.                                  |
#'    |athlete_position            |character |Position abbreviation.                            |
#'    |athlete_jersey              |character |Jersey number.                                    |
#'    |athlete_team_id             |character |ESPN team identifier.                             |
#'    |athlete_team_name           |character |Team name.                                        |
#'    |athlete_team_short_name     |character |Team short name.                                  |
#'    |athlete_team_logo           |character |Team logo URL.                                    |
#'    |headshot_href               |character |Player headshot URL.                              |
#'    |debut_year                  |integer   |Debut year.                                       |
#'    |age                         |integer   |Player age.                                       |
#'    |offensive_goals             |character |Goals (offensive category).                       |
#'    |offensive_assists           |character |Assists.                                          |
#'    |offensive_points            |character |Points.                                           |
#'    |offensive_power_play_goals  |character |Power-play goals.                                 |
#'    |offensive_shots_total       |character |Shots on goal.                                    |
#'    |offensive_shooting_pct      |character |Shooting percentage.                              |
#'    |offensive_game_winning_goals|character |Game-winning goals.                               |
#'    |offensive_rank_points       |character |Rank by points.                                   |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_leaders(category = "offensive", limit = 25))
#' }
espn_wch_leaders <- function(category    = "offensive",
                              season      = most_recent_nhl_season(),
                              season_type = 2L,
                              limit       = 50L,
                              page        = 1L,
                              sort = NULL,
                              ...) {
  .espn_hockey_leaders(league      = "womens-college-hockey",
                        category    = category,
                        season      = season,
                        season_type = season_type,
                        limit       = limit,
                        page        = page,
                        sort = sort,
                        ...)
}

#' @name espn_wch_league_root
NULL
#' @title **Get ESPN WCH League Root (core-v2)**
#' @rdname espn_wch_league_root
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_league_root()`; columns verified against a captured WCH body.
#' @return A one-row `fastRhockey_data` tibble with league metadata:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | id | character | ESPN league identifier (e.g. "90" for WCH). |
#'    | guid | character | League global unique identifier. |
#'    | uid | character | League uid string. |
#'    | name | character | Full league name. |
#'    | display_name | character | League display name. |
#'    | abbreviation | character | League abbreviation (e.g. "WCH"). |
#'    | short_name | character | League short name. |
#'    | slug | character | League slug (e.g. "nhl"). |
#'    | is_tournament | logical | Whether the league is a tournament. |
#'    | gender | character | League gender designation. |
#'    | season_ref | character | `$ref` URL for the current season object. |
#'    | seasons_ref | character | `$ref` URL for the seasons collection. |
#'    | groups_ref | character | `$ref` URL for the groups collection. |
#'    | franchises_ref | character | `$ref` URL for the franchises collection. |
#'    | teams_ref | character | `$ref` URL for the teams collection. |
#'    | events_ref | character | `$ref` URL for the events collection. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_league_root())
#' }
espn_wch_league_root <- function(...) {
  .espn_hockey_league_root(league = "womens-college-hockey", ...)
}

#' @name espn_wch_season_pointer
NULL
#' @title **Get ESPN WCH Current Season Pointer (core-v2)**
#' @rdname espn_wch_season_pointer
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_pointer()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with the current season metadata:
#'
#'    |col_name           |types     |description                                      |
#'    |:------------------|:---------|:------------------------------------------------|
#'    |season             |integer   |Current season year (e.g. 2026).                 |
#'    |start_date         |character |Season start date (ISO 8601).                    |
#'    |end_date           |character |Season end date (ISO 8601).                      |
#'    |display_name       |character |Season display name (e.g. "2025-26").            |
#'    |current_type_id    |character |Current season-type identifier.                  |
#'    |current_type       |integer   |Current season-type code (1=pre,2=reg,3=post).   |
#'    |current_type_name  |character |Current season-type name.                        |
#'    |current_type_slug  |character |Current season-type slug.                        |
#'    |season_ref         |character |`$ref` URL for the full season object.           |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_pointer())
#' }
espn_wch_season_pointer <- function(...) {
  .espn_hockey_season_pointer(league = "womens-college-hockey", ...)
}

#' @name espn_wch_seasons
NULL
#' @title **Get ESPN WCH Seasons List (core-v2)**
#' @rdname espn_wch_seasons
#' @author Saiem Gilani
#' @param limit Maximum number of seasons to return (default `200`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_seasons()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per season:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | ref | character | `$ref` URL for the season object. |
#'    | season | integer | Season year parsed from the `$ref` URL. |
#'    | count | integer | Total number of seasons in the collection. |
#'    | page_count | integer | Total number of pages in the collection. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_seasons(limit = 10))
#' }
espn_wch_seasons <- function(limit = 200, ...) {
  .espn_hockey_seasons(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_season_info
NULL
#' @title **Get ESPN WCH Season Info (core-v2)**
#' @rdname espn_wch_season_info
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_info()`; columns verified against a captured WCH body.
#' @return A one-row `fastRhockey_data` tibble with season metadata:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | season | integer | Season year (echoed from arg). |
#'    | start_date | character | Season start date (ISO 8601). |
#'    | end_date | character | Season end date (ISO 8601). |
#'    | display_name | character | Season display name (e.g. "2025-26"). |
#'    | season_ref | character | `$ref` URL for this season. |
#'    | types_ref | character | `$ref` URL for the season-types collection. |
#'    | type_ref | character | `$ref` URL for the current season type. |
#'    | rankings_ref | character | `$ref` URL for rankings. |
#'    | coaches_ref | character | `$ref` URL for coaches. |
#'    | athletes_ref | character | `$ref` URL for athletes. |
#'    | awards_ref | character | `$ref` URL for awards. |
#'    | futures_ref | character | `$ref` URL for futures. |
#'    | leaders_ref | character | `$ref` URL for leaders. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_info(season = 2026))
#' }
espn_wch_season_info <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_info(league = "womens-college-hockey", season = season, ...)
}

#' @name espn_wch_season_types
NULL
#' @title **Get ESPN WCH Season Types (core-v2)**
#' @rdname espn_wch_season_types
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_types()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per season type
#'   (pre-season, regular, post-season, off-season):
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | ref | character | `$ref` URL for the season-type object. |
#'    | season_type_id | integer | Season type id (1=pre, 2=reg, 3=post, 4=off). |
#'    | season | integer | Season year (echoed from arg). |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_types(season = 2026))
#' }
espn_wch_season_types <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_types(league = "womens-college-hockey", season = season, ...)
}

#' @name espn_wch_season_type
NULL
#' @title **Get ESPN WCH Season Type (core-v2)**
#' @rdname espn_wch_season_type
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_type()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with season-type metadata:
#'
#'    |col_name         |types     |description                                         |
#'    |:----------------|:---------|:---------------------------------------------------|
#'    |season           |integer   |Season year (echoed from arg).                      |
#'    |season_type      |integer   |Season type code (echoed from arg).                 |
#'    |id               |character |Season type identifier.                             |
#'    |type             |integer   |Season type numeric code.                           |
#'    |name             |character |Season type name (e.g. "Regular Season").           |
#'    |abbreviation     |character |Season type abbreviation (e.g. "reg").              |
#'    |slug             |character |Season type slug (e.g. "regular-season").           |
#'    |year             |integer   |Season year from the API.                           |
#'    |start_date       |character |Season type start date (ISO 8601).                  |
#'    |end_date         |character |Season type end date (ISO 8601).                    |
#'    |has_groups       |logical   |Whether this season type has groups.                |
#'    |has_standings    |logical   |Whether this season type has standings.             |
#'    |has_legs         |logical   |Whether this season type has legs.                  |
#'    |groups_ref       |character |`$ref` URL for the groups collection.               |
#'    |weeks_ref        |character |`$ref` URL for the weeks collection.                |
#'    |corrections_ref  |character |`$ref` URL for corrections.                         |
#'    |leaders_ref      |character |`$ref` URL for leaders.                             |
#'    |type_ref         |character |`$ref` URL for this season-type object.             |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_type(season = 2026, season_type = 2))
#' }
espn_wch_season_type <- function(season = most_recent_nhl_season(),
                                  season_type = 2,
                                  ...) {
  .espn_hockey_season_type(league = "womens-college-hockey", season = season,
                            season_type = season_type, ...)
}

#' @name espn_wch_season_group
NULL
#' @title **Get ESPN WCH Season Group (core-v2)**
#' @rdname espn_wch_season_group
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Use
#'   `espn_wch_season_groups()` to discover valid ids for a season/type.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_group()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with group metadata:
#'
#'    |col_name       |types     |description                                         |
#'    |:--------------|:---------|:---------------------------------------------------|
#'    |season         |integer   |Season year (echoed from arg).                      |
#'    |season_type    |integer   |Season type code (echoed from arg).                 |
#'    |group_id       |integer   |Group id (echoed from arg).                         |
#'    |id             |character |ESPN group identifier.                              |
#'    |uid            |character |Group uid string.                                   |
#'    |name           |character |Group name (e.g. "Eastern Conference").             |
#'    |abbreviation   |character |Group abbreviation (e.g. "East").                   |
#'    |slug           |character |Group slug.                                         |
#'    |is_conference  |logical   |Whether this group is a conference (vs. division).  |
#'    |group_ref      |character |`$ref` URL for this group object.                   |
#'    |season_ref     |character |`$ref` URL for the group's season.                  |
#'    |parent_ref     |character |`$ref` URL for the parent group (if any).           |
#'    |children_ref   |character |`$ref` URL for child groups (divisions).            |
#'    |standings_ref  |character |`$ref` URL for group standings.                     |
#'    |teams_ref      |character |`$ref` URL for the teams in this group.             |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_group(season = 2026, season_type = 2, group_id = 7))
#' }
espn_wch_season_group <- function(season = most_recent_nhl_season(),
                                   season_type = 2,
                                   group_id,
                                   ...) {
  .espn_hockey_season_group(league = "womens-college-hockey", season = season,
                             season_type = season_type, group_id = group_id, ...)
}

#' @name espn_wch_season_groups
NULL
#' @title **Get ESPN WCH Season Groups (core-v2)**
#' @rdname espn_wch_season_groups
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_groups()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per group
#'   (conference or division):
#'
#'    |col_name     |types     |description                                         |
#'    |:------------|:---------|:---------------------------------------------------|
#'    |ref          |character |`$ref` URL for the group object.                    |
#'    |group_id     |integer   |Group id parsed from the `$ref` URL.               |
#'    |season       |integer   |Season year (echoed from arg).                      |
#'    |season_type  |integer   |Season type code (echoed from arg).                 |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_groups(season = 2026, season_type = 2))
#' }
espn_wch_season_groups <- function(season = most_recent_nhl_season(),
                                    season_type = 2,
                                    ...) {
  .espn_hockey_season_groups(league = "womens-college-hockey", season = season,
                              season_type = season_type, ...)
}

#' @name espn_wch_season_group_teams
NULL
#' @title **Get ESPN WCH Season Group Teams (core-v2)**
#' @rdname espn_wch_season_group_teams
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Use
#'   `espn_wch_season_groups()` to discover valid ids for a season/type.
#' @param limit Maximum number of teams to return (default `500`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_group_teams()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name     |types     |description                                         |
#'    |:------------|:---------|:---------------------------------------------------|
#'    |ref          |character |`$ref` URL for the team-season object.              |
#'    |team_id      |character |ESPN team id parsed from the `$ref` URL.            |
#'    |season       |integer   |Season year (echoed from arg).                      |
#'    |season_type  |integer   |Season type code (echoed from arg).                 |
#'    |group_id     |integer   |Group id (echoed from arg).                         |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     grps <- espn_wch_season_groups(season = 2026, season_type = 2)
#'     espn_wch_season_group_teams(season = 2026, season_type = 2,
#'                                  group_id = grps$group_id[1])
#'   })
#' }
espn_wch_season_group_teams <- function(season = most_recent_nhl_season(),
                                         season_type = 2,
                                         group_id,
                                         limit = 500,
                                         ...) {
  .espn_hockey_season_group_teams(league = "womens-college-hockey", season = season,
                                   season_type = season_type,
                                   group_id = group_id, limit = limit, ...)
}

#' @name espn_wch_season_group_children
NULL
#' @title **Get ESPN WCH Season Group Children (core-v2)**
#' @rdname espn_wch_season_group_children
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Pass a
#'   conference-level `group_id` from `espn_wch_season_groups()` to retrieve
#'   its child divisions.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_group_children()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per child group (division
#'   under a conference). Conference-level groups typically have 2 division
#'   children each:
#'
#'    |col_name        |types     |description                                          |
#'    |:---------------|:---------|:----------------------------------------------------|
#'    |ref             |character |`$ref` URL for the child group object.               |
#'    |child_group_id  |character |Child group id parsed from the `$ref` URL.           |
#'    |season          |integer   |Season year (echoed from arg).                       |
#'    |season_type     |integer   |Season type code (echoed from arg).                  |
#'    |group_id        |integer   |Parent group id (echoed from arg).                   |
#'    |count           |integer   |Total children in the collection.                    |
#'    |page_count      |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     grps <- espn_wch_season_groups(season = 2026, season_type = 2)
#'     espn_wch_season_group_children(season = 2026, season_type = 2,
#'                                     group_id = grps$group_id[1])
#'   })
#' }
espn_wch_season_group_children <- function(season      = most_recent_nhl_season(),
                                            season_type = 2,
                                            group_id,
                                            ...) {
  .espn_hockey_season_group_children(league = "womens-college-hockey", season = season,
                                      season_type = season_type,
                                      group_id = group_id, ...)
}

#' @name espn_wch_season_type_leaders
NULL
#' @title **Get ESPN WCH Season Type Leaders (core-v2)**
#' @rdname espn_wch_season_type_leaders
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_type_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination. For a typical WCH regular season there are 9 categories
#'   with 25 leaders each (225 rows):
#'
#'    |col_name                     |types     |description                                       |
#'    |:----------------------------|:---------|:-------------------------------------------------|
#'    |category_name                |character |Stat category name (e.g. "goals", "assists").     |
#'    |category_display_name        |character |Category display name.                            |
#'    |category_short_display_name  |character |Category short display name.                      |
#'    |category_abbreviation        |character |Category abbreviation.                            |
#'    |display_value                |character |Leader stat display value.                        |
#'    |value                        |numeric   |Leader stat numeric value.                        |
#'    |rel                          |character |Relation type (e.g. "athlete").                   |
#'    |athlete_ref                  |character |`$ref` URL for the athlete.                       |
#'    |team_ref                     |character |`$ref` URL for the athlete's team.                |
#'    |statistics_ref               |character |`$ref` URL for the athlete's season statistics.   |
#'    |season                       |integer   |Season year (echoed from arg).                    |
#'    |season_type                  |integer   |Season type code (echoed from arg).               |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_type_leaders(season = 2026, season_type = 2))
#' }
espn_wch_season_type_leaders <- function(season      = most_recent_nhl_season(),
                                          season_type = 2,
                                          ...) {
  .espn_hockey_season_type_leaders(league = "womens-college-hockey", season = season,
                                    season_type = season_type, ...)
}

#' @name espn_wch_season_type_corrections
NULL
#' @title **Get ESPN WCH Season Type Corrections (core-v2)**
#' @rdname espn_wch_season_type_corrections
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_type_corrections()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per correction entry, or
#'   an empty tibble when no corrections are recorded. **Note**: ESPN does not
#'   publish score corrections for WCH — this endpoint returns empty for all
#'   tested seasons:
#'
#'    |col_name        |types     |description                                          |
#'    |:---------------|:---------|:----------------------------------------------------|
#'    |ref             |character |`$ref` URL for the correction object.                |
#'    |correction_id   |character |Correction id parsed from the `$ref` URL.            |
#'    |season          |integer   |Season year (echoed from arg).                       |
#'    |season_type     |integer   |Season type code (echoed from arg).                  |
#'    |count           |integer   |Total corrections in the collection.                 |
#'    |page_count      |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not publish corrections for hockey)
#'   try(espn_wch_season_type_corrections(season = 2026, season_type = 2))
#' }
espn_wch_season_type_corrections <- function(season      = most_recent_nhl_season(),
                                              season_type = 2,
                                              ...) {
  .espn_hockey_season_type_corrections(league = "womens-college-hockey", season = season,
                                        season_type = season_type, ...)
}

#' @name espn_wch_season_weeks
NULL
#' @title **Get ESPN WCH Season Weeks (core-v2)**
#' @rdname espn_wch_season_weeks
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_weeks()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per week, or an empty
#'   tibble when the endpoint returns no items. **Note**: The WCH does not use
#'   weekly scheduling in the ESPN core-v2 API — this endpoint consistently
#'   returns `count=0` for WCH. An empty tibble is returned gracefully:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the week object.                      |
#'    |week         |integer   |Week number parsed from the `$ref` URL.              |
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |count        |integer   |Total weeks in the collection (0 for WCH).           |
#'    |page_count   |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not populate weeks for hockey)
#'   try(espn_wch_season_weeks(season = 2026, season_type = 2))
#' }
espn_wch_season_weeks <- function(season      = most_recent_nhl_season(),
                                   season_type = 2,
                                   ...) {
  .espn_hockey_season_weeks(league = "womens-college-hockey", season = season,
                             season_type = season_type, ...)
}

#' @name espn_wch_season_week
NULL
#' @title **Get ESPN WCH Season Week (core-v2)**
#' @rdname espn_wch_season_week
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param week Week number (integer). Derive valid week numbers from
#'   `espn_wch_season_weeks()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_week()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with week metadata, or an empty
#'   tibble when the endpoint is unavailable. **Note**: NHL does not use weeks
#'   in ESPN core-v2 — HTTP 404 is the expected response:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |week         |integer   |Week number (echoed from arg).                       |
#'    |number       |integer   |Week number as returned by the API.                  |
#'    |start_date   |character |Week start date (ISO 8601).                          |
#'    |end_date     |character |Week end date (ISO 8601).                            |
#'    |text         |character |Week display text.                                   |
#'    |week_ref     |character |`$ref` URL for this week object.                     |
#'    |events_ref   |character |`$ref` URL for the events in this week.              |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
#'   try(espn_wch_season_week(season = 2026, season_type = 2, week = 1))
#' }
espn_wch_season_week <- function(season      = most_recent_nhl_season(),
                                  season_type = 2,
                                  week,
                                  ...) {
  .espn_hockey_season_week(league = "womens-college-hockey", season = season,
                            season_type = season_type, week = week, ...)
}

#' @name espn_wch_season_week_games
NULL
#' @title **Get ESPN WCH Season Week Games (core-v2)**
#' @rdname espn_wch_season_week_games
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param week Week number (integer). Derive valid week numbers from
#'   `espn_wch_season_weeks()`.
#' @param limit Maximum number of events to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_week_games()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per game (event) in the
#'   week, or an empty tibble when the endpoint is unavailable. **Note**: NHL
#'   does not use weeks in ESPN core-v2 — HTTP 404 is expected:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the event object.                     |
#'    |event_id     |character |ESPN event id parsed from the `$ref` URL.            |
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |week         |integer   |Week number (echoed from arg).                       |
#'    |count        |integer   |Total events in the collection.                      |
#'    |page_count   |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
#'   try(espn_wch_season_week_games(season = 2026, season_type = 2, week = 1))
#' }
espn_wch_season_week_games <- function(season      = most_recent_nhl_season(),
                                        season_type = 2,
                                        week,
                                        limit       = 100,
                                        ...) {
  .espn_hockey_season_week_games(league = "womens-college-hockey", season = season,
                                  season_type = season_type, week = week,
                                  limit = limit, ...)
}

#' @name espn_wch_season_teams
NULL
#' @title **Get ESPN WCH Season Teams (core-v2)**
#' @rdname espn_wch_season_teams
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of teams to return per page (default `100`).
#'   The WCH has 32 active teams; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_teams()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per team in the season:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the team-in-season object.           |
#'    |team_id     |character |ESPN team id parsed from the `$ref` URL.            |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |count       |integer   |Total teams in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_teams(season = 2026))
#' }
espn_wch_season_teams <- function(season = most_recent_nhl_season(),
                                   limit  = 100,
                                   ...) {
  .espn_hockey_season_teams(league = "womens-college-hockey", season = season,
                             limit = limit, ...)
}

#' @name espn_wch_season_team
NULL
#' @title **Get ESPN WCH Season Team (core-v2)**
#' @rdname espn_wch_season_team
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param team_id ESPN team identifier (character or numeric). Use
#'   `espn_wch_season_teams()` to discover valid team ids for a season.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_team()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with team-in-season metadata:
#'
#'    |col_name             |types     |description                                          |
#'    |:--------------------|:---------|:----------------------------------------------------|
#'    |season               |integer   |Season year (echoed from arg).                       |
#'    |team_id              |character |ESPN team id (echoed from arg).                      |
#'    |id                   |character |ESPN team identifier.                                |
#'    |guid                 |character |Team global unique identifier.                       |
#'    |uid                  |character |Team uid string.                                     |
#'    |slug                 |character |Team URL slug.                                       |
#'    |location             |character |Team city/location.                                  |
#'    |name                 |character |Team mascot/name.                                    |
#'    |nickname             |character |Team nickname.                                       |
#'    |abbreviation         |character |Team abbreviation.                                   |
#'    |display_name         |character |Team display name.                                   |
#'    |short_display_name   |character |Team short display name.                             |
#'    |color                |character |Team primary color hex.                              |
#'    |alternate_color      |character |Team alternate color hex.                            |
#'    |is_active            |logical   |Whether the team is currently active.                |
#'    |is_all_star          |logical   |Whether this is an all-star team.                    |
#'    |team_ref             |character |`$ref` URL for this team-in-season object.           |
#'    |record_ref           |character |`$ref` URL for the team record.                      |
#'    |athletes_ref         |character |`$ref` URL for the team athletes.                    |
#'    |venue_ref            |character |`$ref` URL for the team venue.                       |
#'    |groups_ref           |character |`$ref` URL for the team groups.                      |
#'    |statistics_ref       |character |`$ref` URL for the team statistics.                  |
#'    |leaders_ref          |character |`$ref` URL for the team leaders.                     |
#'    |injuries_ref         |character |`$ref` URL for the team injuries.                    |
#'    |awards_ref           |character |`$ref` URL for the team awards.                      |
#'    |franchise_ref        |character |`$ref` URL for the team franchise.                   |
#'    |events_ref           |character |`$ref` URL for the team events.                      |
#'    |transactions_ref     |character |`$ref` URL for the team transactions.                |
#'    |coaches_ref          |character |`$ref` URL for the team coaches.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     tms <- espn_wch_season_teams(season = 2026)
#'     espn_wch_season_team(season = 2026, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_season_team <- function(season  = most_recent_nhl_season(),
                                  team_id,
                                  ...) {
  .espn_hockey_season_team(league = "womens-college-hockey", season = season,
                            team_id = team_id, ...)
}

#' @name espn_wch_season_players
NULL
#' @title **Get ESPN WCH Season Players (core-v2)**
#' @rdname espn_wch_season_players
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Number of players per page (default `100`). The WCH season
#'   player list exceeds 11,000 entries across ~2,300 pages; use `page` to
#'   iterate.
#' @param page Page number (1-indexed, default `1`). Check the returned
#'   `page_count` column to determine how many pages are available.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_players()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per player reference for
#'   the requested page. This function hits the same endpoint as
#'   `espn_wch_season_athletes()` (`seasons/{season}/athletes`) but uses the
#'   `player_id` column name for consistency with the `sdv-py`
#'   `espn_wch_season_players` naming convention:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete-in-season object.         |
#'    |player_id   |character |ESPN player id parsed from the `$ref` URL.           |
#'    |season      |integer   |Season year (echoed from arg).                       |
#'    |page        |integer   |Page number (echoed from arg).                       |
#'    |count       |integer   |Total players in the collection.                     |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_players(season = 2026, limit = 10, page = 1))
#' }
espn_wch_season_players <- function(season = most_recent_nhl_season(),
                                     limit  = 100,
                                     page   = 1,
                                     ...) {
  .espn_hockey_season_players(league = "womens-college-hockey", season = season,
                               limit = limit, page = page, ...)
}

#' @name espn_wch_season_coaches
NULL
#' @title **Get ESPN WCH Season Coaches (core-v2)**
#' @rdname espn_wch_season_coaches
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of coaches to return (default `100`). The WCH
#'   typically lists ~30 head coaches per season.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_coaches()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per coach:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the coach-in-season object.          |
#'    |coach_id    |character |ESPN coach id parsed from the `$ref` URL.           |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |count       |integer   |Total coaches in the collection.                    |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_coaches(season = 2026))
#' }
espn_wch_season_coaches <- function(season = most_recent_nhl_season(),
                                     limit  = 100,
                                     ...) {
  .espn_hockey_season_coaches(league = "womens-college-hockey", season = season,
                               limit = limit, ...)
}

#' @name espn_wch_season_draft
NULL
#' @title **Get ESPN WCH Season Draft (core-v2)**
#' @rdname espn_wch_season_draft
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_draft()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with draft round references, or an
#'   empty tibble when the endpoint is unavailable (HTTP 404 is normal for
#'   NHL — the draft sub-tree is not served by this core-v2 path):
#'
#'    |col_name  |types     |description                                           |
#'    |:---------|:---------|:-----------------------------------------------------|
#'    |ref       |character |`$ref` URL for the draft round object (if returned).  |
#'    |draft_id  |character |Draft/round id parsed from the `$ref` URL.            |
#'    |season    |integer   |Season year (echoed from arg).                        |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try(espn_wch_season_draft(season = 2025))
#' }
espn_wch_season_draft <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_draft(league = "womens-college-hockey", season = season, ...)
}

#' @name espn_wch_season_draft_round_picks
NULL
#' @title **Get ESPN WCH Season Draft Round Picks (core-v2)**
#' @rdname espn_wch_season_draft_round_picks
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param round_num Draft round number (default `1`). Used to build the path
#'   `seasons/{season}/draft/rounds/{round_num}/picks`.
#' @param limit Maximum picks to return per page (default `100`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_draft_round_picks()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per draft pick, or an
#'   empty tibble when the endpoint is unavailable. The confirmed path is
#'   `seasons/{season}/draft/rounds/{round_num}/picks`; for NHL this returns
#'   HTTP 404 — the draft data is not served by the core-v2 sub-tree:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the draft pick object.               |
#'    |pick_id     |character |Pick id parsed from the `$ref` URL.                 |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |round_num   |integer   |Round number (echoed from arg).                     |
#'    |count       |integer   |Total picks in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try(espn_wch_season_draft_round_picks(season = 2025, round_num = 1))
#' }
espn_wch_season_draft_round_picks <- function(season    = most_recent_nhl_season(),
                                               round_num = 1,
                                               limit     = 100,
                                               ...) {
  .espn_hockey_season_draft_round_picks(league = "womens-college-hockey", season = season,
                                         round_num = round_num, limit = limit, ...)
}

#' @name espn_wch_season_futures
NULL
#' @title **Get ESPN WCH Season Futures (core-v2)**
#' @rdname espn_wch_season_futures
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_futures()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per futures market per
#'   betting provider. For a typical WCH season there are ~12 markets across
#'   1-2 providers:
#'
#'    |col_name             |types     |description                                          |
#'    |:--------------------|:---------|:----------------------------------------------------|
#'    |future_ref           |character |`$ref` URL for the futures market object.            |
#'    |future_id            |character |ESPN futures market identifier.                      |
#'    |future_name          |character |Futures market name (e.g. "WCH - Stanley Cup - Winner"). |
#'    |future_type          |character |Futures market type (e.g. "winLeague").              |
#'    |future_display_name  |character |Futures market display name.                         |
#'    |provider_id          |character |Betting provider identifier.                         |
#'    |provider_name        |character |Betting provider name (e.g. "ESPN BET").             |
#'    |provider_active      |logical   |Whether the provider is active.                      |
#'    |provider_priority    |integer   |Provider display priority.                           |
#'    |season               |integer   |Season year (echoed from arg).                       |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_futures(season = 2026))
#' }
espn_wch_season_futures <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_futures(league = "womens-college-hockey", season = season, ...)
}

#' @name espn_wch_season_freeagents
NULL
#' @title **Get ESPN WCH Season Free Agents (core-v2)**
#' @rdname espn_wch_season_freeagents
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of free agents to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_freeagents()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per free-agent athlete
#'   reference. **Note**: The WCH free-agents endpoint (`seasons/{season}/freeagents`)
#'   currently returns `count=0` for all tested seasons — the endpoint exists
#'   but ESPN does not populate it for WCH. An empty tibble is returned
#'   gracefully in that case:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete-in-season object.        |
#'    |athlete_id  |character |ESPN athlete id parsed from the `$ref` URL.         |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |count       |integer   |Total free agents in the collection (typically 0).  |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not populate this endpoint)
#'   try(espn_wch_season_freeagents(season = 2026))
#' }
espn_wch_season_freeagents <- function(season = most_recent_nhl_season(),
                                        limit  = 100,
                                        ...) {
  .espn_hockey_season_freeagents(league = "womens-college-hockey", season = season,
                                  limit = limit, ...)
}

#' @name espn_wch_season_powerindex
NULL
#' @title **Get ESPN WCH Season Power Index (core-v2)**
#' @rdname espn_wch_season_powerindex
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param team_id Optional ESPN team identifier (character or numeric). When
#'   provided, fetches power-index data for a single team. Defaults to `NULL`
#'   (collection for all teams).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_powerindex()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per power-index entry, or
#'   an empty tibble when no data is available. **Note**: ESPN does not publish
#'   power-index data for WCH — this endpoint consistently returns empty
#'   results for hockey:
#'
#'    |col_name        |types     |description                                      |
#'    |:---------------|:---------|:------------------------------------------------|
#'    |ref             |character |`$ref` URL for the power-index object.           |
#'    |powerindex_id   |character |Power-index id parsed from the `$ref` URL.       |
#'    |season          |integer   |Season year (echoed from arg).                   |
#'    |team_id         |character |Team id (echoed from arg, if supplied).          |
#'    |count           |integer   |Total entries in the collection.                 |
#'    |page_count      |integer   |Total pages in the collection.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not publish powerindex for hockey)
#'   try(espn_wch_season_powerindex(season = 2026))
#' }
espn_wch_season_powerindex <- function(season  = most_recent_nhl_season(),
                                        team_id = NULL,
                                        ...) {
  .espn_hockey_season_powerindex(league = "womens-college-hockey", season = season,
                                  team_id = team_id, ...)
}

#' @name espn_wch_season_powerindex_leaders
NULL
#' @title **Get ESPN WCH Season Power Index Leaders (core-v2)**
#' @rdname espn_wch_season_powerindex_leaders
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_powerindex_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per power-index leader, or
#'   an empty tibble when no data is available. **Note**: ESPN does not publish
#'   power-index data for WCH:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the power-index leader object.        |
#'    |leader_id   |character |Leader id parsed from the `$ref` URL.                |
#'    |season      |integer   |Season year (echoed from arg).                       |
#'    |count       |integer   |Total entries in the collection.                     |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not publish powerindex for hockey)
#'   try(espn_wch_season_powerindex_leaders(season = 2026))
#' }
espn_wch_season_powerindex_leaders <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_powerindex_leaders(league = "womens-college-hockey", season = season, ...)
}

#' @name espn_wch_season_awards
NULL
#' @title **Get ESPN WCH Season Awards (core-v2)**
#' @rdname espn_wch_season_awards
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Optional maximum number of awards (ESPN `limit`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_season_awards()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per award reference, or an
#'   empty tibble when no awards are available for the season. Awards are
#'   typically populated after the regular season ends:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the award object.                     |
#'    |award_id    |character |ESPN award id parsed from the `$ref` URL.            |
#'    |season      |integer   |Season year (echoed from arg).                       |
#'    |count       |integer   |Total awards in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_awards(season = 2025))
#' }
espn_wch_season_awards <- function(season = most_recent_nhl_season(), limit = NULL, ...) {
  .espn_hockey_season_awards(league = "womens-college-hockey", season = season, limit = limit, ...)
}

#' @name espn_wch_players_index
NULL
#' @title **Get ESPN WCH Players Index**
#' @rdname espn_wch_players_index
#' @author Saiem Gilani
#' @description Returns a paginated index of WCH athletes from ESPN. This is a
#'   site_v2-named parity alias for athletes_index; both wrap the
#'   same `core_v2 /athletes` backing endpoint. Prefer
#'   athletes_index for the established function with the same
#'   interface.
#' @param active Filter by active status. `TRUE` (default) = active players
#'   only; `FALSE` = inactive only; `NULL` = no filter.
#' @param limit Number of athletes per page (default `100`; max ~200).
#' @param page Page number (default `1`). Use the `page_count` column in the
#'   result to determine the total number of pages.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_players_index()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per athlete in the
#'   requested page:
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete object.                    |
#'    |athlete_id  |character |ESPN athlete id parsed from the `$ref` URL.           |
#'    |active      |logical   |The `active` filter value echoed from arg.            |
#'    |page        |integer   |The `page` value echoed from arg.                     |
#'    |count       |integer   |Total athlete count matching the filter.              |
#'    |page_count  |integer   |Total number of pages.                                |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @seealso athletes_index for the established alias.
#' @examples
#' \donttest{
#'   try(espn_wch_players_index(active = TRUE, limit = 10, page = 1))
#' }
espn_wch_players_index <- function(active = TRUE,
                                    limit  = 100,
                                    page   = 1,
                                    ...) {
  .espn_hockey_players_index(league = "womens-college-hockey",
                              active = active,
                              limit  = limit,
                              page   = page,
                              ...)
}

#' @name espn_wch_player_core
NULL
#' @title **Get ESPN WCH Player Core (core-v2)**
#' @rdname espn_wch_player_core
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_core()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with athlete metadata:
#'
#'    |col_name           |types     |description                                           |
#'    |:------------------|:---------|:-----------------------------------------------------|
#'    |athlete_id         |character |ESPN athlete id (echoed from arg).                    |
#'    |uid                |character |Athlete uid string.                                   |
#'    |guid               |character |Athlete global unique identifier.                     |
#'    |type               |character |Sport type (e.g. "hockey").                           |
#'    |first_name         |character |First name.                                           |
#'    |last_name          |character |Last name.                                            |
#'    |full_name          |character |Full display name.                                    |
#'    |display_name       |character |Display name.                                         |
#'    |short_name         |character |Short name (e.g. "P. Laine").                         |
#'    |weight             |numeric   |Weight in pounds.                                     |
#'    |display_weight     |character |Formatted weight string.                              |
#'    |height             |numeric   |Height in inches.                                     |
#'    |display_height     |character |Formatted height string.                              |
#'    |age                |integer   |Current age.                                          |
#'    |date_of_birth      |character |Date of birth (ISO 8601).                             |
#'    |debut_year         |integer   |Year of WCH debut.                                    |
#'    |birth_city         |character |Birth city.                                           |
#'    |birth_country      |character |Birth country abbreviation.                           |
#'    |slug               |character |URL slug.                                             |
#'    |jersey             |character |Jersey number.                                        |
#'    |hand               |character |Handedness (e.g. "L" or "R").                         |
#'    |active             |logical   |Whether athlete is currently active.                  |
#'    |position_ref       |character |`$ref` URL for the position object.                   |
#'    |team_ref           |character |`$ref` URL for the current team object.               |
#'    |statistics_ref     |character |`$ref` URL for career statistics.                     |
#'    |contracts_ref      |character |`$ref` URL for contracts.                             |
#'    |statisticslog_ref  |character |`$ref` URL for the statistics log.                    |
#'    |athlete_ref        |character |`$ref` URL for this athlete object.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_core(athlete_id = "5263060"))
#' }
espn_wch_player_core <- function(athlete_id, ...) {
  .espn_hockey_player_core(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_career_stats
NULL
#' @title **Get ESPN WCH Player Career Stats (core-v2)**
#' @rdname espn_wch_player_career_stats
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5263060"`). Obtain via [espn_wch_team_roster()].
#' @param stat_type Optional integer stat-type path segment (default `NULL`,
#'   which uses the base `/statistics` path). ESPN redirects the base path to
#'   `/statistics/0` (All Splits). Pass `0` explicitly for identical behaviour.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_career_stats()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with career aggregate statistics
#'   flattened wide across all stat categories. Columns vary by player type
#'   (skater vs goalie). Core columns include:
#'
#'    |col_name                    |types     |description                                          |
#'    |:---------------------------|:---------|:----------------------------------------------------|
#'    |athlete_id                  |character |ESPN athlete identifier (echoed from arg).           |
#'    |split_id                    |character |Split identifier (e.g. "0" = All Splits).            |
#'    |split_name                  |character |Split name (e.g. "All Splits").                      |
#'    |split_type                  |character |Split type (e.g. "total").                           |
#'    |split_abbr                  |character |Split abbreviation (e.g. "TOTAL").                   |
#'    |offensive_goals             |numeric   |Career goals.                                        |
#'    |offensive_assists           |numeric   |Career assists.                                      |
#'    |offensive_points            |numeric   |Career points.                                       |
#'    |general_games               |numeric   |Career games played.                                 |
#'    |penalties_penalty_minutes   |numeric   |Career penalty minutes.                              |
#'    |...                         |numeric   |Additional stat columns per category.                |
#'
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_career_stats(athlete_id = "5263060"))
#' }
espn_wch_player_career_stats <- function(athlete_id, stat_type = NULL, ...) {
  .espn_hockey_player_career_stats(league     = "womens-college-hockey",
                                    athlete_id = athlete_id,
                                    stat_type  = stat_type, ...)
}

#' @name espn_wch_player_statisticslog
NULL
#' @title **Get ESPN WCH Player Statistics Log (core-v2)**
#' @rdname espn_wch_player_statisticslog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_statisticslog()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per season-entry.
#'   This endpoint is the WCH game-log substitute (web-v3 gamelog 404s for WCH).
#'   Each row carries `$ref` URLs to follow for per-season totals and team splits.
#'
#'    |col_name          |types     |description                                          |
#'    |:-----------------|:---------|:----------------------------------------------------|
#'    |athlete_id        |character |ESPN athlete id (echoed from arg).                   |
#'    |season            |integer   |Season year parsed from the season `$ref`.           |
#'    |season_ref        |character |`$ref` URL for the season object.                    |
#'    |total_stats_ref   |character |`$ref` URL for season totals statistics.             |
#'    |team_stats_ref    |character |`$ref` URL for team-level statistics.                |
#'    |team_ref          |character |`$ref` URL for the team object.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_statisticslog(athlete_id = "5263060"))
#' }
espn_wch_player_statisticslog <- function(athlete_id, ...) {
  .espn_hockey_player_statisticslog(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_eventlog
NULL
#' @title **Get ESPN WCH Player Event Log (core-v2)**
#' @rdname espn_wch_player_eventlog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_eventlog()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per event (game) in the
#'   current season. Use `statistics_ref` to retrieve per-game statistics.
#'
#'    |col_name         |types     |description                                           |
#'    |:----------------|:---------|:-----------------------------------------------------|
#'    |athlete_id       |character |ESPN athlete id (echoed from arg).                    |
#'    |event_id         |character |ESPN event (game) id.                                 |
#'    |team_id          |character |Team id the athlete played for in this event.         |
#'    |played           |logical   |Whether the athlete played in the event.              |
#'    |event_ref        |character |`$ref` URL for the event object.                      |
#'    |competition_ref  |character |`$ref` URL for the competition object.                |
#'    |statistics_ref   |character |`$ref` URL for per-game athlete statistics.           |
#'    |count            |integer   |Total event count for this athlete.                   |
#'    |page_count       |integer   |Total page count for this endpoint.                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_eventlog(athlete_id = "5263060"))
#' }
espn_wch_player_eventlog <- function(athlete_id, ...) {
  .espn_hockey_player_eventlog(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_contracts
NULL
#' @title **Get ESPN WCH Player Contracts (core-v2)**
#' @rdname espn_wch_player_contracts
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_contracts()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per contract, or an empty
#'   data frame if no contract data is served. As of 2026-06-08, ESPN core-v2
#'   returns `count=0` for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#'
#'    |col_name     |types     |description                                           |
#'    |:------------|:---------|:-----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the contract object.                   |
#'    |contract_id  |character |Contract id parsed from the `$ref` URL.               |
#'    |athlete_id   |character |ESPN athlete id (echoed from arg).                    |
#'    |count        |integer   |Total contract count (0 in practice for NHL).         |
#'    |page_count   |integer   |Total page count.                                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_contracts(athlete_id = "5263060"))
#' }
espn_wch_player_contracts <- function(athlete_id, ...) {
  .espn_hockey_player_contracts(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_awards
NULL
#' @title **Get ESPN WCH Player Awards (core-v2)**
#' @rdname espn_wch_player_awards
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_awards()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per award, or an empty
#'   data frame if the athlete has no award entries. Each `$ref` follows to
#'   a season-award object (e.g. Hart Trophy, Conn Smythe, etc.).
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the award object.                      |
#'    |award_id    |character |Award id parsed from the `$ref` URL.                  |
#'    |athlete_id  |character |ESPN athlete id (echoed from arg).                    |
#'    |count       |integer   |Total award count.                                    |
#'    |page_count  |integer   |Total page count.                                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_awards(athlete_id = "5263060"))
#' }
espn_wch_player_awards <- function(athlete_id, ...) {
  .espn_hockey_player_awards(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_seasons
NULL
#' @title **Get ESPN WCH Player Seasons (core-v2)**
#' @rdname espn_wch_player_seasons
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_seasons()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/seasons` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint is not served for the NHL).
#'   For season-level data use `espn_wch_player_statisticslog()`.
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_seasons(athlete_id = "5263060"))
#' }
espn_wch_player_seasons <- function(athlete_id, ...) {
  .espn_hockey_player_seasons(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_records
NULL
#' @title **Get ESPN WCH Player Records (core-v2)**
#' @rdname espn_wch_player_records
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_records()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per record, or an empty
#'   data frame if no records are served. As of 2026-06-08, ESPN core-v2
#'   returns `count=0` for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#'
#'    |col_name    |types     |description                                           |
#'    |:-----------|:---------|:-----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the record object.                     |
#'    |record_id   |character |Record id parsed from the `$ref` URL.                 |
#'    |athlete_id  |character |ESPN athlete id (echoed from arg).                    |
#'    |count       |integer   |Total record count (0 in practice for NHL).           |
#'    |page_count  |integer   |Total page count.                                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_records(athlete_id = "5263060"))
#' }
espn_wch_player_records <- function(athlete_id, ...) {
  .espn_hockey_player_records(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_injuries
NULL
#' @title **Get ESPN WCH Player Injuries (core-v2)**
#' @rdname espn_wch_player_injuries
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_injuries()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/injuries` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   Use `espn_wch_injuries()` (site-v2) for league-level injury data.
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_injuries(athlete_id = "5263060"))
#' }
espn_wch_player_injuries <- function(athlete_id, ...) {
  .espn_hockey_player_injuries(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_notes
NULL
#' @title **Get ESPN WCH Player Notes (core-v2)**
#' @rdname espn_wch_player_notes
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_notes()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/notes` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_notes(athlete_id = "5263060"))
#' }
espn_wch_player_notes <- function(athlete_id, ...) {
  .espn_hockey_player_notes(league = "womens-college-hockey", athlete_id = athlete_id, ...)
}

#' @name espn_wch_player_vs_player
NULL
#' @title **Get ESPN WCH Player vs Player (core-v2)**
#' @rdname espn_wch_player_vs_player
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param opp_id ESPN opponent athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_player_vs_player()` (columns inherited); a collection may be empty.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{id}/vsathlete/{opp_id}` returns HTTP 404 for all queried NHL
#'   player pairs (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   There is no column schema: the call always yields a zero-row, zero-column `data.frame()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_player_vs_player(athlete_id = "5263060", opponent_id = "5263060"))
#' }
espn_wch_player_vs_player <- function(athlete_id, opp_id, ...) {
  .espn_hockey_player_vs_player(league = "womens-college-hockey", athlete_id = athlete_id,
                                 opponent_id = opp_id, ...)
}

#' @name espn_wch_games
NULL
#' @title **Get ESPN WCH Games (core-v2)**
#' @rdname espn_wch_games
#' @author Saiem Gilani
#' @param dates Optional date string `"YYYYMMDD"` or range `"YYYYMMDD-YYYYMMDD"`.
#'   When `NULL` (default) the most recent games are returned.
#' @param limit Maximum number of events per page (default `500`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_games()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per game event reference:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the event object.                     |
#'    |event_id    |character |ESPN event id parsed from the `$ref` URL.            |
#'    |count       |integer   |Total events in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'    |dates       |character |Dates filter (echoed from arg, if supplied).         |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_games(limit = 10))
#' }
espn_wch_games <- function(dates = NULL, limit = 500, ...) {
  .espn_hockey_games(league = "womens-college-hockey", dates = dates, limit = limit, ...)
}

#' @name espn_wch_game
NULL
#' @title **Get ESPN WCH Game Metadata (core-v2)**
#' @rdname espn_wch_game
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_wch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game()` (columns inherited); a collection may be empty.
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
#'    |competition_id    |character |Id of the primary competition (equals `event_id` for WCH). |
#'    |competition_ref   |character |`$ref` URL for the primary competition object.        |
#'    |venue_ref         |character |`$ref` URL for the venue.                             |
#'    |season_ref        |character |`$ref` URL for the season.                            |
#'    |season_type_ref   |character |`$ref` URL for the season type.                       |
#'    |league_ref        |character |`$ref` URL for the league.                            |
#'    |event_ref         |character |`$ref` URL for this event object.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game <- function(event_id, ...) {
  .espn_hockey_game(league = "womens-college-hockey", event_id = event_id, ...)
}

#' @name espn_wch_game_competition
NULL
#' @title **Get ESPN WCH Game Competition Detail (core-v2)**
#' @rdname espn_wch_game_competition
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id` (for WCH the single
#'   competition always has the same id as its parent event).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_competition()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_competition(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_competition <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_competition(league = "womens-college-hockey", event_id = event_id,
                                cid = cid, ...)
}

#' @name espn_wch_game_teams
NULL
#' @title **Get ESPN WCH Game Teams (Competitors Collection, core-v2)**
#' @rdname espn_wch_game_teams
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_teams()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per competitor (2 rows for
#'   a regular WCH game):
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_teams(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_teams <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_teams(league = "womens-college-hockey", event_id = event_id, cid = cid, ...)
}

#' @name espn_wch_game_team
NULL
#' @title **Get ESPN WCH Game Team (Competitor Detail, core-v2)**
#' @rdname espn_wch_game_team
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier. Use `espn_wch_game_teams()`
#'   to discover valid team ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_wch_game_teams(event_id = eid)
#'     espn_wch_game_team(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_game_team <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team(league = "womens-college-hockey", event_id = event_id, cid = cid,
                         team_id = team_id, ...)
}

#' @name espn_wch_game_team_roster
NULL
#' @title **Get ESPN WCH Game Team Roster (core-v2)**
#' @rdname espn_wch_game_team_roster
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_roster()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_wch_game_teams(event_id = eid)
#'     espn_wch_game_team_roster(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_game_team_roster <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_roster(league = "womens-college-hockey", event_id = event_id, cid = cid,
                                team_id = team_id, ...)
}

#' @name espn_wch_game_team_linescores
NULL
#' @title **Get ESPN WCH Game Team Linescores (core-v2)**
#' @rdname espn_wch_game_team_linescores
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_linescores()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_wch_game_teams(event_id = eid)
#'     espn_wch_game_team_linescores(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_game_team_linescores <- function(event_id, cid = event_id,
                                          team_id, ...) {
  .espn_hockey_game_team_linescores(league = "womens-college-hockey", event_id = event_id,
                                    cid = cid, team_id = team_id, ...)
}

#' @name espn_wch_game_team_statistics
NULL
#' @title **Get ESPN WCH Game Team Statistics (core-v2)**
#' @rdname espn_wch_game_team_statistics
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_statistics()` (columns inherited); a collection may be empty.
#' @return A wide one-row `fastRhockey_data` tibble. The first three columns are
#'   always present; the remaining stat columns are named `<category>_<stat>`
#'   (display value) and `<category>_<stat>_value` (numeric value) for each stat
#'   in each category. For a typical completed WCH game there are 5 stat
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_wch_game_teams(event_id = eid)
#'     espn_wch_game_team_statistics(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_game_team_statistics <- function(event_id, cid = event_id,
                                          team_id, ...) {
  .espn_hockey_game_team_statistics(league = "womens-college-hockey", event_id = event_id,
                                    cid = cid, team_id = team_id, ...)
}

#' @name espn_wch_game_team_record
NULL
#' @title **Get ESPN WCH Game Team Record (core-v2)**
#' @rdname espn_wch_game_team_record
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_record()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per record type (e.g.
#'   overall, home, road). **Note:** this endpoint returns 404 for most WCH
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_wch_game_teams(event_id = eid)
#'     espn_wch_game_team_record(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_game_team_record <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_record(league = "womens-college-hockey", event_id = event_id,
                                cid = cid, team_id = team_id, ...)
}

#' @name espn_wch_game_team_leaders
NULL
#' @title **Get ESPN WCH Game Team Leaders (core-v2)**
#' @rdname espn_wch_game_team_leaders
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param team_id ESPN team/competitor identifier. Use `espn_wch_game_teams(event_id)$team_id`
#'   to find valid team ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_team_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader).
#'   For a typical completed WCH game there are 6 stat categories (goals, assists,
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     tms <- espn_wch_game_teams(event_id = eid)
#'     espn_wch_game_team_leaders(event_id = eid, team_id = tms$team_id[1])
#'   })
#' }
espn_wch_game_team_leaders <- function(event_id, cid = event_id, team_id, ...) {
  .espn_hockey_game_team_leaders(league = "womens-college-hockey", event_id = event_id,
                                 cid = cid, team_id = team_id, ...)
}

#' @name espn_wch_game_odds
NULL
#' @title **Get ESPN WCH Game Odds (core-v2)**
#' @rdname espn_wch_game_odds
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_odds()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_odds(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_odds <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_odds(league = "womens-college-hockey", event_id = event_id, cid = cid, ...)
}

#' @name espn_wch_game_probabilities
NULL
#' @title **Get ESPN WCH Game Win Probabilities (core-v2)**
#' @rdname espn_wch_game_probabilities
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param limit Maximum number of probability entries to return. Defaults to `300`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_probabilities()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per play probability entry. **Note:**
#'   This endpoint is sparse for WCH — most games return an empty tibble with a
#'   warning. When populated, columns include:
#'
#'    |col_name            |types     |description                                           |
#'    |:-------------------|:---------|:-----------------------------------------------------|
#'    |event_id            |character |ESPN event id (echoed from arg).                      |
#'    |cid                 |character |Competition id (echoed from arg).                     |
#'    |sequence_number     |integer   |Play sequence number this probability is tied to.     |
#'    |home_win_percentage |numeric   |Home team win probability (0-1).                      |
#'    |away_win_percentage |numeric   |Away team win probability (0-1).                      |
#'    |tie_percentage      |numeric   |Tie probability (0-1; typically 0 for WCH).           |
#'    |home_toss_up_change |numeric   |Change in home team win probability.                  |
#'    |away_toss_up_change |numeric   |Change in away team win probability.                  |
#'    |play_ref            |character |`$ref` URL for the associated play object.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_probabilities(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_probabilities <- function(event_id, cid = event_id,
                                        limit = 300, ...) {
  .espn_hockey_game_probabilities(league = "womens-college-hockey", event_id = event_id,
                                  cid = cid, limit = limit, ...)
}

#' @name espn_wch_game_plays
NULL
#' @title **Get ESPN WCH Game Plays (core-v2 Play-by-Play)**
#' @rdname espn_wch_game_plays
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_wch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id` (for WCH the single
#'   competition always has the same id as its parent event).
#' @param limit Maximum number of plays to return. Defaults to `1000`. A typical
#'   WCH game has 200-260 plays.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_plays()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per play:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | event_id | character | ESPN event id (echoed from arg). |
#'    | cid | character | Competition id (echoed from arg). |
#'    | id | character | ESPN play identifier. |
#'    | sequence_number | integer | Play sequence number within the game. |
#'    | type_id | character | Play type id. |
#'    | type_text | character | Play type label (e.g. "Goal", "Shot", "Face Off"). |
#'    | type_abbreviation | character | Play type abbreviation. |
#'    | text | character | Full play description text. |
#'    | short_text | character | Short play description text. |
#'    | period_number | integer | Period number (1-3 regulation, 4+ OT). |
#'    | period_display | character | Period display string (e.g. "1st"). |
#'    | clock_value | numeric | Clock value in seconds. |
#'    | clock_display | character | Clock display string (e.g. "19:34"). |
#'    | away_score | integer | Away team score after this play. |
#'    | home_score | integer | Home team score after this play. |
#'    | scoring_play | logical | Whether this play resulted in a goal. |
#'    | score_value | integer | Point value of the scoring play (1 for a goal). |
#'    | priority | logical | Whether this is a priority play for display. |
#'    | shooting_play | logical | Whether this is a shot on goal. |
#'    | is_penalty | logical | Whether this is a penalty play. |
#'    | wallclock | character | Wall-clock UTC timestamp of the play. |
#'    | coordinate_x | numeric | X coordinate on the ice surface. |
#'    | coordinate_y | numeric | Y coordinate on the ice surface. |
#'    | strength_id | character | Strength situation id (e.g. even strength, PP, SH). |
#'    | strength_text | character | Strength situation description. |
#'    | team_ref | character | `$ref` URL for the team associated with the play. |
#'    | play_ref | character | `$ref` URL for this play object. |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_plays(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_plays <- function(event_id, cid = event_id, limit = 1000, ...) {
  .espn_hockey_game_plays(league = "womens-college-hockey", event_id = event_id,
                          cid = cid, limit = limit, ...)
}

#' @name espn_wch_game_play
NULL
#' @title **Get ESPN WCH Game Play Detail (core-v2)**
#' @rdname espn_wch_game_play
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_wch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id` (for WCH the single
#'   competition always has the same id as its parent event).
#' @param play_id ESPN play identifier. Use `espn_wch_game_plays(event_id)$id`
#'   to find valid play ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_play()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     pls <- espn_wch_game_plays(event_id = eid)
#'     espn_wch_game_play(event_id = eid, play_id = pls$id[1])
#'   })
#' }
espn_wch_game_play <- function(event_id, cid = event_id, play_id, ...) {
  .espn_hockey_game_play(league = "womens-college-hockey", event_id = event_id,
                         cid = cid, play_id = play_id, ...)
}

#' @name espn_wch_game_play_personnel
NULL
#' @title **Get ESPN WCH Game Play Personnel (core-v2)**
#' @rdname espn_wch_game_play_personnel
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param play_id ESPN play identifier.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_play_personnel()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (competitor, athlete)
#'   pair on the play. For WCH this endpoint is typically sparse: the competitor
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb  <- espn_wch_scoreboard(dates = "20250110")
#'     eid <- sb$game_id[1]
#'     pls <- espn_wch_game_plays(event_id = eid)
#'     espn_wch_game_play_personnel(event_id = eid, play_id = pls$id[1])
#'   })
#' }
espn_wch_game_play_personnel <- function(event_id, cid = event_id, play_id, ...) {
  .espn_hockey_game_play_personnel(league = "womens-college-hockey", event_id = event_id,
                                   cid = cid, play_id = play_id, ...)
}

#' @name espn_wch_game_situation
NULL
#' @title **Get ESPN WCH Game Situation (core-v2)**
#' @rdname espn_wch_game_situation
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_situation()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_situation(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_situation <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_situation(league = "womens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_wch_game_status
NULL
#' @title **Get ESPN WCH Game Status (core-v2)**
#' @rdname espn_wch_game_status
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_wch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_status()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_status(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_status <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_status(league = "womens-college-hockey", event_id = event_id,
                            cid = cid, ...)
}

#' @name espn_wch_game_officials
NULL
#' @title **Get ESPN WCH Game Officials (core-v2)**
#' @rdname espn_wch_game_officials
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_officials()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_officials(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_officials <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_officials(league = "womens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_wch_game_broadcasts
NULL
#' @title **Get ESPN WCH Game Broadcasts (core-v2)**
#' @rdname espn_wch_game_broadcasts
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_broadcasts()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_broadcasts(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_broadcasts <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_broadcasts(league = "womens-college-hockey", event_id = event_id,
                               cid = cid, ...)
}

#' @name espn_wch_game_predictor
NULL
#' @title **Get ESPN WCH Game Predictor (core-v2)**
#' @rdname espn_wch_game_predictor
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_predictor()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with ESPN Predictor data. **Note:**
#'   This endpoint is sparse for WCH — most games return an empty tibble with a
#'   warning. When populated, scalar fields from the predictor object are returned
#'   as character columns alongside `event_id` and `cid`.
#'
#'   Columns when populated:
#'
#'    |col_name|type     |description                                                                                                                                                    |
#'    |:-------|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------|
#'    |event_id|character|ESPN event identifier (echoed from the argument).                                                                                                              |
#'    |cid     |character|Competition identifier (echoed from the argument).                                                                                                             |
#'    |<field> |character|One column per scalar field of the ESPN predictor object, cleaned by `janitor::clean_names()`; `predictor_ref` (the `$ref` URL) when the object has no scalars.|
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_predictor(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_predictor <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_predictor(league = "womens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_wch_game_powerindex
NULL
#' @title **Get ESPN WCH Game Power Index (core-v2)**
#' @rdname espn_wch_game_powerindex
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_powerindex()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with ESPN Power Index data. **Note:**
#'   This endpoint is sparse for WCH — most games return an empty tibble with a
#'   warning. When populated, columns contain power-index values alongside
#'   `event_id` and `cid`.
#'
#'   Columns when populated:
#'
#'    |col_name|type     |description                                                                                                                                                 |
#'    |:-------|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|
#'    |event_id|character|ESPN event identifier (echoed from the argument).                                                                                                           |
#'    |cid     |character|Competition identifier (echoed from the argument).                                                                                                          |
#'    |pi_ref  |character|`$ref` URL of the power-index item (list responses).                                                                                                        |
#'    |<field> |character|One column per scalar field of the ESPN power-index object (object responses), cleaned by `janitor::clean_names()`; `powerindex_ref` when it has no scalars.|
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_powerindex(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_powerindex <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_powerindex(league = "womens-college-hockey", event_id = event_id,
                               cid = cid, ...)
}

#' @name espn_wch_game_propbets
NULL
#' @title **Get ESPN WCH Game Prop Bets (core-v2)**
#' @rdname espn_wch_game_propbets
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier. Use `espn_wch_scoreboard()$game_id`
#'   to find valid event ids.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_propbets()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per prop bet. **Note:** this
#'   endpoint returns 404 for most WCH games; an empty `data.frame()` is returned
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_propbets(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_propbets <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_propbets(league = "womens-college-hockey", event_id = event_id,
                              cid = cid, ...)
}

#' @name espn_wch_game_leaders
NULL
#' @title **Get ESPN WCH Game Leaders (core-v2)**
#' @rdname espn_wch_game_leaders
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_leaders()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader) pair.
#'   **Note:** This endpoint is sparse for WCH — most games return an empty tibble
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_leaders(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_leaders <- function(event_id, cid = event_id, ...) {
  .espn_hockey_game_leaders(league = "womens-college-hockey", event_id = event_id, cid = cid, ...)
}

#' @name espn_wch_game_scoringplays
NULL
#' @title **Get ESPN WCH Game Scoring Plays (core-v2)**
#' @rdname espn_wch_game_scoringplays
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param limit Maximum number of plays to fetch for filtering. Defaults to `300`.
#'   Only used when the dedicated `scoringplays` endpoint is unavailable and the
#'   function falls back to filtering `game_plays()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_scoringplays()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per scoring play (goal).
#'   **Implementation note:** The dedicated `/scoringplays` core-v2 endpoint
#'   returns 404 for WCH; this function falls back to fetching all plays via
#'   `espn_wch_game_plays()` and filtering to rows where `scoring_play == TRUE`.
#'   Columns are the same as `espn_wch_game_plays()`:
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
#'    |score_value       |integer   |Always 1 for WCH goals.                               |
#'    |coordinate_x      |numeric   |X coordinate of the shot.                             |
#'    |coordinate_y      |numeric   |Y coordinate of the shot.                             |
#'    |strength_text     |character |Strength situation (e.g. "Even Strength", "Power Play"). |
#'    |team_ref          |character |`$ref` URL for the scoring team.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb <- espn_wch_scoreboard(dates = "20250110")
#'     espn_wch_game_scoringplays(event_id = sb$game_id[1])
#'   })
#' }
espn_wch_game_scoringplays <- function(event_id, cid = event_id,
                                       limit = 300, ...) {
  .espn_hockey_game_scoringplays(league = "womens-college-hockey", event_id = event_id,
                                 cid = cid, limit = limit, ...)
}

#' @name espn_wch_game_official_detail
NULL
#' @title **Get ESPN WCH Game Official Detail (core-v2)**
#' @rdname espn_wch_game_official_detail
#' @author Saiem Gilani
#' @param event_id ESPN event (game) identifier.
#' @param cid Competition identifier. Defaults to `event_id`.
#' @param official_id ESPN official identifier. Use `espn_wch_game_officials(event_id)$official_id`
#'   to find valid official ids for a game.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_game_official_detail()` (columns inherited); a collection may be empty.
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
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     sb   <- espn_wch_scoreboard(dates = "20250110")
#'     eid  <- sb$game_id[1]
#'     offs <- espn_wch_game_officials(event_id = eid)
#'     espn_wch_game_official_detail(event_id = eid, official_id = offs$official_id[1])
#'   })
#' }
espn_wch_game_official_detail <- function(event_id, cid = event_id,
                                          official_id, ...) {
  .espn_hockey_game_official_detail(league = "womens-college-hockey", event_id = event_id,
                                    cid = cid, official_id = official_id, ...)
}

#' @name espn_wch_teams_core
NULL
#' @title **Get ESPN WCH Teams Core (core-v2 Collection)**
#' @rdname espn_wch_teams_core
#' @author Saiem Gilani
#' @param limit Maximum number of teams to return (default `1000`; the WCH has
#'   32 active teams so the default returns all).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_teams_core()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name   |types     |description                                         |
#'    |:----------|:---------|:---------------------------------------------------|
#'    |ref        |character |Core-v2 `$ref` URL for the team resource.           |
#'    |team_id    |character |Team identifier (trailing path segment of ref URL). |
#'    |count      |integer   |Total number of teams in the collection.            |
#'    |page_count |integer   |Number of pages in the paginated response.          |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_teams_core(limit = 100))
#' }
espn_wch_teams_core <- function(limit = 1000, ...) {
  .espn_hockey_teams_core(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_team_core
NULL
#' @title **Get ESPN WCH Team Core (core-v2)**
#' @rdname espn_wch_team_core
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"2137"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_team_core()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row containing enriched
#'   team metadata from the core-v2 endpoint, including `$ref` links to
#'   sub-resources:
#'
#'    |col_name            |types     |description                               |
#'    |:-------------------|:---------|:-----------------------------------------|
#'    |team_id             |character |ESPN team identifier (echoed from arg).   |
#'    |id                  |character |ESPN team identifier.                     |
#'    |guid                |character |Team global unique identifier.            |
#'    |uid                 |character |Team uid.                                 |
#'    |alternate_id_sdr    |character |SDR alternate identifier.                 |
#'    |slug                |character |URL slug.                                 |
#'    |location            |character |Team city/location.                       |
#'    |name                |character |Team mascot name.                         |
#'    |nickname            |character |Team nickname.                            |
#'    |abbreviation        |character |Team abbreviation.                        |
#'    |display_name        |character |Full display name.                        |
#'    |short_display_name  |character |Short display name.                       |
#'    |color               |character |Primary color hex.                        |
#'    |alternate_color     |character |Alternate color hex.                      |
#'    |is_active           |logical   |Whether the team is active.               |
#'    |is_all_star         |logical   |Whether the team is an all-star team.     |
#'    |logo                |character |Primary logo URL.                         |
#'    |record_ref          |character |Ref URL for team record resource.         |
#'    |athletes_ref        |character |Ref URL for team athletes resource.       |
#'    |venue_ref           |character |Ref URL for team venue resource.          |
#'    |groups_ref          |character |Ref URL for team groups resource.         |
#'    |statistics_ref      |character |Ref URL for team statistics resource.     |
#'    |leaders_ref         |character |Ref URL for team leaders resource.        |
#'    |injuries_ref        |character |Ref URL for team injuries resource.       |
#'    |awards_ref          |character |Ref URL for team awards resource.         |
#'    |franchise_ref       |character |Ref URL for franchise resource.           |
#'    |events_ref          |character |Ref URL for team events resource.         |
#'    |transactions_ref    |character |Ref URL for team transactions resource.   |
#'    |coaches_ref         |character |Ref URL for team coaches resource.        |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_team_core(team_id = "2137"))
#' }
espn_wch_team_core <- function(team_id, ...) {
  .espn_hockey_team_core(league = "womens-college-hockey", team_id = team_id, ...)
}

#' @name espn_wch_venues
NULL
#' @title **Get ESPN WCH Venues (core-v2)**
#' @rdname espn_wch_venues
#' @author Saiem Gilani
#' @param limit Maximum number of venues to return (default `1000`). The WCH
#'   venue catalog contains ~180 entries; `200` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_venues()`; columns verified against a captured WCH body.
#' @return A `fastRhockey_data` tibble with one row per venue reference:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | ref | character | `$ref` URL for the venue object. |
#'    | venue_id | character | ESPN venue id parsed from the `$ref` URL. |
#'    | count | integer | Total venues in the collection. |
#'    | page_count | integer | Total pages in the collection. |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_venues())
#' }
espn_wch_venues <- function(limit = 1000, ...) {
  .espn_hockey_venues(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_venue
NULL
#' @title **Get ESPN WCH Venue (core-v2)**
#' @rdname espn_wch_venue
#' @author Saiem Gilani
#' @param venue_id ESPN venue identifier (character or numeric). Use
#'   `espn_wch_venues()` to discover valid venue ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_venue()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with venue detail:
#'
#'    |col_name   |types     |description                                        |
#'    |:----------|:---------|:--------------------------------------------------|
#'    |venue_id   |character |ESPN venue id (echoed from arg).                   |
#'    |id         |character |ESPN venue identifier from response.               |
#'    |guid       |character |Venue global unique identifier (if present).       |
#'    |full_name  |character |Venue full name.                                   |
#'    |city       |character |Venue city.                                        |
#'    |state      |character |Venue state/province abbreviation.                 |
#'    |country    |character |Venue country.                                     |
#'    |grass      |logical   |Whether the surface is grass.                      |
#'    |indoor     |logical   |Whether the venue is indoors.                      |
#'    |capacity   |integer   |Seating capacity (if returned; often `NA` for WCH).|
#'    |venue_ref  |character |`$ref` URL for this venue object.                  |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     vns <- espn_wch_venues()
#'     espn_wch_venue(venue_id = vns$venue_id[1])
#'   })
#' }
espn_wch_venue <- function(venue_id, ...) {
  .espn_hockey_venue(league = "womens-college-hockey", venue_id = venue_id, ...)
}

#' @name espn_wch_franchises
NULL
#' @title **Get ESPN WCH Franchises (core-v2)**
#' @rdname espn_wch_franchises
#' @author Saiem Gilani
#' @param limit Maximum number of franchises to return (default `200`). The
#'   WCH has 32 active franchises; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_franchises()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per franchise reference:
#'
#'    |col_name      |types     |description                                        |
#'    |:-------------|:---------|:--------------------------------------------------|
#'    |ref           |character |`$ref` URL for the franchise object.               |
#'    |franchise_id  |character |ESPN franchise id parsed from the `$ref` URL.      |
#'    |count         |integer   |Total franchises in the collection.                |
#'    |page_count    |integer   |Total pages in the collection.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_franchises())
#' }
espn_wch_franchises <- function(limit = 200, ...) {
  .espn_hockey_franchises(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_franchise
NULL
#' @title **Get ESPN WCH Franchise (core-v2)**
#' @rdname espn_wch_franchise
#' @author Saiem Gilani
#' @param franchise_id ESPN franchise identifier (character or numeric). Use
#'   `espn_wch_franchises()` to discover valid franchise ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_franchise()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with franchise detail:
#'
#'    |col_name            |types     |description                                         |
#'    |:-------------------|:---------|:---------------------------------------------------|
#'    |franchise_id        |character |ESPN franchise id (echoed from arg).                |
#'    |id                  |character |ESPN franchise identifier from response.            |
#'    |uid                 |character |Franchise uid string.                               |
#'    |slug                |character |Franchise URL slug.                                 |
#'    |location            |character |Franchise city/location.                            |
#'    |name                |character |Franchise mascot name.                              |
#'    |nickname            |character |Franchise nickname.                                 |
#'    |abbreviation        |character |Franchise abbreviation.                             |
#'    |display_name        |character |Franchise display name.                             |
#'    |short_display_name  |character |Franchise short display name.                       |
#'    |color               |character |Franchise primary color hex.                        |
#'    |is_active           |logical   |Whether the franchise is currently active.          |
#'    |venue_ref           |character |`$ref` URL for the franchise's current venue.       |
#'    |team_ref            |character |`$ref` URL for the current season team object.      |
#'    |franchise_ref       |character |`$ref` URL for this franchise object.               |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     fcs <- espn_wch_franchises()
#'     espn_wch_franchise(franchise_id = fcs$franchise_id[1])
#'   })
#' }
espn_wch_franchise <- function(franchise_id, ...) {
  .espn_hockey_franchise(league = "womens-college-hockey", franchise_id = franchise_id, ...)
}

#' @name espn_wch_coach
NULL
#' @title **Get ESPN WCH Coach (core-v2)**
#' @rdname espn_wch_coach
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_wch_coaches()` to discover valid coach ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_coach()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with coach detail. Note that
#'   `careerRecords` and `coachSeasons` are sparse (empty arrays) for many WCH
#'   coaches — see `espn_wch_coach_record()` for season-scoped records:
#'
#'    |col_name       |types     |description                                        |
#'    |:--------------|:---------|:--------------------------------------------------|
#'    |coach_id       |character |ESPN coach id (echoed from arg).                   |
#'    |id             |character |ESPN coach identifier from response.               |
#'    |uid            |character |Coach uid string.                                  |
#'    |first_name     |character |Coach first name.                                  |
#'    |last_name      |character |Coach last name.                                   |
#'    |date_of_birth  |character |Coach date of birth (ISO 8601).                    |
#'    |birth_city     |character |Coach birth city.                                  |
#'    |birth_country  |character |Coach birth country.                               |
#'    |experience     |integer   |Years of coaching experience (if returned).        |
#'    |coach_ref      |character |`$ref` URL for this coach object.                  |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     ccs <- espn_wch_coaches(season = 2026)
#'     espn_wch_coach(coach_id = ccs$coach_id[1])
#'   })
#' }
espn_wch_coach <- function(coach_id, ...) {
  .espn_hockey_coach(league = "womens-college-hockey", coach_id = coach_id, ...)
}

#' @name espn_wch_coach_record
NULL
#' @title **Get ESPN WCH Coach Record (core-v2)**
#' @rdname espn_wch_coach_record
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_wch_coaches()` to discover valid coach ids.
#' @param record_type Record type path segment (`coaches/{id}/record/{record_type}`); default `0`, as sdv-py. `NULL` drops the segment.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_coach_record()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with coach career record data, or an
#'   empty tibble when the endpoint is unavailable. **Note**: The coach record
#'   path (`coaches/{id}/record`) returns HTTP 404 for NHL — this endpoint is
#'   not populated by ESPN for the NHL league. An empty tibble is returned
#'   gracefully with a warning:
#'
#'    |col_name   |types     |description                                            |
#'    |:----------|:---------|:------------------------------------------------------|
#'    |coach_id   |character |ESPN coach id (echoed from arg).                       |
#'    |...        |varies    |Additional record fields if returned (sparse for NHL). |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try({
#'     ccs <- espn_wch_coaches(season = 2026)
#'     espn_wch_coach_record(coach_id = ccs$coach_id[1])
#'   })
#' }
espn_wch_coach_record <- function(coach_id, record_type = 0, ...) {
  .espn_hockey_coach_record(league = "womens-college-hockey", coach_id = coach_id, record_type = record_type, ...)
}

#' @name espn_wch_coach_season
NULL
#' @title **Get ESPN WCH Coach Season (core-v2)**
#' @rdname espn_wch_coach_season
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_wch_season_coaches()` to find valid IDs.
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_coach_season()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per coach:
#'
#'    |col_name       |types     |description                                            |
#'    |:--------------|:---------|:------------------------------------------------------|
#'    |coach_id       |character |ESPN coach identifier (echoed from arg).               |
#'    |season         |integer   |Season year (echoed from arg).                         |
#'    |espn_id        |character |ESPN internal coach id.                                |
#'    |uid            |character |ESPN unique uid.                                       |
#'    |first_name     |character |Coach first name.                                      |
#'    |last_name      |character |Coach last name.                                       |
#'    |date_of_birth  |character |Date of birth (ISO 8601).                              |
#'    |birth_city     |character |Birth city.                                            |
#'    |birth_country  |character |Birth country.                                         |
#'    |person_ref     |character |`$ref` URL for the coach person object on core-v2.    |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_coach_season(coach_id = "900", season = 2026))
#' }
espn_wch_coach_season <- function(coach_id,
                                   season = most_recent_nhl_season(),
                                   ...) {
  .espn_hockey_coach_season(league   = "womens-college-hockey",
                              coach_id = coach_id,
                              season   = season,
                              coach_first = TRUE,
                              ...)
}

#' @name espn_wch_positions
NULL
#' @title **Get ESPN WCH Positions (core-v2)**
#' @rdname espn_wch_positions
#' @author Saiem Gilani
#' @param limit Maximum number of positions to return (default `200`). The
#'   WCH position catalog contains ~28 entries (positions + sub-positions).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_positions()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per position reference:
#'
#'    |col_name     |types     |description                                        |
#'    |:------------|:---------|:--------------------------------------------------|
#'    |ref          |character |`$ref` URL for the position object.                |
#'    |position_id  |character |ESPN position id parsed from the `$ref` URL.       |
#'    |count        |integer   |Total positions in the collection.                 |
#'    |page_count   |integer   |Total pages in the collection.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_positions())
#' }
espn_wch_positions <- function(limit = 200, ...) {
  .espn_hockey_positions(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_position
NULL
#' @title **Get ESPN WCH Position (core-v2)**
#' @rdname espn_wch_position
#' @author Saiem Gilani
#' @param position_id ESPN position identifier (character or numeric). Use
#'   `espn_wch_positions()` to discover valid position ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_position()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with position detail:
#'
#'    |col_name      |types     |description                                        |
#'    |:-------------|:---------|:--------------------------------------------------|
#'    |position_id   |character |ESPN position id (echoed from arg).                |
#'    |id            |character |ESPN position identifier from response.            |
#'    |name          |character |Position name (e.g. "Center").                     |
#'    |display_name  |character |Position display name.                             |
#'    |abbreviation  |character |Position abbreviation (e.g. "C").                  |
#'    |leaf          |logical   |Whether this is a leaf (non-parent) position.      |
#'    |parent_ref    |character |`$ref` URL for the parent position (if any).       |
#'    |position_ref  |character |`$ref` URL for this position object.               |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try({
#'     pos <- espn_wch_positions()
#'     espn_wch_position(position_id = pos$position_id[1])
#'   })
#' }
espn_wch_position <- function(position_id, ...) {
  .espn_hockey_position(league = "womens-college-hockey", position_id = position_id, ...)
}

#' @name espn_wch_tournaments
NULL
#' @title **Get ESPN WCH Tournaments (core-v2)**
#' @rdname espn_wch_tournaments
#' @author Saiem Gilani
#' @param limit Maximum number of tournaments to return (default `200`).
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_tournaments()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per tournament reference.
#'   The WCH tournaments endpoint is sparsely populated (typically 1 entry):
#'
#'    |col_name       |types     |description                                          |
#'    |:--------------|:---------|:----------------------------------------------------|
#'    |ref            |character |`$ref` URL for the tournament object.                |
#'    |tournament_id  |character |ESPN tournament id parsed from the `$ref` URL.       |
#'    |count          |integer   |Total tournaments in the collection.                 |
#'    |page_count     |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_tournaments())
#' }
espn_wch_tournaments <- function(limit = 200, ...) {
  .espn_hockey_tournaments(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_awards
NULL
#' @title **Get ESPN WCH Awards (core-v2)**
#' @rdname espn_wch_awards
#' @author Saiem Gilani
#' @param limit Maximum number of awards to return per page (default `200`).
#'   The WCH has 34 awards; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_awards()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per award:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the award object.                     |
#'    |award_id    |character |ESPN award id parsed from the `$ref` URL.            |
#'    |count       |integer   |Total awards in the collection.                      |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_awards())
#' }
espn_wch_awards <- function(limit = 200, ...) {
  .espn_hockey_awards(league = "womens-college-hockey", limit = limit, ...)
}

#' @name espn_wch_award
NULL
#' @title **Get ESPN WCH Award (core-v2)**
#' @rdname espn_wch_award
#' @author Saiem Gilani
#' @param award_id ESPN award identifier (character or numeric). Use
#'   `espn_wch_awards()$award_id` to discover valid award ids.
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_award()` (columns inherited); a collection may be empty.
#' @return A one-row `fastRhockey_data` tibble with award metadata, or an
#'   empty tibble when the endpoint is unavailable. **Note**: The individual
#'   award endpoint (`core_v2/awards/{award_id}`) currently returns HTTP 500
#'   for all tested WCH award ids — an empty tibble is returned gracefully:
#'
#'    |col_name      |types     |description                                         |
#'    |:-------------|:---------|:---------------------------------------------------|
#'    |award_id      |character |Award id (echoed from arg).                         |
#'    |id            |character |ESPN award identifier (if returned).                |
#'    |name          |character |Award name (if returned).                           |
#'    |display_name  |character |Award display name (if returned).                   |
#'    |description   |character |Award description (if returned).                    |
#'    |award_ref     |character |`$ref` URL for the award object (if returned).      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN returns HTTP 500 for this endpoint)
#'   try({
#'     awards <- espn_wch_awards()
#'     if (nrow(awards) > 0) espn_wch_award(award_id = awards$award_id[1])
#'   })
#' }
espn_wch_award <- function(award_id, ...) {
  .espn_hockey_award(league = "womens-college-hockey", award_id = award_id, ...)
}

#' @name espn_wch_leaders_core
NULL
#' @title **Get ESPN WCH League Leaders (core-v2)**
#' @rdname espn_wch_leaders_core
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_leaders_core()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination. For a typical WCH season there are 7 stat categories with
#'   25 leaders each (175 rows). For season-specific / season-type leaders
#'   use [espn_wch_season_type_leaders()].
#'
#'    |col_name                     |types     |description                                       |
#'    |:----------------------------|:---------|:-------------------------------------------------|
#'    |category_name                |character |Stat category type (e.g. "offensive").            |
#'    |category_display_name        |character |Category display name (e.g. "Goals").             |
#'    |category_short_display_name  |character |Category short display name.                      |
#'    |category_abbreviation        |character |Category abbreviation.                            |
#'    |display_value                |character |Leader stat display value.                        |
#'    |value                        |numeric   |Leader stat numeric value.                        |
#'    |rel                          |character |Relation type (e.g. "athlete").                   |
#'    |athlete_ref                  |character |`$ref` URL for the athlete on core-v2.            |
#'    |statistics_ref               |character |`$ref` URL for the athlete statistics.            |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_leaders_core())
#' }
espn_wch_leaders_core <- function(...) {
  .espn_hockey_leaders_core(league = "womens-college-hockey", ...)
}

#' @name espn_wch_league_notes
NULL
#' @title **Get ESPN WCH League Notes (core-v2)**
#' @rdname espn_wch_league_notes
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_league_notes()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per league note reference,
#'   or an empty tibble when the endpoint is unavailable. **Note**: The WCH
#'   league notes endpoint (`core_v2/notes`) currently returns `count=0` —
#'   an empty tibble is returned gracefully:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the note object.                      |
#'    |note_id     |character |ESPN note id parsed from the `$ref` URL.             |
#'    |count       |integer   |Total notes in the collection (typically 0).         |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not populate this endpoint)
#'   try(espn_wch_league_notes())
#' }
espn_wch_league_notes <- function(...) {
  .espn_hockey_league_notes(league = "womens-college-hockey", ...)
}

#' @name espn_wch_talentpicks
NULL
#' @title **Get ESPN WCH Talent Picks (core-v2)**
#' @rdname espn_wch_talentpicks
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @details League slug `womens-college-hockey`; `season` is the END year (`2026` = 2025-26). Same ESPN helper as `espn_nhl_talentpicks()` (columns inherited); a collection may be empty.
#' @return A `fastRhockey_data` tibble with one row per talent pick reference,
#'   or an empty tibble when the endpoint is unavailable. **Note**: The WCH
#'   talent picks endpoint (`core_v2/talentpicks`) currently returns `count=0`
#'   — an empty tibble is returned gracefully:
#'
#'    |col_name    |types     |description                                          |
#'    |:-----------|:---------|:----------------------------------------------------|
#'    |ref         |character |`$ref` URL for the talent pick object.               |
#'    |pick_id     |character |ESPN pick id parsed from the `$ref` URL.             |
#'    |count       |integer   |Total talent picks in the collection (typically 0).  |
#'    |page_count  |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for WCH (ESPN does not populate this endpoint)
#'   try(espn_wch_talentpicks())
#' }
espn_wch_talentpicks <- function(...) {
  .espn_hockey_talentpicks(league = "womens-college-hockey", ...)
}

#' @name espn_wch_rankings
NULL
#' @title **Get ESPN WCH Rankings (Site v2 polls)**
#' @rdname espn_wch_rankings
#' @author Saiem Gilani
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details ESPN women's college hockey polls (USCHO), league slug `womens-college-hockey`. `rank` is `NA` on
#'   the vote-receiving rows (`ranked = FALSE`); `previous_rank` keeps ESPN's `0`
#'   for a team unranked in the previous poll. Mirrors sdv-py `espn_wch_rankings()`.
#' @return A `fastRhockey_data` tibble with one row per (poll, team), always with
#'   the same 25 columns; zero rows (with those columns) when no poll is published:
#'
#'    | col_name | type | description |
#'    |:---|:---|:---|
#'    | poll_id | integer | ESPN poll identifier. |
#'    | poll_name | character | Poll name. |
#'    | poll_short_name | character | Poll short name. |
#'    | poll_type | character | Poll type code. |
#'    | season | integer | Season end-year. |
#'    | season_type | integer | Season type (2 = regular season, 3 = postseason). |
#'    | week | integer | Poll week within the season type. |
#'    | week_display | character | Poll week display string. |
#'    | poll_date | character | Poll date (ISO 8601). |
#'    | ranked | logical | `TRUE` for a ranked team, `FALSE` for a team receiving votes. |
#'    | team_id | character | ESPN team identifier. |
#'    | rank | integer | Current rank (`NA` when not ranked). |
#'    | previous_rank | integer | Previous poll rank (`0` = unranked). |
#'    | points | numeric | Poll points. |
#'    | first_place_votes | integer | First-place votes (`NA` when not published). |
#'    | trend | character | Rank trend indicator. |
#'    | record_summary | character | Team record summary. |
#'    | team_uid | character | ESPN team UID. |
#'    | team_location | character | Team location. |
#'    | team_name | character | Team name. |
#'    | team_nickname | character | Team nickname. |
#'    | team_abbreviation | character | Team abbreviation. |
#'    | team_color | character | Team colour hex (`NA` when ESPN ships `"NULL"`). |
#'    | team_logo | character | Team logo URL. |
#'    | last_updated | character | Entry last-updated timestamp. |
#'
#' @importFrom dplyr bind_rows
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_rankings())
#' }
espn_wch_rankings <- function(...) {
  .espn_hockey_rankings(league = "womens-college-hockey", ...)
}

#' @name espn_wch_fpi
NULL
#' @title **Get ESPN WCH Power Index (fitt-v3 FPI)**
#' @rdname espn_wch_fpi
#' @author Saiem Gilani
#' @param season Season (4-digit END year) whose table to return; `NULL` (the
#'   default) returns the current season.
#' @param limit Page size. `NULL` (the default) leaves ESPN's default.
#' @param page Page number. `NULL` (the default) leaves ESPN's default.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. One row per team with a column per FPI field,
#'   snake-cased from the payload's `categories` labels (zipped by position and
#'   length-guarded); a zero-row `data.frame` when ESPN publishes no table for
#'   women's college hockey. Mirrors sdv-py `espn_wch_fpi()`.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name                |types     |description                                  |
#'    |:-----------------------|:---------|:--------------------------------------------|
#'    |team_id                 |character |ESPN team identifier.                        |
#'    |team_uid                |character |ESPN team UID.                               |
#'    |team_abbreviation       |character |Team abbreviation.                           |
#'    |team_display_name       |character |Team display name.                           |
#'    |team_short_display_name |character |Team short display name.                     |
#'    |team_nickname           |character |Team nickname.                               |
#'    |season                  |integer   |Requested season (END year).                 |
#'    |...                     |varies    |One column per FPI field in the payload.     |
#'
#' @importFrom dplyr bind_rows
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_fpi(season = 2026))
#' }
espn_wch_fpi <- function(season = NULL, limit = NULL, page = NULL, ...) {
  query <- list()
  if (!is.null(season)) query[["season"]] <- season
  if (!is.null(limit))  query[["limit"]]  <- limit
  if (!is.null(page))   query[["page"]]   <- page
  .espn_hockey_powerindex(league = "womens-college-hockey", kind = "fpi", query = query, ...)
}

#' @name espn_wch_season_week_powerindex
NULL
#' @title **Get ESPN WCH Weekly Power Index (core-v2)**
#' @rdname espn_wch_season_week_powerindex
#' @author Saiem Gilani
#' @param season Season END year (e.g. `2026`).
#' @param season_type Season type (1 = preseason, 2 = regular season, 3 = postseason).
#' @param week Week number within the season type.
#' @param limit Page size; `NULL` (the default) leaves ESPN's default.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. One row per team, `predictives` and
#'   `efficiencies` flattened to one column per field (a name colliding across
#'   the two blocks is suffixed). A zero-row `data.frame` when unpublished.
#'   Mirrors sdv-py `espn_wch_season_week_powerindex()`.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name          |types     |description                                  |
#'    |:-----------------|:---------|:--------------------------------------------|
#'    |team_id           |integer   |ESPN team identifier (parsed from the team `$ref`). |
#'    |season            |integer   |Season END year.                             |
#'    |last_updated      |character |When ESPN computed the snapshot.             |
#'    |run_date_time_key |character |ESPN run key for the snapshot.               |
#'    |...               |varies    |One column per predictive / efficiency field.|
#'
#' @importFrom dplyr bind_rows
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_week_powerindex(season = 2026, season_type = 2, week = 3))
#' }
espn_wch_season_week_powerindex <- function(season, season_type, week, limit = NULL, ...) {
  query <- list()
  if (!is.null(limit)) query[["limit"]] <- limit
  .espn_hockey_powerindex(
    league = "womens-college-hockey", kind = "weekly",
    path = paste0("seasons/", season, "/types/", season_type, "/weeks/", week, "/powerindex"),
    query = query, ...)
}

#' @name espn_wch_season_week_rankings
NULL
#' @title **Get ESPN WCH Weekly Rankings (core-v2)**
#' @rdname espn_wch_season_week_rankings
#' @author Saiem Gilani
#' @param season Season END year (e.g. `2026`).
#' @param season_type Season type (1 = preseason, 2 = regular season, 3 = postseason).
#' @param week Week number within the season type.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. Core-v2 `weeks/{week}/rankings` collection;
#'   `$ref`-only items become `ref` + `ranking_id`, inline items are flattened.
#'   Mirrors sdv-py `espn_wch_season_week_rankings()`.
#' @return A `fastRhockey_data` tibble with one row per ranking item:
#'
#'    |col_name     |types     |description                                  |
#'    |:------------|:---------|:--------------------------------------------|
#'    |ref          |character |`$ref` URL of the ranking object.            |
#'    |ranking_id  |character |Trailing id of the ranking `$ref`.           |
#'    |season       |integer   |Season END year (echoed from arg).           |
#'    |season_type  |integer   |Season type (echoed from arg).               |
#'    |week         |integer   |Week (echoed from arg).                      |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_week_rankings(season = 2026, season_type = 2, week = 3))
#' }
espn_wch_season_week_rankings <- function(season, season_type, week, ...) {
  .espn_hockey_items(
    league = "womens-college-hockey",
    path = paste0("seasons/", season, "/types/", season_type, "/weeks/", week, "/rankings"),
    id_col = "ranking_id", label = "Week Rankings",
    echo = list(season = as.integer(season), season_type = as.integer(season_type),
                week = as.integer(week)), ...)
}

#' @name espn_wch_season_recruits
NULL
#' @title **Get ESPN WCH Season Recruits (core-v2)**
#' @rdname espn_wch_season_recruits
#' @author Saiem Gilani
#' @param season Season END year (e.g. `2026`).
#' @param limit Maximum number of items per page. Defaults to `1000`.
#' @param page Page number. Defaults to `1`.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. Core-v2 `seasons/{season}/recruits`;
#'   `$ref`-only items become `ref` + `recruit_id`, inline items are flattened.
#'   Mirrors sdv-py `espn_wch_season_recruits()`; ESPN may publish nothing for women's college hockey.
#' @return A `fastRhockey_data` tibble with one row per recruit:
#'
#'    |col_name   |types     |description                                  |
#'    |:----------|:---------|:--------------------------------------------|
#'    |ref        |character |`$ref` URL of the recruit object.            |
#'    |recruit_id |character |Trailing id of the recruit `$ref`.           |
#'    |season     |integer   |Season END year (echoed from arg).           |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_season_recruits(season = 2026))
#' }
espn_wch_season_recruits <- function(season, limit = 1000, page = 1, ...) {
  .espn_hockey_items(
    league = "womens-college-hockey", path = paste0("seasons/", season, "/recruits"),
    id_col = "recruit_id", label = "Season Recruits",
    query = list(limit = limit, page = page),
    echo = list(season = as.integer(season)), ...)
}

#' @name espn_wch_recruiting_years
NULL
#' @title **Get ESPN WCH Recruiting Years (core-v2)**
#' @rdname espn_wch_recruiting_years
#' @author Saiem Gilani
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. Core-v2 `recruiting` collection (one item per
#'   recruiting class year). Mirrors sdv-py `espn_wch_recruiting_years()`.
#' @return A `fastRhockey_data` tibble with one row per recruiting year:
#'
#'    |col_name |types     |description                                  |
#'    |:--------|:---------|:--------------------------------------------|
#'    |ref      |character |`$ref` URL of the recruiting-year object.    |
#'    |year     |character |Trailing id of the `$ref` (the class year).  |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_recruiting_years())
#' }
espn_wch_recruiting_years <- function(...) {
  .espn_hockey_items(league = "womens-college-hockey", path = "recruiting", id_col = "year",
                     label = "Recruiting Years", ...)
}

#' @name espn_wch_recruiting_players
NULL
#' @title **Get ESPN WCH Recruiting Players (core-v2)**
#' @rdname espn_wch_recruiting_players
#' @author Saiem Gilani
#' @param year Recruiting class year.
#' @param limit Maximum number of items per page. Defaults to `1000`.
#' @param page Page number. Defaults to `1`.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. Core-v2 `recruiting/{year}/athletes`; inline
#'   athlete objects are flattened and snake-cased, `$ref`-only items become `ref` +
#'   `recruit_id`. Mirrors sdv-py `espn_wch_recruiting_players()`.
#' @return A `fastRhockey_data` tibble with one row per recruit:
#'
#'    |col_name   |types     |description                                  |
#'    |:----------|:---------|:--------------------------------------------|
#'    |ref        |character |`$ref` URL of the recruit (when ref-only).   |
#'    |recruit_id |character |Trailing id of the `$ref` (when ref-only).   |
#'    |year       |integer   |Recruiting class year (echoed from arg).     |
#'    |...        |varies    |Flattened athlete fields when ESPN inlines them. |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_recruiting_players(year = 2026))
#' }
espn_wch_recruiting_players <- function(year, limit = 1000, page = 1, ...) {
  .espn_hockey_items(
    league = "womens-college-hockey", path = paste0("recruiting/", year, "/athletes"),
    id_col = "recruit_id", label = "Recruiting Players",
    query = list(limit = limit, page = page),
    echo = list(year = as.integer(year)), ...)
}

#' @name espn_wch_recruiting_rankings
NULL
#' @title **Get ESPN WCH Recruiting Rankings (core-v2)**
#' @rdname espn_wch_recruiting_rankings
#' @author Saiem Gilani
#' @param year Recruiting class year.
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. Core-v2 `recruiting/{year}/rankings`;
#'   `$ref`-only items become `ref` + `ranking_id`. Mirrors sdv-py
#'   `espn_wch_recruiting_rankings()`.
#' @return A `fastRhockey_data` tibble with one row per ranking set:
#'
#'    |col_name   |types     |description                                  |
#'    |:----------|:---------|:--------------------------------------------|
#'    |ref        |character |`$ref` URL of the ranking set.               |
#'    |ranking_id |character |Trailing id of the `$ref`.                   |
#'    |year       |integer   |Recruiting class year (echoed from arg).     |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_recruiting_rankings(year = 2026))
#' }
espn_wch_recruiting_rankings <- function(year, ...) {
  .espn_hockey_items(
    league = "womens-college-hockey", path = paste0("recruiting/", year, "/rankings"),
    id_col = "ranking_id", label = "Recruiting Rankings",
    echo = list(year = as.integer(year)), ...)
}

#' @name espn_wch_standings_core
NULL
#' @title **Get ESPN WCH Standings (core-v2)**
#' @rdname espn_wch_standings_core
#' @author Saiem Gilani
#' @param ... Passed through to `.retry_request()` (e.g. `proxy =`).
#' @details League slug `womens-college-hockey`. Core-v2 league-level `standings`; ESPN answers
#'   with a bare `$ref` pointer to the current season's group standings, returned
#'   as one `ref` row. Mirrors sdv-py `espn_wch_standings_core()`, which takes
#'   no arguments; use `espn_wch_standings()` for the per-team table.
#' @return A `fastRhockey_data` tibble with one row per standings item:
#'
#'    |col_name     |types     |description                                  |
#'    |:------------|:---------|:--------------------------------------------|
#'    |ref          |character |`$ref` URL of the standings object.          |
#'    |standings_id |character |Trailing id of the `$ref`.                   |
#'
#' @export
#' @family ESPN WCH Functions
#' @examples
#' \donttest{
#'   try(espn_wch_standings_core())
#' }
espn_wch_standings_core <- function(...) {
  .espn_hockey_items(league = "womens-college-hockey", path = "standings", id_col = "standings_id",
                     label = "Standings", ...)
}
