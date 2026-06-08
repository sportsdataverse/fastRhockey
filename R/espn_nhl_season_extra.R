# espn_nhl_season_extra.R
# ESPN NHL: core-v2 season long-tail wrappers (Group D)
# Endpoints: season_weeks, season_week, season_week_games,
#            season_awards, season_powerindex, season_powerindex_leaders,
#            season_group_children, season_type_corrections, season_players
# All use the core_v2 host:
#   https://sports.core.api.espn.com/v2/sports/hockey/leagues/{league}/...
#
# Live verification notes (2026-06-08):
#   season_weeks            -> count=0 / empty items for NHL (NHL does not use weeks)
#   season_week             -> 404 for NHL (no weeks)
#   season_week_games       -> 404 for NHL (no weeks)
#   season_awards           -> collection of award $refs; may be empty off-season
#   season_powerindex       -> count=0 / empty for NHL (not published for hockey)
#   season_powerindex_leaders -> count=0 / empty for NHL
#   season_group_children   -> children collection under a conference group (OK)
#   season_type_corrections -> count=0 / empty for NHL
#   season_players          -> alias of season_athletes (same path); works OK


# ===========================================================================
# 1. Season Weeks (collection) — DONE_WITH_CONCERNS: NHL does not use weeks
# ===========================================================================

#' Internal: ESPN hockey season-type weeks collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_weeks <- function(league      = "nhl",
                                      season      = most_recent_nhl_season(),
                                      season_type = 2,
                                      ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type, "/weeks")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} seasons/{season}/types/{season_type}/weeks returned no items (NHL does not use weeks)."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "week")
      tbl$week        <- as.integer(tbl$week)
      tbl$season      <- as.integer(season)
      tbl$season_type <- as.integer(season_type)
      tbl$count       <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count  <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Weeks data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = "ESPN {league} season weeks for {season} type {season_type} not available (normal for NHL).",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season weeks",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_weeks
NULL
#' @title **Get ESPN NHL Season Weeks (core-v2)**
#' @rdname espn_nhl_season_weeks
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per week, or an empty
#'   tibble when the endpoint returns no items. **Note**: The NHL does not use
#'   weekly scheduling in the ESPN core-v2 API — this endpoint consistently
#'   returns `count=0` for NHL. An empty tibble is returned gracefully:
#'
#'    |col_name     |types     |description                                          |
#'    |:------------|:---------|:----------------------------------------------------|
#'    |ref          |character |`$ref` URL for the week object.                      |
#'    |week         |integer   |Week number parsed from the `$ref` URL.              |
#'    |season       |integer   |Season year (echoed from arg).                       |
#'    |season_type  |integer   |Season type code (echoed from arg).                  |
#'    |count        |integer   |Total weeks in the collection (0 for NHL).           |
#'    |page_count   |integer   |Total pages in the collection.                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not populate weeks for hockey)
#'   try(espn_nhl_season_weeks(season = 2026, season_type = 2))
#' }
espn_nhl_season_weeks <- function(season      = most_recent_nhl_season(),
                                   season_type = 2,
                                   ...) {
  .espn_hockey_season_weeks(league = "nhl", season = season,
                             season_type = season_type, ...)
}


# ===========================================================================
# 2. Season Week (singular) — DONE_WITH_CONCERNS: 404 for NHL
# ===========================================================================

#' Internal: ESPN hockey single season week object (league-generic, core-v2)
#' @noRd
.espn_hockey_season_week <- function(league      = "nhl",
                                     season      = most_recent_nhl_season(),
                                     season_type = 2,
                                     week,
                                     ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type, "/weeks/", week)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }

      row <- list(
        season          = as.integer(season),
        season_type     = as.integer(season_type),
        week            = as.integer(week),
        number          = as.integer(raw$number      %||% NA_integer_),
        start_date      = as.character(raw$startDate  %||% NA_character_),
        end_date        = as.character(raw$endDate    %||% NA_character_),
        text            = as.character(raw$text       %||% NA_character_),
        week_ref        = as.character(raw[["$ref"]]  %||% NA_character_),
        events_ref      = .ref_of(raw$events)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Week data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = "ESPN {league} season week {week} for {season} type {season_type} not available (normal for NHL).",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season week",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_week
NULL
#' @title **Get ESPN NHL Season Week (core-v2)**
#' @rdname espn_nhl_season_week
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param week Week number (integer). Derive valid week numbers from
#'   `espn_nhl_season_weeks()`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
#'   try(espn_nhl_season_week(season = 2026, season_type = 2, week = 1))
#' }
espn_nhl_season_week <- function(season      = most_recent_nhl_season(),
                                  season_type = 2,
                                  week,
                                  ...) {
  .espn_hockey_season_week(league = "nhl", season = season,
                            season_type = season_type, week = week, ...)
}


# ===========================================================================
# 3. Season Week Games (collection) — DONE_WITH_CONCERNS: 404 for NHL
# ===========================================================================

#' Internal: ESPN hockey season week games (events) collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_week_games <- function(league      = "nhl",
                                           season      = most_recent_nhl_season(),
                                           season_type = 2,
                                           week,
                                           limit       = 100,
                                           ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type,
                     "/weeks/", week, "/events")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} seasons/{season}/types/{season_type}/weeks/{week}/events returned no items."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "event_id")
      tbl$event_id    <- as.character(tbl$event_id)
      tbl$season      <- as.integer(season)
      tbl$season_type <- as.integer(season_type)
      tbl$week        <- as.integer(week)
      tbl$count       <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count  <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Week Games data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = "ESPN {league} season week {week} games for {season} type {season_type} not available (normal for NHL).",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season week games",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_week_games
NULL
#' @title **Get ESPN NHL Season Week Games (core-v2)**
#' @rdname espn_nhl_season_week_games
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param week Week number (integer). Derive valid week numbers from
#'   `espn_nhl_season_weeks()`.
#' @param limit Maximum number of events to return (default `100`).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
#'   try(espn_nhl_season_week_games(season = 2026, season_type = 2, week = 1))
#' }
espn_nhl_season_week_games <- function(season      = most_recent_nhl_season(),
                                        season_type = 2,
                                        week,
                                        limit       = 100,
                                        ...) {
  .espn_hockey_season_week_games(league = "nhl", season = season,
                                  season_type = season_type, week = week,
                                  limit = limit, ...)
}


# ===========================================================================
# 4. Season Awards (collection) — sparse off-season, may be empty
# ===========================================================================

#' Internal: ESPN hockey season awards collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_awards <- function(league = "nhl",
                                       season = most_recent_nhl_season(),
                                       ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/awards")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} seasons/{season}/awards returned no items."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "award_id")
      tbl$award_id   <- as.character(tbl$award_id)
      tbl$season     <- as.integer(season)
      tbl$count      <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Awards data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season awards for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season awards",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_awards
NULL
#' @title **Get ESPN NHL Season Awards (core-v2)**
#' @rdname espn_nhl_season_awards
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_awards(season = 2025))
#' }
espn_nhl_season_awards <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_awards(league = "nhl", season = season, ...)
}


# ===========================================================================
# 5. Season Power Index — DONE_WITH_CONCERNS: not published for NHL
# ===========================================================================

#' Internal: ESPN hockey season power index (league-generic, core-v2)
#' @noRd
.espn_hockey_season_powerindex <- function(league  = "nhl",
                                           season  = most_recent_nhl_season(),
                                           team_id = NULL,
                                           ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      seg  <- if (!is.null(team_id)) paste0("/", team_id) else ""
      path <- paste0("seasons/", season, "/powerindex", seg)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} seasons/{season}/powerindex returned no items (not published for NHL)."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "powerindex_id")
      tbl$powerindex_id <- as.character(tbl$powerindex_id)
      tbl$season        <- as.integer(season)
      if (!is.null(team_id)) tbl$team_id <- as.character(team_id)
      tbl$count         <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count    <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Power Index data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = "ESPN {league} season powerindex for {season} not available (not published for NHL).",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season powerindex",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_powerindex
NULL
#' @title **Get ESPN NHL Season Power Index (core-v2)**
#' @rdname espn_nhl_season_powerindex
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param team_id Optional ESPN team identifier (character or numeric). When
#'   provided, fetches power-index data for a single team. Defaults to `NULL`
#'   (collection for all teams).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per power-index entry, or
#'   an empty tibble when no data is available. **Note**: ESPN does not publish
#'   power-index data for NHL — this endpoint consistently returns empty
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not publish powerindex for hockey)
#'   try(espn_nhl_season_powerindex(season = 2026))
#' }
espn_nhl_season_powerindex <- function(season  = most_recent_nhl_season(),
                                        team_id = NULL,
                                        ...) {
  .espn_hockey_season_powerindex(league = "nhl", season = season,
                                  team_id = team_id, ...)
}


# ===========================================================================
# 6. Season Power Index Leaders — DONE_WITH_CONCERNS: not published for NHL
# ===========================================================================

#' Internal: ESPN hockey season power index leaders (league-generic, core-v2)
#' @noRd
.espn_hockey_season_powerindex_leaders <- function(league = "nhl",
                                                   season = most_recent_nhl_season(),
                                                   ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/powerindex/leaders")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} seasons/{season}/powerindex/leaders returned no items (not published for NHL)."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "leader_id")
      tbl$leader_id  <- as.character(tbl$leader_id)
      tbl$season     <- as.integer(season)
      tbl$count      <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Power Index Leaders data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = "ESPN {league} season powerindex leaders for {season} not available (not published for NHL).",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season powerindex leaders",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_powerindex_leaders
NULL
#' @title **Get ESPN NHL Season Power Index Leaders (core-v2)**
#' @rdname espn_nhl_season_powerindex_leaders
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per power-index leader, or
#'   an empty tibble when no data is available. **Note**: ESPN does not publish
#'   power-index data for NHL:
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not publish powerindex for hockey)
#'   try(espn_nhl_season_powerindex_leaders(season = 2026))
#' }
espn_nhl_season_powerindex_leaders <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_powerindex_leaders(league = "nhl", season = season, ...)
}


# ===========================================================================
# 7. Season Group Children (collection)
# ===========================================================================

#' Internal: ESPN hockey season-type group children collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_group_children <- function(league      = "nhl",
                                               season      = most_recent_nhl_season(),
                                               season_type = 2,
                                               group_id,
                                               ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type,
                     "/groups/", group_id, "/children")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} season group {group_id} has no children for {season} type {season_type}."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "child_group_id")
      tbl$child_group_id <- as.character(tbl$child_group_id)
      tbl$season         <- as.integer(season)
      tbl$season_type    <- as.integer(season_type)
      tbl$group_id       <- as.integer(group_id)
      tbl$count          <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count     <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Group Children data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season group children for {season} type {season_type} group {group_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season group children",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_group_children
NULL
#' @title **Get ESPN NHL Season Group Children (core-v2)**
#' @rdname espn_nhl_season_group_children
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Pass a
#'   conference-level `group_id` from `espn_nhl_season_groups()` to retrieve
#'   its child divisions.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
#'     espn_nhl_season_group_children(season = 2026, season_type = 2,
#'                                     group_id = grps$group_id[1])
#'   })
#' }
espn_nhl_season_group_children <- function(season      = most_recent_nhl_season(),
                                            season_type = 2,
                                            group_id,
                                            ...) {
  .espn_hockey_season_group_children(league = "nhl", season = season,
                                      season_type = season_type,
                                      group_id = group_id, ...)
}


# ===========================================================================
# 8. Season Type Corrections — DONE_WITH_CONCERNS: empty for NHL
# ===========================================================================

#' Internal: ESPN hockey season-type corrections (league-generic, core-v2)
#' @noRd
.espn_hockey_season_type_corrections <- function(league      = "nhl",
                                                 season      = most_recent_nhl_season(),
                                                 season_type = 2,
                                                 ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type, "/corrections")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} seasons/{season}/types/{season_type}/corrections returned no items."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "correction_id")
      tbl$correction_id <- as.character(tbl$correction_id)
      tbl$season        <- as.integer(season)
      tbl$season_type   <- as.integer(season_type)
      tbl$count         <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count    <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Type Corrections data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season type corrections for {season} type {season_type} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season type corrections",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_type_corrections
NULL
#' @title **Get ESPN NHL Season Type Corrections (core-v2)**
#' @rdname espn_nhl_season_type_corrections
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per correction entry, or
#'   an empty tibble when no corrections are recorded. **Note**: ESPN does not
#'   publish score corrections for NHL — this endpoint returns empty for all
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not publish corrections for hockey)
#'   try(espn_nhl_season_type_corrections(season = 2026, season_type = 2))
#' }
espn_nhl_season_type_corrections <- function(season      = most_recent_nhl_season(),
                                              season_type = 2,
                                              ...) {
  .espn_hockey_season_type_corrections(league = "nhl", season = season,
                                        season_type = season_type, ...)
}


# ===========================================================================
# 9. Season Players (alias of season_athletes, same path)
# ===========================================================================

#' Internal: ESPN hockey season players (athletes) collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_players <- function(league = "nhl",
                                        season = most_recent_nhl_season(),
                                        limit  = 100,
                                        page   = 1,
                                        ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      # Same path as season_athletes: seasons/{season}/athletes
      path <- paste0("seasons/", season, "/athletes")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit, page = page),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "player_id")
      tbl$player_id  <- as.character(tbl$player_id)
      tbl$season     <- as.integer(season)
      tbl$page       <- as.integer(page)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Players data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season players for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season players",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_players
NULL
#' @title **Get ESPN NHL Season Players (core-v2)**
#' @rdname espn_nhl_season_players
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Number of players per page (default `100`). The NHL season
#'   player list exceeds 11,000 entries across ~2,300 pages; use `page` to
#'   iterate.
#' @param page Page number (1-indexed, default `1`). Check the returned
#'   `page_count` column to determine how many pages are available.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per player reference for
#'   the requested page. This function hits the same endpoint as
#'   `espn_nhl_season_athletes()` (`seasons/{season}/athletes`) but uses the
#'   `player_id` column name for consistency with the `sdv-py`
#'   `espn_nhl_season_players` naming convention:
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_players(season = 2026, limit = 10, page = 1))
#' }
espn_nhl_season_players <- function(season = most_recent_nhl_season(),
                                     limit  = 100,
                                     page   = 1,
                                     ...) {
  .espn_hockey_season_players(league = "nhl", season = season,
                               limit = limit, page = page, ...)
}
