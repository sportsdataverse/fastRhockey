# espn_nhl_season_collections.R
# ESPN NHL: core-v2 season collections (Tier 3b)
# Endpoints: season_teams, season_team, season_athletes, season_coaches,
#            season_draft, season_draft_round_picks, season_futures,
#            season_freeagents, season_type_leaders
# All use the core_v2 host:
#   https://sports.core.api.espn.com/v2/sports/hockey/leagues/{league}/...
#
# Live verification notes (2026-06-08):
#   season_teams       -> 32 teams, 7 pages  (collection OK)
#   season_team        -> singular team-in-season object (OK)
#   season_athletes    -> 11706 athletes, 2342 pages (paginated collection OK)
#   season_coaches     -> 30 coaches, 6 pages  (collection OK)
#   season_draft       -> HTTP 404 for 2025 and 2026 (DONE_WITH_CONCERNS)
#   season_draft_round_picks  -> HTTP 404 for 2025 rounds/1/picks;
#                          draft/rounds returns count=0 (DONE_WITH_CONCERNS)
#   season_futures     -> 12 items with inline fields (id, name, type, futures)
#   season_freeagents  -> count=0 / empty for NHL (DONE_WITH_CONCERNS)
#   season_type_leaders -> 9 categories, 25 leaders each (OK)
#
# Draft-picks path investigation:
#   Confirmed paths tried:
#     seasons/{s}/draft          -> 404 for both 2025 and 2026
#     seasons/{s}/draft/rounds   -> count=0, empty items
#     seasons/{s}/draft/rounds/1/picks -> 404
#   Conclusion: NHL draft data is NOT served by this core-v2 endpoint tree.
#   Both espn_nhl_season_draft() and espn_nhl_season_draft_round_picks()
#   return empty gracefully with a cli warning.


# ===========================================================================
# 1. Season Teams (collection)
# ===========================================================================

#' Internal: ESPN hockey season teams collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_teams <- function(league = "nhl",
                                      season = most_recent_nhl_season(),
                                      limit  = 100,
                                      ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/teams")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "team_id")
      tbl$team_id   <- as.character(tbl$team_id)
      tbl$season    <- as.integer(season)
      tbl$count     <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Teams data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season teams for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season teams",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_teams
NULL
#' @title **Get ESPN NHL Season Teams (core-v2)**
#' @rdname espn_nhl_season_teams
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of teams to return per page (default `100`).
#'   The NHL has 32 active teams; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_teams(season = 2026))
#' }
espn_nhl_season_teams <- function(season = most_recent_nhl_season(),
                                   limit  = 100,
                                   ...) {
  .espn_hockey_season_teams(league = "nhl", season = season,
                             limit = limit, ...)
}


# ===========================================================================
# 2. Season Team (singular)
# ===========================================================================

#' Internal: ESPN hockey single team-in-season object (league-generic, core-v2)
#' @noRd
.espn_hockey_season_team <- function(league  = "nhl",
                                     season  = most_recent_nhl_season(),
                                     team_id,
                                     ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/teams/", team_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }

      row <- list(
        season           = as.integer(season),
        team_id          = as.character(team_id),
        id               = as.character(raw$id              %||% NA_character_),
        guid             = as.character(raw$guid            %||% NA_character_),
        uid              = as.character(raw$uid             %||% NA_character_),
        slug             = as.character(raw$slug            %||% NA_character_),
        location         = as.character(raw$location        %||% NA_character_),
        name             = as.character(raw$name            %||% NA_character_),
        nickname         = as.character(raw$nickname        %||% NA_character_),
        abbreviation     = as.character(raw$abbreviation    %||% NA_character_),
        display_name     = as.character(raw$displayName     %||% NA_character_),
        short_display_name = as.character(raw$shortDisplayName %||% NA_character_),
        color            = as.character(raw$color           %||% NA_character_),
        alternate_color  = as.character(raw$alternateColor  %||% NA_character_),
        is_active        = as.logical(raw$isActive          %||% NA),
        is_all_star      = as.logical(raw$isAllStar         %||% NA),
        team_ref         = as.character(raw[["$ref"]]       %||% NA_character_),
        record_ref       = .ref_of(raw$record),
        athletes_ref     = .ref_of(raw$athletes),
        venue_ref        = .ref_of(raw$venue),
        groups_ref       = .ref_of(raw$groups),
        statistics_ref   = .ref_of(raw$statistics),
        leaders_ref      = .ref_of(raw$leaders),
        injuries_ref     = .ref_of(raw$injuries),
        awards_ref       = .ref_of(raw$awards),
        franchise_ref    = .ref_of(raw$franchise),
        events_ref       = .ref_of(raw$events),
        transactions_ref = .ref_of(raw$transactions),
        coaches_ref      = .ref_of(raw$coaches)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Team data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season team {team_id} for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season team",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_team
NULL
#' @title **Get ESPN NHL Season Team (core-v2)**
#' @rdname espn_nhl_season_team
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param team_id ESPN team identifier (character or numeric). Use
#'   `espn_nhl_season_teams()` to discover valid team ids for a season.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     tms <- espn_nhl_season_teams(season = 2026)
#'     espn_nhl_season_team(season = 2026, team_id = tms$team_id[1])
#'   })
#' }
espn_nhl_season_team <- function(season  = most_recent_nhl_season(),
                                  team_id,
                                  ...) {
  .espn_hockey_season_team(league = "nhl", season = season,
                            team_id = team_id, ...)
}


# ===========================================================================
# 3. Season Athletes (paginated collection)
# ===========================================================================

#' Internal: ESPN hockey season athletes collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_athletes <- function(league = "nhl",
                                         season = most_recent_nhl_season(),
                                         limit  = 100,
                                         page   = 1,
                                         ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
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

      tbl <- .espn_core_collection(items, id_col = "athlete_id")
      tbl$athlete_id  <- as.character(tbl$athlete_id)
      tbl$season      <- as.integer(season)
      tbl$page        <- as.integer(page)
      tbl$count       <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count  <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Athletes data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season athletes for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season athletes",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_athletes
NULL
#' @title **Get ESPN NHL Season Athletes (core-v2)**
#' @rdname espn_nhl_season_athletes
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Number of athletes per page (default `100`). The NHL season
#'   athlete list exceeds 11,000 entries across ~2,300 pages; use `page` to
#'   iterate.
#' @param page Page number (1-indexed, default `1`). Check the returned
#'   `page_count` column to determine how many pages are available.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per athlete reference for
#'   the requested page:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the athlete-in-season object.        |
#'    |athlete_id  |character |ESPN athlete id parsed from the `$ref` URL.         |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |page        |integer   |Page number (echoed from arg).                      |
#'    |count       |integer   |Total athletes in the collection.                   |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_athletes(season = 2026, limit = 10, page = 1))
#' }
espn_nhl_season_athletes <- function(season = most_recent_nhl_season(),
                                      limit  = 100,
                                      page   = 1,
                                      ...) {
  .espn_hockey_season_athletes(league = "nhl", season = season,
                                limit = limit, page = page, ...)
}


# ===========================================================================
# 4. Season Coaches (collection)
# ===========================================================================

#' Internal: ESPN hockey season coaches collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_coaches <- function(league = "nhl",
                                        season = most_recent_nhl_season(),
                                        limit  = 100,
                                        ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/coaches")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "coach_id")
      tbl$coach_id   <- as.character(tbl$coach_id)
      tbl$season     <- as.integer(season)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Coaches data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season coaches for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season coaches",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_coaches
NULL
#' @title **Get ESPN NHL Season Coaches (core-v2)**
#' @rdname espn_nhl_season_coaches
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of coaches to return (default `100`). The NHL
#'   typically lists ~30 head coaches per season.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_coaches(season = 2026))
#' }
espn_nhl_season_coaches <- function(season = most_recent_nhl_season(),
                                     limit  = 100,
                                     ...) {
  .espn_hockey_season_coaches(league = "nhl", season = season,
                               limit = limit, ...)
}


# ===========================================================================
# 5. Season Draft  (DONE_WITH_CONCERNS — 404 for NHL)
# ===========================================================================

#' Internal: ESPN hockey season draft (league-generic, core-v2)
#' @noRd
.espn_hockey_season_draft <- function(league = "nhl",
                                      season = most_recent_nhl_season(),
                                      ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/draft")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      # Collection shape: {count, items:[{$ref}]}
      if (!is.null(raw$items)) {
        items <- raw$items
        if (is.data.frame(items) && nrow(items) > 0) {
          tbl <- .espn_core_collection(items, id_col = "draft_id")
          tbl$season <- as.integer(season)
          result <- tbl %>%
            janitor::clean_names() %>%
            make_fastRhockey_data(
              paste0(toupper(league), " Season Draft data from ESPN core-v2"),
              Sys.time()
            )
          return(result)
        }
      }

      # Singular shape: {year, rounds:[...]}
      if (!is.null(raw$year)) {
        rounds <- raw$rounds %||% list()
        if (is.data.frame(rounds) && nrow(rounds) > 0) {
          rounds$season <- as.integer(season)
          result <- rounds %>%
            janitor::clean_names() %>%
            make_fastRhockey_data(
              paste0(toupper(league), " Season Draft data from ESPN core-v2"),
              Sys.time()
            )
        } else {
          result <- data.frame(season = as.integer(season),
                               year   = as.integer(raw$year),
                               stringsAsFactors = FALSE) %>%
            make_fastRhockey_data(
              paste0(toupper(league), " Season Draft data from ESPN core-v2"),
              Sys.time()
            )
        }
      }
    },
    error = function(e) {
      .report_api_warning(e,
        hint = "ESPN {league} season draft for {season} is not available (HTTP 404 is normal for NHL).",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season draft",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_draft
NULL
#' @title **Get ESPN NHL Season Draft (core-v2)**
#' @rdname espn_nhl_season_draft
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try(espn_nhl_season_draft(season = 2025))
#' }
espn_nhl_season_draft <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_draft(league = "nhl", season = season, ...)
}


# ===========================================================================
# 6. Season Draft Round Picks  (DONE_WITH_CONCERNS — 404 for NHL)
# ===========================================================================

#' Internal: ESPN hockey season draft round picks (league-generic, core-v2)
#' @noRd
.espn_hockey_season_draft_round_picks <- function(league     = "nhl",
                                                   season     = most_recent_nhl_season(),
                                                   round_num  = 1,
                                                   limit      = 100,
                                                   ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      # Confirmed path: seasons/{season}/draft/rounds/{round_num}/picks
      # For NHL this returns HTTP 404; draft/rounds returns count=0.
      path <- paste0("seasons/", season, "/draft/rounds/", round_num, "/picks")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "pick_id")
      tbl$pick_id    <- as.character(tbl$pick_id)
      tbl$season     <- as.integer(season)
      tbl$round_num  <- as.integer(round_num)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Draft Round Picks data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = paste0("ESPN {league} season draft round picks for {season} round {round_num} ",
                      "is not available (HTTP 404 is normal for NHL)."),
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season draft round picks",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_draft_round_picks
NULL
#' @title **Get ESPN NHL Season Draft Round Picks (core-v2)**
#' @rdname espn_nhl_season_draft_round_picks
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param round_num Draft round number (default `1`). Used to build the path
#'   `seasons/{season}/draft/rounds/{round_num}/picks`.
#' @param limit Maximum picks to return per page (default `100`).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try(espn_nhl_season_draft_round_picks(season = 2025, round_num = 1))
#' }
espn_nhl_season_draft_round_picks <- function(season    = most_recent_nhl_season(),
                                               round_num = 1,
                                               limit     = 100,
                                               ...) {
  .espn_hockey_season_draft_round_picks(league = "nhl", season = season,
                                         round_num = round_num, limit = limit, ...)
}


# ===========================================================================
# 7. Season Futures (collection with inline fields)
# ===========================================================================

#' Internal: ESPN hockey season futures collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_futures <- function(league = "nhl",
                                        season = most_recent_nhl_season(),
                                        ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/futures")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      # Futures items have inline fields: id, name, type, displayName, futures (nested)
      # Expand to one row per futures market (item), one row per provider
      rows <- lapply(seq_len(nrow(items)), function(i) {
        it   <- items[i, , drop = FALSE]
        mkt_ref  <- as.character(it[["$ref"]] %||% NA_character_)
        mkt_id   <- as.character(it$id        %||% NA_character_)
        mkt_name <- as.character(it$name      %||% NA_character_)
        mkt_type <- as.character(it$type      %||% NA_character_)
        mkt_dn   <- as.character(it$displayName %||% NA_character_)

        providers_df <- if (is.list(it$futures) && length(it$futures) > 0)
          it$futures[[1]] else data.frame()

        if (is.data.frame(providers_df) && nrow(providers_df) > 0 &&
            "provider" %in% colnames(providers_df)) {
          lapply(seq_len(nrow(providers_df)), function(j) {
            prov <- providers_df$provider[j, , drop = FALSE]
            list(
              future_ref           = mkt_ref,
              future_id            = mkt_id,
              future_name          = mkt_name,
              future_type          = mkt_type,
              future_display_name  = mkt_dn,
              provider_id          = as.character(prov$id       %||% NA_character_),
              provider_name        = as.character(prov$name     %||% NA_character_),
              provider_active      = as.logical( prov$active    %||% NA),
              provider_priority    = as.integer( prov$priority  %||% NA_integer_),
              season               = as.integer(season)
            )
          })
        } else {
          list(list(
            future_ref           = mkt_ref,
            future_id            = mkt_id,
            future_name          = mkt_name,
            future_type          = mkt_type,
            future_display_name  = mkt_dn,
            provider_id          = NA_character_,
            provider_name        = NA_character_,
            provider_active      = NA,
            provider_priority    = NA_integer_,
            season               = as.integer(season)
          ))
        }
      })
      rows_flat <- unlist(rows, recursive = FALSE)

      result <- dplyr::bind_rows(lapply(rows_flat, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Futures data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season futures for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season futures",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_futures
NULL
#' @title **Get ESPN NHL Season Futures (core-v2)**
#' @rdname espn_nhl_season_futures
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per futures market per
#'   betting provider. For a typical NHL season there are ~12 markets across
#'   1-2 providers:
#'
#'    |col_name             |types     |description                                          |
#'    |:--------------------|:---------|:----------------------------------------------------|
#'    |future_ref           |character |`$ref` URL for the futures market object.            |
#'    |future_id            |character |ESPN futures market identifier.                      |
#'    |future_name          |character |Futures market name (e.g. "NHL - Stanley Cup - Winner"). |
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_futures(season = 2026))
#' }
espn_nhl_season_futures <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_futures(league = "nhl", season = season, ...)
}


# ===========================================================================
# 8. Season Free Agents  (DONE_WITH_CONCERNS — empty for NHL)
# ===========================================================================

#' Internal: ESPN hockey season free agents collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_freeagents <- function(league = "nhl",
                                            season = most_recent_nhl_season(),
                                            limit  = 100,
                                            ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/freeagents")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        # NHL returns count=0 for free agents; return gracefully
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "athlete_id")
      tbl$athlete_id <- as.character(tbl$athlete_id)
      tbl$season     <- as.integer(season)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Free Agents data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season free agents for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season free agents",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_freeagents
NULL
#' @title **Get ESPN NHL Season Free Agents (core-v2)**
#' @rdname espn_nhl_season_freeagents
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param limit Maximum number of free agents to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per free-agent athlete
#'   reference. **Note**: The NHL free-agents endpoint (`seasons/{season}/freeagents`)
#'   currently returns `count=0` for all tested seasons — the endpoint exists
#'   but ESPN does not populate it for NHL. An empty tibble is returned
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
#'   try(espn_nhl_season_freeagents(season = 2026))
#' }
espn_nhl_season_freeagents <- function(season = most_recent_nhl_season(),
                                        limit  = 100,
                                        ...) {
  .espn_hockey_season_freeagents(league = "nhl", season = season,
                                  limit = limit, ...)
}


# ===========================================================================
# 9. Season Type Leaders (categories x leaders)
# ===========================================================================

#' Internal: ESPN hockey season-type leaders (league-generic, core-v2)
#' @noRd
.espn_hockey_season_type_leaders <- function(league      = "nhl",
                                              season      = most_recent_nhl_season(),
                                              season_type = 2,
                                              ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type, "/leaders")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      cats <- raw$categories
      if (is.null(cats) || (is.data.frame(cats) && nrow(cats) == 0)) {
        return(result)
      }

      rows <- lapply(seq_len(nrow(cats)), function(i) {
        cat_row  <- cats[i, , drop = FALSE]
        cat_name <- as.character(cat_row$name         %||% NA_character_)
        cat_dn   <- as.character(cat_row$displayName  %||% NA_character_)
        cat_sdn  <- as.character(cat_row$shortDisplayName %||% NA_character_)
        cat_abbr <- as.character(cat_row$abbreviation %||% NA_character_)

        leaders_df <- if (is.list(cat_row$leaders) && length(cat_row$leaders) > 0)
          cat_row$leaders[[1]] else data.frame()

        if (!is.data.frame(leaders_df) || nrow(leaders_df) == 0) {
          return(list(data.frame(
            category_name         = cat_name,
            category_display_name = cat_dn,
            category_short_display_name = cat_sdn,
            category_abbreviation = cat_abbr,
            display_value         = NA_character_,
            value                 = NA_real_,
            rel                   = NA_character_,
            athlete_ref           = NA_character_,
            team_ref              = NA_character_,
            statistics_ref        = NA_character_,
            season                = as.integer(season),
            season_type           = as.integer(season_type),
            stringsAsFactors = FALSE
          )))
        }

        # athlete, team, statistics are single-column data.frames with "$ref"
        ath_refs  <- if (is.data.frame(leaders_df$athlete) &&
                         "$ref" %in% colnames(leaders_df$athlete))
          as.character(leaders_df$athlete[["$ref"]]) else rep(NA_character_, nrow(leaders_df))
        team_refs <- if (is.data.frame(leaders_df$team) &&
                         "$ref" %in% colnames(leaders_df$team))
          as.character(leaders_df$team[["$ref"]]) else rep(NA_character_, nrow(leaders_df))
        stat_refs <- if (is.data.frame(leaders_df$statistics) &&
                         "$ref" %in% colnames(leaders_df$statistics))
          as.character(leaders_df$statistics[["$ref"]]) else rep(NA_character_, nrow(leaders_df))

        list(data.frame(
          category_name               = cat_name,
          category_display_name       = cat_dn,
          category_short_display_name = cat_sdn,
          category_abbreviation       = cat_abbr,
          display_value               = as.character(leaders_df$displayValue %||% NA_character_),
          value                       = as.numeric(leaders_df$value          %||% NA_real_),
          rel                         = as.character(leaders_df$rel          %||% NA_character_),
          athlete_ref                 = ath_refs,
          team_ref                    = team_refs,
          statistics_ref              = stat_refs,
          season                      = as.integer(season),
          season_type                 = as.integer(season_type),
          stringsAsFactors            = FALSE
        ))
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) r[[1]])) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Type Leaders data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season type leaders for {season} type {season_type} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season type leaders",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_type_leaders
NULL
#' @title **Get ESPN NHL Season Type Leaders (core-v2)**
#' @rdname espn_nhl_season_type_leaders
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination. For a typical NHL regular season there are 9 categories
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_type_leaders(season = 2026, season_type = 2))
#' }
espn_nhl_season_type_leaders <- function(season      = most_recent_nhl_season(),
                                          season_type = 2,
                                          ...) {
  .espn_hockey_season_type_leaders(league = "nhl", season = season,
                                    season_type = season_type, ...)
}
