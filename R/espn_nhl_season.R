# espn_nhl_season.R
# ESPN NHL: core-v2 season-structure wrappers
# Endpoints: league_root, season_pointer, seasons, season_info,
#            season_types, season_type, season_groups, season_group,
#            season_group_teams
# All use the core_v2 host: https://sports.core.api.espn.com/v2/sports/hockey/leagues/{league}/...

# ===========================================================================
# 1. League Root
# ===========================================================================

#' Internal: ESPN hockey league root (league-generic, core-v2)
#' @noRd
.espn_hockey_league_root <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "", league = league,
                                  simplifyVector = TRUE, ...)

      # Singular-object shape: flatten scalar fields; keep $ref links as *_ref cols
      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }

      row <- list(
        id              = as.character(raw$id          %||% NA_character_),
        guid            = as.character(raw$guid        %||% NA_character_),
        uid             = as.character(raw$uid         %||% NA_character_),
        name            = as.character(raw$name        %||% NA_character_),
        display_name    = as.character(raw$displayName %||% NA_character_),
        abbreviation    = as.character(raw$abbreviation %||% NA_character_),
        short_name      = as.character(raw$shortName   %||% NA_character_),
        slug            = as.character(raw$slug        %||% NA_character_),
        is_tournament   = as.logical( raw$isTournament %||% NA),
        gender          = as.character(raw$gender      %||% NA_character_),
        season_ref      = .ref_of(raw$season),
        seasons_ref     = .ref_of(raw$seasons),
        groups_ref      = .ref_of(raw$groups),
        franchises_ref  = .ref_of(raw$franchises),
        teams_ref       = .ref_of(raw$teams),
        events_ref      = .ref_of(raw$events)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " League Root data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} league root data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} league root",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_league_root
NULL
#' @title **Get ESPN NHL League Root (core-v2)**
#' @rdname espn_nhl_league_root
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with league metadata:
#'
#'    |col_name        |types     |description                                      |
#'    |:---------------|:---------|:------------------------------------------------|
#'    |id              |character |ESPN league identifier (e.g. "90" for NHL).      |
#'    |guid            |character |League global unique identifier.                 |
#'    |uid             |character |League uid string.                               |
#'    |name            |character |Full league name.                                |
#'    |display_name    |character |League display name.                             |
#'    |abbreviation    |character |League abbreviation (e.g. "NHL").                |
#'    |short_name      |character |League short name.                               |
#'    |slug            |character |League slug (e.g. "nhl").                        |
#'    |is_tournament   |logical   |Whether the league is a tournament.              |
#'    |gender          |character |League gender designation.                       |
#'    |season_ref      |character |`$ref` URL for the current season object.        |
#'    |seasons_ref     |character |`$ref` URL for the seasons collection.           |
#'    |groups_ref      |character |`$ref` URL for the groups collection.            |
#'    |franchises_ref  |character |`$ref` URL for the franchises collection.        |
#'    |teams_ref       |character |`$ref` URL for the teams collection.             |
#'    |events_ref      |character |`$ref` URL for the events collection.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_league_root())
#' }
espn_nhl_league_root <- function(...) {
  .espn_hockey_league_root(league = "nhl", ...)
}


# ===========================================================================
# 2. Season Pointer (current season)
# ===========================================================================

#' Internal: ESPN hockey current-season pointer (league-generic, core-v2)
#' @noRd
.espn_hockey_season_pointer <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      # path "season" returns the current season object directly
      raw <- .espn_hockey_request("core_v2", path = "season", league = league,
                                  simplifyVector = TRUE, ...)

      # Singular object: year, startDate, endDate, displayName, current type info
      type_obj <- raw$type %||% list()

      row <- list(
        season           = as.integer(raw$year        %||% NA_integer_),
        start_date       = as.character(raw$startDate  %||% NA_character_),
        end_date         = as.character(raw$endDate    %||% NA_character_),
        display_name     = as.character(raw$displayName %||% NA_character_),
        current_type_id  = as.character(type_obj$id   %||% NA_character_),
        current_type     = as.integer( type_obj$type  %||% NA_integer_),
        current_type_name = as.character(type_obj$name %||% NA_character_),
        current_type_slug = as.character(type_obj$slug %||% NA_character_),
        season_ref       = as.character(raw[["$ref"]]  %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Current Season Pointer data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season pointer data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season pointer",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_pointer
NULL
#' @title **Get ESPN NHL Current Season Pointer (core-v2)**
#' @rdname espn_nhl_season_pointer
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_pointer())
#' }
espn_nhl_season_pointer <- function(...) {
  .espn_hockey_season_pointer(league = "nhl", ...)
}


# ===========================================================================
# 3. Seasons List
# ===========================================================================

#' Internal: ESPN hockey seasons collection (league-generic, core-v2)
#' @noRd
.espn_hockey_seasons <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "seasons",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "season")
      tbl$season    <- as.integer(tbl$season)
      tbl$count     <- as.integer(raw$count     %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Seasons List data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} seasons data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} seasons",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_seasons
NULL
#' @title **Get ESPN NHL Seasons List (core-v2)**
#' @rdname espn_nhl_seasons
#' @author Saiem Gilani
#' @param limit Maximum number of seasons to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per season:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the season object.                   |
#'    |season      |integer   |Season year parsed from the `$ref` URL.             |
#'    |count       |integer   |Total number of seasons in the collection.          |
#'    |page_count  |integer   |Total number of pages in the collection.            |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_seasons(limit = 10))
#' }
espn_nhl_seasons <- function(limit = 100, ...) {
  .espn_hockey_seasons(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 4. Season Info
# ===========================================================================

#' Internal: ESPN hockey single-season info (league-generic, core-v2)
#' @noRd
.espn_hockey_season_info <- function(league = "nhl",
                                     season = most_recent_nhl_season(),
                                     ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season)
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }

      row <- list(
        season          = as.integer(raw$year         %||% as.integer(season)),
        start_date      = as.character(raw$startDate   %||% NA_character_),
        end_date        = as.character(raw$endDate     %||% NA_character_),
        display_name    = as.character(raw$displayName %||% NA_character_),
        season_ref      = as.character(raw[["$ref"]]   %||% NA_character_),
        types_ref       = .ref_of(raw$types),
        type_ref        = .ref_of(raw$type),
        rankings_ref    = .ref_of(raw$rankings),
        coaches_ref     = .ref_of(raw$coaches),
        athletes_ref    = .ref_of(raw$athletes),
        awards_ref      = .ref_of(raw$awards),
        futures_ref     = .ref_of(raw$futures),
        leaders_ref     = .ref_of(raw$leaders)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Info data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season info for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season info",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_info
NULL
#' @title **Get ESPN NHL Season Info (core-v2)**
#' @rdname espn_nhl_season_info
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with season metadata:
#'
#'    |col_name       |types     |description                                      |
#'    |:--------------|:---------|:------------------------------------------------|
#'    |season         |integer   |Season year (echoed from arg).                   |
#'    |start_date     |character |Season start date (ISO 8601).                    |
#'    |end_date       |character |Season end date (ISO 8601).                      |
#'    |display_name   |character |Season display name (e.g. "2025-26").            |
#'    |season_ref     |character |`$ref` URL for this season.                      |
#'    |types_ref      |character |`$ref` URL for the season-types collection.      |
#'    |type_ref       |character |`$ref` URL for the current season type.          |
#'    |rankings_ref   |character |`$ref` URL for rankings.                         |
#'    |coaches_ref    |character |`$ref` URL for coaches.                          |
#'    |athletes_ref   |character |`$ref` URL for athletes.                         |
#'    |awards_ref     |character |`$ref` URL for awards.                           |
#'    |futures_ref    |character |`$ref` URL for futures.                          |
#'    |leaders_ref    |character |`$ref` URL for leaders.                          |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_info(season = 2026))
#' }
espn_nhl_season_info <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_info(league = "nhl", season = season, ...)
}


# ===========================================================================
# 5. Season Types (collection)
# ===========================================================================

#' Internal: ESPN hockey season types collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_types <- function(league = "nhl",
                                      season = most_recent_nhl_season(),
                                      ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "season_type_id")
      tbl$season          <- as.integer(season)
      tbl$season_type_id  <- as.integer(tbl$season_type_id)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Types data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season types for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season types",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_types
NULL
#' @title **Get ESPN NHL Season Types (core-v2)**
#' @rdname espn_nhl_season_types
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per season type
#'   (pre-season, regular, post-season, off-season):
#'
#'    |col_name        |types     |description                                          |
#'    |:---------------|:---------|:----------------------------------------------------|
#'    |ref             |character |`$ref` URL for the season-type object.               |
#'    |season_type_id  |integer   |Season type id (1=pre, 2=reg, 3=post, 4=off).        |
#'    |season          |integer   |Season year (echoed from arg).                       |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_types(season = 2026))
#' }
espn_nhl_season_types <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_season_types(league = "nhl", season = season, ...)
}


# ===========================================================================
# 6. Season Type (singular)
# ===========================================================================

#' Internal: ESPN hockey single season-type object (league-generic, core-v2)
#' @noRd
.espn_hockey_season_type <- function(league = "nhl",
                                     season = most_recent_nhl_season(),
                                     season_type = 2,
                                     ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type)
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }

      row <- list(
        season          = as.integer(season),
        season_type     = as.integer(season_type),
        id              = as.character(raw$id           %||% NA_character_),
        type            = as.integer( raw$type          %||% NA_integer_),
        name            = as.character(raw$name         %||% NA_character_),
        abbreviation    = as.character(raw$abbreviation %||% NA_character_),
        slug            = as.character(raw$slug         %||% NA_character_),
        year            = as.integer( raw$year          %||% NA_integer_),
        start_date      = as.character(raw$startDate    %||% NA_character_),
        end_date        = as.character(raw$endDate      %||% NA_character_),
        has_groups      = as.logical( raw$hasGroups     %||% NA),
        has_standings   = as.logical( raw$hasStandings  %||% NA),
        has_legs        = as.logical( raw$hasLegs       %||% NA),
        groups_ref      = .ref_of(raw$groups),
        weeks_ref       = .ref_of(raw$weeks),
        corrections_ref = .ref_of(raw$corrections),
        leaders_ref     = .ref_of(raw$leaders),
        type_ref        = as.character(raw[["$ref"]]    %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Type data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season type {season_type} for {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season type",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_type
NULL
#' @title **Get ESPN NHL Season Type (core-v2)**
#' @rdname espn_nhl_season_type
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_type(season = 2026, season_type = 2))
#' }
espn_nhl_season_type <- function(season = most_recent_nhl_season(),
                                  season_type = 2,
                                  ...) {
  .espn_hockey_season_type(league = "nhl", season = season,
                            season_type = season_type, ...)
}


# ===========================================================================
# 7. Season Groups (collection)
# ===========================================================================

#' Internal: ESPN hockey season-type groups collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_groups <- function(league = "nhl",
                                       season = most_recent_nhl_season(),
                                       season_type = 2,
                                       ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type, "/groups")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "group_id")
      tbl$group_id    <- as.integer(tbl$group_id)
      tbl$season      <- as.integer(season)
      tbl$season_type <- as.integer(season_type)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Groups data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season groups for {season} type {season_type} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season groups",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_groups
NULL
#' @title **Get ESPN NHL Season Groups (core-v2)**
#' @rdname espn_nhl_season_groups
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_groups(season = 2026, season_type = 2))
#' }
espn_nhl_season_groups <- function(season = most_recent_nhl_season(),
                                    season_type = 2,
                                    ...) {
  .espn_hockey_season_groups(league = "nhl", season = season,
                              season_type = season_type, ...)
}


# ===========================================================================
# 8. Season Group (singular)
# ===========================================================================

#' Internal: ESPN hockey single season-type group (league-generic, core-v2)
#' @noRd
.espn_hockey_season_group <- function(league = "nhl",
                                      season = most_recent_nhl_season(),
                                      season_type = 2,
                                      group_id,
                                      ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type,
                     "/groups/", group_id)
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }

      row <- list(
        season          = as.integer(season),
        season_type     = as.integer(season_type),
        group_id        = as.integer(group_id),
        id              = as.character(raw$id           %||% NA_character_),
        uid             = as.character(raw$uid          %||% NA_character_),
        name            = as.character(raw$name         %||% NA_character_),
        abbreviation    = as.character(raw$abbreviation %||% NA_character_),
        slug            = as.character(raw$slug         %||% NA_character_),
        is_conference   = as.logical( raw$isConference  %||% NA),
        group_ref       = as.character(raw[["$ref"]]    %||% NA_character_),
        season_ref      = .ref_of(raw$season),
        parent_ref      = .ref_of(raw$parent),
        children_ref    = .ref_of(raw$children),
        standings_ref   = .ref_of(raw$standings),
        teams_ref       = .ref_of(raw$teams)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Group data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season group {group_id} for {season} type {season_type} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season group",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_group
NULL
#' @title **Get ESPN NHL Season Group (core-v2)**
#' @rdname espn_nhl_season_group
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Use
#'   `espn_nhl_season_groups()` to discover valid ids for a season/type.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_season_group(season = 2026, season_type = 2, group_id = 7))
#' }
espn_nhl_season_group <- function(season = most_recent_nhl_season(),
                                   season_type = 2,
                                   group_id,
                                   ...) {
  .espn_hockey_season_group(league = "nhl", season = season,
                             season_type = season_type, group_id = group_id, ...)
}


# ===========================================================================
# 9. Season Group Teams (collection)
# ===========================================================================

#' Internal: ESPN hockey season-type-group teams collection (league-generic, core-v2)
#' @noRd
.espn_hockey_season_group_teams <- function(league = "nhl",
                                            season = most_recent_nhl_season(),
                                            season_type = 2,
                                            group_id,
                                            limit = 100,
                                            ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/types/", season_type,
                     "/groups/", group_id, "/teams")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "team_id")
      tbl$team_id     <- as.character(tbl$team_id)
      tbl$season      <- as.integer(season)
      tbl$season_type <- as.integer(season_type)
      tbl$group_id    <- as.integer(group_id)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Season Group Teams data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} season group teams for {season} type {season_type} group {group_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} season group teams",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_season_group_teams
NULL
#' @title **Get ESPN NHL Season Group Teams (core-v2)**
#' @rdname espn_nhl_season_group_teams
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season,
#'   `3`=post-season, `4`=off-season. Defaults to `2`.
#' @param group_id ESPN group identifier (integer or character). Use
#'   `espn_nhl_season_groups()` to discover valid ids for a season/type.
#' @param limit Maximum number of teams to return (default `100`).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
#'     espn_nhl_season_group_teams(season = 2026, season_type = 2,
#'                                  group_id = grps$group_id[1])
#'   })
#' }
espn_nhl_season_group_teams <- function(season = most_recent_nhl_season(),
                                         season_type = 2,
                                         group_id,
                                         limit = 100,
                                         ...) {
  .espn_hockey_season_group_teams(league = "nhl", season = season,
                                   season_type = season_type,
                                   group_id = group_id, limit = limit, ...)
}
