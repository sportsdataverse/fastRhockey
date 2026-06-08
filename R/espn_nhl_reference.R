# espn_nhl_reference.R
# ESPN NHL: core-v2 reference catalog (Tier 6)
# Endpoints: venues, venue, franchises, franchise, positions, position,
#            coaches, coach, coach_record, countries, providers
#
# All endpoints are league-scoped under:
#   https://sports.core.api.espn.com/v2/sports/hockey/leagues/nhl/...
#
# Live verification notes (2026-06-08):
#   venues         -> OK  (180 venues, league-scoped)
#   venue          -> OK  (singular: fullName, address, grass, indoor)
#   franchises     -> OK  (32 franchises, league-scoped)
#   franchise      -> OK  (singular: displayName, abbreviation, venue ref, team ref)
#   positions      -> OK  (28 positions, league-scoped)
#   position       -> OK  (singular: name, abbreviation, leaf, parent ref)
#   coaches        -> 404 at .../leagues/nhl/coaches (league-level collection 404)
#                    Singular .../coaches/{id} is OK (careerRecords/coachSeasons sparse)
#   coach          -> OK  (singular: firstName, lastName, dateOfBirth, birthPlace)
#   coach_record   -> 404 for NHL (both /coaches/{id}/record and season/type paths)
#                    Implemented gracefully (empty tibble + warning)
#   countries      -> OK  (league-scoped, count=0 for NHL — graceful empty)
#   providers      -> OK  (33 providers, league-scoped)


# ===========================================================================
# 1. Venues (collection)
# ===========================================================================

#' Internal: ESPN hockey venues collection (league-generic, core-v2)
#' @noRd
.espn_hockey_venues <- function(league = "nhl", limit = 200, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "venues",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No venue items returned for {league}.")
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "venue_id")
      tbl$venue_id   <- as.character(tbl$venue_id)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Venues data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} venues available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} venues",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_venues
NULL
#' @title **Get ESPN NHL Venues (core-v2)**
#' @rdname espn_nhl_venues
#' @author Saiem Gilani
#' @param limit Maximum number of venues to return (default `200`). The NHL
#'   venue catalog contains ~180 entries; `200` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per venue reference:
#'
#'    |col_name    |types     |description                                        |
#'    |:-----------|:---------|:--------------------------------------------------|
#'    |ref         |character |`$ref` URL for the venue object.                   |
#'    |venue_id    |character |ESPN venue id parsed from the `$ref` URL.          |
#'    |count       |integer   |Total venues in the collection.                    |
#'    |page_count  |integer   |Total pages in the collection.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_venues())
#' }
espn_nhl_venues <- function(limit = 200, ...) {
  .espn_hockey_venues(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 2. Venue (singular)
# ===========================================================================

#' Internal: ESPN hockey singular venue (league-generic, core-v2)
#' @noRd
.espn_hockey_venue <- function(league = "nhl", venue_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("venues/", venue_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      addr <- raw$address
      row <- list(
        venue_id    = as.character(venue_id),
        id          = as.character(raw$id       %||% NA_character_),
        guid        = as.character(raw$guid     %||% NA_character_),
        full_name   = as.character(raw$fullName %||% NA_character_),
        city        = as.character(if (is.list(addr)) addr$city    %||% NA_character_ else NA_character_),
        state       = as.character(if (is.list(addr)) addr$state   %||% NA_character_ else NA_character_),
        country     = as.character(if (is.list(addr)) addr$country %||% NA_character_ else NA_character_),
        grass       = as.logical(raw$grass   %||% NA),
        indoor      = as.logical(raw$indoor  %||% NA),
        capacity    = as.integer(raw$capacity %||% NA_integer_),
        venue_ref   = as.character(raw[["$ref"]] %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Venue data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} venue {venue_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} venue",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_venue
NULL
#' @title **Get ESPN NHL Venue (core-v2)**
#' @rdname espn_nhl_venue
#' @author Saiem Gilani
#' @param venue_id ESPN venue identifier (character or numeric). Use
#'   `espn_nhl_venues()` to discover valid venue ids.
#' @param ... Reserved for forward compatibility.
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
#'    |capacity   |integer   |Seating capacity (if returned; often `NA` for NHL).|
#'    |venue_ref  |character |`$ref` URL for this venue object.                  |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     vns <- espn_nhl_venues()
#'     espn_nhl_venue(venue_id = vns$venue_id[1])
#'   })
#' }
espn_nhl_venue <- function(venue_id, ...) {
  .espn_hockey_venue(league = "nhl", venue_id = venue_id, ...)
}


# ===========================================================================
# 3. Franchises (collection)
# ===========================================================================

#' Internal: ESPN hockey franchises collection (league-generic, core-v2)
#' @noRd
.espn_hockey_franchises <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "franchises",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No franchise items returned for {league}.")
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "franchise_id")
      tbl$franchise_id <- as.character(tbl$franchise_id)
      tbl$count        <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count   <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Franchises data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} franchises available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} franchises",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_franchises
NULL
#' @title **Get ESPN NHL Franchises (core-v2)**
#' @rdname espn_nhl_franchises
#' @author Saiem Gilani
#' @param limit Maximum number of franchises to return (default `100`). The
#'   NHL has 32 active franchises; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_franchises())
#' }
espn_nhl_franchises <- function(limit = 100, ...) {
  .espn_hockey_franchises(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 4. Franchise (singular)
# ===========================================================================

#' Internal: ESPN hockey singular franchise (league-generic, core-v2)
#' @noRd
.espn_hockey_franchise <- function(league = "nhl", franchise_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("franchises/", franchise_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else if (is.data.frame(x) && "$ref" %in% colnames(x)) as.character(x[["$ref"]][1])
        else NA_character_
      }

      row <- list(
        franchise_id       = as.character(franchise_id),
        id                 = as.character(raw$id                 %||% NA_character_),
        uid                = as.character(raw$uid                %||% NA_character_),
        slug               = as.character(raw$slug               %||% NA_character_),
        location           = as.character(raw$location           %||% NA_character_),
        name               = as.character(raw$name               %||% NA_character_),
        nickname           = as.character(raw$nickname           %||% NA_character_),
        abbreviation       = as.character(raw$abbreviation       %||% NA_character_),
        display_name       = as.character(raw$displayName        %||% NA_character_),
        short_display_name = as.character(raw$shortDisplayName   %||% NA_character_),
        color              = as.character(raw$color              %||% NA_character_),
        is_active          = as.logical(raw$isActive             %||% NA),
        venue_ref          = .ref_of(raw$venue),
        team_ref           = .ref_of(raw$team),
        franchise_ref      = as.character(raw[["$ref"]]          %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Franchise data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} franchise {franchise_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} franchise",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_franchise
NULL
#' @title **Get ESPN NHL Franchise (core-v2)**
#' @rdname espn_nhl_franchise
#' @author Saiem Gilani
#' @param franchise_id ESPN franchise identifier (character or numeric). Use
#'   `espn_nhl_franchises()` to discover valid franchise ids.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     fcs <- espn_nhl_franchises()
#'     espn_nhl_franchise(franchise_id = fcs$franchise_id[1])
#'   })
#' }
espn_nhl_franchise <- function(franchise_id, ...) {
  .espn_hockey_franchise(league = "nhl", franchise_id = franchise_id, ...)
}


# ===========================================================================
# 5. Positions (collection)
# ===========================================================================

#' Internal: ESPN hockey positions collection (league-generic, core-v2)
#' @noRd
.espn_hockey_positions <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "positions",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No position items returned for {league}.")
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "position_id")
      tbl$position_id <- as.character(tbl$position_id)
      tbl$count       <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count  <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Positions data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} positions available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} positions",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_positions
NULL
#' @title **Get ESPN NHL Positions (core-v2)**
#' @rdname espn_nhl_positions
#' @author Saiem Gilani
#' @param limit Maximum number of positions to return (default `100`). The
#'   NHL position catalog contains ~28 entries (positions + sub-positions).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_positions())
#' }
espn_nhl_positions <- function(limit = 100, ...) {
  .espn_hockey_positions(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 6. Position (singular)
# ===========================================================================

#' Internal: ESPN hockey singular position (league-generic, core-v2)
#' @noRd
.espn_hockey_position <- function(league = "nhl", position_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("positions/", position_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      parent_ref <- if (is.list(raw$parent) && !is.null(raw$parent[["$ref"]]))
        as.character(raw$parent[["$ref"]])
      else
        NA_character_

      row <- list(
        position_id   = as.character(position_id),
        id            = as.character(raw$id            %||% NA_character_),
        name          = as.character(raw$name          %||% NA_character_),
        display_name  = as.character(raw$displayName   %||% NA_character_),
        abbreviation  = as.character(raw$abbreviation  %||% NA_character_),
        leaf          = as.logical(raw$leaf            %||% NA),
        parent_ref    = parent_ref,
        position_ref  = as.character(raw[["$ref"]]     %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Position data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} position {position_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} position",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_position
NULL
#' @title **Get ESPN NHL Position (core-v2)**
#' @rdname espn_nhl_position
#' @author Saiem Gilani
#' @param position_id ESPN position identifier (character or numeric). Use
#'   `espn_nhl_positions()` to discover valid position ids.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     pos <- espn_nhl_positions()
#'     espn_nhl_position(position_id = pos$position_id[1])
#'   })
#' }
espn_nhl_position <- function(position_id, ...) {
  .espn_hockey_position(league = "nhl", position_id = position_id, ...)
}


# ===========================================================================
# 7. Coaches (league-level collection)
# ===========================================================================
# NOTE: .../leagues/nhl/coaches (league-level collection) returns HTTP 404.
# The season-scoped coaches collection at .../seasons/{s}/coaches is
# implemented as espn_nhl_season_coaches() in espn_nhl_season_collections.R.
# espn_nhl_coaches() wraps that season-scoped path as a convenience alias
# for the most recent season.

#' Internal: ESPN hockey league-level coaches collection (league-generic, core-v2)
#' @noRd
.espn_hockey_coaches <- function(league = "nhl",
                                  season = most_recent_nhl_season(),
                                  limit  = 100,
                                  page   = 1,
                                  ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      # The league-level /coaches collection (404 for NHL); fall back to
      # season-scoped coaches which is confirmed to work.
      path <- paste0("seasons/", season, "/coaches")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   query  = list(limit = limit, page = page),
                                   league = league,
                                   simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No coach items returned for {league} season {season}.")
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "coach_id")
      tbl$coach_id   <- as.character(tbl$coach_id)
      tbl$season     <- as.integer(season)
      tbl$page       <- as.integer(page)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Coaches data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} coaches for season {season} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} coaches",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_coaches
NULL
#' @title **Get ESPN NHL Coaches (core-v2)**
#' @rdname espn_nhl_coaches
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`. The league-level coaches collection
#'   (`/leagues/nhl/coaches`) returns HTTP 404 for NHL; this function uses the
#'   season-scoped path (`seasons/{season}/coaches`) which is confirmed to work.
#' @param limit Maximum number of coaches to return per page (default `100`).
#'   The NHL typically lists ~30 head coaches per season.
#' @param page Page number (1-indexed, default `1`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per coach reference:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the coach-in-season object.          |
#'    |coach_id    |character |ESPN coach id parsed from the `$ref` URL.           |
#'    |season      |integer   |Season year (echoed from arg).                      |
#'    |page        |integer   |Page number (echoed from arg).                      |
#'    |count       |integer   |Total coaches in the collection.                    |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_coaches(season = 2026))
#' }
espn_nhl_coaches <- function(season = most_recent_nhl_season(),
                               limit  = 100,
                               page   = 1,
                               ...) {
  .espn_hockey_coaches(league = "nhl", season = season,
                        limit = limit, page = page, ...)
}


# ===========================================================================
# 8. Coach (singular)
# ===========================================================================

#' Internal: ESPN hockey singular coach (league-generic, core-v2)
#' @noRd
.espn_hockey_coach <- function(league = "nhl", coach_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("coaches/", coach_id)
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      bp <- raw$birthPlace
      row <- list(
        coach_id         = as.character(coach_id),
        id               = as.character(raw$id          %||% NA_character_),
        uid              = as.character(raw$uid         %||% NA_character_),
        first_name       = as.character(raw$firstName   %||% NA_character_),
        last_name        = as.character(raw$lastName    %||% NA_character_),
        date_of_birth    = as.character(raw$dateOfBirth %||% NA_character_),
        birth_city       = as.character(if (is.list(bp)) bp$city    %||% NA_character_ else NA_character_),
        birth_country    = as.character(if (is.list(bp)) bp$country %||% NA_character_ else NA_character_),
        experience       = as.integer(raw$experience    %||% NA_integer_),
        coach_ref        = as.character(raw[["$ref"]]   %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Coach data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} coach {coach_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} coach",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_coach
NULL
#' @title **Get ESPN NHL Coach (core-v2)**
#' @rdname espn_nhl_coach
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_nhl_coaches()` to discover valid coach ids.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with coach detail. Note that
#'   `careerRecords` and `coachSeasons` are sparse (empty arrays) for many NHL
#'   coaches — see `espn_nhl_coach_record()` for season-scoped records:
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try({
#'     ccs <- espn_nhl_coaches(season = 2026)
#'     espn_nhl_coach(coach_id = ccs$coach_id[1])
#'   })
#' }
espn_nhl_coach <- function(coach_id, ...) {
  .espn_hockey_coach(league = "nhl", coach_id = coach_id, ...)
}


# ===========================================================================
# 9. Coach Record
# ===========================================================================
# NOTE: Both .../leagues/nhl/coaches/{id}/record and the season/type scoped
# path return HTTP 404 for NHL. This function returns gracefully with an
# empty tibble and a cli warning.

#' Internal: ESPN hockey coach record (league-generic, core-v2)
#' @noRd
.espn_hockey_coach_record <- function(league = "nhl", coach_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("coaches/", coach_id, "/record")
      raw  <- .espn_hockey_request("core_v2", path = path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      # If data is returned, flatten into a wide tibble
      if (is.null(raw) || length(raw) == 0) {
        cli::cli_alert_warning(
          "{Sys.time()}: No coach record returned for coach {coach_id} ({league}).")
        return(result)
      }

      row <- list(coach_id = as.character(coach_id))
      for (k in names(raw)) {
        v <- raw[[k]]
        if (length(v) == 1 && (is.character(v) || is.numeric(v) || is.logical(v))) {
          row[[k]] <- v
        }
      }

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Coach Record data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_warning(e,
        hint = paste0("ESPN {league} coach record for coach {coach_id} is not available ",
                      "(HTTP 404 is normal for NHL)."),
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} coach record",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_coach_record
NULL
#' @title **Get ESPN NHL Coach Record (core-v2)**
#' @rdname espn_nhl_coach_record
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_nhl_coaches()` to discover valid coach ids.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns an empty tibble for NHL (404 from ESPN)
#'   try({
#'     ccs <- espn_nhl_coaches(season = 2026)
#'     espn_nhl_coach_record(coach_id = ccs$coach_id[1])
#'   })
#' }
espn_nhl_coach_record <- function(coach_id, ...) {
  .espn_hockey_coach_record(league = "nhl", coach_id = coach_id, ...)
}


# ===========================================================================
# 10. Countries (collection)
# ===========================================================================
# NOTE: .../leagues/nhl/countries returns count=0 for NHL (ESPN does not
# populate this endpoint for the NHL). Returns gracefully with an empty tibble.

#' Internal: ESPN hockey countries collection (league-generic, core-v2)
#' @noRd
.espn_hockey_countries <- function(league = "nhl", limit = 200, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "countries",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0) ||
          (is.integer(raw$count) && raw$count == 0L) ||
          (is.numeric(raw$count) && raw$count == 0)) {
        cli::cli_alert_warning(
          "{Sys.time()}: No country items returned for {league} (count=0 is normal for NHL).")
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "country_id")
      tbl$country_id <- as.character(tbl$country_id)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Countries data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} countries available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} countries",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_countries
NULL
#' @title **Get ESPN NHL Countries (core-v2)**
#' @rdname espn_nhl_countries
#' @author Saiem Gilani
#' @param limit Maximum number of countries to return (default `200`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per country reference, or
#'   an empty tibble when the endpoint is unpopulated. **Note**: The NHL
#'   countries endpoint (`leagues/nhl/countries`) returns `count=0` — ESPN
#'   does not populate this collection for the NHL. An empty tibble is returned
#'   gracefully with a warning:
#'
#'    |col_name    |types     |description                                         |
#'    |:-----------|:---------|:---------------------------------------------------|
#'    |ref         |character |`$ref` URL for the country object.                  |
#'    |country_id  |character |ESPN country id parsed from the `$ref` URL.         |
#'    |count       |integer   |Total countries (typically `0` for NHL).            |
#'    |page_count  |integer   |Total pages in the collection.                      |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
#'   try(espn_nhl_countries())
#' }
espn_nhl_countries <- function(limit = 200, ...) {
  .espn_hockey_countries(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 11. Providers (collection)
# ===========================================================================

#' Internal: ESPN hockey providers collection (league-generic, core-v2)
#' @noRd
.espn_hockey_providers <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "providers",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning("{Sys.time()}: No provider items returned for {league}.")
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "provider_id")
      tbl$provider_id <- as.character(tbl$provider_id)
      tbl$count       <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count  <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Providers data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} providers available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} providers",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_providers
NULL
#' @title **Get ESPN NHL Providers (core-v2)**
#' @rdname espn_nhl_providers
#' @author Saiem Gilani
#' @param limit Maximum number of providers to return (default `100`). The
#'   NHL provider catalog contains ~33 entries; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per odds provider reference:
#'
#'    |col_name     |types     |description                                        |
#'    |:------------|:---------|:--------------------------------------------------|
#'    |ref          |character |`$ref` URL for the provider object.                |
#'    |provider_id  |character |ESPN provider id parsed from the `$ref` URL.       |
#'    |count        |integer   |Total providers in the collection.                 |
#'    |page_count   |integer   |Total pages in the collection.                     |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_providers())
#' }
espn_nhl_providers <- function(limit = 100, ...) {
  .espn_hockey_providers(league = "nhl", limit = limit, ...)
}
