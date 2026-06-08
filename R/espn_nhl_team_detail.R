# espn_nhl_team_detail.R
# ESPN NHL: team sub-resource wrappers (Group B)
# Endpoints: team, team_record, team_depthcharts, team_injuries,
#            team_transactions, team_history, team_news, team_leaders,
#            team_core (core-v2 singular), teams_core (core-v2 collection)
#
# Live verification notes (2026-06-08, team_id="4" Chicago Blackhawks):
#   espn_nhl_team           -> OK  (1-row tibble, team metadata)
#   espn_nhl_team_record    -> {} empty body (DONE_WITH_CONCERNS - graceful)
#   espn_nhl_team_depthcharts -> OK (metadata tibble; depth slots not present for NHL)
#   espn_nhl_team_injuries  -> {} empty body (DONE_WITH_CONCERNS - graceful)
#   espn_nhl_team_transactions -> {} empty body (DONE_WITH_CONCERNS - graceful)
#   espn_nhl_team_history   -> {} empty body (DONE_WITH_CONCERNS - graceful)
#   espn_nhl_team_news      -> {} empty body (DONE_WITH_CONCERNS - graceful)
#   espn_nhl_team_leaders   -> {} empty body (DONE_WITH_CONCERNS - graceful)
#   espn_nhl_team_core      -> OK  (1-row tibble, enriched core-v2 fields)
#   espn_nhl_teams_core     -> OK  (32 rows, one per team; $ref + team_id)
#
# Note: record, injuries, transactions, history, news, and leaders all return {}
# (empty JSON object) from site.api.espn.com for NHL — these are not served
# at this endpoint tree for NHL. All graceful-empty with cli_alert_warning.


# ===========================================================================
# 1. Team Detail (singular)
# ===========================================================================

#' Internal: ESPN hockey team detail (league-generic, site-v2)
#' @noRd
.espn_hockey_team <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id)
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      tm <- raw[["team"]] %||% list()
      if (length(tm) == 0) return(result)

      # logos: take first href
      logos <- tm[["logos"]] %||% list()
      logo_href <- if (is.data.frame(logos) && nrow(logos) > 0 &&
                       "href" %in% colnames(logos))
        as.character(logos$href[[1]])
      else if (is.list(logos) && length(logos) > 0)
        logos[[1]][["href"]] %||% NA_character_
      else NA_character_

      logo_dark_href <- if (is.data.frame(logos) && nrow(logos) > 1 &&
                            "href" %in% colnames(logos))
        as.character(logos$href[[2]])
      else if (is.list(logos) && length(logos) > 1)
        logos[[2]][["href"]] %||% NA_character_
      else NA_character_

      # franchise ref
      franchise <- tm[["franchise"]] %||% list()
      franchise_ref <- if (is.list(franchise) && !is.null(franchise[["$ref"]]))
        as.character(franchise[["$ref"]])
      else NA_character_

      row <- list(
        team_id          = as.character(team_id),
        id               = as.character(tm$id            %||% NA_character_),
        uid              = as.character(tm$uid            %||% NA_character_),
        slug             = as.character(tm$slug           %||% NA_character_),
        location         = as.character(tm$location       %||% NA_character_),
        name             = as.character(tm$name           %||% NA_character_),
        nickname         = as.character(tm$nickname       %||% NA_character_),
        abbreviation     = as.character(tm$abbreviation   %||% NA_character_),
        display_name     = as.character(tm$displayName    %||% NA_character_),
        short_display_name = as.character(tm$shortDisplayName %||% NA_character_),
        color            = as.character(tm$color          %||% NA_character_),
        alternate_color  = as.character(tm$alternateColor %||% NA_character_),
        is_active        = as.logical(tm$isActive         %||% NA),
        logo             = logo_href,
        logo_dark        = logo_dark_href,
        stand_summary    = as.character(tm$standingSummary %||% NA_character_),
        franchise_ref    = franchise_ref
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team detail",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team
NULL
#' @title **Get ESPN NHL Team Detail**
#' @rdname espn_nhl_team
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#'    |col_name            |types     |description                             |
#'    |:-------------------|:---------|:---------------------------------------|
#'    |team_id             |character |ESPN team identifier (echoed from arg). |
#'    |id                  |character |ESPN team identifier.                   |
#'    |uid                 |character |ESPN team uid.                          |
#'    |slug                |character |URL slug.                               |
#'    |location            |character |Team city/location.                     |
#'    |name                |character |Team mascot name.                       |
#'    |nickname            |character |Team nickname.                          |
#'    |abbreviation        |character |Team abbreviation.                      |
#'    |display_name        |character |Full display name.                      |
#'    |short_display_name  |character |Short display name.                     |
#'    |color               |character |Primary color hex.                      |
#'    |alternate_color     |character |Alternate color hex.                    |
#'    |is_active           |logical   |Whether the team is active.             |
#'    |logo                |character |Primary logo URL.                       |
#'    |logo_dark           |character |Dark logo URL.                          |
#'    |stand_summary       |character |Standing summary string.                |
#'    |franchise_ref       |character |Franchise resource ref URL.             |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team(team_id = "4"))
#' }
espn_nhl_team <- function(team_id, ...) {
  .espn_hockey_team(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 2. Team Record
# ===========================================================================

#' Internal: ESPN hockey team record (league-generic, site-v2)
#'
#' The `/record` endpoint returns `{}` for NHL — graceful-empty with warning.
#' @noRd
.espn_hockey_team_record <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/record")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      items <- raw[["items"]] %||% list()
      if (length(items) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team record returned no data for team_id={team_id}. \\
          This endpoint may not be populated for {toupper(league)}."
        )
        return(result)
      }

      rows <- lapply(items, function(item) {
        stats <- item[["stats"]] %||% list()
        stats_flat <- stats
        if (is.list(stats)) {
          sv <- tryCatch(
            setNames(
              lapply(stats, function(s) s[["value"]] %||% NA_real_),
              vapply(stats, function(s) janitor::make_clean_names(s[["name"]] %||% "stat"), character(1))
            ),
            error = function(e) list()
          )
          stats_flat <- sv
        }
        base <- list(
          team_id     = as.character(team_id),
          description = as.character(item[["description"]] %||% NA_character_),
          type        = as.character(item[["type"]]        %||% NA_character_),
          summary     = as.character(item[["summary"]]     %||% NA_character_)
        )
        c(base, stats_flat)
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Record data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team record data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team record",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_record
NULL
#' @title **Get ESPN NHL Team Record**
#' @rdname espn_nhl_team_record
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per record type
#'   (overall/home/away) when data is available. The `/record` endpoint
#'   currently returns an empty response for NHL; an empty tibble is returned
#'   with a cli warning. Columns when populated include `team_id`,
#'   `description`, `type`, `summary`, and one numeric column per stat.
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_record(team_id = "4"))
#' }
espn_nhl_team_record <- function(team_id, ...) {
  .espn_hockey_team_record(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 3. Team Depth Charts
# ===========================================================================

#' Internal: ESPN hockey team depthcharts (league-generic, site-v2)
#' @noRd
.espn_hockey_team_depthcharts <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/depthcharts")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      if (length(raw) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team depthcharts returned no data for team_id={team_id}."
        )
        return(result)
      }

      tm <- raw[["team"]] %||% list()
      season <- raw[["season"]] %||% list()

      row <- list(
        team_id          = as.character(team_id),
        timestamp        = as.character(raw[["timestamp"]]   %||% NA_character_),
        status           = as.character(raw[["status"]]      %||% NA_character_),
        season_year      = as.integer(season[["year"]]       %||% NA_integer_),
        season_type      = as.integer(season[["type"]]       %||% NA_integer_),
        season_name      = as.character(season[["name"]]     %||% NA_character_),
        team_espn_id     = as.character(tm[["id"]]           %||% NA_character_),
        abbreviation     = as.character(tm[["abbreviation"]] %||% NA_character_),
        location         = as.character(tm[["location"]]     %||% NA_character_),
        name             = as.character(tm[["name"]]         %||% NA_character_),
        display_name     = as.character(tm[["displayName"]]  %||% NA_character_),
        color            = as.character(tm[["color"]]        %||% NA_character_),
        logo             = as.character(tm[["logo"]]         %||% NA_character_),
        record_summary   = as.character(tm[["recordSummary"]] %||% NA_character_),
        season_summary   = as.character(tm[["seasonSummary"]] %||% NA_character_),
        standing_summary = as.character(tm[["standingSummary"]] %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Depth Charts data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team depthcharts data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team depthcharts",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_depthcharts
NULL
#' @title **Get ESPN NHL Team Depth Charts**
#' @rdname espn_nhl_team_depthcharts
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_depthcharts(team_id = "4"))
#' }
espn_nhl_team_depthcharts <- function(team_id, ...) {
  .espn_hockey_team_depthcharts(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 4. Team Injuries
# ===========================================================================

#' Internal: ESPN hockey team injuries (league-generic, site-v2)
#'
#' Returns `{}` for NHL — graceful-empty with warning.
#' @noRd
.espn_hockey_team_injuries <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/injuries")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      items <- raw[["items"]] %||% list()
      if (length(items) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team injuries returned no data for team_id={team_id}. \\
          This endpoint may not be populated for {toupper(league)}."
        )
        return(result)
      }

      rows <- lapply(items, function(item) {
        athlete <- item[["athlete"]] %||% list()
        injury  <- item[["injury"]]  %||% list()
        list(
          team_id            = as.character(team_id),
          athlete_id         = as.character(athlete[["id"]]          %||% NA_character_),
          athlete_display_name = as.character(athlete[["displayName"]] %||% NA_character_),
          athlete_position   = as.character(athlete[["position"]]    %||% NA_character_),
          injury_type        = as.character(injury[["type"]]         %||% NA_character_),
          injury_status      = as.character(injury[["status"]]       %||% NA_character_),
          injury_date        = as.character(injury[["date"]]         %||% NA_character_),
          injury_description = as.character(injury[["longComment"]]  %||% item[["description"]] %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Injuries data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team injuries data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team injuries",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_injuries
NULL
#' @title **Get ESPN NHL Team Injuries**
#' @rdname espn_nhl_team_injuries
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per injury entry when data
#'   is available. The `/injuries` endpoint currently returns an empty response
#'   for NHL; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `athlete_id`, `athlete_display_name`,
#'   `athlete_position`, `injury_type`, `injury_status`, `injury_date`, and
#'   `injury_description`.
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_injuries(team_id = "4"))
#' }
espn_nhl_team_injuries <- function(team_id, ...) {
  .espn_hockey_team_injuries(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 5. Team Transactions
# ===========================================================================

#' Internal: ESPN hockey team transactions (league-generic, site-v2)
#'
#' Returns `{}` for NHL — graceful-empty with warning.
#' @noRd
.espn_hockey_team_transactions <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/transactions")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      items <- raw[["items"]] %||% list()
      if (length(items) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team transactions returned no data for team_id={team_id}. \\
          This endpoint may not be populated for {toupper(league)}."
        )
        return(result)
      }

      rows <- lapply(items, function(item) {
        list(
          team_id     = as.character(team_id),
          id          = as.character(item[["id"]]          %||% NA_character_),
          date        = as.character(item[["date"]]        %||% NA_character_),
          description = as.character(item[["description"]] %||% NA_character_),
          type        = as.character(item[["type"]]        %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Transactions data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team transactions data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team transactions",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_transactions
NULL
#' @title **Get ESPN NHL Team Transactions**
#' @rdname espn_nhl_team_transactions
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per transaction when data
#'   is available. The `/transactions` endpoint currently returns an empty
#'   response for NHL; an empty tibble is returned with a cli warning. Columns
#'   when populated include `team_id`, `id`, `date`, `description`, and `type`.
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_transactions(team_id = "4"))
#' }
espn_nhl_team_transactions <- function(team_id, ...) {
  .espn_hockey_team_transactions(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 6. Team History
# ===========================================================================

#' Internal: ESPN hockey team history (league-generic, site-v2)
#'
#' Returns `{}` for NHL — graceful-empty with warning.
#' @noRd
.espn_hockey_team_history <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/history")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      items <- raw[["items"]] %||% list()
      if (length(items) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team history returned no data for team_id={team_id}. \\
          This endpoint may not be populated for {toupper(league)}."
        )
        return(result)
      }

      rows <- lapply(items, function(item) {
        list(
          team_id      = as.character(team_id),
          season       = as.integer(item[["season"]]      %||% NA_integer_),
          wins         = as.integer(item[["wins"]]        %||% NA_integer_),
          losses       = as.integer(item[["losses"]]      %||% NA_integer_),
          ties         = as.integer(item[["ties"]]        %||% NA_integer_),
          ot_losses    = as.integer(item[["otLosses"]]    %||% NA_integer_),
          points       = as.integer(item[["points"]]      %||% NA_integer_),
          summary      = as.character(item[["summary"]]   %||% NA_character_),
          description  = as.character(item[["description"]] %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team History data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team history data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team history",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_history
NULL
#' @title **Get ESPN NHL Team History**
#' @rdname espn_nhl_team_history
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per season entry when data
#'   is available. The `/history` endpoint currently returns an empty response
#'   for NHL; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `season`, `wins`, `losses`, `ties`,
#'   `ot_losses`, `points`, `summary`, and `description`.
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_history(team_id = "4"))
#' }
espn_nhl_team_history <- function(team_id, ...) {
  .espn_hockey_team_history(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 7. Team News
# ===========================================================================

#' Internal: ESPN hockey team news (league-generic, site-v2)
#'
#' Returns `{}` for NHL — graceful-empty with warning.
#' @noRd
.espn_hockey_team_news <- function(league = "nhl", team_id, limit = 50, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path  <- paste0("teams/", team_id, "/news")
      query <- list(limit = limit)
      raw   <- .espn_hockey_request("site_v2", path,
                                    query  = query,
                                    league = league,
                                    simplifyVector = FALSE, ...)

      articles <- raw[["articles"]] %||% list()
      if (length(articles) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team news returned no data for team_id={team_id}. \\
          This endpoint may not be populated for {toupper(league)}."
        )
        return(result)
      }

      rows <- lapply(articles, function(art) {
        list(
          team_id     = as.character(team_id),
          id          = as.character(art[["id"]]          %||% NA_character_),
          headline    = as.character(art[["headline"]]    %||% NA_character_),
          description = as.character(art[["description"]] %||% NA_character_),
          type        = as.character(art[["type"]]        %||% NA_character_),
          published   = as.character(art[["published"]]   %||% NA_character_),
          last_modified = as.character(art[["lastModified"]] %||% NA_character_),
          premium     = as.logical(art[["premium"]]       %||% NA),
          url         = as.character(art[["links"]][["web"]][["href"]] %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team News data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team news data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team news",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_news
NULL
#' @title **Get ESPN NHL Team News**
#' @rdname espn_nhl_team_news
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param limit Maximum number of articles to return. Default `50`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per news article when data
#'   is available. The `/news` endpoint currently returns an empty response
#'   for NHL; an empty tibble is returned with a cli warning. Columns when
#'   populated include `team_id`, `id`, `headline`, `description`, `type`,
#'   `published`, `last_modified`, `premium`, and `url`.
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_news(team_id = "4", limit = 10))
#' }
espn_nhl_team_news <- function(team_id, limit = 50, ...) {
  .espn_hockey_team_news(league = "nhl", team_id = team_id, limit = limit, ...)
}


# ===========================================================================
# 8. Team Leaders
# ===========================================================================

#' Internal: ESPN hockey team leaders (league-generic, site-v2)
#'
#' Returns `{}` for NHL — graceful-empty with warning.
#' @noRd
.espn_hockey_team_leaders <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id, "/leaders")
      raw  <- .espn_hockey_request("site_v2", path,
                                   league = league,
                                   simplifyVector = FALSE, ...)

      leaders <- raw[["leaders"]] %||% list()
      if (length(leaders) == 0) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} team leaders returned no data for team_id={team_id}. \\
          This endpoint may not be populated for {toupper(league)}."
        )
        return(result)
      }

      rows <- list()
      for (cat_entry in leaders) {
        cat_name  <- cat_entry[["name"]]         %||% NA_character_
        cat_abbr  <- cat_entry[["abbreviation"]] %||% NA_character_
        cat_items <- cat_entry[["leaders"]]      %||% list()
        for (ldr in cat_items) {
          athlete <- ldr[["athlete"]] %||% list()
          rows <- c(rows, list(list(
            team_id           = as.character(team_id),
            category_name     = as.character(cat_name),
            category_abbr     = as.character(cat_abbr),
            rank              = as.integer(ldr[["rank"]]     %||% NA_integer_),
            value             = as.numeric(ldr[["value"]]    %||% NA_real_),
            display_value     = as.character(ldr[["displayValue"]] %||% NA_character_),
            athlete_id        = as.character(athlete[["id"]]       %||% NA_character_),
            athlete_full_name = as.character(athlete[["fullName"]] %||% NA_character_)
          )))
        }
      }

      if (length(rows) == 0) return(result)

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Leaders data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team leaders data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team leaders",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_leaders
NULL
#' @title **Get ESPN NHL Team Leaders**
#' @rdname espn_nhl_team_leaders
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination when data is available. The `/leaders` endpoint currently
#'   returns an empty response for NHL; an empty tibble is returned with a cli
#'   warning. Columns when populated include `team_id`, `category_name`,
#'   `category_abbr`, `rank`, `value`, `display_value`, `athlete_id`, and
#'   `athlete_full_name`.
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_leaders(team_id = "4"))
#' }
espn_nhl_team_leaders <- function(team_id, ...) {
  .espn_hockey_team_leaders(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 9. Team Core (core-v2 singular)
# ===========================================================================

#' Internal: ESPN hockey team core (league-generic, core-v2)
#' @noRd
.espn_hockey_team_core <- function(league = "nhl", team_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("teams/", team_id)
      raw  <- .espn_hockey_request("core_v2", path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      if (length(raw) == 0) return(result)

      # Extract scalar fields; $ref fields for nested objects
      .ref_str <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else if (is.data.frame(x) && "$ref" %in% colnames(x)) as.character(x[["$ref"]][[1]])
        else NA_character_
      }

      # logos: first href
      logos <- raw[["logos"]] %||% list()
      logo_href <- if (is.data.frame(logos) && nrow(logos) > 0 &&
                       "href" %in% colnames(logos))
        as.character(logos$href[[1]])
      else if (is.list(logos) && length(logos) > 0 && !is.null(logos[[1]][["href"]]))
        as.character(logos[[1]][["href"]])
      else NA_character_

      # alternateIds
      alt_ids <- raw[["alternateIds"]] %||% list()
      sdr_id <- if (is.list(alt_ids)) alt_ids[["sdr"]] %||% NA_character_
                else NA_character_

      row <- list(
        team_id             = as.character(team_id),
        id                  = as.character(raw$id                %||% NA_character_),
        guid                = as.character(raw$guid              %||% NA_character_),
        uid                 = as.character(raw$uid               %||% NA_character_),
        alternate_id_sdr    = as.character(sdr_id),
        slug                = as.character(raw$slug              %||% NA_character_),
        location            = as.character(raw$location          %||% NA_character_),
        name                = as.character(raw$name              %||% NA_character_),
        nickname            = as.character(raw$nickname          %||% NA_character_),
        abbreviation        = as.character(raw$abbreviation      %||% NA_character_),
        display_name        = as.character(raw$displayName       %||% NA_character_),
        short_display_name  = as.character(raw$shortDisplayName  %||% NA_character_),
        color               = as.character(raw$color             %||% NA_character_),
        alternate_color     = as.character(raw$alternateColor    %||% NA_character_),
        is_active           = as.logical(raw$isActive            %||% NA),
        is_all_star         = as.logical(raw$isAllStar           %||% NA),
        logo                = logo_href,
        record_ref          = .ref_str(raw[["record"]]),
        athletes_ref        = .ref_str(raw[["athletes"]]),
        venue_ref           = .ref_str(raw[["venue"]]),
        groups_ref          = .ref_str(raw[["groups"]]),
        statistics_ref      = .ref_str(raw[["statistics"]]),
        leaders_ref         = .ref_str(raw[["leaders"]]),
        injuries_ref        = .ref_str(raw[["injuries"]]),
        awards_ref          = .ref_str(raw[["awards"]]),
        franchise_ref       = .ref_str(raw[["franchise"]]),
        events_ref          = .ref_str(raw[["events"]]),
        transactions_ref    = .ref_str(raw[["transactions"]]),
        coaches_ref         = .ref_str(raw[["coaches"]])
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Team Core data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team core data for team_id={team_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team core",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_team_core
NULL
#' @title **Get ESPN NHL Team Core (core-v2)**
#' @rdname espn_nhl_team_core
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Chicago Blackhawks).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_core(team_id = "4"))
#' }
espn_nhl_team_core <- function(team_id, ...) {
  .espn_hockey_team_core(league = "nhl", team_id = team_id, ...)
}


# ===========================================================================
# 10. Teams Core (core-v2 collection)
# ===========================================================================

#' Internal: ESPN hockey teams core collection (league-generic, core-v2)
#' @noRd
.espn_hockey_teams_core <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", "teams",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw[["items"]]
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "team_id")
      tbl$team_id    <- as.character(tbl$team_id)
      tbl$count      <- as.integer(raw[["count"]]      %||% NA_integer_)
      tbl$page_count <- as.integer(raw[["pageCount"]]  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Teams Core data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} teams core data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} teams core",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_teams_core
NULL
#' @title **Get ESPN NHL Teams Core (core-v2 Collection)**
#' @rdname espn_nhl_teams_core
#' @author Saiem Gilani
#' @param limit Maximum number of teams to return (default `100`; the NHL has
#'   32 active teams so the default returns all).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_teams_core(limit = 100))
#' }
espn_nhl_teams_core <- function(limit = 100, ...) {
  .espn_hockey_teams_core(league = "nhl", limit = limit, ...)
}
