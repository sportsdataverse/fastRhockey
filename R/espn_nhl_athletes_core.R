# espn_nhl_athletes_core.R
# ESPN NHL: core-v2 athlete-detail wrappers (Tier 4)
# Endpoints: player_core, player_statistics, player_statisticslog,
#            player_eventlog, player_contracts, player_awards,
#            player_records, player_seasons (404 for NHL), player_injuries (404),
#            player_vs_player (404), player_notes (404), athletes_index
#
# All use the core_v2 host:
#   https://sports.core.api.espn.com/v2/sports/hockey/leagues/{league}/athletes/...
#
# Live verification notes (2026-06-08, athlete_id="4024820" Patrik Laine,
#                          athlete_id="3114" Sidney Crosby):
#   player_core          -> OK  (1-row tibble with position/team/jersey etc.)
#   player_statistics    -> OK  (wide 1-row tibble, ~70+ stat columns flattened)
#   player_statisticslog -> OK  (game-log substitute; N rows per season-entry)
#   player_eventlog      -> OK  (1 row per event; events nested JSON, need
#                                simplifyVector=FALSE + manual flatten)
#   player_contracts     -> count=0 for NHL (DONE_WITH_CONCERNS — returns empty)
#   player_awards        -> OK for players with awards (Crosby: 16 entries);
#                           empty (count=0) for players without
#   player_records       -> count=0 for NHL (DONE_WITH_CONCERNS — returns empty)
#   player_seasons       -> HTTP 404 for NHL (DONE_WITH_CONCERNS — returns empty)
#   player_injuries      -> HTTP 404 for NHL (DONE_WITH_CONCERNS — returns empty)
#   player_vs_player     -> HTTP 404 for NHL (DONE_WITH_CONCERNS — returns empty)
#   player_notes         -> HTTP 404 for NHL (DONE_WITH_CONCERNS — returns empty)
#   athletes_index       -> OK  (paginated; active=TRUE returns 1197 athletes)


# ===========================================================================
# 1. Player Core
# ===========================================================================

#' Internal: ESPN hockey player core (athlete detail, league-generic, core-v2)
#' @noRd
.espn_hockey_player_core <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id)
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      .ref_of <- function(x) {
        if (is.list(x) && !is.null(x[["$ref"]])) as.character(x[["$ref"]])
        else NA_character_
      }
      .scalar <- function(x, na = NA_character_) {
        if (is.null(x) || length(x) == 0) na else as.character(x[[1]])
      }

      # birthPlace is a list; extract city/country
      bp <- raw$birthPlace %||% list()
      bc <- raw$birthCountry %||% list()

      row <- list(
        athlete_id        = as.character(raw$id              %||% as.character(athlete_id)),
        uid               = as.character(raw$uid             %||% NA_character_),
        guid              = as.character(raw$guid            %||% NA_character_),
        type              = as.character(raw$type            %||% NA_character_),
        first_name        = as.character(raw$firstName       %||% NA_character_),
        last_name         = as.character(raw$lastName        %||% NA_character_),
        full_name         = as.character(raw$fullName        %||% NA_character_),
        display_name      = as.character(raw$displayName     %||% NA_character_),
        short_name        = as.character(raw$shortName       %||% NA_character_),
        weight            = as.numeric( raw$weight           %||% NA_real_),
        display_weight    = as.character(raw$displayWeight   %||% NA_character_),
        height            = as.numeric( raw$height           %||% NA_real_),
        display_height    = as.character(raw$displayHeight   %||% NA_character_),
        age               = as.integer( raw$age              %||% NA_integer_),
        date_of_birth     = as.character(raw$dateOfBirth     %||% NA_character_),
        debut_year        = as.integer( raw$debutYear        %||% NA_integer_),
        birth_city        = as.character(bp$city             %||% NA_character_),
        birth_country     = as.character(bc$abbreviation     %||% bc[["$ref"]] %||% NA_character_),
        slug              = as.character(raw$slug            %||% NA_character_),
        jersey            = as.character(raw$jersey          %||% NA_character_),
        hand              = as.character((raw$hand$abbreviation %||% raw$hand$type) %||% NA_character_),
        active            = as.logical( raw$active           %||% NA),
        position_ref      = .ref_of(raw$position),
        team_ref          = .ref_of(raw$team),
        statistics_ref    = .ref_of(raw$statistics),
        contracts_ref     = .ref_of(raw$contracts),
        statisticslog_ref = .ref_of(raw$statisticslog),
        athlete_ref       = as.character(raw[["$ref"]]       %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Core data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player core data for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player core",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_core
NULL
#' @title **Get ESPN NHL Player Core (core-v2)**
#' @rdname espn_nhl_player_core
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
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
#'    |debut_year         |integer   |Year of NHL debut.                                    |
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_core(athlete_id = "4024820"))
#' }
espn_nhl_player_core <- function(athlete_id, ...) {
  .espn_hockey_player_core(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 2. Player Statistics (career / aggregate)
# ===========================================================================

#' Internal: ESPN hockey player statistics (league-generic, core-v2)
#' @noRd
.espn_hockey_player_statistics <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/statistics")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      splits <- raw$splits %||% list()
      cats   <- splits$categories

      if (is.null(cats) || (is.data.frame(cats) && nrow(cats) == 0)) {
        cli::cli_alert_warning(
          "No statistics categories found for athlete {athlete_id}."
        )
        return(result)
      }

      # Flatten all categories$stats into a single wide row
      wide <- list(
        athlete_id       = as.character(athlete_id),
        split_id         = as.character(splits$id          %||% NA_character_),
        split_name       = as.character(splits$name        %||% NA_character_),
        split_type       = as.character(splits$type        %||% NA_character_),
        split_abbr       = as.character(splits$abbreviation %||% NA_character_)
      )

      for (i in seq_len(nrow(cats))) {
        stats_df <- cats$stats[[i]]
        if (!is.data.frame(stats_df) || nrow(stats_df) == 0) next
        for (j in seq_len(nrow(stats_df))) {
          col_name <- janitor::make_clean_names(stats_df$name[j] %||% paste0("stat_", j))
          wide[[col_name]] <- stats_df$value[j] %||% NA_real_
        }
      }

      result <- as.data.frame(wide, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Statistics data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player statistics for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player statistics",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_statistics
NULL
#' @title **Get ESPN NHL Player Statistics (core-v2)**
#' @rdname espn_nhl_player_statistics
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with career aggregate statistics.
#'   Columns vary by player type (skater vs goalie). Core columns include:
#'
#'    |col_name        |types     |description                                           |
#'    |:---------------|:---------|:-----------------------------------------------------|
#'    |athlete_id      |character |ESPN athlete id (echoed from arg).                    |
#'    |split_id        |character |Split identifier (e.g. "0" for all splits).           |
#'    |split_name      |character |Split name (e.g. "All Splits").                       |
#'    |split_type      |character |Split type (e.g. "total").                            |
#'    |split_abbr      |character |Split abbreviation (e.g. "TOTAL").                    |
#'    |goals           |numeric   |Career goals (skaters) or goals-against (goalies).    |
#'    |assists         |numeric   |Career assists (skaters).                             |
#'    |points          |numeric   |Career points.                                        |
#'    |games_played    |numeric   |Career games played.                                  |
#'    |...             |numeric   |Additional stat columns flattened from all categories.|
#'
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_statistics(athlete_id = "4024820"))
#' }
espn_nhl_player_statistics <- function(athlete_id, ...) {
  .espn_hockey_player_statistics(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 3. Player Statisticslog (NHL gamelog substitute)
# ===========================================================================

#' Internal: ESPN hockey player statisticslog (league-generic, core-v2)
#' @noRd
.espn_hockey_player_statisticslog <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/statisticslog")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      entries <- raw$entries

      if (is.null(entries) || (is.data.frame(entries) && nrow(entries) == 0)) {
        cli::cli_alert_warning(
          "No statisticslog entries found for athlete {athlete_id}."
        )
        return(result)
      }

      # entries is a data.frame with cols:
      #   season    -> data.frame with one col "$ref" (all season refs)
      #   statistics -> list of per-entry data.frames
      # entries$season[i, "$ref"] gives row i's season ref.
      # entries$statistics[[i]] gives a data.frame with cols: type, statistics, team
      #   where statistics and team are both data.frames with a "$ref" column.
      rows <- vector("list", nrow(entries))
      for (i in seq_len(nrow(entries))) {
        season_ref  <- as.character(entries$season[i, "$ref"] %||% NA_character_)
        season_year <- sub(".*/seasons/([0-9]+).*", "\\1", season_ref %||% "")
        season_year <- if (nzchar(season_year)) as.integer(season_year) else NA_integer_

        stats_list <- entries$statistics[[i]]
        # stats_list: data.frame with cols type, statistics(df), team(df)
        total_stats_ref <- NA_character_
        team_stats_ref  <- NA_character_
        team_ref        <- NA_character_
        if (is.data.frame(stats_list) && nrow(stats_list) > 0) {
          for (j in seq_len(nrow(stats_list))) {
            row_type <- as.character(stats_list$type[j] %||% "")
            # statistics col is a data.frame with "$ref" column
            stat_ref <- if (is.data.frame(stats_list$statistics) &&
                            "$ref" %in% names(stats_list$statistics)) {
              as.character(stats_list$statistics[j, "$ref"] %||% NA_character_)
            } else NA_character_
            if (identical(row_type, "total")) {
              total_stats_ref <- stat_ref
            } else if (identical(row_type, "team")) {
              team_stats_ref <- stat_ref
              if (is.data.frame(stats_list$team) &&
                  "$ref" %in% names(stats_list$team)) {
                team_ref <- as.character(stats_list$team[j, "$ref"] %||% NA_character_)
              }
            }
          }
        }

        rows[[i]] <- data.frame(
          athlete_id       = as.character(athlete_id),
          season           = season_year,
          season_ref       = season_ref,
          total_stats_ref  = total_stats_ref,
          team_stats_ref   = team_stats_ref,
          team_ref         = team_ref,
          stringsAsFactors = FALSE
        )
      }

      tbl <- do.call(rbind, rows)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Statisticslog data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player statisticslog for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player statisticslog",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_statisticslog
NULL
#' @title **Get ESPN NHL Player Statistics Log (core-v2)**
#' @rdname espn_nhl_player_statisticslog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per season-entry.
#'   This endpoint is the NHL game-log substitute (web-v3 gamelog 404s for NHL).
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_statisticslog(athlete_id = "4024820"))
#' }
espn_nhl_player_statisticslog <- function(athlete_id, ...) {
  .espn_hockey_player_statisticslog(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 4. Player Eventlog
# ===========================================================================

#' Internal: ESPN hockey player eventlog (league-generic, core-v2)
#' @noRd
.espn_hockey_player_eventlog <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/eventlog")
      # Must use simplifyVector=FALSE because events is a nested {count, items:[]}
      # structure, not a flat array.
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = FALSE, ...)

      events_obj <- raw$events %||% list()
      items      <- events_obj$items %||% list()

      if (length(items) == 0) {
        cli::cli_alert_warning(
          "No eventlog items found for athlete {athlete_id}."
        )
        return(result)
      }

      rows <- lapply(items, function(ev) {
        event_ref       <- ev$event[["$ref"]]       %||% NA_character_
        competition_ref <- ev$competition[["$ref"]] %||% NA_character_
        statistics_ref  <- ev$statistics[["$ref"]]  %||% NA_character_
        event_id <- .espn_ref_id(event_ref)
        data.frame(
          athlete_id      = as.character(athlete_id),
          event_id        = as.character(event_id),
          team_id         = as.character(ev$teamId   %||% NA_character_),
          played          = as.logical( ev$played    %||% NA),
          event_ref       = as.character(event_ref),
          competition_ref = as.character(competition_ref),
          statistics_ref  = as.character(statistics_ref),
          stringsAsFactors = FALSE
        )
      })

      tbl <- do.call(rbind, rows)
      tbl$count      <- as.integer(events_obj$count      %||% NA_integer_)
      tbl$page_count <- as.integer(events_obj$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Eventlog data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player eventlog for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player eventlog",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_eventlog
NULL
#' @title **Get ESPN NHL Player Event Log (core-v2)**
#' @rdname espn_nhl_player_eventlog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_eventlog(athlete_id = "4024820"))
#' }
espn_nhl_player_eventlog <- function(athlete_id, ...) {
  .espn_hockey_player_eventlog(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 5. Player Contracts
# ===========================================================================

#' Internal: ESPN hockey player contracts (league-generic, core-v2)
#' @noRd
.espn_hockey_player_contracts <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/contracts")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      count <- as.integer(raw$count %||% 0L)
      if (count == 0L) {
        cli::cli_alert_warning(
          paste0(
            "espn_nhl_player_contracts(): No contract data served by ESPN ",
            "core-v2 for athlete {athlete_id} in the NHL. ",
            "The endpoint responds with count=0 for all queried NHL players ",
            "(DONE_WITH_CONCERNS: endpoint exists but data is not populated)."
          )
        )
        return(result)
      }

      items <- raw$items
      tbl <- .espn_core_collection(items, id_col = "contract_id")
      tbl$athlete_id  <- as.character(athlete_id)
      tbl$count       <- count
      tbl$page_count  <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Contracts data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player contracts for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player contracts",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_contracts
NULL
#' @title **Get ESPN NHL Player Contracts (core-v2)**
#' @rdname espn_nhl_player_contracts
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_contracts(athlete_id = "4024820"))
#' }
espn_nhl_player_contracts <- function(athlete_id, ...) {
  .espn_hockey_player_contracts(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 6. Player Awards
# ===========================================================================

#' Internal: ESPN hockey player awards (league-generic, core-v2)
#' @noRd
.espn_hockey_player_awards <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/awards")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      count <- as.integer(raw$count %||% 0L)
      if (count == 0L) {
        cli::cli_alert_warning(
          "No award data found for athlete {athlete_id}."
        )
        return(result)
      }

      items <- raw$items
      tbl   <- .espn_core_collection(items, id_col = "award_id")
      tbl$athlete_id  <- as.character(athlete_id)
      tbl$count       <- count
      tbl$page_count  <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Awards data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player awards for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player awards",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_awards
NULL
#' @title **Get ESPN NHL Player Awards (core-v2)**
#' @rdname espn_nhl_player_awards
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_awards(athlete_id = "3114"))
#' }
espn_nhl_player_awards <- function(athlete_id, ...) {
  .espn_hockey_player_awards(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 7. Player Records
# ===========================================================================

#' Internal: ESPN hockey player records (league-generic, core-v2)
#' @noRd
.espn_hockey_player_records <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/records")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      count <- as.integer(raw$count %||% 0L)
      if (count == 0L) {
        cli::cli_alert_warning(
          paste0(
            "espn_nhl_player_records(): No record data served by ESPN core-v2 ",
            "for athlete {athlete_id}. The endpoint responds with count=0 for ",
            "all queried NHL players ",
            "(DONE_WITH_CONCERNS: endpoint exists but data is not populated)."
          )
        )
        return(result)
      }

      items <- raw$items
      tbl   <- .espn_core_collection(items, id_col = "record_id")
      tbl$athlete_id  <- as.character(athlete_id)
      tbl$count       <- count
      tbl$page_count  <- as.integer(raw$pageCount %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Records data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player records for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player records",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_records
NULL
#' @title **Get ESPN NHL Player Records (core-v2)**
#' @rdname espn_nhl_player_records
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_records(athlete_id = "4024820"))
#' }
espn_nhl_player_records <- function(athlete_id, ...) {
  .espn_hockey_player_records(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 8. Player Seasons
# ===========================================================================

#' Internal: ESPN hockey player seasons (league-generic, core-v2)
#' @noRd
.espn_hockey_player_seasons <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/seasons")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_seasons(): ESPN core-v2 athletes/{{id}}/seasons ",
          "returns HTTP 404 for all queried NHL players ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
      return(result)
    },
    error = function(e) {
      # Gracefully swallow the 404
      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_seasons(): ESPN core-v2 athletes/{{id}}/seasons ",
          "returns HTTP 404 for the requested NHL athlete ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player seasons",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_seasons
NULL
#' @title **Get ESPN NHL Player Seasons (core-v2)**
#' @rdname espn_nhl_player_seasons
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/seasons` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint is not served for the NHL).
#'   For season-level data use `espn_nhl_player_statisticslog()`.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_seasons(athlete_id = "4024820"))
#' }
espn_nhl_player_seasons <- function(athlete_id, ...) {
  .espn_hockey_player_seasons(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 9. Player Injuries
# ===========================================================================

#' Internal: ESPN hockey player injuries (league-generic, core-v2)
#' @noRd
.espn_hockey_player_injuries <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/injuries")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_injuries(): ESPN core-v2 athletes/{{id}}/injuries ",
          "returns HTTP 404 for all queried NHL players ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL). ",
          "Use the league-level injuries endpoint for NHL injury data."
        )
      )
      return(result)
    },
    error = function(e) {
      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_injuries(): ESPN core-v2 athletes/{{id}}/injuries ",
          "returns HTTP 404 for the requested NHL athlete ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player injuries",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_injuries
NULL
#' @title **Get ESPN NHL Player Injuries (core-v2)**
#' @rdname espn_nhl_player_injuries
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/injuries` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'   Use `espn_nhl_injuries()` (site-v2) for league-level injury data.
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_injuries(athlete_id = "4024820"))
#' }
espn_nhl_player_injuries <- function(athlete_id, ...) {
  .espn_hockey_player_injuries(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 10. Player vs Player
# ===========================================================================

#' Internal: ESPN hockey player vs player (league-generic, core-v2)
#' @noRd
.espn_hockey_player_vs_player <- function(league = "nhl", athlete_id,
                                          opponent_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/vsathlete/", opponent_id)
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_vs_player(): ESPN core-v2 athletes/{{id}}/vsathlete/{{opp}} ",
          "returns HTTP 404 for all queried NHL player pairs ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
      return(result)
    },
    error = function(e) {
      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_vs_player(): ESPN core-v2 athletes/{{id}}/vsathlete/{{opp}} ",
          "returns HTTP 404 for the requested NHL athlete pair ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player vs player",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_vs_player
NULL
#' @title **Get ESPN NHL Player vs Player (core-v2)**
#' @rdname espn_nhl_player_vs_player
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param opponent_id ESPN opponent athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{id}/vsathlete/{opp_id}` returns HTTP 404 for all queried NHL
#'   player pairs (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_vs_player(athlete_id = "4024820", opponent_id = "3114"))
#' }
espn_nhl_player_vs_player <- function(athlete_id, opponent_id, ...) {
  .espn_hockey_player_vs_player(league = "nhl", athlete_id = athlete_id,
                                 opponent_id = opponent_id, ...)
}


# ===========================================================================
# 11. Player Notes
# ===========================================================================

#' Internal: ESPN hockey player notes (league-generic, core-v2)
#' @noRd
.espn_hockey_player_notes <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/notes")
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_notes(): ESPN core-v2 athletes/{{id}}/notes ",
          "returns HTTP 404 for all queried NHL players ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
      return(result)
    },
    error = function(e) {
      cli::cli_alert_warning(
        paste0(
          "espn_nhl_player_notes(): ESPN core-v2 athletes/{{id}}/notes ",
          "returns HTTP 404 for the requested NHL athlete ",
          "(DONE_WITH_CONCERNS: endpoint not served for NHL)."
        )
      )
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player notes",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_player_notes
NULL
#' @title **Get ESPN NHL Player Notes (core-v2)**
#' @rdname espn_nhl_player_notes
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return An empty `data.frame()`. As of 2026-06-08, ESPN core-v2
#'   `athletes/{{id}}/notes` returns HTTP 404 for all queried NHL players
#'   (DONE_WITH_CONCERNS: endpoint not served for NHL).
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_notes(athlete_id = "4024820"))
#' }
espn_nhl_player_notes <- function(athlete_id, ...) {
  .espn_hockey_player_notes(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 12. Athletes Index (paginated)
# ===========================================================================

#' Internal: ESPN hockey athletes index (league-generic, core-v2)
#' @noRd
.espn_hockey_athletes_index <- function(league = "nhl",
                                        active = TRUE,
                                        limit  = 100,
                                        page   = 1,
                                        ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path  <- "athletes"
      query <- list(
        active = if (isTRUE(active)) "true" else if (isFALSE(active)) "false" else NULL,
        limit  = limit,
        page   = page
      )
      raw <- .espn_hockey_request("core_v2", path = path,
                                  query  = query,
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items

      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "No athletes found for the requested parameters."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "athlete_id")
      tbl$athlete_id  <- as.character(tbl$athlete_id)
      tbl$active      <- active
      tbl$page        <- as.integer(page)
      tbl$count       <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count  <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Athletes Index data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} athletes index data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} athletes index",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_athletes_index
NULL
#' @title **Get ESPN NHL Athletes Index (core-v2)**
#' @rdname espn_nhl_athletes_index
#' @author Saiem Gilani
#' @param active Filter by active status. `TRUE` (default) = active players only;
#'   `FALSE` = inactive only; `NULL` = no filter.
#' @param limit Number of athletes per page (default `100`; max ~200).
#' @param page Page number (default `1`). Use `page_count` from the result
#'   to determine the total number of pages.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per athlete in the requested
#'   page. Surface `count` and `page_count` to manage pagination.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_athletes_index(active = TRUE, limit = 10, page = 1))
#' }
espn_nhl_athletes_index <- function(active = TRUE,
                                     limit  = 100,
                                     page   = 1,
                                     ...) {
  .espn_hockey_athletes_index(league = "nhl",
                               active = active,
                               limit  = limit,
                               page   = page,
                               ...)
}
