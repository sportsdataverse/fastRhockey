# espn_nhl_athletes_web.R
# ESPN NHL: web-v3 athlete endpoints + league leaders.
# Internal league-generic helpers + public NHL shims.
# Endpoints: athletes/{id}/overview, athletes/{id}/stats,
#            athletes/{id}/splits, statistics/byathlete

# ===========================================================================
# 1. Player Overview
# ===========================================================================

#' Internal: ESPN hockey player overview (league-generic, web-v3)
#'
#' @param league ESPN league slug. Defaults to `"nhl"`.
#' @param athlete_id ESPN athlete identifier.
#' @param ... Passed through to `.retry_request()`.
#' @return A `fastRhockey_data` tibble, one row per stats split.
#' @noRd
.espn_hockey_player_overview <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  overview <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/overview")
      raw  <- .espn_hockey_request("web_v3", path,
                                   league         = league,
                                   simplifyVector = FALSE, ...)

      # ---- athlete bio ----
      ath <- raw[["athlete"]] %||% list()
      pos <- ath[["position"]] %||% list()
      team <- ath[["team"]] %||% list()

      bio <- list(
        athlete_id              = as.character(athlete_id),
        athlete_espn_id         = ath[["id"]] %||% NA_character_,
        athlete_display_name    = ath[["displayName"]] %||% NA_character_,
        athlete_short_name      = ath[["shortName"]] %||% NA_character_,
        athlete_jersey          = ath[["jersey"]] %||% NA_character_,
        athlete_position        = pos[["abbreviation"]] %||% NA_character_,
        athlete_team_id         = team[["id"]] %||% NA_character_,
        athlete_team_abbreviation = team[["abbreviation"]] %||% NA_character_
      )

      # ---- statistics splits ----
      stats_block <- raw[["statistics"]] %||% list()
      names_vec   <- unlist(stats_block[["names"]] %||% list())
      splits      <- stats_block[["splits"]] %||% list()

      if (length(splits) == 0) {
        # No splits: return a one-row bio-only tibble
        overview <- as.data.frame(bio, stringsAsFactors = FALSE) %>%
          janitor::clean_names() %>%
          make_fastRhockey_data(
            paste0(toupper(league), " Player Overview data from ESPN.com"),
            Sys.time()
          )
        return(overview)
      }

      rows <- lapply(splits, function(sp) {
        stats_vals <- unlist(sp[["stats"]] %||% list())
        row <- c(
          bio,
          list(
            split_display_name = sp[["displayName"]] %||% NA_character_
          )
        )
        if (length(names_vec) > 0 && length(stats_vals) > 0) {
          n <- min(length(names_vec), length(stats_vals))
          for (i in seq_len(n)) {
            row[[ janitor::make_clean_names(names_vec[i]) ]] <- stats_vals[i]
          }
        }
        row
      })

      overview <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Overview data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player overview data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player overview",
        args = .args)
    },
    finally = {}
  )
  return(overview)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_overview
NULL
#' @title **Get ESPN NHL Player Overview**
#' @rdname espn_nhl_player_overview
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_overview(athlete_id = "5149125"))
#' }
espn_nhl_player_overview <- function(athlete_id, ...) {
  .espn_hockey_player_overview(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 2. Player Stats V3
# ===========================================================================

#' Internal: ESPN hockey player stats v3 (league-generic, web-v3)
#'
#' Returns per-season stats for an athlete from the web-v3 endpoint.
#' One row per season entry (each `categories[1].statistics[]` element).
#'
#' @param league ESPN league slug.
#' @param athlete_id ESPN athlete identifier.
#' @param season Season end-year (e.g. `2025`). When `NULL` the current
#'   season is used.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per season row.
#' @noRd
.espn_hockey_player_stats_v3 <- function(league = "nhl",
                                          athlete_id,
                                          season = most_recent_nhl_season(),
                                          ...) {
  .args <- .capture_args()

  player_stats <- data.frame()

  tryCatch(
    expr = {
      path  <- paste0("athletes/", athlete_id, "/stats")
      query <- list()
      if (!is.null(season)) query[["season"]] <- season

      raw <- .espn_hockey_request("web_v3", path,
                                  query          = query,
                                  league         = league,
                                  simplifyVector = FALSE, ...)

      categories <- raw[["categories"]] %||% list()
      if (length(categories) == 0) return(player_stats)

      # teams lookup: list of team objects indexed by id
      teams_list <- raw[["teams"]] %||% list()
      team_lookup <- list()
      for (tm in teams_list) {
        tid <- as.character(tm[["id"]] %||% "")
        if (nzchar(tid)) team_lookup[[tid]] <- tm
      }

      rows <- list()
      for (cat_obj in categories) {
        cat_name  <- cat_obj[["name"]]       %||% NA_character_
        cat_label <- cat_obj[["displayName"]] %||% NA_character_
        names_vec <- unlist(cat_obj[["names"]] %||% list())
        stats_entries <- cat_obj[["statistics"]] %||% list()
        totals_vals   <- unlist(cat_obj[["totals"]] %||% list())

        for (entry in stats_entries) {
          team_id  <- as.character(entry[["teamId"]] %||% NA_character_)
          tm_info  <- team_lookup[[team_id]] %||% list()
          seas_raw <- entry[["season"]] %||% list()
          seas_year <- if (is.list(seas_raw))
            as.integer(seas_raw[[1]] %||% NA_integer_)
          else
            as.integer(seas_raw %||% NA_integer_)
          stats_vals <- unlist(entry[["stats"]] %||% list())

          row <- list(
            athlete_id   = as.character(athlete_id),
            season       = as.integer(season %||% NA_integer_),
            season_year  = seas_year,
            category     = cat_name,
            category_display = cat_label,
            position     = entry[["position"]] %||% NA_character_,
            team_id      = team_id,
            team_slug    = entry[["teamSlug"]] %||% NA_character_,
            team_display_name = tm_info[["displayName"]] %||% NA_character_,
            team_abbreviation = tm_info[["abbreviation"]] %||% NA_character_
          )
          if (length(names_vec) > 0 && length(stats_vals) > 0) {
            n <- min(length(names_vec), length(stats_vals))
            for (i in seq_len(n)) {
              row[[ janitor::make_clean_names(names_vec[i]) ]] <- stats_vals[i]
            }
          }
          rows <- c(rows, list(row))
        }

        # Append totals row if present
        if (length(totals_vals) > 0) {
          row_tot <- list(
            athlete_id        = as.character(athlete_id),
            season            = as.integer(season %||% NA_integer_),
            season_year       = NA_integer_,
            category          = cat_name,
            category_display  = cat_label,
            position          = NA_character_,
            team_id           = "TOTAL",
            team_slug         = NA_character_,
            team_display_name = "Career Total",
            team_abbreviation = "TOT"
          )
          if (length(names_vec) > 0) {
            n <- min(length(names_vec), length(totals_vals))
            for (i in seq_len(n)) {
              row_tot[[ janitor::make_clean_names(names_vec[i]) ]] <- totals_vals[i]
            }
          }
          rows <- c(rows, list(row_tot))
        }
      }

      if (length(rows) == 0) return(player_stats)

      player_stats <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Stats V3 data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player stats data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player stats v3",
        args = .args)
    },
    finally = {}
  )
  return(player_stats)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_stats_v3
NULL
#' @title **Get ESPN NHL Player Stats (web-v3)**
#' @rdname espn_nhl_player_stats_v3
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`. When `NULL` the current season is used.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_stats_v3(athlete_id = "5149125", season = 2025))
#' }
espn_nhl_player_stats_v3 <- function(athlete_id,
                                      season = most_recent_nhl_season(),
                                      ...) {
  .espn_hockey_player_stats_v3(league     = "nhl",
                                athlete_id = athlete_id,
                                season     = season,
                                ...)
}


# ===========================================================================
# 3. Player Splits
# ===========================================================================

#' Internal: ESPN hockey player splits (league-generic, web-v3)
#'
#' Situational splits: one row per (split category × split).
#'
#' @param league ESPN league slug.
#' @param athlete_id ESPN athlete identifier.
#' @param season Season end-year. When `NULL` the current season is used.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per split.
#' @noRd
.espn_hockey_player_splits <- function(league = "nhl",
                                        athlete_id,
                                        season = most_recent_nhl_season(),
                                        ...) {
  .args <- .capture_args()

  splits_df <- data.frame()

  tryCatch(
    expr = {
      path  <- paste0("athletes/", athlete_id, "/splits")
      query <- list()
      if (!is.null(season)) query[["season"]] <- season

      raw <- .espn_hockey_request("web_v3", path,
                                  query          = query,
                                  league         = league,
                                  simplifyVector = FALSE, ...)

      # The splits payload is flat at the top level:
      # { displayName, labels, names, splitCategories[] }
      names_vec <- unlist(raw[["names"]] %||% list())
      split_cats <- raw[["splitCategories"]] %||% list()

      if (length(split_cats) == 0) return(splits_df)

      rows <- list()
      for (sc in split_cats) {
        sc_name    <- sc[["name"]]        %||% NA_character_
        sc_display <- sc[["displayName"]] %||% NA_character_
        sp_list    <- sc[["splits"]]      %||% list()

        for (sp in sp_list) {
          stats_vals <- unlist(sp[["stats"]] %||% list())
          row <- list(
            athlete_id              = as.character(athlete_id),
            season                  = as.integer(season %||% NA_integer_),
            split_category          = sc_name,
            split_category_display  = sc_display,
            split_display_name      = sp[["displayName"]]  %||% NA_character_,
            split_abbreviation      = sp[["abbreviation"]] %||% NA_character_
          )
          if (length(names_vec) > 0 && length(stats_vals) > 0) {
            n <- min(length(names_vec), length(stats_vals))
            for (i in seq_len(n)) {
              row[[ janitor::make_clean_names(names_vec[i]) ]] <- stats_vals[i]
            }
          }
          rows <- c(rows, list(row))
        }
      }

      if (length(rows) == 0) return(splits_df)

      splits_df <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Splits data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player splits data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player splits",
        args = .args)
    },
    finally = {}
  )
  return(splits_df)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_splits
NULL
#' @title **Get ESPN NHL Player Splits**
#' @rdname espn_nhl_player_splits
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_splits(athlete_id = "5149125", season = 2025))
#' }
espn_nhl_player_splits <- function(athlete_id,
                                    season = most_recent_nhl_season(),
                                    ...) {
  .espn_hockey_player_splits(league     = "nhl",
                              athlete_id = athlete_id,
                              season     = season,
                              ...)
}


# ===========================================================================
# 4. League Leaders (statistics/byathlete)
# ===========================================================================

#' Internal: ESPN hockey league leaders (league-generic, web-v3)
#'
#' The `statistics/byathlete` endpoint returns per-athlete stats across
#' all stat categories (general, offensive, defensive, penalties). One row
#' per athlete. The `category` parameter filters to a single category.
#'
#' **NOTE**: The `season` and `seasontype` query parameters return 400 for
#' NHL on the web-v3 host. The endpoint always returns the current/most
#' recent season. Pass `season = NULL` and `season_type = NULL` (or omit
#' them) to hit the endpoint successfully. The `season` and `season_type`
#' arguments are echoed on the output rows for bookkeeping only.
#'
#' @param league ESPN league slug.
#' @param category Stat category filter. For NHL: `"offensive"`,
#'   `"defensive"`, `"general"`, `"penalties"`. `NULL` returns all.
#' @param season Season end-year. Echoed on output; not sent as query param.
#' @param season_type Season type code. Echoed on output; not sent as query.
#' @param limit Number of athletes per page (default 50).
#' @param page Page number (default 1).
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per athlete.
#' @noRd
.espn_hockey_leaders <- function(league      = "nhl",
                                  category    = "offensive",
                                  season      = most_recent_nhl_season(),
                                  season_type = 2L,
                                  limit       = 50L,
                                  page        = 1L,
                                  ...) {
  .args <- .capture_args()

  leaders <- data.frame()

  tryCatch(
    expr = {
      # NOTE: passing season/seasontype to this endpoint returns 400 for NHL.
      # The endpoint always uses the current season. We echo the args on the
      # output for consumer convenience.
      query <- list()
      if (!is.null(limit))    query[["limit"]] <- limit
      if (!is.null(page))     query[["page"]]  <- page

      raw <- .espn_hockey_request("web_v3", "statistics/byathlete",
                                  query          = query,
                                  league         = league,
                                  simplifyVector = FALSE, ...)

      athletes_list <- raw[["athletes"]] %||% list()
      if (length(athletes_list) == 0) return(leaders)

      # Build category label map: name -> vector of stat names
      cat_labels_map <- list()
      for (cat_obj in (raw[["categories"]] %||% list())) {
        cat_labels_map[[ cat_obj[["name"]] ]] <- unlist(cat_obj[["names"]] %||% list())
      }

      # Pagination meta
      pg <- raw[["pagination"]] %||% list()
      pg_count <- as.integer(pg[["count"]]  %||% NA_integer_)
      pg_limit <- as.integer(pg[["limit"]]  %||% NA_integer_)
      pg_page  <- as.integer(pg[["page"]]   %||% NA_integer_)
      pg_pages <- as.integer(pg[["pages"]]  %||% NA_integer_)

      # League meta
      lg <- raw[["league"]] %||% list()

      # requestedSeason meta
      rs <- raw[["requestedSeason"]] %||% list()
      rs_year <- as.integer(rs[["year"]] %||% NA_integer_)

      rows <- lapply(athletes_list, function(aw) {
        ath <- aw[["athlete"]] %||% list()
        pos <- ath[["position"]] %||% list()

        # teams: field can be teamId (scalar) or teams (list)
        team_name_val <- ath[["teamName"]] %||% NA_character_
        team_id_val   <- as.character(ath[["teamId"]]   %||% NA_character_)
        # teamLogos list
        logos <- ath[["teamLogos"]] %||% list()
        team_logo <- if (length(logos) > 0) logos[[1]][["href"]] %||% NA_character_ else NA_character_

        row <- list(
          season              = as.integer(season      %||% NA_integer_),
          season_type         = as.integer(season_type %||% NA_integer_),
          requested_season_year = rs_year,
          page                = pg_page,
          pagination_count    = pg_count,
          pagination_limit    = pg_limit,
          pagination_pages    = pg_pages,
          league_id           = lg[["id"]] %||% NA_character_,
          league_name         = lg[["name"]] %||% NA_character_,
          league_abbreviation = lg[["abbreviation"]] %||% NA_character_,
          athlete_id          = as.character(ath[["id"]] %||% NA_character_),
          athlete_display_name = ath[["displayName"]] %||% NA_character_,
          athlete_short_name  = ath[["shortName"]] %||% NA_character_,
          athlete_slug        = ath[["slug"]] %||% NA_character_,
          athlete_position    = pos[["abbreviation"]] %||% NA_character_,
          athlete_jersey      = ath[["jersey"]] %||% NA_character_,
          athlete_team_id     = team_id_val,
          athlete_team_name   = team_name_val,
          athlete_team_short_name = ath[["teamShortName"]] %||% NA_character_,
          athlete_team_logo   = team_logo,
          headshot_href       = (ath[["headshot"]] %||% list())[["href"]] %||% NA_character_,
          debut_year          = as.integer(ath[["debutYear"]] %||% NA_integer_),
          age                 = as.integer(ath[["age"]] %||% NA_integer_)
        )

        # stats per category
        for (cat_row in (aw[["categories"]] %||% list())) {
          cn        <- cat_row[["name"]] %||% ""
          # Filter to requested category if specified
          if (!is.null(category) && !identical(cn, category) && nzchar(category)) next
          names_vec <- cat_labels_map[[cn]] %||% character(0)
          totals    <- unlist(cat_row[["totals"]] %||% list())
          values    <- unlist(cat_row[["values"]] %||% list())
          ranks     <- unlist(cat_row[["ranks"]]  %||% list())
          # Use totals (display values) as primary; values as secondary
          vals <- if (length(totals) > 0) totals else values
          if (length(names_vec) > 0 && length(vals) > 0) {
            n <- min(length(names_vec), length(vals))
            for (i in seq_len(n)) {
              col_name <- paste0(cn, "_", janitor::make_clean_names(names_vec[i]))
              row[[col_name]] <- vals[i]
            }
          }
          if (length(ranks) > 0 && length(names_vec) > 0) {
            n <- min(length(names_vec), length(ranks))
            for (i in seq_len(n)) {
              col_name <- paste0(cn, "_rank_", janitor::make_clean_names(names_vec[i]))
              row[[col_name]] <- ranks[i]
            }
          }
        }
        row
      })

      if (length(rows) == 0) return(leaders)

      leaders <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Leaders data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} leaders data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} leaders",
        args = .args)
    },
    finally = {}
  )
  return(leaders)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_leaders
NULL
#' @title **Get ESPN NHL League Leaders**
#' @rdname espn_nhl_leaders
#' @author Saiem Gilani
#' @param category Stat category to display. For NHL one of `"offensive"`,
#'   `"defensive"`, `"general"`, `"penalties"`, or `NULL` for all
#'   categories. Defaults to `"offensive"`.
#'   **Note**: the `season` and `season_type` query parameters are not
#'   supported by the underlying ESPN endpoint for NHL and are ignored
#'   server-side; they are echoed on the output rows only.
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`. Echoed on output rows; not sent to API.
#' @param season_type Season type code (1 = pre, 2 = regular, 3 = post).
#'   Defaults to `2`. Echoed on output rows; not sent to API.
#' @param limit Athletes per page (default `50`).
#' @param page Page number (default `1`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per athlete:
#'
#'    |col_name                    |types     |description                                       |
#'    |:---------------------------|:---------|:-------------------------------------------------|
#'    |season                      |integer   |Season end-year (echoed from arg).                |
#'    |season_type                 |integer   |Season type code (echoed from arg).               |
#'    |requested_season_year       |integer   |Season year returned by the API.                  |
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_leaders(category = "offensive", limit = 25))
#' }
espn_nhl_leaders <- function(category    = "offensive",
                              season      = most_recent_nhl_season(),
                              season_type = 2L,
                              limit       = 50L,
                              page        = 1L,
                              ...) {
  .espn_hockey_leaders(league      = "nhl",
                        category    = category,
                        season      = season,
                        season_type = season_type,
                        limit       = limit,
                        page        = page,
                        ...)
}
