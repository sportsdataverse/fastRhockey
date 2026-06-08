# espn_nhl_aliases.R
# ESPN NHL: parity Group F -- variants/aliases + calendar variants.
#
# Functions in this file:
#   espn_nhl_teams_site          -- alias for espn_nhl_teams (site_v2 teams)
#   espn_nhl_team_schedule       -- alias for espn_nhl_schedule (site_v2 team schedule)
#   espn_nhl_standings_core      -- core_v2 standings (seasons/{s}/types/{t}/groups/9/standings)
#   espn_nhl_leaders_core        -- core_v2 league leaders (leagues/nhl/leaders)
#   espn_nhl_coach_season        -- core_v2 coach-in-season (seasons/{s}/coaches/{coach_id})
#   espn_nhl_calendar_ondays     -- core_v2 calendar/ondays (game dates)
#   espn_nhl_calendar_offseason  -- core_v2 seasons/{s}/types/1/calendar/offseason
#   espn_nhl_calendar_postseason -- core_v2 seasons/{s}/types/3/calendar/postseason
#   espn_nhl_calendar_regular_season -- core_v2 calendar/whitelist (regular-season game dates)
#
# Confirmed endpoint paths (live-verified 2026-06-08):
#   standings_core     : core_v2 seasons/{season}/types/{season_type}/groups/9/standings
#                        (items -> follow each $ref -> standings[] entries with team + records)
#   leaders_core       : core_v2 (leagues/{league}/leaders) -- categories with leaders list
#   coach_season       : core_v2 seasons/{season}/coaches/{coach_id}
#                        (sdv-py says coaches/{id}/seasons/{s} but that 404s; correct path verified)
#   calendar_ondays    : core_v2 calendar/ondays -> eventDate.dates[]
#   calendar_offseason : core_v2 seasons/{season}/types/1/calendar/offseason (site_v2 path 404s)
#   calendar_postseason: core_v2 seasons/{season}/types/3/calendar/postseason (site_v2 path 404s)
#   calendar_regular_season: core_v2 calendar/whitelist (sdv-py path calendar/regular-season 400s;
#                        whitelist returns identical shape and is the working equivalent)


# ===========================================================================
# 1. espn_nhl_teams_site  (alias for espn_nhl_teams)
# ===========================================================================

#' @name espn_nhl_teams_site
NULL
#' @title **Get ESPN NHL Teams (site_v2) -- alias for `espn_nhl_teams()`**
#' @rdname espn_nhl_teams_site
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per NHL team. This is
#'   identical to the output of [espn_nhl_teams()].
#'
#'    |col_name         |types     |description                         |
#'    |:----------------|:---------|:-----------------------------------|
#'    |espn_team_id     |character |ESPN team identifier.               |
#'    |team             |character |Team location/city name.            |
#'    |mascot           |character |Team mascot/nickname.               |
#'    |display_name     |character |Team display name.                  |
#'    |short_name       |character |Short team display name.            |
#'    |abbreviation     |character |Team abbreviation.                  |
#'    |color            |character |Primary color hex.                  |
#'    |alternate_color  |character |Alternate color hex.                |
#'    |logo             |character |Light-background logo URL.          |
#'    |logo_dark        |character |Dark-background logo URL.           |
#'
#' @importFrom dplyr group_by ungroup select rename
#' @importFrom tidyr unnest_wider
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @seealso [espn_nhl_teams()] which this function wraps.
#' @examples
#' \donttest{
#'   try(espn_nhl_teams_site())
#' }
espn_nhl_teams_site <- function(...) {
  .espn_hockey_teams(league = "nhl", ...)
}


# ===========================================================================
# 2. espn_nhl_team_schedule  (alias for espn_nhl_schedule)
# ===========================================================================

#' @name espn_nhl_team_schedule
NULL
#' @title **Get ESPN NHL Team Schedule (site_v2) -- alias for `espn_nhl_schedule()`**
#' @rdname espn_nhl_team_schedule
#' @author Saiem Gilani
#' @param team_id ESPN team identifier (character or numeric, e.g. `"4"` for
#'   Pittsburgh Penguins).
#' @param season Season end-year (e.g. `2025`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per game. Identical to
#'   [espn_nhl_schedule()].
#'
#'    |col_name              |types     |description                                        |
#'    |:---------------------|:---------|:--------------------------------------------------|
#'    |game_id               |character |ESPN event identifier.                             |
#'    |date                  |character |Game date-time (ISO 8601).                         |
#'    |name                  |character |Full game name.                                    |
#'    |short_name            |character |Short game name.                                   |
#'    |season_year           |integer   |Season end year.                                   |
#'    |season_type           |integer   |Season type code.                                  |
#'    |time_valid            |logical   |Whether the game time is confirmed.                |
#'    |status_type_id        |character |Status type identifier.                            |
#'    |status_type_name      |character |Status type name.                                  |
#'    |status_type_state     |character |Status state (pre/in/post).                        |
#'    |status_type_completed |logical   |Whether the game is complete.                      |
#'    |neutral_site          |logical   |Whether the game is at a neutral site.             |
#'    |attendance            |integer   |Game attendance.                                   |
#'    |venue_id              |character |Venue identifier.                                  |
#'    |venue_full_name       |character |Venue full name.                                   |
#'    |venue_city            |character |Venue city.                                        |
#'    |venue_state           |character |Venue state.                                       |
#'    |broadcast             |character |Broadcast network(s).                              |
#'    |home_id               |character |Home team ESPN identifier.                         |
#'    |home_name             |character |Home team display name.                            |
#'    |home_abbreviation     |character |Home team abbreviation.                            |
#'    |home_location         |character |Home team city.                                    |
#'    |home_logo             |character |Home team logo URL.                                |
#'    |home_score            |character |Home team score.                                   |
#'    |home_winner           |logical   |Whether the home team won.                         |
#'    |away_id               |character |Away team ESPN identifier.                         |
#'    |away_name             |character |Away team display name.                            |
#'    |away_abbreviation     |character |Away team abbreviation.                            |
#'    |away_location         |character |Away team city.                                    |
#'    |away_logo             |character |Away team logo URL.                                |
#'    |away_score            |character |Away team score.                                   |
#'    |away_winner           |logical   |Whether the away team won.                         |
#'    |team_id               |character |ESPN team identifier (echoed from arg).            |
#'    |season                |integer   |Season end-year (echoed from arg).                 |
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @seealso [espn_nhl_schedule()] which this function wraps.
#' @examples
#' \donttest{
#'   try(espn_nhl_team_schedule(team_id = "4", season = 2025))
#' }
espn_nhl_team_schedule <- function(team_id,
                                    season = most_recent_nhl_season(),
                                    ...) {
  .espn_hockey_schedule(league = "nhl", team_id = team_id,
                         season = season, ...)
}


# ===========================================================================
# 3. espn_nhl_standings_core  (core_v2 standings)
# ===========================================================================

#' Internal: ESPN hockey standings via core-v2 (league-generic)
#'
#' Endpoint: `seasons/{season}/types/{season_type}/groups/{group_id}/standings`
#' (group_id defaults to 9, the "all-teams" rollup group verified for NHL).
#' Each item in the returned collection is followed to extract per-team records
#' and stats from the `standings[].records[].stats[]` shape.
#'
#' @noRd
.espn_hockey_standings_core <- function(league      = "nhl",
                                         season      = most_recent_nhl_season(),
                                         season_type = 2,
                                         group_id    = 9,
                                         ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season,
                     "/types/", season_type,
                     "/groups/", group_id,
                     "/standings")

      raw <- .espn_hockey_request("core_v2", path = path,
                                   league         = league,
                                   simplifyVector = FALSE, ...)

      items <- raw$items
      if (is.null(items) || length(items) == 0) {
        return(result)
      }

      all_rows <- list()

      for (item in items) {
        ref_url <- item[["$ref"]] %||% NULL
        if (is.null(ref_url) || !nzchar(ref_url)) next

        group_obj <- tryCatch(
          .espn_follow_ref(ref_url, simplifyVector = FALSE, ...),
          error = function(e) NULL
        )
        if (is.null(group_obj)) next

        group_name <- group_obj$name %||% NA_character_
        group_dn   <- group_obj$displayName %||% NA_character_
        group_id_v <- group_obj$id %||% NA_character_

        standings_list <- group_obj$standings
        if (is.null(standings_list) || length(standings_list) == 0) next

        for (entry in standings_list) {
          team_ref_url <- entry$team[["$ref"]] %||% NA_character_
          team_id_val  <- .espn_ref_id(team_ref_url)

          records <- entry$records %||% list()
          # Use the "total" record type for top-level stats
          total_rec <- NULL
          for (rec in records) {
            if (identical(rec$type %||% "", "total")) {
              total_rec <- rec
              break
            }
          }
          if (is.null(total_rec) && length(records) > 0) {
            total_rec <- records[[1]]
          }

          row <- list(
            group_id_v     = as.character(group_id_v),
            group_name     = as.character(group_name),
            group_display_name = as.character(group_dn),
            team_id        = as.character(team_id_val),
            team_ref       = as.character(team_ref_url),
            record_type    = as.character(total_rec$type %||% NA_character_),
            record_summary = as.character(total_rec$summary %||% NA_character_),
            season         = as.integer(season),
            season_type    = as.integer(season_type)
          )

          stats_list <- total_rec$stats %||% list()
          for (stat in stats_list) {
            col <- janitor::make_clean_names(stat$name %||% stat$abbreviation %||% "stat")
            row[[col]] <- stat$value %||% NA_real_
          }

          all_rows <- c(all_rows, list(row))
        }
      }

      if (length(all_rows) == 0) return(result)

      result <- dplyr::bind_rows(lapply(all_rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Standings (core-v2) data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} core standings for season {season} type {season_type} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} core standings",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_standings_core
NULL
#' @title **Get ESPN NHL Standings (core-v2)**
#' @rdname espn_nhl_standings_core
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param season_type Season type code: `1`=pre-season, `2`=regular season
#'   (default), `3`=post-season.
#' @param group_id Standings group identifier. Defaults to `9`, which is the
#'   verified all-teams rollup group for NHL on core-v2.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per team per standings
#'   group. Key columns:
#'
#'    |col_name            |types     |description                                           |
#'    |:-------------------|:---------|:-----------------------------------------------------|
#'    |group_id_v          |character |Standings group identifier.                           |
#'    |group_name          |character |Group name (e.g. "overall").                          |
#'    |group_display_name  |character |Group display name.                                   |
#'    |team_id             |character |ESPN team identifier (parsed from `$ref`).            |
#'    |team_ref            |character |`$ref` URL for the team-in-season.                   |
#'    |record_type         |character |Record type used (always "total").                    |
#'    |record_summary      |character |Record summary string (e.g. "53-22-7").               |
#'    |season              |integer   |Season year (echoed from arg).                        |
#'    |season_type         |integer   |Season type code (echoed from arg).                   |
#'    |ot_losses           |numeric   |Overtime losses (and other stat columns dynamically). |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_standings_core(season = 2026, season_type = 2))
#' }
espn_nhl_standings_core <- function(season      = most_recent_nhl_season(),
                                     season_type = 2,
                                     group_id    = 9,
                                     ...) {
  .espn_hockey_standings_core(league      = "nhl",
                               season      = season,
                               season_type = season_type,
                               group_id    = group_id,
                               ...)
}


# ===========================================================================
# 4. espn_nhl_leaders_core  (core_v2 league leaders)
# ===========================================================================

#' Internal: ESPN hockey league leaders via core-v2 (league-generic)
#'
#' Endpoint: `https://sports.core.api.espn.com/v2/sports/hockey/leagues/{league}/leaders`
#' Returns `categories[]` each with `leaders[]` containing `displayValue`,
#' `value`, `athlete.$ref`, `statistics.$ref`.
#'
#' NOTE: this endpoint does not accept a season parameter; it returns the
#' current/most-recent season leaders. For season-specific leaders use
#' [espn_nhl_season_type_leaders()].
#'
#' @noRd
.espn_hockey_leaders_core <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = "leaders",
                                   league         = league,
                                   simplifyVector = TRUE, ...)

      cats <- raw$categories
      if (is.null(cats) || (is.data.frame(cats) && nrow(cats) == 0)) {
        return(result)
      }

      rows <- lapply(seq_len(nrow(cats)), function(i) {
        cat_row  <- cats[i, , drop = FALSE]
        cat_name <- as.character(cat_row$name              %||% NA_character_)
        cat_dn   <- as.character(cat_row$displayName       %||% NA_character_)
        cat_sdn  <- as.character(cat_row$shortDisplayName  %||% NA_character_)
        cat_abbr <- as.character(cat_row$abbreviation      %||% NA_character_)

        leaders_df <- if (is.list(cat_row$leaders) && length(cat_row$leaders) > 0)
          cat_row$leaders[[1]] else data.frame()

        if (!is.data.frame(leaders_df) || nrow(leaders_df) == 0) {
          return(list(data.frame(
            category_name               = cat_name,
            category_display_name       = cat_dn,
            category_short_display_name = cat_sdn,
            category_abbreviation       = cat_abbr,
            display_value               = NA_character_,
            value                       = NA_real_,
            rel                         = NA_character_,
            athlete_ref                 = NA_character_,
            statistics_ref              = NA_character_,
            stringsAsFactors            = FALSE
          )))
        }

        ath_refs  <- if (is.data.frame(leaders_df$athlete) &&
                         "$ref" %in% colnames(leaders_df$athlete))
          as.character(leaders_df$athlete[["$ref"]]) else rep(NA_character_, nrow(leaders_df))
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
          statistics_ref              = stat_refs,
          stringsAsFactors            = FALSE
        ))
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) r[[1]])) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Leaders (core-v2) data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} core leaders data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} core leaders",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_leaders_core
NULL
#' @title **Get ESPN NHL League Leaders (core-v2)**
#' @rdname espn_nhl_leaders_core
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (category, leader)
#'   combination. For a typical NHL season there are 7 stat categories with
#'   25 leaders each (175 rows). For season-specific / season-type leaders
#'   use [espn_nhl_season_type_leaders()].
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_leaders_core())
#' }
espn_nhl_leaders_core <- function(...) {
  .espn_hockey_leaders_core(league = "nhl", ...)
}


# ===========================================================================
# 5. espn_nhl_coach_season  (core_v2 coach in season)
# ===========================================================================

#' Internal: ESPN hockey coach-in-season (league-generic, core-v2)
#'
#' Endpoint: `seasons/{season}/coaches/{coach_id}`
#'
#' Note: sdv-py documents the path as `coaches/{coach_id}/seasons/{season}`
#' but live verification confirms only `seasons/{season}/coaches/{coach_id}`
#' returns HTTP 200. The sdv-py path returns 404 for all tested coach IDs.
#'
#' @noRd
.espn_hockey_coach_season <- function(league   = "nhl",
                                       coach_id,
                                       season   = most_recent_nhl_season(),
                                       ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("seasons/", season, "/coaches/", coach_id)

      raw <- .espn_hockey_request("core_v2", path = path,
                                   league         = league,
                                   simplifyVector = TRUE, ...)

      if (is.null(raw) || length(raw) == 0) {
        return(result)
      }

      bp <- raw$birthPlace
      if (!is.data.frame(bp)) bp <- data.frame()

      person <- raw$person
      if (!is.data.frame(person) && !is.list(person)) person <- list()

      row <- list(
        coach_id       = as.character(coach_id),
        season         = as.integer(season),
        espn_id        = as.character(raw$id         %||% NA_character_),
        uid            = as.character(raw$uid        %||% NA_character_),
        first_name     = as.character(raw$firstName  %||% NA_character_),
        last_name      = as.character(raw$lastName   %||% NA_character_),
        date_of_birth  = as.character(raw$dateOfBirth %||% NA_character_),
        birth_city     = if (is.data.frame(bp) && "city" %in% colnames(bp))
          as.character(bp$city)  else NA_character_,
        birth_country  = if (is.data.frame(bp) && "country" %in% colnames(bp))
          as.character(bp$country) else NA_character_,
        person_ref     = if (is.list(person) && !is.null(person[["$ref"]]))
          as.character(person[["$ref"]]) else NA_character_
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Coach Season data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} coach {coach_id} season {season} data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} coach season",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_coach_season
NULL
#' @title **Get ESPN NHL Coach Season (core-v2)**
#' @rdname espn_nhl_coach_season
#' @author Saiem Gilani
#' @param coach_id ESPN coach identifier (character or numeric). Use
#'   `espn_nhl_season_coaches()` to find valid IDs.
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_coach_season(coach_id = "900", season = 2026))
#' }
espn_nhl_coach_season <- function(coach_id,
                                   season = most_recent_nhl_season(),
                                   ...) {
  .espn_hockey_coach_season(league   = "nhl",
                              coach_id = coach_id,
                              season   = season,
                              ...)
}


# ===========================================================================
# 6. Calendar helpers  (core_v2)
# ===========================================================================

#' Parse a core-v2 calendar endpoint's eventDate.dates into a tibble.
#' Shared by all four calendar variant wrappers.
#' @param league ESPN league slug.
#' @param path  Path relative to the core_v2 + league prefix.
#' @param extra_cols Named list of extra scalar columns to append (echoed args).
#' @param label  Data label for make_fastRhockey_data().
#' @noRd
.espn_hockey_calendar_dates <- function(league = "nhl",
                                         path,
                                         extra_cols = list(),
                                         label,
                                         ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", path = path,
                                   league         = league,
                                   simplifyVector = TRUE, ...)

      event_date <- raw$eventDate
      if (is.null(event_date)) return(result)

      dates_vec <- unlist(event_date$dates %||% list())
      if (length(dates_vec) == 0) return(result)

      df <- data.frame(
        date       = as.character(dates_vec),
        date_type  = as.character(event_date$type     %||% NA_character_),
        start_date = as.character(raw$startDate       %||% NA_character_),
        end_date   = as.character(raw$endDate         %||% NA_character_),
        stringsAsFactors = FALSE
      )

      # Append echoed args
      for (nm in names(extra_cols)) {
        df[[nm]] <- extra_cols[[nm]]
      }

      result <- df %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " ", label, " data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = paste0("Invalid arguments or no ESPN {league} ", label, " data available!"),
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = paste0("Warning fetching ESPN {league} ", label),
        args = .args)
    },
    finally = {}
  )
  return(result)
}


# ---------------------------------------------------------------------------
# 6a. espn_nhl_calendar_ondays
# ---------------------------------------------------------------------------

#' @name espn_nhl_calendar_ondays
NULL
#' @title **Get ESPN NHL Calendar On-Days (core-v2)**
#' @rdname espn_nhl_calendar_ondays
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per game date. The
#'   endpoint returns all dates in the current season that have games
#'   (typically ~227 dates for a full regular season).
#'
#'    |col_name   |types     |description                                      |
#'    |:----------|:---------|:------------------------------------------------|
#'    |date       |character |Game date (ISO 8601 datetime string).            |
#'    |date_type  |character |Calendar date type (e.g. `"day"`).              |
#'    |start_date |character |Season start date.                               |
#'    |end_date   |character |Season end date.                                 |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_calendar_ondays())
#' }
espn_nhl_calendar_ondays <- function(...) {
  .espn_hockey_calendar_dates(
    league     = "nhl",
    path       = "calendar/ondays",
    extra_cols = list(),
    label      = "Calendar On-Days",
    ...
  )
}


# ---------------------------------------------------------------------------
# 6b. espn_nhl_calendar_offseason
# ---------------------------------------------------------------------------

#' Internal: ESPN hockey calendar offseason (league-generic, core-v2)
#' Path: `seasons/{season}/types/1/calendar/offseason`
#' @noRd
.espn_hockey_calendar_offseason <- function(league = "nhl",
                                             season = most_recent_nhl_season(),
                                             ...) {
  path <- paste0("seasons/", season, "/types/1/calendar/offseason")
  .espn_hockey_calendar_dates(
    league     = league,
    path       = path,
    extra_cols = list(season = as.integer(season)),
    label      = "Calendar Offseason",
    ...
  )
}

#' @name espn_nhl_calendar_offseason
NULL
#' @title **Get ESPN NHL Calendar Offseason Dates (core-v2)**
#' @rdname espn_nhl_calendar_offseason
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per offseason date.
#'   May return zero rows when the offseason calendar is empty for the
#'   requested season.
#'
#'    |col_name   |types     |description                                      |
#'    |:----------|:---------|:------------------------------------------------|
#'    |date       |character |Date (ISO 8601 datetime string).                 |
#'    |date_type  |character |Calendar date type.                              |
#'    |start_date |character |Season start date.                               |
#'    |end_date   |character |Season end date.                                 |
#'    |season     |integer   |Season year (echoed from arg).                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_calendar_offseason(season = 2026))
#' }
espn_nhl_calendar_offseason <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_calendar_offseason(league = "nhl", season = season, ...)
}


# ---------------------------------------------------------------------------
# 6c. espn_nhl_calendar_postseason
# ---------------------------------------------------------------------------

#' Internal: ESPN hockey calendar postseason (league-generic, core-v2)
#' Path: `seasons/{season}/types/3/calendar/postseason`
#' @noRd
.espn_hockey_calendar_postseason <- function(league = "nhl",
                                              season = most_recent_nhl_season(),
                                              ...) {
  path <- paste0("seasons/", season, "/types/3/calendar/postseason")
  .espn_hockey_calendar_dates(
    league     = league,
    path       = path,
    extra_cols = list(season = as.integer(season)),
    label      = "Calendar Postseason",
    ...
  )
}

#' @name espn_nhl_calendar_postseason
NULL
#' @title **Get ESPN NHL Calendar Postseason Dates (core-v2)**
#' @rdname espn_nhl_calendar_postseason
#' @author Saiem Gilani
#' @param season Season end-year (e.g. `2026`). Defaults to
#'   `most_recent_nhl_season()`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per postseason date.
#'   May return zero rows when postseason calendar is empty for the
#'   requested season.
#'
#'    |col_name   |types     |description                                      |
#'    |:----------|:---------|:------------------------------------------------|
#'    |date       |character |Date (ISO 8601 datetime string).                 |
#'    |date_type  |character |Calendar date type.                              |
#'    |start_date |character |Season start date.                               |
#'    |end_date   |character |Season end date.                                 |
#'    |season     |integer   |Season year (echoed from arg).                   |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_calendar_postseason(season = 2026))
#' }
espn_nhl_calendar_postseason <- function(season = most_recent_nhl_season(), ...) {
  .espn_hockey_calendar_postseason(league = "nhl", season = season, ...)
}


# ---------------------------------------------------------------------------
# 6d. espn_nhl_calendar_regular_season
# ---------------------------------------------------------------------------

#' @name espn_nhl_calendar_regular_season
NULL
#' @title **Get ESPN NHL Calendar Regular-Season Dates (core-v2)**
#' @rdname espn_nhl_calendar_regular_season
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per regular-season game
#'   date. Uses the core-v2 `calendar/whitelist` endpoint, which is the
#'   working equivalent of the sdv-py `calendar/regular-season` path (the
#'   `regular-season` path returns HTTP 400/503 for NHL; `whitelist` returns
#'   identical data with HTTP 200).
#'
#'    |col_name   |types     |description                                      |
#'    |:----------|:---------|:------------------------------------------------|
#'    |date       |character |Game date (ISO 8601 datetime string).            |
#'    |date_type  |character |Calendar date type (e.g. `"day"`).              |
#'    |start_date |character |Season start date.                               |
#'    |end_date   |character |Season end date.                                 |
#'
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_calendar_regular_season())
#' }
espn_nhl_calendar_regular_season <- function(...) {
  .espn_hockey_calendar_dates(
    league     = "nhl",
    path       = "calendar/whitelist",
    extra_cols = list(),
    label      = "Calendar Regular-Season",
    ...
  )
}
