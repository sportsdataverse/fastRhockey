# espn_nhl_player_detail.R
# ESPN NHL: Group C player-detail wrappers
#   espn_nhl_player_bio         -> site_v2 athletes/{id}/bio
#   espn_nhl_player_news        -> site_v2 athletes/{id}/news
#   espn_nhl_player_info        -> site_v2 athletes/{id}
#   espn_nhl_player_career_stats -> core_v2 athletes/{id}/statistics
#   espn_nhl_player_gamelog     -> web_v3  athletes/{id}/gamelog
#   espn_nhl_players_index      -> core_v2 athletes (paginated list)
#
# NOTE on site_v2 athlete endpoints (bio, info):
#   As of 2026-06-08, ESPN's site_v2 NHL athletes/{id} and athletes/{id}/bio
#   return HTTP 404. The wrappers are implemented for parity / forward-compat
#   and return a graceful empty data.frame() + cli_alert_warning when a 404
#   is encountered.
#
# NOTE on players_index:
#   sdv-py espn_nhl_players_index uses core_v2 /athletes (same backing endpoint
#   as espn_nhl_athletes_index). This wrapper provides a site_v2-named parity
#   alias with an identical implementation. Use espn_nhl_athletes_index for the
#   established function.


# ===========================================================================
# 1. Player Bio  (site_v2 athletes/{id}/bio)
# ===========================================================================

#' Internal: ESPN hockey player bio (league-generic, site_v2)
#' @noRd
.espn_hockey_player_bio <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id, "/bio")
      url  <- .espn_hockey_url("site_v2", path, league)
      resp <- .retry_request(url, ...)

      # Handle 404 gracefully before check_status() throws generically
      if (httr2::resp_status(resp) == 404L) {
        warning_msg <- "ESPN site_v2 athlete bio (athletes/[id]/bio) returns HTTP 404 for NHL; use espn_nhl_player_core() instead."
        cli::cli_alert_warning(warning_msg)
        return(result)
      }
      check_status(resp)

      raw <- .resp_json(resp, simplifyVector = FALSE)

      # The bio payload may carry top-level fields or an athlete sub-object.
      ath <- raw[["athlete"]] %||% raw

      pos   <- ath[["position"]] %||% list()
      team  <- ath[["team"]]     %||% list()
      bp    <- ath[["birthPlace"]] %||% list()
      links <- ath[["links"]]    %||% list()

      row <- list(
        athlete_id         = as.character(athlete_id),
        athlete_espn_id    = as.character(ath[["id"]]           %||% NA_character_),
        display_name       = as.character(ath[["displayName"]]  %||% NA_character_),
        short_name         = as.character(ath[["shortName"]]    %||% NA_character_),
        full_name          = as.character(ath[["fullName"]]     %||% NA_character_),
        first_name         = as.character(ath[["firstName"]]    %||% NA_character_),
        last_name          = as.character(ath[["lastName"]]     %||% NA_character_),
        jersey             = as.character(ath[["jersey"]]       %||% NA_character_),
        position           = as.character(pos[["abbreviation"]] %||% pos[["name"]] %||% NA_character_),
        age                = as.integer( ath[["age"]]           %||% NA_integer_),
        date_of_birth      = as.character(ath[["dateOfBirth"]]  %||% NA_character_),
        birth_city         = as.character(bp[["city"]]          %||% NA_character_),
        birth_state        = as.character(bp[["state"]]         %||% NA_character_),
        birth_country      = as.character(bp[["country"]]       %||% NA_character_),
        weight             = as.character(ath[["displayWeight"]] %||% ath[["weight"]] %||% NA_character_),
        height             = as.character(ath[["displayHeight"]] %||% ath[["height"]] %||% NA_character_),
        team_id            = as.character(team[["id"]]          %||% NA_character_),
        team_display_name  = as.character(team[["displayName"]] %||% NA_character_),
        team_abbreviation  = as.character(team[["abbreviation"]] %||% NA_character_),
        slug               = as.character(ath[["slug"]]         %||% NA_character_),
        debut_year         = as.integer( ath[["debutYear"]]     %||% NA_integer_),
        active             = as.logical( ath[["active"]]        %||% NA),
        description        = as.character(raw[["description"]]  %||% ath[["description"]] %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Bio data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player bio data for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player bio",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_bio
NULL
#' @title **Get ESPN NHL Player Bio**
#' @rdname espn_nhl_player_bio
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with athlete bio fields, or an
#'   empty `data.frame()` when ESPN returns HTTP 404. As of 2026-06-08,
#'   ESPN's `site_v2` NHL `athletes/{id}/bio` endpoint returns 404; use
#'   [espn_nhl_player_core()] for athlete metadata in the meantime.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_bio(athlete_id = "5149125"))
#' }
espn_nhl_player_bio <- function(athlete_id, ...) {
  .espn_hockey_player_bio(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 2. Player News  (site_v2 athletes/{id}/news)
# ===========================================================================

#' Internal: ESPN hockey player news (league-generic, site_v2)
#' @noRd
.espn_hockey_player_news <- function(league = "nhl", athlete_id, limit = 50, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path  <- paste0("athletes/", athlete_id, "/news")
      query <- list(limit = limit)
      raw   <- .espn_hockey_request("site_v2", path = path,
                                    query          = query,
                                    league         = league,
                                    simplifyVector = FALSE, ...)

      articles <- raw[["articles"]] %||% list()

      if (length(articles) == 0) {
        cli::cli_alert_warning(
          "No news articles found for athlete {athlete_id}."
        )
        return(result)
      }

      rows <- lapply(articles, function(a) {
        cats <- a[["categories"]] %||% list()
        # Extract first league/team/athlete category refs if present
        cat_league  <- NA_character_
        cat_team    <- NA_character_
        cat_athlete <- NA_character_
        for (cat in cats) {
          ctype <- cat[["type"]] %||% ""
          if (ctype == "league"  && is.na(cat_league))  cat_league  <- as.character(cat[["description"]] %||% cat[["id"]] %||% NA_character_)
          if (ctype == "team"    && is.na(cat_team))    cat_team    <- as.character(cat[["description"]] %||% cat[["id"]] %||% NA_character_)
          if (ctype == "athlete" && is.na(cat_athlete)) cat_athlete <- as.character(cat[["description"]] %||% cat[["id"]] %||% NA_character_)
        }

        imgs <- a[["images"]] %||% list()
        img_url <- if (length(imgs) > 0 && is.list(imgs[[1]])) imgs[[1]][["url"]] %||% NA_character_ else NA_character_

        list(
          athlete_id    = as.character(athlete_id),
          article_id    = as.character(a[["id"]]          %||% NA_character_),
          type          = as.character(a[["type"]]         %||% NA_character_),
          headline      = as.character(a[["headline"]]     %||% NA_character_),
          description   = as.character(a[["description"]]  %||% NA_character_),
          published     = as.character(a[["published"]]    %||% NA_character_),
          last_modified = as.character(a[["lastModified"]] %||% NA_character_),
          premium       = as.logical( a[["premium"]]       %||% NA),
          source        = as.character(a[["source"]]       %||% NA_character_),
          byline        = as.character(a[["byline"]]       %||% NA_character_),
          article_url   = as.character((a[["links"]] %||% list())[["web"]] %||%
                                        (a[["links"]] %||% list())[["mobile"]] %||% NA_character_),
          image_url     = img_url,
          category_league  = cat_league,
          category_team    = cat_team,
          category_athlete = cat_athlete
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player News data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player news for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player news",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_news
NULL
#' @title **Get ESPN NHL Player News**
#' @rdname espn_nhl_player_news
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param limit Maximum number of articles to return (default `50`).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_news(athlete_id = "5149125", limit = 10))
#' }
espn_nhl_player_news <- function(athlete_id, limit = 50, ...) {
  .espn_hockey_player_news(league = "nhl", athlete_id = athlete_id,
                            limit = limit, ...)
}


# ===========================================================================
# 3. Player Info  (site_v2 athletes/{id})
# ===========================================================================

#' Internal: ESPN hockey player info (league-generic, site_v2 athlete payload)
#' @noRd
.espn_hockey_player_info <- function(league = "nhl", athlete_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("athletes/", athlete_id)
      url  <- .espn_hockey_url("site_v2", path, league)
      resp <- .retry_request(url, ...)

      # Handle 404 gracefully before check_status() throws generically
      if (httr2::resp_status(resp) == 404L) {
        warning_msg <- "ESPN site_v2 athlete info (athletes/[id]) returns HTTP 404 for NHL; use espn_nhl_player_core() instead."
        cli::cli_alert_warning(warning_msg)
        return(result)
      }
      check_status(resp)

      raw <- .resp_json(resp, simplifyVector = FALSE)

      # site_v2 athlete payload may be flat or wrapped in athlete sub-object
      ath  <- raw[["athlete"]] %||% raw

      pos  <- ath[["position"]] %||% list()
      team <- ath[["team"]]     %||% list()

      headshot <- ath[["headshot"]] %||% list()
      hs_href  <- headshot[["href"]] %||% NA_character_

      row <- list(
        athlete_id         = as.character(athlete_id),
        athlete_espn_id    = as.character(ath[["id"]]            %||% NA_character_),
        uid                = as.character(ath[["uid"]]            %||% NA_character_),
        guid               = as.character(ath[["guid"]]           %||% NA_character_),
        display_name       = as.character(ath[["displayName"]]   %||% NA_character_),
        short_name         = as.character(ath[["shortName"]]     %||% NA_character_),
        full_name          = as.character(ath[["fullName"]]      %||% NA_character_),
        first_name         = as.character(ath[["firstName"]]     %||% NA_character_),
        last_name          = as.character(ath[["lastName"]]      %||% NA_character_),
        jersey             = as.character(ath[["jersey"]]        %||% NA_character_),
        slug               = as.character(ath[["slug"]]          %||% NA_character_),
        position           = as.character(pos[["abbreviation"]]  %||% pos[["name"]] %||% NA_character_),
        position_name      = as.character(pos[["name"]]          %||% NA_character_),
        age                = as.integer( ath[["age"]]            %||% NA_integer_),
        date_of_birth      = as.character(ath[["dateOfBirth"]]   %||% NA_character_),
        debut_year         = as.integer( ath[["debutYear"]]      %||% NA_integer_),
        weight             = as.character(ath[["displayWeight"]] %||% ath[["weight"]] %||% NA_character_),
        height             = as.character(ath[["displayHeight"]] %||% ath[["height"]] %||% NA_character_),
        team_id            = as.character(team[["id"]]           %||% NA_character_),
        team_display_name  = as.character(team[["displayName"]]  %||% NA_character_),
        team_abbreviation  = as.character(team[["abbreviation"]] %||% NA_character_),
        headshot_href      = as.character(hs_href),
        active             = as.logical( ath[["active"]]         %||% NA),
        status             = as.character(ath[["status"]]        %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Info data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player info for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player info",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_info
NULL
#' @title **Get ESPN NHL Player Info**
#' @rdname espn_nhl_player_info
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with athlete metadata from the
#'   `site_v2` athletes endpoint, or an empty `data.frame()` when ESPN returns
#'   HTTP 404. As of 2026-06-08, ESPN's `site_v2` NHL `athletes/{id}` endpoint
#'   returns 404; use [espn_nhl_player_core()] for athlete metadata instead.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_info(athlete_id = "5149125"))
#' }
espn_nhl_player_info <- function(athlete_id, ...) {
  .espn_hockey_player_info(league = "nhl", athlete_id = athlete_id, ...)
}


# ===========================================================================
# 4. Player Career Stats  (core_v2 athletes/{id}/statistics)
# ===========================================================================

#' Internal: ESPN hockey player career stats (league-generic, core_v2)
#'
#' Wraps `core_v2 athletes/{id}/statistics`. Returns all split categories
#' flattened into a single wide tibble row (one row per athlete). This mirrors
#' [espn_nhl_player_statistics()] which uses the same endpoint but this
#' function preserves the `stat_type` optional path-segment per sdv-py parity.
#'
#' @param league ESPN league slug.
#' @param athlete_id ESPN athlete identifier.
#' @param stat_type Optional integer path segment (e.g. `0`). When `NULL`
#'   the base `/statistics` path is used (ESPN redirects to `/statistics/0`).
#' @param ... Passed through.
#' @return A one-row `fastRhockey_data` tibble (wide).
#' @noRd
.espn_hockey_player_career_stats <- function(league = "nhl", athlete_id,
                                              stat_type = NULL, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      seg  <- if (!is.null(stat_type)) paste0("/", stat_type) else ""
      path <- paste0("athletes/", athlete_id, "/statistics", seg)
      raw  <- .espn_hockey_request("core_v2", path = path, league = league,
                                   simplifyVector = TRUE, ...)

      splits <- raw$splits %||% list()
      cats   <- splits$categories

      if (is.null(cats) || (is.data.frame(cats) && nrow(cats) == 0) ||
          (is.list(cats) && length(cats) == 0)) {
        cli::cli_alert_warning(
          "No career-stats categories found for athlete {athlete_id}."
        )
        return(result)
      }

      wide <- list(
        athlete_id   = as.character(athlete_id),
        split_id     = as.character(splits$id           %||% NA_character_),
        split_name   = as.character(splits$name         %||% NA_character_),
        split_type   = as.character(splits$type         %||% NA_character_),
        split_abbr   = as.character(splits$abbreviation %||% NA_character_)
      )

      # cats can be a data.frame (simplifyVector=TRUE) or a list
      cat_list <- if (is.data.frame(cats)) {
        lapply(seq_len(nrow(cats)), function(i) cats[i, , drop = FALSE])
      } else {
        cats
      }

      for (cat_row in cat_list) {
        cat_name  <- if (is.data.frame(cat_row)) cat_row$name[1] else cat_row[["name"]]
        stats_df  <- if (is.data.frame(cat_row)) cat_row$stats[[1]] else cat_row[["stats"]]
        if (!is.data.frame(stats_df) || nrow(stats_df) == 0) next
        for (j in seq_len(nrow(stats_df))) {
          sname <- stats_df$name[j] %||% paste0("stat_", j)
          col   <- paste0(janitor::make_clean_names(cat_name %||% ""), "_",
                          janitor::make_clean_names(sname))
          wide[[col]] <- stats_df$value[j] %||% NA_real_
        }
      }

      result <- as.data.frame(wide, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Career Stats data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player career stats for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player career stats",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_career_stats
NULL
#' @title **Get ESPN NHL Player Career Stats (core-v2)**
#' @rdname espn_nhl_player_career_stats
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param stat_type Optional integer stat-type path segment (default `NULL`,
#'   which uses the base `/statistics` path). ESPN redirects the base path to
#'   `/statistics/0` (All Splits). Pass `0` explicitly for identical behaviour.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_career_stats(athlete_id = "5149125"))
#' }
espn_nhl_player_career_stats <- function(athlete_id, stat_type = NULL, ...) {
  .espn_hockey_player_career_stats(league     = "nhl",
                                    athlete_id = athlete_id,
                                    stat_type  = stat_type, ...)
}


# ===========================================================================
# 5. Player Gamelog  (web_v3 athletes/{id}/gamelog)
# ===========================================================================

#' Internal: ESPN hockey player gamelog (league-generic, web_v3)
#'
#' Returns per-game statistics from the web_v3 gamelog endpoint. One row per
#' game event per athlete, joining game metadata from the top-level `events`
#' array with per-game stat values from `seasonTypes[].categories[].events[]`.
#'
#' **NOTE**: ESPN's web-v3 gamelog was not available for NHL in earlier
#' package versions; as of 2026-06-08 the endpoint returns data for skaters.
#' If the endpoint returns HTTP 404 in the future, an empty `data.frame()` and
#' `cli_alert_warning` are returned. Use [espn_nhl_player_statisticslog()] as
#' the primary game-log source (core-v2, more reliable).
#'
#' @param league ESPN league slug.
#' @param athlete_id ESPN athlete identifier.
#' @param season Season end-year (e.g. `2025`). When `NULL` the current season
#'   is used by ESPN.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per game (may be empty).
#' @noRd
.espn_hockey_player_gamelog <- function(league = "nhl", athlete_id,
                                         season = NULL, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path  <- paste0("athletes/", athlete_id, "/gamelog")
      query <- list()
      if (!is.null(season)) query[["season"]] <- season

      url  <- .espn_hockey_url("web_v3", path, league)
      resp_gl <- .retry_request(url, params = query, ...)

      # Handle 404 gracefully before check_status() throws generically
      if (httr2::resp_status(resp_gl) == 404L) {
        warning_msg <- paste0("ESPN web-v3 gamelog returned HTTP 404 for NHL athlete ",
                              athlete_id, "; use espn_nhl_player_statisticslog() instead.")
        cli::cli_alert_warning(warning_msg)
        return(result)
      }
      check_status(resp_gl)

      raw <- .resp_json(resp_gl, simplifyVector = FALSE)

      # Build a lookup: eventId -> game metadata from top-level events array
      events_list <- raw[["events"]] %||% list()
      event_meta  <- list()
      for (ev in events_list) {
        eid <- as.character(ev[["id"]] %||% "")
        if (nzchar(eid)) {
          opp  <- ev[["opponent"]] %||% list()
          team <- ev[["team"]]     %||% list()
          event_meta[[eid]] <- list(
            game_date           = as.character(ev[["gameDate"]]      %||% NA_character_),
            at_vs               = as.character(ev[["atVs"]]          %||% NA_character_),
            score               = as.character(ev[["score"]]         %||% NA_character_),
            home_team_id        = as.character(ev[["homeTeamId"]]    %||% NA_character_),
            away_team_id        = as.character(ev[["awayTeamId"]]    %||% NA_character_),
            home_team_score     = as.character(ev[["homeTeamScore"]] %||% NA_character_),
            away_team_score     = as.character(ev[["awayTeamScore"]] %||% NA_character_),
            game_result         = as.character(ev[["gameResult"]]    %||% NA_character_),
            opponent_id         = as.character(opp[["id"]]           %||% NA_character_),
            opponent_name       = as.character(opp[["displayName"]]  %||% NA_character_),
            opponent_abbr       = as.character(opp[["abbreviation"]] %||% NA_character_),
            team_id             = as.character(team[["id"]]          %||% NA_character_),
            team_abbr           = as.character(team[["abbreviation"]] %||% NA_character_),
            league_name         = as.character(ev[["leagueName"]]    %||% NA_character_),
            league_abbreviation = as.character(ev[["leagueAbbreviation"]] %||% NA_character_)
          )
        }
      }

      names_vec <- unlist(raw[["names"]] %||% list())

      season_types <- raw[["seasonTypes"]] %||% list()
      if (length(season_types) == 0 && length(events_list) == 0) {
        cli::cli_alert_warning(
          "No gamelog data found for athlete {athlete_id}."
        )
        return(result)
      }

      rows <- list()
      for (st in season_types) {
        st_display <- st[["displayName"]] %||% NA_character_
        cats <- st[["categories"]] %||% list()
        for (cat in cats) {
          cat_display  <- cat[["displayName"]] %||% NA_character_
          cat_type     <- cat[["type"]]        %||% NA_character_
          cat_split    <- cat[["splitType"]]   %||% NA_character_
          cat_events   <- cat[["events"]]      %||% list()
          for (ce in cat_events) {
            eid       <- as.character(ce[["eventId"]] %||% "")
            stats_raw <- unlist(ce[["stats"]] %||% list())
            meta      <- event_meta[[eid]] %||% list()

            row <- c(
              list(
                athlete_id          = as.character(athlete_id),
                event_id            = eid,
                season_type_display = as.character(st_display),
                category_display    = as.character(cat_display),
                category_type       = as.character(cat_type),
                category_split_type = as.character(cat_split)
              ),
              meta
            )

            if (length(names_vec) > 0 && length(stats_raw) > 0) {
              n <- min(length(names_vec), length(stats_raw))
              for (i in seq_len(n)) {
                row[[ janitor::make_clean_names(names_vec[i]) ]] <- stats_raw[i]
              }
            }
            rows <- c(rows, list(row))
          }
        }
      }

      if (length(rows) == 0) {
        cli::cli_alert_warning(
          "No gamelog rows parsed for athlete {athlete_id}."
        )
        return(result)
      }

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Player Gamelog data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player gamelog for athlete {athlete_id} available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player gamelog",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_player_gamelog
NULL
#' @title **Get ESPN NHL Player Gamelog**
#' @rdname espn_nhl_player_gamelog
#' @author Saiem Gilani
#' @param athlete_id ESPN athlete identifier (character or numeric,
#'   e.g. `"5149125"`). Obtain via [espn_nhl_team_roster()].
#' @param season Season end-year (e.g. `2025`). When `NULL` ESPN returns the
#'   current season. Defaults to `NULL`.
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per game, or an empty
#'   `data.frame()` when no data is available (e.g. if the endpoint returns
#'   HTTP 404 in a future ESPN schema change). If the endpoint becomes
#'   unavailable, use [espn_nhl_player_statisticslog()] from the core-v2
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_gamelog(athlete_id = "5149125"))
#' }
espn_nhl_player_gamelog <- function(athlete_id, season = NULL, ...) {
  .espn_hockey_player_gamelog(league     = "nhl",
                               athlete_id = athlete_id,
                               season     = season, ...)
}


# ===========================================================================
# 6. Players Index  (site_v2 / core_v2 athletes list)
# ===========================================================================

#' Internal: ESPN hockey players index, site_v2 parity alias (league-generic)
#'
#' sdv-py `espn_nhl_players_index` uses `core_v2 /athletes` (same backing
#' endpoint as [espn_nhl_athletes_index()]). This wrapper provides a
#' `players_index`-named alias for parity.
#'
#' @param league ESPN league slug.
#' @param active Filter by active status. `TRUE` (default) = active players
#'   only; `FALSE` = inactive; `NULL` = no filter.
#' @param limit Athletes per page (default `100`; max ~200).
#' @param page Page number (default `1`).
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per athlete.
#' @noRd
.espn_hockey_players_index <- function(league = "nhl",
                                        active = TRUE,
                                        limit  = 100,
                                        page   = 1,
                                        ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      query <- list(
        active = if (isTRUE(active)) "true" else if (isFALSE(active)) "false" else NULL,
        limit  = limit,
        page   = page
      )
      raw <- .espn_hockey_request("core_v2", path = "athletes",
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
          paste0(toupper(league), " Players Index data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} players index data available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} players index",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

# ---------------------------------------------------------------------------
# Public shim
# ---------------------------------------------------------------------------

#' @name espn_nhl_players_index
NULL
#' @title **Get ESPN NHL Players Index**
#' @rdname espn_nhl_players_index
#' @author Saiem Gilani
#' @description Returns a paginated index of NHL athletes from ESPN. This is a
#'   site_v2-named parity alias for [espn_nhl_athletes_index()]; both wrap the
#'   same `core_v2 /athletes` backing endpoint. Prefer
#'   [espn_nhl_athletes_index()] for the established function with the same
#'   interface.
#' @param active Filter by active status. `TRUE` (default) = active players
#'   only; `FALSE` = inactive only; `NULL` = no filter.
#' @param limit Number of athletes per page (default `100`; max ~200).
#' @param page Page number (default `1`). Use the `page_count` column in the
#'   result to determine the total number of pages.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @seealso [espn_nhl_athletes_index()] for the established alias.
#' @examples
#' \donttest{
#'   try(espn_nhl_players_index(active = TRUE, limit = 10, page = 1))
#' }
espn_nhl_players_index <- function(active = TRUE,
                                    limit  = 100,
                                    page   = 1,
                                    ...) {
  .espn_hockey_players_index(league = "nhl",
                              active = active,
                              limit  = limit,
                              page   = page,
                              ...)
}
