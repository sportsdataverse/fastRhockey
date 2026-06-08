# espn_nhl_league.R
# ESPN NHL: league-level endpoints (Group A parity with sdv-py)
# Endpoints: news, injuries, transactions, conferences, statistics_league,
#            draft, games, awards, award, tournaments, talentpicks, league_notes
#
# Live verification notes (2026-06-08):
#   news              -> 14+ articles (site_v2)
#   injuries          -> 28 teams with nested player injuries (site_v2)
#   transactions      -> 25 transactions (site_v2)
#   conferences       -> 2 conferences + children (site_v2/groups)
#   statistics_league -> 4 stat categories (site_v2/statistics) - sparse structure
#   draft             -> HTTP 500 for NHL (site_v2/draft) - graceful empty
#   games             -> collection, 1 current event (core_v2/events)
#   awards            -> 34 award refs (core_v2/awards)
#   award             -> HTTP 500 for all tested IDs (core_v2/awards/{id}) - graceful
#   tournaments       -> 1 tournament ref (core_v2/tournaments)
#   talentpicks       -> count=0 (core_v2/talentpicks) - graceful empty
#   league_notes      -> count=0 (core_v2/notes) - graceful empty


# ===========================================================================
# 1. News
# ===========================================================================

#' Internal: ESPN hockey news (league-generic, site_v2)
#' @noRd
.espn_hockey_news <- function(league = "nhl", limit = 50, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "news",
                                  query  = list(limit = limit),
                                  league = league, ...)

      articles <- raw[["articles"]]
      if (is.null(articles) || (is.data.frame(articles) && nrow(articles) == 0) ||
          (is.list(articles) && length(articles) == 0)) {
        return(result)
      }

      if (!is.data.frame(articles)) {
        stop("Unexpected articles format", call. = FALSE)
      }

      # Extract scalar columns; skip deeply-nested list columns
      rows <- lapply(seq_len(nrow(articles)), function(i) {
        a <- articles[i, , drop = FALSE]
        links_df <- if (is.data.frame(a$links)) a$links else data.frame()
        list(
          article_id        = as.character(a$id             %||% NA_character_),
          now_id            = as.character(a$nowId          %||% NA_character_),
          content_key       = as.character(a$contentKey     %||% NA_character_),
          type              = as.character(a$type           %||% NA_character_),
          headline          = as.character(a$headline       %||% NA_character_),
          description       = as.character(a$description    %||% NA_character_),
          last_modified     = as.character(a$lastModified   %||% NA_character_),
          published         = as.character(a$published      %||% NA_character_),
          byline            = as.character(a$byline         %||% NA_character_),
          premium           = if (!is.null(a$premium)) as.logical(a$premium) else NA,
          # First image href if images list available
          image_href        = tryCatch({
            imgs <- a$images[[1]]
            if (is.data.frame(imgs) && "url" %in% colnames(imgs)) as.character(imgs$url[1])
            else if (is.list(imgs) && length(imgs) > 0) as.character(imgs[[1]]$url %||% NA_character_)
            else NA_character_
          }, error = function(e) NA_character_),
          # Web link href
          web_href          = tryCatch({
            if (is.data.frame(links_df) && "web" %in% colnames(links_df) &&
                is.data.frame(links_df$web)) {
              as.character(links_df$web$href[1] %||% NA_character_)
            } else NA_character_
          }, error = function(e) NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " News data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} news available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} news",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_news
NULL
#' @title **Get ESPN NHL News**
#' @rdname espn_nhl_news
#' @author Saiem Gilani
#' @param limit Maximum number of articles to return (default `50`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per article:
#'
#'    |col_name       |types     |description                                         |
#'    |:--------------|:---------|:---------------------------------------------------|
#'    |article_id     |character |ESPN article identifier.                            |
#'    |now_id         |character |ESPN Now identifier.                                |
#'    |content_key    |character |Content management key.                             |
#'    |type           |character |Article type (e.g. "Story", "Media").               |
#'    |headline       |character |Article headline.                                   |
#'    |description    |character |Article description/excerpt.                        |
#'    |last_modified  |character |ISO 8601 last-modified datetime.                    |
#'    |published      |character |ISO 8601 published datetime.                        |
#'    |byline         |character |Author byline.                                      |
#'    |premium        |logical   |Whether the article is premium content.             |
#'    |image_href     |character |URL of the lead image.                              |
#'    |web_href       |character |URL of the article on ESPN.com.                     |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_news(limit = 10))
#' }
espn_nhl_news <- function(limit = 50, ...) {
  .espn_hockey_news(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 2. Injuries
# ===========================================================================

#' Internal: ESPN hockey injuries (league-generic, site_v2)
#' @noRd
.espn_hockey_injuries <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "injuries",
                                  league = league,
                                  simplifyVector = FALSE, ...)

      injuries_top <- raw[["injuries"]] %||% list()
      if (length(injuries_top) == 0) return(result)

      rows <- list()
      for (team_entry in injuries_top) {
        team_id_val   <- as.character(team_entry$id          %||% NA_character_)
        team_name_val <- as.character(team_entry$displayName %||% NA_character_)
        players       <- team_entry$injuries %||% list()

        for (pl in players) {
          ath  <- pl$athlete %||% list()
          typ  <- pl$type    %||% list()
          dtls <- pl$details %||% list()
          rows <- c(rows, list(list(
            team_id            = team_id_val,
            team_display_name  = team_name_val,
            player_id          = as.character(ath$id          %||% NA_character_),
            player_display_name= as.character(ath$displayName %||% NA_character_),
            player_short_name  = as.character(ath$shortName   %||% NA_character_),
            player_position    = as.character(
              if (!is.null(ath$position) && is.list(ath$position))
                ath$position$abbreviation %||% NA_character_
              else NA_character_
            ),
            injury_id          = as.character(pl$id           %||% NA_character_),
            injury_status      = as.character(pl$status       %||% NA_character_),
            injury_date        = as.character(pl$date         %||% NA_character_),
            injury_short_comment  = as.character(pl$shortComment %||% NA_character_),
            injury_long_comment   = as.character(pl$longComment  %||% NA_character_),
            injury_type        = as.character(typ$name         %||% NA_character_),
            injury_abbreviation= as.character(typ$abbreviation %||% NA_character_),
            detail_side        = as.character(dtls$side        %||% NA_character_),
            detail_fantasy_status = as.character(dtls$fantasyStatus %||% NA_character_),
            detail_return_date = as.character(dtls$returnDate  %||% NA_character_)
          )))
        }
      }

      if (length(rows) == 0) return(result)

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Injuries data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} injuries available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} injuries",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_injuries
NULL
#' @title **Get ESPN NHL Injuries**
#' @rdname espn_nhl_injuries
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_injuries())
#' }
espn_nhl_injuries <- function(...) {
  .espn_hockey_injuries(league = "nhl", ...)
}


# ===========================================================================
# 3. Transactions
# ===========================================================================

#' Internal: ESPN hockey transactions (league-generic, site_v2)
#' @noRd
.espn_hockey_transactions <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "transactions",
                                  league = league,
                                  simplifyVector = FALSE, ...)

      txns <- raw[["transactions"]] %||% list()
      if (length(txns) == 0) return(result)

      rows <- lapply(txns, function(tx) {
        team <- tx$team %||% list()
        list(
          date              = as.character(tx$date        %||% NA_character_),
          description       = as.character(tx$description %||% NA_character_),
          team_id           = as.character(team$id           %||% NA_character_),
          team_location     = as.character(team$location     %||% NA_character_),
          team_name         = as.character(team$name         %||% NA_character_),
          team_abbreviation = as.character(team$abbreviation %||% NA_character_),
          team_display_name = as.character(team$displayName  %||% NA_character_),
          team_color        = as.character(team$color        %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Transactions data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} transactions available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} transactions",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_transactions
NULL
#' @title **Get ESPN NHL Transactions**
#' @rdname espn_nhl_transactions
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_transactions())
#' }
espn_nhl_transactions <- function(...) {
  .espn_hockey_transactions(league = "nhl", ...)
}


# ===========================================================================
# 4. Conferences (groups)
# ===========================================================================

#' Internal: ESPN hockey conferences/groups (league-generic, site_v2)
#' @noRd
.espn_hockey_conferences <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "groups",
                                  league = league,
                                  simplifyVector = FALSE, ...)

      groups <- raw[["groups"]] %||% list()
      if (length(groups) == 0) return(result)

      rows <- list()
      for (g in groups) {
        conf_name  <- as.character(g$name         %||% NA_character_)
        conf_abbr  <- as.character(g$abbreviation %||% NA_character_)
        children   <- g$children %||% list()

        if (length(children) > 0) {
          for (div in children) {
            rows <- c(rows, list(list(
              conference_name         = conf_name,
              conference_abbreviation = conf_abbr,
              division_name           = as.character(div$name         %||% NA_character_),
              division_abbreviation   = as.character(div$abbreviation %||% NA_character_)
            )))
          }
        } else {
          rows <- c(rows, list(list(
            conference_name         = conf_name,
            conference_abbreviation = conf_abbr,
            division_name           = NA_character_,
            division_abbreviation   = NA_character_
          )))
        }
      }

      if (length(rows) == 0) return(result)

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Conferences data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} conferences available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} conferences",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_conferences
NULL
#' @title **Get ESPN NHL Conferences**
#' @rdname espn_nhl_conferences
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per conference/division
#'   combination (4 rows for NHL: 2 conferences x 2 divisions each):
#'
#'    |col_name                 |types     |description                             |
#'    |:------------------------|:---------|:---------------------------------------|
#'    |conference_name          |character |Conference name (e.g. "Eastern Conference"). |
#'    |conference_abbreviation  |character |Conference abbreviation.                |
#'    |division_name            |character |Division name (e.g. "Atlantic Division"). |
#'    |division_abbreviation    |character |Division abbreviation.                  |
#'
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_conferences())
#' }
espn_nhl_conferences <- function(...) {
  .espn_hockey_conferences(league = "nhl", ...)
}


# ===========================================================================
# 5. League Statistics
# ===========================================================================

#' Internal: ESPN hockey league statistics (league-generic, site_v2)
#' @noRd
.espn_hockey_statistics_league <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      # simplifyVector=TRUE collapses the stats list to a named list (not list-of-lists)
      # when ESPN returns a single stat-group. Use simplifyVector=FALSE for safe iteration.
      raw <- .espn_hockey_request("site_v2", "statistics",
                                  league = league,
                                  simplifyVector = TRUE, ...)

      stats_raw <- raw[["stats"]]
      if (is.null(stats_raw)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} statistics endpoint returned no stat categories."
        )
        return(result)
      }

      # stats may arrive as:
      #   (a) a list with $id, $name, $abbreviation, $categories (single group)
      #   (b) a list-of-lists (multiple groups) — normalize both to a list-of-groups
      is_single <- is.list(stats_raw) && !is.null(stats_raw$categories)
      groups <- if (is_single) list(stats_raw) else stats_raw

      rows <- list()
      for (grp in groups) {
        if (!is.list(grp)) next
        cats_df <- grp$categories
        if (is.null(cats_df) || (is.data.frame(cats_df) && nrow(cats_df) == 0)) next

        for (ci in seq_len(nrow(cats_df))) {
          cat_row  <- cats_df[ci, , drop = FALSE]
          cat_name <- as.character(cat_row$name         %||% NA_character_)
          cat_abbr <- as.character(cat_row$abbreviation %||% NA_character_)

          leaders_df <- if (is.list(cat_row$leaders) && length(cat_row$leaders) > 0)
            cat_row$leaders[[1]] else data.frame()

          if (!is.data.frame(leaders_df) || nrow(leaders_df) == 0) {
            rows <- c(rows, list(list(
              category_name         = cat_name,
              category_abbreviation = cat_abbr,
              display_value         = NA_character_,
              value                 = NA_real_,
              athlete_id            = NA_character_,
              athlete_display_name  = NA_character_,
              team_id               = NA_character_,
              team_abbreviation     = NA_character_
            )))
            next
          }

          ath_ids  <- if (is.data.frame(leaders_df$athlete) &&
                          "id" %in% colnames(leaders_df$athlete))
            as.character(leaders_df$athlete$id) else rep(NA_character_, nrow(leaders_df))
          ath_dns  <- if (is.data.frame(leaders_df$athlete) &&
                          "displayName" %in% colnames(leaders_df$athlete))
            as.character(leaders_df$athlete$displayName) else rep(NA_character_, nrow(leaders_df))
          team_ids <- if (is.data.frame(leaders_df$team) &&
                          "id" %in% colnames(leaders_df$team))
            as.character(leaders_df$team$id) else rep(NA_character_, nrow(leaders_df))
          team_abbrs <- if (is.data.frame(leaders_df$team) &&
                            "abbreviation" %in% colnames(leaders_df$team))
            as.character(leaders_df$team$abbreviation) else rep(NA_character_, nrow(leaders_df))

          for (li in seq_len(nrow(leaders_df))) {
            rows <- c(rows, list(list(
              category_name         = cat_name,
              category_abbreviation = cat_abbr,
              display_value         = as.character(leaders_df$displayValue[[li]] %||% NA_character_),
              value                 = as.numeric(leaders_df$value[[li]]           %||% NA_real_),
              athlete_id            = ath_ids[li],
              athlete_display_name  = ath_dns[li],
              team_id               = team_ids[li],
              team_abbreviation     = team_abbrs[li]
            )))
          }
        }
      }

      if (length(rows) == 0) return(result)

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " League Statistics data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} league statistics available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} league statistics",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_statistics_league
NULL
#' @title **Get ESPN NHL League Statistics**
#' @rdname espn_nhl_statistics_league
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per (stat category, leader).
#'   The ESPN NHL statistics endpoint returns a sparse set of top-of-league
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_statistics_league())
#' }
espn_nhl_statistics_league <- function(...) {
  .espn_hockey_statistics_league(league = "nhl", ...)
}


# ===========================================================================
# 6. Draft (site_v2 — HTTP 500 for NHL; graceful empty)
# ===========================================================================

#' Internal: ESPN hockey league draft (league-generic, site_v2)
#' @noRd
.espn_hockey_draft <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("site_v2", "draft",
                                  league = league,
                                  simplifyVector = FALSE, ...)

      items <- raw[["items"]] %||% list()
      if (length(items) == 0) return(result)

      rows <- lapply(items, function(it) {
        list(
          id          = as.character(it$id   %||% NA_character_),
          name        = as.character(it$name %||% NA_character_)
        )
      })

      result <- dplyr::bind_rows(lapply(rows, function(r) {
        as.data.frame(r, stringsAsFactors = FALSE)
      })) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Draft data from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) {
      cli::cli_alert_warning(
        "ESPN {toupper(league)} draft endpoint is not available (HTTP 500 is normal for NHL)."
      )
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} draft",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_draft
NULL
#' @title **Get ESPN NHL Draft**
#' @rdname espn_nhl_draft
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with draft data, or an empty tibble
#'   when the endpoint is unavailable. **Note**: The NHL draft endpoint
#'   (`site/v2/sports/hockey/nhl/draft`) currently returns HTTP 500 for NHL —
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN returns HTTP 500 for this endpoint)
#'   try(espn_nhl_draft())
#' }
espn_nhl_draft <- function(...) {
  .espn_hockey_draft(league = "nhl", ...)
}


# ===========================================================================
# 7. Games (core_v2 events collection)
# ===========================================================================

#' Internal: ESPN hockey games collection (league-generic, core_v2)
#' @noRd
.espn_hockey_games <- function(league = "nhl", dates = NULL, limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      query <- list(limit = limit)
      if (!is.null(dates)) query[["dates"]] <- dates

      raw <- .espn_hockey_request("core_v2", "events",
                                  query  = query,
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "event_id")
      tbl$event_id   <- as.character(tbl$event_id)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)
      if (!is.null(dates)) tbl$dates <- as.character(dates)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Games data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} games available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} games",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_games
NULL
#' @title **Get ESPN NHL Games (core-v2)**
#' @rdname espn_nhl_games
#' @author Saiem Gilani
#' @param dates Optional date string `"YYYYMMDD"` or range `"YYYYMMDD-YYYYMMDD"`.
#'   When `NULL` (default) the most recent games are returned.
#' @param limit Maximum number of events per page (default `100`).
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_games(limit = 10))
#' }
espn_nhl_games <- function(dates = NULL, limit = 100, ...) {
  .espn_hockey_games(league = "nhl", dates = dates, limit = limit, ...)
}


# ===========================================================================
# 8. Awards (core_v2 collection)
# ===========================================================================

#' Internal: ESPN hockey awards collection (league-generic, core_v2)
#' @noRd
.espn_hockey_awards <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", "awards",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "award_id")
      tbl$award_id   <- as.character(tbl$award_id)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Awards data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} awards available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} awards",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_awards
NULL
#' @title **Get ESPN NHL Awards (core-v2)**
#' @rdname espn_nhl_awards
#' @author Saiem Gilani
#' @param limit Maximum number of awards to return per page (default `100`).
#'   The NHL has 34 awards; `100` returns all in one request.
#' @param ... Reserved for forward compatibility.
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_awards())
#' }
espn_nhl_awards <- function(limit = 100, ...) {
  .espn_hockey_awards(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 9. Award (singular — core_v2; HTTP 500 for NHL; graceful empty)
# ===========================================================================

#' Internal: ESPN hockey single award (league-generic, core_v2)
#' @noRd
.espn_hockey_award <- function(league = "nhl", award_id, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      path <- paste0("awards/", award_id)
      raw  <- .espn_hockey_request("core_v2", path,
                                   league = league,
                                   simplifyVector = TRUE, ...)

      row <- list(
        award_id    = as.character(award_id),
        id          = as.character(raw$id          %||% NA_character_),
        name        = as.character(raw$name        %||% NA_character_),
        display_name = as.character(raw$displayName %||% NA_character_),
        description = as.character(raw$description %||% NA_character_),
        award_ref   = as.character(raw[["$ref"]]   %||% NA_character_)
      )

      result <- as.data.frame(row, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Award data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      cli::cli_alert_warning(
        paste0("ESPN ", toupper(league), " award ", award_id,
               " is not available (HTTP 500 is normal for NHL individual award endpoints).")
      )
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} award {award_id}",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_award
NULL
#' @title **Get ESPN NHL Award (core-v2)**
#' @rdname espn_nhl_award
#' @author Saiem Gilani
#' @param award_id ESPN award identifier (character or numeric). Use
#'   `espn_nhl_awards()$award_id` to discover valid award ids.
#' @param ... Reserved for forward compatibility.
#' @return A one-row `fastRhockey_data` tibble with award metadata, or an
#'   empty tibble when the endpoint is unavailable. **Note**: The individual
#'   award endpoint (`core_v2/awards/{award_id}`) currently returns HTTP 500
#'   for all tested NHL award ids — an empty tibble is returned gracefully:
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN returns HTTP 500 for this endpoint)
#'   try({
#'     awards <- espn_nhl_awards()
#'     if (nrow(awards) > 0) espn_nhl_award(award_id = awards$award_id[1])
#'   })
#' }
espn_nhl_award <- function(award_id, ...) {
  .espn_hockey_award(league = "nhl", award_id = award_id, ...)
}


# ===========================================================================
# 10. Tournaments (core_v2 collection — sparse for NHL)
# ===========================================================================

#' Internal: ESPN hockey tournaments collection (league-generic, core_v2)
#' @noRd
.espn_hockey_tournaments <- function(league = "nhl", limit = 100, ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", "tournaments",
                                  query  = list(limit = limit),
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} tournaments endpoint returned no items."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "tournament_id")
      tbl$tournament_id <- as.character(tbl$tournament_id)
      tbl$count         <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count    <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Tournaments data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} tournaments available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} tournaments",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_tournaments
NULL
#' @title **Get ESPN NHL Tournaments (core-v2)**
#' @rdname espn_nhl_tournaments
#' @author Saiem Gilani
#' @param limit Maximum number of tournaments to return (default `100`).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per tournament reference.
#'   The NHL tournaments endpoint is sparsely populated (typically 1 entry):
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_tournaments())
#' }
espn_nhl_tournaments <- function(limit = 100, ...) {
  .espn_hockey_tournaments(league = "nhl", limit = limit, ...)
}


# ===========================================================================
# 11. Talent Picks (core_v2 — count=0 for NHL; graceful empty)
# ===========================================================================

#' Internal: ESPN hockey talent picks (league-generic, core_v2)
#' @noRd
.espn_hockey_talentpicks <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", "talentpicks",
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} talentpicks endpoint returned no items (count=0 is normal for NHL)."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "pick_id")
      tbl$pick_id    <- as.character(tbl$pick_id)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " Talent Picks data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} talent picks available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} talent picks",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_talentpicks
NULL
#' @title **Get ESPN NHL Talent Picks (core-v2)**
#' @rdname espn_nhl_talentpicks
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per talent pick reference,
#'   or an empty tibble when the endpoint is unavailable. **Note**: The NHL
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
#'   try(espn_nhl_talentpicks())
#' }
espn_nhl_talentpicks <- function(...) {
  .espn_hockey_talentpicks(league = "nhl", ...)
}


# ===========================================================================
# 12. League Notes (core_v2 — count=0 for NHL; graceful empty)
# ===========================================================================

#' Internal: ESPN hockey league notes (league-generic, core_v2)
#' @noRd
.espn_hockey_league_notes <- function(league = "nhl", ...) {
  .args <- .capture_args()

  result <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_request("core_v2", "notes",
                                  league = league,
                                  simplifyVector = TRUE, ...)

      items <- raw$items
      if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
          (is.list(items) && length(items) == 0)) {
        cli::cli_alert_warning(
          "ESPN {toupper(league)} league notes endpoint returned no items (count=0 is normal for NHL)."
        )
        return(result)
      }

      tbl <- .espn_core_collection(items, id_col = "note_id")
      tbl$note_id    <- as.character(tbl$note_id)
      tbl$count      <- as.integer(raw$count      %||% NA_integer_)
      tbl$page_count <- as.integer(raw$pageCount  %||% NA_integer_)

      result <- tbl %>%
        janitor::clean_names() %>%
        make_fastRhockey_data(
          paste0(toupper(league), " League Notes data from ESPN core-v2"),
          Sys.time()
        )
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} league notes available!",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} league notes",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_league_notes
NULL
#' @title **Get ESPN NHL League Notes (core-v2)**
#' @rdname espn_nhl_league_notes
#' @author Saiem Gilani
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per league note reference,
#'   or an empty tibble when the endpoint is unavailable. **Note**: The NHL
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
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   # NOTE: Returns empty tibble for NHL (ESPN does not populate this endpoint)
#'   try(espn_nhl_league_notes())
#' }
espn_nhl_league_notes <- function(...) {
  .espn_hockey_league_notes(league = "nhl", ...)
}
