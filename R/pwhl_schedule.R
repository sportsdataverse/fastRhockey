#' @title  **PWHL Schedule**
#' @description PWHL Schedule lookup
#'
#' @param season Season (YYYY) to pull the schedule from, the concluding year in XXXX-YY format
#' @param game_type Game type: `"both"` (default), `"regular"`, `"playoffs"`,
#'   or `"preseason"`. When `"both"`, regular-season and playoff games for the
#'   season are combined into a single, chronologically ordered data frame
#'   (preseason is excluded from `"both"` -- request it explicitly with
#'   `game_type = "preseason"`).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name     |types     |description                              |
#'    |:------------|:---------|:----------------------------------------|
#'    |game_id      |character |Unique game identifier.                  |
#'    |season       |numeric   |Season (concluding year, YYYY).          |
#'    |game_date    |character |Game date.                               |
#'    |game_status  |character |Status of the game.                      |
#'    |home_team    |character |Home team name.                          |
#'    |home_team_id |character |Home team identifier.                    |
#'    |away_team    |character |Away team name.                          |
#'    |away_team_id |character |Away team identifier.                    |
#'    |home_score   |character |Home team score.                         |
#'    |away_score   |character |Away team score.                         |
#'    |winner       |character |Winning team.                            |
#'    |venue        |character |Venue where the game was played.         |
#'    |venue_url    |character |URL for the venue.                       |
#'    |game_type    |character |Game type the row belongs to.            |
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_schedule(season = 2024))
#'   try(pwhl_schedule(season = 2024, game_type = "playoffs"))
#' }

pwhl_schedule <- function(season, game_type = "both") {

  game_type <- match.arg(
    game_type,
    choices = c("both", "regular", "playoffs", "preseason")
  )

  # "both" expands to regular + playoffs (preseason intentionally excluded).
  game_types <- if (game_type == "both") c("regular", "playoffs") else game_type

  seasons <- pwhl_season_id()

  schedule_data <- data.frame()

  for (gt in game_types) {
    matched <- seasons %>%
      dplyr::filter(.data$season_yr == !!season, .data$game_type_label == !!gt)

    # A requested game_type may not exist for this season yet (e.g. playoffs
    # for an in-progress season). Skip it rather than error so "both" still
    # returns the regular-season games that do exist.
    if (nrow(matched) == 0) next

    for (sid in matched$season_id) {
      schedule_data <- dplyr::bind_rows(
        schedule_data,
        .pwhl_fetch_schedule(season_id = sid, season = season, game_type = gt)
      )
    }
  }

  if (nrow(schedule_data) == 0) {
    message(glue::glue("{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"))
    return(schedule_data)
  }

  schedule_data <- schedule_data %>%
    dplyr::mutate(
      # Scores arrive as character goal counts; compare them as integers so
      # double-digit scores order correctly (a lexical ">" makes "10" < "9").
      home_goals = suppressWarnings(as.integer(.data$home_score)),
      away_goals = suppressWarnings(as.integer(.data$away_score)),
      # A winner exists only once a game is final. PWHL games cannot end in a
      # regulation tie -- overtime/shootout always decides them -- so a game
      # that has not been played (or whose scores are not yet numeric) has no
      # winner and is left as NA rather than guessed.
      winner = dplyr::case_when(
        !grepl("^Final", .data$game_status) ~ NA_character_,
        is.na(.data$home_goals) | is.na(.data$away_goals) ~ NA_character_,
        .data$home_goals > .data$away_goals ~ .data$home_team,
        .data$away_goals > .data$home_goals ~ .data$away_team,
        TRUE ~ NA_character_
      ),
      season = !!season,
      # Internal sort keys (dropped by the final select). The feed has no ISO
      # date, so derive one from "date_with_day" for a true chronological sort.
      sort_date = .pwhl_schedule_sort_date(.data$game_date, !!season),
      sort_id = suppressWarnings(as.numeric(.data$game_id))
    ) %>%
    dplyr::arrange(.data$sort_date, .data$sort_id) %>%
    dplyr::select(
      dplyr::all_of(c(
          "game_id",
          "season",
          "game_date",
          "game_status",
          "home_team",
          "home_team_id",
          "away_team",
          "away_team_id",
          "home_score",
          "away_score",
          "winner",
          "venue",
          "venue_url",
          "game_type"
        ))
    )

  return(schedule_data)

}

#' Fetch and parse a single PWHL season-feed schedule.
#'
#' Pulls one HockeyTech schedule feed for a resolved `season_id` and returns the
#' parsed games as a data frame with a `game_type` column. Network/parse errors
#' yield an empty data frame so the caller can combine multiple feeds safely.
#'
#' @param season_id Numeric HockeyTech season identifier.
#' @param season Numeric season year echoed into the `season` column.
#' @param game_type Character label stamped onto every returned row.
#' @return A data frame of games (possibly zero rows).
#' @keywords internal
#' @noRd
.pwhl_fetch_schedule <- function(season_id, season, game_type) {

  full_url <- glue::glue(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=schedule&team=-1&season={season_id}&month=-1&location=homeaway&key=694cfeed58c932ee&client_code=pwhl&site_id=2&league_id=1&division_id=-1&lang=en&callback=angular.callbacks._1"
  )

  schedule_data <- data.frame()

  tryCatch(
    expr = {
      res <- httr::RETRY("GET", full_url) %>%
        httr::content(as = "text", encoding = "utf-8")

      callback_pattern <- "angular.callbacks._\\d+\\("
      res <- gsub(callback_pattern, "", res)
      res <- gsub("}}]}]}])", "}}]}]}]", res)

      r <- res %>%
        jsonlite::parse_json()

      gm <- r[[1]]$sections[[1]]$data

      for (i in seq_along(gm)) {

        if (is.null(gm[[i]]$prop$venue_name$venueUrl)) {
          venue <- "TBD"
        } else {
          venue <- gm[[i]]$prop$venue_name$venueUrl
        }

        game_info <- data.frame(
          "game_id" = c(gm[[i]]$row$game_id),
          "season" = c(season),
          "game_date" = c(gm[[i]]$row$date_with_day),
          "game_status" = c(gm[[i]]$row$game_status),
          "home_team" = c(gm[[i]]$row$home_team_city),
          "home_team_id" = c(gm[[i]]$prop$home_team_city$teamLink),
          "away_team" = c(gm[[i]]$row$visiting_team_city),
          "away_team_id" = c(gm[[i]]$prop$visiting_team_city$teamLink),
          "home_score" = c(gm[[i]]$row$home_goal_count),
          "away_score" = c(gm[[i]]$row$visiting_goal_count),
          "venue" = c(gm[[i]]$row$venue_name),
          "venue_url" = c(venue),
          "game_type" = c(game_type),
          stringsAsFactors = FALSE
        )

        schedule_data <- rbind(
          schedule_data,
          game_info
        )

      }
    },
    error = function(e) {
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(schedule_data)
}

#' Derive a sortable date from a PWHL "date_with_day" display string.
#'
#' The schedule feed exposes no ISO date, only strings like `"Mon, Jan 1"`.
#' This parses the month and day locale-independently (via the base R
#' `month.abb` constant, which is always English regardless of `LC_TIME`) and
#' infers the calendar year from the season: PWHL seasons span Nov-May, so
#' months in September or later belong to the prior calendar year
#' (`season - 1`) while January-August belong to `season`.
#'
#' @param date_with_day Character vector of display dates.
#' @param season Numeric season (concluding) year.
#' @return Numeric vector (days since epoch); `NA` where parsing fails.
#' @keywords internal
#' @noRd
.pwhl_schedule_sort_date <- function(date_with_day, season) {
  matches <- regmatches(
    date_with_day,
    regexec("([A-Za-z]{3,})\\s+(\\d{1,2})", date_with_day)
  )

  vapply(matches, function(parts) {
    if (length(parts) < 3) return(NA_real_)
    month_num <- match(substr(parts[2], 1, 3), month.abb)
    day_num <- suppressWarnings(as.integer(parts[3]))
    if (is.na(month_num) || is.na(day_num)) return(NA_real_)
    year_num <- if (month_num >= 9) season - 1 else season
    as.numeric(as.Date(sprintf("%04d-%02d-%02d", year_num, month_num, day_num)))
  }, numeric(1))
}
