#' @title  **PWHL Schedule**
#' @description PWHL Schedule lookup
#'
#' @param season Season (YYYY) to pull the schedule from, the concluding year in XXXX-YY format
#' @return A data frame with schedule data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_schedule(season = 2023))
#' }

pwhl_schedule <- function(season, game_type = "regular") {

  seasons <- pwhl_season_id() %>%
    dplyr::filter(season_yr == season, game_type_label == game_type)

  season_id <- seasons$season_id

  base_url = glue::glue(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=schedule&team=-1&season={season_id}&month=-1&location=homeaway&key=694cfeed58c932ee&client_code=pwhl&site_id=2&league_id=1&division_id=-1&lang=en&callback=angular.callbacks._1"
  )
  full_url = base_url

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")
  callback_pattern <- "angular.callbacks._\\d+\\("
  res <- gsub(callback_pattern, "", res)
  # res <- gsub("\\}\\]\\)$", "}}]", res)
  # res <- gsub("angular.callbacks._1\\(", "", res)
  res <- gsub("}}]}]}])", "}}]}]}]", res)

  r <- res %>%
    jsonlite::parse_json()

  gm <- r[[1]]$sections[[1]]$data

  schedule_data <- data.frame()

  tryCatch(
    expr = {
      for (i in 1:length(gm)) {

        if (is.null(gm[[i]]$prop$venue_name$venueUrl)) {
          venue <-'TBD'
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
          "venue_url" = c(venue)
        )

        schedule_data <- rbind(
          schedule_data,
          game_info
        )

      }

      schedule_data <- schedule_data %>%
        dplyr::mutate(
          winner = dplyr::case_when(
            .data$home_score == '' | .data$away_score == "-" ~ '-',
            .data$home_score > .data$away_score ~ .data$home_team,
            .data$away_score > .data$home_score ~ .data$away_team,
            .data$home_score == .data$away_score & .data$home_score != "-" ~ "Tie",
            TRUE ~ NA_character_
          ),
          season = season
        ) %>%
        dplyr::select(
          c(
              "game_id",
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
              "venue_url"
            )
        )
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(schedule_data)

}

