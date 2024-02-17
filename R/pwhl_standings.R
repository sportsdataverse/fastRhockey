#' @title  **PWHL Standings**
#' @description PWHL Standings lookup
#'
#' @param season Season (YYYY) to pull the roster from, the concluding year in XXXX-YY format
#' @param regular Bool for whether to pull regular or pre-season rosters
#' @return A data frame with standings data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue

pwhl_standings <- function(season = 2023, regular = TRUE) {
  if (regular) {
    season_id <- 1
  } else if (! regular) {
    season_id <- 2
  }

  REG_URL = paste0("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=teams&groupTeamsBy=league&context=overall&site_id=2&season=", season_id, "&special=false&key=694cfeed58c932ee&client_code=pwhl&league_id=1&division=undefined&sort=points&lang=en&callback=angular.callbacks._b")
  URL = paste0("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=teams&groupTeamsBy=division&context=overall&site_id=2&season=", season_id, "&special=true&key=694cfeed58c932ee&client_code=pwhl&league_id=1&division=-1&sort=points&lang=en&callback=angular.callbacks._4")
  reg_res <- httr::RETRY(
    "GET",
    REG_URL
  )

  reg_res <- reg_res %>%
    httr::content(as = "text", encoding = "utf-8")

  reg_res <- gsub("angular.callbacks._b\\(", "", reg_res)
  reg_res <- gsub("}}]}]}])", "}}]}]}]", reg_res)
  r_reg <- reg_res %>%
    jsonlite::parse_json()

  res <- httr::RETRY(
    "GET",
    URL
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._4\\(", "", res)
  res <- gsub("}}]}]}])", "}}]}]}]", res)
  r <- res %>%
    jsonlite::parse_json()

  reg_data <- r_reg[[1]]$sections[[1]]$data
  data <- r[[1]]$sections[[1]]$data

  reg_standings <- data.frame()
  standings <- data.frame()
  # data[[1]]

  # jsonlite::fromJSON(r[[1]]$sections[[1]]$data)
  # jsonlite::flatten(data)

  tryCatch(
    expr = {
      for (y in 1:length(reg_data)) {

        reg_team_stand <- data.frame(
          team_rank = c(reg_data[[y]]$row$rank),
          team = c(reg_data[[y]]$row$name),
          team_code = c(reg_data[[y]]$row$team_code),
          games_played = c(reg_data[[y]]$row$games_played),
          points = c(reg_data[[y]]$row$points),
          wins = c(reg_data[[y]]$row$regulation_wins),
          non_reg_wins = c(reg_data[[y]]$row$non_reg_wins),
          losses = c(reg_data[[y]]$row$losses),
          non_reg_losses = c(reg_data[[y]]$row$non_reg_losses),
          goals_for = c(reg_data[[y]]$row$goals_for),
          goals_against = c(reg_data[[y]]$row$goals_against),
          games_remaining = c(reg_data[[y]]$row$games_remaining)
        )

        reg_standings <- dplyr::bind_rows(reg_standings, reg_team_stand)

      }


      for (y in 1:length(data)) {

        team_stand <- data.frame(
          # team_rank = c(data[[y]]$row$rank),
          team = c(data[[y]]$row$name),
          team_code = c(data[[y]]$row$team_code),
          # games = c(data[[y]]$row$games_played),
          ot_wins = c(data[[y]]$row$ot_wins),
          ot_losses = c(data[[y]]$row$ot_losses),
          so_wins = c(data[[y]]$row$shootout_wins),
          so_losses = c(data[[y]]$row$shootout_losses),
          power_play_goals = c(data[[y]]$row$power_play_goals),
          power_play_goals_against = c(data[[y]]$row$power_play_goals_against),
          power_plays = c(data[[y]]$row$power_plays),
          power_play_pct = c(data[[y]]$row$power_play_pct),
          short_handed_goals = c(data[[y]]$row$short_handed_goals_for),
          short_handed_goals_against = c(data[[y]]$row$short_handed_goals_against),
          times_short_handed = c(data[[y]]$row$times_short_handed),
          penalty_kill_pct = c(data[[y]]$row$penalty_kill_pct)
        )

        standings <- dplyr::bind_rows(standings, team_stand)

      }

      lg_standings <- reg_standings %>%
        dplyr::left_join(standings, by = c("team", "team_code"))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(lg_standings)

}
