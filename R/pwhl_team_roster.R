#' @title  **PWHL Rosters**
#' @description PWHL Rosters lookup
#'
#' @param season Season (YYYY) to pull the roster from, the concluding year in XXXX-YY format
#' @param team Team to pull the roster data for
#' @param regular Bool for whether to pull regular or pre-season rosters
#' @return A data frame with roster data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_team_roster(season = 2023))
#' }

pwhl_team_roster <- function(team, season, regular) {

  # team_id <- 1 # will need the team/season look ups
  team_id <- pwhl_teams() %>% dplyr::filter(team_label == team)
  # season_id <- 2 # 1 is regular season, 2 is pre-season
  if (regular) {
    season_id <- 1
  } else if (! regular) {
    season_id <- 2
  }
  # base_url <- "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=1&season_id=2&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  full_url <- paste0(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=",
    team_id$team_id,
    "&season_id=",
    season_id,
    "&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  )

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._h\\(", "", res)
  res <- gsub("}}]}]}]})", "}}]}]}]}", res)

  r <- res %>% jsonlite::parse_json()

  team_name <- r[[1]]
  team_logo <- r[[2]]
  roster_year <- r[[3]]
  league <- r[[4]]

  players <- r[[5]][[1]]$sections

  roster_data <- data.frame()
  staff_data <- data.frame()

  player_types <- c("Forwards", "Defenders", "Goalies")

  tryCatch(
    expr = {
      for (i in 1:length(players)) {

        # i = 1

        if (players[[i]]$title %in% player_types) {
          # print('yes')

          for (p in 1:length(players[[i]]$data)) {

            if (is.null(players[[i]]$data[[p]]$row$shoots)) {
              hand <- players[[i]]$data[[p]]$row$catches
            } else {
              hand <- players[[i]]$data[[p]]$row$shoots
            }

            player_info <- data.frame(
              "player_id" = c(players[[i]]$data[[p]]$row$player_id),
              "player_name" = c(players[[i]]$data[[p]]$row$name),
              "primary_hand" = c(hand),
              "dob" = c(players[[i]]$data[[p]]$row$birthdate),
              "height" = c(players[[i]]$data[[p]]$row$height_hyphenated),
              "position" = c(players[[i]]$data[[p]]$row$position),
              "home_town" = c(players[[i]]$data[[p]]$row$hometown)
            )

            # players[[i]]$data[[p]]$prop

            roster_data <- rbind(
              roster_data,
              player_info
            )

          }

        }

      }

      roster_data <- roster_data %>%
        dplyr::mutate(
          age = round(time_length(as.Date(paste0(season, "-01-01")) - as.Date(dob), "years")),
          player_headshot = paste0("https://assets.leaguestat.com/pwhl/240x240/", player_id, ".jpg"),
          regular_season = ifelse(season == 1, TRUE, FALSE)
        )
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(roster_data)


}