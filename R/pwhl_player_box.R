#' @title  **PWHL Player Game Box Scores**
#' @description PWHL Player Box Scores
#'
#' @param game_id Game ID that you want play-by-play for
#' @return A data frame with play-by-play data from the PWHL
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#'   try(pwhl_player_box(game_id = 27))
#' }

pwhl_player_box <- function(game_id) {

  tryCatch(
    expr = {
      URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=gameSummary&game_id={game_id}&key=694cfeed58c932ee&site_id=2&client_code=pwhl&lang=en&league_id=&callback=angular.callbacks._6")

      res <- httr::RETRY("GET", URL)
      res <- res %>%
        httr::content(as = "text", encoding = "utf-8")
      callback_pattern <- "angular.callbacks._\\d+\\("
      res <- gsub(callback_pattern, "", res)
      res <- gsub("}}})", "}}}", res)

      r <- res %>% jsonlite::parse_json()

      suppressWarnings(
        expr = {

        home_goalie <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$homeTeam$goalies, function(x) unlist(x))))) %>% janitor::clean_names()
        away_goalie <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$visitingTeam$goalies, function(x) unlist(x))))) %>% janitor::clean_names()

        home_skaters <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$homeTeam$skaters, function(x) unlist(x))))) %>% janitor::clean_names()
        away_skaters <- data.frame(row.names = NULL, (do.call(rbind, lapply(r$visitingTeam$skaters, function(x) unlist(x))))) %>% janitor::clean_names()

      })

      home_goalie$team_id <- r$homeTeam$info$id
      away_goalie$team_id <- r$visitingTeam$info$id
      home_skaters$team_id <- r$homeTeam$info$id
      away_skaters$team_id <- r$visitingTeam$info$id

      # home_skatersi

      goalies <- dplyr::bind_rows(home_goalie, away_goalie)
      skaters <- dplyr::bind_rows(home_skaters, away_skaters)
      skaters$game_id <- r$details$id
      goalies$game_id <- r$details$id

      skaters <- skaters %>%
        dplyr::select(-c("info_birth_date", "info_jersey_number", "info_player_image_url", "status")) %>%
        dplyr::mutate(league = "pwhl")

      goalies <- goalies %>%
        dplyr::select(-c("info_birth_date", "info_jersey_number", "info_player_image_url", "status")) %>%
        dplyr::mutate(starting = ifelse(starting != "1", 0, 1)) %>%
        dplyr::mutate(league = "pwhl")

      skater_cols <- c("player_id", "first_name", "last_name", "position", "goals", "assists", "points", "penalty_minutes", "plus_minus", "faceoff_attempts",
                       "faceoff_wins", "shots", "hits", "blocked_shots", "toi", "starting", "team_id", "game_id", "league")
      goalie_cols <- c("player_id", "first_name", "last_name", "position", "goals", "assists", "points", "penalty_minutes", "plus_mins", "faceoff_attempts",
                       "faceoff_wins", "toi", "shots_against", "goals_against", "saves", "starting", "team_id", "game_id", "league")
      colnames(skaters) <- skater_cols
      colnames(goalies) <- goalie_cols

      suppressWarnings(
        expr = {

        skaters <- skaters %>%
          dplyr::mutate_at(c( "goals", "assists", "points", "penalty_minutes", "plus_minus", "faceoff_attempts",
                              "faceoff_wins", "shots", "hits", "blocked_shots"), as.numeric) %>%
          dplyr::mutate(
            faceoff_losses = faceoff_attempts - faceoff_wins,
            faceoff_pct = faceoff_wins / faceoff_attempts
          ) %>%
          tidyr::separate(toi, into = c("minute", "second"), remove = FALSE) %>%
          dplyr::mutate(time_on_ice = round((as.numeric(minute) * 60 + as.numeric(second)) / 60, 1)) %>%
          dplyr::select(c("player_id", "first_name", "last_name", "position", "team_id", "game_id", "league", "toi", "time_on_ice",
                          "goals", "assists", "points", "shots", "hits", "blocked_shots",
                          "penalty_minutes", "plus_minus", "faceoff_attempts", "faceoff_wins", "faceoff_losses", "faceoff_pct", "starting"))

        goalies <- goalies %>%
          dplyr::mutate_at(c( "goals", "assists", "points", "penalty_minutes", "faceoff_attempts",
                              "faceoff_wins", "saves", "shots_against",
                              "goals_against"), as.numeric) %>%
          dplyr::mutate(
            faceoff_losses = faceoff_attempts - faceoff_wins,
            faceoff_pct = faceoff_wins / faceoff_attempts
          ) %>%
          tidyr::separate(toi, into = c("minute", "second"), remove = FALSE) %>%
          dplyr::mutate(time_on_ice = round((as.numeric(minute) * 60 + as.numeric(second)) / 60, 1)) %>%
          dplyr::select(c("player_id", "first_name", "last_name", "position", "team_id", "game_id", "league", "toi", "time_on_ice",
                          "saves", "goals_against", "shots_against",
                          "goals", "assists", "points",
                          "penalty_minutes", "faceoff_attempts", "faceoff_wins", "faceoff_losses", "faceoff_pct", "starting"))

        })

      player_box <- c(list(skaters), list(goalies))
      names(player_box) <- c("skaters", "goalies")
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no player box data available! Try using `phf_schedule` to find a game ID to look up!"))
    },
    warning = function(w) {

    },
    finally = {

    }
  )

  return(player_box)
}
