#' @title PWHPA Game Boxscores
#' @description Returns game information for a given Game ID in a list of team stats, skater stats, and goalie stats
#' @param game_id Game Unique ID
#' @return Returns list of dataframes
#' @keywords PWHPA Game Boxscores
#' @import httr
#' @importFrom rvest read_html html_elements html_nodes
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows mutate
#' @export
#' @examples
#' \donttest{
#'   try(pwhpa_boxscore(game_id = "scotiabank-vs-sonnet'))
#' }
pwhpa_boxscore <- function(game_id) {

  base_url <- "https://stats.pwhpa.com/event/"
  full_url <- paste0(base_url,
                     game_id,
                     "/")

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url)

  check_status(res)

  tryCatch(
    expr = {
      base <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        rvest::read_html()

      game <- base %>%
        rvest::html_elements("tbody")

      score_g <- list()
      score <- game[[2]] %>% rvest::html_nodes("tr")

      for (id in 1:length(score)) {

        score_info <- score[[id]] %>%
          rvest::html_nodes("td")

        ls <- process_scores(score_info = score_info)

        score_g[[id]] <- ls

      }

      score_line <- dplyr::bind_rows(score_g) %>%
        tibble::as_tibble()

      # off 1
      off1 <- game[[3]] %>% rvest::html_nodes("tr") %>%
        process_offense_players() %>%
        dplyr::mutate(team = score_line$team[[1]])
      # def 1
      def1 <- game[[4]] %>% rvest::html_nodes("tr") %>%
        process_defense_players() %>%
        dplyr::mutate(team = score_line$team[[1]])

      # off 2
      off2 <- game[[5]] %>% rvest::html_nodes("tr") %>%
        process_offense_players() %>%
        dplyr::mutate(team = score_line$team[[2]])
      # def 2
      def2 <- game[[6]] %>% rvest::html_nodes("tr") %>%
        process_defense_players() %>%
        dplyr::mutate(team = score_line$team[[2]])

      offense_stats <- dplyr::bind_rows(off1, off2) %>%
        tibble::as_tibble()
      defense_stats <- dplyr::bind_rows(def1, def2) %>%
        tibble::as_tibble()

      results <- list(
        "team_scores" = score_line,
        "skaters" = offense_stats,
        "goalies" = defense_stats
      )
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(results)

}
