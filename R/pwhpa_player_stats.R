#' @title PWHPA Player Stats
#' @description Returns stat leaderboards for either skaters or goalies
#' @param position takes skaters or goalies as an argument
#' @return Returns a tibble
#' @keywords PWHPA Player Stats
#' @import httr
#' @importFrom rvest read_html html_elements html_nodes
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows mutate
#' @export
#' @examples
#' \donttest{
#'   try(pwhpa_player_stats(position = "skaters"))
#' }
#' \donttest{
#'   try(pwhpa_player_stats(position = "goalies"))
#' }
pwhpa_player_stats <- function(position = "skaters") {

  # https://stats.pwhpa.com/list/scoring-leaders/
  # https://stats.pwhpa.com/list/goaltending/

  if (position == "skaters") {

    full_url <- "https://stats.pwhpa.com/list/scoring-leaders/"

  } else if (position == "goalies") {

    full_url <- "https://stats.pwhpa.com/list/goaltending/"

  }

  res <- httr::RETRY("GET", full_url)

  check_stats(res)

  tryCatch(
    expr = {
      lst1 <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        rvest::read_html() %>%
        rvest::html_elements("tbody")

      rows <- lst1[[1]] %>% rvest::html_nodes("tr")

      stats <- list()

      for (id in 1:length(rows)) {
        player_info <- rows[[id]] %>%
          rvest::html_nodes("td")

        player <- process_stats(pos = position, player_info = player_info)

        stats[[id]] <- player
      }

      stats <- dplyr::bind_rows(stats) %>%
        tibble::as_tibble()
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no stats available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(stats)

}
