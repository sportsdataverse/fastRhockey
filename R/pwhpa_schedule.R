#' @title PWHPA Schedule Lookup
#' @description Returns PWHPA Schedule information
#' @return Returns a tibble
#' @keywords PWHPA Roster
#' @import httr
#' @importFrom rvest read_html html_elements html_nodes
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' \donttest{
#'   try(pwhpa_schedule())
#' }
pwhpa_schedule <- function() {

  full_url <- "https://stats.pwhpa.com/calendar/secret-dream-gap-tour-schedule/"

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url)

  check_stats(res)

  tryCatch(
    expr = {
      lst <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        rvest::read_html() %>%
        rvest::html_elements("tbody")

      g <- lst[1] %>%
        rvest::html_nodes("tr")

      lst <- list()

      for (id in seq(1:length(g))) {

        game_info <- g[[id]] %>%
          rvest::html_nodes("td")

        game_df <- process_game_info(game_info = game_info)

        lst[[id]] <- game_df

      }

      schedule_df <- dplyr::bind_rows(lst) %>%
        tibble::as_tibble()
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no schedule data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(schedule_df)

}
