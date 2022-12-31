#' @title PWHPA Player Stats by Team
#' @description Returns player stats on a team basis
#' @return Returns a tibble
#' @keywords PWHPA Player Stats by Team
#' @import httr
#' @importFrom rvest read_html html_elements html_nodes html_attr
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows mutate select filter
#' @importFrom stringr str_detect
#' @export
#' @examples
#' \donttest{
#'   try(pwhpa_team_player_stats(team = 'adidas'))
#' }
pwhpa_team_player_stats <- function(team) {

  base_url <- "https://stats.pwhpa.com/team/"
  full_url <- paste0(base_url,
                     tolower(team),
                     "/")

  res <- httr::RETRY("GET", full_url)

  check_stats(res)

  tryCatch(
    expr = {
      lst1 <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        rvest::read_html() %>%
        rvest::html_elements("tbody")

      tms <- lst1[[2]] %>%
        rvest::html_nodes("tr")

      lst <- list()

      for (id in 1:length(tms)) {

        line <- tms[[id]] %>% rvest::html_nodes("td")

        # print(length(line))
        if (length(line) > 0) {
          if (stringr::str_detect(paste(line[[1]]), "data-number")) {

            player <- process_roster(player = line) %>%
              dplyr::mutate(team = team,
                            season = 2022)

          } else {
            next
          }
        } else {
          next
        }

        lst[[id]] <- player

      }

      roster <- dplyr::bind_rows(lst) %>%
        tibble::as_tibble()
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(roster)

}
