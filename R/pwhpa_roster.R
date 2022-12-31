#' @title PWHPA Roster
#' @description Returns team roster information
#' @param team takes a team name as an argument
#' @return Returns a tibble
#' @keywords PWHPA Roster
#' @import httr
#' @importFrom rvest read_html html_elements html_nodes html_attr
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows mutate distinct slice_head left_join rename select filter
#' @export
#' @examples
#' \donttest{
#'   try(pwhpa_roster(team = "adidas"))
#' }
pwhpa_roster <- function(team) {

  full_url <- "https://stats.pwhpa.com/members/"

  res <- httr::RETRY("GET", full_url)

  check_stats(res)

  tryCatch(
    expr = {
      img <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        rvest::read_html() %>%
        rvest::html_elements("section")

      body <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        rvest::read_html() %>%
        rvest::html_elements("tbody")

      body <- body[[1]] %>% rvest::html_nodes("tr")

      mem <- list()

      for (id in 1:length(body)) {
        player_info <- body[[id]] %>%
          rvest::html_nodes("td")

        player <- process_member_info(player_info = player_info)

        mem[[id]] <- player
      }

      player_data <- dplyr::bind_rows(mem)

      headshot <- img[[3]] %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        data.frame() %>%
        dplyr::rename("player_headshot" = ".")

      player_pages <- img[[3]] %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        data.frame() %>%
        dplyr::rename("player_link" = ".") %>%
        dplyr::distinct() %>%
        dplyr::mutate(index = row_number())

      players <- headshot %>%
        dplyr::slice_head(n = nrow(player_pages)) %>%
        dplyr::mutate(index = row_number()) %>%
        dplyr::left_join(player_pages, by = c("index")) %>%
        dplyr::select(-c(index))

      team_roster <- player_data %>%
        dplyr::left_join(players, by = c("player_page" = "player_link"))

      if (! team %in% c("All", "PWHPA", "all", "pwhpa")) {
        team_roster <- team_roster %>%
          dplyr::filter(team_id == tolower(team))
      }

      team_roster <- team_roster %>%
        tibble::as_tibble()
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no roster data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(team_roster)

}
