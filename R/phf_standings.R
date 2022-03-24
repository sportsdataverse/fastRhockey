#' @title phf_standings
#' @description phf_standings: pull in the standings data for a game_id from the PHF/NWHL API
#'
#' @param season Season (YYYY) to pull the standings from, the concluding year in XXXX-YY format
#' @return A data frame of standings data
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom jsonlite parse_json
#' @importFrom purrr pluck map_dfr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_standings(season = most_recent_phf_season()))
#' }
phf_standings <- function(season = most_recent_phf_season()) {
  season_id <- dplyr::case_when(
    season == 2022 ~ 3372,
    season == 2021 ~ 2779,
    season == 2020 ~ 1950,
    season == 2019 ~ 2047,
    season == 2018 ~ 2046,
    season == 2017 ~ 2045,
    season == 2016 ~ 246,
    TRUE ~ NA_real_
  )

  base_url <- "https://web.api.digitalshift.ca/partials/stats/standings/table?league_toggle=division&season_id="
  full_url <- paste0(base_url,
                     season_id)
  # setting the ticket as something that can be changed in case the API decides to change it's authorization
  # rather than hard-coding it in
  auth_ticket <- getOption(
    "fastRhockey.phf_ticket",
    default = 'ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'
  )

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY("GET", full_url,
                     httr::add_headers(`Authorization`= auth_ticket))
  # Check the result
  check_status(res)

  standings <- data.frame()

  tryCatch(
    expr={

      data <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("content") %>%
        rvest::read_html() %>%
        rvest::html_table()

      if(length(data)>0){
        standings <- data[[1]]
        standings <- standings %>%
          janitor::clean_names() %>%
          dplyr::mutate(
            team = str_remove_all(.data$team, "\\d"),
            team_tricode = substr(.data$team, nchar(.data$team) - 2, nchar(.data$team)),
            team = substr(.data$team, 1, nchar(.data$team) - 3))
      } else {
        standings <- data.frame()
      }
      standings <- standings %>%
        make_fastRhockey_data("PHF Standings Information from PremierHockeyFederation.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no standings data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(standings)
}

