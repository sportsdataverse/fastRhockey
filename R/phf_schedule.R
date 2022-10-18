#' @title  **PHF Schedule**
#' @description PHF Schedule lookup
#'
#' @param season Season (YYYY) to pull the schedule from, the concluding year in XXXX-YY format
#' @return A data frame with schedule data
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_schedule(season=2022))
#' }

phf_schedule <- function(season){
  season_id <- phf_get_season_id(season=season)

  base_url <- "https://web.api.digitalshift.ca/partials/stats/schedule/table?limit=100&all=true&season_id="
  full_url <- paste0(base_url,
                     season_id)

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(
      `Authorization`='ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'))

  # Check the result
  check_status(res)

  schedule_data <- data.frame()
  tryCatch(
    expr={
      data <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::parse_json() %>%
        purrr::pluck("content") %>%
        rvest::read_html() %>%
        rvest::html_elements("tbody") %>%
        rvest::html_attr("ng-init")
      data <- stringr::str_remove(data,"ctrl.schedule=")
      schedule_data <- purrr::map(data, jsonlite::fromJSON)
      schedule_data <- schedule_data[[1]]

      schedule_data <- schedule_data %>%
        dplyr::mutate(
          winner = dplyr::case_when(
            .data$away_score > .data$home_score ~ .data$away_team,
            .data$home_score > .data$away_score ~ .data$home_team,
            .data$home_score == .data$away_score & .data$date_group < Sys.Date() ~ "Tie",
            .data$home_score == .data$away_score & .data$date_group >= Sys.Date() ~ "",
            TRUE ~ NA_character_)) %>%
        make_fastRhockey_data("PHF Schedule Information from PremierHockeyFederation.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no schedule data available! Try a season from 2016-2021!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(schedule_data)
}

