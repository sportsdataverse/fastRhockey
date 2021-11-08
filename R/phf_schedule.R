#' @title PHF League Information per year
#'
#' @param season Season (YYYY) to pull the league info and IDs for. Season is the concluding year in XXXX-YY format
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples \dontrun{
#' phf_league_info(season = 2022)
#' }
phf_league_info <- function(season = 2022){

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
 base_url <- "https://web.api.digitalshift.ca/partials/stats/filters?type=season&id="
 full_url <- paste0(base_url,
                    season_id)
 res <- httr::RETRY("GET", full_url,
                    httr::add_headers(
                      `Authorization`='ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'))
 check_status(res)

 tryCatch(
   expr = {
 data <- res %>%
   httr::content(as = "text", encoding="utf-8") %>%
   jsonlite::fromJSON()
 seasons <- data$season$options
 divisions <- data$division$options
 teams <- data$team$options
 leagues <- data$league$options
 facilities <- data$facility$options
 officials <- data$official$options
 brackets <- data$bracket$options
 data <- c(list(seasons), list(divisions), list(teams), list(leagues),
           list(officials), list(brackets))
 names(data)<- c("seasons", "divisions", "teams", "league",  "officials", "brackets")
   },
 error = function(e) {
   message(glue::glue("{Sys.time()}: Invalid season or no data available! Try 2016 through 2021!"))
 },
 warning = function(w) {

 },
 finally = {

    })

 return(data)

}

#' @title phf_schedule
#' @description phf_schedule lookup
#'
#' @param season Season (YYYY) to pull the schedule from, the concluding year in XXXX-YY format
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \dontrun{
#'   full_schedule <- phf_schedule(season=2021)
#' }

phf_schedule <- function(season = 2021){

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
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no schedule data available! Try a season from 2016-2021!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  schedule_data <- schedule_data %>%
    dplyr::mutate(winner = ifelse(.data$away_score > .data$home_score, .data$away_team,
                                ifelse(.data$home_score > .data$away_score, .data$home_team,
                                    ifelse(.data$home_score == .data$away_score, "Tie", NA))))

  return(schedule_data)
}

