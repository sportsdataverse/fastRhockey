#' @title PHF League Information per year
#'
#' @param season Season (YYYY) to pull the league info and IDs for. Season is the concluding year in XXXX-YY format
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#'   phf_league_info(season = 2021)
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
