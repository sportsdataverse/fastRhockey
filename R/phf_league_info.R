#' @title **PHF League Information**
#' @description  PHF League Information per year
#' @param season Season (YYYY) to pull the league info and IDs for. Season is the concluding year in XXXX-YY format
#' @return A named list of data frames: seasons, divisions, teams, league,  officials, brackets
#'
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#' @examples \donttest{
#'   try(phf_league_info(season = 2023))
#'   try(phf_league_info(season = 2016))
#' }
phf_league_info <- function(season = most_recent_phf_season()){

  season_id <- phf_get_season_id(season=season)
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
      seasons <- data$season$options %>%
        make_fastRhockey_data("PHF Seasons Information from PremierHockeyFederation.com",Sys.time())
      divisions <- data$division$options %>%
        make_fastRhockey_data("PHF Division Information from PremierHockeyFederation.com",Sys.time())
      division_id <- divisions %>% dplyr::filter(.data$name %in% c("PHF","NWHL", "Founding Four")) %>% dplyr::pull("id")
      base_url <- "https://web.api.digitalshift.ca/partials/stats/filters?type=division&id="
      full_url <- paste0(base_url,
                         division_id)
      res <- httr::RETRY("GET", full_url,
                         httr::add_headers(
                           `Authorization`='ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'))
      check_status(res)

      data <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::fromJSON()

      teams <- data$team$options %>%
        make_fastRhockey_data("PHF Teams Information from PremierHockeyFederation.com",Sys.time())
      leagues <- data$league$options %>%
        make_fastRhockey_data("PHF Leagues Information from PremierHockeyFederation.com",Sys.time())
      facilities <- data$facility$options %>%
        make_fastRhockey_data("PHF Facility Information from PremierHockeyFederation.com",Sys.time())
      officials <- data$official$options %>%
        make_fastRhockey_data("PHF Officials Information from PremierHockeyFederation.com",Sys.time())
      brackets <- data$bracket$options %>%
        make_fastRhockey_data("PHF Brackets Information from PremierHockeyFederation.com",Sys.time())
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
