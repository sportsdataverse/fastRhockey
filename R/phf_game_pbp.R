#' @title phf_pbp
#' @description phf_pbp: pull in the raw data for a game_id from the PHF/NWHL API
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A data frame of play by play information
#' @import rvest
#' @import httr
#' @import dplyr
#' @importFrom jsonlite parse_json
#' @importFrom purrr pluck map_dfr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_pbp(game_id = 268127))
#' }
phf_pbp <- function(game_id) {
  base_url <- "https://web.api.digitalshift.ca/partials/stats/game/play-by-play?game_id="
  full_url <- paste0(base_url, game_id)

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

  plays_data <- data.frame()
  tryCatch(
    expr={
      data <- res %>%
        httr::content(as = "text", encoding="utf-8") %>%
        jsonlite::fromJSON() %>%
        purrr::pluck("content") %>%
        rvest::read_html() %>%
        rvest::html_table()

      plays_data <- data[
        !sapply(
          lapply(data, function(x){
            if("Time" %in% colnames(x) && nrow(x)>0){
              return(x)
            }
            if("Play" %in% colnames(x) && nrow(x)>0){
              return(x)
            }
          }),is.null)]

      if(length(plays_data) %in% c(5,6)){
        plays_data <- plays_data[1:5]
      } else if(length(plays_data)>6) {
        plays_data
      }

      plays_df <- purrr::map_dfr(1:length(plays_data), function(x){
        plays_data[[x]] %>%
          helper_phf_pbp_normalize_columns() %>%
          dplyr::mutate(
            period_id = x,
            game_id = game_id)})
      game_details <- phf_game_details(game_id)

      plays_df <- plays_df %>%
        dplyr::left_join(game_details, by = "game_id")

      plays_df <- helper_phf_pbp_data(plays_df) %>%
        make_fastRhockey_data("PHF Play-by-Play Information from PremierHockeyFederation.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no game data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(plays_df)
}

