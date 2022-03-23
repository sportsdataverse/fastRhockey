#' @title **NHL Player Info**
#' @description Returns player information for a given player ID
#' @param player_id Player unique ID
#' @return Returns a tibble
#' @keywords NHL Player info
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_player_info(player_id=8476899))
#' }
nhl_player_info <- function(player_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/people/"

  full_url <- paste0(base_url,
                     player_id)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      player_df <- jsonlite::fromJSON(resp)[["people"]]
      player_df <- jsonlite::fromJSON(jsonlite::toJSON(player_df),flatten=TRUE)
      player_df <- player_df %>%
        dplyr::rename(player_id = .data$id) %>%
        janitor::clean_names()
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player info data for {player_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(player_df)
}
