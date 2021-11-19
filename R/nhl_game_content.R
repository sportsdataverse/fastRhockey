#' @title **NHL Game Content**
#' @description Returns game content for a given game ID
#' @param game_id Game unique ID 
#' @return Returns a tibble
#' @keywords NHL Game Content
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' \donttest{
#'   nhl_game_content(game_id=2021020182)
#' }
nhl_game_content <- function(game_id){
  
  base_url <- "https://statsapi.web.nhl.com/api/v1/game/"
  
  full_url <- paste0(base_url, 
                     game_id,
                     "/content")
  
  
  res <- httr::RETRY("GET", full_url)
  
  # Check the result
  check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      game_highlights_df <- jsonlite::fromJSON(resp)[["highlights"]]
      game_highlights_df <- game_highlights_df$gameCenter
      game_highlights_df <- game_highlights_df$items
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no game content data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(game_highlights_df)
}