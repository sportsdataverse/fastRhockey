#' @title **NHL Teams Stats**
#' @description Returns NHL Teams stats information for a given team ID
#' @param team_id A unique team ID
#' @return Returns a tibble
#' @keywords NHL Teams Stats
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' \donttest{
#'   nhl_teams_stats(team_id = 14)
#' }
nhl_teams_stats <- function(team_id){
  
  base_url <- "https://statsapi.web.nhl.com/api/v1/teams/"
  
  full_url <- paste0(base_url,
                     team_id,
                     "/stats")
  
  
  res <- httr::RETRY("GET", full_url)
  
  # Check the result
  check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      teams_df <- jsonlite::fromJSON(resp)[["stats"]]
      teams_df <- jsonlite::fromJSON(jsonlite::toJSON(teams_df),flatten=TRUE)
      teams_df <- teams_df %>% 
        tidyr::unnest_wider(.data$splits, names_sep='_') %>% 
        janitor::clean_names()
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team stats data for {team_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(teams_df)
}