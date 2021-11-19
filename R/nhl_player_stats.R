#' @title **NHL Player Stats**
#' @description Returns player stats for a given player ID
#' @param player_id Player unique ID 
#' @return Returns a tibble
#' @keywords NHL Player stats
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' \donttest{
#'   nhl_player_stats(player_id=8476899)
#' }
nhl_player_stats <- function(player_id){
  
  base_url <- "https://statsapi.web.nhl.com/api/v1/people/"
  
  full_url <- paste0(base_url, 
                     player_id,
                     "?expand=person.stats&stats=yearByYear,yearByYearPlayoffs,careerRegularSeason&expand=stats.team&site=en_nhl")
  
  
  res <- httr::RETRY("GET", full_url)
  
  # Check the result
  check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      player_df <- jsonlite::fromJSON(resp)[["people"]]
      player_df <- jsonlite::fromJSON(jsonlite::toJSON(player_df),flatten=TRUE)
      player_df <- player_df %>% 
        dplyr::rename(player_id = .data$id) %>% 
        janitor::clean_names()
      stats <- player_df$stats[[1]]
      stats_lst <- purrr::map(1:length(stats$splits),function(x){
        stats$splits[[x]]$split_type <-  stats$type.displayName[x]
        return(stats$splits[[x]])
      })
      stats_df <- data.table::rbindlist(stats_lst,fill=TRUE)
      player_df <- player_df %>% 
        dplyr::select(-.data$stats)
      player_stats <- player_df %>% 
        dplyr::bind_cols(stats_df)
      player_stats <- player_stats %>% 
        janitor::clean_names()
                
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player stats data for {player_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(player_stats)
}