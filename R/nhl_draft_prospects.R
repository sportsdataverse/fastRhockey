#' @title **NHL Draft Prospects**
#' @description Returns information on draft prospects
#' @return Returns a data frame:
#'     * prospect_id - 
#'     * full_name - 
#'     * link - 
#'     * first_name - 
#'     * last_name - 
#'     * birth_date - 
#'     * birth_city - 
#'     * birth_state_province - 
#'     * birth_country - 
#'     * height - 
#'     * weight - 
#'     * shoots_catches - 
#'     * nhl_player_id - 
#'     * draft_status - 
#'     * nationality - 
#'     * primary_position_code - 
#'     * primary_position_name - 
#'     * primary_position_type - 
#'     * primary_position_abbreviation - 
#'     * prospect_category_id - 
#'     * prospect_category_short_name - 
#'     * prospect_category_name - 
#'     * amateur_team_name - 
#'     * amateur_team_link - 
#'     * amateur_league_name - 
#'     * amateur_league_link - 
#'     * ranks_final_rank - 
#'     * ranks_draft_year -
#' @keywords NHL Draft Prospects
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' \donttest{
#'   nhl_draft_prospects()
#' }
nhl_draft_prospects <- function(){
  
  base_url <- "https://statsapi.web.nhl.com/api/v1/draft/prospects"
  
  full_url <- paste0(base_url)
  
  
  res <- httr::RETRY("GET", full_url)
  
  # Check the result
  check_status(res)
  
  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      draft_prospects_df <- jsonlite::fromJSON(resp)[["prospects"]]
      draft_prospects_df <- jsonlite::fromJSON(jsonlite::toJSON(draft_prospects_df),flatten=TRUE)
      
      draft_prospects_df <- draft_prospects_df %>% 
        janitor::clean_names() %>% 
        dplyr::rename(prospect_id = .data$id) %>% 
        as.data.frame()
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft prospects data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(draft_prospects_df)
}