#' @title **NHL Conference Info**
#' @description Returns information on a conference by conference ID
#' @param conference_id Conference ID
#' @return Returns a data frame
#' * conference_id - conference ID
#' * name - conference name
#' * link - link to conference information
#' * abbreviation - conference abbreviation
#' * short_name - conference short name
#' * active - active conference flag
#' @keywords NHL Conferences Info
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_conferences_info(conference_id = 7))
#' }
nhl_conferences_info<- function(conference_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/conferences/"

  ## Inputs
  ## game_id
  full_url <- paste0(base_url, conference_id)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      conferences_df <- jsonlite::fromJSON(resp)[["conferences"]]
      conferences_df <- conferences_df %>%
        janitor::clean_names() %>%
        dplyr::rename(conference_id = .data$id) %>%
        as.data.frame()

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no conference info data for {conference_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(conferences_df)
}
