#' @title **NHL Conferences**
#' @description Returns a table of current NHL conferences
#' @return Returns a data frame
#' * conference_id - conference ID
#' * name - conference name
#' * link - link to conference information
#' * abbreviation - conference abbreviation
#' * short_name - conference short name
#' * active - active conference flag
#' @keywords NHL Conferences
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_conferences())
#' }
nhl_conferences<- function(){

  base_url <- "https://statsapi.web.nhl.com/api/v1/conferences"

  full_url <- paste0(base_url)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      conferences_df <- jsonlite::fromJSON(resp)[["conferences"]]
      conferences_df <- conferences_df %>%
        janitor::clean_names() %>%
        dplyr::rename(conference_id = .data$id) %>%
        as.data.frame()

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no conferences data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(conferences_df)
}
