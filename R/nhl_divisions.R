#' @title **NHL Divisions**
#' @description Returns information on divisions
#' @return Returns a data frame
#' * division_id -
#' * name -
#' * name_short -
#' * link -
#' * abbreviation -
#' * active -
#' * conference_id -
#' * conference_name -
#' * conference_link -
#' @keywords NHL Divisions
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_divisions())
#' }
nhl_divisions <- function(){

  base_url <- "https://statsapi.web.nhl.com/api/v1/divisions/"

  full_url <- paste0(base_url)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      divisions_df <- jsonlite::fromJSON(resp)[["divisions"]]
      divisions_df <- jsonlite::fromJSON(jsonlite::toJSON(divisions_df),flatten=TRUE)

      divisions_df <- divisions_df %>%
        janitor::clean_names() %>%
        dplyr::rename(division_id = .data$id) %>%
        as.data.frame()

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no division data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(divisions_df)
}
