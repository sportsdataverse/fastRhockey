#' @title **NHL Divisions Info**
#' @description Returns information on a division by division ID
#' @param division_id Division ID
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
#' @keywords NHL Division Info
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_divisions_info(division_id=17))
#' }
nhl_divisions_info<- function(division_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/divisions/"

  full_url <- paste0(base_url, division_id)


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
        as.data.frame() %>%
        make_fastRhockey_data("NHL Divisions Information from NHL.com",Sys.time())

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no division info data for {division_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(divisions_df)
}
