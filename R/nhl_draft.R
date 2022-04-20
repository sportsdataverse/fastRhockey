#' @title **NHL Draft**
#' @description Returns information on draft
#' @return Returns a data frame:
#'     * year -
#'     * round -
#'     * pick_overall -
#'     * pick_in_round -
#'     * team_id -
#'     * team_name -
#'     * team_link -
#'     * prospect_id -
#'     * prospect_full_name -
#'     * prospect_link -
#' @keywords NHL Draft
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft())
#' }
nhl_draft <- function(){

  base_url <- "https://statsapi.web.nhl.com/api/v1/draft/"

  full_url <- paste0(base_url)


  tryCatch(
    expr = {
      res <- httr::RETRY("GET", full_url)

      # Check the result
      check_status(res)

      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      draft_df <- jsonlite::fromJSON(resp)[["drafts"]]
      draft_df <- jsonlite::fromJSON(jsonlite::toJSON(draft_df),flatten=TRUE)
      draft_df <- draft_df[["rounds"]][[1]] %>%
        tidyr::unnest_longer(.data$picks) %>%
        dplyr::select(.data$picks)
      draft_df <- draft_df$picks %>%
        janitor::clean_names() %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Draft Data from NHL.com",Sys.time())

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(draft_df)
}
