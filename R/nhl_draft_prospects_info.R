#' @title **NHL Draft Prospects Info**
#' @description Returns information on draft prospect for a given prospect id
#' @param prospect_id Prospect unique ID
#' @return Returns a data frame:
#'     * prospect_id -
#'     * full_name -
#'     * link -
#'     * first_name -
#'     * last_name -
#'     * birth_date -
#'     * birth_city -
#'     * birth_country -
#'     * nationality -
#'     * height -
#'     * weight -
#'     * shoots_catches -
#'     * nhl_player_id -
#'     * primary_position_code -
#'     * primary_position_name -
#'     * primary_position_type -
#'     * primary_position_abbreviation -
#'     * prospect_category_id -
#'     * prospect_category_short_name -
#'     * prospect_category_name -
#'     * amateur_team_link -
#'     * amateur_league_link -
#' @keywords NHL Draft Prospect Info
#' @import rvest
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_prospects_info(prospect_id = 65242))
#' }
nhl_draft_prospects_info <- function(prospect_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/draft/prospects/"

  full_url <- paste0(base_url, prospect_id)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      draft_prospects_df <- jsonlite::fromJSON(resp)[["prospects"]]
      draft_prospects_df <- jsonlite::fromJSON(jsonlite::toJSON(draft_prospects_df),flatten=TRUE)

      draft_prospects_df <- draft_prospects_df %>%
        janitor::clean_names() %>%
        dplyr::rename("prospect_id" = "id") %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Draft Prospects Information from NHL.com",Sys.time())

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft prospects data for {prospect_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(draft_prospects_df)
}
