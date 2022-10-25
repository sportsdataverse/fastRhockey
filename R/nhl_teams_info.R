#' @title **NHL Teams Info**
#' @description Returns NHL Teams information for a given team ID
#' @param team_id A unique team ID
#' @return Returns a tibble
#' @keywords NHL Teams Info
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_teams_info(team_id = 14))
#' }
nhl_teams_info <- function(team_id){

  base_url <- "https://statsapi.web.nhl.com/api/v1/teams/"

  full_url <- paste0(base_url,
                     team_id)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      teams_df <- jsonlite::fromJSON(resp)[["teams"]]
      teams_df <- jsonlite::fromJSON(jsonlite::toJSON(teams_df),flatten=TRUE)
      teams_df <- teams_df %>%
        dplyr::rename("team_id" = "id") %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("NHL Teams Information from NHL.com",Sys.time())
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team info data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(teams_df)
}
