#' @title **NHL Teams Roster**
#' @description Returns NHL Teams roster information for a given team ID
#' @param team_id A unique team ID
#' @param season season in format XXXXYYYY
#' @return Returns a tibble
#' @keywords NHL Teams Roster
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   nhl_teams_roster(team_id = 14, season = 20202021)
#' }
nhl_teams_roster <- function(team_id, season=most_recent_nhl_season_api_param()){

  base_url <- "https://statsapi.web.nhl.com/api/v1/teams/"

  full_url <- paste0(base_url,
                     team_id,
                     "/roster",
                     "?season=",
                     season)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      teams_df <- jsonlite::fromJSON(resp)[["roster"]]
      teams_df <- jsonlite::fromJSON(jsonlite::toJSON(teams_df),flatten=TRUE)
      teams_df <- teams_df %>%
        dplyr::rename(
          player_id = .data$person.id,
          player_full_name = .data$person.fullName,
          player_link = .data$person.link) %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          team_id = team_id,
          season = season)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team roster data for {team_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(teams_df)
}
