#' @title **NHL Teams Stats**
#' @description Returns NHL Teams stats information for a given team ID
#' @param team_id A unique team ID
#' @param season season in format XXXXYYYY
#' @return Returns a tibble
#' @keywords NHL Teams Stats
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'   try(nhl_teams_stats(team_id = 14))
#' }
nhl_teams_stats <- function(team_id, season=most_recent_nhl_season_api_param()){

  base_url <- "https://statsapi.web.nhl.com/api/v1/teams/"

  full_url <- paste0(base_url,
                     team_id,
                     "/stats",
                     "?season=",
                     season)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  resp <- res %>%
    httr::content(as = "text", encoding = "UTF-8")
  tryCatch(
    expr = {
      teams_df <- jsonlite::fromJSON(resp)[["stats"]]
      types <- teams_df$type$gameType[1,]
      stats_df <- teams_df$splits[[1]]$stat
      ranks_df <- teams_df$splits[[2]]$stat
      team <- teams_df$splits[[1]]$team
      colnames(types) <- paste0("season_type_",colnames(types))
      colnames(ranks_df) <- paste0(colnames(ranks_df),"_rank")
      colnames(team) <- paste0("team_", colnames(team))

      teams_df <- dplyr::bind_cols(team, types, stats_df, ranks_df)
      teams_df <- teams_df %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          team_id = team_id,
          season = season)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team stats data for {team_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(teams_df)
}
