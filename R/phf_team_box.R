#' @title **PHF Team Boxscore**
#' @description phf_team_box: loads the team boxscore and shot/score data for a game into one data frame through just one function
#'
#' @param game_id The unique ID code for the game that you are interested in viewing the data for
#' @return A dataframe of team-level box score information
#' @import rvest
#' @import janitor
#' @import httr
#' @import stringr
#' @import jsonlite
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(phf_team_box(game_id = 420339))
#' }
phf_team_box <- function(game_id) {

  tryCatch(
    expr = {

      y <- game_id

      # load raw data from API
      df <- phf_game_raw(game_id = game_id)

      # turn raw data into a boxscore format
      df <- helper_phf_team_box(data = df)

      df <- df %>%
        dplyr::mutate(game_id = y) %>%
        dplyr::select(
          "team",
          "game_id",
          "winner",
          "total_scoring",
          tidyr::everything()) %>%
        make_fastRhockey_data("PHF Team Boxscore Information from PremierHockeyFederation.com",Sys.time())

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid game_id or no boxscore data available! Try using `phf_schedule` to find a game ID to look up!"))
    },
    warning = function(w) {

    },
    finally = {

    }
  )

  return(df)

}
