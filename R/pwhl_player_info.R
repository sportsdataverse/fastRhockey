#' @title  **PWHL Player Info**
#' @description Retrieves biographical and profile information for a PWHL player.
#'
#' @param player_id Numeric player ID
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name      |types     |description                                |
#'    |:-------------|:---------|:------------------------------------------|
#'    |player_id     |numeric   |Unique player identifier.                  |
#'    |first_name    |character |Player first name.                         |
#'    |last_name     |character |Player last name.                          |
#'    |name          |character |Player full name.                          |
#'    |jersey_number |character |Jersey number.                             |
#'    |position      |character |Player position.                           |
#'    |shoots        |character |Shooting hand.                             |
#'    |catches       |character |Catching hand (goalies).                   |
#'    |birthdate     |character |Date of birth.                             |
#'    |height        |character |Player height.                             |
#'    |weight        |character |Player weight.                             |
#'    |birthtown     |character |Town of birth.                             |
#'    |birthprov     |character |Province/state of birth.                   |
#'    |birthcntry    |character |Country of birth.                          |
#'    |nationality   |character |Player nationality.                        |
#'    |team_id       |numeric   |Current team unique identifier.            |
#'    |team_name     |character |Current team name.                         |
#'    |team_code     |character |Current team abbreviation.                 |
#'    |image_url     |character |Player headshot URL.                       |
#'    |draft_info    |character |Draft information.                         |
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_player_info(player_id = 28))
#' }

pwhl_player_info <- function(player_id) {

  tryCatch(
    expr = {
      url <- .pwhl_modulekit_url(list(
        view = "player",
        category = "profile",
        player_id = player_id
      ))

      r <- .pwhl_api(url)

      player <- r$SiteKit$Player

      player_df <- data.frame(
        player_id = as.numeric(player_id),
        first_name = as.character(player$first_name %||% NA),
        last_name = as.character(player$last_name %||% NA),
        name = as.character(player$name %||% NA),
        jersey_number = as.character(player$jersey_number %||% NA),
        position = as.character(player$position %||% NA),
        shoots = as.character(player$shoots %||% NA),
        catches = as.character(player$catches %||% NA),
        birthdate = as.character(player$birthdate %||% NA),
        height = as.character(player$height %||% NA),
        weight = as.character(player$weight %||% NA),
        birthtown = as.character(player$birthtown %||% NA),
        birthprov = as.character(player$birthprov %||% NA),
        birthcntry = as.character(player$birthcntry %||% NA),
        nationality = as.character(player$nationality %||% NA),
        team_id = as.numeric(player$most_recent_team_id %||% NA),
        team_name = as.character(player$most_recent_team_name %||% NA),
        team_code = as.character(player$most_recent_team_code %||% NA),
        image_url = as.character(player$primary_image %||% NA),
        draft_info = as.character(player$draft_info %||% NA),
        stringsAsFactors = FALSE
      )

      player_df <- make_fastRhockey_data(
        player_df,
        type = "PWHL Player Info",
        timestamp = Sys.time()
      )

      return(player_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid player_id or no player data available! Error: {e$message}"))
      return(NULL)
    }
  )
}
