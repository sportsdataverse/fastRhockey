#' @title  **PWHL Player Search**
#' @description Search for PWHL players by name.
#'
#' @param search_term Character string to search for (e.g., a player name or partial name).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name      |types     |description                                |
#'    |:-------------|:---------|:------------------------------------------|
#'    |player_id     |numeric   |Unique player identifier.                  |
#'    |first_name    |character |Player first name.                         |
#'    |last_name     |character |Player last name.                          |
#'    |position      |character |Player position.                           |
#'    |team_id       |numeric   |Unique team identifier.                    |
#'    |person_id     |numeric   |Unique person identifier.                  |
#'    |team_name     |character |Most recent team name.                     |
#'    |team_code     |character |Most recent team abbreviation.             |
#'    |jersey_number |character |Jersey number.                             |
#'    |shoots        |character |Shooting hand.                             |
#'    |catches       |character |Catching hand (goalies).                   |
#'    |height        |character |Player height.                             |
#'    |weight        |character |Player weight.                             |
#'    |birthdate     |character |Date of birth.                             |
#'    |image_url     |character |Player headshot URL.                       |
#' @import jsonlite
#' @import dplyr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_player_search(search_term = "Poulin"))
#' }

pwhl_player_search <- function(search_term) {

  tryCatch(
    expr = {
      url <- .pwhl_modulekit_url(list(
        view = "searchplayers",
        search_term = utils::URLencode(search_term)
      ))

      r <- .pwhl_api(url)

      players_raw <- r$SiteKit$Searchplayers

      players <- data.frame()

      for (i in seq_along(players_raw)) {
        p <- players_raw[[i]]
        player_row <- data.frame(
          player_id = as.numeric(p$player_id %||% NA),
          first_name = as.character(p$first_name %||% NA),
          last_name = as.character(p$last_name %||% NA),
          position = as.character(p$position %||% NA),
          team_id = as.numeric(p$team_id %||% NA),
          person_id = as.numeric(p$person_id %||% NA),
          team_name = as.character(p$last_team_name %||% NA),
          team_code = as.character(p$last_team_code %||% NA),
          jersey_number = as.character(p$jersey_number %||% NA),
          shoots = as.character(p$shoots %||% NA),
          catches = as.character(p$catches %||% NA),
          height = as.character(p$height %||% NA),
          weight = as.character(p$weight %||% NA),
          birthdate = as.character(p$birthdate %||% NA),
          image_url = as.character(p$profile_image %||% NA),
          stringsAsFactors = FALSE
        )
        players <- dplyr::bind_rows(players, player_row)
      }

      if (nrow(players) == 0) {
        message(glue::glue("{Sys.time()}: No players found matching '{search_term}'."))
        return(NULL)
      }

      players <- make_fastRhockey_data(
        players,
        type = "PWHL Player Search",
        timestamp = Sys.time()
      )

      return(players)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error searching for players. {e$message}"))
      return(NULL)
    }
  )
}
