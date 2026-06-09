#' @title **NHL Player Info**
#' @description Returns biographical and career information for an NHL player.
#' Uses the NHL API (`api-web.nhle.com`).
#' @param player_id Integer player ID (e.g., 8476899)
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                  |
#'    |:---------------|:---------|:--------------------------------------------|
#'    |player_id       |integer   |Unique player identifier.                    |
#'    |first_name      |character |Player first name.                           |
#'    |last_name       |character |Player last name.                            |
#'    |full_name       |character |Player full name.                            |
#'    |team_abbr       |character |Current team abbreviation.                   |
#'    |team_name       |character |Current team name.                           |
#'    |sweater_number  |integer   |Player sweater (jersey) number.              |
#'    |position        |character |Player position.                             |
#'    |shoots_catches  |character |Handedness (shoots or catches).              |
#'    |height_inches   |integer   |Player height in inches.                     |
#'    |weight_pounds   |integer   |Player weight in pounds.                     |
#'    |birth_date      |character |Player date of birth.                        |
#'    |birth_city      |character |City of birth.                               |
#'    |birth_state     |character |State or province of birth.                  |
#'    |birth_country   |character |Country of birth.                            |
#'    |draft_year      |integer   |Year the player was drafted.                 |
#'    |draft_round     |integer   |Draft round.                                 |
#'    |draft_pick      |integer   |Draft pick within the round.                 |
#'    |draft_overall   |integer   |Overall draft selection number.              |
#'    |draft_team_abbr |character |Abbreviation of the drafting team.           |
#'    |is_active       |logical   |Whether the player is currently active.      |
#'    |headshot_url    |character |URL of the player headshot image.            |
#' @keywords NHL Player Info
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_player_info(player_id = 8476899))
#' }
nhl_player_info <- function(player_id) {
    url <- glue::glue("https://api-web.nhle.com/v1/player/{player_id}/landing")

    tryCatch(
        expr = {
            res <- .retry_request(url)
            check_status(res)

            resp_text <- .resp_text(res)
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            # Use if/else (not ifelse) for NULL guards. ifelse() returns a
            # result of length(test) — i.e. length 1 for a scalar !is.null()
            # — so when the field is a vector longer than 1, ifelse silently
            # collapses it to length 1 and tibble then recycles that single
            # value across every row. This function currently builds a
            # 1-row tibble so the bug is masked today, but the latent shape
            # is identical to the bug fixed in .parse_*_games(); switching
            # to if/else now removes the foot-gun before it surfaces.
            player <- dplyr::tibble(
                player_id = raw$playerId,
                first_name = raw$firstName$default,
                last_name = raw$lastName$default,
                full_name = paste(raw$firstName$default, raw$lastName$default),
                team_abbr = if (!is.null(raw$currentTeamAbbrev))
                    raw$currentTeamAbbrev
                else
                    NA_character_,
                team_name = if (!is.null(raw$fullTeamName$default))
                    raw$fullTeamName$default
                else
                    NA_character_,
                sweater_number = if (!is.null(raw$sweaterNumber))
                    as.integer(raw$sweaterNumber)
                else
                    NA_integer_,
                position = raw$position,
                shoots_catches = if (!is.null(raw$shootsCatches))
                    raw$shootsCatches
                else
                    NA_character_,
                height_inches = if (!is.null(raw$heightInInches))
                    raw$heightInInches
                else
                    NA_integer_,
                weight_pounds = if (!is.null(raw$weightInPounds))
                    raw$weightInPounds
                else
                    NA_integer_,
                birth_date = if (!is.null(raw$birthDate))
                    raw$birthDate
                else
                    NA_character_,
                birth_city = if (!is.null(raw$birthCity$default))
                    raw$birthCity$default
                else
                    NA_character_,
                birth_state = if (!is.null(raw$birthStateProvince$default))
                    raw$birthStateProvince$default
                else
                    NA_character_,
                birth_country = if (!is.null(raw$birthCountry))
                    raw$birthCountry
                else
                    NA_character_,
                draft_year = if (!is.null(raw$draftDetails$year))
                    raw$draftDetails$year
                else
                    NA_integer_,
                draft_round = if (!is.null(raw$draftDetails$round))
                    raw$draftDetails$round
                else
                    NA_integer_,
                draft_pick = if (!is.null(raw$draftDetails$pickInRound))
                    raw$draftDetails$pickInRound
                else
                    NA_integer_,
                draft_overall = if (!is.null(raw$draftDetails$overallPick))
                    raw$draftDetails$overallPick
                else
                    NA_integer_,
                draft_team_abbr = if (!is.null(raw$draftDetails$teamAbbrev))
                    raw$draftDetails$teamAbbrev
                else
                    NA_character_,
                is_active = if (!is.null(raw$isActive)) raw$isActive else NA,
                headshot_url = if (!is.null(raw$headshot))
                    raw$headshot
                else
                    NA_character_
            )

            player <- make_fastRhockey_data(
                player,
                "NHL Player Info",
                Sys.time()
            )
            return(player)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching player info for {player_id}: {e$message}"
            ))
            return(NULL)
        }
    )
}
