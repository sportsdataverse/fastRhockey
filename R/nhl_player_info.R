#' @title **NHL Player Info**
#' @description Returns biographical and career information for an NHL player.
#' Uses the NHL API (`api-web.nhle.com`).
#' @param player_id Integer player ID (e.g., 8476899)
#' @return Returns a data frame with player biographical information.
#' @keywords NHL Player Info
#' @importFrom httr RETRY content
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
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            player <- dplyr::tibble(
                player_id = raw$playerId,
                first_name = raw$firstName$default,
                last_name = raw$lastName$default,
                full_name = paste(raw$firstName$default, raw$lastName$default),
                team_abbr = ifelse(
                    !is.null(raw$currentTeamAbbrev),
                    raw$currentTeamAbbrev,
                    NA_character_
                ),
                team_name = ifelse(
                    !is.null(raw$fullTeamName$default),
                    raw$fullTeamName$default,
                    NA_character_
                ),
                sweater_number = ifelse(
                    !is.null(raw$sweaterNumber),
                    as.integer(raw$sweaterNumber),
                    NA_integer_
                ),
                position = raw$position,
                shoots_catches = ifelse(
                    !is.null(raw$shootsCatches),
                    raw$shootsCatches,
                    NA_character_
                ),
                height_inches = ifelse(
                    !is.null(raw$heightInInches),
                    raw$heightInInches,
                    NA_integer_
                ),
                weight_pounds = ifelse(
                    !is.null(raw$weightInPounds),
                    raw$weightInPounds,
                    NA_integer_
                ),
                birth_date = ifelse(
                    !is.null(raw$birthDate),
                    raw$birthDate,
                    NA_character_
                ),
                birth_city = ifelse(
                    !is.null(raw$birthCity$default),
                    raw$birthCity$default,
                    NA_character_
                ),
                birth_state = ifelse(
                    !is.null(raw$birthStateProvince$default),
                    raw$birthStateProvince$default,
                    NA_character_
                ),
                birth_country = ifelse(
                    !is.null(raw$birthCountry),
                    raw$birthCountry,
                    NA_character_
                ),
                draft_year = ifelse(
                    !is.null(raw$draftDetails$year),
                    raw$draftDetails$year,
                    NA_integer_
                ),
                draft_round = ifelse(
                    !is.null(raw$draftDetails$round),
                    raw$draftDetails$round,
                    NA_integer_
                ),
                draft_pick = ifelse(
                    !is.null(raw$draftDetails$pickInRound),
                    raw$draftDetails$pickInRound,
                    NA_integer_
                ),
                draft_overall = ifelse(
                    !is.null(raw$draftDetails$overallPick),
                    raw$draftDetails$overallPick,
                    NA_integer_
                ),
                draft_team_abbr = ifelse(
                    !is.null(raw$draftDetails$teamAbbrev),
                    raw$draftDetails$teamAbbrev,
                    NA_character_
                ),
                is_active = ifelse(!is.null(raw$isActive), raw$isActive, NA),
                headshot_url = ifelse(
                    !is.null(raw$headshot),
                    raw$headshot,
                    NA_character_
                )
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
