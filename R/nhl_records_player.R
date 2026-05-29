#' @title **NHL Records - Player Listing**
#' @description Returns the player listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/player`). Optionally filter to a
#'   single player via `player_id` (switches resource to `player/{player_id}`)
#'   or pass an arbitrary Cayenne expression.
#' @param player_id Optional integer player ID. If supplied, the resource
#'   becomes `player/{player_id}`.
#' @param cayenne_exp Optional Cayenne filter expression string. Ignored
#'   when `player_id` is supplied (single-player endpoint).
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                  |types     |description                                       |
#'    |:-------------------------|:---------|:-------------------------------------------------|
#'    |id                        |integer   |Unique player identifier.                         |
#'    |accrued_seasons           |integer   |Number of accrued NHL seasons.                    |
#'    |add_names                 |character |Additional names for the player.                  |
#'    |age_sign_waiver           |integer   |Age at which the player signed a waiver.          |
#'    |age_signel_fa             |integer   |Age at which the player signed as a free agent.   |
#'    |alert                     |character |Alert flag or note.                               |
#'    |birth_city                |character |Birth city of the player.                         |
#'    |birth_country             |character |Birth country of the player.                      |
#'    |birth_date                |character |Birth date of the player.                         |
#'    |birth_state_province      |character |Birth state or province of the player.            |
#'    |career_team_id            |integer   |Primary career team identifier.                   |
#'    |central_registry_position |character |Position per the central registry.                |
#'    |club_elec_arb             |character |Club election to arbitration indicator.           |
#'    |current_team_id           |integer   |Current team identifier.                          |
#'    |date_of_death             |character |Date of death, if applicable.                     |
#'    |dda_id                    |integer   |DDA identifier.                                   |
#'    |deceased                  |logical   |Whether the player is deceased.                   |
#'    |ep_player_id              |integer   |EliteProspects player identifier.                 |
#'    |fa_group_after_season     |logical   |Free agent group after the season.                |
#'    |first_name                |character |First name of the player.                         |
#'    |first_signed_by_team_id   |integer   |Team that first signed the player.                |
#'    |free_agent_group          |character |Free agent group classification.                  |
#'    |full_name                 |character |Full name of the player.                          |
#'    |group5election            |character |Group 5 free agency election indicator.           |
#'    |group5seasons_earned      |integer   |Group 5 seasons earned.                           |
#'    |group6proration           |logical   |Group 6 proration indicator.                      |
#'    |group6seasons_earned      |integer   |Group 6 seasons earned.                           |
#'    |groups_earned_thru_season |integer   |Season through which groups were earned.          |
#'    |height                    |integer   |Player height in inches.                          |
#'    |hof_induction_year        |integer   |Hockey Hall of Fame induction year.               |
#'    |home_town                 |character |Home town of the player.                          |
#'    |iihf_hof_induction_year   |integer   |IIHF Hall of Fame induction year.                 |
#'    |in_hockey_hof             |logical   |Whether the player is in the Hockey Hall of Fame. |
#'    |in_iihf_hof               |integer   |Whether the player is in the IIHF Hall of Fame.   |
#'    |in_top100alltime          |logical   |Whether the player is in the all-time top 100.    |
#'    |in_us_hockey_hof          |logical   |Whether the player is in the US Hockey Hall of Fame.|
#'    |is_defected               |character |Whether the player defected.                      |
#'    |is_deleted                |character |Whether the record is deleted.                    |
#'    |is_junior                 |character |Whether the player is a junior.                   |
#'    |is_retired                |logical   |Whether the player is retired.                    |
#'    |is_rookie                 |character |Whether the player is a rookie.                   |
#'    |is_suspended              |character |Whether the player is suspended.                  |
#'    |last_amateur_league_id    |integer   |Last amateur league identifier.                   |
#'    |last_amateur_team_id      |integer   |Last amateur team identifier.                     |
#'    |last_nhl_team_id          |integer   |Last NHL team identifier.                          |
#'    |last_name                 |character |Last name of the player.                          |
#'    |loan_cap_exception        |character |Loan cap exception indicator.                     |
#'    |long_term_injury          |character |Long-term injury indicator.                       |
#'    |message                   |character |Status message.                                   |
#'    |middle_name               |character |Middle name of the player.                        |
#'    |nationality               |character |Nationality of the player.                        |
#'    |nhl_experience            |integer   |Years of NHL experience.                          |
#'    |on_roster                 |character |Whether the player is on a roster.                |
#'    |platform_year             |integer   |Platform year for contract purposes.              |
#'    |position                  |character |Player position.                                  |
#'    |pr_name                   |character |Public relations display name.                    |
#'    |pr_stat                   |integer   |Public relations status code.                     |
#'    |pro_year_reduction        |integer   |Professional year reduction.                      |
#'    |reentry_waivers           |character |Re-entry waivers indicator.                       |
#'    |roster_special_code       |character |Roster special status code.                       |
#'    |salary_arbitration_exp    |integer   |Salary arbitration experience.                    |
#'    |shoots_catches            |character |Handedness (shoots or catches).                   |
#'    |sweater_number            |integer   |Sweater number worn by the player.                |
#'    |update_timestamp          |character |Timestamp of the last record update.             |
#'    |us_hof_induction_year     |integer   |US Hockey Hall of Fame induction year.            |
#'    |vet_cap_excptn            |character |Veteran cap exception indicator.                  |
#'    |waiver_amount             |integer   |Waiver amount.                                    |
#'    |waiver_draft              |character |Waiver draft indicator.                           |
#'    |waiver_status             |character |Waiver status.                                    |
#'    |weight                    |integer   |Player weight in pounds.                          |
#'    |years_pro                 |integer   |Number of professional years played.             |
#' @keywords NHL Records Player
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_player())
#' }
nhl_records_player <- function(player_id = NULL, cayenne_exp = NULL) {
    if (!is.null(player_id)) {
        resource <- glue::glue("player/{player_id}")
        raw <- .nhl_records_api(resource = resource)
    } else {
        raw <- .nhl_records_api(
            resource = "player",
            cayenne_exp = cayenne_exp
        )
    }
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No player data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Player", Sys.time())
}
