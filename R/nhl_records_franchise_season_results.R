#' @title **NHL Records - Franchise Season Results**
#' @description Returns franchise season-by-season results from the NHL Records
#'   API (`https://records.nhl.com/site/api/franchise-season-results`).
#' @param franchise_id Optional integer franchise ID. When supplied it is
#'   translated into a `cayenneExp=franchiseId={franchise_id}` filter unless
#'   `cayenne_exp` is already provided.
#' @param cayenne_exp Optional Cayenne filter expression string. Takes
#'   precedence over `franchise_id` when both are supplied.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                                |
#'    |:--------------------|:---------|:------------------------------------------|
#'    |id                   |integer   |Unique season result record identifier.    |
#'    |conference_abbrev    |character |Conference abbreviation.                   |
#'    |conference_name      |character |Conference name.                           |
#'    |conference_sequence  |integer   |Conference standings position.             |
#'    |decision             |character |Series/season decision indicator.          |
#'    |division_abbrev      |character |Division abbreviation.                     |
#'    |division_name        |character |Division name.                             |
#'    |division_sequence    |integer   |Division standings position.               |
#'    |final_playoff_round  |integer   |Final playoff round reached.               |
#'    |franchise_id         |integer   |Unique franchise identifier.               |
#'    |game_type_id         |integer   |Game type identifier (regular/playoffs).   |
#'    |games_played         |integer   |Games played in the season.                |
#'    |goals                |integer   |Goals scored.                              |
#'    |goals_against        |integer   |Goals against.                             |
#'    |home_losses          |integer   |Losses at home.                            |
#'    |home_overtime_losses |integer   |Overtime losses at home.                   |
#'    |home_ties            |integer   |Ties at home.                              |
#'    |home_wins            |integer   |Wins at home.                              |
#'    |in_playoffs          |logical   |Whether the season reached the playoffs.   |
#'    |league_sequence      |integer   |League standings position.                 |
#'    |losses               |integer   |Total losses.                              |
#'    |overtime_losses      |integer   |Total overtime losses.                     |
#'    |penalty_minutes      |integer   |Penalty minutes.                           |
#'    |playoff_round        |integer   |Playoff round identifier.                  |
#'    |points               |integer   |Standings points earned.                   |
#'    |road_losses          |integer   |Losses on the road.                        |
#'    |road_overtime_losses |integer   |Overtime losses on the road.               |
#'    |road_ties            |integer   |Ties on the road.                          |
#'    |road_wins            |integer   |Wins on the road.                          |
#'    |season_id            |integer   |Season identifier.                         |
#'    |series_abbrev        |character |Playoff series abbreviation.               |
#'    |series_title         |character |Playoff series title.                      |
#'    |shutouts             |integer   |Shutouts recorded.                         |
#'    |team_id              |integer   |Unique team identifier.                    |
#'    |team_name            |character |Team name.                                 |
#'    |ties                 |integer   |Total ties.                                |
#'    |tri_code             |character |Team three-letter code.                    |
#'    |wins                 |integer   |Total wins.                                |
#' @keywords NHL Records Franchise Season Results
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_franchise_season_results())
#' }
nhl_records_franchise_season_results <- function(
    franchise_id = NULL,
    cayenne_exp = NULL
) {
    if (is.null(cayenne_exp) && !is.null(franchise_id)) {
        cayenne_exp <- glue::glue("franchiseId={franchise_id}")
    }
    raw <- .nhl_records_api(
        resource = "franchise-season-results",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No franchise season results data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Franchise Season Results",
        Sys.time()
    )
}
