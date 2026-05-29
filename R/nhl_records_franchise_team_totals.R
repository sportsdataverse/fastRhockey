#' @title **NHL Records - Franchise Team Totals**
#' @description Returns per-team-name totals from the NHL Records API
#'   (`https://records.nhl.com/site/api/franchise-team-totals`). Useful for
#'   relocated franchises where multiple team names map to one franchise.
#' @param franchise_id Optional integer franchise ID. When supplied it is
#'   translated into a `cayenneExp=franchiseId={franchise_id}` filter unless
#'   `cayenne_exp` is already provided.
#' @param cayenne_exp Optional Cayenne filter expression string. Takes
#'   precedence over `franchise_id` when both are supplied.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                                     |
#'    |:--------------------|:---------|:-----------------------------------------------|
#'    |id                   |integer   |Unique record identifier.                       |
#'    |active_franchise     |integer   |Indicator of whether the franchise is active.   |
#'    |active_team          |logical   |Indicator of whether the team is active.        |
#'    |cups                 |integer   |Number of Stanley Cup championships.            |
#'    |first_season_id      |integer   |Season ID of the team's first season.           |
#'    |franchise_id         |integer   |Unique franchise identifier.                    |
#'    |game_type_id         |integer   |Game type the totals belong to.                 |
#'    |game_win_pctg        |numeric   |Game-winning percentage.                        |
#'    |games_played         |integer   |Total games played.                             |
#'    |goals_against        |integer   |Goals against.                                  |
#'    |goals_for            |integer   |Goals for.                                      |
#'    |home_losses          |integer   |Losses at home.                                 |
#'    |home_overtime_losses |integer   |Overtime losses at home.                        |
#'    |home_ties            |integer   |Ties at home.                                   |
#'    |home_wins            |integer   |Wins at home.                                   |
#'    |last_season_id       |integer   |Season ID of the team's last season.            |
#'    |losses               |integer   |Total losses.                                   |
#'    |overtime_losses      |integer   |Total overtime losses.                          |
#'    |penalty_minutes      |integer   |Penalty minutes.                                |
#'    |playoff_seasons      |integer   |Number of playoff seasons.                      |
#'    |point_pctg           |numeric   |Points percentage.                              |
#'    |points               |integer   |Total standings points.                         |
#'    |road_losses          |integer   |Losses on the road.                             |
#'    |road_overtime_losses |integer   |Overtime losses on the road.                    |
#'    |road_ties            |integer   |Ties on the road.                               |
#'    |road_wins            |integer   |Wins on the road.                               |
#'    |series_losses        |integer   |Playoff series losses.                          |
#'    |series_played        |integer   |Playoff series played.                          |
#'    |series_win_pctg      |numeric   |Playoff series win percentage.                  |
#'    |series_wins          |integer   |Playoff series wins.                            |
#'    |shootout_losses      |integer   |Shootout losses.                                |
#'    |shootout_wins        |integer   |Shootout wins.                                  |
#'    |shutouts             |integer   |Shutouts recorded.                              |
#'    |team_id              |integer   |Unique team identifier.                         |
#'    |team_name            |character |Team name.                                      |
#'    |ties                 |integer   |Total ties.                                     |
#'    |tri_code             |character |Team three-letter code.                         |
#'    |wins                 |integer   |Total wins.                                     |
#' @keywords NHL Records Franchise Team Totals
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_franchise_team_totals())
#' }
nhl_records_franchise_team_totals <- function(
    franchise_id = NULL,
    cayenne_exp = NULL
) {
    if (is.null(cayenne_exp) && !is.null(franchise_id)) {
        cayenne_exp <- glue::glue("franchiseId={franchise_id}")
    }
    raw <- .nhl_records_api(
        resource = "franchise-team-totals",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No franchise team totals data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(
        df,
        "NHL Records Franchise Team Totals",
        Sys.time()
    )
}
