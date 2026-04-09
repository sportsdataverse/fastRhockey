#' @title **NHL Records - Players by Team**
#' @description Returns every player who has suited up for a given team via
#'   the NHL Records API
#'   (`https://records.nhl.com/site/api/player/byTeam/{team_id}`).
#' @param team_id Integer team ID (required).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A `fastRhockey_data` tibble of players, or `NULL` on failure.
#' @keywords NHL Records Player byTeam
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_player_byteam(team_id = 10))
#' }
nhl_records_player_byteam <- function(team_id, cayenne_exp = NULL) {
    if (missing(team_id) || is.null(team_id)) {
        stop("`team_id` is required for nhl_records_player_byteam().")
    }
    raw <- .nhl_records_api(
        resource = glue::glue("player/byTeam/{team_id}"),
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No player by team data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Player by Team", Sys.time())
}
