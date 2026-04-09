#' @title **NHL Records - Player Listing**
#' @description Returns the player listing from the NHL Records API
#'   (`https://records.nhl.com/site/api/player`). Optionally filter to a
#'   single player via `player_id` (switches resource to `player/{player_id}`)
#'   or pass an arbitrary Cayenne expression.
#' @param player_id Optional integer player ID. If supplied, the resource
#'   becomes `player/{player_id}`.
#' @param cayenne_exp Optional Cayenne filter expression string. Ignored
#'   when `player_id` is supplied (single-player endpoint).
#' @return A `fastRhockey_data` tibble of players, or `NULL` on failure.
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
