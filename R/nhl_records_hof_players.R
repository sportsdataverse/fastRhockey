#' @title **NHL Records - Hall of Fame Players**
#' @description Returns Hall of Fame inductees from the NHL Records API
#'   (`https://records.nhl.com/site/api/hof/players`). Optionally filter by
#'   office ID (switches resource to `hof/players/{office_id}`).
#' @param office_id Optional integer office/category ID. If supplied, the
#'   resource becomes `hof/players/{office_id}`.
#' @return A `fastRhockey_data` tibble of HOF inductees, or `NULL` on failure.
#' @keywords NHL Records Hall of Fame Players
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_hof_players())
#' }
nhl_records_hof_players <- function(office_id = NULL) {
    resource <- if (!is.null(office_id)) {
        glue::glue("hof/players/{office_id}")
    } else {
        "hof/players"
    }
    raw <- .nhl_records_api(resource = resource)
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No HOF players data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records HOF Players", Sys.time())
}
