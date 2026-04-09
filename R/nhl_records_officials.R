#' @title **NHL Records - On-Ice Officials**
#' @description Returns NHL on-ice official listings from the NHL Records API
#'   (`https://records.nhl.com/site/api/officials`). Optionally filter by
#'   type (e.g. `"referee"`, `"linesman"`), which switches the resource to
#'   `officials/{type}`.
#' @param type Optional character official type. If supplied, the resource
#'   becomes `officials/{type}`.
#' @return A `fastRhockey_data` tibble of officials, or `NULL` on failure.
#' @keywords NHL Records Officials
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_officials())
#' }
nhl_records_officials <- function(type = NULL) {
    resource <- if (!is.null(type)) {
        glue::glue("officials/{type}")
    } else {
        "officials"
    }
    raw <- .nhl_records_api(resource = resource)
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No officials data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Officials", Sys.time())
}
