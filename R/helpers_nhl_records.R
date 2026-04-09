#' @title NHL Records API helper (internal)
#' @description Builds an `https://records.nhl.com/site/api/{resource}`
#'   request, supports `cayenneExp` filtering plus pagination, issues a
#'   GET with retry, and parses the JSON response.
#'
#'   The records API responds with `{"data": [...], "total": N}` for
#'   tabular endpoints, mirroring the Stats REST API. This helper extracts
#'   `data` and returns it as a `data.frame`. For non-tabular endpoints
#'   (or any response without a `data` element) the raw parsed list is
#'   returned instead.
#' @param resource Character resource path (without leading slash), e.g.
#'   `"franchise"`, `"player/byTeam/10"`, `"draft-lottery-odds"`.
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @param sort Optional sort expression (JSON string or column name).
#' @param limit Optional integer page size.
#' @param start Optional integer pagination offset.
#' @param query Optional named list of additional query parameters.
#' @return A `data.frame` if the response contains a `data` element,
#'   otherwise the raw parsed list, or `NULL` on failure.
#' @keywords Internal
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @noRd
.nhl_records_api <- function(
    resource,
    cayenne_exp = NULL,
    sort = NULL,
    limit = NULL,
    start = NULL,
    query = NULL
) {
    base_url <- glue::glue("https://records.nhl.com/site/api/{resource}")
    q <- list()
    if (!is.null(cayenne_exp)) q$cayenneExp <- cayenne_exp
    if (!is.null(sort)) q$sort <- sort
    if (!is.null(limit)) q$limit <- limit
    if (!is.null(start)) q$start <- start
    if (!is.null(query) && is.list(query)) q <- c(q, query)

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", base_url, query = q)
            check_status(res)
            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)
            if (is.list(raw) && !is.null(raw$data)) {
                return(raw$data)
            }
            return(raw)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching records resource '{resource}': {e$message}"
            ))
            return(NULL)
        }
    )
}
