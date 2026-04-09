#' @title NHL Edge API helper (internal)
#' @description Builds an `https://api-web.nhle.com/v1/{path}` request,
#'   issues a `GET` with retry, and parses the JSON response. Returns the
#'   parsed list (or `NULL` on failure). Used by every `nhl_edge_*()` and
#'   `nhl_cat_edge_*()` wrapper.
#'
#' @details
#' The Edge family of endpoints all share the same shape: a base path
#' followed by either `/{season}/{gameType}` or `/now`. This helper handles
#' both forms via the `now` flag.
#'
#' @param base Character base path **without** the leading `v1/edge/` or
#'   `v1/cat/edge/` prefix and **without** the trailing `/{season}/{gameType}`
#'   or `/now` suffix. The wrapper supplies that suffix.
#' @param season Character or numeric season. May be:
#'   * `NULL` (default) — fetches the `/now` endpoint
#'   * a numeric year like `2025` — converted to API form `"20242025"`
#'   * a pre-formatted 8-character string like `"20242025"` — used as-is
#' @param game_type Integer game type (1 = preseason, 2 = regular season,
#'   3 = playoffs). Ignored when `season` is `NULL`.
#' @param prefix Character URL prefix. Defaults to `"v1/edge"`. Pass
#'   `"v1/cat/edge"` for the categorical (CAT) variants.
#' @param query Optional named list of query string parameters.
#' @return Parsed JSON list returned by `jsonlite::fromJSON(..., flatten =
#'   TRUE)`, or `NULL` on error.
#' @keywords Internal
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @noRd
.nhl_edge_api <- function(
    base,
    season = NULL,
    game_type = 2L,
    prefix = "v1/edge",
    query = NULL
) {
    api_season <- .nhl_edge_format_season(season)

    if (is.null(api_season)) {
        url <- glue::glue(
            "https://api-web.nhle.com/{prefix}/{base}/now"
        )
    } else {
        url <- glue::glue(
            "https://api-web.nhle.com/{prefix}/{base}/{api_season}/{game_type}"
        )
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url, query = query)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            jsonlite::fromJSON(resp_text, flatten = TRUE)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching {url}: {e$message}"
            ))
            return(NULL)
        }
    )
}

#' @title Format an NHL season for the Edge / api-web URL
#' @description Internal helper that normalizes the various forms users may
#'   pass for `season`. Returns `NULL` (signal `/now`), a pre-formatted
#'   8-character season string, or `NULL` on a parse failure.
#' @param season `NULL`, a 4-digit numeric end-year (e.g. `2025` for the
#'   2024-25 season), or an 8-character API-form string (e.g. `"20242025"`).
#' @return Character season in 8-digit form, or `NULL` if `season` is `NULL`.
#' @keywords Internal
#' @noRd
.nhl_edge_format_season <- function(season) {
    if (is.null(season)) {
        return(NULL)
    }
    season <- as.character(season)
    if (nchar(season) == 8L) {
        return(season)
    }
    if (nchar(season) == 4L) {
        end_year <- as.integer(season)
        return(paste0(end_year - 1L, end_year))
    }
    stop(
        "`season` must be NULL, a 4-digit end year (e.g. 2025), or ",
        "an 8-character API season (e.g. \"20242025\")."
    )
}

#' @title Convert a parsed Edge API response to a data.frame
#' @description Internal helper shared by every `nhl_edge_skater_*()` wrapper
#'   (and the `nhl_cat_edge_*()` variants). The Edge endpoints return a
#'   variety of response shapes: some return a top-level `data.frame`, some
#'   return a named list that contains a single `data.frame` element, and
#'   some return a flat named list that maps one-to-one to a single row.
#'   This helper normalises those shapes to a `data.frame` (or `NULL` if no
#'   tabular payload can be extracted).
#' @param raw Parsed JSON object as returned by `.nhl_edge_api()`.
#' @return A `data.frame` (possibly empty) or `NULL`.
#' @keywords Internal
#' @importFrom dplyr as_tibble
#' @noRd
.nhl_edge_to_df <- function(raw) {
    if (is.null(raw)) {
        return(NULL)
    }
    if (is.data.frame(raw)) {
        return(raw)
    }
    if (is.list(raw)) {
        # Look for a list element that is itself a data.frame
        for (nm in names(raw)) {
            x <- raw[[nm]]
            if (is.data.frame(x)) {
                return(x)
            }
        }
        # Fallback: try to convert flat (non-list) elements to a single-row
        # tibble.
        is_list_elem <- vapply(raw, is.list, logical(1))
        flat <- raw[!is_list_elem]
        if (length(flat)) {
            flat <- lapply(flat, function(v) if (length(v) == 0) NA else v)
            return(dplyr::as_tibble(flat))
        }
    }
    NULL
}
