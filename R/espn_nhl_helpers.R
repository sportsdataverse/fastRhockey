# Internal ESPN hockey host-helper layer. All helpers are league-generic;
# public espn_nhl_*() wrappers fix league = "nhl".

#' @noRd
.espn_hockey_hosts <- function() {
  list(
    site_v2     = "https://site.api.espn.com/apis/site/v2/sports/hockey",
    site_v2_alt = "https://site.api.espn.com/apis/v2/sports/hockey",
    core_v2     = "https://sports.core.api.espn.com/v2/sports/hockey/leagues",
    web_v3      = "https://site.web.api.espn.com/apis/common/v3/sports/hockey"
  )
}

#' Build an ESPN hockey endpoint URL.
#' @noRd
.espn_hockey_url <- function(host, path = "", league = "nhl") {
  base <- .espn_hockey_hosts()[[host]]
  if (is.null(base)) stop(sprintf("Unknown ESPN host '%s'", host), call. = FALSE)
  url <- paste0(base, "/", league)
  if (nzchar(path)) url <- paste0(url, "/", path)
  url
}

#' Fetch + parse an ESPN hockey endpoint (returns parsed JSON list).
#' @noRd
.espn_hockey_request <- function(host, path = "", query = list(), league = "nhl",
                                 simplifyVector = TRUE, ...) {
  url  <- .espn_hockey_url(host, path, league)
  resp <- .retry_request(url, params = query, ...)
  check_status(resp)
  .resp_json(resp, simplifyVector = simplifyVector)
}

#' Follow a core-v2 $ref URL and return the parsed JSON it points to.
#' @noRd
.espn_follow_ref <- function(ref, simplifyVector = TRUE, ...) {
  if (is.null(ref) || !nzchar(ref)) return(NULL)
  resp <- .retry_request(ref, ...)
  check_status(resp)
  .resp_json(resp, simplifyVector = simplifyVector)
}

#' Parse the trailing path-segment id from a core-v2 \code{\$ref} URL.
#'
#' Strips the query string, splits on \code{/}, and returns the last segment.
#' Works for any core-v2 collection ref, e.g.
#' \code{".../seasons/2026"} → \code{"2026"},
#' \code{".../groups/7?lang=en"} → \code{"7"}.
#'
#' @param ref A \code{\$ref} URL string.
#' @return Character scalar (the trailing id segment), or \code{NA_character_}
#'   if \code{ref} is \code{NULL}/empty.
#' @noRd
.espn_ref_id <- function(ref) {
  if (is.null(ref) || !nzchar(ref %||% "")) return(NA_character_)
  no_qs <- sub("[?].*$", "", ref)
  parts  <- strsplit(no_qs, "/")[[1]]
  tail(parts[nzchar(parts)], 1)
}

#' Convert a core-v2 collection response's \code{items} field to a tibble.
#'
#' Core-v2 collections (\code{seasons}, \code{types}, \code{groups}, etc.)
#' return \code{{ count, pageIndex, pageSize, pageCount, items: [{$ref}] }}.
#' This helper turns the \code{items} data.frame (one column: \code{$ref}) into
#' a tibble with a clean \code{ref} column plus a derived \code{<id_col>}
#' column containing the trailing path-segment id.
#'
#' @param items A data.frame with a \code{$ref} column (as returned by
#'   \code{jsonlite::fromJSON(..., simplifyVector=TRUE)}).
#' @param id_col Name to give the derived id column (default \code{"id"}).
#' @return A \link[dplyr]{tibble} with columns \code{ref} and \code{<id_col>},
#'   or an empty \code{data.frame()} if \code{items} is \code{NULL}/empty.
#' @noRd
.espn_core_collection <- function(items, id_col = "id") {
  if (is.null(items) || (is.data.frame(items) && nrow(items) == 0) ||
      (is.list(items) && length(items) == 0)) {
    return(data.frame())
  }
  refs <- if (is.data.frame(items) && "$ref" %in% colnames(items)) {
    as.character(items[["$ref"]])
  } else if (is.list(items)) {
    vapply(items, function(x) x[["$ref"]] %||% NA_character_, character(1))
  } else {
    return(data.frame())
  }
  ids <- vapply(refs, .espn_ref_id, character(1), USE.NAMES = FALSE)
  out <- data.frame(ref = refs, stringsAsFactors = FALSE)
  out[[id_col]] <- ids
  out
}
