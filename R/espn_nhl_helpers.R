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

#' The exported name behind a league-generic helper, for messages:
#' `.espn_fn("mens-college-hockey", "game")` is `"espn_mch_game"`.
#' @noRd
.espn_fn <- function(league, suffix) {
  prefix <- c(nhl = "nhl", "mens-college-hockey" = "mch", "womens-college-hockey" = "wch")
  p <- unname(prefix[league[1]])
  paste0("espn_", if (is.na(p)) league[1] else p, "_", suffix)
}

#' The name of the function that called the calling helper (past any other
#' `.espn_hockey_*()` helper), for a helper shared by exports with different
#' names (`.espn_hockey_items()`); `fallback` when that caller has no plain
#' name (a `do.call()` of a function object). Call it directly in the helper's
#' body, not inside a promise.
#' @noRd
.espn_caller <- function(fallback) {
  k <- 2L
  while (sys.nframe() > k) {
    f <- sys.call(-k)[[1L]]
    if (is.call(f) && as.character(f[[1L]])[1] %in% c("::", ":::")) f <- f[[3L]]
    if (!is.name(f)) return(fallback)
    nm <- as.character(f)
    if (!startsWith(nm, ".espn_hockey_")) return(nm)
    k <- k + 1L
  }
  fallback
}

#' Abort unless `id` is a single, non-missing, non-empty id.
#'
#' `x$id[1]` of an empty listing is `NULL`; pasted into a path it requests the
#' parent collection and fails later on a 1-row frame built from a 0-length id.
#' Call it inside the wrapper's `tryCatch()` so the message is reported the
#' usual way, before any request.
#' @noRd
.espn_check_id <- function(id, arg = rlang::caller_arg(id)) {
  if (length(id) != 1L || is.na(id) || !nzchar(as.character(id))) {
    cli::cli_abort("{.arg {arg}} must be a single, non-missing ESPN id.", call = NULL)
  }
  invisible(id)
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
#' \code{".../seasons/2026"} -> \code{"2026"},
#' \code{".../groups/7?lang=en"} -> \code{"7"}.
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
