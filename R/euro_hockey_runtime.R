# euro_hockey_runtime.R -- shared transport for the European hockey families.
#
# Mirrors sdv-py sportsdataverse/hockey/euro_hockey_runtime.py. Every European
# provider is wrapped politely: at least one second between two requests to the
# same host, on EVERY attempt, retries included. httr2::req_throttle() cannot
# do that (httr2 1.3 throttles once, before its retry loop, and not at all for
# mocked responses), so the per-host clock lives here and .polite_perform()
# runs the retry loop itself.
#
#   .euro_get(url, query)            plain JSON GET (most families)
#   .build_request() + httr2 verbs   a family with its own transport (KHL
#     + .polite_perform(req)         session + POST form, HA POST JSON, VHL
#     + .euro_resp_json(resp)        req_options(resolve = ...)), still throttled
#   .polite_wait(url)                the clock itself, for anything else
#
# Error vocabulary (see the classed conditions in utils.R): 404 ->
# fastRhockey_no_data; 400/422 -> fastRhockey_bad_request; any other non-2xx,
# a connection failure after retries, an empty or non-JSON body ->
# fastRhockey_asset_fetch.

#' Minimum seconds between two requests to one host (Phase B ruling).
#' @noRd
.EURO_MIN_INTERVAL <- 1

# host -> elapsed-clock time of its last reserved request slot
.polite_clock <- new.env(parent = emptyenv())

#' Sleep until `min_interval` seconds have passed since the last request to
#' `url`'s host, and reserve this request's slot.
#'
#' @param url The URL about to be requested; only its host is used.
#' @param min_interval Spacing to enforce, in seconds.
#' @return The seconds slept, invisibly.
#' @keywords internal
#' @noRd
.polite_wait <- function(url, min_interval = .EURO_MIN_INTERVAL) {
  host <- httr2::url_parse(url)$hostname
  now <- proc.time()[["elapsed"]]
  slot <- max(now, (.polite_clock[[host]] %||% -Inf) + min_interval)
  .polite_clock[[host]] <- slot
  if (slot > now) Sys.sleep(slot - now)
  invisible(slot - now)
}

#' Statuses `.polite_perform()` retries.
#' @noRd
.EURO_RETRY_STATUSES <- c(429L, 500L, 502L, 503L, 504L)

#' Cap on a server's `Retry-After`, in seconds (sdv-py `dl_utils._MAX_RETRY_AFTER`).
#' @noRd
.EURO_MAX_RETRY_AFTER <- 120

#' Perform an httr2 request, throttled per host on every attempt.
#'
#' Retries a 429/500/502/503/504 or a connection failure up to `max_tries`
#' attempts (any other status is returned at once). Each attempt first waits its
#' host's slot; after failed attempt k the next waits at least
#' `max(min_interval, backoff * 2^(k - 1))` seconds after the host's last
#' request (1, 2, 4, ... for `backoff = 1`), or the server's `Retry-After`
#' (seconds, capped at 120) when that is longer. Non-2xx responses are
#' returned, not raised: pass the response to `.euro_check_status()` or
#' `.euro_resp_json()`.
#'
#' @param req An httr2 request (e.g. from `.build_request()`). Any
#'   `req_retry()` / `req_error()` policy on it is replaced.
#' @param max_tries Maximum attempts.
#' @param min_interval Spacing between requests to one host, in seconds.
#' @param backoff Base of the retry delay in seconds (1: 1, 2, 4; 2: 2, 4, 8).
#' @param retry_statuses Statuses to retry; a family whose provider answers 500 for a
#'   bad request (Liiga) narrows it.
#' @return An httr2 response.
#' @keywords internal
#' @noRd
.polite_perform <- function(req, max_tries = 3, min_interval = .EURO_MIN_INTERVAL, backoff = 1,
                            retry_statuses = .EURO_RETRY_STATUSES) {
  req <- req |>
    httr2::req_retry(max_tries = 1) |>
    httr2::req_error(is_error = function(resp) FALSE)
  interval <- min_interval
  for (i in seq_len(max_tries)) {
    .polite_wait(req$url, interval)
    resp <- tryCatch(httr2::req_perform(req), httr2_failure = function(e) e)
    failed <- inherits(resp, "httr2_failure")
    if (!failed && !httr2::resp_status(resp) %in% retry_statuses) return(resp)
    interval <- max(min_interval, backoff * 2^(i - 1))
    if (!failed) {
      ra <- suppressWarnings(httr2::resp_retry_after(resp))
      if (length(ra) == 1L && !is.na(ra)) interval <- max(interval, min(ra, .EURO_MAX_RETRY_AFTER))
    }
  }
  if (inherits(resp, "httr2_failure")) {
    where <- .euro_where(req$url)
    .abort_asset_fetch("{where}: fetch failed after {max_tries} attempt{?s}: {conditionMessage(resp)}",
                       parent = resp)
  }
  resp
}

#' `host/path` of a URL, never its query string (which may carry a key).
#' @noRd
.euro_where <- function(url) sub("^[a-z]+://", "", sub("[?#].*$", "", url))

#' Raise unless the response is 2xx (the sdv-py `_check_status` vocabulary).
#'
#' @param resp An httr2 response.
#' @param where Label for messages; defaults to the request's `host/path`.
#' @return `resp`, invisibly, when it is 2xx.
#' @keywords internal
#' @noRd
.euro_check_status <- function(resp, where = .euro_where(resp$url)) {
  status <- httr2::resp_status(resp)
  if (status >= 200 && status < 300) return(invisible(resp))
  excerpt <- substr(gsub("\\s+", " ", tryCatch(.resp_text(resp), error = function(e) "")), 1, 200)
  if (status == 404) .abort_no_data("{where} answered HTTP 404.")
  if (status %in% c(400L, 422L)) .abort_bad_request("{where} rejected the request: HTTP {status}: {excerpt}")
  .abort_asset_fetch("{where} answered HTTP {status}: {excerpt}")
}

#' Decode a response as JSON under the error vocabulary.
#'
#' Non-2xx raises per `.euro_check_status()`; 204/205 is `list()`; an empty or
#' non-JSON 2xx body is `fastRhockey_asset_fetch` (an empty 200 is a block or a
#' rejected parameter, not "nothing").
#'
#' @param resp An httr2 response.
#' @param ... Passed to [jsonlite::fromJSON()] (e.g. `simplifyVector = FALSE`).
#' @return The parsed body.
#' @keywords internal
#' @noRd
.euro_resp_json <- function(resp, ...) {
  .euro_check_status(resp)
  status <- httr2::resp_status(resp)
  if (status %in% c(204L, 205L)) return(list())
  where <- .euro_where(resp$url)
  # resp_body_string() raises its own error on a truly empty body
  text <- if (httr2::resp_has_body(resp)) .resp_text(resp) else ""
  if (!nzchar(trimws(text))) .abort_asset_fetch("{where} answered HTTP {status} with an empty body.")
  tryCatch(
    jsonlite::fromJSON(text, ...),
    error = function(e) {
      excerpt <- substr(gsub("\\s+", " ", text), 1, 200)
      .abort_asset_fetch("{where} answered HTTP {status} with a non-JSON body: {excerpt}", call = NULL)
    }
  )
}

#' Throttled JSON GET: `.build_request()` -> `.polite_perform()` -> `.euro_resp_json()`.
#'
#' Honours `getOption("fastRhockey.proxy")` like every other wrapper.
#'
#' @param url Fully built request URL.
#' @param query Named list of query parameters; `NULL` entries are dropped.
#' @param headers Named list of request headers.
#' @param ... Passed to [jsonlite::fromJSON()].
#' @param retry_statuses Statuses `.polite_perform()` retries.
#' @return The parsed JSON body.
#' @keywords internal
#' @noRd
.euro_get <- function(url, query = list(), headers = NULL, ..., retry_statuses = .EURO_RETRY_STATUSES) {
  query <- Filter(Negate(is.null), query)
  .build_request(url, query, headers) |>
    .polite_perform(retry_statuses = retry_statuses) |>
    .euro_resp_json(...)
}

#' Map a season's END year (the sdv key, 2027 = 2026/27) to the START year a
#' provider keys it by (eSports buckets, ELH, HockeyAllsvenskan, SHL's athlete
#' game log).
#'
#' @param season Season END year.
#' @return Integer START year, `season - 1`.
#' @keywords internal
#' @noRd
.season_start_year <- function(season) {
  end <- suppressWarnings(as.integer(season))
  if (length(end) != 1 || is.na(end) || end < 1000 || end > 9999) {
    .abort_bad_request("{.arg season} must be a four-digit END year (2027 = 2026/27), not {.val {season}}.")
  }
  end - 1L
}
