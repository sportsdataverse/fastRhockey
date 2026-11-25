#' Build a HockeyTech feed URL for any league.
#'
#' The `gc` feed uses a `tab` parameter (not `view`); all other feeds use
#' `view`. This matches a real HockeyTech quirk -- gc with `view=` returns
#' "Undefined Tab".
#'
#' @param league   League key, e.g. `"pwhl"`, `"ahl"`, `"ohl"`.
#' @param feed     Feed name, e.g. `"modulekit"`, `"gc"`, `"statviewfeed"`.
#' @param view     View/tab name, e.g. `"seasons"`, `"gamesummary"`.
#' @param params   Named list of additional query parameters.
#' @return Full URL string.
#' @noRd
#' @keywords internal
.hockeytech_url <- function(league, feed, view, params = list()) {
  cfg <- .hockeytech_leagues()[[league]]
  base_params <- list(
    feed        = feed,
    key         = .hockeytech_resolve_key(league, view = view),
    client_code = cfg$client_code,
    site_id     = cfg$site_id,
    lang        = "en",
    callback    = "angular.callbacks._0"
  )
  if (identical(feed, "gc")) {
    base_params$tab <- view
  } else {
    base_params$view <- view
  }
  all_params <- c(params, base_params)
  query <- paste(names(all_params), all_params, sep = "=", collapse = "&")
  paste0(cfg$base_url, "?", query)
}

#' Fetch a HockeyTech URL and strip the JSONP callback wrapper.
#'
#' Strips a leading `angular.callbacks._N(` (or any valid JS identifier
#' followed by `(`) and a trailing `)`, then parses the remaining JSON. A
#' non-2xx reply raises a `fastRhockey_http_error` instead of being parsed.
#' This is the one place HockeyTech JSONP is stripped; wrappers call it (or
#' `.pwhl_api()`) rather than matching the callback tail themselves.
#'
#' @param url Full URL string including callback parameter.
#' @return Parsed JSON as an R list.
#' @noRd
#' @keywords internal
.hockeytech_api <- function(url) {
  res <- .retry_request(url)
  .fastRhockey_stop_for_http_status(res)
  res <- .resp_text(res)
  res <- sub("^[A-Za-z_$][A-Za-z0-9_.$]*\\(", "", res)
  res <- sub("\\)\\s*$", "", res)
  jsonlite::parse_json(res)
}
