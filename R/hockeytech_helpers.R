#' @keywords internal
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

#' @keywords internal
#' Fetch a HockeyTech URL and strip the JSONP callback wrapper.
#'
#' Strips a leading `angular.callbacks._N(` (or any valid JS identifier
#' followed by `(`) and a trailing `)`, then parses the remaining JSON.
#'
#' @param url Full URL string including callback parameter.
#' @return Parsed JSON as an R list.
#' @noRd
.hockeytech_api <- function(url) {
  res <- .retry_request(url)
  res <- .resp_text(res)
  res <- sub("^[A-Za-z_$][A-Za-z0-9_.$]*\\(", "", res)
  res <- sub("\\)\\s*$", "", res)
  jsonlite::parse_json(res)
}
