#' @keywords internal
#' Internal helper to call a PWHL HockeyTech API endpoint and parse the JSONP response.
#'
#' @param url Full URL including callback parameter
#' @return Parsed JSON as an R list
#' @noRd
.pwhl_api <- function(url) {
  res <- httr::RETRY("GET", url)
  res <- httr::content(res, as = "text", encoding = "utf-8")
  # Strip JSONP callback wrapper: angular.callbacks._X(...)
  res <- sub("^angular\\.callbacks\\._\\w+\\(", "", res)
  res <- sub("\\)\\s*$", "", res)

  jsonlite::parse_json(res)
}

#' @keywords internal
#' Build a PWHL modulekit API URL.
#'
#' @param params Named list of query parameters (excluding key, client_code, callback)
#' @return Full URL string
#' @noRd
.pwhl_modulekit_url <- function(params) {
  base <- "https://lscluster.hockeytech.com/feed/index.php"
  defaults <- list(
    feed = "modulekit",
    key = "446521baf8c38984",
    client_code = "pwhl",
    site_id = "0",
    lang = "en",
    callback = "angular.callbacks._0"
  )
  all_params <- c(params, defaults)
  query <- paste(names(all_params), all_params, sep = "=", collapse = "&")
  paste0(base, "?", query)
}

#' @keywords internal
#' Build a PWHL game center (gc) API URL.
#'
#' @param params Named list of query parameters (excluding key, client_code, callback)
#' @return Full URL string
#' @noRd
.pwhl_gc_url <- function(params) {
  base <- "https://lscluster.hockeytech.com/feed/index.php"
  defaults <- list(
    feed = "gc",
    key = "446521baf8c38984",
    client_code = "pwhl",
    site_id = "0",
    lang = "en",
    callback = "angular.callbacks._0"
  )
  all_params <- c(params, defaults)
  query <- paste(names(all_params), all_params, sep = "=", collapse = "&")
  paste0(base, "?", query)
}
