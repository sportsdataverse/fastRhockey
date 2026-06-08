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
