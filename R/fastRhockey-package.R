#' @keywords internal
#'
#' @section Proxy support:
#' All HTTP requests flow through an internal httr2 helper that resolves a proxy
#' in this order:
#' \enumerate{
#'   \item an explicit `proxy =` argument (only on wrappers that pass `...`);
#'   \item `getOption("fastRhockey.proxy")` -- set once per session with
#'         `options(fastRhockey.proxy = "http://host:port")` and every call
#'         picks it up;
#'   \item the `http_proxy` / `https_proxy` / `no_proxy` environment variables,
#'         read by libcurl when no explicit proxy is supplied.
#' }
#' The proxy value may be a URL string (`"http://host:port"`) or a named list
#' spread into [httr2::req_proxy()] for authenticated proxies, e.g.
#' `list(url = "http://host", port = 8080, username = "u", password = "p",
#' auth = "basic")`. The session-`options()` form is recommended when one proxy
#' covers many calls.
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
