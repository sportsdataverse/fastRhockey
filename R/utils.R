#' Check Status function
#' @param res Response from API
#' @keywords Internal
#' @import rvest
#'
check_status <- function(res) {

  x = httr::status_code(res)

  if(x != 200) stop("The API returned an error", call. = FALSE)

}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @keywords internal
"_PACKAGE"

#' @importFrom Rcpp getRcppVersion
#' @importFrom RcppParallel defaultNumThreads
NULL
