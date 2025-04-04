.datatable.aware <- TRUE

#' **Progressively**
#'
#' This function helps add progress-reporting to any function - given function `f()` and progressr `p()`, it will return a new function that calls `f()` and then (on-exiting) will call `p()` after every iteration.
#'
#' This is inspired by purrr's `safely`, `quietly`, and `possibly` function decorators.
#'
#' @param f a function to add progressr functionality to.
#' @param p a progressr function as created by `progressr::progressor()`
#'
#' @return a function that does the same as `f` but it calls `p()` after iteration.
#' @keywords Internal
#'
progressively <- function(f, p = NULL){
  if(!is.null(p) && !inherits(p, "progressor")) stop("`p` must be a progressor function!")
  if(is.null(p)) p <- function(...) NULL
  force(f)

  function(...){
    on.exit(p("loading..."))
    f(...)
  }

}


#' @title
#' **Load .csv / .csv.gz file from a remote connection**
#' @description
#' This is a thin wrapper on data.table::fread
#' @param ... passed to data.table::fread
#' @keywords Internal
#' @importFrom data.table fread
#' @return a dataframe as created by [`data.table::fread()`]
csv_from_url <- function(...){
  data.table::fread(...)
}

#' @title
#' **Load .rds file from a remote connection**
#' @param url a character url
#' @return a dataframe as created by [`readRDS()`]
#' @keywords Internal
#' @importFrom data.table data.table setDT
#' @import rvest
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    warning(paste0("Failed to readRDS from <", url, ">"), call. = FALSE)
    return(data.table::data.table())
  }

  data.table::setDT(load)
  return(load)
}


# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#' @title
#' **Most Recent NHL Season**
#' @return Value for most recent NHL season
#' @export
most_recent_nhl_season <- function() {
  dplyr::if_else(
    as.double(substr(Sys.Date(), 6, 7)) >= 10,
    as.double(substr(Sys.Date(), 1, 4))+1,
    as.double(substr(Sys.Date(), 1, 4))
  )
}
#' @title
#' **Most Recent NHL Season for NHL API**
#' @return Value for most recent NHL season in the format of the NHL API
#' @export
most_recent_nhl_season_api_param <- function() {
  season <- dplyr::if_else(
    as.double(substr(Sys.Date(), 6, 7)) >= 10,
    as.double(substr(Sys.Date(), 1, 4))+1,
    as.double(substr(Sys.Date(), 1, 4))
  )
  return(glue::glue("{season-1}{season}"))
}

#' @title
#' **Most Recent PHF Season**
#' @return Value for most recent NHL season
#' @export
most_recent_phf_season <- function() {
  2023
}

year_to_season <- function(year){
  first_year <- substr(year,3,4)
  next_year <- as.numeric(first_year)+1
  next_year <- dplyr::case_when(
    next_year <10 & first_year > 0 ~ glue::glue("0{next_year}"),
    first_year == 99 ~ "00",
    TRUE ~ as.character(next_year))
  return(glue::glue("{year}-{next_year}"))
}


# The function `message_completed` to create the green "...completed" message
# only exists to hide the option `in_builder` in dots
message_completed <- function(x, in_builder = FALSE) {
  if (isFALSE(in_builder)) {
    str <- paste0(my_time(), " | ", x)
    cli::cli_alert_success("{{.field {str}}}")
  } else if (in_builder) {
    cli::cli_alert_success("{my_time()} | {x}")
  }
}

user_message <- function(x, type) {
  if (type == "done") {
    cli::cli_alert_success("{my_time()} | {x}")
  } else if (type == "todo") {
    cli::cli_ul("{my_time()} | {x}")
  } else if (type == "info") {
    cli::cli_alert_info("{my_time()} | {x}")
  } else if (type == "oops") {
    cli::cli_alert_danger("{my_time()} | {x}")
  }
}

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      right = paste0("fastRhockey version ", utils::packageVersion("fastRhockey")),
      width = getOption("width")
    )
  )
}

rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      width = getOption("width")
    )
  )
}

#' @import rvest
check_status <- function(res) {
  x = httr::status_code(res)
  if(x != 200) stop("The API returned an error", call. = FALSE)
}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import utils
utils::globalVariables(c("where"))


#' @importFrom Rcpp getRcppVersion
#' @importFrom RcppParallel defaultNumThreads
NULL

`%c%` <- function(x,y){
  ifelse(!is.na(x),x,y)
}


# Functions for custom class
# turn a data.frame into a tibble/fastRhockey_data
make_fastRhockey_data <- function(df,type,timestamp){
  out <- df %>%
    tidyr::as_tibble()

  class(out) <- c("fastRhockey_data","tbl_df","tbl","data.table","data.frame")
  attr(out,"fastRhockey_timestamp") <- timestamp
  attr(out,"fastRhockey_type") <- type
  return(out)
}

#' @export
#' @noRd
print.fastRhockey_data <- function(x,...) {
  cli::cli_rule(left = "{attr(x,'fastRhockey_type')}",right = "{.emph fastRhockey {utils::packageVersion('fastRhockey')}}")

  if(!is.null(attr(x,'fastRhockey_timestamp'))) {
    cli::cli_alert_info(
      "Data updated: {.field {format(attr(x,'fastRhockey_timestamp'), tz = Sys.timezone(), usetz = TRUE)}}"
    )
  }

  NextMethod(print,x)
  invisible(x)
}


# rbindlist but maintain attributes of last file
rbindlist_with_attrs <- function(dflist){

  fastRhockey_timestamp <- attr(dflist[[length(dflist)]], "fastRhockey_timestamp")
  fastRhockey_type <- attr(dflist[[length(dflist)]], "fastRhockey_type")
  out <- data.table::rbindlist(dflist, use.names = TRUE, fill = TRUE)
  attr(out,"fastRhockey_timestamp") <- fastRhockey_timestamp
  attr(out,"fastRhockey_type") <- fastRhockey_type
  out
}
