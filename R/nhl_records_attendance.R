#' @title **NHL Records - Season Attendance**
#' @description Returns historical NHL season attendance from the NHL Records
#'   API (`https://records.nhl.com/site/api/attendance`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A `fastRhockey_data` tibble of attendance records, or `NULL`
#'   on failure.
#' @keywords NHL Records Attendance
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_attendance())
#' }
nhl_records_attendance <- function(cayenne_exp = NULL) {
    raw <- .nhl_records_api(
        resource = "attendance",
        cayenne_exp = cayenne_exp
    )
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No attendance data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Attendance", Sys.time())
}
