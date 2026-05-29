#' @title **NHL Records - Season Attendance**
#' @description Returns historical NHL season attendance from the NHL Records
#'   API (`https://records.nhl.com/site/api/attendance`).
#' @param cayenne_exp Optional Cayenne filter expression string.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types   |description                            |
#'    |:------------------|:-------|:--------------------------------------|
#'    |id                 |integer |Unique attendance record identifier.   |
#'    |playoff_attendance |integer |Total playoff attendance.              |
#'    |regular_attendance |integer |Total regular-season attendance.       |
#'    |season_id          |integer |8-digit season identifier.             |
#'    |total_attendance   |integer |Total attendance for the season.       |
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
