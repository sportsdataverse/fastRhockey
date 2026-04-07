#' @title **NHL Scores**
#' @description Returns scores for all games on a given date.
#' @param date Character date in "YYYY-MM-DD" format. If NULL, returns current scores.
#' @return Returns a data frame with game scores.
#' @keywords NHL Scores
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_scores())
#' }
nhl_scores <- function(date = NULL) {
    if (is.null(date)) {
        url <- "https://api-web.nhle.com/v1/score/now"
    } else {
        url <- glue::glue("https://api-web.nhle.com/v1/score/{date}")
    }

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            games <- raw$games
            if (is.null(games) || length(games) == 0) {
                message(glue::glue("{Sys.time()}: No scores data for {date}"))
                return(NULL)
            }

            if (is.data.frame(games)) {
                df <- games
            } else {
                df <- jsonlite::fromJSON(
                    jsonlite::toJSON(games, auto_unbox = TRUE),
                    flatten = TRUE
                )
            }

            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Scores", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching scores: {e$message}"
            ))
            return(NULL)
        }
    )
}
