#' @title **NHL Team Prospects**
#' @description Returns prospect information for a given team.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @return Returns a data frame with prospect player data.
#' @keywords NHL Team Prospects
#' @importFrom httr RETRY content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr bind_rows
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_team_prospects(team_abbr = "TOR"))
#' }
nhl_team_prospects <- function(team_abbr) {
    url <- glue::glue(
        "https://api-web.nhle.com/v1/prospects/{team_abbr}"
    )

    tryCatch(
        expr = {
            res <- httr::RETRY("GET", url)
            check_status(res)

            resp_text <- httr::content(res, as = "text", encoding = "UTF-8")
            raw <- jsonlite::fromJSON(resp_text, flatten = TRUE)

            # Prospects may be nested under position groups
            result_frames <- list()
            for (key in names(raw)) {
                val <- raw[[key]]
                if (is.data.frame(val) && nrow(val) > 0) {
                    val$prospect_group <- key
                    result_frames[[key]] <- val
                }
            }

            if (length(result_frames) == 0) {
                message(glue::glue(
                    "{Sys.time()}: No prospect data for {team_abbr}"
                ))
                return(NULL)
            }

            df <- dplyr::bind_rows(result_frames)
            df$team_abbr <- team_abbr
            df <- df %>%
                janitor::clean_names() %>%
                make_fastRhockey_data("NHL Team Prospects", Sys.time())

            return(df)
        },
        error = function(e) {
            message(glue::glue(
                "{Sys.time()}: Error fetching prospects for {team_abbr}: {e$message}"
            ))
            return(NULL)
        }
    )
}
