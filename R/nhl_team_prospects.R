#' @title **NHL Team Prospects**
#' @description Returns prospect information for a given team.
#' @param team_abbr Three-letter team abbreviation (e.g., "TOR", "BOS")
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                     |types     |description                                  |
#'    |:----------------------------|:---------|:--------------------------------------------|
#'    |id                           |integer   |Unique player identifier.                    |
#'    |headshot                     |character |URL to the prospect's headshot image.        |
#'    |position_code                |character |Player position code.                        |
#'    |shoots_catches               |character |Handedness (shoots/catches).                 |
#'    |height_in_inches             |integer   |Height in inches.                            |
#'    |weight_in_pounds             |integer   |Weight in pounds.                            |
#'    |height_in_centimeters        |integer   |Height in centimeters.                       |
#'    |weight_in_kilograms          |integer   |Weight in kilograms.                         |
#'    |birth_date                   |character |Player birth date.                           |
#'    |birth_country                |character |Player birth country.                        |
#'    |sweater_number               |integer   |Jersey number.                               |
#'    |first_name_default           |character |First name (default localization).           |
#'    |first_name_cs                |character |First name (Czech localization).             |
#'    |first_name_sk                |character |First name (Slovak localization).            |
#'    |last_name_default            |character |Last name (default localization).            |
#'    |last_name_cs                 |character |Last name (Czech localization).              |
#'    |last_name_sk                 |character |Last name (Slovak localization).             |
#'    |birth_city_default           |character |Birth city (default localization).           |
#'    |birth_city_cs                |character |Birth city (Czech localization).             |
#'    |birth_city_de                |character |Birth city (German localization).            |
#'    |birth_city_fi                |character |Birth city (Finnish localization).           |
#'    |birth_city_fr                |character |Birth city (French localization).            |
#'    |birth_city_sk                |character |Birth city (Slovak localization).            |
#'    |birth_city_sv                |character |Birth city (Swedish localization).           |
#'    |birth_state_province_default |character |Birth state/province (default localization). |
#'    |birth_state_province_fr      |character |Birth state/province (French localization).  |
#'    |birth_state_province_sk      |character |Birth state/province (Slovak localization).  |
#'    |birth_state_province_sv      |character |Birth state/province (Swedish localization). |
#'    |prospect_group               |character |Prospect position group the player belongs to.|
#'    |team_abbr                    |character |Team abbreviation.                           |
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
