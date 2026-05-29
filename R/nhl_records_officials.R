#' @title **NHL Records - On-Ice Officials**
#' @description Returns NHL on-ice official listings from the NHL Records API
#'   (`https://records.nhl.com/site/api/officials`). Optionally filter by
#'   type (e.g. `"referee"`, `"linesman"`), which switches the resource to
#'   `officials/{type}`.
#' @param type Optional character official type. If supplied, the resource
#'   becomes `officials/{type}`.
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                |types     |description                                  |
#'    |:-----------------------|:---------|:--------------------------------------------|
#'    |id                      |integer   |Unique official record identifier.           |
#'    |active                  |logical   |Whether the official is currently active.    |
#'    |association_url         |character |URL to the official's association profile.   |
#'    |birth_city              |character |Birth city of the official.                  |
#'    |birth_date              |character |Birth date of the official.                  |
#'    |coach_id                |integer   |Associated coach identifier, if any.         |
#'    |country_code            |character |Country code of the official.                |
#'    |deceased                |logical   |Whether the official is deceased.            |
#'    |deceased_date           |character |Date of death, if applicable.                |
#'    |first_name              |character |First name of the official.                  |
#'    |first_playoff_game_id   |integer   |Game ID of the official's first playoff game.|
#'    |first_regular_game_id   |integer   |Game ID of the official's first regular game.|
#'    |general_manager_id      |logical   |Associated general manager identifier.       |
#'    |headshot_url            |character |URL to the official's headshot image.        |
#'    |last_name               |character |Last name of the official.                   |
#'    |nationality_code        |character |Nationality code of the official.            |
#'    |official_type           |character |Type of official (e.g. referee, linesman).   |
#'    |officials_schema_id     |integer   |Officials schema identifier.                 |
#'    |player_id               |integer   |Associated player identifier, if any.        |
#'    |referree_association_id |integer   |Referee association identifier.              |
#'    |state_province_code     |character |State or province code of the official.      |
#'    |sweater_number          |integer   |Sweater number worn by the official.         |
#'    |thumb_url               |character |URL to the official's thumbnail image.       |
#' @keywords NHL Records Officials
#' @importFrom janitor clean_names
#' @importFrom dplyr as_tibble
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(nhl_records_officials())
#' }
nhl_records_officials <- function(type = NULL) {
    resource <- if (!is.null(type)) {
        glue::glue("officials/{type}")
    } else {
        "officials"
    }
    raw <- .nhl_records_api(resource = resource)
    if (is.null(raw)) return(NULL)
    if (!is.data.frame(raw) || nrow(raw) == 0) {
        message(sprintf("%s: No officials data", Sys.time()))
        return(NULL)
    }
    df <- janitor::clean_names(dplyr::as_tibble(raw))
    make_fastRhockey_data(df, "NHL Records Officials", Sys.time())
}
