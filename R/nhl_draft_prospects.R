#' @title **NHL Draft Prospects**
#' @description Returns information on draft prospects
#' @return Returns a tibble with the following columns:
#'
#'   |col_name                      |types     |
#'   |:-----------------------------|:---------|
#'   |prospect_id                   |integer   |
#'   |full_name                     |character |
#'   |link                          |character |
#'   |first_name                    |character |
#'   |last_name                     |character |
#'   |birth_date                    |character |
#'   |birth_city                    |character |
#'   |birth_country                 |character |
#'   |height                        |character |
#'   |weight                        |integer   |
#'   |shoots_catches                |character |
#'   |nhl_player_id                 |integer   |
#'   |draft_status                  |character |
#'   |birth_state_province          |character |
#'   |primary_position_code         |character |
#'   |primary_position_name         |character |
#'   |primary_position_type         |character |
#'   |primary_position_abbreviation |character |
#'   |prospect_category_id          |integer   |
#'   |prospect_category_short_name  |character |
#'   |prospect_category_name        |character |
#'   |amateur_team_name             |character |
#'   |amateur_team_link             |character |
#'   |amateur_league_name           |character |
#'   |amateur_league_link           |character |
#'   |ranks_midterm                 |integer   |
#'   |ranks_final_rank              |integer   |
#'   |ranks_draft_year              |integer   |
#' @keywords NHL Draft Prospects
#' @import rvest
#' @importFrom rlang .data
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \donttest{
#'    try(nhl_draft_prospects())
#' }
nhl_draft_prospects <- function(){

  base_url <- "https://statsapi.web.nhl.com/api/v1/draft/prospects"

  full_url <- paste0(base_url)


  res <- httr::RETRY("GET", full_url)

  # Check the result
  check_status(res)

  tryCatch(
    expr = {
      resp <- res %>%
        httr::content(as = "text", encoding = "UTF-8")
      draft_prospects_df <- jsonlite::fromJSON(resp)[["prospects"]]
      draft_prospects_df <- jsonlite::fromJSON(jsonlite::toJSON(draft_prospects_df),flatten=TRUE)

      draft_prospects_df <- draft_prospects_df %>%
        janitor::clean_names() %>%
        dplyr::rename(prospect_id = .data$id) %>%
        as.data.frame() %>%
        make_fastRhockey_data("NHL Draft Prospects data from NHL.com",Sys.time())

    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no draft prospects data available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  return(draft_prospects_df)
}
