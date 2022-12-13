#' @title  **PHF Team Roster**
#' @description PHF Team Roster lookup
#' @param team Team name with nickname (e.g. Boston Pride, Buffalo Beauts)
#' @param season Season (YYYY) to pull the team stats from, the concluding year in XXXX-YY format
#' @return A named list of data frames: roster, team_staff
#' @import rvest
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'    try(phf_team_roster(team = "Buffalo Beauts", season = 2023))
#' }

phf_team_roster <- function(team, season = most_recent_phf_season()){

  league_info <- phf_league_info(season=season)
  season_id <- phf_get_season_id(season=season)
  team_row <- league_info$teams %>%
    dplyr::filter(.data$name == team)
  team_id <- team_row %>%
    dplyr::select("id")
  base_url <- "https://web.api.digitalshift.ca/partials/stats/team/roster?team_id="
  full_url <- paste0(base_url,
                     team_id$id)

  # the link for the game + authorization for accessing the API
  res <- httr::RETRY(
    "GET", full_url,
    httr::add_headers(
      `Authorization`='ticket="4dM1QOOKk-PQTSZxW_zfXnOgbh80dOGK6eUb_MaSl7nUN0_k4LxLMvZyeaYGXQuLyWBOQhY8Q65k6_uwMu6oojuO"'))

  # Check the result
  check_status(res)

  tryCatch(
    expr={
      resp_all <- res %>%
                 httr::content(as = "text", encoding="utf-8") %>%
                 jsonlite::parse_json() %>%
                 purrr::pluck("content") %>%
                 rvest::read_html()
      player_image <- resp_all %>%
        rvest::html_elements(".col > a") %>%
        rvest::html_elements(".photo") %>%
        rvest::html_elements("img") %>%
        rvest::html_attr("src")
      player_image_df <- data.frame(player_image_href = player_image)
      resp <- resp_all %>%
        rvest::html_elements("table")


      roster_href <- resp %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href") %>%
        unique()

      roster <- resp %>%
        rvest::html_table() %>%
        unique()

      roster <- roster[1:(length(roster)-1)]
      roster <- data.table::rbindlist(roster)


      roster <- roster %>%
        dplyr::select(-"V1")
      team_staff <- resp[[length(resp)]] %>%
        rvest::html_table()

      team_staff <- team_staff[1:ncol(team_staff)-1]
      roster_href_df <- data.frame(
        player_href = roster_href
      )

      roster_df <- dplyr::bind_cols(roster, player_image_df, roster_href_df)

      roster_df <- dplyr::bind_cols(team_row, roster_df)
      roster_cols <- c(
        "team_id" = "id",
        "team_name" = "name",
        "player_jersey" = "#",
        "player_name" = "Name",
        "position" = "POS"
      )
      roster_df <- roster_df %>%
        dplyr::rename(dplyr::any_of(roster_cols)) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
          player_id = as.integer(stringr::str_extract(.data$player_href, "\\d+")),
          position = ifelse(.data$position == FALSE, "F", .data$position)) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("PHF Roster Information from PremierHockeyFederation.com",Sys.time())

      team_staff_df <- dplyr::bind_cols(team_row, team_staff)
      staff_cols <- c(
        "team_id" = "id",
        "team_name" = "name",
        "staff_name" = "Name",
        "staff_type" = "Type"
      )
      team_staff_df <- team_staff_df %>%
        dplyr::rename(dplyr::any_of(staff_cols)) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("PHF Team Staff Information from PremierHockeyFederation.com",Sys.time())


      team_roster <- c(list(roster_df), list(team_staff_df))
      names(team_roster) <- c("roster", "team_staff")


    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no team roster data available!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(team_roster)
}
