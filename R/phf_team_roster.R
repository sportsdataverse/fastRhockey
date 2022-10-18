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
#'    try(phf_team_roster(team = "Boston Pride", season = 2022))
#' }

phf_team_roster <- function(team, season = most_recent_phf_season()){

  league_info <- phf_league_info(season=season)
  season_id <- get_phf_season_id(season=season)
  team_row <- league_info$teams %>%
    dplyr::filter(.data$name == team)
  team_id <- team_row %>%
    dplyr::select(.data$id)
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
      resp <- (res %>%
                 httr::content(as = "text", encoding="utf-8") %>%
                 jsonlite::parse_json() %>%
                 purrr::pluck("content") %>%
                 rvest::read_html() %>%
                 rvest::html_elements("table"))

      resp <- unique(resp)
      roster_href <- resp[[1]] %>%
        rvest::html_elements("tr") %>%
        rvest::html_elements("td > a") %>%
        rvest::html_attr("href")

      roster <- resp[[1]] %>%
        rvest::html_table()
      roster <- roster[1:(ncol(roster)-1)]
      team_staff <- resp[[3]] %>%
        rvest::html_table()
      team_staff <- team_staff[1:ncol(team_staff)-1]
      roster_href <- data.frame(
        player_href = roster_href
      )

      roster_df <- dplyr::bind_cols(roster, roster_href)

      roster_df <- dplyr::bind_cols(team_row, roster_df)

      roster_df <- roster_df %>%
        dplyr::rename(
          team_id = .data$id,
          team_name = .data$name,
          player_jersey = .data$`#`,
          player_name = .data$Name,
          position = .data$POS) %>%
        dplyr::mutate(
          player_name = stringr::str_replace(.data$player_name,pattern = "#\\d+",replacement=""),
          player_id = as.integer(stringr::str_extract(.data$player_href, "\\d+"))) %>%
        janitor::clean_names() %>%
        make_fastRhockey_data("PHF Roster Information from PremierHockeyFederation.com",Sys.time())

      team_staff_df <- dplyr::bind_cols(team_row, team_staff)
      team_staff_df <- team_staff_df %>%
        dplyr::rename(
          team_id = .data$id,
          team_name = .data$name,
          staff_name = .data$Name,
          staff_type = .data$Type) %>%
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
