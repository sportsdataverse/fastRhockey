#' @title  **PWHL Teams**
#' @description PWHL Teams lookup
#'
#' @return A data frame with team data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_teams())
#' }


pwhl_teams <- function() {

  full_url = "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=teamsForSeason&season=2&key=694cfeed58c932ee&client_code=pwhl&site_id=2&callback=angular.callbacks._4"

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._4\\(", "", res)
  res <- gsub("}]})", "}]}", res)

  r <- res %>%
    jsonlite::parse_json()

  team_info <- r$teamsNoAll
  teams <- data.frame()

  tryCatch(
    expr = {
      for (i in 1:length(team_info)) {

        team_df <- data.frame(
          "team_id" = c(team_info[[i]]$id),
          "team_name" = c(team_info[[i]]$name),
          "team_code" = c(team_info[[i]]$team_code),
          "team_nickname" = c(team_info[[i]]$nickname),
          "division" = c(team_info[[i]]$division_id),
          "team_logo" = c(team_info[[i]]$logo)
        )

        t <- data.frame(
          team_name = c("PWHL Boston", "PWHL Minnesota", "PWHL Montreal", "PWHL New York", "PWHL Ottawa", "PWHL Toronto"),
          team_label = c("Boston", "Minnesota", "Montreal", "New York", "Ottawa", "Toronto")
        )

        teams <- rbind(
          teams,
          team_df %>%
            dplyr::left_join(t, by = c("team_name"))
        )

        teams <- teams %>%
          dplyr::select(
            c(
              "team_name",
              "team_id",
              "team_code",
              "team_nickname",
              "team_label",
              "division",
              "team_logo"
            )
          )

      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"))

    },
    warning = function(w) {
    },
    finally = {
    }
  )

  return(teams)

}
