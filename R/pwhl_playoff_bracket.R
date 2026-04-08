#' @title  **PWHL Playoff Bracket**
#' @description Retrieves the playoff bracket for a PWHL season.
#'
#' @param season Season (YYYY) to pull the playoff bracket from.
#'   Defaults to `most_recent_pwhl_season()`.
#' @return A data frame with playoff bracket / series data, or NULL if unavailable.
#'
#'   * `series_id` - Series identifier.
#'   * `round` - Playoff round number.
#'   * `series_name` - Series name or label.
#'   * `team_1_id` - First team ID.
#'   * `team_1_name` - First team name.
#'   * `team_1_wins` - First team series wins.
#'   * `team_2_id` - Second team ID.
#'   * `team_2_name` - Second team name.
#'   * `team_2_wins` - Second team series wins.
#'   * `series_status` - Series completion status.
#'   * `winner_id` - Winning team ID (if series is complete).
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_playoff_bracket(season = 2024))
#' }

pwhl_playoff_bracket <- function(season = most_recent_pwhl_season()) {

  tryCatch(
    expr = {
      season_id <- .pwhl_resolve_season_id(season = season, game_type = "playoffs")

      url <- .pwhl_modulekit_url(list(
        view = "brackets",
        season_id = season_id
      ))

      r <- .pwhl_api(url)

      brackets <- r$SiteKit$Brackets

      bracket_df <- data.frame()

      if (!is.null(brackets)) {
        # Build team name lookup from brackets$teams
        teams_map <- brackets$teams
        .get_team_name <- function(tid) {
          if (is.null(tid) || is.na(tid)) return(NA_character_)
          tid_char <- as.character(tid)
          team <- teams_map[[tid_char]]
          if (!is.null(team) && !is.null(team$name)) {
            return(as.character(team$name))
          }
          return(NA_character_)
        }

        rounds <- brackets$rounds
        if (!is.null(rounds)) {
          for (i in seq_along(rounds)) {
            round <- rounds[[i]]
            round_num <- round$round %||% i
            round_name <- round$round_name %||% NA

            if (!is.null(round$matchups)) {
              for (j in seq_along(round$matchups)) {
                m <- round$matchups[[j]]

                t1_id <- m$team1$id %||% NA
                t2_id <- m$team2$id %||% NA

                row <- data.frame(
                  series_letter = as.character(m$series_letter %||% NA),
                  round = as.numeric(round_num),
                  round_name = as.character(round_name),
                  series_name = as.character(m$series_name %||% NA),
                  team_1_id = as.numeric(t1_id),
                  team_1_name = .get_team_name(t1_id),
                  team_1_wins = as.numeric(m$team1$wins %||% 0),
                  team_2_id = as.numeric(t2_id),
                  team_2_name = .get_team_name(t2_id),
                  team_2_wins = as.numeric(m$team2$wins %||% 0),
                  winner_id = as.numeric(m$winner %||% NA),
                  stringsAsFactors = FALSE
                )
                bracket_df <- dplyr::bind_rows(bracket_df, row)
              }
            }
          }
        }
      }

      if (nrow(bracket_df) == 0) {
        message(glue::glue("{Sys.time()}: No playoff bracket data found for season={season}."))
        return(NULL)
      }

      bracket_df <- make_fastRhockey_data(
        bracket_df,
        type = "PWHL Playoff Bracket",
        timestamp = Sys.time()
      )

      return(bracket_df)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving PWHL playoff bracket. {e$message}"))
      return(NULL)
    }
  )
}
