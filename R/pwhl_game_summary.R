#' @title  **PWHL Game Summary**
#' @description Retrieves a detailed game summary from the PWHL game center feed,
#'   including scoring summary, penalty summary, shots by period, and three stars.
#'
#' @param game_id Game ID to retrieve the summary for.
#' @return A named list of data frames: `details`, `scoring`, `penalties`, `shots_by_period`, `three_stars`.
#'
#'    **details**
#'
#'    |col_name      |types     |description                          |
#'    |:-------------|:---------|:------------------------------------|
#'    |game_id       |numeric   |Unique game identifier.              |
#'    |date          |character |Game date.                           |
#'    |status        |character |Game status.                         |
#'    |venue         |character |Venue where the game was played.     |
#'    |attendance    |character |Reported attendance.                 |
#'    |home_team     |character |Home team name.                      |
#'    |home_team_id  |numeric   |Home team identifier.                |
#'    |home_score    |numeric   |Home team final score.               |
#'    |away_team     |character |Away team name.                      |
#'    |away_team_id  |numeric   |Away team identifier.                |
#'    |away_score    |numeric   |Away team final score.               |
#'
#'    **scoring**
#'
#'    |col_name   |types     |description                          |
#'    |:----------|:---------|:------------------------------------|
#'    |period     |character |Period in which the goal was scored. |
#'    |time       |character |Time of the goal.                    |
#'    |team       |character |Team that scored.                    |
#'    |team_id    |numeric   |Identifier of the scoring team.      |
#'    |scorer     |character |Name of the goal scorer.             |
#'    |scorer_id  |numeric   |Identifier of the goal scorer.       |
#'    |assist_1   |character |Name of the primary assister.        |
#'    |assist_2   |character |Name of the secondary assister.      |
#'    |goal_type  |character |Type of goal.                        |
#'
#'    **penalties**
#'
#'    |col_name    |types     |description                          |
#'    |:-----------|:---------|:------------------------------------|
#'    |period      |character |Period in which the penalty occurred.|
#'    |time        |character |Time of the penalty.                 |
#'    |team        |character |Penalized team.                      |
#'    |team_id     |numeric   |Identifier of the penalized team.    |
#'    |player      |character |Penalized player name.               |
#'    |player_id   |numeric   |Identifier of the penalized player.  |
#'    |infraction  |character |Penalty infraction description.      |
#'    |minutes     |numeric   |Penalty minutes.                     |
#'
#'    **shots_by_period**
#'
#'    |col_name     |types     |description                          |
#'    |:------------|:---------|:------------------------------------|
#'    |period       |character |Period number.                       |
#'    |home_shots   |numeric   |Home team shots in the period.       |
#'    |away_shots   |numeric   |Away team shots in the period.       |
#'
#'    **three_stars**
#'
#'    |col_name    |types     |description                          |
#'    |:-----------|:---------|:------------------------------------|
#'    |star        |numeric   |Star ranking (1 to 3).               |
#'    |player_id   |numeric   |Identifier of the selected player.   |
#'    |first_name  |character |Player first name.                   |
#'    |last_name   |character |Player last name.                    |
#'    |team_id     |numeric   |Identifier of the player's team.     |
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_game_summary(game_id = 27))
#' }

pwhl_game_summary <- function(game_id) {

  tryCatch(
    expr = {
      url <- .pwhl_gc_url(list(
        tab = "gamesummary",
        game_id = game_id
      ))

      r <- .pwhl_api(url)

      gc <- r$GC %||% r

      # --- Details ---
      details <- data.frame(
        game_id = as.numeric(gc$details$id %||% game_id),
        date = as.character(gc$details$date %||% NA),
        status = as.character(gc$details$status %||% NA),
        venue = as.character(gc$details$venue %||% NA),
        attendance = as.character(gc$details$attendance %||% NA),
        home_team = as.character(gc$homeTeam$info$name %||% NA),
        home_team_id = as.numeric(gc$homeTeam$info$id %||% NA),
        home_score = as.numeric(gc$homeTeam$stats$goals %||% 0),
        away_team = as.character(gc$visitingTeam$info$name %||% NA),
        away_team_id = as.numeric(gc$visitingTeam$info$id %||% NA),
        away_score = as.numeric(gc$visitingTeam$stats$goals %||% 0),
        stringsAsFactors = FALSE
      )

      # --- Scoring Summary ---
      scoring <- data.frame()
      if (!is.null(gc$goals)) {
        for (i in seq_along(gc$goals)) {
          g <- gc$goals[[i]]
          goal_row <- data.frame(
            period = as.character(g$period %||% NA),
            time = as.character(g$time %||% NA),
            team = as.character(g$team %||% NA),
            team_id = as.numeric(g$team_id %||% NA),
            scorer = as.character(g$scorerName %||% g$scorer_name %||% NA),
            scorer_id = as.numeric(g$goal_scorer %||% g$scorer_id %||% NA),
            assist_1 = as.character(g$assist1Name %||% g$assist1_name %||% NA),
            assist_2 = as.character(g$assist2Name %||% g$assist2_name %||% NA),
            goal_type = as.character(g$goal_type %||% NA),
            stringsAsFactors = FALSE
          )
          scoring <- dplyr::bind_rows(scoring, goal_row)
        }
      }

      # --- Penalty Summary ---
      penalties <- data.frame()
      if (!is.null(gc$penalties)) {
        for (i in seq_along(gc$penalties)) {
          p <- gc$penalties[[i]]
          pen_row <- data.frame(
            period = as.character(p$period %||% NA),
            time = as.character(p$time %||% NA),
            team = as.character(p$team %||% NA),
            team_id = as.numeric(p$team_id %||% NA),
            player = as.character(p$player_name %||% p$player %||% NA),
            player_id = as.numeric(p$player_id %||% NA),
            infraction = as.character(p$infraction %||% p$description %||% NA),
            minutes = as.numeric(p$minutes %||% NA),
            stringsAsFactors = FALSE
          )
          penalties <- dplyr::bind_rows(penalties, pen_row)
        }
      }

      # --- Shots by Period ---
      shots_by_period <- data.frame()
      if (!is.null(gc$shotsByPeriod) || !is.null(gc$shots_by_period)) {
        sbp <- gc$shotsByPeriod %||% gc$shots_by_period
        for (i in seq_along(sbp)) {
          s <- sbp[[i]]
          shot_row <- data.frame(
            period = as.character(s$period %||% i),
            home_shots = as.numeric(s$home %||% 0),
            away_shots = as.numeric(s$away %||% 0),
            stringsAsFactors = FALSE
          )
          shots_by_period <- dplyr::bind_rows(shots_by_period, shot_row)
        }
      }

      # --- Three Stars ---
      three_stars <- data.frame()
      if (!is.null(gc$threeStars) || !is.null(gc$three_stars)) {
        stars <- gc$threeStars %||% gc$three_stars
        for (i in seq_along(stars)) {
          star <- stars[[i]]
          star_row <- data.frame(
            star = as.numeric(i),
            player_id = as.numeric(star$id %||% star$player_id %||% NA),
            first_name = as.character(star$firstName %||% star$first_name %||% NA),
            last_name = as.character(star$lastName %||% star$last_name %||% NA),
            team_id = as.numeric(star$team_id %||% NA),
            stringsAsFactors = FALSE
          )
          three_stars <- dplyr::bind_rows(three_stars, star_row)
        }
      }

      result <- list(
        details = details,
        scoring = scoring,
        penalties = penalties,
        shots_by_period = shots_by_period,
        three_stars = three_stars
      )

      return(result)
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Error retrieving PWHL game summary for game_id={game_id}. {e$message}"))
      return(NULL)
    }
  )
}
