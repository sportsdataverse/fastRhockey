#' @title  **PWHL Stats**
#' @description PWHL Stats lookup
#'
#' @param position either goalie or skater.
#' @param season Season (YYYY) to pull the stats from, the concluding year in XXXX-YY format
#' @param team Team abbreviation to filter skaters by (e.g., "BOS"). Ignored for goalies.
#' @param regular Bool for whether to pull regular or pre-season stats
#' @return A data frame (`fastRhockey_data`) with player stats. Columns vary by
#'   position; the goalie variant returns the following columns:
#'
#'    |col_name           |types     |description                            |
#'    |:------------------|:---------|:--------------------------------------|
#'    |player_id          |character |Unique player identifier.              |
#'    |player_name        |character |Player name.                           |
#'    |team               |character |Team name.                             |
#'    |games_played       |character |Games played.                          |
#'    |minutes            |character |Total minutes played.                  |
#'    |minute             |character |Minutes component of time on ice.      |
#'    |second             |character |Seconds component of time on ice.      |
#'    |shots_faced        |character |Shots faced.                           |
#'    |goals_against      |character |Goals against.                         |
#'    |goals_against_avg  |character |Goals-against average.                 |
#'    |save_percentage    |character |Save percentage.                       |
#'    |shutouts           |character |Shutouts.                              |
#'    |wins               |character |Wins.                                  |
#'    |losses             |character |Losses.                                |
#'    |so_att             |character |Shootout attempts faced.               |
#'    |so_goals_against   |character |Shootout goals against.                |
#'    |so_save_percentage |character |Shootout save percentage.              |
#' @import jsonlite
#' @import dplyr
#' @importFrom glue glue
#' @importFrom cli cli_abort
#' @importFrom rlang %||%
#' @importFrom tidyr separate
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_stats(position = "goalie", season = 2024))
#' }

pwhl_stats <- function(position = "goalie", team = "all", season = 2024, regular = TRUE) {

  season_id <- .pwhl_resolve_season_id(season, if (regular) "regular" else "preseason")

  # Resolve a team code/label/name to the HockeyTech team_id for the URL;
  # "all" (or NULL) keeps the league-wide view.
  team_param <- "all"
  if (!is.null(team) && !identical(tolower(team), "all")) {
    teams_df <- pwhl_teams()
    team_row <- teams_df %>%
      dplyr::filter(
        tolower(.data$team_code) == tolower(team) |
          tolower(.data$team_label) == tolower(team) |
          tolower(.data$team_name) == tolower(team)
      )
    if (nrow(team_row) == 0) {
      cli::cli_abort(
        "No PWHL team matches {.val {team}}. Use a team code ({.val OTT}), label ({.val Ottawa}), or {.val all}."
      )
    }
    team_param <- team_row$team_id[1]
  }

  players <- data.frame()

  tryCatch(
    expr = {
      if (position == "goalie") {

        URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season={season_id}&team={team_param}&position=goalies&rookies=0&statsType=expanded&rosterstatus=undefined&site_id=2&first=0&limit=100&sort=gaa&league_id=1&lang=en&division=-1&qualified=all&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._5")

        r <- .hockeytech_api(URL)

        data <- r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {

          player_df <- data.frame(
            player_id = c(data[[y]]$row$player_id),
            player_name = c(data[[y]]$row$name),
            team = c(data[[y]]$row$team_code),
            games_played = c(data[[y]]$row$games_played),
            minutes = c(data[[y]]$row$minutes_played),
            shots_faced = c(data[[y]]$row$shots),
            goals_against = c(data[[y]]$row$goals_against),
            goals_against_avg = c(data[[y]]$row$goals_against_average),
            save_percentage = c(data[[y]]$row$save_percentage),
            shutouts = c(data[[y]]$row$shutouts),
            wins = c(data[[y]]$row$wins),
            losses = c(data[[y]]$row$losses),
            so_att = c(data[[y]]$row$shootout_attempts),
            so_goals_against = c(data[[y]]$row$shootout_goals_against),
            so_save_percentage = c(data[[y]]$row$shootout_percentage)
          )

          players <- dplyr::bind_rows(players, player_df)

        }

        players <- players %>%
          tidyr::separate(col = "minutes", into = c("minute", "second"), sep = ":", remove = FALSE)
      } else {

        URL <- glue::glue("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season={season_id}&team={team_param}&position=skaters&rookies=0&statsType=standard&rosterstatus=undefined&site_id=2&first=0&limit=100&sort=points&league_id=1&lang=en&division=-1&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._6")

        r <- .hockeytech_api(URL)

        data <- r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {

          player_df <- data.frame(
            player_id = c(data[[y]]$row$player_id),
            player_name = c(data[[y]]$row$name),
            current_team = c(data[[y]]$row$active),
            position = c(data[[y]]$row$position),
            team = c(data[[y]]$row$team_code),
            games_played = c(data[[y]]$row$games_played),
            goals = c(data[[y]]$row$goals),
            shots = c(data[[y]]$row$shots),
            shooting_pct = c(data[[y]]$row$shooting_percentage),
            assists = c(data[[y]]$row$assists),
            points = c(data[[y]]$row$points),
            points_per_game = c(data[[y]]$row$points_per_game),
            plus_minus = c(data[[y]]$row$plus_minus),
            penalty_minutes = c(data[[y]]$row$penalty_minutes),
            penalty_minutes_per_game = c(data[[y]]$row$penalty_minutes_per_game),
            power_play_goals = c(data[[y]]$row$power_play_goals),
            power_play_assists = c(data[[y]]$row$power_play_assists),
            short_handed_goals = c(data[[y]]$row$short_handed_goals),
            short_handed_assists = c(data[[y]]$row$short_handed_assists),
            # the shootout block dropped out of the feed after the
            # inaugural season -- keep the columns, null-safe
            shootout_goals = c(data[[y]]$row$shootout_goals %||% NA),
            shootout_attempts = c(data[[y]]$row$shootout_attempts %||% NA),
            shootout_pct = c(data[[y]]$row$shootout_percentage %||% NA),
            shootout_winning_goals = c(data[[y]]$row$shootout_winning_goals %||% NA),
            faceoff_attempts = c(data[[y]]$row$faceoff_attempts),
            faceoff_wins = c(data[[y]]$row$faceoff_wins),
            faceoff_pct = c(data[[y]]$row$faceoff_pct)
          )

          players <- dplyr::bind_rows(players, player_df)

        }
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid season or no stats data available! Try a season from 2024 onwards!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  if (nrow(players) == 0) {
    return(NULL)
  }

  return(players)
}
