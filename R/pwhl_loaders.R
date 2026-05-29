## ─── PWHL release-tag → loader catalog ────────────────────────────────────
##
## Each row defines one season-level dataset published to a GitHub release on
## sportsdataverse/sportsdataverse-data. The exported `load_pwhl_*` helpers
## below are thin wrappers around `.pwhl_release_loader()` which handles
## season validation, URL construction, parallel download with progress, DB
## insertion, and S3 class tagging. Adding a new dataset = adding a row.
##
## | exported helper             | release tag             | file prefix       |
## |-----------------------------|-------------------------|-------------------|
## | `load_pwhl_pbp()`           | `pwhl_pbp`              | `play_by_play`    |
## | `load_pwhl_player_box()`    | `pwhl_player_boxscores` | `player_box`      |
## | `load_pwhl_skater_box()`    | `pwhl_skater_boxscores` | `skater_box`      |
## | `load_pwhl_goalie_box()`    | `pwhl_goalie_boxscores` | `goalie_box`      |
## | `load_pwhl_team_box()`      | `pwhl_team_boxscores`   | `team_box`        |
## | `load_pwhl_schedule()`      | `pwhl_schedules`        | `pwhl_schedule`   |
## | `load_pwhl_rosters()`       | `pwhl_rosters`          | `rosters`         |
## | `load_pwhl_game_rosters()`  | `pwhl_game_rosters`     | `game_rosters`    |
## | `load_pwhl_game_info()`     | `pwhl_game_info`        | `game_info`       |
## | `load_pwhl_scoring_summary()`| `pwhl_scoring_summary` | `scoring_summary` |
## | `load_pwhl_penalty_summary()`| `pwhl_penalty_summary` | `penalty_summary` |
## | `load_pwhl_three_stars()`   | `pwhl_three_stars`      | `three_stars`     |
## | `load_pwhl_officials()`     | `pwhl_officials`        | `officials`       |
## | `load_pwhl_shots_by_period()`| `pwhl_shots_by_period` | `shots_by_period` |
## | `load_pwhl_shootout()`      | `pwhl_shootout`         | `shootout_summary`|

# Internal worker shared by every load_pwhl_*() helper.
# Validates seasons, builds release URLs, downloads in parallel with optional
# progressr support, optionally writes into a DB, and tags the result with
# the `fastRhockey_data` S3 class.
.pwhl_release_loader <- function(seasons, release_tag, file_prefix,
                                 dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  loader <- rds_from_url
  in_db <- !is.null(dbConnection) && !is.null(tablename)

  if (isTRUE(seasons)) seasons <- 2024:most_recent_pwhl_season()

  stopifnot(is.numeric(seasons),
            seasons >= 2024,
            seasons <= most_recent_pwhl_season())

  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/", release_tag, "/", file_prefix, "_", seasons, ".rds"
  )

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = seasons)

  out <- lapply(urls, progressively(loader, p))
  out <- rbindlist_with_attrs(out)
  if (in_db) {
    DBI::dbWriteTable(dbConnection, tablename, out, append = TRUE)
    out <- NULL
  } else {
    class(out) <- c("fastRhockey_data", "tbl_df", "tbl", "data.table", "data.frame")
  }
  out
}


#' **Load fastRhockey PWHL play-by-play**
#' @name load_pwhl_pbp
NULL
#' @title
#' **Load cleaned PWHL play-by-play from the data repo**
#' @rdname load_pwhl_pbp
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   play-by-play data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments passed to an underlying function that writes
#'   the season data into a database (used by `update_pwhl_db()`).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the play-by-play data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                    |types     |description                                  |
#'    |:---------------------------|:---------|:--------------------------------------------|
#'    |game_id                     |integer   |Unique game identifier.                      |
#'    |event                       |character |Play-by-play event description.              |
#'    |team_id                     |integer   |Team identifier for the event.               |
#'    |period_of_game              |character |Period in which the event occurred.          |
#'    |time_of_period              |character |Game clock time within the period.           |
#'    |player_id                   |integer   |Primary player identifier.                   |
#'    |player_name_first           |character |Primary player first name.                   |
#'    |player_name_last            |character |Primary player last name.                    |
#'    |player_position             |character |Primary player position.                     |
#'    |player_two_id               |integer   |Second player identifier.                    |
#'    |player_two_name_first       |character |Second player first name.                    |
#'    |player_two_name_last        |character |Second player last name.                     |
#'    |player_two_position         |character |Second player position.                      |
#'    |x_coord                     |numeric   |Event x-coordinate on the ice.               |
#'    |y_coord                     |numeric   |Event y-coordinate on the ice.               |
#'    |home_win                    |integer   |Flag for whether the home team won.          |
#'    |player_team_id              |integer   |Team identifier of the primary player.       |
#'    |event_type                  |character |Categorized event type.                      |
#'    |shot_quality                |character |Shot quality descriptor.                     |
#'    |goal                        |logical   |Flag for whether the event was a goal.       |
#'    |goalie_id                   |integer   |Goalie identifier on the play.               |
#'    |goalie_first                |character |Goalie first name.                           |
#'    |goalie_last                 |character |Goalie last name.                            |
#'    |player_three_id             |integer   |Third player identifier.                     |
#'    |player_three_name_first     |character |Third player first name.                     |
#'    |player_three_name_last      |character |Third player last name.                      |
#'    |player_three_position       |character |Third player position.                       |
#'    |empty_net                   |character |Empty-net flag.                              |
#'    |game_winner                 |character |Game-winning-goal flag.                      |
#'    |penalty_shot                |character |Penalty-shot flag.                           |
#'    |insurance                   |character |Insurance-goal flag.                         |
#'    |power_play                  |integer   |Power-play flag.                             |
#'    |short_handed                |character |Short-handed flag.                           |
#'    |plus_player_one_id          |integer   |On-ice plus player one identifier.           |
#'    |plus_player_one_first       |character |On-ice plus player one first name.           |
#'    |plus_player_one_last        |character |On-ice plus player one last name.            |
#'    |plus_player_one_position    |character |On-ice plus player one position.             |
#'    |plus_player_two_id          |integer   |On-ice plus player two identifier.           |
#'    |plus_player_two_first       |character |On-ice plus player two first name.           |
#'    |plus_player_two_last        |character |On-ice plus player two last name.            |
#'    |plus_player_two_position    |character |On-ice plus player two position.             |
#'    |plus_player_three_id        |integer   |On-ice plus player three identifier.         |
#'    |plus_player_three_first     |character |On-ice plus player three first name.         |
#'    |plus_player_three_last      |character |On-ice plus player three last name.          |
#'    |plus_player_three_position  |character |On-ice plus player three position.           |
#'    |plus_player_four_id         |integer   |On-ice plus player four identifier.          |
#'    |plus_player_four_first      |character |On-ice plus player four first name.          |
#'    |plus_player_four_last       |character |On-ice plus player four last name.           |
#'    |plus_player_four_position   |character |On-ice plus player four position.            |
#'    |plus_player_five_id         |integer   |On-ice plus player five identifier.          |
#'    |plus_player_five_first      |character |On-ice plus player five first name.          |
#'    |plus_player_five_last       |character |On-ice plus player five last name.           |
#'    |plus_player_five_position   |character |On-ice plus player five position.            |
#'    |minus_player_one_id         |integer   |On-ice minus player one identifier.          |
#'    |minus_player_one_first      |character |On-ice minus player one first name.          |
#'    |minus_player_one_last       |character |On-ice minus player one last name.           |
#'    |minus_player_one_position   |character |On-ice minus player one position.            |
#'    |minus_player_two_id         |integer   |On-ice minus player two identifier.          |
#'    |minus_player_two_first      |character |On-ice minus player two first name.          |
#'    |minus_player_two_last       |character |On-ice minus player two last name.           |
#'    |minus_player_two_position   |character |On-ice minus player two position.            |
#'    |minus_player_three_id       |integer   |On-ice minus player three identifier.        |
#'    |minus_player_three_first    |character |On-ice minus player three first name.        |
#'    |minus_player_three_last     |character |On-ice minus player three last name.         |
#'    |minus_player_three_position |character |On-ice minus player three position.          |
#'    |minus_player_four_id        |integer   |On-ice minus player four identifier.         |
#'    |minus_player_four_first     |character |On-ice minus player four first name.         |
#'    |minus_player_four_last      |character |On-ice minus player four last name.          |
#'    |minus_player_four_position  |character |On-ice minus player four position.           |
#'    |minus_player_five_id        |integer   |On-ice minus player five identifier.         |
#'    |minus_player_five_first     |character |On-ice minus player five first name.         |
#'    |minus_player_five_last      |character |On-ice minus player five last name.          |
#'    |minus_player_five_position  |character |On-ice minus player five position.           |
#'    |penalty_length              |character |Penalty length in minutes.                   |
#'    |game_date                   |character |Game date.                                   |
#'    |game_season                 |integer   |Season (concluding year, YYYY).              |
#'    |game_season_id              |character |HockeyTech season identifier.                |
#'    |home_team_id                |integer   |Home team identifier.                        |
#'    |home_team                   |character |Home team name.                              |
#'    |away_team_id                |integer   |Away team identifier.                        |
#'    |away_team                   |character |Away team name.                              |
#'    |x_coord_original            |integer   |Original raw x-coordinate.                   |
#'    |y_coord_original            |integer   |Original raw y-coordinate.                   |
#'    |x_coord_neutral             |integer   |Neutral-orientation x-coordinate.            |
#'    |y_coord_neutral             |integer   |Neutral-orientation y-coordinate.            |
#'    |x_coord_fixed               |numeric   |Fixed-orientation x-coordinate.              |
#'    |y_coord_fixed               |numeric   |Fixed-orientation y-coordinate.              |
#'    |x_coord_right               |numeric   |Right-orientation x-coordinate.              |
#'    |y_coord_right               |numeric   |Right-orientation y-coordinate.              |
#'    |x_coord_vertical            |numeric   |Vertical-orientation x-coordinate.           |
#'    |y_coord_vertical            |numeric   |Vertical-orientation y-coordinate.           |
#'    |minute_start                |integer   |Minute the event started.                    |
#'    |second_start                |integer   |Second the event started.                    |
#'    |clock                       |character |Game clock string.                           |
#'    |sec_from_start              |integer   |Seconds elapsed from the start of the game.  |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_pbp(2024))
#' }
load_pwhl_pbp <- function(seasons = most_recent_pwhl_season(), ...,
                          dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_pbp", file_prefix = "play_by_play",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL player box scores**
#' @name load_pwhl_player_box
NULL
#' @title
#' **Load cleaned PWHL player box scores from the data repo**
#' @rdname load_pwhl_player_box
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   player box score data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments passed to an underlying function that writes
#'   the season data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the player box data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name         |types     |description                              |
#'    |:----------------|:---------|:----------------------------------------|
#'    |player_id        |character |Unique player identifier.                |
#'    |first_name       |character |Player first name.                       |
#'    |last_name        |character |Player last name.                        |
#'    |position         |character |Player position.                         |
#'    |team_id          |integer   |Unique team identifier.                  |
#'    |game_id          |integer   |Unique game identifier.                  |
#'    |league           |character |League code.                             |
#'    |toi              |character |Time on ice (MM:SS).                     |
#'    |time_on_ice      |numeric   |Time on ice in seconds.                  |
#'    |goals            |integer   |Goals scored.                            |
#'    |assists          |integer   |Assists.                                 |
#'    |points           |integer   |Total points (goals + assists).          |
#'    |shots            |integer   |Shots on goal.                           |
#'    |hits             |integer   |Hits.                                    |
#'    |blocked_shots    |integer   |Blocked shots.                           |
#'    |penalty_minutes  |integer   |Penalty minutes.                         |
#'    |plus_minus       |integer   |Plus/minus rating.                       |
#'    |faceoff_attempts |integer   |Faceoff attempts.                        |
#'    |faceoff_wins     |integer   |Faceoff wins.                            |
#'    |faceoff_losses   |integer   |Faceoff losses.                          |
#'    |faceoff_pct      |numeric   |Faceoff win percentage.                  |
#'    |starting         |character |Whether the player started the game.     |
#'    |player_type      |character |Player type (skater or goalie).          |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_player_box(2024))
#' }
load_pwhl_player_box <- function(seasons = most_recent_pwhl_season(), ...,
                                 dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_player_boxscores", file_prefix = "player_box",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL schedules**
#' @name load_pwhl_schedule
NULL
#' @title
#' **Load cleaned PWHL schedules from the data repo**
#' @rdname load_pwhl_schedule
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   schedule data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments passed to an underlying function that writes
#'   the season data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the schedule data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                     |
#'    |:---------------|:---------|:-----------------------------------------------|
#'    |game_id         |character |Unique game identifier.                         |
#'    |season          |integer   |Season (concluding year, YYYY).                 |
#'    |game_date       |character |Game date.                                      |
#'    |game_status     |character |Game status text.                               |
#'    |home_team       |character |Home team name.                                 |
#'    |home_team_id    |character |Home team identifier.                           |
#'    |away_team       |character |Away team name.                                 |
#'    |away_team_id    |character |Away team identifier.                           |
#'    |home_score      |character |Home team final score.                          |
#'    |away_score      |character |Away team final score.                          |
#'    |winner          |character |Winning team.                                   |
#'    |venue           |character |Venue name.                                     |
#'    |venue_url       |character |Venue URL.                                      |
#'    |game_type       |character |Game type the row belongs to.                   |
#'    |game_json       |logical   |Whether the game JSON is available.             |
#'    |game_json_url   |glue      |URL to the game JSON feed.                      |
#'    |PBP             |logical   |Whether play-by-play data is available.         |
#'    |player_box      |logical   |Whether player box data is available.           |
#'    |skater_box      |logical   |Whether skater box data is available.           |
#'    |goalie_box      |logical   |Whether goalie box data is available.           |
#'    |team_box        |logical   |Whether team box data is available.             |
#'    |game_info       |logical   |Whether game info data is available.            |
#'    |game_rosters    |logical   |Whether game rosters data is available.         |
#'    |scoring_summary |logical   |Whether scoring summary data is available.      |
#'    |penalty_summary |logical   |Whether penalty summary data is available.      |
#'    |three_stars     |logical   |Whether three stars data is available.          |
#'    |officials       |logical   |Whether officials data is available.            |
#'    |shots_by_period |logical   |Whether shots-by-period data is available.      |
#'    |shootout        |logical   |Whether shootout data is available.             |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_schedule(2024))
#' }
load_pwhl_schedule <- function(seasons = most_recent_pwhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_schedules", file_prefix = "pwhl_schedule",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL team rosters**
#' @name load_pwhl_rosters
NULL
#' @title
#' **Load cleaned PWHL team rosters from the data repo**
#' @rdname load_pwhl_rosters
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   roster data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments passed to an underlying function that writes
#'   the season data into a database.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the rosters data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name      |types     |description                            |
#'    |:-------------|:---------|:--------------------------------------|
#'    |team_id       |integer   |Unique team identifier.                |
#'    |team          |character |Team name.                             |
#'    |team_abbr     |character |Team abbreviation.                     |
#'    |team_side     |character |Home or away indicator.                |
#'    |player_type   |character |Player type (skater or goalie).        |
#'    |player_id     |integer   |Unique player identifier.              |
#'    |first_name    |character |Player first name.                     |
#'    |last_name     |character |Player last name.                      |
#'    |jersey_number |integer   |Jersey number.                         |
#'    |position      |character |Player position.                       |
#'    |birth_date    |character |Player birth date.                     |
#'    |season        |integer   |Season (concluding year, YYYY).        |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_rosters(2024))
#' }
load_pwhl_rosters <- function(seasons = most_recent_pwhl_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_rosters", file_prefix = "rosters",
    dbConnection = dbConnection, tablename = tablename
  )
}


# ───────────────────────────────────────────────────────────────────────────
# New PWHL season-level loaders (added 2026-04)
# ───────────────────────────────────────────────────────────────────────────

#' **Load fastRhockey PWHL skater box scores**
#' @name load_pwhl_skater_box
NULL
#' @title
#' **Load cleaned PWHL skater box scores from the data repo**
#' @rdname load_pwhl_skater_box
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   skater-only box score data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_skater_boxscores`. File naming convention:
#'   `skater_box_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the skater box data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name         |types     |description                              |
#'    |:----------------|:---------|:----------------------------------------|
#'    |player_id        |character |Unique player identifier.                |
#'    |first_name       |character |Player first name.                       |
#'    |last_name        |character |Player last name.                        |
#'    |position         |character |Player position.                         |
#'    |team_id          |integer   |Unique team identifier.                  |
#'    |game_id          |integer   |Unique game identifier.                  |
#'    |league           |character |League code.                             |
#'    |toi              |character |Time on ice (MM:SS).                     |
#'    |time_on_ice      |numeric   |Time on ice in seconds.                  |
#'    |goals            |integer   |Goals scored.                            |
#'    |assists          |integer   |Assists.                                 |
#'    |points           |integer   |Total points (goals + assists).          |
#'    |shots            |integer   |Shots on goal.                           |
#'    |hits             |integer   |Hits.                                    |
#'    |blocked_shots    |integer   |Blocked shots.                           |
#'    |penalty_minutes  |integer   |Penalty minutes.                         |
#'    |plus_minus       |integer   |Plus/minus rating.                       |
#'    |faceoff_attempts |integer   |Faceoff attempts.                        |
#'    |faceoff_wins     |integer   |Faceoff wins.                            |
#'    |faceoff_losses   |integer   |Faceoff losses.                          |
#'    |faceoff_pct      |numeric   |Faceoff win percentage.                  |
#'    |starting         |character |Whether the player started the game.     |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_skater_box(2024))
#' }
load_pwhl_skater_box <- function(seasons = most_recent_pwhl_season(), ...,
                                 dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_skater_boxscores", file_prefix = "skater_box",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL goalie box scores**
#' @name load_pwhl_goalie_box
NULL
#' @title
#' **Load cleaned PWHL goalie box scores from the data repo**
#' @rdname load_pwhl_goalie_box
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   goalie-only box score data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_goalie_boxscores`. File naming convention:
#'   `goalie_box_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the goalie box data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name         |types     |description                              |
#'    |:----------------|:---------|:----------------------------------------|
#'    |player_id        |character |Unique player identifier.                |
#'    |first_name       |character |Goalie first name.                       |
#'    |last_name        |character |Goalie last name.                        |
#'    |position         |character |Player position.                         |
#'    |team_id          |integer   |Unique team identifier.                  |
#'    |game_id          |integer   |Unique game identifier.                  |
#'    |league           |character |League code.                             |
#'    |toi              |character |Time on ice (MM:SS).                     |
#'    |time_on_ice      |numeric   |Time on ice in seconds.                  |
#'    |saves            |integer   |Saves made.                              |
#'    |goals_against    |integer   |Goals against.                           |
#'    |shots_against    |integer   |Shots faced.                             |
#'    |goals            |integer   |Goals scored.                            |
#'    |assists          |integer   |Assists.                                 |
#'    |points           |integer   |Total points (goals + assists).          |
#'    |penalty_minutes  |integer   |Penalty minutes.                         |
#'    |faceoff_attempts |integer   |Faceoff attempts.                        |
#'    |faceoff_wins     |integer   |Faceoff wins.                            |
#'    |faceoff_losses   |integer   |Faceoff losses.                          |
#'    |faceoff_pct      |logical   |Faceoff win percentage.                  |
#'    |starting         |integer   |Whether the goalie started the game.     |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_goalie_box(2024))
#' }
load_pwhl_goalie_box <- function(seasons = most_recent_pwhl_season(), ...,
                                 dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_goalie_boxscores", file_prefix = "goalie_box",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL team box scores**
#' @name load_pwhl_team_box
NULL
#' @title
#' **Load cleaned PWHL team box scores from the data repo**
#' @rdname load_pwhl_team_box
#' @description Helper that loads multiple seasons of pre-scraped PWHL
#'   team-level box score data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_team_boxscores`. Two rows per game (one per
#'   side). File naming convention: `team_box_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the team box data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name          |types     |description                              |
#'    |:-----------------|:---------|:----------------------------------------|
#'    |game_id           |integer   |Unique game identifier.                  |
#'    |team_id           |integer   |Unique team identifier.                  |
#'    |team              |character |Team name.                               |
#'    |team_abbr         |character |Team abbreviation.                       |
#'    |team_side         |character |Home or away indicator.                  |
#'    |shots             |integer   |Shots on goal.                           |
#'    |goals             |integer   |Goals scored.                            |
#'    |hits              |integer   |Hits.                                    |
#'    |pp_goals          |integer   |Power-play goals.                        |
#'    |pp_opportunities  |integer   |Power-play opportunities.                |
#'    |goal_count        |integer   |Total goals recorded.                    |
#'    |assist_count      |integer   |Total assists recorded.                  |
#'    |penalty_minutes   |integer   |Penalty minutes.                         |
#'    |infraction_count  |integer   |Number of infractions.                   |
#'    |faceoff_attempts  |integer   |Faceoff attempts.                        |
#'    |faceoff_wins      |integer   |Faceoff wins.                            |
#'    |faceoff_win_pct   |numeric   |Faceoff win percentage.                  |
#'    |season_wins       |integer   |Season wins entering/after the game.     |
#'    |season_losses     |integer   |Season losses entering/after the game.   |
#'    |season_ot_wins    |integer   |Season overtime wins.                    |
#'    |season_ot_losses  |integer   |Season overtime losses.                  |
#'    |season_so_losses  |integer   |Season shootout losses.                  |
#'    |season_record     |character |Season record after this game.           |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_team_box(2024))
#' }
load_pwhl_team_box <- function(seasons = most_recent_pwhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_team_boxscores", file_prefix = "team_box",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL game info**
#' @name load_pwhl_game_info
NULL
#' @title
#' **Load cleaned PWHL game info from the data repo**
#' @rdname load_pwhl_game_info
#' @description Helper that loads multiple seasons of per-game PWHL metadata
#'   (one row per game) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_game_info`. File naming convention:
#'   `game_info_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the game info data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                |
#'    |:---------------|:---------|:------------------------------------------|
#'    |game_id         |integer   |Unique game identifier.                    |
#'    |game_number     |character |League game number.                        |
#'    |game_date       |character |Human-readable game date.                  |
#'    |game_date_iso   |character |ISO-8601 game start datetime.              |
#'    |start_time      |character |Start time (local).                        |
#'    |end_time        |character |End time (local).                          |
#'    |game_duration   |character |Game length (H:MM).                        |
#'    |game_venue      |character |Venue name.                                |
#'    |attendance      |integer   |Reported attendance.                       |
#'    |game_status     |character |Game status text.                          |
#'    |game_season_id  |integer   |HockeyTech season identifier.              |
#'    |started         |integer   |Flag for whether the game has started.     |
#'    |final           |integer   |Flag for whether the game is final.        |
#'    |home_team_id    |integer   |Home team identifier.                      |
#'    |home_team       |character |Home team name.                            |
#'    |home_team_abbr  |character |Home team abbreviation.                    |
#'    |home_score      |integer   |Home team final score.                     |
#'    |away_team_id    |integer   |Away team identifier.                      |
#'    |away_team       |character |Away team name.                            |
#'    |away_team_abbr  |character |Away team abbreviation.                    |
#'    |away_score      |integer   |Away team final score.                     |
#'    |has_shootout    |integer   |Flag for whether the game went to shootout.|
#'    |game_report_url |character |URL to the game report.                    |
#'    |boxscore_url    |character |URL to the boxscore.                       |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_game_info(2024))
#' }
load_pwhl_game_info <- function(seasons = most_recent_pwhl_season(), ...,
                                dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_game_info", file_prefix = "game_info",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL scoring summaries**
#' @name load_pwhl_scoring_summary
NULL
#' @title
#' **Load cleaned PWHL scoring summaries from the data repo**
#' @rdname load_pwhl_scoring_summary
#' @description Helper that loads multiple seasons of per-goal PWHL scoring
#'   summaries (one row per goal) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_scoring_summary`. File naming convention:
#'   `scoring_summary_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the scoring summary data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name           |types     |description                                |
#'    |:------------------|:---------|:------------------------------------------|
#'    |game_id            |integer   |Unique game identifier.                    |
#'    |period_id          |integer   |Period identifier.                         |
#'    |period             |character |Period long name (e.g. 1st, 1st OT).       |
#'    |time               |character |Game clock at goal (MM:SS).                |
#'    |team_id            |integer   |Scoring team identifier.                   |
#'    |team               |character |Scoring team name.                         |
#'    |team_abbr          |character |Scoring team abbreviation.                 |
#'    |game_goal_id       |integer   |Goal identifier within the game.           |
#'    |scorer_goal_number |integer   |Scorer's season goal number.               |
#'    |scorer_id          |integer   |Goal scorer identifier.                    |
#'    |scorer_first       |character |Scorer first name.                         |
#'    |scorer_last        |character |Scorer last name.                          |
#'    |scorer_position    |character |Scorer position.                           |
#'    |assist_1_id        |integer   |Primary assist player identifier.          |
#'    |assist_1_first     |character |Primary assist first name.                 |
#'    |assist_1_last      |character |Primary assist last name.                  |
#'    |assist_2_id        |integer   |Secondary assist player identifier.        |
#'    |assist_2_first     |character |Secondary assist first name.               |
#'    |assist_2_last      |character |Secondary assist last name.                |
#'    |is_power_play      |integer   |Power-play flag.                           |
#'    |is_short_handed    |integer   |Short-handed flag.                         |
#'    |is_empty_net       |integer   |Empty-net flag.                            |
#'    |is_penalty_shot    |integer   |Penalty-shot flag.                         |
#'    |is_insurance       |integer   |Insurance-goal flag.                       |
#'    |is_game_winning    |integer   |Game-winning-goal flag.                    |
#'    |x_location         |logical   |Goal x-coordinate on the ice.              |
#'    |y_location         |logical   |Goal y-coordinate on the ice.              |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_scoring_summary(2024))
#' }
load_pwhl_scoring_summary <- function(seasons = most_recent_pwhl_season(), ...,
                                      dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_scoring_summary", file_prefix = "scoring_summary",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL penalty summaries**
#' @name load_pwhl_penalty_summary
NULL
#' @title
#' **Load cleaned PWHL penalty summaries from the data repo**
#' @rdname load_pwhl_penalty_summary
#' @description Helper that loads multiple seasons of per-penalty PWHL penalty
#'   summaries (one row per penalty) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_penalty_summary`. File naming convention:
#'   `penalty_summary_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the penalty summary data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name          |types     |description                                 |
#'    |:-----------------|:---------|:-------------------------------------------|
#'    |game_id           |integer   |Unique game identifier.                     |
#'    |period_id         |integer   |Period identifier.                          |
#'    |period            |character |Period long name.                           |
#'    |time              |character |Game clock at infraction (MM:SS).           |
#'    |team_id           |integer   |Penalized team identifier.                  |
#'    |team              |character |Penalized team name.                        |
#'    |team_abbr         |character |Penalized team abbreviation.                |
#'    |game_penalty_id   |integer   |Penalty identifier within the game.         |
#'    |minutes           |integer   |Penalty length in minutes.                  |
#'    |description       |character |Infraction description.                     |
#'    |rule_number       |character |Rulebook rule number.                       |
#'    |is_power_play     |integer   |Power-play flag.                            |
#'    |is_bench          |integer   |Bench-minor flag.                           |
#'    |taken_by_id       |integer   |Identifier of the player who took the penalty.|
#'    |taken_by_first    |character |Offender first name.                        |
#'    |taken_by_last     |character |Offender last name.                         |
#'    |taken_by_position |character |Offender position.                          |
#'    |served_by_id      |integer   |Identifier of the player serving the penalty.|
#'    |served_by_first   |character |First name of the player serving.           |
#'    |served_by_last    |character |Last name of the player serving.            |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_penalty_summary(2024))
#' }
load_pwhl_penalty_summary <- function(seasons = most_recent_pwhl_season(), ...,
                                      dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_penalty_summary", file_prefix = "penalty_summary",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL three stars**
#' @name load_pwhl_three_stars
NULL
#' @title
#' **Load cleaned PWHL three stars of the game from the data repo**
#' @rdname load_pwhl_three_stars
#' @description Helper that loads multiple seasons of PWHL three-stars-of-the-
#'   game data (one to three rows per game) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_three_stars`. File naming convention:
#'   `three_stars_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the three stars data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name      |types     |description                              |
#'    |:-------------|:---------|:----------------------------------------|
#'    |game_id       |integer   |Unique game identifier.                  |
#'    |star          |integer   |Star rank (1-3).                         |
#'    |team_id       |integer   |Star's team identifier.                  |
#'    |team          |character |Star's team name.                        |
#'    |team_abbr     |character |Star's team abbreviation.                |
#'    |player_id     |integer   |Star's player identifier.                |
#'    |first_name    |character |Star's first name.                       |
#'    |last_name     |character |Star's last name.                        |
#'    |jersey_number |integer   |Jersey number.                           |
#'    |position      |character |Player position.                         |
#'    |is_goalie     |integer   |Goalie flag.                             |
#'    |is_home       |integer   |Home-team flag.                          |
#'    |goals         |integer   |Goals scored in this game.               |
#'    |assists       |integer   |Assists in this game.                    |
#'    |points        |integer   |Points in this game.                     |
#'    |shots         |integer   |Shots on goal in this game.              |
#'    |saves         |integer   |Saves made (goalies).                    |
#'    |shots_against |integer   |Shots faced (goalies).                   |
#'    |goals_against |integer   |Goals against (goalies).                 |
#'    |time_on_ice   |character |Time on ice (MM:SS).                     |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_three_stars(2024))
#' }
load_pwhl_three_stars <- function(seasons = most_recent_pwhl_season(), ...,
                                  dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_three_stars", file_prefix = "three_stars",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL officials**
#' @name load_pwhl_officials
NULL
#' @title
#' **Load cleaned PWHL game officials from the data repo**
#' @rdname load_pwhl_officials
#' @description Helper that loads multiple seasons of PWHL game-officials data
#'   (referees, linespersons, scorekeepers) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_officials`. File naming convention:
#'   `officials_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the officials data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name      |types     |description                                  |
#'    |:-------------|:---------|:--------------------------------------------|
#'    |game_id       |integer   |Unique game identifier.                      |
#'    |role          |character |Grouped official role (Referee/Linesperson). |
#'    |first_name    |character |Official's first name.                       |
#'    |last_name     |character |Official's last name.                        |
#'    |jersey_number |integer   |Official's jersey number.                    |
#'    |official_role |character |Official's specific role.                    |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_officials(2024))
#' }
load_pwhl_officials <- function(seasons = most_recent_pwhl_season(), ...,
                                dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_officials", file_prefix = "officials",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL shots-by-period**
#' @name load_pwhl_shots_by_period
NULL
#' @title
#' **Load cleaned PWHL shots-by-period from the data repo**
#' @rdname load_pwhl_shots_by_period
#' @description Helper that loads multiple seasons of per-period shot/goal
#'   totals from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_shots_by_period`. File naming convention:
#'   `shots_by_period_{end_year}.rds`.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the shots-by-period data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name   |types     |description                                |
#'    |:----------|:---------|:------------------------------------------|
#'    |game_id    |integer   |Unique game identifier.                    |
#'    |period_id  |integer   |Period identifier (1-3, 4 = OT, 5 = SO).   |
#'    |period     |character |Period long name.                          |
#'    |home_goals |integer   |Home goals in the period.                  |
#'    |home_shots |integer   |Home shots in the period.                  |
#'    |away_goals |integer   |Away goals in the period.                  |
#'    |away_shots |integer   |Away shots in the period.                  |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_shots_by_period(2024))
#' }
load_pwhl_shots_by_period <- function(seasons = most_recent_pwhl_season(), ...,
                                      dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_shots_by_period", file_prefix = "shots_by_period",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL shootout summaries**
#' @name load_pwhl_shootout
NULL
#' @title
#' **Load cleaned PWHL shootout summaries from the data repo**
#' @rdname load_pwhl_shootout
#' @description Helper that loads multiple seasons of PWHL shootout-attempt
#'   data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_shootout`. File naming convention:
#'   `shootout_summary_{end_year}.rds`. Note that some seasons (notably the
#'   2024 inaugural season) do not contain any shootout games.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the shootout data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per shootout attempt
#'   and the following columns:
#'
#'    |col_name      |types     |description                          |
#'    |:-------------|:---------|:------------------------------------|
#'    |game_id       |integer   |Unique game identifier.              |
#'    |round         |integer   |Shootout round number.               |
#'    |team_side     |character |Shooting team side ("home"/"away").  |
#'    |shooter_id    |integer   |Shooter player identifier.           |
#'    |shooter_first |character |Shooter first name.                  |
#'    |shooter_last  |character |Shooter last name.                   |
#'    |goalie_id     |integer   |Opposing goalie identifier.          |
#'    |goalie_first  |character |Opposing goalie first name.          |
#'    |goalie_last   |character |Opposing goalie last name.           |
#'    |is_goal       |integer   |Whether the attempt scored (1/0).    |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_shootout(2024))
#' }
load_pwhl_shootout <- function(seasons = most_recent_pwhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_shootout", file_prefix = "shootout_summary",
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey PWHL per-game rosters**
#' @name load_pwhl_game_rosters
NULL
#' @title
#' **Load cleaned PWHL per-game rosters from the data repo**
#' @rdname load_pwhl_game_rosters
#' @description Helper that loads multiple seasons of PWHL per-game rosters
#'   (one row per player per game, including starters and scratches) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#'
#'   Source release tag: `pwhl_game_rosters`. File naming convention:
#'   `game_rosters_{end_year}.rds`. Distinct from
#'   [`load_pwhl_rosters()`][load_pwhl_rosters], which returns season-level
#'   team rosters.
#'
#' @param seasons A vector of 4-digit years associated with given PWHL seasons.
#'   (Min: 2024)
#' @param ... Additional arguments (currently unused; kept for API symmetry).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the per-game rosters data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name      |types     |description                              |
#'    |:-------------|:---------|:----------------------------------------|
#'    |game_id       |integer   |Unique game identifier.                  |
#'    |team_id       |integer   |Unique team identifier.                  |
#'    |team          |character |Team name.                               |
#'    |team_abbr     |character |Team abbreviation.                       |
#'    |team_side     |character |Home or away indicator.                  |
#'    |player_type   |character |Player type (skater or goalie).          |
#'    |player_id     |integer   |Unique player identifier.                |
#'    |first_name    |character |Player first name.                       |
#'    |last_name     |character |Player last name.                        |
#'    |jersey_number |integer   |Jersey number.                           |
#'    |position      |character |Player position.                         |
#'    |birth_date    |character |Player birth date.                       |
#'    |starting      |integer   |Whether the player started the game.     |
#'    |status        |character |Status string (e.g. captain markers).    |
#' @export
#' @examples
#' \donttest{
#'   try(load_pwhl_game_rosters(2024))
#' }
load_pwhl_game_rosters <- function(seasons = most_recent_pwhl_season(), ...,
                                   dbConnection = NULL, tablename = NULL) {
  .pwhl_release_loader(seasons,
    release_tag = "pwhl_game_rosters", file_prefix = "game_rosters",
    dbConnection = dbConnection, tablename = tablename
  )
}


# load PWHL games file (internal helper for update_pwhl_db)
load_pwhl_games <- function() {
  release_url <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/pwhl_schedules/pwhl_games_in_data_repo.rds"
  )
  dat <- tryCatch(rds_from_url(release_url), error = function(e) NULL)
  if (is.null(dat)) {
    # Fall back to raw repo file if release not yet populated
    fallback <- paste0(
      "https://raw.githubusercontent.com/sportsdataverse/",
      "fastRhockey-pwhl-data/main/pwhl/pwhl_games_in_data_repo.rds"
    )
    dat <- rds_from_url(fallback)
  }
  return(dat)
}


#' @name update_pwhl_db
#' @aliases update_pwhl_db pwhl_db pwhl_pbp_db
#' @title
#' **Update or create a fastRhockey PWHL play-by-play database**
#' @description `update_pwhl_db()` updates or creates a database with
#'   `fastRhockey` play-by-play data of all completed and available PWHL games
#'   since the 2024 inaugural season.
#'
#' @details This function creates and updates a data table with the name
#'   `tblname` within a SQLite database (other drivers via `db_connection`)
#'   located in `dbdir` and named `dbname`.
#'   The data table combines all play-by-play data for every available game back
#'   to the 2024 season and adds the most recent completed games as soon as they
#'   are available for `fastRhockey`.
#'
#'   The argument `force_rebuild` is of hybrid type. It can rebuild the play-
#'   by-play data table either for the whole fastRhockey PWHL era
#'   (with `force_rebuild = TRUE`) or just for specified seasons
#'   (e.g. `force_rebuild = 2024`).
#'   Please note the following behavior:
#'   * `force_rebuild = TRUE`: The data table with the name `tblname`
#'     will be removed completely and rebuilt from scratch.
#'   * `force_rebuild = c(2024, 2025)`: The data table with the name `tblname`
#'     will be preserved and only rows from the specified seasons will be
#'     deleted and re-added.
#'
#'   The parameter `db_connection` is intended for advanced users who want
#'   to use other DBI drivers, such as MariaDB, Postgres or odbc. Please note
#'   that the arguments `dbdir` and `dbname` are dropped in case a
#'   `db_connection` is provided but the argument `tblname` will still be used
#'   to write the data table into the database.
#'
#' @param dbdir Directory in which the database is or shall be located
#' @param dbname File name of an existing or desired SQLite database within
#'   `dbdir`
#' @param tblname The name of the play-by-play data table within the database
#' @param force_rebuild Hybrid parameter (logical or numeric) to rebuild parts
#'   of or the complete play-by-play data table within the database (please see
#'   details for further information)
#' @param db_connection A `DBIConnection` object, as returned by
#'   [DBI::dbConnect()] (please see details for further information)
#' @return Invisible NULL. Side effect: updates the database.
#' @export
#' @examples
#' \dontrun{
#'   update_pwhl_db()
#' }
update_pwhl_db <- function(dbdir = ".",
                           dbname = "fastRhockey_db",
                           tblname = "fastRhockey_pwhl_pbp",
                           force_rebuild = FALSE,
                           db_connection = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  rule_header("Update fastRhockey PWHL Play-by-Play Database")

  if (!is_installed("DBI") | !is_installed("purrr") |
      (!is_installed("RSQLite") & is.null(db_connection))) {
    cli::cli_abort(
      "{my_time()} | Packages {.val DBI}, {.val RSQLite} and {.val purrr} required for database communication. Please install them."
    )
  }

  if (any(force_rebuild == "NEW")) {
    cli::cli_abort(
      "{my_time()} | The argument {.val force_rebuild = NEW} is only for internal usage!"
    )
  }

  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    cli::cli_abort(
      "{my_time()} | The argument {.val force_rebuild} has to be either logical or numeric!"
    )
  }

  if (!dir.exists(dbdir) & is.null(db_connection)) {
    cli::cli_alert_danger(
      "{my_time()} | Directory {.file {dbdir}} doesn't exist yet. Try creating..."
    )
    dir.create(dbdir)
  }

  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(
      RSQLite::SQLite(),
      glue::glue("{dbdir}/{dbname}")
    )
  } else {
    connection <- db_connection
  }

  # create db if it doesn't exist or user forces rebuild
  if (!DBI::dbExistsTable(connection, tblname)) {
    build_pwhl_db(tblname, connection, rebuild = "NEW")
  } else if (DBI::dbExistsTable(connection, tblname) & all(force_rebuild != FALSE)) {
    build_pwhl_db(tblname, connection, rebuild = force_rebuild)
  }

  # get completed games
  user_message("Checking for missing completed games...", "todo")
  completed_games <- load_pwhl_games() %>%
    dplyr::filter(.data$season >= 2024) %>%
    dplyr::pull("game_id")

  missing <- get_missing_pwhl_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if (length(missing) > 50) {
    build_pwhl_db(
      tblname,
      connection,
      show_message = FALSE,
      rebuild = as.numeric(unique(stringr::str_sub(missing, 1, 4)))
    )
    missing <- get_missing_pwhl_games(completed_games, connection, tblname)
  }

  message_completed("Database update completed", in_builder = TRUE)
  cli::cli_alert_info(
    "{my_time()} | Path to your db: {.file {DBI::dbGetInfo(connection)$dbname}}"
  )
  if (is.null(db_connection)) DBI::dbDisconnect(connection)
  rule_footer("DONE")
}

# helper to build PWHL database from scratch
build_pwhl_db <- function(tblname = "fastRhockey_pwhl_pbp", db_conn,
                          rebuild = FALSE, show_message = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  valid_seasons <- load_pwhl_games() %>%
    dplyr::filter(.data$season >= 2024) %>%
    dplyr::group_by(.data$season) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  if (all(rebuild == TRUE)) {
    cli::cli_ul(
      "{my_time()} | Purging the complete data table {.val {tblname}} in your connected database..."
    )
    DBI::dbRemoveTable(db_conn, tblname)
    seasons <- valid_seasons %>% dplyr::pull("season")
    cli::cli_ul(
      "{my_time()} | Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}..."
    )
  } else if (is.numeric(rebuild) & all(rebuild %in% valid_seasons$season)) {
    string <- paste0(rebuild, collapse = ", ")
    if (show_message) {
      cli::cli_ul(
        "{my_time()} | Purging {string} season(s) from the data table {.val {tblname}} in your connected database..."
      )
    }
    DBI::dbExecute(
      db_conn,
      glue::glue_sql(
        "DELETE FROM {`tblname`} WHERE season IN ({vals*})",
        vals = rebuild, .con = db_conn
      )
    )
    seasons <- valid_seasons %>%
      dplyr::filter(.data$season %in% rebuild) %>%
      dplyr::pull("season")
    cli::cli_ul(
      "{my_time()} | Starting download of the {string} season(s)..."
    )
  } else if (all(rebuild == "NEW")) {
    cli::cli_alert_info(
      "{my_time()} | Can't find the data table {.val {tblname}} in your database. Will load the play-by-play data from scratch."
    )
    seasons <- valid_seasons %>% dplyr::pull("season")
    cli::cli_ul(
      "{my_time()} | Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}..."
    )
  } else {
    seasons <- NULL
    cli::cli_alert_danger(
      "{my_time()} | At least one invalid value passed to argument {.val force_rebuild}. Please try again with valid input."
    )
  }

  if (!is.null(seasons)) {
    load_pwhl_pbp(seasons, dbConnection = db_conn, tablename = tblname)
  }
}

# helper to check completed PWHL games against games in database
get_missing_pwhl_games <- function(completed_games, dbConnection, tablename) {
  db_ids <- dplyr::tbl(dbConnection, tablename) %>%
    dplyr::select("game_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull("game_id")

  need_scrape <- completed_games[!completed_games %in% db_ids]

  cli::cli_alert_info(
    "{my_time()} | You have {length(db_ids)} games and are missing {length(need_scrape)}."
  )
  return(need_scrape)
}
