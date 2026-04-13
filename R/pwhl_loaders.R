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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data` with one row per
#'   skater per game. Common columns include:
#'
#'   | column            | type      | description                            |
#'   |-------------------|-----------|----------------------------------------|
#'   | `game_id`         | integer   | PWHL game id                           |
#'   | `team_id`         | integer   | HockeyTech team id                     |
#'   | `player_id`       | integer   | HockeyTech player id                   |
#'   | `first_name`      | character | player first name                      |
#'   | `last_name`       | character | player last name                       |
#'   | `jersey_number`   | integer   | jersey number                          |
#'   | `position`        | character | position code (C, LW, RW, LD, RD)      |
#'   | `goals`           | integer   | goals                                  |
#'   | `assists`         | integer   | assists                                |
#'   | `points`          | integer   | total points                           |
#'   | `penalty_minutes` | integer   | PIM                                    |
#'   | `plus_minus`      | integer   | plus/minus                             |
#'   | `shots`           | integer   | shots on goal                          |
#'   | `faceoff_attempts`| integer   | faceoff attempts                       |
#'   | `faceoff_wins`    | integer   | faceoff wins                           |
#'   | `time_on_ice`     | character | total TOI (`MM:SS`)                    |
#'   | `starting`        | character | started the game (`0`/`1`)             |
#'   | `player_type`     | character | always `"skater"`                      |
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
#' @return A data frame of class `fastRhockey_data` with one row per goalie
#'   per game. Common columns include:
#'
#'   | column          | type      | description                              |
#'   |-----------------|-----------|------------------------------------------|
#'   | `game_id`       | integer   | PWHL game id                             |
#'   | `team_id`       | integer   | HockeyTech team id                       |
#'   | `player_id`     | integer   | HockeyTech player id                     |
#'   | `first_name`    | character | goalie first name                        |
#'   | `last_name`     | character | goalie last name                         |
#'   | `jersey_number` | integer   | jersey number                            |
#'   | `time_on_ice`   | character | total TOI (`MM:SS`)                      |
#'   | `shots_against` | integer   | shots faced                              |
#'   | `goals_against` | integer   | goals allowed                            |
#'   | `saves`         | integer   | saves made                               |
#'   | `starting`      | character | started the game (`0`/`1`)               |
#'   | `player_type`   | character | always `"goalie"`                        |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column             | type      | description                              |
#'   |--------------------|-----------|------------------------------------------|
#'   | `game_id`          | integer   | PWHL game id                             |
#'   | `team_id`          | integer   | HockeyTech team id                       |
#'   | `team`             | character | team full name                           |
#'   | `team_abbr`        | character | three-letter abbreviation                |
#'   | `team_side`        | character | `"home"` or `"away"`                     |
#'   | `goals`            | integer   | goals scored                             |
#'   | `shots`            | integer   | shots on goal                            |
#'   | `pp_goals`         | integer   | power-play goals                         |
#'   | `pp_opportunities` | integer   | power-play opportunities                 |
#'   | `penalty_minutes`  | integer   | total PIM                                |
#'   | `infraction_count` | integer   | number of infractions                    |
#'   | `faceoff_attempts` | integer   | faceoff attempts                         |
#'   | `faceoff_wins`     | integer   | faceoff wins                             |
#'   | `faceoff_win_pct`  | numeric   | faceoff win percentage                   |
#'   | `season_record`    | character | season record after this game            |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column            | type      | description                                |
#'   |-------------------|-----------|--------------------------------------------|
#'   | `game_id`         | integer   | PWHL game id                               |
#'   | `game_number`     | character | league game number                         |
#'   | `game_date`       | character | human-readable game date                   |
#'   | `game_date_iso`   | character | ISO-8601 game start datetime               |
#'   | `start_time`      | character | start time (local)                         |
#'   | `end_time`        | character | end time (local)                           |
#'   | `game_duration`   | character | game length (`H:MM`)                       |
#'   | `game_venue`      | character | venue name                                 |
#'   | `attendance`      | integer   | reported attendance                        |
#'   | `game_status`     | character | final / status text                        |
#'   | `game_season_id`  | integer   | HockeyTech season id                       |
#'   | `home_team_id`    | integer   | home team id                               |
#'   | `home_team`       | character | home team name                             |
#'   | `home_team_abbr`  | character | home abbreviation                          |
#'   | `home_score`      | integer   | home final score                           |
#'   | `away_team_id`    | integer   | away team id                               |
#'   | `away_team`       | character | away team name                             |
#'   | `away_team_abbr`  | character | away abbreviation                          |
#'   | `away_score`      | integer   | away final score                           |
#'   | `has_shootout`    | integer   | shootout flag                              |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column                | type      | description                              |
#'   |-----------------------|-----------|------------------------------------------|
#'   | `game_id`             | integer   | PWHL game id                             |
#'   | `period_id`           | integer   | period id                                |
#'   | `period`              | character | period long name (e.g. `1st`, `1st OT`)  |
#'   | `time`                | character | game clock at goal (`MM:SS`)             |
#'   | `team_id`             | integer   | scoring team id                          |
#'   | `team`                | character | scoring team name                        |
#'   | `scorer_id`           | integer   | goal scorer id                           |
#'   | `scorer_first`        | character | scorer first name                        |
#'   | `scorer_last`         | character | scorer last name                         |
#'   | `assist_1_id`         | integer   | primary assist id                        |
#'   | `assist_2_id`         | integer   | secondary assist id                      |
#'   | `is_power_play`       | integer   | power-play flag                          |
#'   | `is_short_handed`     | integer   | short-handed flag                        |
#'   | `is_empty_net`        | integer   | empty-net flag                           |
#'   | `is_penalty_shot`     | integer   | penalty-shot flag                        |
#'   | `is_game_winning`     | integer   | game-winning-goal flag                   |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column            | type      | description                                  |
#'   |-------------------|-----------|----------------------------------------------|
#'   | `game_id`         | integer   | PWHL game id                                 |
#'   | `period_id`       | integer   | period id                                    |
#'   | `period`          | character | period long name                             |
#'   | `time`            | character | game clock at infraction (`MM:SS`)           |
#'   | `team_id`         | integer   | penalized team id                            |
#'   | `team`            | character | penalized team name                          |
#'   | `minutes`         | numeric   | penalty length in minutes                    |
#'   | `description`     | character | infraction description                       |
#'   | `is_power_play`   | integer   | power-play flag                              |
#'   | `is_bench`        | integer   | bench-minor flag                             |
#'   | `taken_by_id`     | integer   | player who took the penalty                  |
#'   | `taken_by_first`  | character | offender first name                          |
#'   | `taken_by_last`   | character | offender last name                           |
#'   | `served_by_id`    | integer   | player serving the penalty                   |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column           | type      | description                                |
#'   |------------------|-----------|--------------------------------------------|
#'   | `game_id`        | integer   | PWHL game id                               |
#'   | `star`           | integer   | star rank (1-3)                            |
#'   | `team_id`        | integer   | star's team id                             |
#'   | `team`           | character | star's team name                           |
#'   | `player_id`      | integer   | star's player id                           |
#'   | `first_name`     | character | star's first name                          |
#'   | `last_name`      | character | star's last name                           |
#'   | `jersey_number`  | integer   | jersey number                              |
#'   | `position`       | character | position code                              |
#'   | `is_goalie`      | integer   | goalie flag                                |
#'   | `is_home`        | integer   | home-team flag                             |
#'   | `goals`          | integer   | star's goals in this game                  |
#'   | `assists`        | integer   | star's assists in this game                |
#'   | `points`         | integer   | star's points in this game                 |
#'   | `saves`          | integer   | saves (goalies)                            |
#'   | `shots_against`  | integer   | shots against (goalies)                    |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column          | type      | description                                |
#'   |-----------------|-----------|--------------------------------------------|
#'   | `game_id`       | integer   | PWHL game id                               |
#'   | `role`          | character | grouped role (Referee/Linesperson/etc)     |
#'   | `first_name`    | character | official's first name                      |
#'   | `last_name`     | character | official's last name                       |
#'   | `jersey_number` | integer   | official's jersey number                   |
#'   | `official_role` | character | official's specific role                   |
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
#' @return A data frame of class `fastRhockey_data`. One row per period per
#'   game. Common columns include:
#'
#'   | column        | type      | description                                |
#'   |---------------|-----------|--------------------------------------------|
#'   | `game_id`     | integer   | PWHL game id                               |
#'   | `period_id`   | integer   | period id (1-3, 4 = OT, 5 = SO, etc.)      |
#'   | `period`      | character | period long name                           |
#'   | `home_goals`  | integer   | home goals in period                       |
#'   | `home_shots`  | integer   | home shots in period                       |
#'   | `away_goals`  | integer   | away goals in period                       |
#'   | `away_shots`  | integer   | away shots in period                       |
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
#' @return A data frame of class `fastRhockey_data`. One row per shootout
#'   attempt. Common columns include:
#'
#'   | column         | type      | description                                |
#'   |----------------|-----------|--------------------------------------------|
#'   | `game_id`      | integer   | PWHL game id                               |
#'   | `round`        | integer   | shootout round number                      |
#'   | `team_side`    | character | `"home"` or `"away"`                       |
#'   | `shooter_id`   | integer   | shooter player id                          |
#'   | `shooter_first`| character | shooter first name                         |
#'   | `shooter_last` | character | shooter last name                          |
#'   | `goalie_id`    | integer   | opposing goalie id                         |
#'   | `goalie_first` | character | goalie first name                          |
#'   | `goalie_last`  | character | goalie last name                           |
#'   | `is_goal`      | integer   | scored flag                                |
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
#' @return A data frame of class `fastRhockey_data`. Common columns include:
#'
#'   | column          | type      | description                                |
#'   |-----------------|-----------|--------------------------------------------|
#'   | `game_id`       | integer   | PWHL game id                               |
#'   | `team_id`       | integer   | team id                                    |
#'   | `team`          | character | team name                                  |
#'   | `team_abbr`     | character | three-letter abbreviation                  |
#'   | `team_side`     | character | `"home"` or `"away"`                       |
#'   | `player_type`   | character | `"skater"` or `"goalie"`                   |
#'   | `player_id`     | integer   | player id                                  |
#'   | `first_name`    | character | player first name                          |
#'   | `last_name`     | character | player last name                           |
#'   | `jersey_number` | integer   | jersey number                              |
#'   | `position`      | character | position code                              |
#'   | `birth_date`    | character | birth date                                 |
#'   | `starting`      | integer   | started the game (`0`/`1`)                 |
#'   | `status`        | character | status string (e.g. captain markers)       |
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
