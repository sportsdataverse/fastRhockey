#' @name nhl_loaders
#' @aliases nhl_loaders load_nhl
#' @title **NHL Data Loaders Overview**
#' @description
#' Loaders for season-level NHL datasets published as GitHub releases on
#' `sportsdataverse/sportsdataverse-data`. Each helper is a thin wrapper
#' around [.nhl_release_loader()] which validates the requested seasons,
#' builds the per-asset URLs from a `(release_tag, file_prefix)` pair,
#' downloads in parallel (with optional `progressr` progress + optional
#' `DBI::DBIConnection` insertion), and tags the result with the
#' `fastRhockey_data` S3 class. Adding a new dataset is one new row in
#' the catalog table below.
#'
#' @details
#'
#' ## **Loader catalog**
#'
#' | Function | Release tag | File prefix |
#' |---|---|---|
#' | [load_nhl_pbp()]             | `nhl_pbp_full`           | `play_by_play`       |
#' | [load_nhl_pbp_lite()]        | `nhl_pbp_lite`           | `play_by_play_lite`  |
#' | [load_nhl_player_box()]      | `nhl_player_boxscores`   | `player_box`         |
#' | [load_nhl_skater_box()]      | `nhl_skater_boxscores`   | `skater_box`         |
#' | [load_nhl_goalie_box()]      | `nhl_goalie_boxscores`   | `goalie_box`         |
#' | [load_nhl_team_box()]        | `nhl_team_boxscores`     | `team_box`           |
#' | [load_nhl_schedule()]        | `nhl_schedules`          | `nhl_schedule`       |
#' | [load_nhl_rosters()]         | `nhl_rosters`            | `rosters`            |
#' | [load_nhl_game_rosters()]    | `nhl_game_rosters`       | `game_rosters`       |
#' | [load_nhl_game_info()]       | `nhl_game_info`          | `game_info`          |
#' | [load_nhl_scoring()]         | `nhl_scoring`            | `scoring`            |
#' | [load_nhl_penalties()]       | `nhl_penalties`          | `penalties`          |
#' | [load_nhl_three_stars()]     | `nhl_three_stars`        | `three_stars`        |
#' | [load_nhl_scratches()]       | `nhl_scratches`          | `scratches`          |
#' | [load_nhl_linescore()]       | `nhl_linescore`          | `linescore`          |
#' | [load_nhl_shifts()]          | `nhl_shifts`             | `shifts`             |
#' | [load_nhl_officials()]       | `nhl_officials`          | `officials`          |
#' | [load_nhl_shots_by_period()] | `nhl_shots_by_period`    | `shots_by_period`    |
#' | [load_nhl_shootout()]        | `nhl_shootout`           | `shootout_summary`   |
#'
#' ## **DB helpers**
#'
#' | Function | Purpose |
#' |---|---|
#' | [update_nhl_db()]         | Idempotent loader → DB writer (delta only) |
#' | [build_nhl_db()]          | Bulk-build a DB from release files |
#' | [get_missing_nhl_games()] | Show games missing from a DB target |
#'
#' @keywords NHL Loaders
#' @family NHL Loaders
NULL


## ─── NHL release-tag → loader catalog ─────────────────────────────────────
##
## Each row defines one season-level dataset published to a GitHub release on
## sportsdataverse/sportsdataverse-data. The exported `load_nhl_*` helpers
## below are thin wrappers around `.nhl_release_loader()` which handles
## season validation, URL construction, parallel download with progress, DB
## insertion, and S3 class tagging. Adding a new dataset = adding a row.
##
## | exported helper              | release tag              | file prefix          |
## |------------------------------|--------------------------|----------------------|
## | `load_nhl_pbp()`             | `nhl_pbp_full`           | `play_by_play`       |
## | `load_nhl_pbp_lite()`        | `nhl_pbp_lite`           | `play_by_play_lite`  |
## | `load_nhl_player_box()`      | `nhl_player_boxscores`   | `player_box`         |
## | `load_nhl_skater_box()`      | `nhl_skater_boxscores`   | `skater_box`         |
## | `load_nhl_goalie_box()`      | `nhl_goalie_boxscores`   | `goalie_box`         |
## | `load_nhl_team_box()`        | `nhl_team_boxscores`     | `team_box`           |
## | `load_nhl_schedule()`        | `nhl_schedules`          | `nhl_schedule`       |
## | `load_nhl_rosters()`         | `nhl_rosters`            | `rosters`            |
## | `load_nhl_game_rosters()`    | `nhl_game_rosters`       | `game_rosters`       |
## | `load_nhl_game_info()`       | `nhl_game_info`          | `game_info`          |
## | `load_nhl_scoring()`         | `nhl_scoring`            | `scoring`            |
## | `load_nhl_penalties()`       | `nhl_penalties`          | `penalties`          |
## | `load_nhl_three_stars()`     | `nhl_three_stars`        | `three_stars`        |
## | `load_nhl_scratches()`       | `nhl_scratches`          | `scratches`          |
## | `load_nhl_linescore()`       | `nhl_linescore`          | `linescore`          |
## | `load_nhl_shifts()`          | `nhl_shifts`             | `shifts`             |
## | `load_nhl_officials()`       | `nhl_officials`          | `officials`          |
## | `load_nhl_shots_by_period()` | `nhl_shots_by_period`    | `shots_by_period`    |
## | `load_nhl_shootout()`        | `nhl_shootout`           | `shootout_summary`   |

# Internal worker shared by every load_nhl_*() helper.
# Validates seasons, builds release URLs, downloads in parallel with optional
# progressr support, optionally writes into a DB, and tags the result with
# the `fastRhockey_data` S3 class.
.nhl_release_loader <- function(seasons, release_tag, file_prefix,
                                min_season = 2011,
                                dbConnection = NULL, tablename = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  loader <- rds_from_url
  in_db <- !is.null(dbConnection) && !is.null(tablename)

  if (isTRUE(seasons)) seasons <- min_season:most_recent_nhl_season()

  stopifnot(is.numeric(seasons),
            seasons >= min_season,
            seasons <= most_recent_nhl_season())

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


#' **Load fastRhockey NHL play-by-play**
#' @name load_nhl_pbp
NULL
#' @title
#' **Load cleaned NHL play-by-play from the data repo**
#' @rdname load_nhl_pbp
#' @description Helper that loads multiple seasons of pre-scraped NHL
#'   play-by-play data (full version, including line changes and shifts) from
#'   the [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function that writes
#' the season data into a database (used by `update_nhl_db()`).
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the play by play data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                  |types     |description                                        |
#'    |:-------------------------|:---------|:--------------------------------------------------|
#'    |event_type                |character |Standardized event type code.                      |
#'    |event                     |character |Event description label.                           |
#'    |secondary_type            |character |Secondary event type (e.g. shot type).             |
#'    |event_team_abbr           |character |Abbreviation of the team credited with the event.  |
#'    |event_team_type           |character |Whether the event team is home or away.            |
#'    |description               |character |Full text description of the event.                |
#'    |period                    |integer   |Period number.                                     |
#'    |period_type               |character |Period type (REGULAR/OVERTIME/SHOOTOUT).           |
#'    |period_time               |character |Elapsed time in the period (MM:SS).                |
#'    |period_seconds            |integer   |Elapsed seconds in the period.                     |
#'    |period_seconds_remaining  |integer   |Seconds remaining in the period.                   |
#'    |period_time_remaining     |character |Time remaining in the period (MM:SS).              |
#'    |game_seconds              |integer   |Elapsed seconds in the game.                       |
#'    |game_seconds_remaining    |integer   |Seconds remaining in regulation.                   |
#'    |home_score                |integer   |Home team score after the event.                   |
#'    |away_score                |integer   |Away team score after the event.                   |
#'    |event_player_1_name       |character |Name of the primary event player.                  |
#'    |event_player_1_type       |character |Role of the primary event player.                  |
#'    |event_player_1_id         |integer   |Player id of the primary event player.             |
#'    |event_player_2_name       |character |Name of the secondary event player.                |
#'    |event_player_2_type       |character |Role of the secondary event player.                |
#'    |event_player_2_id         |integer   |Player id of the secondary event player.           |
#'    |event_player_3_name       |character |Name of the tertiary event player.                 |
#'    |event_player_3_type       |character |Role of the tertiary event player.                 |
#'    |event_player_3_id         |integer   |Player id of the tertiary event player.            |
#'    |event_goalie_name         |character |Name of the goalie on the event.                   |
#'    |event_goalie_id           |integer   |Player id of the goalie on the event.              |
#'    |penalty_severity          |character |Severity of the penalty.                           |
#'    |penalty_minutes           |integer   |Penalty minutes assessed.                          |
#'    |strength_state            |character |Strength state (e.g. 5v5, 5v4).                    |
#'    |strength_code             |character |Coded strength state.                              |
#'    |strength                  |character |Strength description.                              |
#'    |empty_net                 |logical   |Whether the net was empty.                         |
#'    |extra_attacker            |logical   |Whether an extra attacker was on the ice.          |
#'    |x                         |integer   |Raw x-coordinate of the event.                     |
#'    |y                         |integer   |Raw y-coordinate of the event.                     |
#'    |x_fixed                   |integer   |Side-adjusted x-coordinate.                        |
#'    |y_fixed                   |integer   |Side-adjusted y-coordinate.                        |
#'    |shot_distance             |numeric   |Distance of the shot from the net.                 |
#'    |shot_angle                |numeric   |Angle of the shot relative to the net.             |
#'    |home_skaters              |integer   |Number of home skaters on the ice.                 |
#'    |away_skaters              |integer   |Number of away skaters on the ice.                 |
#'    |home_on_1                 |character |Name of home skater 1 on the ice.                  |
#'    |home_on_2                 |character |Name of home skater 2 on the ice.                  |
#'    |home_on_3                 |character |Name of home skater 3 on the ice.                  |
#'    |home_on_4                 |character |Name of home skater 4 on the ice.                  |
#'    |home_on_5                 |character |Name of home skater 5 on the ice.                  |
#'    |home_on_6                 |character |Name of home skater 6 on the ice.                  |
#'    |home_on_7                 |character |Name of home skater 7 on the ice.                  |
#'    |away_on_1                 |character |Name of away skater 1 on the ice.                  |
#'    |away_on_2                 |character |Name of away skater 2 on the ice.                  |
#'    |away_on_3                 |character |Name of away skater 3 on the ice.                  |
#'    |away_on_4                 |character |Name of away skater 4 on the ice.                  |
#'    |away_on_5                 |character |Name of away skater 5 on the ice.                  |
#'    |away_on_6                 |character |Name of away skater 6 on the ice.                  |
#'    |away_on_7                 |character |Name of away skater 7 on the ice.                  |
#'    |home_goalie               |character |Name of the home goalie on the ice.                |
#'    |away_goalie               |character |Name of the away goalie on the ice.                |
#'    |num_on                    |integer   |Number of players coming on (line change).         |
#'    |players_on                |character |Names of players coming on.                         |
#'    |num_off                   |integer   |Number of players going off (line change).         |
#'    |players_off               |character |Names of players going off.                        |
#'    |game_id                   |integer   |Unique game identifier.                            |
#'    |season                    |integer   |Season (concluding year, YYYY).                    |
#'    |season_type              |character |Season type (regular/playoffs).                    |
#'    |home_abbr                 |character |Home team abbreviation.                            |
#'    |away_abbr                 |character |Away team abbreviation.                            |
#'    |event_idx                 |integer   |Sequential event index within the game.            |
#'    |event_id                  |integer   |NHL event identifier.                              |
#'    |away_goalie_in            |integer   |Whether the away goalie is in net (1/0).           |
#'    |home_goalie_in            |integer   |Whether the home goalie is in net (1/0).           |
#'    |reason                    |character |Reason for the event (e.g. stoppage reason).       |
#'    |secondaryReason           |character |Secondary reason for the event.                    |
#'    |ids_on                    |character |Player ids coming on.                              |
#'    |ids_off                   |character |Player ids going off.                              |
#'    |home_on_1_id              |integer   |Player id of home skater 1 on the ice.             |
#'    |away_on_1_id              |integer   |Player id of away skater 1 on the ice.             |
#'    |home_on_2_id              |integer   |Player id of home skater 2 on the ice.             |
#'    |away_on_2_id              |integer   |Player id of away skater 2 on the ice.             |
#'    |home_on_3_id              |integer   |Player id of home skater 3 on the ice.             |
#'    |away_on_3_id              |integer   |Player id of away skater 3 on the ice.             |
#'    |home_on_4_id              |integer   |Player id of home skater 4 on the ice.             |
#'    |away_on_4_id              |integer   |Player id of away skater 4 on the ice.             |
#'    |home_on_5_id              |integer   |Player id of home skater 5 on the ice.             |
#'    |away_on_5_id              |integer   |Player id of away skater 5 on the ice.             |
#'    |home_on_6_id              |integer   |Player id of home skater 6 on the ice.             |
#'    |away_on_6_id              |integer   |Player id of away skater 6 on the ice.             |
#'    |home_on_7_id              |integer   |Player id of home skater 7 on the ice.             |
#'    |away_on_7_id              |integer   |Player id of away skater 7 on the ice.             |
#'    |home_goalie_id            |integer   |Player id of the home goalie on the ice.           |
#'    |away_goalie_id            |integer   |Player id of the away goalie on the ice.           |
#'    |xg                        |numeric   |Expected goals value for the shot event.           |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_pbp(2022))
#' }
load_nhl_pbp <- function(seasons = most_recent_nhl_season(), ...,
                         dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_pbp_full", file_prefix = "play_by_play",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}

#' **Load fastRhockey NHL play-by-play (lite)**
#' @name load_nhl_pbp_lite
NULL
#' @title **Load cleaned NHL play-by-play (lite) from the data repo**
#' @description Same as `load_nhl_pbp()` but without line change (CHANGE) events,
#' resulting in smaller file sizes.
#' @rdname load_nhl_pbp_lite
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with the same columns as
#'   [load_nhl_pbp()] but excluding line-change (`CHANGE`) events:
#'
#'    |col_name                  |types     |description                                        |
#'    |:-------------------------|:---------|:--------------------------------------------------|
#'    |event_type                |character |Standardized event type code.                      |
#'    |event                     |character |Event description label.                           |
#'    |secondary_type            |character |Secondary event type (e.g. shot type).             |
#'    |event_team_abbr           |character |Abbreviation of the team credited with the event.  |
#'    |event_team_type           |character |Whether the event team is home or away.            |
#'    |description               |character |Full text description of the event.                |
#'    |period                    |integer   |Period number.                                     |
#'    |period_type               |character |Period type (REGULAR/OVERTIME/SHOOTOUT).           |
#'    |period_time               |character |Elapsed time in the period (MM:SS).                |
#'    |period_seconds            |integer   |Elapsed seconds in the period.                     |
#'    |game_seconds              |integer   |Elapsed seconds in the game.                       |
#'    |home_score                |integer   |Home team score after the event.                   |
#'    |away_score                |integer   |Away team score after the event.                   |
#'    |event_player_1_name       |character |Name of the primary event player.                  |
#'    |event_player_1_id         |integer   |Player id of the primary event player.             |
#'    |event_player_2_name       |character |Name of the secondary event player.                |
#'    |event_player_2_id         |integer   |Player id of the secondary event player.           |
#'    |event_goalie_name         |character |Name of the goalie on the event.                   |
#'    |event_goalie_id           |integer   |Player id of the goalie on the event.              |
#'    |strength_state            |character |Strength state (e.g. 5v5, 5v4).                    |
#'    |x                         |integer   |Raw x-coordinate of the event.                     |
#'    |y                         |integer   |Raw y-coordinate of the event.                     |
#'    |shot_distance             |numeric   |Distance of the shot from the net.                 |
#'    |shot_angle                |numeric   |Angle of the shot relative to the net.             |
#'    |game_id                   |integer   |Unique game identifier.                            |
#'    |season                    |integer   |Season (concluding year, YYYY).                    |
#'    |xg                        |numeric   |Expected goals value for the shot event.           |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_pbp_lite(2026))
#' }
load_nhl_pbp_lite <- function(seasons = most_recent_nhl_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_pbp_lite", file_prefix = "play_by_play_lite",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL team box scores**
#' @name load_nhl_team_box
NULL
#' @title
#' **Load cleaned NHL team box scores from the data repo**
#' @rdname load_nhl_team_box
#' @description Helper that loads multiple seasons of pre-scraped NHL team box
#'   scores from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the team box data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name         |types     |description                              |
#'    |:----------------|:---------|:----------------------------------------|
#'    |home_away        |character |Home or away indicator.                  |
#'    |team_id          |integer   |Unique team identifier.                  |
#'    |team_abbrev      |character |Team abbreviation/code.                  |
#'    |team_name        |character |Team name.                               |
#'    |goals            |integer   |Goals scored.                            |
#'    |shots_on_goal    |integer   |Shots on goal.                           |
#'    |pim              |integer   |Penalty minutes.                         |
#'    |hits             |integer   |Hits.                                    |
#'    |blocked_shots    |integer   |Blocked shots.                           |
#'    |giveaways        |integer   |Giveaways.                               |
#'    |takeaways        |integer   |Takeaways.                               |
#'    |power_play_goals |integer   |Power play goals.                        |
#'    |faceoff_win_pctg |numeric   |Faceoff win percentage.                  |
#'    |saves            |integer   |Saves made.                              |
#'    |save_pctg        |numeric   |Save percentage.                         |
#'    |goals_against    |integer   |Goals against.                           |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_team_box(2022))
#' }
load_nhl_team_box <- function(seasons = most_recent_nhl_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_team_boxscores", file_prefix = "team_box",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL player box scores**
#' @name load_nhl_player_box
NULL
#' @title
#' **Load cleaned NHL player box scores from the data repo**
#' @rdname load_nhl_player_box
#' @description Helper that loads multiple seasons of pre-scraped NHL
#'   player box scores (combined skaters + goalies) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases either into memory or writes it into a database.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the player box data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns
#'   (combined skaters + goalies; goalie-only fields are `NA` for skaters):
#'
#'    |col_name                     |types     |description                                        |
#'    |:----------------------------|:---------|:--------------------------------------------------|
#'    |home_away                    |character |Home or away indicator.                            |
#'    |team_id                      |integer   |Unique team identifier.                            |
#'    |team_abbrev                  |character |Team abbreviation/code.                            |
#'    |player_id                    |integer   |Unique player identifier.                          |
#'    |player_name                  |character |Player name.                                       |
#'    |sweater_number               |integer   |Jersey number.                                     |
#'    |position                     |character |Player position.                                   |
#'    |goals                        |integer   |Goals scored.                                      |
#'    |assists                      |integer   |Assists.                                           |
#'    |points                       |integer   |Total points (goals + assists).                    |
#'    |plus_minus                   |integer   |Plus/minus rating.                                 |
#'    |pim                          |integer   |Penalty minutes.                                   |
#'    |hits                         |integer   |Hits.                                              |
#'    |power_play_goals             |integer   |Power play goals.                                  |
#'    |shots_on_goal                |integer   |Shots on goal.                                     |
#'    |faceoff_winning_pctg         |numeric   |Faceoff win percentage.                            |
#'    |toi                          |character |Time on ice.                                       |
#'    |blocked_shots                |integer   |Blocked shots.                                     |
#'    |shifts                       |integer   |Number of shifts.                                  |
#'    |giveaways                    |integer   |Giveaways.                                         |
#'    |takeaways                    |integer   |Takeaways.                                         |
#'    |even_strength_shots_against  |character |Even-strength shots against (goalies).             |
#'    |power_play_shots_against     |character |Power play shots against (goalies).                |
#'    |shorthanded_shots_against    |character |Shorthanded shots against (goalies).               |
#'    |save_shots_against           |character |Saves / shots against (goalies).                   |
#'    |save_pctg                    |numeric   |Save percentage (goalies).                         |
#'    |even_strength_goals_against  |integer   |Even-strength goals against (goalies).             |
#'    |power_play_goals_against     |integer   |Power play goals against (goalies).                |
#'    |shorthanded_goals_against    |integer   |Shorthanded goals against (goalies).               |
#'    |goals_against                |integer   |Goals against (goalies).                           |
#'    |starter                      |logical   |Whether the goalie started the game.               |
#'    |decision                     |character |Goalie decision (W/L/O).                           |
#'    |shots_against                |integer   |Shots faced (goalies).                             |
#'    |saves                        |integer   |Saves made (goalies).                              |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_player_box(2022))
#' }
load_nhl_player_box <- function(seasons = most_recent_nhl_season(), ...,
                                dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_player_boxscores", file_prefix = "player_box",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL skater box scores**
#' @name load_nhl_skater_box
NULL
#' @title
#' **Load cleaned NHL skater box scores from the data repo**
#' @rdname load_nhl_skater_box
#' @description Helper that loads multiple seasons of pre-scraped NHL skater
#'   box scores from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name             |types     |description                       |
#'    |:--------------------|:---------|:---------------------------------|
#'    |home_away            |character |Home or away indicator.           |
#'    |team_id              |integer   |Unique team identifier.           |
#'    |team_abbrev          |character |Team abbreviation/code.           |
#'    |player_id            |integer   |Unique player identifier.         |
#'    |player_name          |character |Player name.                      |
#'    |sweater_number       |integer   |Jersey number.                    |
#'    |position             |character |Player position.                  |
#'    |goals                |integer   |Goals scored.                     |
#'    |assists              |integer   |Assists.                          |
#'    |points               |integer   |Total points (goals + assists).   |
#'    |plus_minus           |integer   |Plus/minus rating.                |
#'    |pim                  |integer   |Penalty minutes.                  |
#'    |hits                 |integer   |Hits.                             |
#'    |power_play_goals     |integer   |Power play goals.                 |
#'    |shots_on_goal        |integer   |Shots on goal.                    |
#'    |faceoff_winning_pctg |numeric   |Faceoff win percentage.           |
#'    |toi                  |character |Time on ice.                      |
#'    |blocked_shots        |integer   |Blocked shots.                    |
#'    |shifts               |integer   |Number of shifts.                 |
#'    |giveaways            |integer   |Giveaways.                        |
#'    |takeaways            |integer   |Takeaways.                        |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_skater_box(2022))
#' }
load_nhl_skater_box <- function(seasons = most_recent_nhl_season(), ...,
                                dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_skater_boxscores", file_prefix = "skater_box",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL goalie box scores**
#' @name load_nhl_goalie_box
NULL
#' @title
#' **Load cleaned NHL goalie box scores from the data repo**
#' @rdname load_nhl_goalie_box
#' @description Helper that loads multiple seasons of pre-scraped NHL goalie
#'   box scores from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name                    |types     |description                            |
#'    |:---------------------------|:---------|:--------------------------------------|
#'    |home_away                   |character |Home or away indicator.                |
#'    |team_id                     |integer   |Unique team identifier.                |
#'    |team_abbrev                 |character |Team abbreviation/code.                |
#'    |player_id                   |integer   |Unique player identifier.              |
#'    |player_name                 |character |Player name.                           |
#'    |sweater_number              |integer   |Jersey number.                         |
#'    |even_strength_shots_against |character |Even-strength shots against.           |
#'    |power_play_shots_against    |character |Power play shots against.              |
#'    |shorthanded_shots_against   |character |Shorthanded shots against.             |
#'    |save_shots_against          |character |Saves / shots against.                 |
#'    |save_pctg                   |numeric   |Save percentage.                       |
#'    |even_strength_goals_against |integer   |Even-strength goals against.           |
#'    |power_play_goals_against    |integer   |Power play goals against.              |
#'    |shorthanded_goals_against   |integer   |Shorthanded goals against.             |
#'    |pim                         |integer   |Penalty minutes.                       |
#'    |goals_against               |integer   |Goals against.                         |
#'    |toi                         |character |Time on ice.                           |
#'    |starter                     |logical   |Whether the goalie started the game.   |
#'    |decision                    |character |Goalie decision (W/L/O).               |
#'    |shots_against               |integer   |Shots faced.                           |
#'    |saves                       |integer   |Saves made.                            |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_goalie_box(2022))
#' }
load_nhl_goalie_box <- function(seasons = most_recent_nhl_season(), ...,
                                dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_goalie_boxscores", file_prefix = "goalie_box",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL schedules**
#' @name load_nhl_schedule
NULL
#' @title
#' **Load cleaned NHL schedules from the data repo**
#' @rdname load_nhl_schedule
#' @description Helper that loads multiple seasons of pre-scraped NHL
#'   schedule data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases. The schedule includes data-availability flags for each game.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the schedule data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name        |types     |description                                       |
#'    |:---------------|:---------|:-------------------------------------------------|
#'    |game_id         |integer   |Unique game identifier.                           |
#'    |season_full     |character |Full season label (e.g. 20212022).                |
#'    |game_type       |character |Game type the row belongs to (regular/playoffs).  |
#'    |game_date       |character |Game date.                                        |
#'    |game_time       |character |Scheduled start time of the game.                 |
#'    |home_team_abbr  |character |Home team abbreviation.                           |
#'    |away_team_abbr  |character |Away team abbreviation.                           |
#'    |home_team_name  |character |Home team name.                                   |
#'    |away_team_name  |character |Away team name.                                   |
#'    |home_score      |integer   |Home team final score.                            |
#'    |away_score      |integer   |Away team final score.                            |
#'    |game_state      |character |Game status (e.g. FINAL/FUT).                     |
#'    |venue           |character |Venue where the game was played.                  |
#'    |season          |integer   |Season (concluding year, YYYY).                   |
#'    |game_json       |logical   |Whether processed game JSON is available.         |
#'    |game_json_url   |glue      |URL to the processed game JSON.                   |
#'    |PBP             |logical   |Whether play-by-play data is available.           |
#'    |team_box        |logical   |Whether team box score data is available.         |
#'    |player_box      |logical   |Whether player box score data is available.       |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_schedule(2022))
#' }
load_nhl_schedule <- function(seasons = most_recent_nhl_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_schedules", file_prefix = "nhl_schedule",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL team rosters**
#' @name load_nhl_rosters
NULL
#' @title
#' **Load cleaned NHL team rosters from the data repo**
#' @rdname load_nhl_rosters
#' @description Helper that loads multiple seasons of pre-scraped NHL
#'   roster data (unique players per season, de-duplicated from per-game
#'   rosters) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the team rosters data table within the database
#' @return A data frame (`fastRhockey_data`) with the following columns:
#'
#'    |col_name       |types     |description                       |
#'    |:--------------|:---------|:---------------------------------|
#'    |player_id      |integer   |Unique player identifier.         |
#'    |full_name      |character |Player full name.                 |
#'    |first_name     |character |Player first name.                |
#'    |last_name      |character |Player last name.                 |
#'    |team_abbr      |character |Team abbreviation/code.           |
#'    |team_id        |integer   |Unique team identifier.           |
#'    |position_code  |character |Player position code.             |
#'    |sweater_number |integer   |Jersey number.                    |
#'    |season         |integer   |Season (concluding year, YYYY).   |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_rosters(2022))
#' }
load_nhl_rosters <- function(seasons = most_recent_nhl_season(), ...,
                             dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_rosters", file_prefix = "rosters",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL per-game rosters**
#' @name load_nhl_game_rosters
NULL
#' @title
#' **Load cleaned NHL per-game rosters from the data repo**
#' @rdname load_nhl_game_rosters
#' @description Helper that loads multiple seasons of NHL per-game roster data
#'   (one row per player per game) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per player per game
#'   and the following columns:
#'
#'    |col_name       |types     |description                  |
#'    |:--------------|:---------|:----------------------------|
#'    |player_id      |integer   |Unique player identifier.    |
#'    |full_name      |character |Player full name.            |
#'    |first_name     |character |Player first name.           |
#'    |last_name      |character |Player last name.            |
#'    |team_abbr      |character |Team abbreviation/code.      |
#'    |team_id        |integer   |Unique team identifier.      |
#'    |position_code  |character |Player position code.        |
#'    |sweater_number |integer   |Jersey number.               |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_game_rosters(2026))
#' }
load_nhl_game_rosters <- function(seasons = most_recent_nhl_season(), ...,
                                  dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_game_rosters", file_prefix = "game_rosters",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL game info**
#' @name load_nhl_game_info
NULL
#' @title
#' **Load cleaned NHL game info from the data repo**
#' @rdname load_nhl_game_info
#' @description Helper that loads multiple seasons of NHL game-level metadata
#'   (venue, attendance, officials, etc.) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per game and the
#'   following columns:
#'
#'    |col_name       |types     |description                                       |
#'    |:--------------|:---------|:-------------------------------------------------|
#'    |game_id        |integer   |Unique game identifier.                           |
#'    |season         |integer   |Season (concluding year, YYYY).                   |
#'    |game_type      |character |Game type the row belongs to (regular/playoffs).  |
#'    |game_date      |character |Game date.                                        |
#'    |venue          |character |Venue where the game was played.                  |
#'    |home_team_abbr |character |Home team abbreviation.                           |
#'    |away_team_abbr |character |Away team abbreviation.                           |
#'    |home_score     |integer   |Home team final score.                            |
#'    |away_score     |integer   |Away team final score.                            |
#'    |game_state     |character |Game status (e.g. FINAL/FUT).                     |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_game_info(2026))
#' }
load_nhl_game_info <- function(seasons = most_recent_nhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_game_info", file_prefix = "game_info",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL scoring summary**
#' @name load_nhl_scoring
NULL
#' @title
#' **Load cleaned NHL scoring summary from the data repo**
#' @rdname load_nhl_scoring
#' @description Helper that loads multiple seasons of NHL goal-scoring event
#'   data from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per goal and the
#'   following columns:
#'
#'    |col_name                |types     |description                                       |
#'    |:-----------------------|:---------|:-------------------------------------------------|
#'    |situationCode           |character |Strength/situation code for the goal.             |
#'    |eventId                 |integer   |NHL event identifier.                             |
#'    |strength                |character |Strength state at which the goal was scored.      |
#'    |playerId                |integer   |Player id of the goal scorer.                     |
#'    |firstName               |list      |Scorer first name (localized list).               |
#'    |lastName                |list      |Scorer last name (localized list).                |
#'    |name                    |list      |Scorer display name (localized list).             |
#'    |teamAbbrev              |list      |Scoring team abbreviation (localized list).       |
#'    |headshot                |character |URL to the scorer headshot image.                 |
#'    |highlightClipSharingUrl |character |Shareable URL for the goal highlight clip.        |
#'    |highlightClip           |numeric   |Highlight clip identifier.                        |
#'    |discreteClip            |numeric   |Discrete clip identifier.                         |
#'    |goalsToDate             |integer   |Scorer goal total to date in the season.          |
#'    |awayScore               |integer   |Away team score after the goal.                   |
#'    |homeScore               |integer   |Home team score after the goal.                   |
#'    |leadingTeamAbbrev       |list      |Abbreviation of the leading team (localized list).|
#'    |timeInPeriod            |character |Time within the period the goal was scored.       |
#'    |shotType                |character |Type of shot on the goal.                         |
#'    |goalModifier            |character |Goal modifier (e.g. empty-net, power-play).       |
#'    |assists                 |list      |List of assisting players on the goal.            |
#'    |pptReplayUrl            |character |URL to the power-play replay, if any.             |
#'    |homeTeamDefendingSide   |character |Side of the ice the home team defended.           |
#'    |isHome                  |logical   |Whether the scoring team is the home team.        |
#'    |period_number           |integer   |Period number the goal was scored in.             |
#'    |period_type             |character |Period type (REG/OT/SO).                          |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_scoring(2026))
#' }
load_nhl_scoring <- function(seasons = most_recent_nhl_season(), ...,
                             dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_scoring", file_prefix = "scoring",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL penalty summary**
#' @name load_nhl_penalties
NULL
#' @title
#' **Load cleaned NHL penalty summary from the data repo**
#' @rdname load_nhl_penalties
#' @description Helper that loads multiple seasons of NHL penalty event data
#'   from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per penalty and the
#'   following columns:
#'
#'    |col_name          |types     |description                                       |
#'    |:-----------------|:---------|:-------------------------------------------------|
#'    |timeInPeriod      |character |Time within the period the penalty occurred.      |
#'    |type              |character |Penalty type (e.g. minor, major).                 |
#'    |duration          |integer   |Penalty duration in minutes.                      |
#'    |committedByPlayer |list      |Player who committed the penalty (localized list).|
#'    |teamAbbrev        |list      |Penalized team abbreviation (localized list).     |
#'    |drawnBy           |list      |Player who drew the penalty (localized list).     |
#'    |descKey           |character |Penalty description key.                          |
#'    |period_number     |integer   |Period number the penalty occurred in.            |
#'    |period_type       |character |Period type (REG/OT/SO).                          |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_penalties(2026))
#' }
load_nhl_penalties <- function(seasons = most_recent_nhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_penalties", file_prefix = "penalties",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL three stars / decisions**
#' @name load_nhl_three_stars
NULL
#' @title
#' **Load cleaned NHL three stars from the data repo**
#' @rdname load_nhl_three_stars
#' @description Helper that loads multiple seasons of NHL three-star selections
#'   and game decisions from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per star selection
#'   (plus the game winner/loser goalie decision) and the following columns:
#'
#'    |col_name            |types     |description                                       |
#'    |:-------------------|:---------|:-------------------------------------------------|
#'    |star                |integer   |Star ranking (1, 2, or 3).                        |
#'    |playerId            |integer   |Player id of the selected star.                   |
#'    |teamAbbrev          |character |Team abbreviation of the selected star.           |
#'    |headshot            |character |URL to the player headshot image.                 |
#'    |name                |list      |Player display name (localized list).             |
#'    |sweaterNo           |integer   |Jersey number.                                    |
#'    |position            |character |Player position.                                  |
#'    |goals               |integer   |Goals scored in the game (skaters).               |
#'    |assists             |integer   |Assists in the game (skaters).                    |
#'    |points              |integer   |Total points in the game (skaters).               |
#'    |goalsAgainstAverage |numeric   |Goals-against average (goalies).                  |
#'    |savePctg            |numeric   |Save percentage (goalies).                        |
#'    |game_id             |integer   |Unique game identifier.                           |
#'    |winner_id           |integer   |Player id of the winning goalie.                  |
#'    |winner_name         |character |Name of the winning goalie.                       |
#'    |loser_id            |integer   |Player id of the losing goalie.                   |
#'    |loser_name          |character |Name of the losing goalie.                        |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_three_stars(2026))
#' }
load_nhl_three_stars <- function(seasons = most_recent_nhl_season(), ...,
                                 dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_three_stars", file_prefix = "three_stars",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL scratches**
#' @name load_nhl_scratches
NULL
#' @title
#' **Load cleaned NHL scratches from the data repo**
#' @rdname load_nhl_scratches
#' @description Helper that loads multiple seasons of NHL healthy scratch data
#'   from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per scratched player
#'   per game and the following columns:
#'
#'    |col_name  |types     |description                |
#'    |:---------|:---------|:--------------------------|
#'    |id        |integer   |Unique player identifier.  |
#'    |firstName |character |Player first name.         |
#'    |lastName  |character |Player last name.          |
#'    |game_id   |integer   |Unique game identifier.    |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_scratches(2026))
#' }
load_nhl_scratches <- function(seasons = most_recent_nhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_scratches", file_prefix = "scratches",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL linescore**
#' @name load_nhl_linescore
NULL
#' @title
#' **Load cleaned NHL linescore data from the data repo**
#' @rdname load_nhl_linescore
#' @description Helper that loads multiple seasons of NHL linescore data (per-game
#'   home/away goals, shots, shootout flag) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per game and the
#'   following columns:
#'
#'    |col_name       |types     |description                          |
#'    |:--------------|:---------|:------------------------------------|
#'    |game_id        |integer   |Unique game identifier.              |
#'    |home_team_id   |integer   |Unique home team identifier.         |
#'    |home_team_abbr |character |Home team abbreviation.              |
#'    |home_goals     |integer   |Home team goals.                     |
#'    |home_shots     |integer   |Home team shots on goal.             |
#'    |away_team_id   |integer   |Unique away team identifier.         |
#'    |away_team_abbr |character |Away team abbreviation.              |
#'    |away_goals     |integer   |Away team goals.                     |
#'    |away_shots     |integer   |Away team shots on goal.             |
#'    |has_shootout   |logical   |Whether the game ended in a shootout.|
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_linescore(2026))
#' }
load_nhl_linescore <- function(seasons = most_recent_nhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_linescore", file_prefix = "linescore",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL shifts**
#' @name load_nhl_shifts
NULL
#' @title
#' **Load cleaned NHL shift data from the data repo**
#' @rdname load_nhl_shifts
#' @description Helper that loads multiple seasons of NHL shift-by-shift data
#'   from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per shift change and
#'   the following columns:
#'
#'    |col_name               |types     |description                                |
#'    |:----------------------|:---------|:------------------------------------------|
#'    |event_team             |character |Team associated with the shift change.     |
#'    |period                 |integer   |Period number.                             |
#'    |period_time            |character |Elapsed time in the period (MM:SS).        |
#'    |period_seconds         |integer   |Elapsed seconds in the period.             |
#'    |game_seconds           |integer   |Elapsed seconds in the game.               |
#'    |num_on                 |integer   |Number of players coming on.               |
#'    |players_on             |character |Names of players coming on.                |
#'    |ids_on                 |character |Player ids coming on.                      |
#'    |num_off                |integer   |Number of players going off.               |
#'    |players_off            |character |Names of players going off.                |
#'    |ids_off                |character |Player ids going off.                      |
#'    |event                  |character |Event description label.                   |
#'    |event_type             |character |Standardized event type code.              |
#'    |game_seconds_remaining |integer   |Seconds remaining in regulation.           |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_shifts(2026))
#' }
load_nhl_shifts <- function(seasons = most_recent_nhl_season(), ...,
                            dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_shifts", file_prefix = "shifts",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL officials**
#' @name load_nhl_officials
NULL
#' @title
#' **Load cleaned NHL officials from the data repo**
#' @rdname load_nhl_officials
#' @description Helper that loads multiple seasons of NHL on-ice officials
#'   (referees + linesmen) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases. One row per official per game.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per official per
#'   game and the following columns:
#'
#'    |col_name  |types     |description                          |
#'    |:---------|:---------|:------------------------------------|
#'    |game_id   |integer   |Unique game identifier.              |
#'    |season    |integer   |Season (concluding year, YYYY).      |
#'    |game_date |character |Game date.                           |
#'    |role      |character |Official role (referee or linesman). |
#'    |name      |character |Full name of the official.           |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_officials(2026))
#' }
load_nhl_officials <- function(seasons = most_recent_nhl_season(), ...,
                               dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_officials", file_prefix = "officials",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL shots by period**
#' @name load_nhl_shots_by_period
NULL
#' @title
#' **Load cleaned NHL shots-by-period from the data repo**
#' @rdname load_nhl_shots_by_period
#' @description Helper that loads multiple seasons of NHL per-period shot
#'   totals (one row per team per period per game) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per team per period
#'   per game and the following columns:
#'
#'    |col_name      |types     |description                                       |
#'    |:-------------|:---------|:-------------------------------------------------|
#'    |game_id       |integer   |Unique game identifier.                           |
#'    |season        |integer   |Season (concluding year, YYYY).                   |
#'    |game_date     |character |Game date.                                        |
#'    |home_away     |character |Home or away indicator.                           |
#'    |period_number |integer   |Period number (1-3 regulation, 4+ for OT/SO).     |
#'    |period_type   |character |Period type (REG/OT/SO).                          |
#'    |shots         |integer   |Shots on goal in the period.                      |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_shots_by_period(2026))
#' }
load_nhl_shots_by_period <- function(seasons = most_recent_nhl_season(), ...,
                                     dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_shots_by_period", file_prefix = "shots_by_period",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


#' **Load fastRhockey NHL shootout summary**
#' @name load_nhl_shootout
NULL
#' @title
#' **Load cleaned NHL shootout summary from the data repo**
#' @rdname load_nhl_shootout
#' @description Helper that loads multiple seasons of NHL shootout-attempt data
#'   (one row per shooter per shootout, with result + goalie) from the
#'   [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
#'   releases. Only games that ended in a shootout contribute rows.
#' @param seasons A vector of 4-digit years (the *end year* of the NHL
#'   season; e.g., 2026 for the 2025-26 season). Min: 2011.
#' @param ... Additional arguments passed to an underlying function.
#' @param dbConnection A `DBIConnection` object, as returned by [DBI::dbConnect()]
#' @param tablename The name of the data table within the database
#' @return A data frame (`fastRhockey_data`) with one row per shootout attempt
#'   (only games that ended in a shootout contribute rows) and the following
#'   columns:
#'
#'    |col_name    |types     |description                                       |
#'    |:-----------|:---------|:-------------------------------------------------|
#'    |game_id     |integer   |Unique game identifier.                           |
#'    |season      |integer   |Season (concluding year, YYYY).                   |
#'    |game_date   |character |Game date.                                        |
#'    |sequence    |integer   |Order of the attempt within the shootout.         |
#'    |team_abbrev |character |Abbreviation of the shooting team.                |
#'    |player_id   |integer   |Player id of the shooter.                         |
#'    |first_name  |character |Shooter first name.                               |
#'    |last_name   |character |Shooter last name.                                |
#'    |shot_type   |character |Type of shot taken (e.g. wrist, snap, backhand).  |
#'    |result      |character |Attempt result (goal/save/miss).                  |
#'    |game_winner |logical   |Whether this attempt was the decisive one.        |
#' @export
#' @examples
#' \donttest{
#'   try(load_nhl_shootout(2026))
#' }
load_nhl_shootout <- function(seasons = most_recent_nhl_season(), ...,
                              dbConnection = NULL, tablename = NULL) {
  .nhl_release_loader(seasons,
    release_tag = "nhl_shootout", file_prefix = "shootout_summary",
    min_season = 2011,
    dbConnection = dbConnection, tablename = tablename
  )
}


# load games file (from sportsdataverse-data release, falls back to repo)
load_nhl_games <- function() {
  release_url <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/nhl_schedules/nhl_games_in_data_repo.rds"
  )
  dat <- tryCatch(rds_from_url(release_url), error = function(e) NULL)
  if (is.null(dat)) {
    # Fall back to raw repo file if release not yet populated
    fallback <- paste0(
      "https://raw.githubusercontent.com/sportsdataverse/",
      "fastRhockey-nhl-data/main/nhl/nhl_games_in_data_repo.rds"
    )
    dat <- rds_from_url(fallback)
  }
  return(dat)
}

#' @name update_nhl_db
#' @aliases update_nhl_db nhl_db nhl nhl_pbp_db
#' @title
#' **Update or create a fastRhockey NHL play-by-play database**
#' @description update_nhl_db() updates or creates a database with `fastRhockey`
#' play by play data of all completed and available games since 2011.
#'
#' @details This function creates and updates a data table with the name `tblname`
#' within a SQLite database (other drivers via `db_connection`) located in
#' `dbdir` and named `dbname`.
#' The data table combines all play by play data for every available game back
#' to the 2010 season and adds the most recent completed games as soon as they
#' are available for `fastRhockey`.
#'
#' The argument `force_rebuild` is of hybrid type. It can rebuild the play
#' by play data table either for the whole fastRhockey era (with `force_rebuild = TRUE`)
#' or just for specified seasons (e.g. `force_rebuild = c(2019, 2020)`).
#' Please note the following behavior:
#' * `force_rebuild = TRUE`: The data table with the name `tblname`
#'   will be removed completely and rebuilt from scratch. This is helpful when
#'   new columns are added during the Off-Season.
#' * `force_rebuild = c(2019, 2020)`: The data table with the name `tblname`
#'   will be preserved and only rows from the 2019 and 2020 seasons will be
#'   deleted and re-added. This is intended to be used for ongoing seasons because
#'   ESPN's data provider can make changes to the underlying data during the week.
#'
#' The parameter `db_connection` is intended for advanced users who want
#' to use other DBI drivers, such as MariaDB, Postgres or odbc. Please note that
#' the arguments `dbdir` and `dbname` are dropped in case a `db_connection`
#' is provided but the argument `tblname` will still be used to write the
#' data table into the database.
#'
#' @param dbdir Directory in which the database is or shall be located
#' @param dbname File name of an existing or desired SQLite database within `dbdir`
#' @param tblname The name of the play by play data table within the database
#' @param force_rebuild Hybrid parameter (logical or numeric) to rebuild parts
#' of or the complete play by play data table within the database (please see details for further information)
#' @param db_connection A `DBIConnection` object, as returned by
#' [DBI::dbConnect()] (please see details for further information)
#' @return Logical TRUE/FALSE
#' @export
update_nhl_db <- function(dbdir = ".",
                          dbname = "fastRhockey_db",
                          tblname = "fastRhockey_nhl_pbp",
                          force_rebuild = FALSE,
                          db_connection = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  rule_header("Update fastRhockey Play-by-Play Database")

  if (!is_installed("DBI") | !is_installed("purrr") |
      (!is_installed("RSQLite") & is.null(db_connection))) {
    cli::cli_abort("{my_time()} | Packages {.val DBI}, {.val RSQLite} and {.val purrr} required for database communication. Please install them.")
  }

  if (any(force_rebuild == "NEW")) {
    cli::cli_abort("{my_time()} | The argument {.val force_rebuild = NEW} is only for internal usage!")
  }

  if (!(is.logical(force_rebuild) | is.numeric(force_rebuild))) {
    cli::cli_abort("{my_time()} | The argument {.val force_rebuild} has to be either logical or numeric!")
  }

  if (!dir.exists(dbdir) & is.null(db_connection)) {
    cli::cli_alert_danger("{my_time()} | Directory {.file {dbdir}} doesn't exist yet. Try creating...")
    dir.create(dbdir)
  }

  if (is.null(db_connection)) {
    connection <- DBI::dbConnect(RSQLite::SQLite(), glue::glue("{dbdir}/{dbname}"))
  } else {
    connection <- db_connection
  }

  # create db if it doesn't exist or user forces rebuild
  if (!DBI::dbExistsTable(connection, tblname)) {
    build_nhl_db(tblname, connection, rebuild = "NEW")
  } else if (DBI::dbExistsTable(connection, tblname) & all(force_rebuild != FALSE)) {
    build_nhl_db(tblname, connection, rebuild = force_rebuild)
  }

  # get completed games
  user_message("Checking for missing completed games...", "todo")
  completed_games <- load_nhl_games() %>%
    # completed games since 2010-11 (end year >= 2011)
    dplyr::filter(.data$season >= 2011) %>%
    dplyr::pull("game_id")

  # function below
  missing <- get_missing_nhl_games(completed_games, connection, tblname)

  # rebuild db if number of missing games is too large
  if(length(missing) > 100) {
    build_nhl_db(tblname, connection, show_message = FALSE, rebuild = as.numeric(unique(stringr::str_sub(missing, 1, 4))))
    missing <- get_missing_nhl_games(completed_games, connection, tblname)
  }

  message_completed("Database update completed", in_builder = TRUE)
  cli::cli_alert_info("{my_time()} | Path to your db: {.file {DBI::dbGetInfo(connection)$dbname}}")
  if (is.null(db_connection)) DBI::dbDisconnect(connection)
  rule_footer("DONE")
}

# this is a helper function to build fastRhockey database from Scratch
build_nhl_db <- function(tblname = "fastRhockey_nhl_pbp", db_conn, rebuild = FALSE, show_message = TRUE) {

  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  valid_seasons <- load_nhl_games() %>%
    dplyr::filter(.data$season >= 2011) %>%
    dplyr::group_by(.data$season) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  if (all(rebuild == TRUE)) {
    cli::cli_ul("{my_time()} | Purging the complete data table {.val {tblname}} in your connected database...")
    DBI::dbRemoveTable(db_conn, tblname)
    seasons <- valid_seasons %>% dplyr::pull("season")
    cli::cli_ul("{my_time()} | Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}...")
  } else if (is.numeric(rebuild) & all(rebuild %in% valid_seasons$season)) {
    string <- paste0(rebuild, collapse = ", ")
    if (show_message){cli::cli_ul("{my_time()} | Purging {string} season(s) from the data table {.val {tblname}} in your connected database...")}
    DBI::dbExecute(db_conn, glue::glue_sql("DELETE FROM {`tblname`} WHERE season IN ({vals*})", vals = rebuild, .con = db_conn))
    seasons <- valid_seasons %>% dplyr::filter(.data$season %in% rebuild) %>% dplyr::pull("season")
    cli::cli_ul("{my_time()} | Starting download of the {string} season(s)...")
  } else if (all(rebuild == "NEW")) {
    cli::cli_alert_info("{my_time()} | Can't find the data table {.val {tblname}} in your database. Will load the play by play data from scratch.")
    seasons <- valid_seasons %>% dplyr::pull("season")
    cli::cli_ul("{my_time()} | Starting download of {length(seasons)} seasons between {min(seasons)} and {max(seasons)}...")
  } else {
    seasons <- NULL
    cli::cli_alert_danger("{my_time()} | At least one invalid value passed to argument {.val force_rebuild}. Please try again with valid input.")
  }

  if (!is.null(seasons)) {
    # this function lives in R/utils.R
    load_nhl_pbp(seasons, dbConnection = db_conn, tablename = tblname)
  }
}

# this is a helper function to check a list of completed games
# against the games that exist in a database connection
get_missing_nhl_games <- function(completed_games, dbConnection, tablename) {
  db_ids <- dplyr::tbl(dbConnection, tablename) %>%
    dplyr::select("game_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull("game_id")

  need_scrape <- completed_games[!completed_games %in% db_ids]

  cli::cli_alert_info("{my_time()} | You have {length(db_ids)} games and are missing {length(need_scrape)}.")
  return(need_scrape)
}
