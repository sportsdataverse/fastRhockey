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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`
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
#' @return A data frame of class `fastRhockey_data`, with one row per
#'   official per game. Columns include:
#'
#'   | column | description |
#'   |--------|-------------|
#'   | `game_id` | NHL game id |
#'   | `season` | Season end year (e.g. 2026 for 2025-26) |
#'   | `game_date` | ISO date of the game |
#'   | `role` | Either `"referee"` or `"linesman"` |
#'   | `name` | Full name of the official |
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
#' @return A data frame of class `fastRhockey_data`, with one row per team
#'   per period per game. Columns include:
#'
#'   | column | description |
#'   |--------|-------------|
#'   | `game_id` | NHL game id |
#'   | `season` | Season end year (e.g. 2026 for 2025-26) |
#'   | `game_date` | ISO date of the game |
#'   | `home_away` | `"home"` or `"away"` |
#'   | `period_number` | 1-3 for regulation, 4+ for OT/SO |
#'   | `period_type` | `"REG"`, `"OT"`, or `"SO"` |
#'   | `shots` | Shots on goal in the period |
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
#' @return A data frame of class `fastRhockey_data`, with one row per
#'   shootout attempt. Only games that ended in a shootout contribute
#'   rows. Columns include:
#'
#'   | column | description |
#'   |--------|-------------|
#'   | `game_id` | NHL game id |
#'   | `season` | Season end year (e.g. 2026 for 2025-26) |
#'   | `game_date` | ISO date of the game |
#'   | `sequence` | Order of the attempt within the shootout (1, 2, ...) |
#'   | `team_abbrev` | Three-letter abbreviation of the shooting team |
#'   | `player_id` | NHL player id of the shooter |
#'   | `first_name` | Shooter first name |
#'   | `last_name` | Shooter last name |
#'   | `shot_type` | e.g. `"wrist"`, `"snap"`, `"backhand"` |
#'   | `result` | `"goal"`, `"save"`, or `"miss"` |
#'   | `game_winner` | `TRUE` for the decisive attempt, `FALSE` otherwise |
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
