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
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  dots <- rlang::dots_list(...)

  loader <- rds_from_url

  if (!is.null(dbConnection) && !is.null(tablename)) in_db <- TRUE else in_db <- FALSE

  if (isTRUE(seasons)) seasons <- 2024:most_recent_pwhl_season()

  stopifnot(is.numeric(seasons),
            seasons >= 2024,
            seasons <= most_recent_pwhl_season())

  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/pwhl_pbp/play_by_play_", seasons, ".rds"
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
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  dots <- rlang::dots_list(...)

  loader <- rds_from_url

  if (!is.null(dbConnection) && !is.null(tablename)) in_db <- TRUE else in_db <- FALSE

  if (isTRUE(seasons)) seasons <- 2024:most_recent_pwhl_season()

  stopifnot(is.numeric(seasons),
            seasons >= 2024,
            seasons <= most_recent_pwhl_season())

  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/pwhl_player_boxscores/player_box_", seasons, ".rds"
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
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  dots <- rlang::dots_list(...)

  loader <- rds_from_url

  if (!is.null(dbConnection) && !is.null(tablename)) in_db <- TRUE else in_db <- FALSE

  if (isTRUE(seasons)) seasons <- 2024:most_recent_pwhl_season()

  stopifnot(is.numeric(seasons),
            seasons >= 2024,
            seasons <= most_recent_pwhl_season())

  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/pwhl_schedules/pwhl_schedule_", seasons, ".rds"
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
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))
  dots <- rlang::dots_list(...)

  loader <- rds_from_url

  if (!is.null(dbConnection) && !is.null(tablename)) in_db <- TRUE else in_db <- FALSE

  if (isTRUE(seasons)) seasons <- 2024:most_recent_pwhl_season()

  stopifnot(is.numeric(seasons),
            seasons >= 2024,
            seasons <= most_recent_pwhl_season())

  urls <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/pwhl_rosters/rosters_", seasons, ".rds"
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
