# update_nhl_db() / update_pwhl_db() against an in-memory SQLite database, with the
# release loaders mocked: the first call builds the table from scratch, the
# second finds nothing missing, a forced rebuild of one season purges and reloads
# it, and a bad force_rebuild is rejected.

fake_games <- function(seasons) {
  data.frame(season = rep(seasons, each = 2),
             game_id = as.integer(paste0(rep(seasons, each = 2), c(1001, 1002))))
}

fake_pbp_loader <- function(games) {
  function(seasons, ..., dbConnection = NULL, tablename = NULL) {
    pbp <- games[games$season %in% seasons, c("season", "game_id")]
    pbp$event <- "Shot"
    DBI::dbWriteTable(dbConnection, tablename, pbp, append = TRUE)
    invisible(NULL)
  }
}

# rule_header()/rule_footer() print with cat(); keep the test log clean.
quietly <- function(x) invisible(utils::capture.output(suppressMessages(x)))

expect_db_update <- function(update, loader_games, loader_pbp, seasons) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  games <- fake_games(seasons)
  mocks <- list(function() games, fake_pbp_loader(games))
  names(mocks) <- c(loader_games, loader_pbp)
  testthat::local_mocked_bindings(!!!mocks)

  quietly(update(db_connection = con, tblname = "pbp"))
  expect_setequal(DBI::dbGetQuery(con, "SELECT DISTINCT game_id FROM pbp")$game_id, games$game_id)

  # Nothing missing: a second run leaves the table as it is.
  quietly(update(db_connection = con, tblname = "pbp"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM pbp")$n, nrow(games))

  # A forced rebuild of one season purges and reloads only that season.
  quietly(update(db_connection = con, tblname = "pbp", force_rebuild = seasons[1]))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM pbp")$n, nrow(games))

  # A full rebuild drops and reloads the table.
  quietly(update(db_connection = con, tblname = "pbp", force_rebuild = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM pbp")$n, nrow(games))

  expect_error(quietly(update(db_connection = con, force_rebuild = "x")), "force_rebuild")
  expect_error(quietly(update(db_connection = con, force_rebuild = "NEW")), "internal")
}

test_that("update_nhl_db() builds, tops up and rebuilds a database", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  expect_db_update(update_nhl_db, "load_nhl_games", "load_nhl_pbp", c(2023, 2024))
})

test_that("update_pwhl_db() builds, tops up and rebuilds a database", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  expect_db_update(update_pwhl_db, "load_pwhl_games", "load_pwhl_pbp", c(2024, 2025))
})
