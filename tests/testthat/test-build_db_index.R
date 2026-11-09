# build_*_db(rebuild = TRUE) reads the games index, drops the table and reloads
# only the seasons the index lists. A failed, empty or partial index read must
# abort before the drop and leave the existing table as it was.

local_pbp_db <- function(env = parent.frame()) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

expect_table_kept <- function(con, tbl) {
  expect_true(DBI::dbExistsTable(con, tbl))
  expect_equal(DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", tbl))$n, 3L)
}

test_that("build_phf_db(rebuild = TRUE) aborts on a partial games index", {
  con <- local_pbp_db()
  DBI::dbWriteTable(con, "pbp", data.frame(season = 2021:2023, game_id = 1:3))
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("season,game_id\n2021,1\n", destfile)
      0L
    },
    .package = "utils"
  )
  local_mocked_bindings(
    csv_from_url = function(...) {
      warning("Stopped early on line 3.")
      data.table::data.table(season = 2021L, game_id = 1L)
    },
    load_phf_pbp = function(...) stop("must not reload")
  )
  expect_error(
    suppressWarnings(build_phf_db("pbp", con, rebuild = TRUE)),
    "phf_games_in_data_repo.csv"
  )
  expect_table_kept(con, "pbp")
})

test_that("build_nhl_db(rebuild = TRUE) aborts on an unreadable games index", {
  con <- local_pbp_db()
  DBI::dbWriteTable(con, "pbp", data.frame(season = 2021:2023, game_id = 1:3))
  local_mocked_bindings(
    rds_from_url = function(url) data.table::data.table(),
    load_nhl_pbp = function(...) stop("must not reload")
  )
  expect_error(build_nhl_db("pbp", con, rebuild = TRUE), "nhl_games_in_data_repo.rds.*untouched")
  expect_table_kept(con, "pbp")
})

test_that("build_pwhl_db(rebuild = TRUE) aborts on an unreadable games index", {
  con <- local_pbp_db()
  DBI::dbWriteTable(con, "pbp", data.frame(season = 2024:2026, game_id = 1:3))
  local_mocked_bindings(
    rds_from_url = function(url) data.table::data.table(),
    load_pwhl_pbp = function(...) stop("must not reload")
  )
  expect_error(build_pwhl_db("pbp", con, rebuild = TRUE), "pwhl_games_in_data_repo.rds.*untouched")
  expect_table_kept(con, "pbp")
})
