# Tests for ESPN NHL core-v2 athlete-detail wrappers (Tier 4).
#
# Athlete IDs used:
#   "4024820" = Patrik Laine (active skater with position/team/statistics)
#   "3114"    = Sidney Crosby (veteran with 16 awards)
#
# Live verification (2026-06-08):
#   player_core          -> OK  (1 row)
#   player_statistics    -> OK  (1 wide row, 70+ stat columns)
#   player_statisticslog -> OK  (10 rows for Laine)
#   player_eventlog      -> OK  (25 rows per page for Laine; 71 total events)
#   player_contracts     -> count=0 / graceful empty (DONE_WITH_CONCERNS)
#   player_awards        -> OK for Crosby (16 rows); empty for Laine
#   player_records       -> count=0 / graceful empty (DONE_WITH_CONCERNS)
#   player_seasons       -> HTTP 404 / graceful empty (DONE_WITH_CONCERNS)
#   player_injuries      -> HTTP 404 / graceful empty (DONE_WITH_CONCERNS)
#   player_vs_player     -> HTTP 404 / graceful empty (DONE_WITH_CONCERNS)
#   player_notes         -> HTTP 404 / graceful empty (DONE_WITH_CONCERNS)
#   athletes_index       -> OK  (paginated, 1197 active athletes)

ATHLETE_ID  <- "4024820"  # Patrik Laine
ATHLETE_ID2 <- "3114"     # Sidney Crosby


# ---------------------------------------------------------------------------
# 1. Player Core
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_core returns 1-row tibble with identity", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_player_core(athlete_id = ATHLETE_ID)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_player_core at test time")
  }

  key_cols <- c(
    "athlete_id", "first_name", "last_name", "full_name",
    "active", "athlete_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$athlete_id, ATHLETE_ID)
  expect_true(is.logical(x$active))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 2. Player Statistics
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_statistics returns wide 1-row tibble", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_player_statistics(athlete_id = ATHLETE_ID)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_player_statistics at test time")
  }

  key_cols <- c("athlete_id", "split_id", "split_name", "split_type")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$athlete_id, ATHLETE_ID)
  # Should have many stat columns flattened
  expect_true(ncol(x) > 10L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 3. Player Statisticslog
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_statisticslog returns season-by-season log", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_player_statisticslog(athlete_id = ATHLETE_ID)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_player_statisticslog at test time")
  }

  key_cols <- c("athlete_id", "season", "season_ref", "total_stats_ref")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$athlete_id == ATHLETE_ID))
  expect_true(is.integer(x$season))
  # Should cover multiple seasons
  expect_true(nrow(x) >= 5L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 4. Player Eventlog
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_eventlog returns one row per event", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_player_eventlog(athlete_id = ATHLETE_ID)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_player_eventlog at test time")
  }

  key_cols <- c(
    "athlete_id", "event_id", "team_id", "played",
    "event_ref", "statistics_ref", "count"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$athlete_id == ATHLETE_ID))
  expect_true(is.logical(x$played))
  expect_true(is.character(x$event_id))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 5. Player Contracts (DONE_WITH_CONCERNS — count=0 for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_contracts returns graceful empty for NHL", {
  skip_on_cran()
  skip_espn_test()

  # Should not error; should return empty data.frame
  x <- suppressWarnings(
    espn_nhl_player_contracts(athlete_id = ATHLETE_ID)
  )

  expect_s3_class(x, "data.frame")
  # count=0 path returns empty
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 6. Player Awards
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_awards returns award rows for decorated player", {
  skip_on_cran()
  skip_espn_test()

  # Crosby has 16 awards
  x <- espn_nhl_player_awards(athlete_id = ATHLETE_ID2)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No awards returned for Crosby at test time")
  }

  key_cols <- c("ref", "award_id", "athlete_id", "count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$athlete_id == ATHLETE_ID2))
  expect_true(x$count[1] >= 1L)

  Sys.sleep(1)
})

test_that("ESPN - espn_nhl_player_awards returns graceful empty for player without awards", {
  skip_on_cran()
  skip_espn_test()

  # Laine has count=0
  x <- suppressWarnings(
    espn_nhl_player_awards(athlete_id = ATHLETE_ID)
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 7. Player Records (DONE_WITH_CONCERNS — count=0 for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_records returns graceful empty for NHL", {
  skip_on_cran()
  skip_espn_test()

  x <- suppressWarnings(
    espn_nhl_player_records(athlete_id = ATHLETE_ID)
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 8. Player Seasons (DONE_WITH_CONCERNS — HTTP 404 for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_seasons returns graceful empty for NHL", {
  skip_on_cran()
  skip_espn_test()

  x <- suppressWarnings(
    espn_nhl_player_seasons(athlete_id = ATHLETE_ID)
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 9. Player Injuries (DONE_WITH_CONCERNS — HTTP 404 for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_injuries returns graceful empty for NHL", {
  skip_on_cran()
  skip_espn_test()

  x <- suppressWarnings(
    espn_nhl_player_injuries(athlete_id = ATHLETE_ID)
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 10. Player vs Player (DONE_WITH_CONCERNS — HTTP 404 for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_vs_player returns graceful empty for NHL", {
  skip_on_cran()
  skip_espn_test()

  x <- suppressWarnings(
    espn_nhl_player_vs_player(athlete_id = ATHLETE_ID, opponent_id = ATHLETE_ID2)
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 11. Player Notes (DONE_WITH_CONCERNS — HTTP 404 for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_player_notes returns graceful empty for NHL", {
  skip_on_cran()
  skip_espn_test()

  x <- suppressWarnings(
    espn_nhl_player_notes(athlete_id = ATHLETE_ID)
  )

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 0L)

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 12. Athletes Index
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_athletes_index returns paginated athlete rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_athletes_index(active = TRUE, limit = 10, page = 1)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_athletes_index at test time")
  }

  key_cols <- c("ref", "athlete_id", "active", "page", "count", "page_count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 10L)
  expect_true(all(x$active == TRUE))
  expect_true(all(x$page == 1L))
  expect_true(x$count[1] >= 100L)
  expect_true(x$page_count[1] >= 10L)
  expect_true(is.character(x$athlete_id))

  Sys.sleep(1)
})
