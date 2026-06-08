# test-espn_nhl_player_detail.R
# Tests for Group C player-detail wrappers added in feat/espn-nhl-endpoints.
# All tests are guarded by skip_on_cran() + skip_espn_test() (ESPN_TESTS=1).

# ---------------------------------------------------------------------------
# Helper: get a valid athlete_id at test time
# ---------------------------------------------------------------------------
get_test_athlete_id <- function() {
  tryCatch({
    r <- espn_nhl_team_roster(team_id = "4")
    if (is.data.frame(r) && nrow(r) > 0 && "athlete_id" %in% colnames(r)) {
      as.character(r$athlete_id[1])
    } else {
      "5149125"  # Connor Bedard fallback
    }
  }, error = function(e) "5149125")
}


# ===========================================================================
# 1. espn_nhl_player_bio
# ===========================================================================

test_that("espn_nhl_player_bio returns data.frame (404 handled gracefully)", {
  skip_on_cran()
  skip_espn_test()

  athlete_id <- get_test_athlete_id()
  x <- espn_nhl_player_bio(athlete_id = athlete_id)

  # Site_v2 bio returns 404 for NHL as of 2026-06-08; must be graceful
  expect_true(is.data.frame(x))

  Sys.sleep(1)
})


# ===========================================================================
# 2. espn_nhl_player_news
# ===========================================================================

test_that("espn_nhl_player_news returns data.frame", {
  skip_on_cran()
  skip_espn_test()

  athlete_id <- get_test_athlete_id()
  x <- espn_nhl_player_news(athlete_id = athlete_id, limit = 10)

  expect_true(is.data.frame(x))
  # When articles are returned, check core columns exist
  if (nrow(x) > 0) {
    core_cols <- c("athlete_id", "headline")
    expect_in(sort(core_cols), sort(colnames(x)))
    expect_s3_class(x, "data.frame")
  }

  Sys.sleep(1)
})


# ===========================================================================
# 3. espn_nhl_player_info
# ===========================================================================

test_that("espn_nhl_player_info returns data.frame (404 handled gracefully)", {
  skip_on_cran()
  skip_espn_test()

  athlete_id <- get_test_athlete_id()
  x <- espn_nhl_player_info(athlete_id = athlete_id)

  # Site_v2 athletes/{id} returns 404 for NHL as of 2026-06-08; must be graceful
  expect_true(is.data.frame(x))

  Sys.sleep(1)
})


# ===========================================================================
# 4. espn_nhl_player_career_stats
# ===========================================================================

test_that("espn_nhl_player_career_stats returns expected columns", {
  skip_on_cran()
  skip_espn_test()

  athlete_id <- get_test_athlete_id()
  x <- espn_nhl_player_career_stats(athlete_id = athlete_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No career-stats rows returned at test time")
  }

  core_cols <- c("athlete_id", "split_id", "split_name")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)

  Sys.sleep(1)
})


# ===========================================================================
# 5. espn_nhl_player_gamelog
# ===========================================================================

test_that("espn_nhl_player_gamelog returns data.frame (may be empty)", {
  skip_on_cran()
  skip_espn_test()

  athlete_id <- get_test_athlete_id()
  x <- espn_nhl_player_gamelog(athlete_id = athlete_id)

  # Must always return a data.frame (empty or populated)
  expect_true(is.data.frame(x))

  if (nrow(x) > 0) {
    core_cols <- c("athlete_id", "event_id", "game_date", "game_result")
    expect_in(sort(core_cols), sort(colnames(x)))
    expect_s3_class(x, "data.frame")
    # Stat columns from names vector should be present
    stat_cols <- c("goals", "assists", "points")
    expect_in(sort(stat_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})


# ===========================================================================
# 6. espn_nhl_players_index
# ===========================================================================

test_that("espn_nhl_players_index returns expected columns", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_players_index(active = TRUE, limit = 10, page = 1)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No players-index rows returned at test time")
  }

  core_cols <- c("ref", "athlete_id", "active", "page", "count", "page_count")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) <= 10L)
  expect_true(all(!is.na(x$athlete_id)))

  Sys.sleep(1)
})
