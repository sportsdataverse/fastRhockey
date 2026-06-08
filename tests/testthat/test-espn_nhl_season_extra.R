# tests/testthat/test-espn_nhl_season_extra.R
# Group D: ESPN NHL season long-tail wrappers

test_that("ESPN - espn_nhl_season_weeks (DONE_WITH_CONCERNS: empty for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # NHL does not use weekly scheduling in ESPN core-v2; expect empty tibble
  x <- espn_nhl_season_weeks(season = 2026, season_type = 2)

  expect_true(is.data.frame(x))
  # If data returned (not expected for NHL), validate columns
  if (nrow(x) > 0) {
    key_cols <- c("ref", "week", "season", "season_type", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
    expect_true(all(x$season_type == 2L))
    expect_true(is.integer(x$week))
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_week (DONE_WITH_CONCERNS: 404 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # NHL does not use weeks; expect empty tibble
  x <- espn_nhl_season_week(season = 2026, season_type = 2, week = 1)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("season", "season_type", "week", "start_date", "end_date", "week_ref")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_equal(x$season, 2026L)
    expect_equal(x$season_type, 2L)
    expect_equal(x$week, 1L)
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_week_games (DONE_WITH_CONCERNS: 404 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # NHL does not use weeks; expect empty tibble
  x <- espn_nhl_season_week_games(season = 2026, season_type = 2, week = 1)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "event_id", "season", "season_type", "week", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
    expect_true(all(x$season_type == 2L))
    expect_true(all(x$week == 1L))
    expect_true(is.character(x$event_id))
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_awards", {
  skip_on_cran()
  skip_espn_test()

  # Try a completed season (2025) for awards data
  x <- espn_nhl_season_awards(season = 2025)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "award_id", "season", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2025L))
    expect_true(is.character(x$award_id))
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_powerindex (DONE_WITH_CONCERNS: not published for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # ESPN does not publish powerindex for NHL; expect empty tibble
  x <- espn_nhl_season_powerindex(season = 2026)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "powerindex_id", "season", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
    expect_true(is.character(x$powerindex_id))
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_powerindex_leaders (DONE_WITH_CONCERNS: not published for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # ESPN does not publish powerindex for NHL; expect empty tibble
  x <- espn_nhl_season_powerindex_leaders(season = 2026)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "leader_id", "season", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
    expect_true(is.character(x$leader_id))
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_group_children", {
  skip_on_cran()
  skip_espn_test()

  # Derive a conference group_id live from season_groups
  grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
  if (!is.data.frame(grps) || nrow(grps) == 0) {
    skip("Could not retrieve season groups to derive group_id")
  }
  gid <- grps$group_id[1]

  x <- espn_nhl_season_group_children(season = 2026, season_type = 2,
                                       group_id = gid)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "child_group_id", "season", "season_type",
                  "group_id", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
    expect_true(all(x$season_type == 2L))
    expect_true(all(x$group_id == as.integer(gid)))
    expect_true(is.character(x$child_group_id))
    # A conference group should have at least 1 division child
    expect_true(nrow(x) >= 1L)
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_type_corrections (DONE_WITH_CONCERNS: empty for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # ESPN does not publish score corrections for NHL; expect empty tibble
  x <- espn_nhl_season_type_corrections(season = 2026, season_type = 2)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "correction_id", "season", "season_type", "count", "page_count")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
    expect_true(all(x$season_type == 2L))
    expect_true(is.character(x$correction_id))
  }

  Sys.sleep(1)
})


test_that("ESPN - espn_nhl_season_players (alias of season_athletes)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_players(season = 2026, limit = 10, page = 1)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season players endpoint at test time")
  }

  key_cols <- c("ref", "player_id", "season", "page", "count", "page_count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(all(x$page == 1L))
  expect_true(is.character(x$player_id))
  # NHL season has thousands of players across many pages
  expect_true(x$count[1] > 100L)
  expect_true(x$page_count[1] > 10L)

  Sys.sleep(1)
})
