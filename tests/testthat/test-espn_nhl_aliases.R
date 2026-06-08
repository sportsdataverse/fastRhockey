# test-espn_nhl_aliases.R
# Tests for ESPN NHL parity Group F: variants/aliases + calendar variants.

# ---------------------------------------------------------------------------
# 1. espn_nhl_teams_site
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_teams_site returns team rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_teams_site()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_teams_site at test time")
  }

  expect_in(sort(c("espn_team_id", "team", "abbreviation")), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 2. espn_nhl_team_schedule
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_team_schedule returns schedule rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_schedule(team_id = "4", season = 2025)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_team_schedule at test time")
  }

  key_cols <- c("game_id", "date", "home_id", "away_id", "team_id", "season")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 3. espn_nhl_standings_core
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_standings_core returns per-team standings rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_standings_core(season = 2026, season_type = 2)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_standings_core at test time")
  }

  key_cols <- c("team_id", "record_summary", "season", "season_type")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 4. espn_nhl_leaders_core
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_leaders_core returns (category, leader) rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_leaders_core()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_leaders_core at test time")
  }

  key_cols <- c(
    "category_display_name",
    "display_value",
    "value",
    "athlete_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 5. espn_nhl_coach_season
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_coach_season returns one-row coach data", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_coach_season(coach_id = "900", season = 2026)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_coach_season at test time")
  }

  key_cols <- c("coach_id", "season", "first_name", "last_name")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 6. espn_nhl_calendar_ondays
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_calendar_ondays returns game date rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_calendar_ondays()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_calendar_ondays at test time")
  }

  key_cols <- c("date", "start_date", "end_date")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 7. espn_nhl_calendar_offseason
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_calendar_offseason returns gracefully", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_calendar_offseason(season = 2026)

  # Offseason calendar is often empty (0 dates); graceful return is acceptable.
  expect_true(is.data.frame(x))

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 8. espn_nhl_calendar_postseason
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_calendar_postseason returns gracefully", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_calendar_postseason(season = 2026)

  # Postseason calendar may be empty mid-season; graceful return is acceptable.
  expect_true(is.data.frame(x))

  Sys.sleep(1)
})

# ---------------------------------------------------------------------------
# 9. espn_nhl_calendar_regular_season
# ---------------------------------------------------------------------------
test_that("ESPN - espn_nhl_calendar_regular_season returns game date rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_calendar_regular_season()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_calendar_regular_season at test time")
  }

  key_cols <- c("date", "start_date", "end_date")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})
