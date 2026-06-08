## tests/testthat/test-espn_nhl_events_extra.R
## Group E: game long-tail wrappers
## Run with ESPN_TESTS=1 set; skipped on CRAN and CI.

test_that("espn_nhl_game_play returns expected columns", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]
  plays    <- espn_nhl_game_plays(event_id = event_id)

  if (length(event_id) == 0 || is.na(event_id) ||
      nrow(plays) == 0 || is.na(plays$id[1])) {
    skip("Could not derive event_id / play_id for espn_nhl_game_play test")
  }
  play_id <- plays$id[1]

  x <- espn_nhl_game_play(event_id = event_id, play_id = play_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_play at test time")
  }

  expected_cols <- c("event_id", "cid", "play_id", "id", "sequence_number",
                     "type_id", "type_text", "text", "period_number",
                     "clock_value", "scoring_play", "play_ref")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)

  Sys.sleep(1)
})


test_that("espn_nhl_game_play_personnel returns expected structure", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]
  plays    <- espn_nhl_game_plays(event_id = event_id)

  if (length(event_id) == 0 || is.na(event_id) ||
      nrow(plays) == 0 || is.na(plays$id[1])) {
    skip("Could not derive event_id / play_id for espn_nhl_game_play_personnel test")
  }
  play_id <- plays$id[1]

  x <- espn_nhl_game_play_personnel(event_id = event_id, play_id = play_id)

  # This endpoint is sparse for NHL — result may be empty data.frame() or
  # a tibble with competitor_ref rows (no athlete_id populated)
  expect_s3_class(x, "data.frame")

  if (nrow(x) > 0) {
    expected_cols <- c("event_id", "cid", "play_id",
                       "competitor_id", "competitor_ref")
    expect_in(sort(expected_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})


test_that("espn_nhl_game_status returns expected columns", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]

  if (length(event_id) == 0 || is.na(event_id)) {
    skip("Could not derive event_id for espn_nhl_game_status test")
  }

  x <- espn_nhl_game_status(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_status at test time")
  }

  expected_cols <- c("event_id", "cid", "clock", "display_clock",
                     "period", "type_id", "type_name", "type_state",
                     "type_completed", "status_ref")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)

  Sys.sleep(1)
})


test_that("espn_nhl_game_team_leaders returns expected columns", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]
  teams    <- espn_nhl_game_teams(event_id = event_id)

  if (length(event_id) == 0 || is.na(event_id) ||
      nrow(teams) == 0 || is.na(teams$team_id[1])) {
    skip("Could not derive event_id / team_id for espn_nhl_game_team_leaders test")
  }
  team_id <- teams$team_id[1]

  x <- espn_nhl_game_team_leaders(event_id = event_id, team_id = team_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_team_leaders at test time")
  }

  expected_cols <- c("event_id", "cid", "team_id",
                     "category_name", "category_display",
                     "display_value", "value",
                     "athlete_ref", "athlete_id")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)

  Sys.sleep(1)
})


test_that("espn_nhl_game_team_record returns a data.frame (sparse/graceful)", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]
  teams    <- espn_nhl_game_teams(event_id = event_id)

  if (length(event_id) == 0 || is.na(event_id) ||
      nrow(teams) == 0 || is.na(teams$team_id[1])) {
    skip("Could not derive event_id / team_id for espn_nhl_game_team_record test")
  }
  team_id <- teams$team_id[1]

  # This endpoint is 404 for most NHL games; just confirm graceful empty return
  x <- espn_nhl_game_team_record(event_id = event_id, team_id = team_id)
  expect_s3_class(x, "data.frame")

  if (nrow(x) > 0) {
    expected_cols <- c("event_id", "cid", "team_id", "name",
                       "summary", "display_value")
    expect_in(sort(expected_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})


test_that("espn_nhl_game_official_detail returns expected columns", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]
  offs     <- espn_nhl_game_officials(event_id = event_id)

  if (length(event_id) == 0 || is.na(event_id) ||
      nrow(offs) == 0 || is.na(offs$official_id[1])) {
    skip("Could not derive event_id / official_id for espn_nhl_game_official_detail test")
  }
  official_id <- offs$official_id[1]

  x <- espn_nhl_game_official_detail(event_id   = event_id,
                                     official_id = official_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_official_detail at test time")
  }

  expected_cols <- c("event_id", "cid", "official_id", "id",
                     "first_name", "last_name", "full_name",
                     "position_id", "position_name", "official_ref")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)

  Sys.sleep(1)
})


test_that("espn_nhl_game_propbets returns a data.frame (sparse/graceful)", {
  skip_on_cran()
  skip_espn_test()

  sb       <- espn_nhl_scoreboard(dates = "20250110")
  event_id <- sb$game_id[1]

  if (length(event_id) == 0 || is.na(event_id)) {
    skip("Could not derive event_id for espn_nhl_game_propbets test")
  }

  # 404 for most NHL games; confirm graceful empty return
  x <- espn_nhl_game_propbets(event_id = event_id)
  expect_s3_class(x, "data.frame")

  if (nrow(x) > 0) {
    expected_cols <- c("event_id", "cid", "id", "name",
                       "display_value", "propbet_ref")
    expect_in(sort(expected_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})
