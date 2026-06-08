test_that("ESPN NHL Game (event metadata)", {
  skip_on_cran()
  skip_espn_test()

  # Derive a completed game id from the scoreboard
  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0 || !"game_id" %in% colnames(sb)) {
    skip("Could not retrieve scoreboard to derive event_id")
  }
  event_id <- sb$game_id[1]

  x <- espn_nhl_game(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game at test time")
  }

  core_cols <- c("event_id", "id", "date", "name", "short_name",
                 "competition_id", "season_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  # cid == event_id for NHL
  expect_equal(as.character(x$competition_id[1]), as.character(event_id))

  Sys.sleep(1)
})

test_that("ESPN NHL Game Competition", {
  skip_on_cran()
  skip_espn_test()

  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0) skip("no scoreboard")
  event_id <- sb$game_id[1]

  x <- espn_nhl_game_competition(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_competition at test time")
  }

  core_cols <- c("event_id", "cid", "id", "date", "attendance",
                 "neutral_site", "venue_ref", "status_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(as.character(x$cid[1]), as.character(event_id))

  Sys.sleep(1)
})

test_that("ESPN NHL Game Teams (competitors collection)", {
  skip_on_cran()
  skip_espn_test()

  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0) skip("no scoreboard")
  event_id <- sb$game_id[1]

  x <- espn_nhl_game_teams(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_teams at test time")
  }

  core_cols <- c("event_id", "cid", "team_id", "home_away", "winner",
                 "team_ref", "score_ref", "linescores_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 2L)  # NHL games always have 2 competitors
  expect_true(all(x$cid == as.character(event_id)))

  Sys.sleep(1)
})

test_that("ESPN NHL Game Team (singular competitor)", {
  skip_on_cran()
  skip_espn_test()

  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0) skip("no scoreboard")
  event_id <- sb$game_id[1]

  tms <- espn_nhl_game_teams(event_id = event_id)
  if (is.null(tms) || !is.data.frame(tms) || nrow(tms) == 0) {
    skip("Could not retrieve game_teams to derive team_id")
  }
  team_id <- tms$team_id[1]

  x <- espn_nhl_game_team(event_id = event_id, team_id = team_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_team at test time")
  }

  core_cols <- c("event_id", "cid", "team_id", "id", "home_away", "winner",
                 "team_ref", "score_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(as.character(x$team_id[1]), as.character(team_id))

  Sys.sleep(1)
})

test_that("ESPN NHL Game Team Roster", {
  skip_on_cran()
  skip_espn_test()

  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0) skip("no scoreboard")
  event_id <- sb$game_id[1]

  tms <- espn_nhl_game_teams(event_id = event_id)
  if (is.null(tms) || !is.data.frame(tms) || nrow(tms) == 0) {
    skip("Could not retrieve game_teams to derive team_id")
  }
  team_id <- tms$team_id[1]

  x <- espn_nhl_game_team_roster(event_id = event_id, team_id = team_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_team_roster at test time")
  }

  core_cols <- c("event_id", "cid", "team_id", "player_id", "jersey",
                 "display_name", "scratched", "athlete_id", "athlete_ref",
                 "position_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0L)
  expect_true(all(x$team_id == as.character(team_id)))

  Sys.sleep(1)
})

test_that("ESPN NHL Game Team Linescores", {
  skip_on_cran()
  skip_espn_test()

  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0) skip("no scoreboard")
  event_id <- sb$game_id[1]

  tms <- espn_nhl_game_teams(event_id = event_id)
  if (is.null(tms) || !is.data.frame(tms) || nrow(tms) == 0) {
    skip("Could not retrieve game_teams to derive team_id")
  }
  team_id <- tms$team_id[1]

  x <- espn_nhl_game_team_linescores(event_id = event_id, team_id = team_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_team_linescores at test time")
  }

  core_cols <- c("event_id", "cid", "team_id", "period", "value",
                 "display_value")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 3L)  # at least 3 periods for a completed game
  expect_true(all(x$team_id == as.character(team_id)))

  Sys.sleep(1)
})

test_that("ESPN NHL Game Team Statistics", {
  skip_on_cran()
  skip_espn_test()

  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0) skip("no scoreboard")
  event_id <- sb$game_id[1]

  tms <- espn_nhl_game_teams(event_id = event_id)
  if (is.null(tms) || !is.data.frame(tms) || nrow(tms) == 0) {
    skip("Could not retrieve game_teams to derive team_id")
  }
  team_id <- tms$team_id[1]

  x <- espn_nhl_game_team_statistics(event_id = event_id, team_id = team_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_game_team_statistics at test time")
  }

  core_cols <- c("event_id", "cid", "team_id")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  # Should have more than just the 3 echo cols
  expect_true(ncol(x) > 3L)
  expect_equal(as.character(x$team_id[1]), as.character(team_id))

  Sys.sleep(1)
})
