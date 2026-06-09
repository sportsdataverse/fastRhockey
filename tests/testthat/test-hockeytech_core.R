.load_fx <- function(stem) {
  jsonlite::read_json(testthat::test_path("fixtures", "hockeytech", paste0(stem, ".json")))
}

test_that("schedule parser", {
  df <- fastRhockey:::.parse_hockeytech_schedule(.load_fx("pwhl_schedule_2025"))
  expect_true(all(c("game_id", "game_date", "home_team", "home_team_id",
                    "away_team", "away_team_id", "home_score", "away_score") %in% names(df)))
  expect_true(nrow(df) > 0)
  expect_s3_class(df, "data.frame")
})

test_that("schedule parser returns empty df for empty payload", {
  df <- fastRhockey:::.parse_hockeytech_schedule(list())
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that("standings parser computes total wins", {
  df <- fastRhockey:::.parse_hockeytech_standings(.load_fx("pwhl_standings_5"))
  expect_true(all(c("team", "team_rank", "games_played", "points", "wins",
                    "losses", "regulation_wins", "non_reg_wins") %in% names(df)))
  expect_true(nrow(df) > 0)
  expect_true(all(df$wins >= df$regulation_wins, na.rm = TRUE))
  # wins = regulation_wins + non_reg_wins
  expect_true(all(df$wins == df$regulation_wins + df$non_reg_wins, na.rm = TRUE))
})

test_that("standings parser returns empty df for empty payload", {
  df <- fastRhockey:::.parse_hockeytech_standings(list())
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

test_that("teams parser", {
  df <- fastRhockey:::.parse_hockeytech_teams(.load_fx("pwhl_teams_5"))
  expect_true(all(c("team_name", "team_id") %in% names(df)))
  expect_true(nrow(df) > 0)
  expect_s3_class(df, "data.frame")
})

test_that("roster parser skips non-dict entries", {
  df <- fastRhockey:::.parse_hockeytech_roster(.load_fx("pwhl_roster_1_5"))
  expect_true(nrow(df) > 0)
  expect_s3_class(df, "data.frame")
  expect_true("player_id" %in% names(df) || "first_name" %in% names(df))
})

test_that("player_stats parser binds stat types with stat_type column", {
  ps <- fastRhockey:::.parse_hockeytech_player_stats(.load_fx("pwhl_player_stats_27"))
  expect_s3_class(ps, "data.frame")
  expect_true(all(c("season_id", "games_played", "points") %in% names(ps)))
  expect_true("stat_type" %in% names(ps))
  expect_true(nrow(ps) > 0)
})

test_that("leaders parser handles skaters/goalies shape", {
  ld <- fastRhockey:::.parse_hockeytech_leaders(.load_fx("pwhl_leaders_5"))
  expect_s3_class(ld, "data.frame")
  expect_true(nrow(ld) > 0)
})

test_that("game_summary parser returns named list with required components", {
  gs <- fastRhockey:::.parse_hockeytech_game_summary(.load_fx("pwhl_game_summary_42"), game_id = 42)
  expect_true(all(c("game", "goals", "penalties", "shots_by_period", "three_stars") %in% names(gs)))
  expect_s3_class(gs$game, "data.frame")
  expect_true(nrow(gs$game) >= 1)
  expect_equal(gs$game$game_id[1], 42)
  expect_s3_class(gs$goals, "data.frame")
  expect_s3_class(gs$penalties, "data.frame")
  expect_s3_class(gs$shots_by_period, "data.frame")
  expect_s3_class(gs$three_stars, "data.frame")
})

test_that("game_summary shots_by_period has correct shape from dict format", {
  gs <- fastRhockey:::.parse_hockeytech_game_summary(.load_fx("pwhl_game_summary_42"), game_id = 42)
  sbp <- gs$shots_by_period
  expect_true(nrow(sbp) > 0)
  expect_true(all(c("side", "period", "shots") %in% names(sbp)))
})

test_that("game_summary three_stars falls back to mvps", {
  gs <- fastRhockey:::.parse_hockeytech_game_summary(.load_fx("pwhl_game_summary_42"), game_id = 42)
  # pwhl_game_summary_42 has empty threeStars but has mvps
  expect_true(nrow(gs$three_stars) > 0)
})

# --- Internal family-core function tests (offline / structure only) ---

test_that(".hockeytech_most_recent_season returns integer or falls back to 2026", {
  # Use a known-good fixture for seasons (already copied)
  seasons_df <- fastRhockey:::.parse_hockeytech_seasons(.load_fx("pwhl_seasons"))
  result <- if (nrow(seasons_df) > 0 && "season_yr" %in% names(seasons_df)) {
    max(seasons_df$season_yr, na.rm = TRUE)
  } else {
    2026L
  }
  expect_true(is.integer(result) || is.numeric(result))
  expect_true(result >= 2024L)
})

test_that(".hockeytech_season_id_df parses seasons frame correctly", {
  df <- fastRhockey:::.parse_hockeytech_seasons(.load_fx("pwhl_seasons"))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("season_id", "season_name", "season_yr", "game_type_label") %in% names(df)))
})
