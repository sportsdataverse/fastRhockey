# Tests for the HockeyTech PBP parser and enrichment transforms.
# Run with:
#   printf 'devtools::load_all(".")\ntestthat::test_file("tests/testthat/test-hockeytech_pbp.R")\n' > /tmp/run.R
#   Rscript /tmp/run.R

.load_fx <- function(stem) {
  jsonlite::read_json(
    testthat::test_path("fixtures", "hockeytech", paste0(stem, ".json"))
  )
}

test_that("parse_hockeytech_pbp one row per event with fastRhockey columns", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  expect_true(nrow(df) > 0)
  for (col in c("game_id", "event", "period_of_game", "time_of_period", "player_id",
                "x_coord", "y_coord", "goal", "plus_player_one_id", "penalty_shot")) {
    expect_true(col %in% names(df), info = col)
  }
  expect_true(all(df$game_id == 42))
})

test_that("parse_hockeytech_pbp handles empty payload", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(list(), game_id = 99)
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0L)
})

test_that("parse_hockeytech_pbp penalty: servedBy -> player_id, takenBy -> player_two_*", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  pens <- df[!is.na(df$event) & df$event == "penalty", ]
  expect_true(nrow(pens) > 0)
  expect_true("player_two_id" %in% names(df))
  expect_true("penalty_length" %in% names(df))
})

test_that("parse_hockeytech_pbp goal: plus/minus word ordinals", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  for (col in c("plus_player_one_id", "plus_player_two_id",
                "minus_player_one_id", "minus_player_two_id")) {
    expect_true(col %in% names(df), info = col)
  }
})

test_that("coord + clock transforms add parity columns", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  df <- fastRhockey:::hockeytech_add_coord_transforms(df)
  df <- fastRhockey:::hockeytech_add_clock_columns(df)
  for (col in c("x_coord_original", "x_coord_fixed", "x_coord_vertical",
                "minute_start", "clock", "sec_from_start")) {
    expect_true(col %in% names(df), info = col)
  }
  # sec_from_start should be numeric and non-negative for rows with valid times
  valid <- df[!is.na(df$sec_from_start), ]
  expect_true(nrow(valid) > 0)
  expect_true(all(valid$sec_from_start >= 0))
})

test_that("derived coordinates are rotations that keep every event on the rink (real game 42)", {
  testthat::skip_on_cran()
  # The feed puts the home team's attack toward x = 0 (sdv-internal-refs CANVAS.md).
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  home <- .load_fx("pwhl_game_summary_42")$GC$Gamesummary$meta$home_team
  df$home_team_id <- home
  df <- fastRhockey:::hockeytech_add_coord_transforms(df)
  shots <- df[df$event %in% c("shot", "goal") & !is.na(df$x_coord), ]
  is_home <- as.character(shots$team_id) == as.character(home)
  expect_gt(sum(is_home), 0)
  expect_gt(sum(!is_home), 0)
  for (col in c("x_coord_fixed", "x_coord_right", "y_coord_vertical")) {
    expect_true(all(abs(shots[[col]]) <= 100), info = col)
  }
  for (col in c("y_coord_fixed", "y_coord_right", "x_coord_vertical")) {
    expect_true(all(abs(shots[[col]]) <= 42.5), info = col)
  }
  # Every team attacks +x in the right frame; only the home team does in the fixed frame.
  expect_gt(stats::median(shots$x_coord_right[is_home]), 0)
  expect_gt(stats::median(shots$x_coord_right[!is_home]), 0)
  expect_gt(stats::median(shots$x_coord_fixed[is_home]), 0)
  expect_lt(stats::median(shots$x_coord_fixed[!is_home]), 0)
  expect_equal(shots$x_coord_vertical, -shots$y_coord_right)
  expect_equal(shots$y_coord_vertical, shots$x_coord_right)
  # Centre ice is (300,150) on the canvas, so (0,0) in every frame.
  ctr <- df[!is.na(df$x_coord_original) & df$x_coord_original == 300 & df$y_coord_original == 150, ]
  expect_gt(nrow(ctr), 0)
  expect_true(all(ctr$x_coord_fixed == 0 & ctr$y_coord_fixed == 0))
  # Faceoffs carry no team, so their side (and the right/vertical frames) is unknown.
  fo <- df[df$event == "faceoff" & !is.na(df$x_coord), ]
  expect_true(all(is.na(fo$x_coord_right)) && all(!is.na(fo$x_coord_fixed)))
})

test_that("without home_team_id the right and vertical columns are NA", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  df <- fastRhockey:::hockeytech_add_coord_transforms(df)
  expect_true(all(is.na(df$x_coord_right)) && all(is.na(df$y_coord_vertical)))
  expect_true(any(!is.na(df$x_coord_fixed)))
  # An empty home_team_id (no game summary) is unknown too, never "everyone is the visitor".
  df2 <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  df2$home_team_id <- ""
  df2 <- fastRhockey:::hockeytech_add_coord_transforms(df2)
  expect_true(all(is.na(df2$x_coord_right)))
  # Still numeric columns, as in sdv-py and sportsdataverse-js.
  for (col in c("x_coord_right", "y_coord_right", "x_coord_vertical", "y_coord_vertical")) {
    expect_type(df2[[col]], "double")
  }
})

test_that("hockeytech_add_clock_columns: period 2 offset by 1200", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  df <- fastRhockey:::hockeytech_add_clock_columns(df)
  p2 <- df[!is.na(df$period_of_game) & df$period_of_game == "2" &
              !is.na(df$minute_start) & df$minute_start == 0 &
              !is.na(df$second_start) & df$second_start == 0, ]
  if (nrow(p2) > 0) {
    expect_equal(p2$sec_from_start[1], 1200)
  }
})

test_that("hockeytech_backfill_power_play adds short_handed column", {
  testthat::skip_on_cran()
  df <- fastRhockey:::.parse_hockeytech_pbp(.load_fx("pwhl_pbp_42"), game_id = 42)
  df <- fastRhockey:::hockeytech_add_clock_columns(df)
  # backfill needs home_team_id and away_team_id
  df$home_team_id <- "3"
  df$away_team_id <- "1"
  df2 <- fastRhockey:::hockeytech_backfill_power_play(df)
  expect_true("short_handed" %in% names(df2))
  expect_true("power_play" %in% names(df2))
})

test_that("enrich_pbp (offline injected payloads) adds geometry + on-ice + meta", {
  testthat::skip_on_cran()
  pbp <- .load_fx("pwhl_pbp_42")
  df <- fastRhockey:::.parse_hockeytech_pbp(pbp, game_id = 42)
  meta <- .load_fx("pwhl_game_summary_42")
  shifts <- .load_fx("pwhl_gameshifts_42")
  out <- fastRhockey:::hockeytech_enrich_pbp(df, "pwhl", 42,
                                              meta_payload = meta,
                                              shifts_payload = shifts)
  for (col in c("shot_distance", "scoring_chance", "on_ice_home", "on_ice_away",
                "home_team_id", "game_date")) {
    expect_true(col %in% names(out), info = col)
  }
  shots <- out[!is.na(out$event) & out$event == "shot" & !is.na(out$on_ice_home), ]
  expect_true(nrow(shots) > 0)
  expect_true(grepl(",", shots$on_ice_home[1]))  # multiple on-ice players
})
