.load_fx <- function(stem) {
  jsonlite::read_json(testthat::test_path("fixtures", "hockeytech", paste0(stem, ".json")))
}

test_that(".mmss_to_seconds converts MM:SS", {
  expect_equal(fastRhockey:::.mmss_to_seconds("03:16"), 196L)
  expect_equal(fastRhockey:::.mmss_to_seconds("00:00"), 0L)
  expect_true(is.na(fastRhockey:::.mmss_to_seconds(NA)))
  expect_true(is.na(fastRhockey:::.mmss_to_seconds("")))
})

test_that(".derive_season_year handles century rollover", {
  expect_equal(fastRhockey:::.derive_season_year("1999-00 Regular Season"), 2000L)
  expect_equal(fastRhockey:::.derive_season_year("2024-25 Regular Season"), 2025L)
  expect_equal(fastRhockey:::.derive_season_year("2024 Regular Season"), 2024L)
})

test_that(".parse_hockeytech_seasons derives end-year + game type", {
  df <- fastRhockey:::.parse_hockeytech_seasons(.load_fx("pwhl_seasons"))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("season_id","season_name","season_yr","game_type_label") %in% names(df)))
  reg2425 <- df[grepl("2024-25 Regular", df$season_name), ]
  if (nrow(reg2425) > 0) {
    expect_equal(reg2425$season_yr[1], 2025)
    expect_equal(reg2425$game_type_label[1], "regular")
  }
})

test_that(".parse_hockeytech_shifts returns one row per stint, countdown clock", {
  df <- fastRhockey:::.parse_hockeytech_shifts(.load_fx("pwhl_gameshifts_42"), game_id = 42)
  expect_true(nrow(df) > 0)
  expect_true(all(c("game_id","player_id","first_name","last_name","home",
                    "period","start_time","end_time","start_s","end_s") %in% names(df)))
  expect_true(all(df$start_s >= df$end_s, na.rm = TRUE))
  expect_true(all(df$game_id == 42))
})

test_that(".hockeytech_season_id passes through explicit season_id", {
  expect_equal(fastRhockey:::.hockeytech_season_id("pwhl", season_id = 5), 5L)
})
