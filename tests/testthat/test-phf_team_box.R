
test_that("phf_team_box", {
  skip_on_cran()
  skip_on_ci()
  cols <- c(
    "team",
    "game_id",
    "winner",
    "total_scoring",
    "successful_power_play",
    "power_play_opportunities",
    "power_play_percent",
    "penalty_minutes",
    "faceoff_percent",
    "blocked_opponent_shots",
    "takeaways",
    "giveaways",
    "period_1_shots",
    "period_2_shots",
    "period_3_shots",
    "overtime_shots",
    "shootout_made_shots",
    "shootout_missed_shots",
    "total_shots",
    "period_1_scoring",
    "period_2_scoring",
    "period_3_scoring",
    "overtime_scoring",
    "shootout_made_scoring",
    "shootout_missed_scoring"
  )
  x <- phf_team_box(game_id = 40863)
  y <- phf_team_box(game_id = 268079)
  z <- phf_team_box(game_id = 378721)

  expect_equal(colnames(x), cols)
  expect_equal(colnames(y), cols)
  expect_equal(colnames(z), cols)
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  expect_s3_class(z, "data.frame")
})
