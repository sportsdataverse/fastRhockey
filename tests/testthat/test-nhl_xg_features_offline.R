# helper_nhl_prepare_xg_data() turns nhl_game_pbp() output into the xG model
# features. It needs no xgboost: run it on a recorded game, with the model's
# feature list stubbed (one real feature, one that must be zero-padded).
test_that("helper_nhl_prepare_xg_data() builds model features from a recorded game", {
  skip_on_cran()
  local_http_fixtures()
  pbp <- suppressWarnings(suppressMessages(nhl_game_pbp(game_id = 2024020001)))
  expect_gt(nrow(pbp), 0)

  env <- fastRhockey:::.xg_env
  old <- env$xg_feature_names_5v5
  withr::defer(env$xg_feature_names_5v5 <- old)
  env$xg_feature_names_5v5 <- c("shot_distance", "padded_feature")

  m <- fastRhockey:::helper_nhl_prepare_xg_data(pbp)
  expect_s3_class(m, "data.frame")
  expect_gt(nrow(m), 0)
  expect_true(all(c("game_id", "event_id", "strength_state", "shot_distance",
                    "shot_angle", "goal", "padded_feature") %in% names(m)))
  expect_true(all(m$padded_feature == 0L))
  expect_true(all(m$goal %in% c(0, 1)))
})
