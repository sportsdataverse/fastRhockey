test_that("PWHL - Get PWHL Play-by-Play", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_pbp(game_id = 27)

    if (is.data.frame(x) && nrow(x) > 0) {
        expected_cols <- c(
            "game_id",
            "event",
            "period_of_game",
            "time_of_period",
            "team_id"
        )
        for (col in expected_cols) {
            expect_true(col %in% names(x), info = paste("Missing column:", col))
        }

        enriched <- c("x_coord", "y_coord", "shot_distance", "shot_angle",
                      "scoring_chance", "on_ice_home", "on_ice_away")
        for (col in enriched) {
            expect_true(col %in% names(x), info = paste("Missing enriched column:", col))
        }
        expect_true(any(x$event == "blocked_shot"))  # superset now includes blocked shots
    } else {
        expect_s3_class(x, "data.frame")
    }
})
