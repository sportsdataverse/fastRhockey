test_that("PWHL - Get PWHL Scorebar", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_scorebar(days_back = 30, days_ahead = 30)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)

        expected_cols <- c(
            "game_id",
            "home_team",
            "away_team"
        )
        for (col in expected_cols) {
            expect_true(col %in% names(x), info = paste("Missing column:", col))
        }
    }
})
