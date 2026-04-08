test_that("PWHL - Get PWHL Player Streaks", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_streaks(season = 2025)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
