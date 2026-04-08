test_that("PWHL - Get PWHL Skater Leaders", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_leaders(position = "skaters", season = 2025)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})

test_that("PWHL - Get PWHL Goalie Leaders", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_leaders(position = "goalies", season = 2025)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
