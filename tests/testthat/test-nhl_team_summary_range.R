test_that("NHL - Team Summary Range", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_team_summary_range(
        start_season = 2023,
        end_season = 2024
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
        expect_true("season" %in% names(x))
    }
})
