test_that("PWHL - Get PWHL Playoff Bracket", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_playoff_bracket(season = 2024)

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)

        expected_cols <- c(
            "round",
            "team_1_name",
            "team_2_name"
        )
        for (col in expected_cols) {
            expect_true(col %in% names(x), info = paste("Missing column:", col))
        }
    }
})
