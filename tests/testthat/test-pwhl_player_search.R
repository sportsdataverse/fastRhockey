test_that("PWHL - Search PWHL Players", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_player_search(search_term = "Poulin")

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)

        expected_cols <- c(
            "player_id",
            "first_name",
            "last_name",
            "position"
        )
        for (col in expected_cols) {
            expect_true(col %in% names(x), info = paste("Missing column:", col))
        }
    }
})
