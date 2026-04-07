test_that("PWHL - Get PWHL Season IDs", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_season_id()

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "season_yr",
        "game_type_label",
        "season_id"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }

    # Verify known season data exists
    expect_true(2024 %in% x$season_yr)
    expect_true("regular" %in% x$game_type_label)
})
