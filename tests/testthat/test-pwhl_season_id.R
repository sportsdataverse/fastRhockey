test_that("PWHL - Get PWHL Season IDs", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_season_id()

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "season_id",
        "season_name",
        "season_yr",
        "game_type_label"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }

    # Should contain at least the 2024 regular season
    expect_true(any(x$season_yr == 2024 & x$game_type_label == "regular"),
                info = "Expected 2024 regular season to be present")

    # Should contain at least the 2025 regular season
    expect_true(any(x$season_yr == 2025 & x$game_type_label == "regular"),
                info = "Expected 2025 regular season to be present")
})

test_that("PWHL - Resolve season ID", {
    skip_on_cran()
    skip_pwhl_test()

    sid <- .pwhl_resolve_season_id(season = 2025, game_type = "regular")
    expect_true(is.numeric(sid))
    expect_true(sid > 0)
})
