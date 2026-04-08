test_that("PWHL - Get PWHL Game Summary", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_game_summary(game_id = 27)

    expect_true(is.list(x))
    expect_true("details" %in% names(x))
    expect_true("scoring" %in% names(x))
    expect_true("penalties" %in% names(x))

    # Details should be a data frame with game metadata
    expect_s3_class(x$details, "data.frame")
    expect_true(nrow(x$details) > 0)

    expected_detail_cols <- c(
        "game_id",
        "home_team",
        "away_team",
        "home_score",
        "away_score"
    )
    for (col in expected_detail_cols) {
        expect_true(col %in% names(x$details), info = paste("Missing column in details:", col))
    }
})
