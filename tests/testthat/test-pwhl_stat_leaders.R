test_that("PWHL - Get Goalie Stats", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_stats(position = "goalie", season = 2024, regular = TRUE)

    expect_true(!is.null(x))
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "player_id",
        "player_name",
        "team",
        "games_played"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})

test_that("PWHL - Get Skater Stats", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_stats(position = "skater", season = 2024, regular = TRUE)

    expect_true(!is.null(x))
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "player_id",
        "player_name",
        "team",
        "games_played"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
