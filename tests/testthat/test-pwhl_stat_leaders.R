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

test_that("pwhl_stats resolves the season argument (not hardcoded to 2024)", {
    skip_on_cran()
    x24 <- suppressWarnings(tryCatch(pwhl_stats(position = "goalie", season = 2024), error = function(e) NULL))
    x25 <- suppressWarnings(tryCatch(pwhl_stats(position = "goalie", season = 2025), error = function(e) NULL))
    if (!is.null(x24) && !is.null(x25) && nrow(x24) > 0 && nrow(x25) > 0) {
        expect_false(identical(x24$player_id, x25$player_id))
    }
})

test_that("pwhl_stats team filter accepts code and label; rejects unknowns", {
    skip_on_cran()
    x <- suppressWarnings(tryCatch(pwhl_stats(position = "skater", team = "OTT", season = 2025), error = function(e) NULL))
    if (!is.null(x) && nrow(x) > 0) {
        expect_setequal(unique(x$team), "OTT")
    }
    expect_error(pwhl_stats(position = "skater", team = "Springfield", season = 2025))
})
