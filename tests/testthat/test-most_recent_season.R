test_that("most_recent_nhl_season returns a numeric year", {
    x <- most_recent_nhl_season()
    expect_true(is.numeric(x))
    expect_true(x >= 2024)
    expect_true(x <= 2030)
})

test_that("most_recent_nhl_season_api_param returns XXXXYYYY format", {
    x <- most_recent_nhl_season_api_param()
    expect_true(is.character(x) || is.numeric(x) || inherits(x, "glue"))
    expect_true(nchar(as.character(x)) == 8)
})
