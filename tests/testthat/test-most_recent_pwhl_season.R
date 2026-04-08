test_that("most_recent_pwhl_season returns a numeric year", {
    x <- most_recent_pwhl_season()
    expect_true(is.numeric(x))
    expect_true(x >= 2024)
    expect_true(x <= 2100)
})
