test_that("NHL - Get Schedule Calendar (now)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule_calendar()

    expect_type(x, "list")
    expect_true("startDate" %in% names(x))
    expect_true("endDate" %in% names(x))
})

test_that("NHL - Get Schedule Calendar (specific date)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_schedule_calendar(date = "2025-01-15")

    expect_type(x, "list")
    expect_true("startDate" %in% names(x))
})
