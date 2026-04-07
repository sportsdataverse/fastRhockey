test_that("NHL - Get TV Schedule (now)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_tv_schedule()

    expect_type(x, "list")
    expect_true("date" %in% names(x) || "broadcasts" %in% names(x))
})

test_that("NHL - Get TV Schedule (specific date)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_tv_schedule(date = "2025-01-15")

    expect_type(x, "list")
})
