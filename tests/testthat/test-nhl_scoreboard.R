test_that("NHL - Get Scoreboard (now)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_scoreboard()

    expect_type(x, "list")
    expect_true("focusedDate" %in% names(x))
    expect_true("gamesByDate" %in% names(x))
})

test_that("NHL - Get Scoreboard (by date)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_scoreboard(date = "2024-01-15")

    if (!is.null(x)) {
        expect_type(x, "list")
        expect_true("focusedDate" %in% names(x) || "gamesByDate" %in% names(x))
    }
})
