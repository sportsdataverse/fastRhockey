test_that("NHL - Get Playoff Carousel", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_playoff_carousel(season = 2024)

    expect_type(x, "list")
    expect_true("rounds" %in% names(x))
})

test_that("NHL - Get Playoff Bracket", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_playoff_bracket(year = 2024)

    expect_type(x, "list")
    expect_true("series" %in% names(x))
})

test_that("NHL - Get Playoff Schedule (integer season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_playoff_schedule(season = 2024, series_letter = "a")

    expect_type(x, "list")
    expect_true("seriesLetter" %in% names(x))
})

test_that("NHL - Get Playoff Schedule (8-digit season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_playoff_schedule(season = "20232024", series_letter = "a")

    expect_type(x, "list")
    expect_true("seriesLetter" %in% names(x))
})
