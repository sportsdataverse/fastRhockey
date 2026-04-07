test_that("NHL - Get Meta Information", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_meta()

    expect_type(x, "list")
    expect_true("players" %in% names(x) || "teams" %in% names(x))
})

test_that("NHL - Get Meta for Game", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_meta(game_id = 2024020001)

    expect_type(x, "list")
    expect_true("teams" %in% names(x))
})

test_that("NHL - Get Location", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_location()

    expect_type(x, "list")
    expect_true("country" %in% names(x))
})
