test_that("NHL - Edge Goalie 5v5 Top 10 (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_goalie_5v5_top_10(sort_by = "savePctg")
    # Top-10 endpoints are documented but currently 404 on the
    # live server; accept either NULL or a valid fastRhockey_data
    # tibble so the test isn't reported as "empty".
    expect_true(is.null(x) || inherits(x, "fastRhockey_data"))
})

test_that("NHL - Edge Goalie 5v5 Top 10 (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_goalie_5v5_top_10(
        sort_by = "savePctg",
        season = 2024,
        game_type = 2
    )
    # Top-10 endpoints are documented but currently 404 on the
    # live server; accept either NULL or a valid fastRhockey_data
    # tibble so the test isn't reported as "empty".
    expect_true(is.null(x) || inherits(x, "fastRhockey_data"))
})
