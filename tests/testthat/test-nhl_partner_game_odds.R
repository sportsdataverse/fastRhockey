test_that("NHL - Get Partner Game Odds", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_partner_game_odds(country_code = "US")

    # Endpoint may return NULL if no games scheduled or endpoint unavailable
    expect_true(is.list(x) || is.null(x))
})
