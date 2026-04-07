test_that("NHL - Get Partner Game Odds", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_partner_game_odds(country_code = "US")

    expect_type(x, "list")
    expect_true("bettingPartner" %in% names(x) || "games" %in% names(x))
})
