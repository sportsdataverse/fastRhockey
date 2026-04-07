test_that("nhl_roster_season returns data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_roster_season(team_abbr = "TOR")
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 1)
    expect_true("season" %in% colnames(x))
})
