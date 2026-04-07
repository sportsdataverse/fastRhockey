test_that("nhl_club_stats_season returns data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_club_stats_season(team_abbr = "TOR")
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 1)
    expect_true("team_abbr" %in% colnames(x))
})
