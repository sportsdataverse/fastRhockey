test_that("nhl_team_prospects returns data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_team_prospects(team_abbr = "TOR")
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 1)
    expect_true("team_abbr" %in% colnames(x))
})
