test_that("nhl_club_schedule season view returns data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_club_schedule(team_abbr = "TOR")
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 1)
    expect_true("team_abbr" %in% colnames(x))
})

test_that("nhl_club_schedule week view returns data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_club_schedule(team_abbr = "TOR", view = "week")
    expect_true(!is.null(x))
})
