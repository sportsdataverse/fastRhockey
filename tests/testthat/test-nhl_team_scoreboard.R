test_that("nhl_team_scoreboard returns data", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_team_scoreboard(team_abbr = "TOR")
    expect_true(!is.null(x))
})
