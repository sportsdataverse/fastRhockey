test_that("NHL - All Players by Season", {
    skip_on_cran()
    skip_nhl_test()
    skip_if(
        Sys.getenv("RUN_SLOW_NHL_TESTS") != "true",
        "slow test - iterates all 32 NHL teams"
    )
    x <- nhl_all_players_by_season(season = 2024)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
        expect_true("player_id" %in% names(x))
        expect_true("team_abbr" %in% names(x))
        expect_true("season" %in% names(x))
    }
})
