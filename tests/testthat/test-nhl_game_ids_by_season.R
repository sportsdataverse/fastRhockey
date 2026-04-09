test_that("NHL - Game IDs by Season (single team fast path)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_ids_by_season(
        season = 2025,
        team_abbr = "TOR"
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
        expect_true("game_type" %in% names(x))
        expect_true("home_team" %in% names(x))
        expect_true("away_team" %in% names(x))
    }
})

test_that("NHL - Game IDs by Season (all teams, regular season)", {
    skip_on_cran()
    skip_nhl_test()
    skip_if(
        Sys.getenv("RUN_SLOW_NHL_TESTS") != "true",
        "slow test"
    )
    x <- nhl_game_ids_by_season(
        season = 2024,
        game_types = c(2L)
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
        expect_true("game_id" %in% names(x))
    }
})
