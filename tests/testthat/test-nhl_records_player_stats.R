test_that("NHL Records - Player Stats", {
    skip_on_cran()
    skip_nhl_test()
    # The records `player-stats` endpoint does not accept a `playerId`
    # cayenne filter (returns 400). Test the unfiltered, paginated form.
    x <- nhl_records_player_stats(limit = 5)
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})
