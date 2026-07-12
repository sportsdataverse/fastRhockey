test_that("NHL Records - Player Stats", {
    skip_on_cran()
    skip_nhl_test()
    # The records `player-stats` endpoint does not accept a `playerId`
    # cayenne filter (returns 400). Test the unfiltered, paginated form.
    x <- tryCatch(nhl_records_player_stats(limit = 5), error = function(e) NULL)
    # Live-API resilience: records.nhl.com intermittently errors; skip rather
    # than fail when it is unavailable (mirrors the nhl_edge_* top-10 tests).
    skip_if(
        is.null(x) || !is.data.frame(x) || nrow(x) == 0,
        "NHL records player-stats API unavailable"
    )
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})
