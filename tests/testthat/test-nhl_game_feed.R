test_that("NHL - Get NHL Game PBP", {
    skip_on_cran()
    skip_nhl_test()
    # Known completed game from 2024-25 season
    x <- nhl_game_pbp(game_id = 2024020001)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "event_id",
        "event_idx",
        "event_type",
        "period",
        "period_time",
        "game_seconds",
        "x",
        "y",
        "x_fixed",
        "y_fixed",
        "event_team_abbr",
        "home_abbr",
        "away_abbr",
        "game_id",
        "season"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})

test_that("NHL - Get NHL Game Feed returns named list", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_feed(game_id = 2024020001)

    expect_type(x, "list")
    expect_true("pbp" %in% names(x))
    expect_true("game_info" %in% names(x))
    expect_true("rosters" %in% names(x))

    expect_s3_class(x$pbp, "data.frame")
    expect_s3_class(x$game_info, "data.frame")
    expect_s3_class(x$rosters, "data.frame")
})

test_that("NHL - Game Feed PBP has shot metrics", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_pbp(game_id = 2024020001)

    # Shot-related columns should exist
    expect_true("shot_distance" %in% names(x))
    expect_true("shot_angle" %in% names(x))

    # Filter to shots — they should have non-NA values
    shots <- x[x$event_type %in% c("SHOT", "GOAL", "MISSED_SHOT"), ]
    expect_true(nrow(shots) > 0)
    expect_true(any(!is.na(shots$shot_distance)))
})

test_that("NHL - Game Feed PBP has xG column", {
    skip_on_cran()
    skip_nhl_test()
    skip_if_not_installed("xgboost")
    x <- nhl_game_pbp(game_id = 2024020001)

    expect_true("xg" %in% names(x))

    # Shots should have non-NA xG values
    shots <- x[x$event_type %in% c("SHOT", "GOAL", "MISSED_SHOT"), ]
    expect_true(nrow(shots) > 0)
    expect_true(any(!is.na(shots$xg)))

    # xG values should be between 0 and 1
    xg_vals <- shots$xg[!is.na(shots$xg)]
    expect_true(all(xg_vals >= 0 & xg_vals <= 1))
})

test_that("NHL - Game Feed PBP has strength states", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_pbp(game_id = 2024020001)

    expect_true("strength_state" %in% names(x))
    expect_true("home_skaters" %in% names(x))
    expect_true("away_skaters" %in% names(x))
})

test_that("NHL - Game PBP returns fastRhockey_data class", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_pbp(game_id = 2024020001)
    expect_s3_class(x, "fastRhockey_data")
})

test_that("NHL - Game PBP raw parameter returns unprocessed API list", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_pbp(game_id = 2024020001, raw = TRUE)

    expect_type(x, "list")
    # Raw API response should have these top-level fields
    expect_true("plays" %in% names(x))
    expect_true("homeTeam" %in% names(x))
    expect_true("awayTeam" %in% names(x))
    expect_true("season" %in% names(x))
    expect_true("gameDate" %in% names(x))

    # Should NOT have processed columns (these are added by the pipeline)
    expect_false("event_type" %in% names(x))
    expect_false("x_fixed" %in% names(x))
})

test_that("NHL - Game Feed raw parameter returns unprocessed API list", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_feed(game_id = 2024020001, raw = TRUE)

    expect_type(x, "list")
    # Raw response should be the direct API JSON structure
    expect_true("plays" %in% names(x))
    expect_true("homeTeam" %in% names(x))
    expect_true("awayTeam" %in% names(x))
    expect_true("id" %in% names(x))

    # Should NOT be a named list with pbp/game_info/rosters
    expect_false("pbp" %in% names(x))
    expect_false("game_info" %in% names(x))
    expect_false("rosters" %in% names(x))
})
