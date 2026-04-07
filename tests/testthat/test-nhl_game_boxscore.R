test_that("NHL - Get NHL Game Boxscore", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_boxscore(game_id = 2024020001)

    expect_type(x, "list")
    expect_true("game_info" %in% names(x))
    expect_true("team_box" %in% names(x))
    expect_true("skater_stats" %in% names(x))
    expect_true("goalie_stats" %in% names(x))

    expect_s3_class(x$game_info, "data.frame")
    expect_s3_class(x$team_box, "data.frame")
    expect_s3_class(x$skater_stats, "data.frame")
    expect_s3_class(x$goalie_stats, "data.frame")
})

test_that("NHL - Boxscore skater stats have expected columns", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_boxscore(game_id = 2024020001)

    expected_cols <- c("player_id", "player_name", "team_abbrev", "position")
    for (col in expected_cols) {
        expect_true(
            col %in% names(x$skater_stats),
            info = paste("Missing column:", col)
        )
    }
    expect_true(nrow(x$skater_stats) > 0)
})

test_that("NHL - Boxscore goalie stats have expected columns", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_game_boxscore(game_id = 2024020001)

    expected_cols <- c("player_id", "player_name", "team_abbrev")
    for (col in expected_cols) {
        expect_true(
            col %in% names(x$goalie_stats),
            info = paste("Missing column:", col)
        )
    }
    expect_true(nrow(x$goalie_stats) > 0)
})
