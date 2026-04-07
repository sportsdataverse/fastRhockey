test_that("PWHL - Get PWHL Player Box Scores", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_player_box(game_id = 27)

    expect_type(x, "list")
    expect_true("skaters" %in% names(x))
    expect_true("goalies" %in% names(x))

    # Skater box scores
    skaters <- x$skaters
    expect_s3_class(skaters, "data.frame")
    expect_true(nrow(skaters) > 0)

    skater_cols <- c(
        "player_id",
        "first_name",
        "last_name",
        "position",
        "goals",
        "assists",
        "points",
        "penalty_minutes",
        "plus_minus",
        "shots",
        "hits",
        "blocked_shots",
        "toi",
        "team_id",
        "game_id"
    )
    for (col in skater_cols) {
        expect_true(col %in% names(skaters), info = paste("Missing skater column:", col))
    }

    # Goalie box scores
    goalies <- x$goalies
    expect_s3_class(goalies, "data.frame")
    expect_true(nrow(goalies) > 0)

    goalie_cols <- c(
        "player_id",
        "first_name",
        "last_name",
        "position",
        "toi",
        "shots_against",
        "goals_against",
        "saves",
        "team_id",
        "game_id"
    )
    for (col in goalie_cols) {
        expect_true(col %in% names(goalies), info = paste("Missing goalie column:", col))
    }
})
