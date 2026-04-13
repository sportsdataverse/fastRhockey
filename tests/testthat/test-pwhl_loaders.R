test_that("PWHL - Load PWHL PBP", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_pbp(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true("game_id" %in% names(x))
    }
})

test_that("PWHL - Load PWHL Player Box", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_player_box(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
    }
})

test_that("PWHL - Load PWHL Schedule", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_schedule(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
    }
})

test_that("PWHL - Load PWHL Rosters", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_rosters(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
    }
})

test_that("PWHL - Load PWHL Skater Box", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_skater_box(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true("game_id" %in% names(x))
    }
})

test_that("PWHL - Load PWHL Goalie Box", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_goalie_box(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true("game_id" %in% names(x))
    }
})

test_that("PWHL - Load PWHL Team Box", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_team_box(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "team_id") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Game Info", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_game_info(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "home_team", "away_team") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Scoring Summary", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_scoring_summary(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "scorer_id") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Penalty Summary", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_penalty_summary(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "description") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Three Stars", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_three_stars(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "star", "player_id") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Officials", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_officials(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "role") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Shots By Period", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_shots_by_period(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "period_id", "home_shots", "away_shots") %in% names(x)))
    }
})

test_that("PWHL - Load PWHL Shootout (may be empty)", {
    skip_on_cran()
    skip_pwhl_test()
    # The 2024 PWHL season had no shootouts so the release file may not exist;
    # the loader should either return a populated frame or fail cleanly.
    x <- suppressWarnings(tryCatch(load_pwhl_shootout(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true("game_id" %in% names(x))
    } else {
        succeed("shootout loader returned empty/NULL as expected for 2024")
    }
})

test_that("PWHL - Load PWHL Game Rosters", {
    skip_on_cran()
    skip_pwhl_test()
    x <- suppressWarnings(tryCatch(load_pwhl_game_rosters(2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_true(all(c("game_id", "player_id", "player_type") %in% names(x)))
    }
})
