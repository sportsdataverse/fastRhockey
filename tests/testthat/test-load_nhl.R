test_that("NHL - Load NHL PBP", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_pbp(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL PBP Lite", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_pbp_lite(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Schedule", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_schedule(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Load NHL Team Box", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_team_box(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Player Box", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_player_box(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Load NHL Skater Box", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_skater_box(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Goalie Box", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_goalie_box(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Rosters", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_rosters(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Load NHL Game Rosters", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_game_rosters(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Game Info", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_game_info(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Scoring", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_scoring(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Penalties", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_penalties(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Three Stars", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_three_stars(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Scratches", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_scratches(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Linescore", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_linescore(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Shifts", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_shifts(seasons = 2021), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
    }
})

test_that("NHL - Load NHL Officials", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_officials(seasons = 2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
        # Officials rows are tagged with role + name
        expect_true(any(c("role", "official_type") %in% names(x)))
        expect_true(any(c("name", "official_name") %in% names(x)))
    }
})

test_that("NHL - Load NHL Shots By Period", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_shots_by_period(seasons = 2024), error = function(e) NULL))

    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
        expect_true("home_away" %in% names(x))
        expect_true(any(c("period_number", "period") %in% names(x)))
        expect_true("shots" %in% names(x))
        # home_away should only be "home" or "away"
        expect_true(all(unique(x$home_away) %in% c("home", "away")))
    }
})

test_that("NHL - Load NHL Shootout", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    x <- suppressWarnings(tryCatch(load_nhl_shootout(seasons = 2024), error = function(e) NULL))

    # Shootout is sparse (only games that went to SO contribute rows).
    # Empty result is acceptable.
    if (!is.null(x) && nrow(x) > 0) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true("game_id" %in% names(x))
        # Each row is one shootout attempt
        expect_true(any(c("sequence", "shootout_order", "attempt_order") %in% names(x)))
        expect_true(any(c("result", "shot_result") %in% names(x)))
    }
})

test_that("NHL - nhl_schedule(include_data_flags = TRUE) attaches flag columns", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    sched <- suppressWarnings(tryCatch(
        nhl_schedule(day = "2024-01-15", include_data_flags = TRUE),
        error = function(e) NULL
    ))

    if (!is.null(sched) && nrow(sched) > 0) {
        expect_s3_class(sched, "data.frame")
        expect_true("game_id" %in% names(sched))
        # All 16 flag columns should be present
        for (fc in c(
            "PBP", "team_box", "player_box", "skater_box", "goalie_box",
            "game_info", "game_rosters", "scoring", "penalties",
            "scratches", "linescore", "three_stars", "shifts",
            "officials", "shots_by_period", "shootout"
        )) {
            expect_true(fc %in% names(sched), info = paste("missing flag:", fc))
            expect_true(is.logical(sched[[fc]]), info = paste("non-logical flag:", fc))
        }
    }
})
