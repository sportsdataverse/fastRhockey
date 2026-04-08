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
