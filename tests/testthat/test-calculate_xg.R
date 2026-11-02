test_that("NHL - helper_nhl_calculate_xg adds xg column", {
    skip_on_cran()
    skip_nhl_test()
    skip_if_not_installed("xgboost")

    # Check that models are available (loaded by zzz.R .onLoad)
    cache_dir <- tools::R_user_dir("fastRhockey", "cache")
    model_file <- file.path(cache_dir, "xg_model_5v5.json")
    skip_if(
        !file.exists(model_file),
        message = "xG models not cached; run refresh_xg_models() first"
    )

    # Get a small PBP to test with
    pbp <- nhl_game_pbp(game_id = 2025021229)
    result <- pbp

    expect_true("xg" %in% names(result))
    expect_equal(nrow(result), nrow(pbp))
})

test_that("NHL - helper_nhl_calculate_xg values are in [0, 1]", {
    skip_on_cran()
    skip_nhl_test()
    skip_if_not_installed("xgboost")

    cache_dir <- tools::R_user_dir("fastRhockey", "cache")
    model_file <- file.path(cache_dir, "xg_model_5v5.json")
    skip_if(
        !file.exists(model_file),
        message = "xG models not cached; run refresh_xg_models() first"
    )

    pbp <- nhl_game_pbp(game_id = 2025021229)
    result <- pbp

    xg_vals <- result$xg[!is.na(result$xg)]
    expect_true(length(xg_vals) > 0, info = "No non-NA xg values produced")
    expect_true(
        all(xg_vals >= 0 & xg_vals <= 1),
        info = "xg values should be probabilities in [0, 1]"
    )
})

test_that("NHL - helper_nhl_calculate_xg only fills shot events", {
    skip_on_cran()
    skip_nhl_test()
    skip_if_not_installed("xgboost")

    cache_dir <- tools::R_user_dir("fastRhockey", "cache")
    model_file <- file.path(cache_dir, "xg_model_5v5.json")
    skip_if(
        !file.exists(model_file),
        message = "xG models not cached; run refresh_xg_models() first"
    )

    pbp <- nhl_game_pbp(game_id = 2025021229)
    result <- pbp

    shot_types <- c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT")
    non_shot_rows <- result[!(result$event_type %in% shot_types), ]

    # Non-shot events should have NA xg
    if (nrow(non_shot_rows) > 0) {
        expect_true(
            all(is.na(non_shot_rows$xg)),
            info = "Non-shot events should have NA xg values"
        )
    }
})

test_that("NHL - helper_nhl_calculate_xg keeps one row per event across games", {
    skip_on_cran()
    skip_if_not_installed("xgboost")
    skip_if(
        is.null(fastRhockey:::.xg_env$xg_model_5v5),
        message = "xG models not loaded; run refresh_xg_models() first"
    )

    # Two real games (2025021229, 2025021230); event_id restarts at 0 in each.
    games <- readRDS(test_path("fixtures", "nhl", "xg_two_games.rds"))
    two <- rbind(games[[1]], games[[2]])
    out <- helper_nhl_calculate_xg(two)

    expect_equal(nrow(out), nrow(two))
    expect_equal(out$event_id, two$event_id)

    single <- helper_nhl_calculate_xg(games[[1]])
    first <- out[seq_len(nrow(games[[1]])), ]
    shot <- which(!is.na(single$xg))[1]
    expect_false(is.na(shot))
    expect_equal(first$xg[shot], single$xg[shot])
    expect_equal(first$xg, single$xg)
})
