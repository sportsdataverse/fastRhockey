test_that("NHL - helper_nhl_calculate_xg adds xg column", {
    skip_on_cran()
    skip_on_ci()
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
    pbp <- nhl_game_pbp(game_id = 2024020001)
    result <- helper_nhl_calculate_xg(pbp)

    expect_true("xg" %in% names(result))
    expect_equal(nrow(result), nrow(pbp))
})

test_that("NHL - helper_nhl_calculate_xg values are in [0, 1]", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    skip_if_not_installed("xgboost")

    cache_dir <- tools::R_user_dir("fastRhockey", "cache")
    model_file <- file.path(cache_dir, "xg_model_5v5.json")
    skip_if(
        !file.exists(model_file),
        message = "xG models not cached; run refresh_xg_models() first"
    )

    pbp <- nhl_game_pbp(game_id = 2024020001)
    result <- helper_nhl_calculate_xg(pbp)

    xg_vals <- result$xg[!is.na(result$xg)]
    expect_true(length(xg_vals) > 0, info = "No non-NA xg values produced")
    expect_true(
        all(xg_vals >= 0 & xg_vals <= 1),
        info = "xg values should be probabilities in [0, 1]"
    )
})

test_that("NHL - helper_nhl_calculate_xg only fills shot events", {
    skip_on_cran()
    skip_on_ci()
    skip_nhl_test()
    skip_if_not_installed("xgboost")

    cache_dir <- tools::R_user_dir("fastRhockey", "cache")
    model_file <- file.path(cache_dir, "xg_model_5v5.json")
    skip_if(
        !file.exists(model_file),
        message = "xG models not cached; run refresh_xg_models() first"
    )

    pbp <- nhl_game_pbp(game_id = 2024020001)
    result <- helper_nhl_calculate_xg(pbp)

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
