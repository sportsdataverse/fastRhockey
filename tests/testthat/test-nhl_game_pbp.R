test_that("NHL - Get Game PBP (v1 wrapper)", {
    skip_on_cran()
    skip_nhl_test()
    x <- suppressWarnings(nhl_game_pbp(game_id = 2023020001))

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_true(nrow(x) > 0)
    }
})

test_that("NHL - nhl_game_pbp fills game_id on shift CHANGE rows (offline)", {
    # Game 2025021229: the play-by-play JSON and nhl_game_shifts() output,
    # captured from the NHL API, stand in for the two network calls.
    pbp_json <- readRDS(test_path("fixtures", "nhl", "pbp_2025021229.rds"))
    shifts <- readRDS(test_path("fixtures", "nhl", "shifts_2025021229.rds"))
    local_mocked_bindings(
        .retry_request = function(...) NULL,
        check_status = function(...) invisible(NULL),
        .resp_text = function(...) pbp_json,
        nhl_game_shifts = function(...) shifts
    )

    pbp <- suppressMessages(nhl_game_pbp(game_id = 2025021229))
    expect_s3_class(pbp, "data.frame")
    expect_s3_class(pbp, "fastRhockey_data")

    # Every captured shift row made it in, so integration really ran.
    expect_equal(sum(pbp$event_type == "CHANGE"), nrow(shifts))
    expect_false(anyNA(pbp$game_id))
    expect_true(all(pbp$game_id == 2025021229L))
    expect_type(pbp$game_id, "integer")
})
