test_that("NHL - Get PPT Replay (goal)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_ppt_replay_goal(game_id = 2023020001, event_number = 1)

    if (!is.null(x)) {
        expect_type(x, "list")
    }
})
