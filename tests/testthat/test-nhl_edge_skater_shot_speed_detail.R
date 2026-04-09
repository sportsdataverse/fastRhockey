test_that("NHL - Edge Skater Shot Speed Detail (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_skater_shot_speed_detail(player_id = 8478402)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Edge Skater Shot Speed Detail (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_skater_shot_speed_detail(
        player_id = 8478402,
        season = 2024,
        game_type = 2
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
    }
})
