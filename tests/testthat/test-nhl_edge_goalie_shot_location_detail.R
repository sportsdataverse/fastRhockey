test_that("NHL - Edge Goalie Shot Location Detail (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_goalie_shot_location_detail(player_id = 8475883)  # Andrei Vasilevskiy
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Edge Goalie Shot Location Detail (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_goalie_shot_location_detail(
        player_id = 8475883,
        season = 2024,
        game_type = 2
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
    }
})
