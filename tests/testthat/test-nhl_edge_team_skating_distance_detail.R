test_that("NHL - Edge Team Skating Distance Detail (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_team_skating_distance_detail(team_id = 10)  # Toronto Maple Leafs
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Edge Team Skating Distance Detail (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_team_skating_distance_detail(
        team_id = 10,
        season = 2024,
        game_type = 2
    )
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
    }
})
