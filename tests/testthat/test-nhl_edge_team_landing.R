test_that("NHL - Edge Team Landing (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_team_landing()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
    }
})

test_that("NHL - Edge Team Landing (specific season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_edge_team_landing(season = 2024, game_type = 2)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
    }
})
