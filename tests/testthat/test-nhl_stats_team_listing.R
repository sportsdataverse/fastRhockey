test_that("NHL - Stats Team Listing (league-wide)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_team_listing()
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})

test_that("NHL - Stats Team Listing (by id)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_team_listing(team_id = 10)
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
