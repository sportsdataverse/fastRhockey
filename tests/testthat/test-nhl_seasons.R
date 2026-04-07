test_that("NHL - Get Seasons List", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_seasons()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("season_id" %in% names(x))
})

test_that("NHL - Get Draft Rankings (current)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_draft_rankings()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("last_name" %in% names(x))
})

test_that("NHL - Get Draft Rankings (specific year)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_draft_rankings(season = 2024, category = 1)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})
