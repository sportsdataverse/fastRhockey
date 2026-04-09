test_that("NHL - Stats Goalie Leaders (savePctg)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_goalie_leaders(attribute = "savePctg")
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})

test_that("NHL - Stats Goalie Leaders (shutouts)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_goalie_leaders(attribute = "shutouts")
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})
