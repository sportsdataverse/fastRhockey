test_that("NHL - Stats Skater Leaders (assists)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_skater_leaders(attribute = "assists")
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})
