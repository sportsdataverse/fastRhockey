test_that("NHL Records - Award Details", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_award_details()
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})

test_that("NHL Records - Award Details (by season)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_records_award_details(season_id = 20232024)
    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})
