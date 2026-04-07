test_that("NHL Stats - Get Draft Summaries (all)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_draft()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
    expect_true("draft_year" %in% names(x))
})

test_that("NHL Stats - Get Draft Summaries (specific year)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_draft(draft_year = 2024)

    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), 1)
    expect_equal(x$draft_year[1], 2024)
})

test_that("NHL Stats - Get Seasons", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_seasons()

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})

test_that("NHL Stats - Get Glossary", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_misc("glossary")

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)
})

test_that("NHL Stats - Get Shift Charts", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_misc("shiftcharts", game_id = 2024020001)

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})

test_that("NHL Stats - Get Component Season", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_stats_misc("componentSeason")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)
})
