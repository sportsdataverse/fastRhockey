test_that("NHL - Get NHL Teams", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_teams()

    expect_s3_class(x, "data.frame")
    # Expect 32 NHL teams (or at least close to it)
    expect_true(nrow(x) >= 30)

    expected_cols <- c("team_name", "team_abbr")
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})

test_that("NHL - Teams returns fastRhockey_data class", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_teams()
    expect_s3_class(x, "fastRhockey_data")
})
