test_that("NHL - Get NHL Standings", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_standings(date = "2025-01-15")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 30)

    expected_cols <- c(
        "team_name",
        "team_abbr",
        "conference_name",
        "division_name",
        "wins",
        "losses",
        "points",
        "games_played"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})

test_that("NHL - Standings returns fastRhockey_data class", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_standings(date = "2025-01-15")
    expect_s3_class(x, "fastRhockey_data")
})
