test_that("NHL - Get NHL Player Info", {
    skip_on_cran()
    skip_nhl_test()
    # Connor McDavid
    x <- nhl_player_info(player_id = 8478402)

    expect_s3_class(x, "data.frame")
    expect_equal(nrow(x), 1)

    expected_cols <- c(
        "player_id",
        "first_name",
        "last_name",
        "full_name",
        "birth_date",
        "birth_city",
        "position",
        "sweater_number",
        "team_abbr",
        "height_inches",
        "weight_pounds"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
    expect_equal(x$player_id[1], 8478402)
})

test_that("NHL - Player Info returns fastRhockey_data class", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_player_info(player_id = 8478402)
    expect_s3_class(x, "fastRhockey_data")
})


# Regression test: nhl_player_info() used ifelse(!is.null(x), x, NA) for
# every nullable nested field. Because the function returns a 1-row tibble,
# the bug was masked — but if any field ever arrives as a length>1 vector
# (or this function is later refactored to handle multiple players), the
# old ifelse would silently truncate to length 1. This test asserts that
# the values returned by nhl_player_info() match the actual API payload
# row-by-row, which would fail if the truncation regressed.
test_that("nhl_player_info: returns values matching the raw API payload", {
    skip_on_cran()
    skip_nhl_test()
    pid <- 8478402  # Connor McDavid
    url <- sprintf(
        "https://api-web.nhle.com/v1/player/%d/landing",
        pid
    )
    raw <- jsonlite::fromJSON(url, flatten = TRUE)

    x <- nhl_player_info(player_id = pid)

    expect_equal(x$player_id, raw$playerId)
    expect_equal(x$first_name, raw$firstName$default)
    expect_equal(x$last_name, raw$lastName$default)
    expect_equal(x$team_abbr, raw$currentTeamAbbrev)
    expect_equal(x$position, raw$position)
    expect_equal(x$birth_country, raw$birthCountry)
    # Type-coercion sanity check: McDavid was the #1 overall pick in 2015.
    expect_equal(x$draft_year, 2015L)
    expect_equal(x$draft_overall, 1L)
})
