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
