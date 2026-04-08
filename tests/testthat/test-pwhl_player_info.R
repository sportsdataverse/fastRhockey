test_that("PWHL - Get PWHL Player Info", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_player_info(player_id = 28)

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "player_id",
        "first_name",
        "last_name",
        "position",
        "birthdate"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }

    expect_equal(x$player_id[1], 28)
})
