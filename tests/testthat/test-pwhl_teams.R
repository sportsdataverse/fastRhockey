test_that("PWHL - Get PWHL Teams", {
    skip_on_cran()
    skip_pwhl_test()
    x <- pwhl_teams()

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "team_name",
        "team_id",
        "team_code",
        "team_nickname",
        "team_label",
        "division",
        "team_logo"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
