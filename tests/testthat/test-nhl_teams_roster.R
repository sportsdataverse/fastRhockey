test_that("NHL - Get NHL Team Roster", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_teams_roster(team_abbr = "TOR")

    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) > 0)

    expected_cols <- c(
        "player_id",
        "first_name",
        "last_name",
        "sweater_number",
        "position_code"
    )
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})

test_that("NHL - Team Roster returns fastRhockey_data class", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_teams_roster(team_abbr = "EDM")
    expect_s3_class(x, "fastRhockey_data")
})
