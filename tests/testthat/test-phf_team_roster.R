test_that("phf_team_roster", {
  skip_on_cran()
  cols_1 <- c(
    "team_id",
    "team_name",
    "group",
    "division_id",
    "player_jersey",
    "player_name",
    "position",
    "height",
    "hometown",
    "player_image_href",
    "player_href",
    "player_id"
  )
  cols_2 <- c(
    "team_id",
    "team_name",
    "group",
    "division_id",
    "staff_name",
    "staff_type"
  )
  x <- phf_team_roster(team = "Minnesota Whitecaps", season = 2023)
  z <- phf_team_roster(team = "Buffalo Beauts", season = 2023)

  expect_equal(colnames(x$roster), cols_1)
  expect_equal(colnames(x$team_staff), cols_2)
  expect_equal(colnames(z$roster), cols_1)
  expect_equal(colnames(z$team_staff), cols_2)

  expect_s3_class(x$roster, "data.frame")
  expect_s3_class(x$team_staff, "data.frame")
  expect_s3_class(z$roster, "data.frame")
  expect_s3_class(z$team_staff, "data.frame")
})
