test_that("ESPN NHL Player Splits returns valid wide tibble", {
  skip_on_cran()
  skip_espn_test()

  # Derive a live athlete_id from the roster
  roster <- tryCatch(espn_nhl_team_roster(team_id = "4"), error = function(e) data.frame())
  if (!is.data.frame(roster) || nrow(roster) == 0 || !"id" %in% colnames(roster)) {
    skip("Could not obtain a live athlete_id from ESPN NHL team roster")
  }
  athlete_id <- as.character(roster$id[1])

  Sys.sleep(1)

  x <- espn_nhl_player_splits(athlete_id = athlete_id, season = 2025)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_player_splits at test time")
  }

  key_cols <- c(
    "athlete_id",
    "season",
    "split_category",
    "split_display_name",
    "games",
    "goals",
    "assists",
    "points"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(all(x$athlete_id == athlete_id))
  expect_true(all(x$season == 2025L))

  Sys.sleep(1)
})
