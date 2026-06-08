test_that("ESPN - Get ESPN NHL season teams collection", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_teams(season = 2026, limit = 100)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season teams endpoint at test time")
  }

  key_cols <- c("ref", "team_id", "season", "count", "page_count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(is.character(x$team_id))
  # NHL has 32 active teams; with limit=100 all should come in one page
  expect_true(x$count[1] >= 30L)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season team singular", {
  skip_on_cran()
  skip_espn_test()

  # Derive team_id live from the teams collection
  tms <- espn_nhl_season_teams(season = 2026, limit = 10)
  if (!is.data.frame(tms) || nrow(tms) == 0) {
    skip("Could not retrieve season teams to derive team_id")
  }
  tid <- tms$team_id[1]

  x <- espn_nhl_season_team(season = 2026, team_id = tid)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season team endpoint at test time")
  }

  key_cols <- c(
    "season", "team_id", "id", "uid", "slug", "location", "name",
    "abbreviation", "display_name", "is_active",
    "team_ref", "record_ref", "athletes_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$season, 2026L)
  expect_equal(x$team_id, as.character(tid))
  expect_true(is.logical(x$is_active))

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season athletes (paginated collection)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_athletes(season = 2026, limit = 10, page = 1)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season athletes endpoint at test time")
  }

  key_cols <- c("ref", "athlete_id", "season", "page", "count", "page_count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(all(x$page == 1L))
  expect_true(is.character(x$athlete_id))
  # NHL season has thousands of athletes across many pages
  expect_true(x$count[1] > 100L)
  expect_true(x$page_count[1] > 10L)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season coaches collection", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_coaches(season = 2026, limit = 100)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season coaches endpoint at test time")
  }

  key_cols <- c("ref", "coach_id", "season", "count", "page_count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(is.character(x$coach_id))
  # NHL has ~30 head coaches per season
  expect_true(x$count[1] >= 20L)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season draft (DONE_WITH_CONCERNS: 404 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # This endpoint 404s for NHL; function should return an empty data.frame
  x <- espn_nhl_season_draft(season = 2025)

  # Either an empty data.frame or a graceful return is acceptable
  expect_true(is.data.frame(x))
  # If data returned, season column should be present
  if (nrow(x) > 0) {
    expect_true("season" %in% colnames(x))
  }

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season draft round picks (DONE_WITH_CONCERNS: 404 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # Confirmed path: seasons/{season}/draft/rounds/{round_num}/picks
  # Returns HTTP 404 for NHL; function should return an empty data.frame
  x <- espn_nhl_season_draft_round_picks(season = 2025, round_num = 1)

  expect_true(is.data.frame(x))
  if (nrow(x) > 0) {
    key_cols <- c("ref", "pick_id", "season", "round_num")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2025L))
    expect_true(all(x$round_num == 1L))
  }

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season futures", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_futures(season = 2026)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season futures endpoint at test time")
  }

  key_cols <- c(
    "future_ref", "future_id", "future_name", "future_type",
    "future_display_name", "provider_id", "provider_name",
    "provider_active", "provider_priority", "season"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(is.character(x$future_id))
  expect_true(is.character(x$future_name))
  # Spot-check: Stanley Cup should be among the futures markets
  expect_true(any(grepl("Stanley Cup", x$future_name, ignore.case = TRUE)))

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season freeagents (DONE_WITH_CONCERNS: empty for NHL)", {
  skip_on_cran()
  skip_espn_test()

  # NHL returns count=0 for freeagents; function should return an empty data.frame
  x <- espn_nhl_season_freeagents(season = 2026)

  expect_true(is.data.frame(x))
  # If data returned (unlikely for NHL), season column should be present
  if (nrow(x) > 0) {
    key_cols <- c("ref", "athlete_id", "season")
    expect_in(sort(key_cols), sort(colnames(x)))
    expect_true(all(x$season == 2026L))
  }

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season type leaders", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_type_leaders(season = 2026, season_type = 2)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season type leaders endpoint at test time")
  }

  key_cols <- c(
    "category_name", "category_display_name", "category_abbreviation",
    "display_value", "value", "rel",
    "athlete_ref", "team_ref", "statistics_ref",
    "season", "season_type"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(all(x$season_type == 2L))
  expect_true(is.numeric(x$value))

  # NHL should have well-known stat categories
  cats_present <- unique(x$category_name)
  expect_true(any(c("goals", "assists", "points") %in% cats_present))

  # Each leader row should have an athlete_ref
  non_na_ath <- x$athlete_ref[!is.na(x$athlete_ref)]
  expect_true(length(non_na_ath) >= 1L)
  expect_true(all(grepl("athletes", non_na_ath)))

  Sys.sleep(1)
})
