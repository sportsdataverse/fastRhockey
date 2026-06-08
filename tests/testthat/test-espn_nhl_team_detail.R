test_that("ESPN - Get ESPN NHL Team Detail (singular)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team(team_id = "4")

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL team endpoint at test time")
  }

  key_cols <- c("team_id", "id", "location", "name", "abbreviation", "display_name", "is_active")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team Record", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_record(team_id = "4")

  # NHL /record endpoint returns {} — graceful empty is the expected outcome
  expect_s3_class(x, "data.frame")
  # If data comes back verify essential columns
  if (nrow(x) > 0) {
    key_cols <- c("team_id")
    expect_in(sort(key_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team Depth Charts", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_depthcharts(team_id = "4")

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL team depthcharts endpoint at test time")
  }

  key_cols <- c("team_id", "season_year", "abbreviation", "display_name")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team Injuries", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_injuries(team_id = "4")

  # NHL /injuries endpoint returns {} — graceful empty is the expected outcome
  expect_s3_class(x, "data.frame")
  if (nrow(x) > 0) {
    key_cols <- c("team_id")
    expect_in(sort(key_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team Transactions", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_transactions(team_id = "4")

  # NHL /transactions endpoint returns {} — graceful empty is the expected outcome
  expect_s3_class(x, "data.frame")
  if (nrow(x) > 0) {
    key_cols <- c("team_id")
    expect_in(sort(key_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team History", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_history(team_id = "4")

  # NHL /history endpoint returns {} — graceful empty is the expected outcome
  expect_s3_class(x, "data.frame")
  if (nrow(x) > 0) {
    key_cols <- c("team_id")
    expect_in(sort(key_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team News", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_news(team_id = "4", limit = 10)

  # NHL /news endpoint returns {} — graceful empty is the expected outcome
  expect_s3_class(x, "data.frame")
  if (nrow(x) > 0) {
    key_cols <- c("team_id", "headline")
    expect_in(sort(key_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team Leaders", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_leaders(team_id = "4")

  # NHL /leaders endpoint returns {} — graceful empty is the expected outcome
  expect_s3_class(x, "data.frame")
  if (nrow(x) > 0) {
    key_cols <- c("team_id", "category_name", "rank", "value")
    expect_in(sort(key_cols), sort(colnames(x)))
  }

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Team Core (core-v2)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_team_core(team_id = "4")

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL team core endpoint at test time")
  }

  key_cols <- c("team_id", "id", "slug", "location", "name", "abbreviation",
                "display_name", "is_active", "athletes_ref")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})

test_that("ESPN - Get ESPN NHL Teams Core (core-v2 collection)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_teams_core(limit = 32)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL teams core endpoint at test time")
  }

  key_cols <- c("ref", "team_id", "count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1)

  Sys.sleep(1)
})
