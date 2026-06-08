test_that("espn_nhl_news returns articles", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_news(limit = 10)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_news at test time")
  }

  expected_cols <- c("article_id", "headline", "published", "type")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_injuries returns player rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_injuries()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_injuries at test time")
  }

  expected_cols <- c("team_id", "team_display_name", "player_id", "injury_status")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_transactions returns transaction rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_transactions()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_transactions at test time")
  }

  expected_cols <- c("date", "description", "team_id", "team_display_name")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_conferences returns conference/division rows", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_conferences()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_conferences at test time")
  }

  expected_cols <- c("conference_name", "conference_abbreviation",
                     "division_name", "division_abbreviation")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  # NHL has 2 conferences x 2 divisions = 4 rows
  expect_true(nrow(x) >= 2)
  Sys.sleep(1)
})

test_that("espn_nhl_statistics_league returns stat rows or empty gracefully", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_statistics_league()
  expect_s3_class(x, "data.frame")

  if (nrow(x) > 0) {
    expected_cols <- c("category_name", "category_abbreviation")
    expect_in(sort(expected_cols), sort(colnames(x)))
  }
  Sys.sleep(1)
})

test_that("espn_nhl_draft returns empty gracefully (HTTP 500 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_draft()
  # Should return an empty data.frame gracefully (500 for NHL)
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_games returns event refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_games(limit = 10)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_games at test time")
  }

  expected_cols <- c("ref", "event_id", "count", "page_count")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_awards returns award refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_awards()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_awards at test time")
  }

  expected_cols <- c("ref", "award_id", "count")
  expect_in(sort(expected_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  Sys.sleep(1)
})

test_that("espn_nhl_award returns empty gracefully (HTTP 500 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  awards <- espn_nhl_awards()
  award_id_val <- if (is.data.frame(awards) && nrow(awards) > 0 &&
                      "award_id" %in% colnames(awards))
    awards$award_id[1] else "1"

  x <- espn_nhl_award(award_id = award_id_val)
  # Should return empty data.frame gracefully (500 for NHL individual award)
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_tournaments returns tournament refs or empty gracefully", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_tournaments()
  expect_s3_class(x, "data.frame")

  if (nrow(x) > 0) {
    expected_cols <- c("ref", "tournament_id")
    expect_in(sort(expected_cols), sort(colnames(x)))
  }
  Sys.sleep(1)
})

test_that("espn_nhl_talentpicks returns empty gracefully (count=0 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_talentpicks()
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("espn_nhl_league_notes returns empty gracefully (count=0 for NHL)", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_league_notes()
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})
