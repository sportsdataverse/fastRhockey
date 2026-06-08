# Tests for Tier 5b: ESPN NHL core-v2 game-event detail wrappers.
# endpoints: game_plays, game_odds, game_probabilities, game_situation,
#            game_officials, game_broadcasts, game_predictor, game_powerindex,
#            game_leaders, game_scoringplays
#
# Live-API tests; gated by skip_espn_test() + skip_on_cran().
# probabilities / predictor / powerindex / leaders are sparse for NHL and may
# return empty — those tests use skip-if-empty guards.

# Shared event_id helper (resolved once at the top of each test for simplicity)
.get_test_event_id <- function() {
  sb <- espn_nhl_scoreboard(dates = "20250110")
  if (is.null(sb) || !is.data.frame(sb) || nrow(sb) == 0 ||
      !"game_id" %in% colnames(sb)) {
    return(NULL)
  }
  sb$game_id[1]
}


# ---------------------------------------------------------------------------
# 1. Game Plays
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Plays (core-v2 play-by-play)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_plays(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No plays returned at test time")
  }

  core_cols <- c("event_id", "cid", "id", "sequence_number", "type_text",
                 "text", "period_number", "away_score", "home_score",
                 "scoring_play", "wallclock")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  expect_true(all(x$event_id == as.character(event_id)))
  # Must have at least one scoring play in a completed game
  expect_true(any(x$scoring_play, na.rm = TRUE))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 2. Game Odds
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Odds (core-v2)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_odds(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No odds returned at test time")
  }

  core_cols <- c("event_id", "cid", "provider_id", "provider_name",
                 "details", "over_under")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$event_id == as.character(event_id)))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 3. Game Probabilities (sparse for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Probabilities (core-v2, sparse for NHL)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_probabilities(event_id = event_id)

  # Sparse endpoint — may return empty; that is expected behavior
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("Probabilities endpoint is sparse/empty for NHL — expected")
  }

  core_cols <- c("event_id", "cid", "sequence_number",
                 "home_win_percentage", "away_win_percentage")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(all(x$event_id == as.character(event_id)))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 4. Game Situation
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Situation (core-v2)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_situation(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No situation data returned at test time")
  }

  core_cols <- c("event_id", "cid", "power_play", "empty_net",
                 "last_play_ref", "situation_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(as.character(x$event_id[1]), as.character(event_id))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 5. Game Officials
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Officials (core-v2)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_officials(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No officials returned at test time")
  }

  core_cols <- c("event_id", "cid", "official_id", "full_name", "display_name",
                 "position_name", "official_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  # NHL games have 4 officials (2 referees + 2 linesmen)
  expect_true(nrow(x) >= 2L)
  expect_true(all(x$event_id == as.character(event_id)))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 6. Game Broadcasts
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Broadcasts (core-v2)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_broadcasts(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No broadcasts returned at test time")
  }

  core_cols <- c("event_id", "cid", "station", "slug",
                 "type_short_name", "media_name")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$event_id == as.character(event_id)))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 7. Game Predictor (sparse for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Predictor (core-v2, sparse for NHL)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_predictor(event_id = event_id)

  # Sparse endpoint — empty is expected for NHL
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("Predictor endpoint is sparse/empty for NHL — expected")
  }

  expect_in(c("event_id", "cid"), colnames(x))
  expect_s3_class(x, "data.frame")

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 8. Game Power Index (sparse for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Power Index (core-v2, sparse for NHL)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_powerindex(event_id = event_id)

  # Sparse endpoint — empty is expected for NHL
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("Power Index endpoint is sparse/empty for NHL — expected")
  }

  expect_in(c("event_id", "cid"), colnames(x))
  expect_s3_class(x, "data.frame")

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 9. Game Leaders (sparse for NHL)
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Leaders (core-v2, sparse for NHL)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_leaders(event_id = event_id)

  # Sparse endpoint — empty is expected for NHL
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("Game Leaders endpoint is sparse/empty for NHL — expected")
  }

  core_cols <- c("event_id", "cid", "category_name", "value",
                 "display_value", "athlete_ref")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(all(x$event_id == as.character(event_id)))

  Sys.sleep(1)
})


# ---------------------------------------------------------------------------
# 10. Game Scoring Plays (filtered from game_plays since /scoringplays 404s)
# ---------------------------------------------------------------------------
test_that("ESPN NHL Game Scoring Plays (filtered from game_plays)", {
  skip_on_cran()
  skip_espn_test()

  event_id <- .get_test_event_id()
  if (is.null(event_id)) skip("Could not retrieve scoreboard to derive event_id")

  x <- espn_nhl_game_scoringplays(event_id = event_id)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No scoring plays returned at test time")
  }

  core_cols <- c("event_id", "cid", "id", "type_text", "period_number",
                 "away_score", "home_score", "scoring_play")
  expect_in(sort(core_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  # All returned plays should be scoring plays
  expect_true(all(x$scoring_play == TRUE, na.rm = TRUE))
  expect_true(all(x$event_id == as.character(event_id)))
  # A typical NHL game has 4-8 goals
  expect_true(nrow(x) >= 1L)

  Sys.sleep(1)
})
