# Cross-language parity tests: R analytics vs sdv-py on shared fixture game 42.
# Reference values were computed from the Python implementation on identical
# fixtures and are exact. Do NOT change these numbers; fix the R code if they
# diverge.

.load_fx <- function(stem) {
  jsonlite::read_json(testthat::test_path("fixtures", "hockeytech", paste0(stem, ".json")))
}

test_that("R player TOI matches sdv-py on the shared fixture (game 42)", {
  sh  <- fastRhockey:::.parse_hockeytech_shifts(.load_fx("pwhl_gameshifts_42"), game_id = 42)
  toi <- fastRhockey:::hockeytech_player_toi(sh)
  expect_equal(nrow(toi), 39)
  expect_equal(sum(toi$toi_seconds), 40824)
  expect_equal(toi$toi_seconds[match(85, toi$player_id)], 3584)
  expect_equal(toi$toi_seconds[match(6,  toi$player_id)], 2400)
  expect_equal(toi$toi_seconds[match(83, toi$player_id)], 1534)
})

test_that("R team corsi/fenwick matches sdv-py on the shared fixture (game 42)", {
  pbp <- fastRhockey:::.parse_hockeytech_pbp(
    .load_fx("pwhl_pbp_42"), pbp_style = "hockeytech_a", game_id = 42
  )
  cf <- fastRhockey:::hockeytech_corsi_fenwick(pbp)
  expect_equal(sort(cf$corsi_for), c(46, 49))
  expect_equal(sort(cf$fenwick_for), c(32, 38))
  expect_false(any(cf$corsi_includes_missed))
})

test_that("R base pbp event counts match sdv-py (game 42)", {
  pbp <- fastRhockey:::.parse_hockeytech_pbp(
    .load_fx("pwhl_pbp_42"), pbp_style = "hockeytech_a", game_id = 42
  )
  counts <- table(pbp$event)
  expect_equal(as.integer(counts[["shot"]]),         66)
  expect_equal(as.integer(counts[["blocked_shot"]]), 25)
  expect_equal(as.integer(counts[["goal"]]),          4)
  expect_equal(as.integer(counts[["faceoff"]]),      48)
  expect_equal(as.integer(counts[["penalty"]]),       5)
  expect_equal(as.integer(counts[["hit"]]),          21)
})
