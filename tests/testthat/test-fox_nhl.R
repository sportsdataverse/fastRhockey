# Live tests for the Fox Sports (Bifrost) NHL wrappers.
# Stable ids captured 2026-06-10: event 44398, team 1. Subset-direction column
# checks; skip-if-empty guards for transient API errors + ephemeral odds.

test_that("Fox NHL play-by-play", {
  skip_on_cran()
  x <- fox_nhl_pbp("44398")
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No Fox NHL pbp at test time")
  expect_in(c("game_id", "period", "play_id", "clock", "play_text", "team"), colnames(x))
  expect_s3_class(x, "data.frame")
  Sys.sleep(1)
})

test_that("Fox NHL boxscore", {
  skip_on_cran()
  x <- fox_nhl_boxscore("44398")
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) skip("No Fox NHL boxscore at test time")
  expect_in(c("game_id", "team", "stat_group", "player", "stat", "value"), colnames(x))
  Sys.sleep(1)
})

test_that("Fox NHL team roster + stats + gamelog", {
  skip_on_cran()
  ro <- fox_nhl_team_roster("1")
  if (is.null(ro) || !is.data.frame(ro) || nrow(ro) == 0) skip("No Fox NHL roster at test time")
  expect_in(c("team_id", "position_group", "player", "athlete_id"), colnames(ro))
  Sys.sleep(1)
  st <- fox_nhl_team_stats("1")
  if (!is.null(st) && nrow(st) > 0) expect_in(c("team_id", "category", "stat", "player", "value"), colnames(st))
  Sys.sleep(1)
  gl <- fox_nhl_team_gamelog("1")
  if (!is.null(gl) && nrow(gl) > 0) expect_in(c("team_id", "category", "game_id", "stat", "value"), colnames(gl))
  Sys.sleep(1)
})

test_that("Fox NHL standings + leaders + odds", {
  skip_on_cran()
  sd <- fox_nhl_standings("1")
  if (!is.null(sd) && nrow(sd) > 0) expect_in(c("team_id", "section", "entity_id"), colnames(sd))
  Sys.sleep(1)
  ll <- fox_nhl_league_leaders("scoring")
  if (!is.null(ll) && nrow(ll) > 0) expect_in("entity_id", colnames(ll))
  Sys.sleep(1)
  od <- fox_nhl_odds("44398")
  expect_s3_class(od, "data.frame")
  if (!is.null(od) && nrow(od) > 0) expect_in(c("game_id", "team"), colnames(od))
  Sys.sleep(1)
})
