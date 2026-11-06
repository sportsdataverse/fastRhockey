# Offline: a failed request must give the documented NULL plus a message,
# not "object '<result>' not found" from an uninitialised return variable.
fail_request <- function(...) stop("simulated connection failure")

test_that("nhl_teams_stats() returns NULL when the request fails", {
  local_mocked_bindings(.retry_request = fail_request)
  expect_message(res <- nhl_teams_stats(team_abbr = "TBL"), "no team stats")
  expect_null(res)
})

test_that("pwhl_player_box() returns NULL when the request fails", {
  local_mocked_bindings(.retry_request = fail_request)
  expect_message(res <- pwhl_player_box(game_id = 27), "no player box")
  expect_null(res)
})

test_that("nhl_player_stats() returns NULL when the request fails", {
  local_mocked_bindings(.retry_request = fail_request)
  expect_no_error(res <- suppressMessages(nhl_player_stats(player_id = 8476899)))
  expect_null(res)
})

test_that("nhl_game_content() returns NULL when the request fails", {
  local_mocked_bindings(.retry_request = fail_request)
  expect_no_error(res <- suppressMessages(nhl_game_content(game_id = 2024020001)))
  expect_null(res)
})

test_that("pwhl_standings() returns NULL when the response can't be parsed", {
  local_mocked_bindings(
    .pwhl_resolve_season_id = function(...) 5L,
    .hockeytech_api = function(...) list(list(sections = list(list(data = list(list(row = NULL))))))
  )
  expect_message(res <- pwhl_standings(season = 2024), "no roster data")
  expect_null(res)
})
