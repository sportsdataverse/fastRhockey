# .retry_request() turns off httr2's status errors, so a shared helper that
# parsed the body straight away would read an error reply as data.
# .hockeytech_api() (every PWHL and AHL/OHL/WHL/QMJHL wrapper) checks the
# status with .fastRhockey_stop_for_http_status() first.

mock_reply <- function(status, body = "", type = "application/json") {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = status, url = req$url,
                    headers = list(`Content-Type` = type), body = charToRaw(body))
  }, env = parent.frame())
}

test_that(".fastRhockey_stop_for_http_status() passes 2xx and classes the rest", {
  ok <- httr2::response(status_code = 200L, url = "https://x.test/a")
  expect_identical(.fastRhockey_stop_for_http_status(ok), ok)
  expect_silent(.fastRhockey_stop_for_http_status(httr2::response(status_code = 204L)))

  e <- expect_error(
    .fastRhockey_stop_for_http_status(httr2::response(
      status_code = 429L, url = "https://x.test/feed/index.php?key=SECRET",
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw('{"message":"Too many requests"}')
    )),
    class = "fastRhockey_http_error"
  )
  expect_s3_class(e, c("fastRhockey_asset_fetch", "fastRhockey_error"))
  expect_identical(e$status, 429L)
  expect_match(conditionMessage(e), "HTTP 429 Too Many Requests from https://x.test/feed/index.php: Too many requests", fixed = TRUE)
  expect_false(grepl("SECRET", conditionMessage(e), fixed = TRUE))

  expect_error(.fastRhockey_stop_for_http_status(httr2::response(status_code = 404L)),
               class = "fastRhockey_no_data")
  expect_error(.fastRhockey_stop_for_http_status(httr2::response(status_code = 422L)),
               class = "fastRhockey_bad_request")
})

test_that(".hockeytech_api() raises on a non-2xx reply instead of parsing its body", {
  # a JSONP error body parses fine, so it used to come back as data
  mock_reply(503L, 'angular.callbacks._0({"error":"Service unavailable"})')
  e <- expect_error(.hockeytech_api("https://lscluster.hockeytech.com/feed/index.php?key=SECRET&callback=angular.callbacks._0"),
                    class = "fastRhockey_http_error")
  expect_identical(e$status, 503L)
  expect_match(conditionMessage(e), "HTTP 503", fixed = TRUE)
})

test_that(".hockeytech_api() still parses a 200 JSONP reply", {
  mock_reply(200L, 'angular.callbacks._0({"teams":[{"id":"1"}]})\n')
  expect_identical(.hockeytech_api("https://lscluster.hockeytech.com/feed/index.php?callback=angular.callbacks._0"),
                   list(teams = list(list(id = "1"))))
})

test_that("a PWHL wrapper reports a non-2xx reply instead of returning it as rows", {
  # a body the wrapper would otherwise read as one team
  mock_reply(500L, paste0(
    'angular.callbacks._4({"teamsNoAll":[{"id":"1","name":"PWHL Boston",',
    '"team_code":"BOS","nickname":"Boston","division_id":"1","logo":"x"}]})'
  ))
  msgs <- character()
  out <- withCallingHandlers(
    pwhl_teams(),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(nrow(out), 0L)
  expect_true(any(grepl("HTTP 500", msgs, fixed = TRUE)))
})

test_that("a 404 on the game summary or shifts leaves the play-by-play enrichment NA", {
  # HockeyTech answers an unknown game with 404 {"error": "No such game"}
  mock_reply(404L, '{"error":"No such game"}')
  out <- hockeytech_enrich_pbp(data.frame(event = character()), "pwhl", 42)
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("the power-play back-fill keeps the feed's flags when team ids are unknown", {
  # what hockeytech_enrich_pbp() builds after a 404 on the game summary:
  # home_team_id / away_team_id are ""
  pbp <- data.frame(
    event = c("penalty", "shot", "shot"),
    team_id = c("1", "1", "2"),
    home_team_id = "", away_team_id = "",
    sec_from_start = c(100, 150, 160),
    penalty_length = c("2", NA, NA),
    power_play = c("1", "0", "0"),
    short_handed = c(NA, "0", "0"),
    stringsAsFactors = FALSE
  )
  out <- hockeytech_backfill_power_play(pbp)
  expect_identical(out$power_play, pbp$power_play)
  expect_identical(out$short_handed, pbp$short_handed)
})
