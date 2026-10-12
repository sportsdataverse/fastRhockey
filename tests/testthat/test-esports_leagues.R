# Offline tests for the eSports registry and getter (R/esports_leagues.R).

esp_json <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)

test_that("the registry carries sdv-py's four buckets and route sets", {
  skip_on_cran()
  lg <- .esports_leagues()
  expect_setequal(names(lg), c("del", "icehl", "eihl", "metal"))
  expect_equal(vapply(lg, function(x) length(x$routes), integer(1)),
               c(del = 12L, icehl = 12L, eihl = 9L, metal = 8L))
  expect_match(lg$metal$host, "/den\\.hokejovyzapis\\.cz$")
  expect_equal(lg$eihl$league_id, 3L)
  expect_equal(lg$eihl$routes$game_info, "/matches/{match_id}/game-info.json")
  expect_equal(lg$del$routes$game_info, "/matches/{match_id}/game-header.json")
  expect_equal(lg$eihl$routes$standings, "/table/{season}/{league_id}.json")
  expect_equal(lg$del$routes$standings, "/tables/{season}/{league_id}.json")
})

test_that(".esports_get maps the END-year season and builds the bucket URL", {
  skip_on_cran()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req$url
    httr2::response(status_code = 200L, url = req$url,
                    headers = list(`Content-Type` = "application/json"),
                    body = charToRaw('{"matches": [{"season": 2026, "id": 1}]}'))
  })
  body <- .esports_get("eihl", "schedule", season = 2027)
  expect_equal(seen, "https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-matches/2026/3.json")
  expect_length(body$matches, 1)
})

test_that(".esports_get refuses unknown leagues, missing routes and missing ids", {
  skip_on_cran()
  expect_error(.esports_get("nhl", "schedule", season = 2027), class = "fastRhockey_bad_request")
  expect_error(.esports_get("metal", "shots", match_id = 1), class = "fastRhockey_bad_request")
  expect_error(.esports_get("del", "player_box", match_id = 1), class = "fastRhockey_bad_request")
})

test_that(".esports_check applies sdv-py's refusal rules", {
  skip_on_cran()
  h <- "https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz"
  expect_error(.esports_check(paste0(h, "/matches/1/roster.json"), esp_json("[]")), class = "fastRhockey_no_data")
  expect_error(.esports_check(paste0(h, "/matches/1/roster.json"), esp_json("{}")), class = "fastRhockey_no_data")
  expect_error(.esports_check(paste0(h, "/matches/1/roster.json"), esp_json('{"error": "x", "status": 403}')),
               class = "fastRhockey_asset_fetch")
  sched <- paste0(h, "/league-matches/2026/1.json")
  expect_error(.esports_check(sched, esp_json('[{"season": 2026}]')), class = "fastRhockey_asset_fetch")
  expect_error(.esports_check(sched, esp_json('{"matches": [], "x": [1]}')), class = "fastRhockey_no_data")
  expect_error(.esports_check(sched, esp_json('{"matches": [{"season": 2025}]}')), class = "fastRhockey_asset_fetch")
  expect_invisible(.esports_check(sched, esp_json('{"matches": [{"season": 2026}]}')))
  shots <- paste0(h, "/visualization/shots/575.json")
  expect_error(.esports_check(shots, esp_json('{"match": {"shots": [], "status": "LIVE"}}')),
               class = "fastRhockey_no_data")
  expect_error(.esports_check(shots, esp_json('{"match": {"id": 576, "shots": [{"x": 1}]}}')),
               class = "fastRhockey_asset_fetch")
  expect_invisible(.esports_check(shots, esp_json('{"match": {"id": 575, "shots": [{"x": 1}]}}')))
  widget <- paste0(h, "/widget/esports/match/8161.json")
  expect_error(.esports_check(widget, esp_json('{"data": {"gameData": {"id": 1}}}')), class = "fastRhockey_asset_fetch")
  events <- paste0(h, "/matches/1/period-events.json")
  expect_error(.esports_check(events, esp_json('{"1": [], "meta": {"a": 1}}')), class = "fastRhockey_no_data")
  expect_invisible(.esports_check(events, esp_json('{"1": [{"type": "goal"}]}')))
})
