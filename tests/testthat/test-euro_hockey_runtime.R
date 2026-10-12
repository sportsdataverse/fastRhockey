# Offline tests for the shared European hockey runtime (R/euro_hockey_runtime.R)
# and the classed conditions / empty-tibble helper (R/utils.R).

json_resp <- function(body, status = 200L, url = "https://example.test/x.json") {
  httr2::response(status_code = status, url = url,
                  headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(body))
}

# ---- throttle --------------------------------------------------------------

test_that(".polite_wait spaces requests to one host and leaves other hosts alone", {
  skip_on_cran()
  first <- .polite_wait("https://wait-a.test/one", min_interval = 0.4)
  second <- .polite_wait("https://wait-a.test/two?x=1", min_interval = 0.4)
  other <- .polite_wait("https://wait-b.test/one", min_interval = 0.4)
  expect_equal(first, 0)
  expect_gt(second, 0.3)
  expect_equal(other, 0)
})

test_that(".polite_perform waits on every attempt, retries included, and the next call waits after the retry", {
  skip_on_cran()
  seen <- numeric()
  statuses <- c(503L, 200L, 200L)
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1]] <<- proc.time()[["elapsed"]]
    json_resp("{}", statuses[[length(seen)]], req$url)
  })
  req <- httr2::request("https://retry.test/a.json")
  resp <- .polite_perform(req, min_interval = 0.5)
  expect_equal(httr2::resp_status(resp), 200L)
  .polite_perform(req, min_interval = 0.5)
  expect_length(seen, 3)
  gaps <- diff(seen)
  expect_gte(gaps[[1]], 0.95) # retry backoff: at least 1 s after the failed attempt
  expect_gte(gaps[[2]], 0.45) # the retry's slot counts: the next call still waits
})

test_that(".polite_perform returns the last 429 after max_tries for the caller to classify", {
  skip_on_cran()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    json_resp("{}", 429L, req$url)
  })
  resp <- .polite_perform(httr2::request("https://limit.test/a.json"), max_tries = 2, min_interval = 0)
  expect_equal(calls, 2)
  expect_error(.euro_resp_json(resp), class = "fastRhockey_asset_fetch")
})

test_that("a connection failure after retries is fastRhockey_asset_fetch", {
  skip_on_cran()
  req <- httr2::req_timeout(httr2::request("http://127.0.0.1:9/never.json"), 2)
  expect_error(.polite_perform(req, max_tries = 2, min_interval = 0),
               class = "fastRhockey_asset_fetch")
})

# ---- conditions --------------------------------------------------------------

test_that("the status vocabulary maps to the classed conditions", {
  skip_on_cran()
  u <- "https://status.test/path.json?apikey=SECRET"
  expect_invisible(.euro_check_status(json_resp("{}", 200L, u)))
  e404 <- expect_error(.euro_check_status(json_resp("{}", 404L, u)), class = "fastRhockey_no_data")
  expect_s3_class(e404, "fastRhockey_error")
  expect_no_match(conditionMessage(e404), "SECRET") # the query string is never quoted
  expect_match(conditionMessage(e404), "status.test/path.json")
  expect_error(.euro_check_status(json_resp("{}", 400L, u)), class = "fastRhockey_bad_request")
  expect_error(.euro_check_status(json_resp("{}", 422L, u)), class = "fastRhockey_bad_request")
  expect_error(.euro_check_status(json_resp("{}", 403L, u)), class = "fastRhockey_asset_fetch")
  expect_error(.euro_check_status(json_resp("{}", 500L, u)), class = "fastRhockey_asset_fetch")
})

test_that(".euro_resp_json refuses empty and non-JSON bodies and passes 204 as list()", {
  skip_on_cran()
  expect_equal(.euro_resp_json(json_resp('{"a": 1}')), list(a = 1L))
  expect_equal(.euro_resp_json(json_resp("", 204L)), list())
  expect_error(.euro_resp_json(json_resp("  ")), class = "fastRhockey_asset_fetch")
  empty <- httr2::response(status_code = 200L, url = "https://get.test/a.json")  # no body at all
  expect_error(.euro_resp_json(empty), class = "fastRhockey_asset_fetch")
  expect_error(.euro_resp_json(json_resp("<html>blocked</html>")), class = "fastRhockey_asset_fetch")
})

test_that(".euro_get drops NULL query values and parses the body", {
  skip_on_cran()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req$url
    json_resp('[{"id": 7}]', 200L, req$url)
  })
  out <- .euro_get("https://get.test/a.json", query = list(season = 2026, phase = NULL))
  expect_equal(seen, "https://get.test/a.json?season=2026")
  expect_equal(out$id, 7L)
})

test_that("the abort helpers interpolate in the caller's frame and carry both classes", {
  skip_on_cran()
  f <- function(game) .abort_no_data("game {game} has no events")
  e <- expect_error(f(42), class = "fastRhockey_no_data")
  expect_match(conditionMessage(e), "game 42 has no events")
  expect_error(.abort_asset_fetch("x"), class = "fastRhockey_error")
  expect_error(.abort_bad_request("x"), class = "fastRhockey_bad_request")
})

# ---- empty tibble / season -----------------------------------------------------

test_that(".empty_tibble returns zero rows with the documented types", {
  skip_on_cran()
  out <- .empty_tibble(c(match_id = "integer", name = "character", toi = "double",
                         ok = "logical", date = "Date", ts = "POSIXct"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("match_id", "name", "toi", "ok", "date", "ts"))
  expect_type(out$match_id, "integer")
  expect_type(out$toi, "double")
  expect_s3_class(out$date, "Date")
  expect_s3_class(out$ts, "POSIXct")
  expect_error(.empty_tibble(c(x = "factor")), class = "fastRhockey_bad_request")
})

test_that(".season_start_year maps an END year to the START year", {
  skip_on_cran()
  expect_identical(.season_start_year(2027), 2026L)
  expect_identical(.season_start_year("2027"), 2026L)
  expect_error(.season_start_year(27), class = "fastRhockey_bad_request")
  expect_error(.season_start_year("2026-27"), class = "fastRhockey_bad_request")
})
