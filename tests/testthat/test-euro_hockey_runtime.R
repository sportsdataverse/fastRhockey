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

# Mock the transport with a queue of statuses (the last repeats), recording the interval
# each attempt asked the host clock for.
retry_mock <- function(statuses, headers = list(), env = parent.frame()) {
  st <- new.env()
  st$calls <- 0L
  st$intervals <- numeric()
  testthat::local_mocked_bindings(
    .polite_wait = function(url, min_interval = 1) {
      st$intervals <- c(st$intervals, min_interval)
      invisible(0)
    },
    .env = env
  )
  httr2::local_mocked_responses(function(req) {
    st$calls <- st$calls + 1L
    status <- statuses[[min(st$calls, length(statuses))]]
    httr2::response(status_code = status, url = req$url, headers = headers, body = charToRaw("{}"))
  }, env = env)
  st
}

test_that(".polite_perform retries 429/500/502/503/504 with a 1, 2 s backoff", {
  skip_on_cran()
  for (status in c(429L, 500L, 502L, 503L, 504L)) {
    st <- retry_mock(c(status, status, 200L))
    resp <- .polite_perform(httr2::request("https://r.test/a.json"))
    expect_equal(httr2::resp_status(resp), 200L, info = status)
    expect_equal(st$calls, 3L, info = status)
    expect_equal(st$intervals, c(1, 1, 2), info = status)
  }
})

test_that(".polite_perform does not retry other 4xx statuses or successes", {
  skip_on_cran()
  for (status in c(200L, 204L, 400L, 403L, 404L, 422L)) {
    st <- retry_mock(status)
    expect_equal(httr2::resp_status(.polite_perform(httr2::request("https://r.test/a.json"))), status)
    expect_equal(st$calls, 1L, info = status)
  }
})

test_that(".polite_perform backoff scales the retry delay, never below min_interval", {
  skip_on_cran()
  st <- retry_mock(c(502L, 502L, 200L))
  .polite_perform(httr2::request("https://r.test/a.json"), backoff = 2)
  expect_equal(st$intervals, c(1, 2, 4))
  st <- retry_mock(c(502L, 200L))
  .polite_perform(httr2::request("https://r.test/a.json"), min_interval = 5)
  expect_equal(st$intervals, c(5, 5))
})

test_that(".polite_perform honours Retry-After (seconds), capped at 120 s", {
  skip_on_cran()
  st <- retry_mock(c(429L, 503L, 200L), headers = list(`Retry-After` = "7"))
  .polite_perform(httr2::request("https://r.test/a.json"))
  expect_equal(st$intervals, c(1, 7, 7))
  st <- retry_mock(c(503L, 200L), headers = list(`Retry-After` = "99999"))
  .polite_perform(httr2::request("https://r.test/a.json"))
  expect_equal(st$intervals, c(1, 120))
  # a Retry-After shorter than the backoff does not shorten it
  st <- retry_mock(c(503L, 503L, 200L), headers = list(`Retry-After` = "0"))
  .polite_perform(httr2::request("https://r.test/a.json"))
  expect_equal(st$intervals, c(1, 1, 2))
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
