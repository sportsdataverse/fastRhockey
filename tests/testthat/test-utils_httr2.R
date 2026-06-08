## Tests for the httr2 helper layer (R/utils.R)
## These are all network-free unit tests.

# ---------------------------------------------------------------------------
# Step A: .resolve_proxy()
# ---------------------------------------------------------------------------

test_that(".resolve_proxy prefers the explicit arg, then the option, else NULL", {
  withr::local_options(fastRhockey.proxy = NULL)
  expect_null(.resolve_proxy(NULL))

  withr::local_options(fastRhockey.proxy = "http://opt:8080")
  expect_equal(.resolve_proxy(NULL), "http://opt:8080")
  expect_equal(.resolve_proxy("http://arg:3128"), "http://arg:3128")
})

# ---------------------------------------------------------------------------
# Step B: .retry_request()
# ---------------------------------------------------------------------------

test_that(".retry_request builds URL query + headers + proxy onto the request", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .package = "httr2",
    req_perform = function(req, ...) { captured <<- req; structure(list(), class = "httr2_response") }
  )
  .retry_request(
    "https://example.test/api",
    params  = list(season = 2024, page = 2),
    headers = c(Origin = "https://x.test"),
    proxy   = "http://proxy:8080"
  )
  expect_true(grepl("season=2024", captured$url))
  expect_true(grepl("page=2", captured$url))
  expect_equal(captured$headers[["Origin"]], "https://x.test")
  expect_true(!is.null(captured$options$proxy))
})

# ---------------------------------------------------------------------------
# Step C: .resp_text() / .resp_json()
# ---------------------------------------------------------------------------

test_that(".resp_text and .resp_json read an httr2 response body", {
  resp <- httr2::response(
    status_code = 200,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw('{"a":1,"b":"x"}')
  )
  expect_equal(.resp_text(resp), '{"a":1,"b":"x"}')
  parsed <- .resp_json(resp)
  expect_equal(parsed$a, 1)
  expect_equal(parsed$b, "x")
})

# ---------------------------------------------------------------------------
# Step D: check_status() -- httr2-based
# ---------------------------------------------------------------------------

test_that("check_status passes on 200 and errors on non-200 (httr2)", {
  ok  <- httr2::response(status_code = 200, body = charToRaw("{}"))
  bad <- httr2::response(status_code = 404, body = charToRaw("{}"))
  expect_silent(check_status(ok))
  expect_error(check_status(bad), "API returned an error")
})

test_that("check_status also accepts legacy httr responses during the transition", {
  ok_httr  <- structure(list(status_code = 200L), class = "response")
  bad_httr <- structure(list(status_code = 500L), class = "response")
  expect_silent(check_status(ok_httr))
  expect_error(check_status(bad_httr), "API returned an error")
})

# ---------------------------------------------------------------------------
# Step E: .capture_args() + .report_api_error() / .report_api_warning()
# ---------------------------------------------------------------------------

test_that(".capture_args returns the caller's non-... formals", {
  demo <- function(team_id, season = 2024, ...) .capture_args()
  out <- demo(team_id = "1", season = 2025)
  expect_equal(out$team_id, "1")
  expect_equal(out$season, 2025)
  expect_false("..." %in% names(out))
})

test_that("reporters emit cli messages and return invisibly", {
  e <- simpleError("boom")
  expect_message(.report_api_error(e, hint = "load {team_id}", args = list(team_id = "7")), "boom")
  w <- simpleWarning("careful")
  expect_message(.report_api_warning(w, args = list(team_id = "7")), "careful")
})
