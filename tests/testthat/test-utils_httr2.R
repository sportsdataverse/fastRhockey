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
  expect_equal(captured$headers$Origin, "https://x.test")
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
