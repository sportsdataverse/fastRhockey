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
