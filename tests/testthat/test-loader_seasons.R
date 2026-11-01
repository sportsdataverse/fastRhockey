# Offline unit tests for the release-loader season helper in R/utils.R. The
# asset existence check is mocked, so nothing here touches the network.

url_for <- function(s) paste0("https://example.invalid/tag/play_by_play_", s, ".rds")

# .release_asset_exists() stand-in: TRUE for every url except those in `missing`.
mock_assets <- function(missing = character()) {
  function(url) !url %in% missing
}

test_that(".loader_seasons falls back to the previous season only for the default", {
  local_mocked_bindings(.release_asset_exists = mock_assets(url_for(2027)))

  expect_message(
    out <- .loader_seasons(2027, 2024, 2027, what = "pwhl_pbp", url_for = url_for, default = TRUE),
    "2027 is not published yet; loading 2026"
  )
  expect_equal(out, 2026)

  # An explicit request for the unpublished season is kept as asked.
  expect_no_message(
    out <- .loader_seasons(2027, 2024, 2027, what = "pwhl_pbp", url_for = url_for, default = FALSE)
  )
  expect_equal(out, 2027)
})

test_that(".loader_seasons keeps the default season when it is published", {
  local_mocked_bindings(.release_asset_exists = mock_assets())
  expect_no_message(
    out <- .loader_seasons(2027, 2024, 2027, what = "x", url_for = url_for, default = TRUE)
  )
  expect_equal(out, 2027)
})

test_that(".loader_seasons(TRUE) expands to published seasons only", {
  local_mocked_bindings(.release_asset_exists = mock_assets(url_for(2027)))
  expect_equal(.loader_seasons(TRUE, 2024, 2027, what = "x", url_for = url_for), 2024:2026)

  local_mocked_bindings(.release_asset_exists = mock_assets())
  expect_equal(.loader_seasons(TRUE, 2024, 2027, what = "x", url_for = url_for), 2024:2027)
  # Without url_for there is no existence check at all.
  expect_equal(.loader_seasons(TRUE, 2024, 2027, what = "x"), 2024:2027)
})

test_that(".loader_seasons rejects seasons outside the range", {
  expect_error(.loader_seasons(2023, 2024, 2027, what = "x"), "from 2024 to 2027")
  expect_error(.loader_seasons(2028, 2024, 2027, what = "x"), "You requested: 2028")
  expect_error(.loader_seasons(2024.5, 2024, 2027, what = "x"), "whole seasons")
  expect_error(.loader_seasons("2024", 2024, 2027, what = "x"), "whole seasons")
  expect_error(.loader_seasons(NA_real_, 2024, 2027, what = "x"), "whole seasons")
  expect_error(.loader_seasons(numeric(), 2024, 2027, what = "x"), "whole seasons")
})

test_that(".loader_seasons passes multi-season requests through without a check", {
  local_mocked_bindings(.release_asset_exists = function(url) stop("no network expected"))
  expect_equal(.loader_seasons(c(2025, 2026), 2024, 2027, what = "x", url_for = url_for,
                               default = TRUE), c(2025, 2026))
})

test_that("loaders and their aliases flag a default-season call", {
  seen <- NULL
  local_mocked_bindings(.pwhl_release_loader = function(seasons, ..., default = FALSE) {
    seen <<- default
    NULL
  })
  load_pwhl_team_box()
  expect_true(seen)
  load_pwhl_team_box(2025)
  expect_false(seen)
  load_pwhl_team_boxscores()
  expect_true(seen)
  load_pwhl_team_boxscores(2025)
  expect_false(seen)
})

test_that(".hockeytech_season_from_date rolls over in September", {
  expect_identical(.hockeytech_season_from_date(as.Date("2026-08-31")), 2026L)
  expect_identical(.hockeytech_season_from_date(as.Date("2026-09-01")), 2027L)
  expect_identical(.hockeytech_season_from_date(as.Date("2027-04-15")), 2027L)
})
