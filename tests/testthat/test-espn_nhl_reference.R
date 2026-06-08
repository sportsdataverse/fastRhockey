# tests/testthat/test-espn_nhl_reference.R
# ESPN NHL core-v2 reference catalog tests (Tier 6)

# ===========================================================================
# 1. Venues collection
# ===========================================================================
test_that("espn_nhl_venues returns venue refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_venues(limit = 50)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_venues at test time")
  }

  expect_in(c("ref", "venue_id", "count", "page_count"), colnames(x))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  Sys.sleep(1)
})

# ===========================================================================
# 2. Venue singular
# ===========================================================================
test_that("espn_nhl_venue returns venue detail", {
  skip_on_cran()
  skip_espn_test()

  vns <- espn_nhl_venues(limit = 10)
  if (is.null(vns) || !is.data.frame(vns) || nrow(vns) == 0) {
    skip("No venue ids available to test espn_nhl_venue")
  }

  vid <- vns$venue_id[1]
  x   <- espn_nhl_venue(venue_id = vid)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip(paste0("No rows returned from espn_nhl_venue(", vid, ") at test time"))
  }

  cols <- c("venue_id", "id", "full_name", "grass", "indoor", "venue_ref")
  expect_in(cols, colnames(x))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  Sys.sleep(1)
})

# ===========================================================================
# 3. Franchises collection
# ===========================================================================
test_that("espn_nhl_franchises returns franchise refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_franchises()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_franchises at test time")
  }

  expect_in(c("ref", "franchise_id", "count", "page_count"), colnames(x))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  Sys.sleep(1)
})

# ===========================================================================
# 4. Franchise singular
# ===========================================================================
test_that("espn_nhl_franchise returns franchise detail", {
  skip_on_cran()
  skip_espn_test()

  fcs <- espn_nhl_franchises()
  if (is.null(fcs) || !is.data.frame(fcs) || nrow(fcs) == 0) {
    skip("No franchise ids available to test espn_nhl_franchise")
  }

  fid <- fcs$franchise_id[1]
  x   <- espn_nhl_franchise(franchise_id = fid)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip(paste0("No rows returned from espn_nhl_franchise(", fid, ") at test time"))
  }

  cols <- c("franchise_id", "id", "display_name", "abbreviation",
            "is_active", "venue_ref", "franchise_ref")
  expect_in(cols, colnames(x))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  Sys.sleep(1)
})

# ===========================================================================
# 5. Positions collection
# ===========================================================================
test_that("espn_nhl_positions returns position refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_positions()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_positions at test time")
  }

  expect_in(c("ref", "position_id", "count", "page_count"), colnames(x))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  Sys.sleep(1)
})

# ===========================================================================
# 6. Position singular
# ===========================================================================
test_that("espn_nhl_position returns position detail", {
  skip_on_cran()
  skip_espn_test()

  pos <- espn_nhl_positions()
  if (is.null(pos) || !is.data.frame(pos) || nrow(pos) == 0) {
    skip("No position ids available to test espn_nhl_position")
  }

  pid <- pos$position_id[1]
  x   <- espn_nhl_position(position_id = pid)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip(paste0("No rows returned from espn_nhl_position(", pid, ") at test time"))
  }

  cols <- c("position_id", "id", "name", "abbreviation", "leaf", "position_ref")
  expect_in(cols, colnames(x))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  Sys.sleep(1)
})

# ===========================================================================
# 7. Coaches (season-scoped collection)
# ===========================================================================
test_that("espn_nhl_coaches returns coach refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_coaches()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_coaches at test time")
  }

  expect_in(c("ref", "coach_id", "season", "count", "page_count"), colnames(x))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  Sys.sleep(1)
})

# ===========================================================================
# 8. Coach singular
# ===========================================================================
test_that("espn_nhl_coach returns coach detail", {
  skip_on_cran()
  skip_espn_test()

  ccs <- espn_nhl_coaches()
  if (is.null(ccs) || !is.data.frame(ccs) || nrow(ccs) == 0) {
    skip("No coach ids available to test espn_nhl_coach")
  }

  cid <- ccs$coach_id[1]
  x   <- espn_nhl_coach(coach_id = cid)

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip(paste0("No rows returned from espn_nhl_coach(", cid, ") at test time"))
  }

  cols <- c("coach_id", "id", "first_name", "last_name", "coach_ref")
  expect_in(cols, colnames(x))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  Sys.sleep(1)
})

# ===========================================================================
# 9. Coach record (404 for NHL — expect empty graceful return)
# ===========================================================================
test_that("espn_nhl_coach_record returns empty gracefully for NHL", {
  skip_on_cran()
  skip_espn_test()

  ccs <- espn_nhl_coaches()
  if (is.null(ccs) || !is.data.frame(ccs) || nrow(ccs) == 0) {
    skip("No coach ids available to test espn_nhl_coach_record")
  }

  cid <- ccs$coach_id[1]
  # 404 for NHL is expected; should return empty data.frame without error
  x <- suppressWarnings(espn_nhl_coach_record(coach_id = cid))

  expect_true(is.data.frame(x))
  # empty is the expected result for NHL
  Sys.sleep(1)
})

# ===========================================================================
# 10. Countries (count=0 for NHL — expect empty graceful return)
# ===========================================================================
test_that("espn_nhl_countries returns empty gracefully for NHL", {
  skip_on_cran()
  skip_espn_test()

  # count=0 for NHL is expected; should return empty data.frame without error
  x <- suppressWarnings(espn_nhl_countries())

  expect_true(is.data.frame(x))
  Sys.sleep(1)
})

# ===========================================================================
# 11. Providers collection
# ===========================================================================
test_that("espn_nhl_providers returns provider refs", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_providers()

  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from espn_nhl_providers at test time")
  }

  expect_in(c("ref", "provider_id", "count", "page_count"), colnames(x))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) > 0)
  Sys.sleep(1)
})
