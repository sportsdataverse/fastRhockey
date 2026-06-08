test_that("ESPN - Get ESPN NHL league root", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_league_root()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL league root endpoint at test time")
  }

  key_cols <- c(
    "id", "guid", "uid", "name", "display_name", "abbreviation",
    "short_name", "slug", "is_tournament", "gender",
    "season_ref", "seasons_ref", "groups_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$slug, "nhl")

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season pointer", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_pointer()

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season pointer endpoint at test time")
  }

  key_cols <- c(
    "season", "start_date", "end_date", "display_name",
    "current_type_id", "current_type", "current_type_name",
    "current_type_slug", "season_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_true(is.integer(x$season))
  expect_true(x$season >= 2020L)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL seasons list", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_seasons(limit = 10)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL seasons endpoint at test time")
  }

  key_cols <- c("ref", "season", "count", "page_count")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(is.integer(x$season))
  # Seasons are parsed from $ref URLs and should be valid years
  expect_true(all(x$season >= 1917L & x$season <= 2100L, na.rm = TRUE))

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season info", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_info(season = 2026)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season info endpoint at test time")
  }

  key_cols <- c(
    "season", "start_date", "end_date", "display_name",
    "season_ref", "types_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$season, 2026L)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season types", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_types(season = 2026)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season types endpoint at test time")
  }

  key_cols <- c("ref", "season_type_id", "season")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 2L)
  expect_true(all(x$season == 2026L))
  # Standard NHL season types: 1=pre, 2=reg, 3=post, 4=off
  expect_true(2L %in% x$season_type_id)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season type singular", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_type(season = 2026, season_type = 2)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season type endpoint at test time")
  }

  key_cols <- c(
    "season", "season_type", "id", "type", "name", "abbreviation",
    "slug", "year", "start_date", "end_date", "has_groups", "has_standings"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$season, 2026L)
  expect_equal(x$season_type, 2L)

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season groups", {
  skip_on_cran()
  skip_espn_test()

  x <- espn_nhl_season_groups(season = 2026, season_type = 2)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season groups endpoint at test time")
  }

  key_cols <- c("ref", "group_id", "season", "season_type")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(all(x$season_type == 2L))
  expect_true(is.integer(x$group_id))

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season group singular", {
  skip_on_cran()
  skip_espn_test()

  # Derive group_id live from the groups collection
  grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
  if (!is.data.frame(grps) || nrow(grps) == 0) {
    skip("Could not retrieve season groups to derive group_id")
  }
  gid <- grps$group_id[1]

  x <- espn_nhl_season_group(season = 2026, season_type = 2, group_id = gid)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season group endpoint at test time")
  }

  key_cols <- c(
    "season", "season_type", "group_id", "id", "name",
    "is_conference", "group_ref"
  )
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1L)
  expect_equal(x$season, 2026L)
  expect_equal(x$season_type, 2L)
  expect_equal(x$group_id, as.integer(gid))

  Sys.sleep(1)
})


test_that("ESPN - Get ESPN NHL season group teams", {
  skip_on_cran()
  skip_espn_test()

  # Derive group_id live from the groups collection
  grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
  if (!is.data.frame(grps) || nrow(grps) == 0) {
    skip("Could not retrieve season groups to derive group_id")
  }
  gid <- grps$group_id[1]

  x <- espn_nhl_season_group_teams(season = 2026, season_type = 2,
                                    group_id = gid, limit = 100)

  if (!is.data.frame(x) || nrow(x) == 0) {
    skip("No rows returned from ESPN NHL season group teams endpoint at test time")
  }

  key_cols <- c("ref", "team_id", "season", "season_type", "group_id")
  expect_in(sort(key_cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
  expect_true(nrow(x) >= 1L)
  expect_true(all(x$season == 2026L))
  expect_true(all(x$season_type == 2L))
  expect_true(all(x$group_id == as.integer(gid)))
  expect_true(is.character(x$team_id))

  Sys.sleep(1)
})
