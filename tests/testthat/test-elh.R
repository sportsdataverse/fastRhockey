# Czech Extraliga (hokej.cz) wrappers: fixtures copied from sdv-py
# tests/fixtures/elh/, parity CSVs written by sdv-py's parsers on the SAME
# fixtures (fixtures/elh/parity/), the getter mocked. Live tests (one request
# per route) sit behind skip_euro_test().

fx <- function(...) testthat::test_path("fixtures", "elh", ...)
fx_json <- function(...) jsonlite::fromJSON(fx(...), simplifyVector = FALSE)
fx_text <- function(...) paste(readLines(fx(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

FULL <- "full/match__season__full__match_id.json"
SHOTS <- "full/visualization__shots__match_id.json"
EVENTS <- "full/visualization__events__match_id.json"
BOARD <- "full/scoreboard__date.json"

# The documented columns/types of sdv-py (ELH_SCHEMAS), written from sdv-py's
# parsers: polars dtype -> R class. sort_order is the one documented difference
# (Int64 there; its values exceed the 32-bit integer range, so a double here).
sdv_schema <- function(route) {
  s <- jsonlite::fromJSON(fx("parity", "schemas.json"))
  key <- c(pbp_latest = "pbp", list_objects = "game_ids", scoreboard_all = "scoreboard")[route]
  s[[if (is.na(key)) route else key]]
}
r_class <- c(Int64 = "integer", String = "character", Float64 = "numeric", Boolean = "logical", Date = "Date")

expect_schema <- function(tbl, route) {
  s <- sdv_schema(route)
  expect_s3_class(tbl, "tbl_df")
  expect_identical(names(tbl), names(s))
  for (nm in names(s)) {
    want <- r_class[[s[[nm]]]]
    if (nm == "sort_order") want <- "numeric"
    expect_true(inherits(tbl[[nm]], want) || (want == "numeric" && is.numeric(tbl[[nm]])),
                info = sprintf("%s: %s is not %s", route, nm, want))
    if (want == "integer") expect_type(tbl[[nm]], "integer")
    if (want == "numeric") expect_type(tbl[[nm]], "double")
  }
}

# Parity: the R tibble equals sdv-py's frame on the same fixture. An NA on one
# side and a value on the other is a mismatch (never silently equal).
expect_parity <- function(tbl, route, fixture) {
  exp <- utils::read.csv(fx("parity", sprintf("%s__%s.csv", route, fixture)),
    colClasses = "character", na.strings = "", fileEncoding = "UTF-8",
    check.names = FALSE, strip.white = FALSE
  )
  expect_identical(names(tbl), names(exp))
  expect_identical(nrow(tbl), nrow(exp))
  for (nm in names(exp)) {
    g <- tbl[[nm]]
    e <- exp[[nm]]
    info <- sprintf("%s/%s column %s", route, fixture, nm)
    if (is.character(g)) {
      expect_identical(is.na(g), is.na(e), info = info)
      expect_identical(enc2utf8(g[!is.na(g)]), enc2utf8(e[!is.na(e)]), info = info)
    } else if (is.logical(g)) {
      ge <- ifelse(is.na(g), NA_character_, ifelse(g, "true", "false"))
      expect_identical(ge, e, info = info)
    } else if (inherits(g, "Date")) {
      expect_identical(as.character(g), e, info = info)
    } else {
      en <- suppressWarnings(as.numeric(e))
      expect_identical(is.na(g), is.na(en), info = info)
      expect_equal(as.numeric(g[!is.na(g)]), en[!is.na(en)], info = info)
    }
  }
}

# Mock the shared JSON transport with a fixture; capture the URL it is asked for.
mock_json <- function(body, env = parent.frame()) {
  seen <- new.env()
  testthat::local_mocked_bindings(
    .euro_get = function(url, query = list(), headers = NULL, ...) {
      seen$url <- url
      seen$query <- query
      body
    },
    .env = env
  )
  seen
}

mock_text <- function(pages, env = parent.frame()) {
  seen <- new.env()
  seen$queries <- list()
  i <- 0
  testthat::local_mocked_bindings(
    .elh_get_text = function(url, query = list()) {
      i <<- i + 1
      seen$url <- url
      seen$queries[[i]] <- Filter(Negate(is.null), query)
      if (is.function(pages)) pages(i) else pages[[min(i, length(pages))]]
    },
    .env = env
  )
  seen
}

# A synthetic ListObjects page, for the paging / refusal cases.
listing_page <- function(keys, truncated = FALSE, root = "ListBucketResult") {
  contents <- vapply(keys, function(k) {
    sprintf("<Contents><Key>%s</Key><LastModified>2026-09-16T18:57:27.000Z</LastModified><Size>5000</Size></Contents>", k)
  }, character(1))
  sprintf(
    '<?xml version="1.0" encoding="UTF-8"?><%s xmlns="http://s3.amazonaws.com/doc/2006-03-01/"><Name>hokej.cz</Name><IsTruncated>%s</IsTruncated>%s</%s>',
    root, if (truncated) "true" else "false", paste(contents, collapse = ""), root
  )
}

# ---------------------------------------------------------------- season mapping

test_that("season is an END year mapped to the START-year S3 folder", {
  skip_on_cran()
  seen <- mock_json(fx_json(FULL))
  elh_pbp(2027, 2928321)
  expect_match(seen$url, "/hokej.cz/match/2026/full/2928321.json$")
  elh_pbp_latest(2015, 2928321)
  expect_match(seen$url, "/match/2014/short/2928321.json$")
  seen <- mock_json(fx_json("2026/match__season__mobile__match_id.json"))
  elh_pbp_mobile(2027, 2928321)
  expect_match(seen$url, "/match/2026/mobile/2928321.json$")
  seen <- mock_json(fx_json(FULL))
  elh_pbp_goals(2027, 2928321)
  expect_match(seen$url, "/match/2026/full/2928321.json$")
  # shots / events / scoreboard take no season
  seen <- mock_json(fx_json(SHOTS))
  elh_shots(2928321)
  expect_match(seen$url, "/hokej.cz/visualization/shots/2928321.json$")
  seen <- mock_json(fx_json(EVENTS))
  elh_events(2928321)
  expect_match(seen$url, "/visualization/events/2928321.json$")
  seen <- mock_json(fx_json(BOARD))
  elh_scoreboard("2026-10-11")
  expect_match(seen$url, "^https://json.esports.cz/hokejcz/scoreboard/2026-10-11.json$")
  elh_scoreboard(as.Date("2026-10-11"))
  expect_match(seen$url, "/scoreboard/2026-10-11.json$")
})

test_that("a non-year season, match id or date is a bad request before any request", {
  skip_on_cran()
  seen <- mock_json(fx_json(FULL))
  for (bad in list("abc", NA, 12, 20277, c(2026, 2027), NULL)) {
    expect_error(elh_pbp(bad, 2928321), class = "fastRhockey_bad_request")
    expect_error(elh_game_ids(bad), class = "fastRhockey_bad_request")
  }
  for (bad in list("abc", NA, -1, 0, 1.5, c(1, 2), "../x")) {
    expect_error(elh_pbp(2027, bad), class = "fastRhockey_bad_request")
    expect_error(elh_shots(bad), class = "fastRhockey_bad_request")
  }
  for (bad in list("2026-10", "11-10-2026", NA, 20261011, "2026-10-11/../x")) {
    expect_error(elh_scoreboard(bad), class = "fastRhockey_bad_request")
  }
  expect_null(seen$url)
})

# ---------------------------------------------------------------- columns, types, parity

test_that("elh_pbp: sdv-py columns and parity on 2026-27 (truncated + full) and 2016-17", {
  skip_on_cran()
  for (fixture in c("2026", "2016", "full")) {
    mock_json(fx_json(fixture, "match__season__full__match_id.json"))
    got <- elh_pbp(2027, 2928321)
    expect_schema(got, "pbp")
    expect_parity(got, "pbp", fixture)
  }
  got <- elh_pbp(2027, 2928321)
  expect_equal(nrow(got), 119)
  expect_identical(got$sort_order, sort(got$sort_order)) # oldest first
  expect_true(all(got$comment_id[!is.na(got$comment_id)] > 0))
  expect_type(got$sort_order, "double") # exceeds the 32-bit integer range
  expect_true(any(got$sort_order > .Machine$integer.max))
})

test_that("elh_pbp_goals: sdv-py columns and parity", {
  skip_on_cran()
  for (fixture in c("2026", "2016", "full")) {
    mock_json(fx_json(fixture, "match__season__full__match_id.json"))
    got <- elh_pbp_goals(2027, 2928321)
    expect_schema(got, "pbp_goals")
    expect_parity(got, "pbp_goals", fixture)
  }
  # 2016-17 goals carry no on-ice block: ONE row with NA on-ice columns
  mock_json(fx_json("2016/match__season__full__match_id.json"))
  g16 <- elh_pbp_goals(2017, 2508598)
  expect_equal(nrow(g16), 1)
  expect_true(all(is.na(g16[, 11:17])))
})

test_that("elh_pbp_latest and elh_pbp_mobile: sdv-py columns and parity", {
  skip_on_cran()
  for (fixture in c("2026", "2016")) {
    mock_json(fx_json(fixture, "match__season__short__match_id.json"))
    got <- elh_pbp_latest(2027, 2928321)
    expect_schema(got, "pbp_latest")
    expect_parity(got, "pbp_latest", fixture)
    mock_json(fx_json(fixture, "match__season__mobile__match_id.json"))
    got <- elh_pbp_mobile(2027, 2928321)
    expect_schema(got, "pbp_mobile")
    expect_parity(got, "pbp_mobile", fixture)
  }
})

test_that("elh_shots: sdv-py columns, hokej.cz id types and parity (all eras)", {
  skip_on_cran()
  for (fixture in c("2026", "2016", "full")) {
    mock_json(fx_json(fixture, "visualization__shots__match_id.json"))
    got <- elh_shots(if (fixture == "2016") 2508598 else 2928321)
    expect_schema(got, "shots")
    expect_parity(got, "shots", fixture)
  }
  mock_json(fx_json(SHOTS))
  got <- elh_shots(2928321)
  expect_type(got$hokejcz_player_id, "integer")
  expect_type(got$coordinate_x, "double")
  expect_type(got$is_goal, "logical")
  expect_setequal(unique(got$team_side), c("home", "away"))
  expect_identical(got$is_goal, got$shot_result_id == 4L)
  expect_identical(got$match_shot_resutl_id, got$shot_result_id)
  expect_true(all(got$period %in% 1:4))
})

test_that("elh_events: sdv-py columns and parity", {
  skip_on_cran()
  for (fixture in c("2026", "full")) {
    mock_json(fx_json(fixture, "visualization__events__match_id.json"))
    got <- elh_events(2928321)
    expect_schema(got, "events")
    expect_parity(got, "events", fixture)
  }
  expect_equal(nrow(got), 120) # 60 minutes x 2 teams
})

test_that("elh_scoreboard: sdv-py columns, both id spaces, parity; other leagues filtered", {
  skip_on_cran()
  for (fixture in c("2026", "full")) {
    mock_json(fx_json(fixture, "scoreboard__date.json"))
    got <- elh_scoreboard("2026-10-11")
    expect_schema(got, "scoreboard")
    expect_parity(got, "scoreboard", fixture)
    # league_id = NULL keeps every league (internal parser; same as sdv-py league_id=None)
    expect_parity(.elh_parse_scoreboard(fx_json(fixture, "scoreboard__date.json"), league_id = NULL),
                  "scoreboard_all", fixture)
  }
  expect_true(all(got$league_id == 101L))
  expect_s3_class(got$game_date, "Date")
  # the scoreboard maps onlajny team 156 to hokej.cz team 8 (Sparta)
  sparta <- got[got$onlajny_home_team_id %in% 156L | got$onlajny_away_team_id %in% 156L, ]
  expect_true(nrow(sparta) >= 1)
  expect_true(all(sparta$hokejcz_home_team_id[sparta$onlajny_home_team_id %in% 156L] == 8L))
})

test_that("parsed wrappers return fastRhockey_data; the raw list stays plain", {
  skip_on_cran()
  mock_json(fx_json(FULL))
  expect_s3_class(elh_pbp(2027, 2928321), "fastRhockey_data")
  expect_false(inherits(elh_pbp(2027, 2928321, return_parsed = FALSE), "fastRhockey_data"))
  mock_json(fx_json(BOARD))
  expect_s3_class(elh_scoreboard("2026-10-11"), "fastRhockey_data")
  mock_text(fx_text("2026/list.xml"))
  expect_s3_class(elh_list_objects(), "fastRhockey_data")
})

test_that("elh_list_objects: sdv-py columns and parity; raw XML on return_parsed = FALSE", {
  skip_on_cran()
  xml <- fx_text("2026/list.xml")
  seen <- mock_text(xml)
  got <- elh_list_objects(prefix = "match/2026/full/", max_keys = "3")
  expect_schema(got, "list_objects")
  expect_parity(got, "list_objects", "2026")
  expect_identical(seen$queries[[1]], list(prefix = "match/2026/full/", `max-keys` = "3"))
  expect_match(seen$url, "/hokej.cz/$")
  expect_identical(elh_list_objects(return_parsed = FALSE), xml)
})

test_that("return_parsed = FALSE returns the raw parsed JSON", {
  skip_on_cran()
  mock_json(fx_json(FULL))
  raw <- elh_pbp(2027, 2928321, return_parsed = FALSE)
  expect_type(raw, "list")
  expect_true(!is.null(raw$comments$comment))
  mock_json(fx_json(SHOTS))
  expect_true(is.list(elh_shots(2928321, return_parsed = FALSE)$match$shots))
})

# ---------------------------------------------------------------- empty payloads

test_that("a malformed or empty payload parses to a zero-row tibble with the documented columns", {
  skip_on_cran()
  for (empty in list(NULL, list(), "x", 1, list(comments = list()), setNames(list(), character()))) {
    expect_schema(.elh_parse_pbp(empty), "pbp")
    expect_equal(nrow(.elh_parse_pbp(empty)), 0)
    expect_schema(.elh_parse_pbp_goals(empty), "pbp_goals")
    expect_equal(nrow(.elh_parse_pbp_goals(empty)), 0)
    expect_schema(.elh_parse_pbp_mobile(empty), "pbp_mobile")
    expect_schema(.elh_parse_shots(empty), "shots")
    expect_schema(.elh_parse_events(empty), "events")
    expect_schema(.elh_parse_scoreboard(empty), "scoreboard")
    expect_equal(nrow(.elh_parse_scoreboard(empty)), 0)
  }
  expect_schema(.elh_parse_game_ids(character()), "list_objects")
  expect_schema(.elh_parse_game_ids("not xml"), "list_objects")
  expect_equal(nrow(.elh_parse_game_ids(listing_page(character()))), 0)
  # empty id columns keep their types
  z <- .elh_parse_shots(NULL)
  expect_type(z$hokejcz_game_id, "integer")
  expect_type(z$coordinate_x, "double")
  expect_type(z$is_goal, "logical")
  expect_s3_class(.elh_parse_scoreboard(NULL)$game_date, "Date")
})

test_that("the feed's 0 id placeholder and odd cells become NA, not 0 or an error", {
  skip_on_cran()
  expect_identical(.elh_id("0"), NA_real_)
  expect_identical(.elh_id(0), NA_real_)
  expect_identical(.elh_id("2928321"), 2928321)
  expect_identical(.elh_int(TRUE), NA_real_)
  expect_identical(.elh_int(1.5), NA_real_)
  expect_identical(.elh_int(" 7 "), 7)
  expect_identical(.elh_int("1.5"), NA_real_) # a float string is not an integer (no "123.0" ids)
  expect_identical(.elh_str(""), NA_character_)
  expect_identical(.elh_str(list()), NA_character_)
  # an id beyond the 32-bit range is refused, never wrapped to NA
  expect_error(.elh_frame(list(list(a = 3e9)), c(a = "integer")), class = "fastRhockey_asset_fetch")
})

test_that("a pbp entry with two penalties sums minutes and comma-joins offenses", {
  skip_on_cran()
  mk <- function(len2) list(
    `@attributes` = list(id = "1", order = "10", label = "penalty"),
    time = "05:00",
    details = list(detail = list(
      `@attributes` = list(id = "9"),
      opponent = list(`@attributes` = list(id = "156", code = "SPA")),
      player1 = list(`@attributes` = list(id = "5"),
        penalties = list(penalty = list(
          list(`@attributes` = list(length = "2", offense = "HOOK")),
          list(`@attributes` = list(length = len2, offense = "TRIP"))
        )))
    ))
  )
  got <- .elh_parse_pbp(list(comments = list(comment = list(mk("2")))))
  expect_equal(got$penalty_minutes, 4L)
  expect_identical(got$penalty_offense, "HOOK,TRIP")
  expect_equal(got$period, 1L)
  expect_equal(got$game_seconds, 300L)
})

# ---------------------------------------------------------------- refusals: body scope

test_that("a 2xx error envelope is asset_fetch for every JSON route", {
  skip_on_cran()
  mock_json(list(error = "Forbidden", message = "nope"))
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_asset_fetch")
  expect_error(elh_pbp_goals(2027, 1), class = "fastRhockey_asset_fetch")
  expect_error(elh_pbp_latest(2027, 1), class = "fastRhockey_asset_fetch")
  expect_error(elh_pbp_mobile(2027, 1), class = "fastRhockey_asset_fetch")
  expect_error(elh_shots(1), class = "fastRhockey_asset_fetch")
  expect_error(elh_events(1), class = "fastRhockey_asset_fetch")
  expect_error(elh_scoreboard("2026-10-11"), class = "fastRhockey_asset_fetch")
  # a body that merely contains an envelope key next to real data is not an envelope
  mock_json(c(fx_json(FULL), list(status = "ok")))
  expect_s3_class(elh_pbp(2027, 2928321), "tbl_df")
})

test_that("a stub body (no comments, shots or minutes) is no_data, not an empty frame", {
  skip_on_cran()
  empty_pbp <- list(comments = list(comment = list()))
  mock_json(empty_pbp)
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_no_data")
  expect_error(elh_pbp_goals(2027, 1), class = "fastRhockey_no_data")
  expect_error(elh_pbp_latest(2027, 1), class = "fastRhockey_no_data")
  mock_json(list(comments = list(comment = list(setNames(list(), character())))))
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_no_data")
  mock_json(list(comments = list()))
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_no_data")
  mock_json(list(commentEvents = list()))
  expect_error(elh_pbp_mobile(2027, 1), class = "fastRhockey_no_data")
  mock_json(list(match = list(id = 1, shots = list())))
  expect_error(elh_shots(1), class = "fastRhockey_no_data")
  ev <- fx_json(EVENTS)
  ev$visitor$shots <- list()
  mock_json(ev)
  expect_error(elh_events(2928321), class = "fastRhockey_no_data")
  ev <- fx_json(EVENTS)
  ev$home <- NULL
  mock_json(ev)
  expect_error(elh_events(2928321), class = "fastRhockey_no_data")
})

test_that("a shots file for another game is asset_fetch", {
  skip_on_cran()
  mock_json(fx_json(SHOTS)) # game 2928321
  expect_error(elh_shots(2928322), class = "fastRhockey_asset_fetch")
  expect_s3_class(elh_shots(2928321), "tbl_df")
})

test_that("a scoreboard dated another day is asset_fetch; a day without an ELH game is no_data", {
  skip_on_cran()
  mock_json(fx_json(BOARD)) # games dated 11-10-2026
  expect_error(elh_scoreboard("2026-10-10"), class = "fastRhockey_asset_fetch")
  expect_s3_class(elh_scoreboard("2026-10-11"), "tbl_df")
  board <- fx_json(BOARD)
  board[["101"]] <- NULL
  mock_json(board)
  expect_error(elh_scoreboard("2026-10-11"), class = "fastRhockey_no_data")
  mock_json(setNames(list(), character()))
  expect_error(elh_scoreboard("2026-10-11"), class = "fastRhockey_no_data")
})

# ---------------------------------------------------------------- refusals: listing

test_that("a listing that is not a ListBucketResult, not XML, or a truncated page without a key is asset_fetch", {
  skip_on_cran()
  mock_text("<html><body>Access Denied</body></html>")
  expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch")
  mock_text(listing_page("match/2026/full/1.json", root = "Error"))
  expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch")
  mock_text("this is not xml")
  expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch")
  mock_text(listing_page(character(), truncated = TRUE))
  expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch", regexp = "no key to page on")
})

test_that("a listing body is never read as a file path or URL", {
  skip_on_cran()
  expect_error(.elh_listing_root("https://x/y", "/etc/passwd"), class = "fastRhockey_asset_fetch")
  expect_equal(nrow(.elh_parse_game_ids("http://127.0.0.1:9/never")), 0)
})

# ---------------------------------------------------------------- elh_game_ids paging

test_that("elh_game_ids pages with marker, drops non-match keys, dedups and sorts", {
  skip_on_cran()
  pages <- list(
    listing_page(c("match/2026/full/300.json", "match/2026/full/100.json"), truncated = TRUE),
    listing_page(c("match/2026/full/200.json", "match/2026/full/100.json", "match/2026/full/notanid.json"), truncated = TRUE),
    listing_page("match/2026/full/400.json")
  )
  seen <- mock_text(pages)
  got <- elh_game_ids(2027)
  expect_s3_class(got, "fastRhockey_data")
  expect_identical(got$hokejcz_game_id, c(100L, 200L, 300L, 400L))
  expect_identical(names(got), c("hokejcz_game_id", "season", "key", "last_modified", "size"))
  expect_type(got$season, "integer")
  expect_true(all(got$season == 2027L))
  expect_length(seen$queries, 3)
  expect_identical(seen$queries[[1]], list(prefix = "match/2026/full/"))
  expect_identical(seen$queries[[2]]$marker, "match/2026/full/100.json")
  expect_identical(seen$queries[[3]]$marker, "match/2026/full/notanid.json")
  expect_true(all(seen$queries[[2]]$prefix == "match/2026/full/"))
})

test_that("elh_game_ids: an empty folder is no_data; a listing that does not advance or never ends is asset_fetch", {
  skip_on_cran()
  mock_text(listing_page(character()))
  expect_error(elh_game_ids(2027), class = "fastRhockey_no_data")
  mock_text(listing_page("match/2026/other/1.json"))
  expect_error(elh_game_ids(2027), class = "fastRhockey_no_data") # no *full* game key
  # the same last key twice: no progress
  mock_text(listing_page("match/2026/full/1.json", truncated = TRUE))
  expect_error(elh_game_ids(2027), class = "fastRhockey_asset_fetch", regexp = "does not advance")
  # always a new key, never ends
  mock_text(function(i) listing_page(sprintf("match/2026/full/%d.json", i), truncated = TRUE))
  expect_error(elh_game_ids(2027), class = "fastRhockey_asset_fetch", regexp = "did not end")
})

# ---------------------------------------------------------------- transport error vocabulary

test_that("HTTP statuses map to the classed conditions (real .euro_get / .elh_get_text path)", {
  skip_on_cran()
  fake <- function(status, body = "", type = "application/json") {
    force(status)
    function(req, max_tries = 3, min_interval = 1) {
      httr2::response(status_code = status, url = req$url, headers = list(`Content-Type` = type),
                      body = charToRaw(body))
    }
  }
  testthat::local_mocked_bindings(.polite_perform = fake(404, "<Error>NoSuchKey</Error>", "application/xml"))
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_no_data")
  expect_error(elh_shots(1), class = "fastRhockey_no_data")
  expect_error(elh_scoreboard("2026-10-11"), class = "fastRhockey_no_data")
  expect_error(elh_list_objects(), class = "fastRhockey_no_data")
  testthat::local_mocked_bindings(.polite_perform = fake(400, "bad"))
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_bad_request")
  expect_error(elh_list_objects(), class = "fastRhockey_bad_request")
  testthat::local_mocked_bindings(.polite_perform = fake(422, "bad"))
  expect_error(elh_events(1), class = "fastRhockey_bad_request")
  for (status in c(401, 403, 429, 500, 503)) {
    testthat::local_mocked_bindings(.polite_perform = fake(status, "x"))
    expect_error(elh_pbp(2027, 1), class = "fastRhockey_asset_fetch", info = status)
    expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch", info = status)
  }
  # 200 with an empty or non-JSON body is not "nothing": the answer is unknown
  testthat::local_mocked_bindings(.polite_perform = fake(200, ""))
  expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch")
  testthat::local_mocked_bindings(.polite_perform = fake(200, "  \n"))
  expect_error(elh_pbp(2027, 1), class = "fastRhockey_asset_fetch")
  expect_error(elh_list_objects(), class = "fastRhockey_asset_fetch")
  testthat::local_mocked_bindings(.polite_perform = fake(200, "<html>captcha</html>", "text/html"))
  expect_error(elh_shots(1), class = "fastRhockey_asset_fetch")
  # a real 200 goes through the whole stack
  testthat::local_mocked_bindings(.polite_perform = fake(200, readChar(fx(SHOTS), 1e6, useBytes = TRUE)))
  expect_equal(nrow(elh_shots(2928321)), 121)
  testthat::local_mocked_bindings(.polite_perform = fake(200, paste(readLines(fx("2026/list.xml"), warn = FALSE), collapse = "\n"), "application/xml"))
  expect_equal(nrow(elh_list_objects()), 3)
})

test_that("requests are throttled per host through the shared mocked-response layer", {
  skip_on_cran()
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, url = req$url, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(readChar(fx(SHOTS), 1e6, useBytes = TRUE)))
  })
  rm(list = ls(.polite_clock), envir = .polite_clock)
  t0 <- proc.time()[["elapsed"]]
  elh_shots(2928321)
  elh_shots(2928321)
  expect_gte(proc.time()[["elapsed"]] - t0, 0.95) # >= 1 s between two requests to s3-eu-west-1
})

# ---------------------------------------------------------------- live (one request per route)

test_that("live: the routes answer with the documented columns", {
  skip_on_cran()
  skip_euro_test()
  ids <- elh_list_objects(prefix = "match/2026/full/", max_keys = "3")
  expect_schema(ids, "list_objects")
  expect_schema(elh_pbp(2027, 2928321), "pbp")
  expect_schema(elh_pbp_mobile(2027, 2928321), "pbp_mobile")
  expect_schema(elh_shots(2928321), "shots")
  expect_schema(elh_events(2928321), "events")
  board <- tryCatch(elh_scoreboard(format(Sys.Date() - 1)), fastRhockey_no_data = function(e) NULL)
  if (!is.null(board)) expect_schema(board, "scoreboard")
})
