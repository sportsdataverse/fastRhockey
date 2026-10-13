# eSports hokejovyzapis families: del_*, icehl_*, eihl_*, metal_*.
# Fixture tests always run (the getter is mocked); live tests need RUN_EURO_TESTS=true.
#
# Fixtures are sdv-py's tests/fixtures/<league>_esports bodies. The parity/ CSVs are
# sdv-py's frames (esports_parsers.* with the documented columns, as the generated
# wrappers call them) written from the SAME bodies; parity/dtypes.csv has their
# types and parity/manifest.csv the call that fetches each body.

fx <- function(...) testthat::test_path("fixtures", "esports", ...)
manifest <- utils::read.csv(fx("parity", "manifest.csv"), stringsAsFactors = FALSE)
dtypes <- utils::read.csv(fx("parity", "dtypes.csv"), stringsAsFactors = FALSE)
read_body <- function(rel) jsonlite::fromJSON(fx(rel), simplifyVector = FALSE)
read_oracle <- function(key) {
  d <- dtypes[dtypes$key == key, ]
  utils::read.csv(fx("parity", paste0(key, ".csv")),
    colClasses = stats::setNames(d$type, d$column), na.strings = "<NA>",
    check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
  )
}
oracle_key <- function(i) paste0(manifest$league[i], "_", manifest$route[i], if (manifest$kind[i] == "full") "_full" else "")
the_call <- function(i) {
  args <- jsonlite::fromJSON(manifest$args[i], simplifyVector = TRUE)
  do.call(get(paste0(manifest$league[i], "_", manifest$route[i])), as.list(args))
}

# Mock the transport: record the URL asked, answer `body`.
serve_body <- function(body, env = parent.frame()) {
  asked <- new.env()
  asked$urls <- character()
  testthat::local_mocked_bindings(
    .euro_get = function(url, query = list(), headers = NULL, ...) {
      asked$urls <- c(asked$urls, url)
      body
    },
    .env = env
  )
  asked
}

# Mock the HTTP layer so .euro_get() runs its real status/JSON checks.
serve_http <- function(status, text, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .polite_perform = function(req, ...) {
      httr2::response(status_code = status, url = req$url, body = charToRaw(text),
                      headers = list(`Content-Type` = "application/json"))
    },
    .env = env
  )
}

# NA-vs-value counts as a mismatch; numbers to 1e-9 relative.
count_mismatches <- function(r, py) {
  bad <- 0L
  for (col in names(py)) {
    a <- r[[col]]
    b <- py[[col]]
    na_a <- is.na(a)
    na_b <- is.na(b)
    bad <- bad + sum(na_a != na_b)
    both <- !na_a & !na_b
    if (is.numeric(b)) {
      bad <- bad + sum(abs(as.numeric(a[both]) - b[both]) > 1e-9 * pmax(1, abs(b[both])))
    } else {
      bad <- bad + sum(a[both] != b[both])
    }
  }
  bad
}

leagues <- c(del = 12L, icehl = 12L, eihl = 9L, metal = 8L)

test_that("each league exports the sdv-py route set, with its signatures", {
  skip_on_cran()
  ns <- ls(asNamespace("fastRhockey"))
  for (lg in names(leagues)) {
    fns <- grep(paste0("^", lg, "_"), ns, value = TRUE)
    expect_length(fns, leagues[[lg]])
    expect_setequal(fns, paste0(lg, "_", manifest$route[manifest$league == lg & manifest$kind == "base"]))
  }
  expect_named(formals(del_schedule), c("season", "league_id", "return_parsed"))
  expect_identical(formals(del_schedule)$league_id, 1L)
  expect_identical(formals(eihl_schedule)$league_id, 3L)
  expect_identical(formals(icehl_schedule)$league_id, 1L)
  expect_identical(formals(metal_standings)$league_id, 1L)
  expect_named(formals(del_team_player_stats), c("season", "team_id", "league_id", "return_parsed"))
  expect_named(formals(del_player_box), c("match_id", "team_id", "return_parsed"))
  expect_named(formals(icehl_league_part_player_stats), c("league_part_id", "return_parsed"))
  expect_named(formals(metal_pbp), c("match_id", "return_parsed"))
})

test_that("the registry matches the wrappers: league default, paths per bucket", {
  skip_on_cran()
  reg <- .esports_leagues()
  expect_equal(vapply(reg, function(x) x$league_id, 1L), c(del = 1L, icehl = 1L, eihl = 3L, metal = 1L))
  expect_false("shots" %in% names(reg$metal$routes)) # Denmark: empty shells only
  expect_true(all(c("roster", "shifts") %in% names(reg$del$routes)))
  expect_false(any(c("roster", "shifts") %in% c(names(reg$icehl$routes), names(reg$eihl$routes), names(reg$metal$routes))))
  expect_match(reg$eihl$routes$game_info, "game-info.json", fixed = TRUE)
  expect_match(reg$del$routes$standings, "/tables/", fixed = TRUE)
  expect_match(reg$metal$routes$standings, "/table/", fixed = TRUE)
})

for (i in seq_len(nrow(manifest))) {
  local({
    i <- i
    key <- oracle_key(i)
    test_that(paste("parity with sdv-py:", key), {
      skip_on_cran()
      body <- read_body(manifest$file[i])
      asked <- serve_body(body)
      r <- the_call(i)
      py <- read_oracle(key)
      # the URL asked: the END-year season is the bucket's START year
      expect_identical(asked$urls, manifest$url[i])
      expect_s3_class(r, "fastRhockey_data")
      expect_identical(nrow(r), manifest$rows[i])
      # same column set, same order, same types as sdv-py's frame
      expect_identical(names(r), names(py))
      r_types <- vapply(r, function(x) if (is.logical(x)) "logical" else if (is.integer(x)) "integer" else if (is.double(x)) "numeric" else "character", "")
      py_types <- dtypes$type[dtypes$key == key]
      expect_identical(unname(r_types), py_types)
      expect_identical(count_mismatches(r, py), 0L)
      # every id column is one integer type
      ids <- grep("(^|_)id$", names(r), value = TRUE)
      expect_true(all(vapply(r[ids], is.integer, NA)))
      # the raw body comes back untouched when asked for
      expect_identical(do.call(
        get(paste0(manifest$league[i], "_", manifest$route[i])),
        c(as.list(jsonlite::fromJSON(manifest$args[i])), list(return_parsed = FALSE))
      ), body)
    })
  })
}

test_that("the documented columns equal sdv-py's schema for every route", {
  skip_on_cran()
  for (lg in names(leagues)) {
    for (route in manifest$route[manifest$league == lg & manifest$kind == "base"]) {
      cols <- names(.es_schema(lg, route))
      py <- dtypes$column[dtypes$key == paste0(lg, "_", route)]
      expect_identical(cols, py[seq_along(cols)], info = paste(lg, route))
    }
  }
})

test_that("an unusable payload is a zero-row tibble with the documented columns and types", {
  skip_on_cran()
  bad <- list(NULL, list(), setNames(list(), character()), "", "x", 7, list(1, 2, 3))
  for (lg in names(leagues)) {
    for (route in manifest$route[manifest$league == lg & manifest$kind == "base"]) {
      schema <- .es_tags(.es_schema(lg, route))
      for (payload in bad) {
        r <- .es_parse(route, payload, lg)
        expect_s3_class(r, "tbl_df")
        expect_identical(nrow(r), 0L)
        expect_identical(names(r), names(schema))
        expect_identical(unname(vapply(r, function(x) if (is.integer(x)) "integer" else if (is.double(x)) "double" else if (is.logical(x)) "logical" else "character", "")), unname(schema))
      }
    }
  }
})

test_that("a heterogeneous body is a tibble, not an error", {
  skip_on_cran()
  payloads <- list(
    list(list(id = 7L), list(id = "8")),
    list(list(x = list(1L, 2L)), list(x = 5L)),
    list(list(x = list(a = 1L)), list(x = 5L)),
    list(list(HomeTeam = 1L, homeTeam = 2L))
  )
  for (route in names(.ES_PARSERS)) {
    for (p in payloads) expect_s3_class(.es_parse(route, p, "del"), "tbl_df")
  }
  g <- .es_parse("top_scorers", payloads[[1]], "del")
  expect_identical(nrow(g), 2L)
  expect_type(g$id, "integer") # "8" survives the cast, so the id column is integer
})

test_that("an all-null column takes its schema type, not the name rule", {
  skip_on_cran()
  raw <- read_body("del/tables__season__league_id.json")
  for (b in seq_along(raw)) for (t in seq_along(raw[[b]]$tableData$table)) raw[[b]]$tableData$table[[t]]["position"] <- list(NULL)
  expect_type(.es_parse("standings", raw, "del")$position, "integer")
})

test_that("an undocumented column follows the documented ones", {
  skip_on_cran()
  raw <- read_body("del/matches__match_id__shiftsSC.json")
  raw[[1]]$brandNew <- 1L
  r <- .es_parse("shifts", raw, "del")
  expect_identical(names(r), c(names(.es_schema("del", "shifts")), "brand_new"))
})

test_that("a pbp without penalties still carries the penalty columns", {
  skip_on_cran()
  for (lg in names(leagues)) {
    raw <- read_body(manifest$file[manifest$league == lg & manifest$route == "pbp" & manifest$kind == "base"])
    bare <- lapply(raw, function(v) if (is.list(v) && is.null(names(v))) Filter(function(e) !identical(e$type, "penalty"), v) else v)
    full <- .es_parse("pbp", raw, lg)
    partial <- .es_parse("pbp", bare, lg)
    expect_identical(names(partial), names(full))
    expect_identical(vapply(partial, typeof, ""), vapply(full, typeof, ""))
    penalty <- grep("^(penalty_|penalized_)", names(partial), value = TRUE)
    expect_length(penalty, 7L)
    expect_true(all(vapply(partial[penalty], function(x) all(is.na(x)), NA)))
  }
})

test_that("shots: ids, goal codes, xg, sides; pbp and shots share the playerId key space", {
  skip_on_cran()
  for (lg in c("del", "icehl", "eihl")) {
    shots <- .es_parse("shots", read_body(paste0(lg, "/full/visualization__shots__match_id.json")), lg)
    expect_type(shots$match_id, "integer")
    expect_length(unique(shots$match_id), 1L)
    expect_identical(shots$shot_result_id, shots$match_shot_resutl_id)
    expect_identical(shots$is_goal, shots$shot_result_id == 4L)
    expect_type(shots$xg, "double")
    expect_type(shots$coordinate_x, "double")
    expect_identical(!anyNA(shots$xg), lg != "del")
    home <- shots[shots$team_side %in% "home", ]
    away <- shots[shots$team_side %in% "visitor", ]
    expect_identical(nrow(home) + nrow(away), nrow(shots))
    wrong <- rbind(home[home$coordinate_x < 0, ], away[away$coordinate_x > 0, ])
    # one known exception: an EIHL 4440 visitor goal recorded at x = +70 (shot 786469)
    expect_identical(wrong$shot_id, if (lg == "eihl") 786469L else integer())
  }
  shots <- .es_parse("shots", read_body("del/full/visualization__shots__match_id.json"), "del")
  pbp <- .es_parse("pbp", read_body("del/full/matches__match_id__period-events.json"), "del")
  goals <- pbp$scorer_player_id[pbp$event_type %in% "goal"]
  scored <- shots$player_id[shots$is_goal %in% TRUE]
  expect_type(goals, "integer")
  expect_type(scored, "integer")
  expect_identical(sort(goals), sort(scored))
})

test_that("pbp flattens periods and penalty windows; overtime and shootout keys", {
  skip_on_cran()
  r <- .es_parse("pbp", read_body("del/full/matches__match_id__period-events.json"), "del")
  expect_identical(r$event_index, seq_len(nrow(r)) - 1L)
  expect_false(is.unsorted(r$period_number))
  expect_type(r$time, "integer") # DEL writes periodEnd times as 1200.0
  expect_identical(sum(r$event_type == "goal"), 7L)
  pen <- r[r$event_type == "penalty", ]
  expect_identical(nrow(pen), 8L)
  expect_false(anyNA(pen$penalty_from_scoreboard_time))
  expect_false(anyNA(pen$penalty_to_real_time))
  raw <- list(`1` = list(list(time = 5L, type = "goal")), overtime = list(list(time = 3700L, type = "goal")),
              shootout = list(list(time = 3900L, type = "goal")))
  o <- .es_parse("pbp", raw, "del")
  expect_identical(o$period, c("1", "OT", "SO"))
  expect_identical(o$period_number, c(1L, 4L, 5L))
})

test_that("schedule: season is the END year, round has no BOM, overtime_count is integer", {
  skip_on_cran()
  r <- .es_parse("schedule", read_body("del/full/league-matches__season__league_id.json"), "del")
  expect_identical(unique(r$season), 2027L)
  expect_identical(nrow(r), 364L)
  expect_identical(length(unique(r$match_id)), 364L)
  expect_false(any(grepl(intToUtf8(0xFEFF), r$round, fixed = TRUE)))
  expect_type(r$overtime_count, "integer")
})

test_that("standings read both shapes; player_box sums the venue split; lineup and game_info", {
  skip_on_cran()
  de <- .es_parse("standings", read_body("del/tables__season__league_id.json"), "del")
  dk <- .es_parse("standings", read_body("metal/table__season__league_id.json"), "metal")
  expect_true(all(c("league_part_id", "team_id", "points") %in% names(de)))
  expect_true(all(c("group_name", "team_id", "points") %in% names(dk)))
  box <- .es_parse("player_box", read_body("del/matches__match_id__team-stats__team_id.json"), "del")
  expect_identical(names(box)[1], "player_id")
  expect_identical(box$goals, box$goals_home + box$goals_away)
  hdr <- read_body("icehl/matches__match_id__game-header.json")
  ros <- .es_parse("roster", read_body("del/matches__match_id__roster.json"), "del")
  ice <- .es_rows_lineup(hdr)
  expect_true(all(c("team_side", "slot", "player_id", "match_player_id") %in% names(ros)))
  expect_true(length(ice) > 0)
  info <- .es_parse("game_info", hdr, "icehl")
  expect_identical(nrow(info), 1L)
  expect_false(any(startsWith(names(info), "roster")))
})

test_that("keys, python-style numbers and list cells", {
  skip_on_cran()
  expect_identical(.es_key("xG"), "xg")
  expect_identical(.es_key("savesPercent"), "saves_percent")
  expect_identical(.es_key("toi/gp"), "toi_gp")
  expect_identical(.es_key("looser"), "loser")
  expect_identical(.es_fmt_double(2), "2.0")
  expect_identical(.es_fmt_double(0.1), "0.1")
  expect_identical(.es_dumps(list(1L, list(a = "x", b = NULL))), "[1, {\"a\": \"x\", \"b\": null}]")
  expect_true(.es_pyeq(96L, 96))
  expect_false(.es_pyeq("96", 96L))
  expect_true(.es_pyeq(NULL, NULL))
})

# ---------------------------------------------------------------------------
# Season mapping and refusals
# ---------------------------------------------------------------------------

test_that("season is the END year; the bucket key is the START year", {
  skip_on_cran()
  expect_identical(.season_start_year(2027), 2026L)
  expect_identical(.season_start_year("2017"), 2016L)
  expect_error(.season_start_year(27), class = "fastRhockey_bad_request")
  body <- read_body("metal/league-matches__season__league_id.json")
  asked <- serve_body(body)
  metal_schedule(season = 2027)
  expect_match(asked$urls, "den.hokejovyzapis.cz/league-matches/2026/1.json", fixed = TRUE)
  expect_error(metal_schedule(season = 27), class = "fastRhockey_bad_request")
})

test_that("loaders leave the caller's options alone", {
  skip_on_cran()
  serve_body(read_body("metal/league-matches__season__league_id.json"))
  withr::local_options(scipen = 7L)
  metal_schedule(season = 2027)
  expect_identical(getOption("scipen"), 7L)
})

xml <- function(code) paste0('<?xml version="1.0" encoding="UTF-8"?><Error><Code>', code, "</Code><Message>m</Message></Error>")
B <- "https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz"
js <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")

test_that("refusals: S3 answers", {
  skip_on_cran()
  serve_http(404, xml("NoSuchKey"))
  expect_error(metal_pbp(match_id = 1), class = "fastRhockey_no_data")
  serve_http(403, xml("AccessDenied"))
  expect_error(metal_pbp(match_id = 1), class = "fastRhockey_asset_fetch")
  serve_http(200, xml("NoSuchKey")) # a 200 that is not JSON
  expect_error(metal_pbp(match_id = 1), class = "fastRhockey_asset_fetch")
  serve_http(400, "bad")
  expect_error(metal_pbp(match_id = 1), class = "fastRhockey_bad_request")
  serve_http(500, "boom")
  expect_error(metal_pbp(match_id = 1), class = "fastRhockey_asset_fetch")
})

test_that("refusals: bodies that are not data", {
  skip_on_cran()
  refuse <- function(call, body, class) {
    serve_http(200, as.character(js(body)))
    expect_error(call, class = class)
  }
  refuse(metal_goalie_box(match_id = 1), list(error = "x", message = "y"), "fastRhockey_asset_fetch")
  refuse(metal_goalie_box(match_id = 1), list(), "fastRhockey_no_data")
  serve_http(200, "{}")
  expect_error(metal_goalie_box(match_id = 1), class = "fastRhockey_no_data")
  serve_http(200, "[]")
  expect_error(metal_goalie_box(match_id = 1), class = "fastRhockey_no_data")
  serve_http(200, '{"matches": []}')
  expect_error(metal_schedule(season = 2027), class = "fastRhockey_no_data")
  serve_http(200, '{"matches": [{"id": 1, "season": "2025"}]}') # another season
  expect_error(metal_schedule(season = 2027), class = "fastRhockey_asset_fetch")
  serve_http(200, '[{"id": 1, "season": "2026"}]') # a list where the schedule is an object
  expect_error(metal_schedule(season = 2027), class = "fastRhockey_asset_fetch")
  # Denmark's shot shell: an empty file for every game (icehl routes share the check)
  serve_http(200, '{"match": {"id": "575", "shots": [], "status": "LIVE"}}')
  expect_error(icehl_shots(match_id = 575), class = "fastRhockey_no_data")
  serve_http(200, '{"match": {"id": "576", "shots": [{"id": 1}]}}') # another match
  expect_error(icehl_shots(match_id = 575), class = "fastRhockey_asset_fetch")
  serve_http(200, '[{"id": 1}]')
  expect_error(icehl_shots(match_id = 575), class = "fastRhockey_asset_fetch")
  serve_http(200, '{"data": {"gameData": {"id": 576}}}') # another match
  expect_error(metal_game_center(match_id = 575), class = "fastRhockey_asset_fetch")
  serve_http(200, '[{"id": 575}]')
  expect_error(metal_game_center(match_id = 575), class = "fastRhockey_asset_fetch")
  serve_http(200, '{"periods": {"1": {"start": null}}, "1": [], "2": [], "3": [], "overtime": [], "shootout": []}')
  expect_error(metal_pbp(match_id = 575), class = "fastRhockey_no_data")
  serve_http(200, '[{"type": "goal"}]')
  expect_error(metal_pbp(match_id = 575), class = "fastRhockey_asset_fetch")
})

test_that("refusals: an unknown league, route or a missing id is a bad request", {
  skip_on_cran()
  expect_error(.esports_get("khl", "schedule", season = 2027), class = "fastRhockey_bad_request")
  expect_error(.esports_get("metal", "shots", match_id = 1), class = "fastRhockey_bad_request")
  expect_error(.esports_get("del", "pbp"), class = "fastRhockey_bad_request")
  expect_error(del_pbp(), "match_id")
})

test_that("a good body is returned and parsed after the throttle", {
  skip_on_cran()
  body <- read_body("metal/league-matches__season__league_id.json")
  serve_http(200, as.character(js(body)))
  expect_gt(nrow(metal_schedule(season = 2027)), 0L)
})

# ---------------------------------------------------------------------------
# Live (RUN_EURO_TESTS=true)
# ---------------------------------------------------------------------------

test_that("live: the DEL schedule and standings answer", {
  skip_on_cran()
  skip_on_cran()
  skip_euro_test()
  s <- del_schedule(season = 2027)
  expect_gt(nrow(s), 0L)
  expect_identical(unique(s$season), 2027L)
  Sys.sleep(1)
  expect_gt(nrow(del_standings(season = 2027)), 0L)
})

test_that(".es_cast writes whole doubles to character without a decimal part", {
  expect_identical(.es_cast(c(3e9, 12, NA), "double", "character"), c("3000000000", "12", NA))
  expect_identical(.es_cast(1.5, "double", "character"), "1.5")
})
