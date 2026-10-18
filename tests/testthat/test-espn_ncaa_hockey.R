# test-espn_ncaa_hockey.R
# espn_mch_*() / espn_wch_*(): ESPN men's / women's college hockey.
#
# Fixture tests (always run): sdv-py's own mch/wch payloads, a getter mock, and
# CSVs of the sdv-py frames built on the SAME fixtures. Live tests are gated by
# skip_euro_test() (RUN_EURO_TESTS=true).

fx <- function(...) testthat::test_path("fixtures", "espn_ncaa_hockey", ...)
read_fx <- function(f) paste(readLines(fx(f), warn = FALSE), collapse = "\n")

SLUG <- c(mch = "mens-college-hockey", wch = "womens-college-hockey")

json_response <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

# Mock the package getter: answer every request with `body`, record the URLs.
mock_getter <- function(body = "{}", status = 200L, env = parent.frame()) {
  seen <- new.env()
  seen$urls <- character()
  seen$params <- list()
  testthat::local_mocked_bindings(
    .retry_request = function(url, params = list(), ...) {
      seen$urls <- c(seen$urls, url)
      seen$params[[length(seen$params) + 1L]] <- params
      json_response(body, status)
    },
    .package = "fastRhockey",
    .env = env
  )
  seen
}

# sdv-py frame (CSV) vs R tibble: NA-vs-value counts as a mismatch.
parity_mismatches <- function(r, csv, cols = intersect(names(r), names(csv))) {
  vapply(cols, function(cn) {
    a <- r[[cn]]
    b <- csv[[cn]]
    if (is.logical(a)) b <- as.logical(b)
    else if (is.numeric(a)) b <- suppressWarnings(as.numeric(b))
    else a <- as.character(a)
    na_diff <- sum(is.na(a) != is.na(b))
    both <- !is.na(a) & !is.na(b)
    val_diff <- if (is.numeric(a)) sum(abs(a[both] - b[both]) > 1e-9) else sum(a[both] != b[both])
    as.integer(na_diff + val_diff)
  }, integer(1))
}
read_parity <- function(f) {
  utils::read.csv(fx("parity", f), na.strings = "<NA>", colClasses = "character",
                  check.names = FALSE, strip.white = FALSE, encoding = "UTF-8")
}

routes <- utils::read.csv(fx("routes.csv"), stringsAsFactors = FALSE, na.strings = character())

DUMMY <- list(
  event_id = "E1", team_id = "T1", athlete_id = "A1", season = 2026, season_type = 2,
  week = 3, group_id = "G1", play_id = "P1", official_id = "O1", coach_id = "C1",
  venue_id = "V1", franchise_id = "F1", position_id = "X1", award_id = "W1",
  round_num = 1, year = 2026, opp_id = "A2"
)

# ---------------------------------------------------------------------------
# 1. Surface: the same 118 names as sdv-py, per league
# ---------------------------------------------------------------------------
test_that("espn_mch_*/espn_wch_* export the sdv-py function set", {
  skip_on_cran()
  expect_equal(nrow(routes), 118L)
  for (lg in names(SLUG)) {
    for (fn in routes$fn) {
      nm <- paste0("espn_", lg, "_", fn)
      expect_true(exists(nm, envir = asNamespace("fastRhockey"), inherits = FALSE), info = nm)
    }
  }
})

# ---------------------------------------------------------------------------
# 2. Routing: every wrapper requests the sdv-py URL for its league slug
# ---------------------------------------------------------------------------
# Routes where R deliberately builds a different path than sdv-py. Pinned so a
# silent change is noticed.
# calendar: sdv-py's site-v2 /calendar answers 404 for both college leagues
# (checked live 2026-10-18), so R keeps reading leagues[].calendar out of the
# scoreboard payload, which answers.
KNOWN_ROUTE_DEVIATIONS <- c("calendar")

test_that("every wrapper hits the sdv-py URL for its league", {
  skip_on_cran()
  bad <- character()
  for (lg in names(SLUG)) {
    prefix <- c(mch = "mens", wch = "womens")[[lg]]
    for (i in seq_len(nrow(routes))) {
      fn <- get(paste0("espn_", lg, "_", routes$fn[i]), envir = asNamespace("fastRhockey"))
      req_r <- setdiff(names(Filter(function(x) identical(x, quote(expr = )), formals(fn))), "...")
      req_py <- if (nzchar(routes$args[i])) sub("=.*", "", strsplit(routes$args[i], ";")[[1]]) else character()
      args <- DUMMY[union(req_r, intersect(req_py, names(formals(fn))))]
      seen <- mock_getter("{}")
      suppressMessages(do.call(fn, args))
      expected <- sub("{slug}", prefix, routes$url[i], fixed = TRUE)
      got <- seen$urls[1]
      if (is.na(got) || !identical(got, expected)) {
        bad <- c(bad, sprintf("%s_%s: got %s | want %s", lg, routes$fn[i], got, expected))
      }
    }
  }
  expect_setequal(sub(":.*$", "", sub("^(mch|wch)_", "", bad)), KNOWN_ROUTE_DEVIATIONS)
  expect_length(bad, 2L * length(KNOWN_ROUTE_DEVIATIONS))
})

test_that("coach routes keep the NHL path order; college uses sdv-py's", {
  skip_on_cran()
  seen <- mock_getter("{}")
  suppressMessages(espn_nhl_coach_season(coach_id = "C1", season = 2026))
  expect_match(seen$urls[1], "/leagues/nhl/seasons/2026/coaches/C1$")
  seen <- mock_getter("{}")
  suppressMessages(espn_nhl_coach_record(coach_id = "C1"))
  expect_match(seen$urls[1], "/leagues/nhl/coaches/C1/record$")
  seen <- mock_getter("{}")
  suppressMessages(espn_wch_coach_record(coach_id = "C1", record_type = 2))
  expect_match(seen$urls[1], "/leagues/womens-college-hockey/coaches/C1/record/2$")
})

test_that("pbp / team_box / player_box shims read the summary route", {
  skip_on_cran()
  for (lg in names(SLUG)) {
    for (k in c("pbp", "team_box", "player_box")) {
      fn <- get(paste0("espn_", lg, "_", k), envir = asNamespace("fastRhockey"))
      expect_identical(names(formals(fn)),
                       names(formals(get(paste0("espn_nhl_", k), envir = asNamespace("fastRhockey")))))
      seen <- mock_getter(read_fx(paste0(lg, "_summary.json")))
      x <- fn(game_id = "E1")
      expect_identical(seen$urls[1], paste0(
        "https://site.api.espn.com/apis/site/v2/sports/hockey/", SLUG[[lg]], "/summary"))
      expect_identical(seen$params[[1]]$event, "E1")
      expect_s3_class(x, "fastRhockey_data")
      expect_gt(nrow(x), 0L)
    }
  }
})

# ---------------------------------------------------------------------------
# 3. Season mapping: END year goes to the provider as given
# ---------------------------------------------------------------------------
test_that("season is passed through as the END year", {
  skip_on_cran()
  seen <- mock_getter("{}")
  suppressMessages(espn_mch_standings(season = 2026))
  expect_equal(seen$params[[1]]$season, 2026)
  seen <- mock_getter("{}")
  suppressMessages(espn_wch_season_info(season = 2026))
  expect_match(seen$urls[1], "/leagues/womens-college-hockey/seasons/2026$")
})

# ---------------------------------------------------------------------------
# 4. Scoreboard: parity with the sdv-py frame on the same fixture
# ---------------------------------------------------------------------------
for (lg in names(SLUG)) {
  local({
    lg <- lg
    test_that(paste0("espn_", lg, "_scoreboard matches sdv-py on the fixture"), {
  skip_on_cran()
      body <- paste0('{"events": ', read_fx(paste0(lg, "_scoreboard_sample.json")), "}")
      mock_getter(body)
      x <- get(paste0("espn_", lg, "_scoreboard"))(dates = "20250110")
      csv <- read_parity(paste0(lg, "_scoreboard.csv"))
      expect_s3_class(x, "fastRhockey_data")
      expect_equal(sort(names(x)), sort(names(csv)))
      expect_equal(nrow(x), nrow(csv))
      mm <- parity_mismatches(x, csv)
      # Both sides join every network name with ", " (sdv-py _broadcast_names);
      # the CSV is sdv-py main's frame on the same fixture. Zero mismatches.
      expect_identical(names(mm)[mm > 0], character())
      if (lg == "wch") expect_true("ESPNU, ESPN+" %in% x$broadcast)
      # id dtypes (one dtype per id, as in sdv-py)
      expect_type(x$game_id, "character")
      expect_type(x$home_id, "character")
      expect_type(x$away_id, "character")
      expect_type(x$season_year, "integer")
    })
  })
}

test_that("scoreboard: an empty payload is a zero-row frame; a 500 is not data", {
  skip_on_cran()
  mock_getter('{"events": []}')
  expect_equal(nrow(suppressMessages(espn_mch_scoreboard(dates = "20250101"))), 0L)
  mock_getter("{}", status = 500L)
  expect_equal(nrow(suppressMessages(espn_mch_scoreboard(dates = "20250101"))), 0L)
  mock_getter("{}", status = 404L)
  expect_equal(nrow(suppressMessages(espn_wch_scoreboard(dates = "20250101"))), 0L)
})

# ---------------------------------------------------------------------------
# 5. Rankings (new route, not in espn_nhl_*): exact parity incl. dtypes
# ---------------------------------------------------------------------------
for (lg in names(SLUG)) {
  local({
    lg <- lg
    test_that(paste0("espn_", lg, "_rankings matches sdv-py on the fixture"), {
  skip_on_cran()
      seen <- mock_getter(read_fx(paste0("rankings_", lg, ".json")))
      x <- get(paste0("espn_", lg, "_rankings"))()
      csv <- read_parity(paste0(lg, "_rankings.csv"))
      expect_match(seen$urls[1], paste0("/hockey/", SLUG[[lg]], "/rankings$"))
      expect_s3_class(x, "fastRhockey_data")
      expect_identical(names(x), names(csv))
      expect_equal(nrow(x), nrow(csv))
      mm <- parity_mismatches(x, csv)
      expect_equal(sum(mm), 0L, info = paste(names(mm)[mm > 0], collapse = ", "))
      expect_identical(
        vapply(x, function(v) class(v)[1], character(1), USE.NAMES = TRUE)[c("poll_id", "ranked", "rank", "points", "team_id")],
        c(poll_id = "integer", ranked = "logical", rank = "integer", points = "numeric", team_id = "character")
      )
      expect_true(all(is.na(x$rank[!x$ranked])))
    })
  })
}

test_that("rankings: empty payload keeps the 25 documented columns", {
  skip_on_cran()
  mock_getter('{"rankings": []}')
  x <- espn_mch_rankings()
  expect_equal(nrow(x), 0L)
  expect_equal(ncol(x), 25L)
  expect_identical(names(x), names(read_parity("mch_rankings.csv")))
})

# ---------------------------------------------------------------------------
# 6. Game plays: overlapping columns equal the sdv-py frame
# ---------------------------------------------------------------------------
for (lg in names(SLUG)) {
  local({
    lg <- lg
    test_that(paste0("espn_", lg, "_game_plays agrees with sdv-py on shared columns"), {
  skip_on_cran()
      mock_getter(read_fx(paste0(lg, "_game_plays.json")))
      x <- get(paste0("espn_", lg, "_game_plays"))(event_id = "401717648")
      csv <- read_parity(paste0(lg, "_game_plays.csv"))
      expect_equal(nrow(x), nrow(csv))
      shared <- intersect(names(x), names(csv))
      expect_true(all(c("id", "sequence_number", "type_text", "text", "away_score", "home_score",
                        "scoring_play", "is_penalty", "wallclock") %in% shared))
      mm <- parity_mismatches(x, csv, shared)
      expect_equal(sum(mm), 0L, info = paste(names(mm)[mm > 0], collapse = ", "))
      expect_type(x$id, "character")
    })
  })
}

test_that("game_plays: a completed game can have zero plays", {
  skip_on_cran()
  mock_getter(read_fx("mch_game_plays_empty_sample.json"))
  expect_equal(nrow(suppressMessages(espn_mch_game_plays(event_id = "401711791"))), 0L)
})

# ---------------------------------------------------------------------------
# 7. Summary: the raw nested list, with the sdv-py section keys
# ---------------------------------------------------------------------------
test_that("summary returns the nested payload (event_id, as in sdv-py)", {
  skip_on_cran()
  for (lg in names(SLUG)) {
    seen <- mock_getter(read_fx(paste0(lg, "_summary.json")))
    s <- get(paste0("espn_", lg, "_summary"))(event_id = "401717648")
    expect_match(seen$urls[1], paste0("/hockey/", SLUG[[lg]], "/summary$"))
    expect_true(all(c("boxscore", "gameInfo", "plays", "header") %in% names(s)))
  }
})

# ---------------------------------------------------------------------------
# 8. Hand-written routes: items collections and power index
# ---------------------------------------------------------------------------
test_that("core-v2 items routes: ref + id, zero rows when empty", {
  skip_on_cran()
  mock_getter('{"items": [{"$ref": "http://x/recruiting/2026?lang=en"}, {"$ref": "http://x/recruiting/2025"}]}')
  x <- espn_mch_recruiting_years()
  expect_s3_class(x, "fastRhockey_data")
  expect_identical(x$year, c("2026", "2025"))
  expect_identical(names(x), c("ref", "year"))
  mock_getter('{"items": []}')
  expect_equal(nrow(espn_wch_recruiting_players(year = 2026)), 0L)
  mock_getter('{"items": [{"$ref": "http://x/rankings/9"}]}')
  y <- espn_wch_season_week_rankings(season = 2026, season_type = 2, week = 3)
  expect_equal(y$ranking_id, "9")
  expect_equal(y$week, 3L)
})

test_that("standings_core: a bare $ref pointer is one ref row", {
  skip_on_cran()
  mock_getter('{"$ref": "http://x/leagues/mens-college-hockey/seasons/2027/types/2/groups/51/standings?lang=en"}')
  x <- espn_mch_standings_core()
  expect_equal(nrow(x), 1L)
  expect_match(x$ref, "groups/51/standings")
})

test_that("fpi and weekly power index: labels zipped by position, empty is zero rows", {
  skip_on_cran()
  fpi <- paste0(
    '{"requestedSeason": 2026, "categories": [{"name": "fpi", "names": ["FPI", "FPI RANK"]}],',
    ' "teams": [{"team": {"id": "5", "uid": "s:1", "abbreviation": "AAA", "displayName": "A U",',
    ' "shortDisplayName": "A", "nickname": "Aa"}, "categories": [{"name": "fpi", "values": [1.5, 2]}]}]}'
  )
  seen <- mock_getter(fpi)
  x <- espn_mch_fpi(season = 2026)
  expect_match(seen$urls[1], "/apis/fitt/v3/sports/hockey/mens-college-hockey/powerindex$")
  expect_identical(x$team_id, "5")
  expect_equal(x$fpi, 1.5)
  expect_equal(x$fpi_rank, 2)
  mock_getter("{}")
  expect_equal(nrow(suppressMessages(espn_wch_fpi())), 0L)

  wk <- paste0(
    '{"items": [{"team": {"$ref": "http://x/seasons/2026/teams/104?lang=en"}, "season": 2026,',
    ' "lastUpdated": "t", "runDateTimeKey": "k", "predictives": [{"name": "teamRating", "value": 2.5}],',
    ' "efficiencies": [{"name": "teamRating", "value": 3.5}]}]}'
  )
  mock_getter(wk)
  z <- espn_mch_season_week_powerindex(season = 2026, season_type = 2, week = 3)
  expect_identical(z$team_id, 104L)
  expect_equal(z$team_rating, 2.5)
  expect_equal(z$team_rating_eff, 3.5)
})

# ---------------------------------------------------------------------------
# 9. Live (residential, gated)
# ---------------------------------------------------------------------------
test_that("LIVE: espn_mch_rankings / espn_wch_scoreboard answer", {
  skip_on_cran()
  skip_on_cran()
  skip_euro_test()
  x <- espn_mch_rankings()
  expect_true(is.data.frame(x))
  Sys.sleep(1)
  y <- espn_wch_scoreboard(dates = "20250110")
  expect_true(is.data.frame(y))
})

# ---------------------------------------------------------------------------
# F2: every verified @return table matches the columns the wrapper returns on
# the captured mch/wch body it was verified against (fixtures/.../bodies/).
# ---------------------------------------------------------------------------
doc_table <- function(src, name) {
  i <- grep(paste0("^#' @rdname ", name, "$"), src)
  j <- grep(paste0("^", name, " <- function"), src)
  rows <- regmatches(src[i:j], regexpr("^#'\\s+\\| [a-z0-9_]+ \\| [a-z]+ \\|", src[i:j]))
  rows <- rows[!grepl("| col_name |", rows, fixed = TRUE)]
  parts <- strsplit(sub("^#'\\s+\\| ", "", rows), " \\| ")
  stats::setNames(vapply(parts, function(x) sub(" \\|$", "", x[2]), ""), vapply(parts, `[`, "", 1))
}

test_that("verified @return tables match the columns on the captured bodies", {
  skip_on_cran()
  src_file <- testthat::test_path("..", "..", "R", "espn_ncaa_hockey.R")
  skip_if_not(file.exists(src_file), "package source not available")
  src <- readLines(src_file, warn = FALSE)
  args <- list(team = list(team_id = "2005"), team_schedule = list(team_id = "2005", season = 2027),
               standings = list(season = 2026), season_info = list(season = 2026),
               season_types = list(season = 2026))
  bodies <- list.files(fx("bodies"), pattern = "\\.json$")
  bodies <- c(bodies, "mch_calendar.json", "wch_calendar.json")
  for (b in bodies) {
    lg <- sub("_.*", "", b)
    fn <- sub("^[mw]ch_(.*)\\.json$", "\\1", b)
    if (fn %in% c("team_roster", "injuries", "transactions")) next  # empty in every capture
    path <- if (fn == "calendar") fx("bodies", paste0(lg, "_scoreboard.json")) else fx("bodies", b)
    mock_getter(read_fx(file.path("bodies", basename(path))))
    name <- paste0("espn_", lg, "_", fn)
    x <- suppressMessages(do.call(get(name, envir = asNamespace("fastRhockey")), args[[fn]] %||% list()))
    expect_gt(nrow(x), 0L, label = name)
    got <- vapply(x, function(col) class(col)[1], "")
    expect_identical(got, doc_table(src, name), label = name)
  }
  # sdv-py's committed mch/wch fixtures
  for (lg in names(SLUG)) {
    for (fn in c("pbp", "team_box", "player_box", "game_plays", "rankings")) {
      f <- switch(fn, game_plays = paste0(lg, "_game_plays.json"),
                  rankings = paste0("rankings_", lg, ".json"), paste0(lg, "_summary.json"))
      mock_getter(read_fx(f))
      name <- paste0("espn_", lg, "_", fn)
      a <- switch(fn, game_plays = list(event_id = "E1"), rankings = list(), list(game_id = "E1"))
      x <- suppressMessages(do.call(get(name, envir = asNamespace("fastRhockey")), a))
      expect_gt(nrow(x), 0L, label = name)
      expect_identical(vapply(x, function(col) class(col)[1], ""), doc_table(src, name), label = name)
    }
  }
})

test_that("broadcast names follow sdv-py's _broadcast_names", {
  skip_on_cran()
  bn <- fastRhockey:::.espn_broadcast_names
  expect_identical(bn(list()), "")
  expect_identical(bn(list(list(market = "national", names = list("ESPNU", "ESPN+")),
                           list(market = "home", names = list("ESPN+", "NESN")))), "ESPNU, ESPN+, NESN")
  expect_identical(bn(list(list(market = "national", names = list()))), "national")
  expect_identical(bn(list(list(market = list(id = "1"), names = list()))), "")
  expect_identical(bn(list(list(names = "ABC"))), "ABC")
  # simplified (data.frame) form, as the scoreboard reads it
  df <- jsonlite::fromJSON('[{"market":"national","names":["ESPNU","ESPN+"]},{"market":"away","names":[]}]')
  expect_identical(bn(df), "ESPNU, ESPN+, away")
})

test_that("team_schedule keeps played and unplayed games together", {
  skip_on_cran()
  mock_getter(read_fx(file.path("bodies", "mch_team_schedule.json")))
  x <- espn_mch_team_schedule(team_id = "2005", season = 2027)
  expect_equal(nrow(x), 3L)
  expect_type(x$home_score, "character")
  expect_equal(sum(is.na(x$home_score)), 1L)
})
