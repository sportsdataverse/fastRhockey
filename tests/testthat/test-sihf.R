# Offline tests for the SIHF family (R/sihf.R): 33 wrappers over
# https://data.sihf.ch/statistic/api/cms, at parity with sdv-py
# sportsdataverse.hockey.sihf (sdv-py #779 + #826).
#
# Fixtures: tests/testthat/fixtures/sihf/ holds sdv-py's real captures
# (tests/fixtures/sihf_api_web/), plus
#   columns.json         the documented columns of every route (sdv-py _PARSER_COLUMNS)
#   parity/<case>.csv    the sdv-py frame on that SAME fixture (polars; lists comma-joined,
#                        null = "<NA>"), written with the sdv-py oracle at origin/main
#   parity_dtypes.csv    the polars dtype of every column of every case
#   parity_cases.csv     case -> route -> fixture
# The HTTP getter (.euro_get) is mocked with the fixtures; nothing here touches the network
# except the live tests at the bottom (skip_euro_test()).

fx_path <- function(...) testthat::test_path("fixtures", "sihf", ...)

# A table fixture cut to its first rows still announces the full row count in `pages`
# (README): set it to the rows kept so the completeness check sees a whole table.
fx_body <- function(rel) {
  b <- jsonlite::fromJSON(fx_path(rel), simplifyVector = FALSE)
  if (is.numeric(b$pages) && b$pages > 0 && is.list(b$data)) b$pages <- length(b$data)
  b
}

mock_get <- function(body, calls = NULL) {
  force(body)
  function(url, query = list(), headers = NULL, ...) {
    if (!is.null(calls)) {
      calls$url <- c(calls$url, url)
      calls$query[[length(calls$query) + 1L]] <- query
    }
    if (is.function(body)) body(url, query) else body
  }
}
new_calls <- function() {
  e <- new.env()
  e$url <- character()
  e$query <- list()
  e
}

# A table body whose filters[].selected echo is `...` (each filter lists its value among its entries).
sihf_body <- function(rows = list(list("1", "x")), ...) {
  sel <- list(...)
  list(
    filters = unname(Map(function(k, v) list(alias = k, selected = v, entries = list(list(alias = v))),
                         names(sel), sel)),
    header = list(list(alias = "rank"), list(alias = "team")),
    data = rows
  )
}
# The honest answer to whatever filters the request carried.
echo_body <- function(url, query) {
  by <- strsplit(query$filterBy %||% "", ",")[[1]]
  qs <- strsplit(query$filterQuery %||% "", "/", fixed = TRUE)[[1]]
  if (length(by) > length(qs)) qs <- c(qs, rep("", length(by) - length(qs)))
  do.call(sihf_body, c(list(), stats::setNames(as.list(qs[seq_along(by)]), by)))
}

documented <- jsonlite::fromJSON(fx_path("columns.json"), simplifyVector = FALSE)
documented <- lapply(documented, unlist)
parity_cases <- utils::read.csv(fx_path("parity_cases.csv"), stringsAsFactors = FALSE)
parity_dtypes <- utils::read.csv(fx_path("parity_dtypes.csv"), stringsAsFactors = FALSE)

# The wrapper call for a fixture: <alias>__<season>__<league>[__phases], or gameoverview__<game>.
call_for <- function(route, fixture) {
  f <- basename(fixture)
  fn <- get(paste0("sihf_", route), envir = asNamespace("fastRhockey"))
  if (grepl("^gameoverview__", f)) {
    return(function() fn(game_id = sub("^gameoverview__([0-9]+)\\.json$", "\\1", f)))
  }
  m <- regmatches(f, regexec("^table__([A-Za-z]+)__([0-9]{4})__([0-9]+)(__phases)?\\.json$", f))[[1]]
  season <- as.integer(m[3])
  league <- as.integer(m[4])
  if (identical(route, "phases")) {
    tbl <- if (identical(m[2], "standing")) "standing" else m[2]
    return(function() fn(season = season, league_id = league, table = tbl))
  }
  function() fn(season = season, league_id = league)
}

r_class <- function(dtype, col) {
  if (identical(col, "game_id")) return("numeric")
  switch(sub("\\(.*$", "", dtype),
         Int64 = "integer", Float64 = "numeric", String = "character", Date = "Date",
         Boolean = "logical", List = "list", stop("unmapped dtype ", dtype))
}

# Count disagreements between an R column and the sdv-py text for it. An NA on one side and a
# value on the other counts as a mismatch (memory: parity_agreement_counts_na_mismatch).
column_mismatches <- function(x, p) {
  if (is.list(x)) {
    x <- vapply(x, function(v) {
      if (is.null(v)) NA_character_ else paste(ifelse(is.na(v), "NA", as.character(v)), collapse = ",")
    }, character(1))
  } else if (inherits(x, "Date")) {
    x <- as.character(x)
  } else if (is.logical(x)) {
    x <- ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
  }
  if (is.numeric(x)) {
    pn <- suppressWarnings(as.numeric(p))
    return(sum(is.na(x) != is.na(pn)) + sum(abs(x - pn) > 1e-9 * pmax(1, abs(pn)), na.rm = TRUE))
  }
  sum(is.na(x) != is.na(p)) + sum(x != p, na.rm = TRUE)
}

test_that("sihf: the family exports the 33 sdv-py wrappers and documents every route", {
  skip_on_cran()
  fns <- grep("^sihf_", getNamespaceExports("fastRhockey"), value = TRUE)
  expect_length(fns, 33L)
  expect_setequal(sub("^sihf_", "", fns), names(documented))
})

test_that("sihf parity: every route matches sdv-py on the same fixture, NA-vs-value counted", {
  skip_on_cran()
  expect_gte(nrow(parity_cases), 52L)
  for (i in seq_len(nrow(parity_cases))) {
    case <- parity_cases$case[i]
    route <- parity_cases$route[i]
    fixture <- parity_cases$fixture[i]
    local_mocked_bindings(.euro_get = mock_get(fx_body(fixture)), .package = "fastRhockey")
    got <- call_for(route, fixture)()
    py <- utils::read.csv(fx_path("parity", paste0(case, ".csv")), colClasses = "character",
                          na.strings = "<NA>", encoding = "UTF-8", check.names = FALSE,
                          stringsAsFactors = FALSE)
    dt <- parity_dtypes[parity_dtypes$case == case, ]
    info <- paste("case", case)
    expect_s3_class(got, "fastRhockey_data")
    expect_identical(names(got), names(py), info = info)
    expect_identical(names(got), dt$col, info = info)
    expect_setequal(names(got), documented[[route]])
    expect_identical(nrow(got), nrow(py), info = info)
    classes <- vapply(names(got), function(cn) class(got[[cn]])[1], character(1))
    expect_identical(unname(classes), unname(mapply(r_class, dt$dtype, dt$col)), info = info)
    bad <- vapply(names(got), function(cn) column_mismatches(got[[cn]], py[[cn]]), numeric(1))
    expect_true(all(bad == 0), info = paste(info, "mismatched:", paste(names(bad)[bad > 0], collapse = ", ")))
  }
})

test_that("sihf: ids are integers (game_id is numeric: 14 digits), quotients are numeric", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(fx_body("table__results__2026__1.json")), .package = "fastRhockey")
  r <- sihf_results(2026, 1)
  expect_type(r$game_id, "double")
  expect_identical(r$game_id[1], 20261105000001)
  for (id in c("season", "phase_id", "home_team_id", "away_team_id", "status_id")) expect_type(r[[id]], "integer")
  expect_s3_class(r$date, "Date")
  expect_identical(r$score_per_period_home[[1]], c(0L, 1L, 1L))
  expect_true(is.na(r$decision[1]))  # "" = regulation
  local_mocked_bindings(.euro_get = mock_get(fx_body("full/table__teamGoal__2026__1.json")), .package = "fastRhockey")
  g <- sihf_team_goals(2026)
  expect_type(g$goals_for_quotient, "double")
  expect_type(g$team_id, "integer")
  expect_type(g$goals_for, "integer")
})

test_that("sihf: summary rows are marked and their pseudo-team never joins to a real team", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(fx_body("full/table__teamSpectator__2026__1.json")), .package = "fastRhockey")
  a <- sihf_team_attendance(2026)
  expect_identical(sum(a$row_type == "total"), 1L)
  tot <- a[a$row_type == "total", ]
  expect_true(is.na(tot$team_id) && is.na(tot$team_acronym) && is.na(tot$rank))
  expect_identical(tot$team_name, "Total")
  expect_gt(tot$spectators, 3000000L)  # 3'458'381 once the thousands separator is dropped
  local_mocked_bindings(.euro_get = mock_get(fx_body("full/table__goalkeeperWin__2026__1.json")), .package = "fastRhockey")
  w <- sihf_goalie_wins(2026)
  expect_identical(sum(w$row_type == "average"), 1L)
  expect_true(is.na(w$team_id[w$row_type == "average"]))
  # an empty rank alone is NOT a summary row: goaltenders below the qualification line have one too
  expect_gt(sum(is.na(w$rank) & w$row_type == "data"), 10L)
})

test_that("sihf: a pseudo-team row untagged by highlightedPositions is still a summary row", {
  skip_on_cran()
  b <- fx_body("full/table__goalkeeperWin__2026__1.json")
  b$highlightedPositions <- list()
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  w <- sihf_goalie_wins(2026)
  expect_identical(sum(w$row_type == "average"), 1L)
  expect_true(is.na(w$team_acronym[w$row_type == "average"]))
})

# ---- request construction -------------------------------------------------------

test_that("sihf: the league travels in searchQuery; the other filters fold into one composite pair", {
  skip_on_cran()
  calls <- new_calls()
  local_mocked_bindings(.euro_get = mock_get(echo_body, calls), .package = "fastRhockey")
  suppressWarnings(try(sihf_player_stats(2026, league_id = 2L), silent = TRUE))
  q <- calls$query[[1]]
  expect_identical(calls$url[1], "https://data.sihf.ch/statistic/api/cms/table")
  expect_identical(q$searchQuery, "1//2")
  expect_identical(q$alias, "player")
  expect_identical(q$filterBy, "Season")
  expect_identical(q$filterQuery, "2026")
  expect_null(q$League)
  expect_null(q$Season)

  sihf_player_stats(2026, league_id = 1, phase_id = 4940)
  q <- calls$query[[2]]
  expect_identical(c(q$filterBy, q$filterQuery), c("Season,Phase", "2026/4940"))

  sihf_standings(2026)
  q <- calls$query[[3]]
  expect_identical(c(q$alias, q$filterBy, q$filterQuery), c("standing", "Season,ContentType", "2026/All"))
  sihf_standings(2026, phase_id = 7, venue = "Home")
  q <- calls$query[[4]]
  expect_identical(c(q$filterBy, q$filterQuery), c("Season,Phase,ContentType", "2026/7/Home"))
})

test_that("sihf: results always send the season-wide Date range (END year mapping)", {
  skip_on_cran()
  calls <- new_calls()
  local_mocked_bindings(.euro_get = mock_get(echo_body, calls), .package = "fastRhockey")
  sihf_results(2027)
  q <- calls$query[[1]]
  expect_identical(c(q$filterBy, q$filterQuery), c("Season,Date", "2027/01.07.2026-30.06.2027"))
  expect_identical(q$searchQuery, "1//1")
  sihf_results(2026, league_id = 100)
  expect_identical(calls$query[[2]]$filterQuery, "2026/01.07.2025-30.06.2026")
  expect_identical(calls$query[[2]]$searchQuery, "1//100")
  expect_identical(.sihf_season_date_range("2026"), "01.07.2025-30.06.2026")
  expect_identical(.sihf_season(2026L), "2026")
  expect_identical(.sihf_season("2026"), "2026")
})

test_that("sihf: unset filters are not sent; an empty Phase asks for the phase list", {
  skip_on_cran()
  calls <- new_calls()
  local_mocked_bindings(.euro_get = mock_get(echo_body, calls), .package = "fastRhockey")
  sihf_team_shots(2026, league_id = NULL)
  q <- calls$query[[1]]
  expect_identical(q$searchQuery, "1//")
  expect_identical(q$filterBy, "Season")
  sihf_phases(2026, 1, "player")
  q <- calls$query[[2]]
  expect_identical(c(q$alias, q$filterBy, q$filterQuery), c("player", "Season,Phase", "2026/"))
  sihf_phases(2026)  # the results table also needs the Date range
  q <- calls$query[[3]]
  expect_identical(c(q$alias, q$filterBy, q$filterQuery), c("results", "Season,Phase,Date", "2026//01.07.2025-30.06.2026"))
})

test_that("sihf: the game routes send the game id as the searchQuery, untouched", {
  skip_on_cran()
  calls <- new_calls()
  local_mocked_bindings(.euro_get = mock_get(fx_body("gameoverview__20261105000325.json"), calls),
                        .package = "fastRhockey")
  sihf_pbp(20261105000325)
  q <- calls$query[[1]]
  expect_identical(calls$url[1], "https://data.sihf.ch/statistic/api/cms/gameoverview")
  expect_identical(q$searchQuery, "20261105000325")  # not 2.0261105000325e+13
  expect_identical(q$alias, "gameDetail")
  expect_identical(q$language, "en")
  expect_null(q$filterBy)
  sihf_game_info("20261105000325", language = "de")
  expect_identical(calls$query[[2]]$language, "de")
})

test_that("sihf: the 22 detail routes send their alias", {
  skip_on_cran()
  aliases <- c(
    team_goals = "teamGoal", team_goals_by_position = "teamGoalPosNation", team_shots = "teamShotDetail",
    team_power_play = "teamPp", team_penalty_kill = "teamPk", team_penalties = "teamFoul",
    team_shootouts = "teamShootout", team_faceoffs = "teamFaceoff", team_faceoffs_by_zone = "teamFaceoffZone",
    team_faceoffs_by_zone_per_game = "teamFaceoffZoneGame", team_attendance = "teamSpectator",
    player_goals_assists = "playerGoalAssist", player_shots = "playerShotDetail", player_penalties = "playerFoul",
    player_shootouts = "playerShootout", player_faceoffs = "playerFaceoff",
    player_faceoffs_by_zone = "playerFaceoffZone", player_faceoffs_by_zone_per_game = "playerFaceoffZoneGame",
    player_time_on_ice = "playerTimeOnIce", goalie_shots = "goalkeeperShotDetail", goalie_wins = "goalkeeperWin",
    goalie_shootouts = "goalkeeperShootout"
  )
  for (short in names(aliases)) {
    calls <- new_calls()
    local_mocked_bindings(.euro_get = mock_get(echo_body, calls), .package = "fastRhockey")
    get(paste0("sihf_", short), envir = asNamespace("fastRhockey"))(2026)
    expect_identical(calls$query[[1]]$alias, unname(aliases[[short]]), info = short)
  }
})

# ---- empty payloads --------------------------------------------------------------

test_that("sihf: an empty payload is a zero-row tibble with the documented columns and populated dtypes", {
  skip_on_cran()
  for (route in names(documented)) {
    case <- parity_cases$case[parity_cases$route == route]
    case <- case[length(case)]
    dt <- parity_dtypes[parity_dtypes$case == case, ]
    expected <- unname(mapply(r_class, dt$dtype, dt$col))
    fn <- get(paste0("sihf_", route), envir = asNamespace("fastRhockey"))
    if (startsWith(route, "game_") || route == "pbp") {
      # a game page with nothing in it (not yet played): the documented columns, never an exception
      parser <- get(paste0(".sihf_parse_", if (route == "pbp") "pbp" else route), envir = asNamespace("fastRhockey"))
      got <- parser(list())
      # ... and the wrapper over a body that only echoes the game id
      local_mocked_bindings(.euro_get = mock_get(function(url, query) list(gameId = query$searchQuery)),
                            .package = "fastRhockey")
      wrapped <- fn(game_id = 20261105000325)
      expect_identical(names(wrapped), documented[[route]], info = route)
      expect_lte(nrow(wrapped), 1L)
    } else {
      body <- function(url, query) {
        b <- echo_body(url, query)
        b$header <- list()
        b$data <- list()
        b
      }
      local_mocked_bindings(.euro_get = mock_get(body), .package = "fastRhockey")
      got <- fn(season = 2026)
    }
    expect_identical(nrow(got), 0L, info = route)
    expect_identical(names(got), documented[[route]], info = route)
    classes <- vapply(names(got), function(cn) class(got[[cn]])[1], character(1))
    expect_identical(unname(classes), expected, info = route)
  }
})

test_that("sihf: malformed payloads parse to zero rows rather than raising", {
  skip_on_cran()
  for (bad in list(NULL, list(), "x", 1, list(header = list(), data = list()), list(header = "a", data = "b"))) {
    expect_identical(nrow(.sihf_parse_table(bad, documented$results)), 0L)
    expect_identical(names(.sihf_parse_table(bad, documented$results)), documented$results)
    expect_identical(nrow(.sihf_parse_phases(bad)), 0L)
    expect_identical(nrow(.sihf_parse_pbp(bad)), 0L)
    expect_identical(nrow(.sihf_parse_game_info(bad)), 0L)
    expect_identical(names(.sihf_parse_game_lineups(bad)), documented$game_lineups)
  }
  expect_identical(ncol(.sihf_parse_table(NULL)), 0L)
  # a cell of the wrong type becomes NA, never an error
  b <- fx_body("table__results__2026__1.json")
  b$data[[1]][[4]] <- list("a", "b")
  expect_no_error(.sihf_parse_table(b, documented$results))
})

# ---- refusals ------------------------------------------------------------------

test_that("sihf: a fallback answer (another season / phase / league) is no_data, not another frame", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(sihf_body(Season = "2027")), .package = "fastRhockey")
  expect_error(sihf_player_stats(1890), class = "fastRhockey_no_data")
  expect_error(sihf_player_stats(2026), class = "fastRhockey_no_data", regexp = "answered Season=2027")
  local_mocked_bindings(.euro_get = mock_get(sihf_body(Season = "2026", Phase = "5")), .package = "fastRhockey")
  expect_error(sihf_player_stats(2026, phase_id = 7), class = "fastRhockey_no_data")
  # an unknown league answers one sentinel row
  b <- sihf_body(rows = list(list("Keine Tabelle gefunden!")), Season = "2026")
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_error(sihf_player_stats(2026, league_id = 999), class = "fastRhockey_no_data", regexp = "Keine Tabelle")
})

test_that("sihf: a filter missing from the echo, or a body with none, is asset_fetch (unverifiable)", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(sihf_body(Season = "2026")), .package = "fastRhockey")
  expect_error(sihf_player_stats(2026, phase_id = 5), class = "fastRhockey_asset_fetch", regexp = "dropped the Phase")
  b <- sihf_body(Season = "2026")
  b$filters <- NULL
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_error(sihf_player_stats(2026), class = "fastRhockey_asset_fetch", regexp = "no filter echo")
  # selected: null echoes nothing, exactly like a dropped filter
  b <- sihf_body(Season = "2026")
  b$filters[[1]]["selected"] <- list(NULL)
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_error(sihf_player_stats(2026), class = "fastRhockey_asset_fetch")
})

test_that("sihf: a dropped or swapped ContentType is a failed fetch, never an empty season", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(sihf_body(Season = "2026")), .package = "fastRhockey")
  expect_error(sihf_standings(2026), class = "fastRhockey_asset_fetch")
  local_mocked_bindings(.euro_get = mock_get(sihf_body(Season = "2026", ContentType = "Away")), .package = "fastRhockey")
  expect_error(sihf_standings(2026), class = "fastRhockey_asset_fetch", regexp = "answered ContentType=Away")
  local_mocked_bindings(.euro_get = mock_get(sihf_body(Season = "2026", ContentType = "Home")), .package = "fastRhockey")
  expect_s3_class(sihf_standings(2026, venue = "Home"), "fastRhockey_data")
})

test_that("sihf: the Phase echo must list the requested phase among its entries", {
  skip_on_cran()
  b <- sihf_body(Season = "2026", Phase = "0")
  b$filters[[2]]$entries <- list(list(alias = "all"), list(alias = "4940"))  # echoed back, but not listed
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_error(sihf_results(2026, phase_id = 0), class = "fastRhockey_no_data", regexp = "lists no Phase=0")
  b$filters[[2]]$entries <- NULL
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_error(sihf_results(2026, phase_id = 0), class = "fastRhockey_asset_fetch", regexp = "no entries")
})

test_that("sihf: an empty Phase asks for the list: the echo is not checked, the entries must be there", {
  skip_on_cran()
  b <- sihf_body(Season = "2026", Phase = "")
  b$filters[[2]]$selected <- "4940"  # SIHF picked its own default phase
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_s3_class(sihf_phases(2026, 1, "player"), "fastRhockey_data")
  b2 <- sihf_body(Season = "2026", Phase = "")
  b2$filters[[2]]$entries <- NULL
  local_mocked_bindings(.euro_get = mock_get(b2), .package = "fastRhockey")
  expect_error(sihf_phases(2026, 1, "player"), class = "fastRhockey_asset_fetch", regexp = "no Phase entries")
  # the Season is still checked
  b3 <- sihf_body(Season = "2027", Phase = "")
  local_mocked_bindings(.euro_get = mock_get(b3), .package = "fastRhockey")
  expect_error(sihf_phases(2026, 1, "player"), class = "fastRhockey_no_data")
})

test_that("sihf: a truncated table is asset_fetch; a whole or unpaged one passes", {
  skip_on_cran()
  b <- sihf_body(Season = "2026")
  b$pages <- 5
  local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
  expect_error(sihf_player_stats(2026), class = "fastRhockey_asset_fetch", regexp = "announces 5 rows but serves 1")
  for (pages in c(0, 1)) {
    b$pages <- pages
    local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
    expect_s3_class(sihf_player_stats(2026), "fastRhockey_data")
  }
})

test_that("sihf: stub and redirect bodies (the drifting error shapes) are asset_fetch", {
  skip_on_cran()
  stub <- list(stub = TRUE, data = list(), generatedAt = "2026-10-12T07:45:00")
  redirect <- list(action = "Redirect", redirectTo = list(alias = "teamGoal"))
  for (b in list(stub, redirect)) {
    local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
    expect_error(sihf_team_goals(2026), class = "fastRhockey_asset_fetch")
    expect_error(sihf_pbp(20261105000325), class = "fastRhockey_asset_fetch")
  }
  local_mocked_bindings(.euro_get = mock_get(redirect), .package = "fastRhockey")
  expect_error(sihf_team_goals(2026), regexp = "teamGoal")
})

test_that("sihf: a game body must be the game asked for", {
  skip_on_cran()
  body <- fx_body("gameoverview__20261105000325.json")
  local_mocked_bindings(.euro_get = mock_get(body), .package = "fastRhockey")
  expect_error(sihf_pbp(20261105000009), class = "fastRhockey_asset_fetch", regexp = "asked for game 20261105000009")
  expect_error(sihf_game_info(1), class = "fastRhockey_asset_fetch")
  for (b in list(list(), list(gameId = NULL), list(gameId = 7), list("x"))) {
    local_mocked_bindings(.euro_get = mock_get(b), .package = "fastRhockey")
    expect_error(sihf_pbp(20261105000325), class = "fastRhockey_asset_fetch")
  }
  # the same id echoed as a string passes
  body$gameId <- "20261105000325"
  local_mocked_bindings(.euro_get = mock_get(body), .package = "fastRhockey")
  expect_gt(nrow(sihf_pbp(20261105000325)), 0L)
})

test_that("sihf: invalid arguments are bad_request before any request is made", {
  skip_on_cran()
  calls <- new_calls()
  local_mocked_bindings(.euro_get = mock_get(echo_body, calls), .package = "fastRhockey")
  expect_error(sihf_results(2026, language = "xx"), class = "fastRhockey_bad_request")
  expect_error(sihf_pbp(1, language = "es"), class = "fastRhockey_bad_request")
  expect_error(sihf_standings(2026, venue = "Neutral"), class = "fastRhockey_bad_request")
  expect_error(sihf_phases(2026, table = "team"), class = "fastRhockey_bad_request")
  expect_error(sihf_results("abc"), class = "fastRhockey_bad_request")
  expect_error(sihf_results(26), class = "fastRhockey_bad_request")
  expect_error(sihf_results(c(2025, 2026)), class = "fastRhockey_bad_request")
  expect_error(sihf_pbp(NA), class = "fastRhockey_bad_request")
  expect_error(sihf_game_lineups(c(1, 2)), class = "fastRhockey_bad_request")
  expect_length(calls$url, 0L)
})

test_that("sihf: the HTTP layer maps statuses to the package vocabulary (via .polite_perform)", {
  skip_on_cran()
  local_mocked_bindings(.polite_wait = function(url, min_interval = 1) invisible(0), .package = "fastRhockey")
  reply <- function(status, text) {
    force(status)
    force(text)
    function(req) httr2::response(status_code = status, url = req$url,
                                  headers = list(`Content-Type` = "application/json"), body = charToRaw(text))
  }
  httr2::local_mocked_responses(reply(404L, '{"error":"GameNotFound"}'))
  expect_error(sihf_pbp(1), class = "fastRhockey_no_data")
  httr2::local_mocked_responses(reply(400L, '{"error":"MissingSearchQuery"}'))
  expect_error(sihf_pbp(1), class = "fastRhockey_bad_request")
  httr2::local_mocked_responses(reply(400L, '{"error":"MissingLeagueId"}'))
  expect_error(sihf_team_goals(2026), class = "fastRhockey_bad_request")
  httr2::local_mocked_responses(reply(501L, ""))  # the empty 501 an unknown alias drifted to
  expect_error(sihf_team_goals(2026), class = "fastRhockey_asset_fetch")
  httr2::local_mocked_responses(reply(200L, ""))  # httr2 refuses to read it: still the package vocabulary
  expect_error(sihf_team_goals(2026), class = "fastRhockey_asset_fetch", regexp = "empty body")
  httr2::local_mocked_responses(reply(200L, "  \n"))
  expect_error(sihf_team_goals(2026), class = "fastRhockey_asset_fetch", regexp = "empty body")
  httr2::local_mocked_responses(reply(200L, "<html>blocked</html>"))
  expect_error(sihf_team_goals(2026), class = "fastRhockey_asset_fetch", regexp = "non-JSON")
  httr2::local_mocked_responses(reply(200L, '{"stub":true,"data":[]}'))
  expect_error(sihf_team_goals(2026), class = "fastRhockey_asset_fetch", regexp = "stub")
  fx <- paste(readLines(fx_path("gameoverview__20261105000325.json"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  httr2::local_mocked_responses(reply(200L, fx))
  expect_gt(nrow(sihf_pbp(20261105000325)), 0L)
})

# ---- the documented behaviour of the parsers ------------------------------------

test_that("sihf pbp: events are ordered by game clock; goals carry tags, score and plus/minus", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(fx_body("gameoverview__20261105000325.json")), .package = "fastRhockey")
  p <- sihf_pbp(20261105000325)
  expect_identical(p$event_index, seq_len(nrow(p)))
  expect_false(is.unsorted(p$game_seconds))
  expect_setequal(unique(p$event_type), c("goal", "penalty", "goalie", "revoked_goal"))
  g <- p[p$event_type == "goal", ]
  expect_identical(g$goal_tags[1], "EQ")
  expect_identical(c(g$score_home[1], g$score_away[1]), c(1L, 0L))
  expect_type(g$plus_player_ids, "list")
  expect_true(all(p$goalie_change[p$event_type == "goalie"] %in% c("in", "out")))
  expect_identical(p$revoke_reason[p$event_type == "revoked_goal"], "CC-Offiside")
  local_mocked_bindings(.euro_get = mock_get(fx_body("gameoverview__20261105000009.json")), .package = "fastRhockey")
  s <- sihf_pbp(20261105000009)
  so <- s[s$event_type == "shootout", ]
  expect_gt(nrow(so), 0L)
  expect_type(so$shootout_scored, "logical")
  expect_false(anyNA(so$team_side))
})

test_that("sihf game box scores: both sides stacked, totals flagged, licence resolved from the roster", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(fx_body("gameoverview__20261105000325.json")), .package = "fastRhockey")
  b <- sihf_game_player_stats(20261105000325)
  expect_setequal(b$team_side, c("home", "away"))
  expect_identical(sum(b$row_type == "total"), 2L)
  expect_true(all(is.na(b$player_id[b$row_type == "total"])))
  expect_true(all(!is.na(b$player_id[b$row_type == "data"])))
  expect_type(b$player_id, "integer")
  expect_type(b$time_on_ice, "character")
  goalies <- sihf_game_goalie_stats(20261105000325)
  expect_identical(nrow(goalies), 4L)
  expect_type(goalies$saves_percentage, "double")
  ts <- sihf_game_team_stats(20261105000325)
  expect_true(any(startsWith(ts$statistic, "SOG")))
  l <- sihf_game_lineups(20261105000325)
  expect_true(all(c("goalkeeper", "defense_left") %in% l$unit))
  expect_type(l$is_captain, "logical")
  i <- sihf_game_info(20261105000325)
  expect_identical(nrow(i), 1L)
  expect_type(i$home_score_by_period, "list")
})

test_that("sihf phases: the count is a game count only on the results table", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(fx_body("table__results__2026__1__phases.json")), .package = "fastRhockey")
  r <- sihf_phases(2026, 1, "results")
  expect_true(any(!is.na(r$n_games)))
  expect_false(any(grepl("\\(\\d+\\)$", r$phase_name)))
  expect_type(r$phase_id, "integer")
  local_mocked_bindings(.euro_get = mock_get(fx_body("table__standing__2026__1__phases.json")), .package = "fastRhockey")
  s <- sihf_phases(2026, 1, "standing")
  expect_identical(s$phase_name, "Regular Season")
  expect_true(is.na(s$n_games))  # (3) is not a game count here
})

test_that("sihf: a placeholder '-' is NA in every cell", {
  skip_on_cran()
  local_mocked_bindings(.euro_get = mock_get(fx_body("full/table__playerTimeOnIce__2026__1.json")), .package = "fastRhockey")
  t <- sihf_player_time_on_ice(2026)
  expect_identical(nrow(t), 427L)
  expect_false(any(t == "-", na.rm = TRUE))
})

# ---- live (opt in: RUN_EURO_TESTS=true; one request per second per host) -----------

test_that("sihf live: standings, results and a game keep their documented columns", {
  skip_on_cran()
  skip_euro_test()
  s <- sihf_standings(2026, 1)
  expect_identical(names(s), documented$standings)
  expect_identical(nrow(s), 14L)
  expect_true(all(s$games_played == 52L))
  r <- sihf_results(2026, 1)
  expect_identical(names(r), documented$results)
  expect_identical(nrow(r), 414L)
  p <- sihf_pbp(20261105000325)
  expect_identical(names(p), documented$pbp)
  expect_gt(nrow(p), 0L)
})

test_that("sihf live: a detail table and the phase list", {
  skip_on_cran()
  skip_euro_test()
  g <- sihf_team_goals(2026, 1)
  expect_identical(names(g), documented$team_goals)
  expect_identical(sum(g$row_type == "data"), 14L)
  w <- sihf_goalie_wins(2026, 1)
  expect_identical(names(w), documented$goalie_wins)
  ph <- sihf_phases(2026, 1, "results")
  expect_identical(names(ph), documented$phases)
  expect_gt(nrow(ph), 1L)
})
