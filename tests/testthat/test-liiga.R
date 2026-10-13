# Offline tests for the Liiga family (R/liiga.R, liiga.fi/api/v2).
#
# Fixtures are sdv-py's tests/fixtures/liiga/ (refs captures, truncated) plus its two full
# bodies in full/. The parity frames in fixtures/liiga/parity/ are written by sdv-py's own
# parsers (hockey/liiga/liiga_parsers.py at sdv-py main, #826) over the SAME fixtures:
# <route>.csv (nulls as \N, a raw CR as <CR>) and parity.json (fixture + dtype of every
# column, in frame order = sdv-py's column list). Live tests are behind skip_euro_test().

liiga_fx <- function(...) testthat::test_path("fixtures", "liiga", ...)
liiga_body <- function(file) jsonlite::fromJSON(liiga_fx(file), simplifyVector = FALSE)
liiga_meta <- jsonlite::fromJSON(liiga_fx("parity", "parity.json"), simplifyVector = FALSE)

# The wrapper call that reproduces each parity route on its fixture.
liiga_calls <- list(
  tournament = quote(liiga_tournament()),
  games = quote(liiga_games(season = 2026, tournament = "playoffs")),
  goals = quote(liiga_goals(season = 2026, tournament = "playoffs")),
  schedule = quote(liiga_schedule(season = 2026, tournament = "runkosarja")),
  gameweeks = quote(liiga_gameweeks(season = 2026)),
  game = quote(liiga_game(2026, 1)),
  game_goals = quote(liiga_game_goals(2026, 1)),
  game_penalties = quote(liiga_game_penalties(2026, 1)),
  game_shootout = quote(liiga_game_shootout(2026, 1)),
  game_lineups = quote(liiga_game_lineups(2026, 1)),
  game_players_stats = quote(liiga_game_players_stats(2026, 1)),
  game_stats = quote(liiga_game_stats(2026, 1)),
  game_team_stats = quote(liiga_game_team_stats(2026, 1)),
  shotmap = quote(liiga_shotmap(2026, 1)),
  standings = quote(liiga_standings(season = 2026)),
  teams_history = quote(liiga_teams_history()),
  teams = quote(liiga_teams()),
  team_stats = quote(liiga_team_stats("shots", 2026, 2026)),
  players_info = quote(liiga_players_info(2026, 2026)),
  player = quote(liiga_player(21870028)),
  player_career = quote(liiga_player_career(21870028)),
  player_game_log = quote(liiga_player_game_log(21870028, 2026)),
  player_stats = quote(liiga_player_stats(2026, 2026, "runkosarja"))
)
for (nm in names(liiga_meta)) {
  if (grepl("^player_stats__", nm)) {
    liiga_calls[[nm]] <- bquote(liiga_player_stats(2026, 2026, "runkosarja", data_type = .(sub("^player_stats__", "", nm))))
  }
  if (grepl("^team_stats__", nm)) {
    liiga_calls[[nm]] <- bquote(liiga_team_stats(.(sub("^team_stats__", "", nm)), 2026, 2026))
  }
}

# Run a wrapper call with the HTTP body mocked at the shared getter (.euro_get): the Liiga
# refusal rules (.liiga_get) still run over it.
liiga_run <- function(call, body, seen = NULL) {
  testthat::with_mocked_bindings(
    eval(call, parent.frame()),
    .euro_get = function(url, query = list(), headers = NULL, ...) {
      if (!is.null(seen)) seen$url <- url
      if (!is.null(seen)) seen$query <- query
      body
    }
  )
}

# The sdv-py frame of a route, read back with its dtypes (NA = null).
liiga_oracle <- function(route) {
  w <- utils::read.csv(liiga_fx("parity", paste0(route, ".csv")), colClasses = "character", na.strings = "\\N",
                       fileEncoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  w[] <- lapply(w, function(v) gsub("<CR>", "\r", v, fixed = TRUE))
  for (cn in names(w)) {
    w[[cn]] <- switch(liiga_meta[[route]]$dtypes[[cn]],
      Int64 = , Float64 = as.numeric(w[[cn]]), Boolean = as.logical(w[[cn]]), String = w[[cn]]
    )
  }
  w
}

# Count the disagreements between the R tibble and the sdv-py frame: a missing column, a
# class mismatch, an NA-vs-value disagreement and a value difference all count.
liiga_mismatches <- function(res, want, dtypes) {
  bad <- setdiff(names(want), names(res)) |> length()
  bad <- bad + length(setdiff(names(res), names(want))) + abs(nrow(res) - nrow(want))
  for (cn in intersect(names(want), names(res))) {
    dt <- dtypes[[cn]]
    r <- res[[cn]]
    w <- want[[cn]]
    cls_ok <- switch(dt, Int64 = is.numeric(r) && !is.logical(r), Float64 = is.double(r), Boolean = is.logical(r),
                     String = is.character(r))
    if (!isTRUE(cls_ok) || length(r) != length(w)) {
      bad <- bad + 1
      next
    }
    if (dt == "Int64" && !is.integer(r) && any(abs(r) <= .Machine$integer.max, na.rm = TRUE)) bad <- bad + 1
    na_off <- is.na(r) != is.na(w)
    both <- !is.na(r) & !is.na(w)
    diff <- logical(length(r))
    diff[both] <- if (dt == "Float64") abs(r[both] - w[both]) > 1e-9 * pmax(1, abs(w[both])) else r[both] != w[both]
    bad <- bad + sum(na_off | diff)
  }
  bad
}

liiga_reset_clock <- function() rm(list = ls(.polite_clock, all.names = TRUE), envir = .polite_clock)

# ---- columns, ids, dtypes ---------------------------------------------------------------

test_that("every route's column set and order equal sdv-py's frame", {
  skip_on_cran()
  for (route in names(liiga_meta)) {
    res <- liiga_run(liiga_calls[[route]], liiga_body(liiga_meta[[route]]$fixture))
    expect_identical(names(res), names(liiga_meta[[route]]$dtypes), info = route)
    expect_s3_class(res, "fastRhockey_data")
    expect_true(all(grepl("^[a-z0-9]+(_[a-z0-9]+)*$", names(res))), info = route)
  }
})

test_that("the documented columns (.LG_COLS) are sdv-py's returns-schema columns", {
  skip_on_cran()
  for (route in names(liiga_calls)) {
    short <- liiga_meta[[route]]$short
    expect_identical(names(.LG_COLS[[short]]), names(liiga_meta[[short]]$dtypes), info = short)
  }
  expect_length(.LG_COLS, 23L)
  expect_equal(sum(lengths(.LG_COLS)), 766L)
})

test_that("ids are integers, rates are doubles, season is an integer", {
  skip_on_cran()
  for (route in names(liiga_meta)) {
    res <- liiga_run(liiga_calls[[route]], liiga_body(liiga_meta[[route]]$fixture))
    id_cols <- grep("(^|_)id$", names(res), value = TRUE)
    for (cn in id_cols) expect_type(res[[cn]], "integer")
    if ("season" %in% names(res)) {
      expect_type(res$season, "integer")
      expect_false(anyNA(res$season), info = route)
    }
    for (cn in grep("percent|per_game|expected|average|speed|distance|duration|hardest", names(res), value = TRUE)) {
      if (!grepl("(^|_)id$", cn)) expect_type(res[[cn]], "double")
    }
  }
})

test_that("a whole-number xG sent as an integer is still a double", {
  skip_on_cran()
  games <- liiga_run(liiga_calls$games, liiga_body("full/games__playoffs__2026__3games.json"))
  expect_type(games$away_expected_goals, "double")
  expect_type(games$home_expected_goals, "double")
  # game 55116 sends its away xG as the JSON integer 3
  expect_true(3 %in% games$away_expected_goals[games$game_id == 55116L])
})

test_that("team ids: the three wire forms become one integer id plus a slug where there is one", {
  skip_on_cran()
  s <- .lg_split_team_id("55786244:hpk")
  expect_identical(s$num, 55786244L)
  expect_identical(s$slug, "hpk")
  expect_identical(.lg_split_team_id(55786244L)$num, 55786244L)
  expect_null(.lg_split_team_id(55786244L)$slug)
  expect_identical(.lg_split_team_id("495643563")$num, 495643563L)
  expect_null(.lg_split_team_id(TRUE)$num)
  expect_null(.lg_split_team_id("abc")$num)
  teams <- liiga_run(liiga_calls$teams, liiga_body("teams__info__all.json"))
  expect_type(teams$team_id, "integer")
  shots <- liiga_run(liiga_calls$shotmap, liiga_body("shotmap__season__id.json"))
  expect_true(all(c("shooting_team_id", "left_team_id", "right_team_id") %in% names(shots)))
  expect_false(any(grepl("_slug$", names(shots))))
})

test_that(".lg_number parses a percentage with either comma convention", {
  skip_on_cran()
  # the sdv-py fix: "4,583" is a thousands separator (4583), never 4.583
  expect_equal(.lg_number(c("4,583", "4,583.33", "0,00", "12,5", "92.86", "1,234,567", "-4,583", " 7 ", "abc", "", NA)),
               c(4583, 4583.33, 0, 12.5, 92.86, 1234567, -4583, 7, NA, NA, NA))
  career <- liiga_run(liiga_calls$player_career, liiga_body("players__info__playerId.json"))
  expect_type(career$win_percentage, "double")
})

# ---- empty payloads ---------------------------------------------------------------------

liiga_parsers <- list(
  tournament = .lg_parse_generic, games = .lg_parse_games, goals = .lg_parse_goals, schedule = .lg_parse_schedule,
  gameweeks = .lg_parse_generic, game = .lg_parse_game, game_goals = .lg_parse_game_goals,
  game_penalties = .lg_parse_game_penalties, game_shootout = .lg_parse_game_shootout,
  game_lineups = .lg_parse_game_lineups, game_players_stats = .lg_parse_game_players_stats,
  game_stats = .lg_parse_game_stats, game_team_stats = .lg_parse_game_team_stats, shotmap = .lg_parse_shotmap,
  standings = .lg_parse_standings, teams_history = .lg_parse_generic, teams = .lg_parse_teams,
  team_stats = .lg_parse_generic, players_info = .lg_parse_players_info, player = .lg_parse_player,
  player_career = .lg_parse_player_career, player_game_log = .lg_parse_player_game_log,
  player_stats = .lg_parse_generic
)

test_that("an empty payload parses to zero rows with the populated frame's columns and classes", {
  skip_on_cran()
  expect_setequal(names(liiga_parsers), names(.LG_COLS))
  for (short in names(liiga_parsers)) {
    full <- liiga_run(liiga_calls[[short]], liiga_body(liiga_meta[[short]]$fixture))
    for (empty in list(list(), NULL, setNames(list(), character()))) {
      none <- liiga_parsers[[short]](empty, .LG_COLS[[short]])
      expect_equal(nrow(none), 0L, info = short)
      expect_identical(names(none), names(full), info = short)
      expect_identical(vapply(none, function(x) class(x)[1], ""), vapply(full, function(x) class(x)[1], ""), info = short)
      expect_equal(nrow(dplyr::bind_rows(none, as.data.frame(full))), nrow(full), info = short)
    }
  }
})

test_that("a valid payload with no rows for the frame is a zero-row tibble, not an error", {
  skip_on_cran()
  game <- list(game = list(id = 1L, season = 2026L, homeTeam = list(teamId = "1:a"), awayTeam = list(teamId = "2:b")))
  for (fn in c("liiga_game_goals", "liiga_game_penalties", "liiga_game_shootout", "liiga_game_lineups")) {
    res <- liiga_run(call(fn, 2026, 1), game)
    short <- sub("^liiga_", "", fn)
    expect_equal(nrow(res), 0L, info = fn)
    expect_identical(names(res), names(.LG_COLS[[short]]), info = fn)
  }
})

# ---- refusals ---------------------------------------------------------------------------

test_that("bodies that say 'nothing here' raise fastRhockey_no_data", {
  skip_on_cran()
  expect_error(liiga_run(quote(liiga_game_stats(2026, 999999)), list(stats = "Remote server error")),
               class = "fastRhockey_no_data")
  expect_error(liiga_run(quote(liiga_shotmap(2014, 1)), list()), class = "fastRhockey_no_data")
  expect_error(liiga_run(quote(liiga_schedule(season = 2026, tournament = "playout")), list()),
               class = "fastRhockey_no_data")
  expect_error(liiga_run(quote(liiga_team_stats("shots", 2026, 2026)), list(teamStats = list())),
               class = "fastRhockey_no_data")
  expect_error(liiga_run(quote(liiga_tournament()), list(a = NULL, b = NULL)), class = "fastRhockey_no_data")
  expect_error(liiga_run(quote(liiga_tournament()), NULL), class = "fastRhockey_no_data")
  expect_error(liiga_run(quote(liiga_games(season = 2026)), list(list(), list())), class = "fastRhockey_no_data")
})

test_that("error-shaped 200s raise fastRhockey_asset_fetch", {
  skip_on_cran()
  expect_error(liiga_run(quote(liiga_tournament()), "Internal server error"), class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_tournament()), list(message = "Internal server error")),
               class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_tournament()), list(code = 500L, message = "x")), class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_tournament()), list(game_week = 1L, note = "Error in backend")),
               class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_tournament()), list(game_week = 1L, detail = list(error = "x"))),
               class = "fastRhockey_asset_fetch")
})

test_that("a body for another season or game than the one asked for raises fastRhockey_asset_fetch", {
  skip_on_cran()
  game <- liiga_body("games__season__id.json")
  expect_s3_class(liiga_run(quote(liiga_game(2026, 1)), game), "fastRhockey_data")
  expect_error(liiga_run(quote(liiga_game(2025, 1)), game), class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_game(2026, 2)), game), class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_game_goals(2026, 1)), list(game = NULL, homeTeamPlayers = list(list(id = 1L)))),
               class = "fastRhockey_asset_fetch")
  game$game$id <- TRUE # JSON true is not game 1
  expect_error(liiga_run(quote(liiga_game(2026, 1)), game), class = "fastRhockey_asset_fetch")
  games <- liiga_body("full/games__playoffs__2026__3games.json")
  expect_error(liiga_run(quote(liiga_games(season = 2025, tournament = "playoffs")), games),
               class = "fastRhockey_asset_fetch")
  expect_s3_class(liiga_run(quote(liiga_games(season = 2026, tournament = "playoffs")), games), "fastRhockey_data")
  # no season sent: nothing to check
  expect_s3_class(liiga_run(quote(liiga_games(tournament = "playoffs")), games), "fastRhockey_data")
  expect_error(liiga_run(quote(liiga_schedule(season = 2025)), liiga_body("schedule.json")),
               class = "fastRhockey_asset_fetch")
  expect_error(liiga_run(quote(liiga_gameweeks(season = 2025)), liiga_body("gameweeks.json")),
               class = "fastRhockey_asset_fetch")
  log <- liiga_body("players__info__playerId__games__season.json")
  expect_s3_class(liiga_run(quote(liiga_player_game_log(21870028, 2026)), log), "fastRhockey_data")
  expect_error(liiga_run(quote(liiga_player_game_log(21870028, 2025)), log), class = "fastRhockey_asset_fetch")
})

test_that("a bad argument raises fastRhockey_bad_request before any request is sent", {
  skip_on_cran()
  seen <- new.env()
  seen$url <- NULL
  expect_error(liiga_run(quote(liiga_games(season = 2026, tournament = "nhl")), list(), seen),
               class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_schedule(tournament = "RUNKOSARJA")), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_gameweeks(tournament = "nhl")), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_gameweeks(tournament = NULL)), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_team_stats("hits")), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_player_stats(2026, 2026, "playoffs", data_type = "x")), list(), seen),
               class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_player_stats(2026, 2026, "all")), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_game(2026.5, 1)), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_game("2026", 1)), list(), seen), class = "fastRhockey_bad_request")
  expect_error(liiga_run(quote(liiga_player(NA)), list(), seen), class = "fastRhockey_bad_request")
  expect_null(seen$url)
})

test_that("HTTP statuses map to the refusal vocabulary and a 500 is not retried", {
  skip_on_cran()
  respond <- function(status, body = "", ct = "application/json") {
    n <- 0L
    list(
      calls = function() n,
      fn = function(req) {
        n <<- n + 1L
        httr2::response(status_code = status, url = req$url, headers = list(`Content-Type` = ct), body = charToRaw(body))
      }
    )
  }
  run <- function(status, body = "") {
    liiga_reset_clock()
    r <- respond(status, body)
    httr2::local_mocked_responses(r$fn)
    err <- tryCatch(liiga_game_stats(2026, 1), error = function(e) e)
    list(err = err, calls = r$calls())
  }
  r404 <- run(404L, "{}")
  expect_s3_class(r404$err, "fastRhockey_no_data")
  r400 <- run(400L, '{"message": "bad"}')
  expect_s3_class(r400$err, "fastRhockey_bad_request")
  r500 <- run(500L, '"Remote server error"')
  expect_s3_class(r500$err, "fastRhockey_asset_fetch")
  expect_equal(r500$calls, 1L)
  r200 <- run(200L, "  ")
  expect_s3_class(r200$err, "fastRhockey_asset_fetch")
  expect_s3_class(run(200L, "<html>blocked</html>")$err, "fastRhockey_asset_fetch")
  ok <- run(200L, '{"stats": "Remote server error"}')
  expect_s3_class(ok$err, "fastRhockey_no_data")
  expect_s3_class(ok$err, "fastRhockey_error")
})

# ---- request shape: season = END year, query keys, fixed params -------------------------------

test_that("requests carry the END-year season and sdv-py's paths, query keys and fixed params", {
  skip_on_cran()
  seen <- new.env()
  chk <- function(call, body, url, query = list()) {
    liiga_run(call, body, seen)
    expect_identical(seen$url, paste0("https://liiga.fi/api/v2", url), info = deparse(call))
    expect_identical(unlist(seen$query[sort(names(seen$query))]), unlist(query[sort(names(query))]), info = deparse(call))
  }
  chk(quote(liiga_tournament()), liiga_body("tournament.json"), "/tournament")
  chk(quote(liiga_games(season = 2026, tournament = "playoffs")), liiga_body("full/games__playoffs__2026__3games.json"),
      "/games", list(season = 2026L, tournament = "playoffs"))
  chk(quote(liiga_games()), liiga_body("full/games__playoffs__2026__3games.json"), "/games", list())
  chk(quote(liiga_gameweeks(season = 2026)), liiga_body("gameweeks.json"), "/gameweeks",
      list(season = 2026L, tournament = "runkosarja"))
  chk(quote(liiga_game(2026, 1)), liiga_body("games__season__id.json"), "/games/2026/1")
  chk(quote(liiga_game_players_stats(2026, 1)), liiga_body("games__season__id__playersWithStats.json"), "/games/2026/1",
      list(dataType = "playersWithStats"))
  chk(quote(liiga_game_stats(2026, 1)), liiga_body("full/games__stats__2026__1.json"), "/games/stats/2026/1")
  chk(quote(liiga_shotmap(2026, 1)), liiga_body("shotmap__season__id.json"), "/shotmap/2026/1")
  chk(quote(liiga_standings(season = 2026)), liiga_body("standings.json"), "/standings/", list(season = 2026L))
  chk(quote(liiga_teams_history()), liiga_body("teams__info.json"), "/teams/info", list(team = "all"))
  chk(quote(liiga_teams()), liiga_body("teams__info__all.json"), "/teams/info")
  chk(quote(liiga_team_stats("shots", 2026, 2026)), liiga_body("teams__stats__shots.json"), "/teams/stats",
      list(dataType = "shots", seasonFrom = 2026L, seasonTo = 2026L, tournament = "runkosarja"))
  chk(quote(liiga_players_info(2026, 2026)), liiga_body("players__info.json"), "/players/info",
      list(fromSeason = 2026L, toSeason = 2026L, tournament = "runkosarja"))
  chk(quote(liiga_player(21870028)), liiga_body("players__info__playerId.json"), "/players/info/21870028")
  chk(quote(liiga_player_game_log(21870028, 2026)), liiga_body("players__info__playerId__games__season.json"),
      "/players/info/21870028/games/2026")
  chk(quote(liiga_player_stats(2026, 2026, "playoffs", data_type = "goalStats")),
      liiga_body("players__stats__summed__goalStats.json"),
      "/players/stats/summed/2026/2026/playoffs/true", list(dataType = "goalStats"))
})

test_that("the runtime stamps the request into a body that does not name its season / game", {
  skip_on_cran()
  stats <- testthat::with_mocked_bindings(
    .liiga_get("/games/stats/2026/1"), .euro_get = function(url, query = list(), headers = NULL, ...) liiga_body("full/games__stats__2026__1.json")
  )
  expect_identical(stats[["_request"]], list(season = 2026L, game_id = 1L))
  shots <- testthat::with_mocked_bindings(
    .liiga_get("/shotmap/2026/1"), .euro_get = function(url, query = list(), headers = NULL, ...) liiga_body("shotmap__season__id.json")
  )
  expect_true(all(vapply(shots, function(r) identical(r[["_request"]], list(season = 2026L, game_id = 1L)), NA)))
  res <- liiga_run(quote(liiga_shotmap(2026, 1)), liiga_body("shotmap__season__id.json"))
  expect_true(all(res$season == 2026L) && all(res$game_id == 1L))
})

test_that("wrappers scope scipen / stringsAsFactors for the call and leave the caller's options alone", {
  skip_on_cran()
  old <- options(scipen = 3)
  on.exit(options(old), add = TRUE)
  liiga_run(quote(liiga_tournament()), liiga_body("tournament.json"))
  expect_equal(getOption("scipen"), 3)
  during <- NULL
  testthat::with_mocked_bindings(
    liiga_tournament(),
    .euro_get = function(url, query = list(), headers = NULL, ...) {
      during <<- c(getOption("scipen"), getOption("stringsAsFactors"))
      liiga_body("tournament.json")
    }
  )
  expect_equal(during, c(999, 0))
})

test_that("wrappers raise, they never warn and return an empty tibble", {
  skip_on_cran()
  expect_no_warning(liiga_run(quote(liiga_games(season = 2026, tournament = "playoffs")),
                              liiga_body("full/games__playoffs__2026__3games.json")))
  expect_error(liiga_run(quote(liiga_games(season = 2026)), list()), class = "fastRhockey_error")
})

# ---- parity with sdv-py on the same fixtures --------------------------------------------

test_that("every route matches the sdv-py frame on its fixture, NA-vs-value counted as a mismatch", {
  skip_on_cran()
  expect_length(liiga_meta, 43L)
  total <- 0
  for (route in names(liiga_meta)) {
    res <- liiga_run(liiga_calls[[route]], liiga_body(liiga_meta[[route]]$fixture))
    want <- liiga_oracle(route)
    n <- liiga_mismatches(res, want, liiga_meta[[route]]$dtypes)
    expect_identical(names(res), names(want), info = route)
    expect_equal(n, 0, info = route)
    expect_equal(nrow(res), liiga_meta[[route]]$rows, info = route)
    total <- total + n
  }
  expect_equal(total, 0)
})

test_that("the parity check itself counts an NA-vs-value disagreement", {
  skip_on_cran()
  res <- liiga_run(liiga_calls$tournament, liiga_body("tournament.json"))
  want <- liiga_oracle("tournament")
  expect_equal(liiga_mismatches(res, want, liiga_meta$tournament$dtypes), 0)
  want$game_week[1] <- NA
  expect_equal(liiga_mismatches(res, want, liiga_meta$tournament$dtypes), 1)
  res$max_game_week[1] <- NA
  expect_equal(liiga_mismatches(res, want, liiga_meta$tournament$dtypes), 2)
})

# ---- live (one request per route, >= 1 s apart; off unless RUN_EURO_TESTS=true) -----------

test_that("live: tournament, schedule and a box score answer", {
  skip_on_cran()
  skip_euro_test()
  tour <- liiga_tournament()
  expect_gt(nrow(tour), 0)
  sched <- liiga_schedule(season = 2026, tournament = "runkosarja")
  expect_gt(nrow(sched), 100)
  expect_true(all(sched$season == 2026L))
  expect_error(liiga_game_stats(2026, 99999999), class = "fastRhockey_no_data")
})

test_that("game stats join tracking rows whose player_id is double against integer box-score ids", {
  body <- liiga_body("full/games__stats__2026__1.json")
  base <- liiga_run(quote(liiga_game_stats(2026, 1)), body)
  for (key in c("homeTeamGamePlayerStats", "awayTeamGamePlayerStats")) {
    body[[key]] <- lapply(body[[key]], function(r) { r$playerId <- as.double(r$playerId); r })
  }
  got <- liiga_run(quote(liiga_game_stats(2026, 1)), body)
  expect_equal(as.data.frame(got), as.data.frame(base), ignore_attr = TRUE)
})
