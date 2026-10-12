# Offline tests for the National League app API family (R/nationalleague.R, prefix nl_).
#
# Fixtures are sdv-py's committed captures (tests/testthat/fixtures/nationalleague/): the 12
# route captures (arrays cut to 3 elements) and the whole/trimmed bodies under full/ (the dtype
# source). The getter is mocked, so no test touches the network, except the live one behind
# skip_euro_test(). Parity files (parity/*.csv) and columns.csv were written from sdv-py's
# parse_nationalleague_* frames on these same fixtures.
#
# Tests marked [needs-euro-resp-json-fix] depend on the shared-runtime fix for a truly empty
# 200 body in .euro_resp_json() (the coordinator lands it at integration).

nl_fx <- function(...) testthat::test_path("fixtures", "nationalleague", ...)
nl_json <- function(name) jsonlite::fromJSON(nl_fx(name), simplifyVector = FALSE)
nl_text_json <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)
nl_columns <- function() utils::read.csv(nl_fx("columns.csv"), stringsAsFactors = FALSE)
nl_documented <- function(route) {
  cols <- nl_columns()
  cols <- cols[cols$route == route, ]
  stats::setNames(cols$r_type, cols$column)
}
nl_class <- function(x) typeof(x)

# Count disagreements between an R tibble and the sdv-py frame written on the same fixture.
# NA-vs-value counts as a mismatch (memory: parity_agreement_counts_na_mismatch).
nl_mismatches <- function(df, csv, types) {
  ref <- utils::read.csv(csv, colClasses = "character", na.strings = "NA", check.names = FALSE,
                         encoding = "UTF-8", stringsAsFactors = FALSE)
  expect_identical(names(df), names(ref))
  expect_identical(nrow(df), nrow(ref))
  bad <- 0L
  for (col in names(ref)) {
    r <- df[[col]]
    p <- ref[[col]]
    type <- unname(types[col])
    if (type %in% c("double", "integer")) {
      p <- suppressWarnings(as.numeric(p))
      r <- as.numeric(r)
      disagree <- xor(is.na(r), is.na(p)) |
        (!is.na(r) & !is.na(p) & abs(r - p) > 1e-9 * pmax(1, abs(p)))
    } else if (type == "logical") {
      p <- ifelse(is.na(p), NA, p == "true")
      disagree <- xor(is.na(r), is.na(p)) | (!is.na(r) & !is.na(p) & r != p)
    } else {
      disagree <- xor(is.na(r), is.na(p)) | (!is.na(r) & !is.na(p) & r != p)
    }
    bad <- bad + sum(disagree)
  }
  bad
}

# route -> (wrapper call, capture, full body or NULL, parser, expected request path + query)
NL_GAME <- 20261105000325
NL_ROUTES <- list(
  schedule = list(quote(nl_schedule(season = 2026)), "games.json", NULL, ".nl_parse_games", "/games", list(season = "2026")),
  current_games = list(quote(nl_current_games(season = 2027)), "games__current.json", NULL, ".nl_parse_games", "/games/current", list(season = "2027")),
  team_schedule = list(quote(nl_team_schedule(team_id = 101144, season = 2026)), "games__team__teamId.json", NULL, ".nl_parse_games", "/games/team/101144", list(season = "2026")),
  playoffs = list(quote(nl_playoffs(season = 2026)), "games__playoffs.json", NULL, ".nl_parse_bracket", "/games/playoffs", list(season = "2026")),
  playouts = list(quote(nl_playouts(season = 2026)), "games__playouts.json", NULL, ".nl_parse_bracket", "/games/playouts", list(season = "2026")),
  game_overview = list(quote(nl_game_overview(game_id = NL_GAME)), "games__gameId.json", "game_20261105000325.json", ".nl_parse_game_overview", "/games/20261105000325", list()),
  pbp = list(quote(nl_pbp(game_id = NL_GAME)), "games__gameId.json", "game_20261105000325.json", ".nl_parse_pbp", "/games/20261105000325", list()),
  lineups = list(quote(nl_lineups(game_id = NL_GAME)), "games__gameId.json", "game_20261105000325.json", ".nl_parse_lineups", "/games/20261105000325", list()),
  player_box = list(quote(nl_player_box(game_id = NL_GAME)), "games__gameId.json", "game_20261105000325.json", ".nl_parse_player_box", "/games/20261105000325", list()),
  team_stats = list(quote(nl_team_stats(game_id = NL_GAME)), "games__gameId.json", "game_20261105000325.json", ".nl_parse_team_stats", "/games/20261105000325", list()),
  shots = list(quote(nl_shots(game_id = NL_GAME)), "games__gameId.json", "game_20261105000325.json", ".nl_parse_shots", "/games/20261105000325", list()),
  player_stats = list(quote(nl_player_stats(season = 2026, phase = 4940)), "player.json", "player_2026_4940.json", ".nl_parse_player_stats", "/player", list(season = "2026", phase = "4940")),
  top_scorers = list(quote(nl_top_scorers(take = 3, season = 2026, phase = 4940)), "player__topscorer.json", NULL, ".nl_parse_player_stats", "/player/topscorer", list(take = "3", season = "2026", phase = "4940")),
  team_player_stats = list(quote(nl_team_player_stats(team_id = 101144, season = 2026, phase = 4940)), "player__team__teamId.json", NULL, ".nl_parse_player_stats", "/player/team/101144", list(season = "2026", phase = "4940")),
  player_season = list(quote(nl_player_season(player_id = 305519, season = 2026, phase = 4940)), "player__playerId.json", NULL, ".nl_parse_player_stats", "/player/305519", list(season = "2026", phase = "4940")),
  standings = list(quote(nl_standings(season = 2026, phase = 4940)), "teams.json", "teams_2026_4940.json", ".nl_parse_standings", "/teams", list(season = "2026", phase = "4940")),
  team_info = list(quote(nl_team_info(team_id = 101144)), "teams__teamId__info.json", NULL, ".nl_parse_team_info", "/teams/101144/info", list())
)
NL_HOST <- "https://www.nationalleague.ch/api"

# Mock the family getter: records what the wrapper asked for, answers with `body`.
nl_mock_get <- function(body, env = parent.frame()) {
  seen <- new.env()
  testthat::local_mocked_bindings(
    .nl_get = function(url, params = list()) {
      seen$url <- url
      seen$params <- params
      body
    },
    .env = env
  )
  seen
}

test_that("the family exports exactly sdv-py's 17 wrappers", {
  skip_on_cran()
  expect_setequal(names(NL_ROUTES), sub("^nl_", "", sub("\\(.*$", "", vapply(NL_ROUTES, function(r) deparse(r[[1]]), ""))))
  for (route in names(NL_ROUTES)) {
    expect_true(is.function(get(paste0("nl_", route))), info = route)
  }
  expect_length(NL_ROUTES, 17)
})

test_that("every wrapper asks for its route and returns the documented columns and types", {
  skip_on_cran()
  for (route in names(NL_ROUTES)) {
    spec <- NL_ROUTES[[route]]
    seen <- nl_mock_get(nl_json(spec[[2]]))
    df <- eval(spec[[1]])
    expect_equal(seen$url, paste0(NL_HOST, spec[[5]]), info = route)
    expect_equal(seen$params, spec[[6]], info = route)
    expect_s3_class(df, "fastRhockey_data")
    expect_gt(nrow(df), 0)
    documented <- nl_documented(route)
    expect_identical(names(df), names(documented), info = route)
    expect_identical(unname(vapply(df, nl_class, "")), unname(documented), info = route)
    ids <- grep("(^id|_id)$", names(df), value = TRUE)
    expect_true(length(ids) > 0 && all(vapply(df[ids], is.double, NA)), info = route)
  }
})

test_that("PARITY: the R tibble equals sdv-py's frame on the same capture, route by route", {
  skip_on_cran()
  for (route in names(NL_ROUTES)) {
    spec <- NL_ROUTES[[route]]
    nl_mock_get(nl_json(spec[[2]]))
    df <- eval(spec[[1]])
    expect_equal(nl_mismatches(df, nl_fx("parity", paste0(route, ".csv")), nl_documented(route)), 0L, info = route)
  }
})

test_that("PARITY: the R tibble equals sdv-py's frame on the full bodies (the dtype source)", {
  skip_on_cran()
  for (route in names(NL_ROUTES)) {
    spec <- NL_ROUTES[[route]]
    if (is.null(spec[[3]])) next
    df <- get(spec[[4]])(nl_json(file.path("full", spec[[3]])))
    expect_equal(nl_mismatches(df, nl_fx("parity", paste0(route, "__full.csv")), nl_documented(route)), 0L, info = route)
    expect_identical(unname(vapply(df, nl_class, "")), unname(nl_documented(route)), info = route)
  }
})

test_that("a truncated capture has the full body's dtypes (a whole-number rate stays double)", {
  skip_on_cran()
  for (route in names(NL_ROUTES)) {
    spec <- NL_ROUTES[[route]]
    if (is.null(spec[[3]])) next
    parse <- get(spec[[4]])
    expect_identical(vapply(parse(nl_json(spec[[2]])), nl_class, ""),
                     vapply(parse(nl_json(file.path("full", spec[[3]]))), nl_class, ""), info = route)
  }
  raw <- nl_json("games__gameId.json")
  expect_equal(raw$playerStatsAway[[1]]$savePercentage, 90) # a whole number in the capture ...
  expect_type(.nl_parse_player_box(raw)$save_percentage, "double") # ... a double column all the same
  expect_type(.nl_parse_player_stats(nl_json("full/player_2026_4940.json"))$ps_percentage, "double")
})

test_that("an empty or malformed payload is a zero-row tibble with the documented columns", {
  skip_on_cran()
  one_row_ok <- c("player_stats", "top_scorers", "team_player_stats", "player_season", "team_info")
  for (route in names(NL_ROUTES)) {
    parse <- get(NL_ROUTES[[route]][[4]])
    for (raw in list(nl_text_json("{}"), nl_text_json("[]"), NULL, "x", 3L, nl_text_json('{"overview": "x"}'))) {
      if (is.list(raw) && !is.null(raw$overview) && route %in% one_row_ok) next
      df <- parse(raw)
      expect_equal(nrow(df), 0L, info = route)
      expect_identical(names(df), names(nl_documented(route)), info = route)
      expect_identical(unname(vapply(df, nl_class, "")), unname(nl_documented(route)), info = route)
    }
  }
})

test_that("a valid game with no events, shots or lineups is a zero-row documented tibble", {
  skip_on_cran()
  game <- nl_json("games__gameId.json")
  for (part in c("actions", "shotsHome", "shotsAway", "lineupHome", "lineupAway")) game[[part]] <- list()
  expect_equal(nrow(.nl_parse_pbp(game)), 0)
  expect_identical(names(.nl_parse_pbp(game)), names(nl_documented("pbp")))
  expect_equal(nrow(.nl_parse_lineups(game)), 0)
  expect_equal(nrow(.nl_parse_shots(game)), 0)
  expect_gt(nrow(.nl_parse_game_overview(game)), 0)
})

test_that("a value the documented type cannot hold is kept, and an id never flips type", {
  skip_on_cran()
  df <- .nl_parse_games(list(list(gameId = "20261105000325", gameTime = 12.5)))
  expect_type(df$game_time, "double")
  expect_equal(df$game_time, 12.5)
  rows <- list(list(gameId = "20261105000325", homeTeamId = "101151"), list(gameId = "x1", homeTeamId = "abc"))
  df <- .nl_parse_games(rows)
  expect_type(df$game_id, "double")
  expect_equal(df$game_id, c(20261105000325, NA))
  expect_equal(df$home_team_id, c(101151, NA))
  extra <- .nl_parse_games(list(list(gameId = "20261105000325", newThingId = "n/a")))
  expect_type(extra$new_thing_id, "double")
  expect_true(is.na(extra$new_thing_id))
  new_rate <- .nl_parse_games(list(list(gameId = "1", fooPercentage = 90L, barCount = 3L, bazFlag = TRUE)))
  expect_type(new_rate$foo_percentage, "double")
  expect_type(new_rate$bar_count, "integer")
  expect_type(new_rate$baz_flag, "logical")
})

test_that("the generic row rules: nested objects flatten, ids stay whole numbers, empty cells stay typed", {
  skip_on_cran()
  df <- .nl_frame(.nl_flat_rows(list(list(gameId = "1", x = list(y = 2L)))), "games")
  expect_true("x_y" %in% names(df))
  expect_equal(df$game_id, 1)
  expect_equal(.nl_flatten(nl_text_json('{"a": {"bC": 1, "d": {}}, "e": []}')),
               list(a_b_c = 1L, a_d = structure(list(), names = character()), e = list()))
  expect_equal(.nl_underscore(c("teamID", "sogOt1", "assist1FirstName", "posXPercentage", "otA1")),
               c("team_id", "sog_ot1", "assist1_first_name", "pos_x_percentage", "ot_a1"))
})

test_that("list cells are Python-style JSON strings (parity with sdv-py's json.dumps)", {
  skip_on_cran()
  uuml <- intToUtf8(252)
  expect_identical(.nl_json_dumps(list("a", paste0("b", uuml), list())), paste0('["a", "b', uuml, '", []]'))
  expect_identical(.nl_json_dumps(nl_text_json('{"k": ["x"], "e": {}}')), '{"k": ["x"], "e": {}}')
  expect_identical(.nl_json_dumps(list()), "[]")
  games <- .nl_parse_games(nl_json("games.json"))
  expect_match(games$referees[[1]], paste0("^\\[\"#13 Stefan H", intToUtf8(252), "rlimann\", "))
  expect_identical(games$tv_partner[[1]], "[]")
  streak <- .nl_parse_standings(nl_json("full/teams_2026_4940.json"))$streak
  expect_match(streak[[1]], "^\\[\"[WL]\", \"")
})

# ---- the game page -------------------------------------------------------------------------

nl_game <- function() nl_json("full/game_20261105000325.json")

test_that("fo is faceoffs in the game box and penalties in season rows", {
  skip_on_cran()
  box <- .nl_parse_player_box(nl_game())
  team <- .nl_parse_team_stats(nl_game())
  for (df in list(box, team)) {
    expect_false(any(c("fo", "penalties") %in% names(df)))
    expect_true(all(df$faceoffs == df$fow + df$fol))
  }
  season <- .nl_parse_player_stats(nl_json("full/player_2026_4940.json"))
  expect_false(any(c("fo", "faceoffs") %in% names(season)))
  expect_true(all(season$penalties == season$fo2 + season$fo5 + season$fo10 + season$gm + season$mp))
  expect_true("penalties" %in% names(.nl_parse_standings(nl_json("full/teams_2026_4940.json"))))
})

test_that("shots: coordinate ranges, percentages, xG on a BLOCK, order, shooter names", {
  skip_on_cran()
  shots <- .nl_parse_shots(nl_game())
  expect_equal(nrow(shots), 105)
  expect_setequal(unique(shots$team_side), c("home", "away"))
  expect_true(all(shots$shot_type %in% c("SOG", "MISS", "POST", "BLOCK", "GOAL")))
  expect_true(all(shots$pos_x >= 0 & shots$pos_x <= 30))
  expect_true(all(shots$pos_y >= 0 & shots$pos_y <= 60))
  expect_true(all(abs(shots$pos_y_percentage - shots$pos_y / 24) < 1e-9))
  expect_true(all(abs(shots$pos_x_percentage - shots$pos_x / 30) < 1e-9))
  expect_true(all(shots$xg[shots$shot_type == "BLOCK"] == 0))
  expect_true(any(shots$xg[shots$shot_type != "BLOCK"] > 0))
  expect_false(is.unsorted(shots$time))
  expect_false(anyNA(shots$player_last_name))
})

test_that("box-score rows get their player_id from the lineup by sweater number", {
  skip_on_cran()
  box <- .nl_parse_player_box(nl_game())
  lineups <- .nl_parse_lineups(nl_game())
  expect_false(anyNA(box$player_id))
  key <- function(d) paste(d$team_side, d$number, d$player_id)
  expect_true(all(key(box) %in% key(lineups)))
})

test_that("pbp: the MMSS clock becomes seconds, events keep timeline order", {
  skip_on_cran()
  pbp <- .nl_parse_pbp(nl_game())
  expect_equal(pbp$event_index, seq_len(nrow(pbp)))
  first <- pbp[1, ]
  expect_identical(c(first$time, first$action), c("822", "goal"))
  expect_equal(c(first$game_seconds, first$period), c(502, 1))
  expect_false(is.unsorted(pbp$game_seconds))
  expect_equal(.nl_clock_seconds(c("822")), 502)
  expect_equal(.nl_clock_seconds("5"), 5)
  expect_true(is.na(.nl_clock_seconds("")) && is.na(.nl_clock_seconds(NULL)) && is.na(.nl_clock_seconds("8:22")))
})

test_that("the one integer position is cast to a string and the bracket carries round and series", {
  skip_on_cran()
  expect_type(.nl_parse_player_stats(nl_json("full/player_2026_4940.json"))$position, "character")
  expect_true("0" %in% .nl_parse_player_stats(nl_json("full/player_2026_4940.json"))$position)
  df <- .nl_parse_bracket(nl_json("games__playoffs.json"))
  expect_setequal(unique(df$round), c("final", "semifinal", "quarterfinal"))
  final <- df[df$round == "final", ]
  expect_equal(unique(final$series_best_of), 7)
  expect_equal(unique(final$series_home_team_id), 101151)
  expect_equal(unique(substr(format(df$game_id, scientific = FALSE), 1, 4)), "2026")
  for (games in list("x", 3L, nl_text_json('{"a": 1}'), NULL)) {
    raw <- list(list(round = "final", games = games), list(round = "semifinal", games = list(list(games = games))))
    out <- .nl_parse_bracket(raw)
    expect_equal(nrow(out), 0)
    expect_identical(names(out), names(nl_documented("playoffs")))
  }
})

test_that("player_id (the SIHF licence number) is a whole-number double and game ids are exact", {
  skip_on_cran()
  lineups <- .nl_parse_lineups(nl_game())
  expect_type(lineups$player_id, "double")
  expect_identical(unique(lineups$game_id), 20261105000325)
  expect_identical(sprintf("%.0f", unique(lineups$game_id)), "20261105000325")
  expect_equal(nrow(lineups), 44)
})

# ---- redaction ---------------------------------------------------------------------------------

nl_with_key <- function() {
  raw <- nl_json("teams__teamId__info.json")
  raw$fields$apiKey <- "s3cr3t-club-key"
  raw$fields$website <- "https://example.ch/?apiKey=s3cr3t-club-key&x=1"
  raw
}

test_that("team_info never returns the api key", {
  skip_on_cran()
  df <- .nl_parse_team_info(nl_with_key())
  expect_false(any(grepl("api_key", names(df))))
  expect_false(grepl("s3cr3t", paste(unlist(df), collapse = " ")))
  expect_equal(df$team_id, 101144)
  expect_equal(df$cms_node_id, 1627)
})

test_that("the getter drops the api key from the raw body", {
  skip_on_cran()
  testthat::local_mocked_bindings(.euro_get = function(url, query = list(), headers = NULL, ...) nl_with_key())
  raw <- .nl_get(paste0(NL_HOST, "/teams/101144/info"))
  expect_null(raw$fields$apiKey)
  expect_false(grepl("s3cr3t", jsonlite::toJSON(raw, auto_unbox = TRUE)))
  expect_false(grepl("s3cr3t", jsonlite::toJSON(nl_team_info(101144, return_parsed = FALSE), auto_unbox = TRUE)))
})

# ---- the getter's checks (sdv-py nationalleague_runtime._check) ---------------------------------------

test_that("every real capture passes the getter's checks, over the request it was captured with", {
  skip_on_cran()
  requests <- list(
    list("/games", list(season = 2026), "games.json"),
    list("/games/current", list(season = 2027), "games__current.json"),
    list("/games/playoffs", list(season = 2026), "games__playoffs.json"),
    list("/games/playouts", list(season = 2026), "games__playouts.json"),
    list("/games/team/101144", list(season = 2026), "games__team__teamId.json"),
    list("/games/20261105000325", list(), "games__gameId.json"),
    list("/player", list(season = 2026, phase = 4940), "player.json"),
    list("/player/topscorer", list(take = 3, season = 2026, phase = 4940), "player__topscorer.json"),
    list("/player/team/101144", list(season = 2026, phase = 4940), "player__team__teamId.json"),
    list("/player/305519", list(season = 2026, phase = 4940), "player__playerId.json"),
    list("/teams", list(season = 2026, phase = 4940), "teams.json"),
    list("/teams/101144/info", list(), "teams__teamId__info.json")
  )
  for (r in requests) {
    body <- nl_json(r[[3]])
    testthat::local_mocked_bindings(.euro_get = function(url, query = list(), headers = NULL, ...) body)
    expect_no_error(.nl_get(paste0(NL_HOST, r[[1]]), r[[2]]))
  }
})

test_that("the getter's refusals follow sdv-py's: no data, wrong answer, malformed body", {
  skip_on_cran()
  chk <- function(path, params, body) .nl_check(paste0(NL_HOST, path), params, body)
  game <- function(...) {
    body <- nl_json("games__gameId.json")
    body$overview[names(list(...))] <- list(...)
    body
  }
  nodata <- "fastRhockey_no_data"
  fetch <- "fastRhockey_asset_fetch"
  # error envelopes and stub bodies
  expect_error(chk("/games", list(season = 2026), nl_text_json('{"error": "NotFound", "status": 500}')), class = fetch)
  expect_error(chk("/games/20261105000325", list(), nl_text_json('{"message": "oops"}')), class = fetch)
  expect_error(chk("/games", list(season = 2016), nl_text_json("[]")), class = nodata)
  expect_error(chk("/player", list(season = 2026), nl_text_json("[]")), class = nodata)
  expect_error(chk("/games/20261105000325", list(), nl_text_json("{}")), class = nodata)
  expect_error(chk("/games", list(season = 2026), nl_text_json('{"games": []}')), class = fetch)
  expect_error(chk("/player/305519", list(), nl_text_json("[]")), class = fetch)
  expect_error(chk("/games/playouts", list(season = 2026), nl_text_json('[{"round": "playoutFinal", "games": []}]')), class = nodata)
  # wrong scope
  expect_error(chk("/games/team/101151", list(season = 2026), nl_json("games__team__teamId.json")), class = fetch)
  expect_error(chk("/player/team/101151", list(season = 2026), nl_json("player__team__teamId.json")), class = fetch)
  expect_error(chk("/games/20261105000326", list(), nl_json("games__gameId.json")), class = fetch)
  expect_error(chk("/player/305520", list(), nl_json("player__playerId.json")), class = fetch)
  expect_error(chk("/teams/101151/info", list(), nl_json("teams__teamId__info.json")), class = fetch)
  expect_error(chk("/games/playoffs", list(season = 2027), nl_json("games__playoffs.json")), class = fetch)
  # malformed rows and ids (type-strict: a number where a digit string belongs is refused)
  expect_error(chk("/games/20261105000325", list(), nl_text_json('{"result": {}}')), class = fetch)
  expect_error(chk("/games/20261105000325", list(), game(gameId = 20261105000325)), class = fetch)
  expect_error(chk("/games", list(season = 2026), nl_text_json('[{"gameId": 20261105000325}]')), class = fetch)
  expect_error(chk("/games/playoffs", list(season = 2026), nl_text_json('["final"]')), class = fetch)
  expect_error(chk("/player", list(season = 2026), nl_text_json('[{"playerId": 305519}]')), class = fetch)
  expect_error(chk("/teams", list(season = 2026), nl_text_json('[{"name": "EV Zug"}]')), class = fetch)
  # and the good ones pass
  expect_invisible(chk("/games", list(season = 2026), nl_json("games.json")))
  expect_invisible(chk("/games/team/101144", list(season = 2026), nl_json("games__team__teamId.json")))
})

test_that("the season goes on the wire unchanged", {
  skip_on_cran()
  seen <- nl_mock_get(nl_json("games.json"))
  nl_schedule(season = 2026)
  expect_identical(seen$params, list(season = "2026"))
  nl_schedule()
  expect_identical(seen$params, list())
})

test_that("a wrong-season answer is refused", {
  skip_on_cran()
  testthat::local_mocked_bindings(.euro_get = function(url, query = list(), headers = NULL, ...) nl_json("games.json"))
  expect_error(nl_schedule(season = 2025), "asked season 2025", class = "fastRhockey_asset_fetch")
})

test_that("bad arguments are refused before any request", {
  skip_on_cran()
  testthat::local_mocked_bindings(.nl_get = function(url, params = list()) stop("no request expected"))
  expect_error(nl_pbp(game_id = NA), class = "fastRhockey_bad_request")
  expect_error(nl_team_info("abc"), class = "fastRhockey_bad_request")
  expect_error(nl_team_info("1/../2"), class = "fastRhockey_bad_request")
  expect_error(nl_player_season(player_id = 305519.5), class = "fastRhockey_bad_request")
  expect_error(nl_schedule(season = "x"), class = "fastRhockey_bad_request")
  expect_error(nl_schedule(season = c(2025, 2026)), class = "fastRhockey_bad_request")
})

test_that("ids may be digit strings, and a 14-digit game id is sent exactly", {
  skip_on_cran()
  seen <- nl_mock_get(nl_json("games__gameId.json"))
  nl_pbp(game_id = "20261105000325")
  expect_equal(seen$url, paste0(NL_HOST, "/games/20261105000325"))
  nl_pbp(game_id = 20261105000325)
  expect_equal(seen$url, paste0(NL_HOST, "/games/20261105000325"))
  nl_team_player_stats(team_id = "101144", season = "2026")
  expect_equal(seen$url, paste0(NL_HOST, "/player/team/101144"))
  expect_equal(seen$params, list(season = "2026"))
})

test_that("return_parsed = FALSE returns the raw body", {
  skip_on_cran()
  nl_mock_get(nl_json("games.json"))
  raw <- nl_schedule(season = 2026, return_parsed = FALSE)
  expect_type(raw, "list")
  expect_equal(length(raw), 3)
  expect_false(inherits(raw, "tbl_df"))
})

test_that("a loader leaves the caller's options alone", {
  skip_on_cran()
  nl_mock_get(nl_json("games.json"))
  old <- options(scipen = 0)
  on.exit(options(old), add = TRUE)
  nl_schedule(season = 2026)
  expect_equal(getOption("scipen"), 0)
})

# ---- transport: status vocabulary, throttle on every attempt -------------------------------------------

nl_mock_http <- function(status = 200L, body = "[]", env = parent.frame()) {
  waits <- new.env()
  waits$n <- 0L
  testthat::local_mocked_bindings(
    .polite_wait = function(url, min_interval = 1) {
      waits$n <- waits$n + 1L
      invisible(0)
    },
    .env = env
  )
  httr2::local_mocked_responses(function(req) {
    code <- if (is.function(status)) status() else status
    httr2::response(status_code = code, url = req$url,
                    headers = list(`Content-Type` = "application/json"),
                    body = if (is.character(body)) charToRaw(body) else body)
  }, env = env)
  waits
}

test_that("non-200 answers follow the error vocabulary", {
  skip_on_cran()
  url <- paste0(NL_HOST, "/games")
  nl_mock_http(404L, "upstream says no")
  expect_error(.nl_get(url, list(season = 2026)), class = "fastRhockey_no_data")
  nl_mock_http(400L, "upstream says no")
  expect_error(.nl_get(url, list(season = 2026)), class = "fastRhockey_bad_request")
  nl_mock_http(422L, "upstream says no")
  expect_error(.nl_get(url, list(season = 2026)), class = "fastRhockey_bad_request")
  nl_mock_http(403L, "upstream says no")
  expect_error(.nl_get(url, list(season = 2026)), class = "fastRhockey_asset_fetch")
  nl_mock_http(500L, "upstream says no")
  expect_error(.nl_get(url, list(season = 2026)), class = "fastRhockey_asset_fetch")
})

test_that("an HTML or truncated body is a failed fetch", {
  skip_on_cran()
  url <- paste0(NL_HOST, "/games")
  for (text in c("<html>blocked</html>", '[{"gameId": "2026110')) {
    nl_mock_http(200L, text)
    expect_error(.nl_get(url, list(season = 2026)), class = "fastRhockey_asset_fetch")
  }
})

test_that("a whitespace-only 200 body is a failed fetch", {
  skip_on_cran()
  nl_mock_http(200L, "   \n")
  expect_error(.nl_get(paste0(NL_HOST, "/games"), list(season = 2026)), class = "fastRhockey_asset_fetch")
})

test_that("[needs-euro-resp-json-fix] a truly empty 200 body is a failed fetch", {
  skip_on_cran()
  nl_mock_http(200L, raw(0))
  expect_error(.nl_get(paste0(NL_HOST, "/games"), list(season = 2026)), class = "fastRhockey_asset_fetch")
})

test_that("every attempt, retries included, waits for the host's 1 s slot", {
  skip_on_cran()
  attempts <- 0L
  waits <- nl_mock_http(function() {
    attempts <<- attempts + 1L
    if (attempts < 3L) 503L else 200L
  }, body = jsonlite::toJSON(nl_json("games.json"), auto_unbox = TRUE))
  testthat::local_mocked_bindings(Sys.sleep = function(time) invisible(NULL), .package = "base")
  out <- .nl_get(paste0(NL_HOST, "/games"), list(season = 2026))
  expect_equal(attempts, 3L)
  expect_equal(waits$n, 3L)
  expect_length(out, 3)
  # a connection that never answers aborts as a failed fetch after the attempts, each throttled
  waits <- nl_mock_http(503L, "busy")
  expect_error(.nl_get(paste0(NL_HOST, "/games"), list(season = 2026)), class = "fastRhockey_asset_fetch")
  expect_equal(waits$n, 3L)
})

test_that("two requests to the host are at least one second apart (the real clock)", {
  skip_on_cran()
  body <- jsonlite::toJSON(nl_json("games.json"), auto_unbox = TRUE)
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200L, url = req$url, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw(body))
  })
  .nl_get(paste0(NL_HOST, "/games"), list(season = 2026))
  t0 <- proc.time()[["elapsed"]]
  .nl_get(paste0(NL_HOST, "/games"), list(season = 2026))
  expect_gte(proc.time()[["elapsed"]] - t0, 0.95)
})

# ---- live (opt in: RUN_EURO_TESTS=true) ----------------------------------------------------------------

test_that("live: the schedule and one game page parse to the documented columns", {
  skip_on_cran()
  skip_euro_test()
  sched <- nl_schedule(season = 2026)
  expect_gt(nrow(sched), 100)
  expect_identical(names(sched), names(nl_documented("schedule")))
  expect_true(all(startsWith(sprintf("%.0f", sched$game_id), "2026")))
  box <- nl_player_box(game_id = 20261105000325)
  expect_identical(names(box), names(nl_documented("player_box")))
  expect_false(anyNA(box$player_id))
})
