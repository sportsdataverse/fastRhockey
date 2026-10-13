# HockeyAllsvenskan (ha) wrappers: fixtures copied from sdv-py tests/fixtures/ha/,
# parity CSVs written by sdv-py's parsers on the SAME fixtures (fixtures/ha/parity/,
# `\N` = null), the transport mocked. Live tests (one request per route) sit
# behind skip_euro_test().

fx <- function(...) testthat::test_path("fixtures", "ha", ...)
fx_text <- function(...) paste(readLines(fx(...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
fx_json <- function(...) jsonlite::parse_json(fx_text(...))
to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))

HOST <- "https://hockeyallsvenskan.se"
ROUTE <- c(
  schedule = "/pages/matcher", game = "/api/game", pbp = "/api/play-by-play",
  games = "/api/all-time-games-stats", standings = "/api/league-standings-all-time",
  skater_stats = "/api/all-time-skaters-stats", goalie_stats = "/api/all-time-goalkeepers-stats",
  team_stats = "/api/all-time-teams-stats", teams = "/api/all-time-goalkeepers-stats",
  leaders = "/api/player-leaderboard", players = "/api/all-players"
)
PARSER <- list(
  schedule = .ha_parse_schedule, game = .ha_parse_schedule, pbp = .ha_parse_pbp,
  games = .ha_parse_games, standings = .ha_parse_standings, skater_stats = .ha_parse_generic,
  goalie_stats = .ha_parse_generic, team_stats = .ha_parse_team_stats, teams = .ha_parse_teams,
  leaders = .ha_parse_leaders, players = .ha_parse_players
)
SHORTS <- names(ROUTE)
r_class <- c(Int64 = "integer", String = "character", Float64 = "double", Boolean = "logical")

parity_schemas <- function() jsonlite::fromJSON(fx("parity", "schemas.json"))
manifest <- function() jsonlite::fromJSON(fx("parity", "manifest.json"), simplifyVector = FALSE)

# A fixture body stamped the way the runtime stamps a live one.
stamped_fixture <- function(fixture, short, request) {
  body <- fx_json(fixture)
  if (!is.null(request)) {
    built <- .ha_build_body(ROUTE[[short]], request)
    if (length(built$stamp)) body[["_request"]] <- built$stamp
  }
  body
}

expect_schema <- function(tbl, short) {
  s <- parity_schemas()[[short]]
  expect_s3_class(tbl, "tbl_df")
  expect_identical(names(tbl), names(s))
  for (nm in names(s)) {
    expect_identical(typeof(tbl[[nm]]), r_class[[s[[nm]]]], info = sprintf("%s: %s", short, nm))
  }
}

# Parity: the R tibble equals sdv-py's frame on the same fixture. An NA on one
# side and a value on the other is a mismatch (never silently equal).
expect_parity <- function(tbl, short, tag) {
  expect_parity_file(tbl, sprintf("%s__%s.csv", short, tag), sprintf("%s/%s", short, tag))
}
expect_parity_file <- function(tbl, file, label) {
  exp <- utils::read.csv(fx("parity", file),
    colClasses = "character", na.strings = "\\N", fileEncoding = "UTF-8",
    check.names = FALSE, strip.white = FALSE
  )
  expect_identical(names(tbl), names(exp))
  expect_identical(nrow(tbl), nrow(exp))
  for (nm in names(exp)) {
    g <- tbl[[nm]]
    e <- exp[[nm]]
    info <- sprintf("%s column %s", label, nm)
    if (is.character(g)) {
      expect_identical(is.na(g), is.na(e), info = info)
      expect_identical(enc2utf8(g[!is.na(g)]), enc2utf8(e[!is.na(e)]), info = info)
    } else if (is.logical(g)) {
      expect_identical(ifelse(is.na(g), NA_character_, ifelse(g, "true", "false")), e, info = info)
    } else {
      en <- suppressWarnings(as.numeric(e))
      expect_identical(is.na(g), is.na(en), info = info)
      expect_equal(as.numeric(g[!is.na(g)]), en[!is.na(en)], info = info)
    }
  }
}

# ---- fake transport ----------------------------------------------------------

fake_resp <- function(text = "", status = 200L, ctype = "application/json", headers = list(),
                      url = paste0(HOST, "/api/x")) {
  httr2::response(
    status_code = as.integer(status), url = url,
    headers = c(list(`Content-Type` = ctype), headers),
    body = if (nzchar(text)) charToRaw(enc2utf8(text)) else raw(0)
  )
}
ok_resp <- function(body) fake_resp(if (is.character(body)) body else to_json(body))
fail_cond <- function(msg = "connection reset") {
  structure(class = c("httr2_failure", "httr2_error", "rlang_error", "error", "condition"),
            list(message = msg, call = NULL))
}

# Queue the answers (the last one repeats); every request, clock wait (and the interval it
# asked for) and re-fetch sleep is recorded. `st$waits` is the interval of each retry.
mock_transport <- function(..., env = parent.frame()) {
  st <- new.env()
  st$queue <- list(...)
  st$reqs <- list()
  st$waited <- character()
  st$intervals <- numeric()
  st$slept <- numeric()
  take <- function(req) {
    st$reqs[[length(st$reqs) + 1L]] <- req
    nxt <- if (length(st$queue) > 1L) {
      x <- st$queue[[1L]]
      st$queue <- st$queue[-1L]
      x
    } else {
      st$queue[[1L]]
    }
    if (inherits(nxt, "condition")) stop(nxt)
    nxt
  }
  testthat::local_mocked_bindings(
    .polite_wait = function(url, min_interval = 1) {
      st$waited <- c(st$waited, url)
      st$intervals <- c(st$intervals, min_interval)
      invisible(0)
    },
    .ha_sleep = function(seconds) st$slept <- c(st$slept, seconds),
    .env = env
  )
  httr2::local_mocked_responses(function(req) take(req), env = env)
  st
}
sent_body <- function(st, i = 1L) st$reqs[[i]]$body$data

# ---- the documented columns and dtypes ---------------------------------------

test_that("the embedded schemas are sdv-py's returns-schemas", {
  skip_on_cran()
  s <- parity_schemas()
  for (short in SHORTS) {
    expect_identical(names(.HA_SCHEMAS[[short]]), names(s[[short]]), info = short)
  }
  expect_identical(.HA_DTYPES[["player_statnet_id"]], "integer")
  expect_identical(.HA_DTYPES[["team_statnet_id"]], "character")
  expect_identical(.HA_DTYPES[["statnet_game_number"]], "integer")
  expect_identical(.HA_DTYPES[["game_cms_id"]], "integer")
  expect_identical(.HA_DTYPES[["home_powerplay_efficiency"]], "numeric")
})

test_that("every wrapper's fixture parses to sdv-py's frame: columns, dtypes, snake_case", {
  skip_on_cran()
  for (m in manifest()) {
    if (m$tag != "base") next
    short <- m$short
    tbl <- PARSER[[short]](stamped_fixture(m$fixture, short, m$request), names(.HA_SCHEMAS[[short]]))
    expect_schema(tbl, short)
    expect_gt(nrow(tbl), 0L)
    expect_true(all(grepl("^[a-z0-9]+(_[a-z0-9]+)*$", names(tbl))), info = short)
    if ("season" %in% names(tbl)) {
      expect_type(tbl$season, "integer")
      expect_false(anyNA(tbl$season))
    }
  }
})

test_that("parity: the R tibble equals sdv-py's frame on every committed fixture", {
  skip_on_cran()
  for (m in manifest()) {
    short <- m$short
    tbl <- PARSER[[short]](stamped_fixture(m$fixture, short, m$request), names(.HA_SCHEMAS[[short]]))
    expect_equal(nrow(tbl), m$rows, info = paste(short, m$tag))
    expect_parity(tbl, short, m$tag)
  }
})

test_that("parity on hand-built edge payloads: string counts, mixed types, nested lists, unicode", {
  skip_on_cran()
  cases <- fx_json("parity", "edge_cases.json")
  expect_gte(length(cases), 9L)
  parsers <- c(
    parse_ha_schedule = ".ha_parse_schedule", parse_ha_games = ".ha_parse_games",
    parse_ha_standings = ".ha_parse_standings", parse_ha_leaders = ".ha_parse_leaders",
    parse_ha_pbp = ".ha_parse_pbp", parse_ha_players = ".ha_parse_players",
    parse_ha_teams = ".ha_parse_teams", parse_ha_team_stats = ".ha_parse_team_stats",
    parse_ha = ".ha_parse_generic"
  )
  for (nm in names(cases)) {
    tbl <- get(parsers[[cases[[nm]]$parser]])(cases[[nm]]$payload)
    expect_parity_file(tbl, paste0(nm, ".csv"), nm)
  }
})

test_that("the full games body has float rates whatever the wire type", {
  skip_on_cran()
  raw <- fx_json("full", "all-time-games-stats__2026__played.json")
  wire <- unique(vapply(raw$results, function(g) {
    class(g$GameStatistics$home_team$powerplay$efficiency)[1]
  }, ""))
  expect_true(all(c("character", "integer", "NULL") %in% wire)) # "14.29", 0 and null in one column
  tbl <- .ha_parse_games(stamped_fixture(
    "full/all-time-games-stats__2026__played.json", "games", list(season = 2027, game_status = "played")
  ))
  expect_equal(nrow(tbl), raw$count)
  expect_equal(nrow(tbl), 68L)
  for (side in c("home", "away")) {
    expect_type(tbl[[paste0(side, "_powerplay_efficiency")]], "double")
    expect_type(tbl[[paste0(side, "_shots_efficiency")]], "double")
    expect_type(tbl[[paste0(side, "_shots_total")]], "integer")
  }
  expect_lt(sum(is.na(tbl$home_powerplay_efficiency)), nrow(tbl))
})

test_that("every refs capture parses (with and without a stamped season)", {
  skip_on_cran()
  caps <- c(
    "pages__matcher" = "schedule", "api__game" = "schedule", "api__play-by-play" = "pbp",
    "api__all-time-games-stats" = "games", "api__league-standings-all-time" = "standings",
    "api__all-time-skaters-stats" = "generic", "api__all-time-goalkeepers-stats" = "generic",
    "api__all-time-teams-stats" = "team_stats", "api__player-leaderboard" = "leaders",
    "api__all-players" = "players"
  )
  files <- setdiff(basename(list.files(fx(), "\\.json$")), "spec_snapshot.json")
  expect_equal(length(files), 14L)
  for (f in files) {
    parser <- get(paste0(".ha_parse_", caps[[sub("@.*$|\\.json$", "", f)]]))
    tbl <- parser(fx_json(f))
    expect_gt(nrow(tbl), 0L)
    expect_true(all(grepl("^[a-z0-9]+(_[a-z0-9]+)*$", names(tbl))), info = f)
  }
})

test_that("an empty or malformed payload is a zero-row tibble with the documented columns", {
  skip_on_cran()
  payloads <- list(
    list(), setNames(list(), character()), NULL, "x", 3, list("x"),
    list(`_request` = list(season = 2026L)), list(results = "x"), list(data = list(1)), list(players = 5)
  )
  for (short in SHORTS) {
    cols <- names(.HA_SCHEMAS[[short]])
    for (payload in payloads) {
      tbl <- PARSER[[short]](payload, cols)
      expect_s3_class(tbl, "tbl_df")
      expect_equal(nrow(tbl), 0L, info = short)
      expect_identical(names(tbl), cols, info = short)
      expect_identical(
        unname(vapply(tbl, typeof, "")),
        unname(c(integer = "integer", numeric = "double", character = "character", logical = "logical")[.HA_SCHEMAS[[short]]]),
        info = short
      )
      expect_equal(nrow(PARSER[[short]](payload)), 0L, info = short) # no columns asked for: still zero rows
    }
  }
})

test_that("an empty payload keeps the pinned dtypes of a name rule too", {
  skip_on_cran()
  cols <- c("season", "team_statnet_id", "player_statnet_id", "team_cms_id", "team_code", "svs_perc", "is_played")
  for (f in list(.ha_parse_generic, .ha_parse_leaders, .ha_parse_pbp, .ha_parse_games)) {
    tbl <- f(list(), cols)
    expect_identical(
      unname(vapply(tbl, typeof, "")),
      c("integer", "character", "integer", "integer", "character", "double", "logical")
    )
  }
})

test_that("an empty frame concatenates with a populated one", {
  skip_on_cran()
  for (m in manifest()) {
    if (m$tag != "base") next
    short <- m$short
    cols <- names(.HA_SCHEMAS[[short]])
    full <- PARSER[[short]](stamped_fixture(m$fixture, short, m$request), cols)
    empty <- PARSER[[short]](list(), cols)
    both <- dplyr::bind_rows(empty, full)
    expect_equal(nrow(both), nrow(full), info = short)
    expect_identical(unname(vapply(empty, typeof, "")), unname(vapply(full[cols], typeof, "")), info = short)
  }
})

test_that("a documented column missing from the body is added as nulls, in documented order", {
  skip_on_cran()
  tbl <- .ha_parse_leaders(fx_json("api__player-leaderboard.json"), c("team_code", "rank", "never_sent"))
  expect_identical(names(tbl)[1:3], c("team_code", "rank", "never_sent"))
  expect_gt(ncol(tbl), 3L)
  expect_true(all(is.na(tbl$never_sent)))
})

test_that("attendance is an integer count whatever separator the site sends", {
  skip_on_cran()
  for (case in list(list("13950", 13950L), list("13 950", 13950L), list("1,386", 1386L),
                    list("", NA_integer_), list(987L, 987L))) {
    tbl <- .ha_parse_schedule(list(data = list(list(id = 1L, attendance = case[[1]]))))
    expect_identical(tbl$attendance, case[[2]])
  }
})

# ---- the id traps ------------------------------------------------------------

test_that("CMS games keep the three team key spaces apart", {
  skip_on_cran()
  m <- Filter(function(m) m$short == "game", manifest())[[1]]
  g <- .ha_parse_schedule(stamped_fixture(m$fixture, "game", NULL))
  expect_identical(c(g$home_statnet_id, g$away_statnet_id), c("AIK", "MODO")) # StatNet
  expect_identical(c(g$home_team_code, g$away_team_code), c("AIK", "MoDo")) # display
  expect_identical(g$home_team_cms_id, 81L) # CMS
  expect_identical(c(g$statnet_game_number, g$game_cms_id), c(23401L, 6566L))
  expect_identical(c(g$season, g$cms_season), c(2027L, "season_2026")) # CMS keys a START year
})

test_that("the teams list is the StatNet id to display code crosswalk", {
  skip_on_cran()
  tbl <- .ha_parse_teams(stamped_fixture("api__all-time-goalkeepers-stats.json", "teams", list(season = 2026)))
  pairs <- setNames(tbl$team_statnet_id, tbl$team_code)
  expect_identical(
    unname(pairs[c("MoDo", "MORA", "NVIF", "ÖIK")]),
    c("MODO", "MIK", "NYB", "OSIK")
  )
})

test_that("standings' mixed teamCode is kept apart from the display code", {
  skip_on_cran()
  tbl <- .ha_parse_standings(stamped_fixture("api__league-standings-all-time.json", "standings", list(season = 2026)))
  mixed <- setNames(tbl$team_code_mixed, tbl$team_code)
  expect_identical(mixed[["MoDo"]], "MODO") # the StatNet id
  expect_identical(unname(mixed[c("MORA", "NVIF", "ÖIK")]), c("MORA", "NVIF", "ÖIK")) # display codes
})

test_that("standings wins are regulation only, team-table wins are totals", {
  skip_on_cran()
  st <- .ha_parse_standings(stamped_fixture("api__league-standings-all-time.json", "standings", list(season = 2026)))
  tm <- .ha_parse_team_stats(stamped_fixture("api__all-time-teams-stats.json", "team_stats", list(season = 2026)))
  expect_identical(typeof(st$team_code), typeof(tm$team_code)) # one key space, one dtype, before the join
  ifb <- merge(st, tm, by = c("season", "team_code"))
  ifb <- ifb[ifb$team_code == "IFB", ]
  expect_identical(
    c(ifb$regulation_wins, ifb$overtime_and_shootout_wins, ifb$wins_total), c(34L, 7L, 41L)
  )
  expect_equal(ifb$regulation_wins + ifb$otw + ifb$sow, ifb$wins_total)
  expect_false("wins" %in% names(st))
  expect_false("w" %in% names(tm))
})

test_that("player ids exist only on play-by-play and the leaderboard", {
  skip_on_cran()
  pbp <- .ha_parse_pbp(stamped_fixture("api__play-by-play.json", "pbp", list(
    statNetGameNumber = "23401", homeStatNetId = "AIK", awayStatNetId = "MODO",
    scheduledDateTime = "2026-09-18T17:00:00.000Z"
  )))
  leaders <- .ha_parse_leaders(fx_json("api__player-leaderboard.json"))
  expect_type(pbp$player_statnet_id, "integer")
  expect_type(leaders$player_statnet_id, "integer")
  expect_true(920L %in% pbp$player_statnet_id) # Jhonas Enroth (= SHL isa 920)
  for (short in c("skater_stats", "goalie_stats", "games")) {
    cols <- names(.HA_SCHEMAS[[short]])
    expect_false(any(grepl("^player_.*id$", cols)), info = short)
  }
  expect_identical(.HA_DTYPES[["team_statnet_id"]], "character")
})

test_that("play-by-play rows carry the request and no coordinates", {
  skip_on_cran()
  pbp <- .ha_parse_pbp(stamped_fixture("api__play-by-play.json", "pbp", list(
    statNetGameNumber = "23401", homeStatNetId = "AIK", awayStatNetId = "MODO",
    scheduledDateTime = "2026-09-18T17:00:00.000Z"
  )))
  expect_identical(unique(pbp$statnet_game_number), 23401L)
  expect_true(all(pbp$team_statnet_id %in% c("AIK", "MODO")))
  expect_setequal(pbp$event_type, c("GoalkeeperEvent", "Shot", "Goal", "Penalty", "Timeout"))
  expect_type(pbp$time, "integer")
  expect_type(pbp$period, "integer")
  expect_type(pbp$is_penalty_shot, "logical")
  expect_false(any(grepl("(^|_)(x|y|coord|on_ice)", names(pbp))))
})

test_that("the player directory's ids are CMS ids", {
  skip_on_cran()
  pl <- .ha_parse_players(stamped_fixture("api__all-players.json", "players", list(teamShortName = "AIK")))
  expect_type(pl$player_cms_id, "integer")
  expect_identical(unique(pl$team_cms_id), 81L)
  expect_identical(unique(pl$team_code), "AIK")
  expect_false("headshots" %in% names(pl))
})

test_that("unplayed games are returned and flagged, never dropped", {
  skip_on_cran()
  body <- fx_json("api__all-time-games-stats@2026-unplayed.json")
  body[["_request"]] <- list(season = 2027L)
  tbl <- .ha_parse_games(body)
  expect_equal(nrow(tbl), 3L)
  expect_identical(tbl$is_played, rep(FALSE, 3))
  expect_identical(tbl$decided_in, rep("", 3))
  expect_true(all(is.na(tbl$attendance)))
  expect_true(all(is.na(tbl$period_scores)))
  expect_identical(tbl$home_score + tbl$away_score, rep(0L, 3))
})

# ---- seasons -----------------------------------------------------------------

test_that("the current season rolls over on July 1", {
  skip_on_cran()
  expect_identical(.ha_current_season(as.Date("2026-10-12")), 2027L)
  expect_identical(.ha_current_season(as.Date("2027-06-30")), 2027L)
  expect_identical(.ha_current_season(as.Date("2027-07-01")), 2028L)
})

test_that("a season END year maps to the START-year body key", {
  skip_on_cran()
  expect_identical(.ha_body_season(2026), list(end = 2026L, wire = "2025"))
  expect_identical(.ha_body_season(2026L), list(end = 2026L, wire = "2025"))
  expect_identical(.ha_body_season("2026"), list(end = 2026L, wire = "2025"))
  expect_identical(.ha_body_season(NULL, today = as.Date("2026-10-12")), list(end = 2027L, wire = "2026"))
  built <- .ha_build_body("/api/all-time-skaters-stats", list(season = 2006, categories = "goals"))
  expect_identical(built$body$season, "2005")
  expect_identical(built$stamp, list(season = 2006L))
  expect_identical(built$body$categories, "goals")
})

test_that("a season before the floor is no data, without a request", {
  skip_on_cran()
  for (s in c(2005, 1999)) {
    expect_error(.ha_body_season(s), class = "fastRhockey_no_data", regexp = "2005/06")
  }
  st <- mock_transport(fake_resp("{}"))
  expect_error(ha_team_stats(season = 2005), class = "fastRhockey_no_data")
  expect_length(st$reqs, 0L)
})

test_that("a season must be a year", {
  skip_on_cran()
  for (s in list(TRUE, 2026.5, "x", c(2026, 2027), NA_real_)) {
    expect_error(.ha_body_season(s), class = "fastRhockey_bad_request", regexp = "END year")
  }
})

# ---- default bodies ----------------------------------------------------------

test_that("the runtime's default bodies are the spec's request examples", {
  skip_on_cran()
  snap <- fx_json("spec_snapshot.json")
  expect_gt(length(snap$request_examples), 0L)
  for (path in names(snap$request_examples)) {
    example <- snap$request_examples[[path]]
    example <- example[setdiff(names(example), c("season", "teamShortName"))]
    if (!length(example) || path == "/api/play-by-play") next
    default <- .HA_BODIES[[path]]
    default <- default[setdiff(names(default), c("season", "teamShortName"))]
    expect_equal(default[order(names(default))], example[order(names(example))], info = path)
  }
  expect_setequal(names(snap$categories), names(.HA_CATEGORIES))
  for (path in names(snap$categories)) {
    expect_identical(unlist(snap$categories[[path]]), .HA_CATEGORIES[[path]], info = path)
  }
})

test_that("request bodies and stamps equal sdv-py's build_body", {
  skip_on_cran()
  cases <- fx_json("parity", "bodies.json")
  expect_gt(length(cases), 10L)
  for (case in cases) {
    built <- .ha_build_body(case$path, case$params, today = as.Date("2026-10-12"))
    info <- paste(case$path, jsonlite::toJSON(case$params, auto_unbox = TRUE, null = "null"))
    expect_setequal(names(built$body), names(case$body))
    expect_equal(built$body[order(names(built$body))], case$body[order(names(case$body))], info = info)
    expect_equal(length(built$stamp), length(case$stamp), info = info)
    if (length(case$stamp)) {
      expect_equal(built$stamp[order(names(built$stamp))], case$stamp[order(names(case$stamp))], info = info)
    }
  }
})

# ---- transport: POST ---------------------------------------------------------

skaters_body <- function(...) {
  b <- fx_json("api__all-time-skaters-stats.json")
  b$count <- 3L
  b$total_pages <- 1L
  modifyList(b, list(...))
}

test_that("the runtime POSTs the default body with the mapped season, throttled", {
  skip_on_cran()
  st <- mock_transport(ok_resp(skaters_body()))
  body <- .ha_get(paste0(HOST, "/api/all-time-skaters-stats"), list(season = 2026, categories = NULL, team = NULL))
  sent <- sent_body(st)
  expect_identical(st$reqs[[1]]$method %||% "POST", "POST")
  expect_identical(
    sent[order(names(sent))],
    c(.HA_BODIES[["/api/all-time-skaters-stats"]], list(season = "2025"))[order(names(c(.HA_BODIES[["/api/all-time-skaters-stats"]], list(season = "2025"))))]
  )
  expect_identical(st$waited, paste0(HOST, "/api/all-time-skaters-stats"))
  expect_identical(body[["_request"]], list(season = 2026L))
})

test_that("the runtime stamps the play-by-play request", {
  skip_on_cran()
  st <- mock_transport(ok_resp(fx_text("api__play-by-play.json")))
  req <- list(
    statNetGameNumber = 23401, homeStatNetId = "AIK", awayStatNetId = "MODO",
    scheduledDateTime = "2026-09-18T17:00:00.000Z", playedDateTime = NULL
  )
  body <- .ha_get(paste0(HOST, "/api/play-by-play"), req)
  expect_identical(sent_body(st)$statNetGameNumber, "23401") # the site's string form
  expect_false("playedDateTime" %in% names(sent_body(st)))
  expect_identical(
    body[["_request"]],
    list(statnet_game_number = "23401", home_statnet_id = "AIK", away_statnet_id = "MODO")
  )
  expect_identical(unique(.ha_parse_pbp(body)$statnet_game_number), 23401L)
})

test_that("the runtime GETs a game record by slug", {
  skip_on_cran()
  st <- mock_transport(ok_resp(fx_text("api__game.json")))
  body <- .ha_get(paste0(HOST, "/api/game"), list(slug = "20260918-aik-modo"))
  expect_match(st$reqs[[1]]$url, "slug=20260918-aik-modo", fixed = TRUE)
  expect_null(st$reqs[[1]]$body)
  expect_false("_request" %in% names(body))
})

test_that("a 502 POST is retried, each attempt behind the clock", {
  skip_on_cran()
  st <- mock_transport(fake_resp('{"x":1}', 502L), ok_resp(fx_text("api__league-standings-all-time.json")))
  .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026))
  expect_length(st$reqs, 2L)
  expect_length(st$waited, 2L)
  expect_equal(st$intervals[-1], 2)
})

test_that("non-200 answers follow the error vocabulary", {
  skip_on_cran()
  for (case in list(list(404L, "fastRhockey_no_data"), list(400L, "fastRhockey_bad_request"),
                    list(422L, "fastRhockey_bad_request"), list(403L, "fastRhockey_asset_fetch"),
                    list(502L, "fastRhockey_asset_fetch"))) {
    mock_transport(fake_resp('{"error":"Missing league or season"}', case[[1]]))
    expect_error(
      .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026)),
      class = case[[2]]
    )
  }
})

test_that("a 429 or 503 Retry-After lengthens the wait to what the server asks", {
  skip_on_cran()
  for (status in c(429L, 503L)) {
    busy <- fake_resp('{"x":1}', status, headers = list(`Retry-After` = "7"))
    st <- mock_transport(busy, fake_resp('{"x":1}', status), ok_resp(fx_text("api__league-standings-all-time.json")))
    .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026))
    expect_equal(st$intervals[-1], c(7, 4)) # the server's wait, then the plain backoff
    expect_length(st$waited, 3L)
  }
})

test_that("a Retry-After beyond the cap is capped at 120 s", {
  skip_on_cran()
  busy <- fake_resp('{"x":1}', 503L, headers = list(`Retry-After` = "99999"))
  st <- mock_transport(busy, ok_resp(fx_text("api__league-standings-all-time.json")))
  .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026))
  expect_equal(st$intervals[-1], 120)
})

test_that("a dead connection is an asset-fetch error after three attempts", {
  skip_on_cran()
  st <- mock_transport(fail_cond())
  expect_error(
    .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026)),
    class = "fastRhockey_asset_fetch", regexp = "fetch failed"
  )
  expect_length(st$reqs, 3L)
  expect_equal(st$intervals[-1], c(2, 4))
})

test_that("a cut-off 2xx JSON body is fetched again, twice, then refused", {
  skip_on_cran()
  cut <- fake_resp('{"standings": [{"team')
  st <- mock_transport(cut, ok_resp(fx_text("api__league-standings-all-time.json")))
  body <- .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026))
  expect_length(st$reqs, 2L)
  expect_equal(st$slept, 1)
  expect_true(length(body$standings) > 0L)
  st <- mock_transport(cut)
  expect_error(
    .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026)),
    class = "fastRhockey_asset_fetch", regexp = "non-JSON"
  )
  expect_length(st$reqs, 3L)
})

test_that("a non-JSON 2xx body is refused at once", {
  skip_on_cran()
  st <- mock_transport(fake_resp("<html>blocked</html>", ctype = "text/html"))
  expect_error(
    .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026)),
    class = "fastRhockey_asset_fetch"
  )
  expect_length(st$reqs, 1L)
})

test_that("an empty 200 is an asset-fetch error", {
  skip_on_cran()
  mock_transport(fake_resp(""))
  expect_error(
    .ha_get(paste0(HOST, "/api/league-standings-all-time"), list(season = 2026)),
    class = "fastRhockey_asset_fetch", regexp = "empty body"
  )
})

test_that("bad fields are refused before sending", {
  skip_on_cran()
  cases <- list(
    list("/api/all-time-skaters-stats", list(categories = "all"), "categories"),
    list("/api/all-time-teams-stats", list(categories = "summary"), "categories"),
    list("/api/all-time-goalkeepers-stats", list(categories = "points"), "categories"),
    list("/api/league-standings-all-time", list(phase = "SHL"), "phase"),
    list("/api/player-leaderboard", list(metric = ""), "metric"),
    list("/api/all-players", list(teamShortName = NULL), "team")
  )
  for (case in cases) {
    st <- mock_transport(fake_resp("{}"))
    expect_error(.ha_get(paste0(HOST, case[[1]]), case[[2]]), class = "fastRhockey_bad_request", regexp = case[[3]])
    expect_length(st$reqs, 0L)
  }
  expect_error(.ha_build_body("/api/nope", list()), class = "fastRhockey_bad_request")
})

test_that("empty and error-shaped bodies are refused", {
  skip_on_cran()
  cases <- list(
    list("/api/all-time-skaters-stats", list(count = 0L, total_pages = 0L, results = list()), "fastRhockey_no_data", "no rows"),
    list("/api/all-time-teams-stats", list(data = list(), sort_by_options = list()), "fastRhockey_no_data", "no rows"),
    list("/api/league-standings-all-time", list(standings = list()), "fastRhockey_no_data", "no rows"),
    list("/api/league-standings-all-time", list(error = "StatNet API error"), "fastRhockey_asset_fetch", "error envelope"),
    list("/api/league-standings-all-time", list("x"), "fastRhockey_asset_fetch", "not an object"),
    list("/api/league-standings-all-time", list(standings = list("x")), "fastRhockey_asset_fetch", "list of objects")
  )
  for (case in cases) {
    mock_transport(ok_resp(case[[2]]))
    expect_error(
      .ha_get(paste0(HOST, case[[1]]), list(season = 2026)),
      class = case[[3]], regexp = case[[4]]
    )
  }
})

test_that("a truncated page is refused", {
  skip_on_cran()
  for (patch in list(list(total_pages = 2L), list(count = 999L), list(total_pages = TRUE), list(count = NULL))) {
    body <- modifyList(modifyList(fx_json("api__all-time-goalkeepers-stats.json"), list(count = 3L, total_pages = 1L)), patch)
    mock_transport(ok_resp(body))
    expect_error(
      .ha_get(paste0(HOST, "/api/all-time-goalkeepers-stats"), list(season = 2026)),
      class = "fastRhockey_asset_fetch", regexp = "truncated"
    )
  }
})

test_that("unplayed games are returned, out-of-season rows refused", {
  skip_on_cran()
  body <- fx_json("api__all-time-games-stats@2026-unplayed.json")
  body$count <- 3L
  body$total_pages <- 1L
  mock_transport(ok_resp(body))
  raw <- .ha_get(paste0(HOST, "/api/all-time-games-stats"), list(season = 2027))
  expect_identical(.ha_parse_games(raw)$is_played, rep(FALSE, 3))
  mock_transport(ok_resp(body))
  expect_error(
    .ha_get(paste0(HOST, "/api/all-time-games-stats"), list(season = 2026)),
    class = "fastRhockey_asset_fetch", regexp = "outside season 2026"
  )
  bad <- body
  bad$results <- list(modifyList(body$results[[1]], list(decided_in = NA)))
  bad$results[[1]]["decided_in"] <- list(NULL)
  bad$count <- 1L
  mock_transport(ok_resp(bad))
  expect_error(
    .ha_get(paste0(HOST, "/api/all-time-games-stats"), list(season = 2027)),
    class = "fastRhockey_asset_fetch", regexp = "decided_in"
  )
})

test_that("a truncated leaderboard is refused", {
  skip_on_cran()
  for (patch in list(list(totalPages = 2L), list(totalCount = 25L), list(totalPages = TRUE))) {
    body <- modifyList(modifyList(fx_json("api__player-leaderboard.json"), list(totalCount = 3L, totalPages = 1L)), patch)
    mock_transport(ok_resp(body))
    expect_error(
      .ha_get(paste0(HOST, "/api/player-leaderboard"), list(metric = "TP")),
      class = "fastRhockey_asset_fetch", regexp = "truncated board"
    )
  }
})

test_that("the leaderboard metric is checked type-strictly", {
  skip_on_cran()
  for (row_metric in list("G", NULL, list("TP"), 1L)) {
    body <- fx_json("api__player-leaderboard.json")
    body$totalCount <- 3L
    body$totalPages <- 1L
    body$players[[2]]["metric"] <- list(row_metric)
    mock_transport(ok_resp(body))
    expect_error(
      .ha_get(paste0(HOST, "/api/player-leaderboard"), list(metric = "TP")),
      class = "fastRhockey_asset_fetch", regexp = "outside metric"
    )
  }
})

test_that("a complete leaderboard is accepted and its default body sent", {
  skip_on_cran()
  body <- modifyList(fx_json("api__player-leaderboard.json"), list(totalCount = 3L, totalPages = 1L))
  st <- mock_transport(ok_resp(body))
  .ha_get(paste0(HOST, "/api/player-leaderboard"), list(metric = "TP"))
  expect_identical(sent_body(st)$pageSize, 100L)
  expect_true("team" %in% names(sent_body(st)))
  expect_null(sent_body(st)$team)
})

test_that("a multi-page directory is refused; rows must be the requested team", {
  skip_on_cran()
  for (pg in list(list(page = 1L, pageSize = 100L, pageCount = 2L, total = 130L),
                  list(page = 1L, pageSize = 100L, pageCount = 1L, total = 29L))) {
    mock_transport(ok_resp(modifyList(fx_json("api__all-players.json"), list(pagination = pg))))
    expect_error(
      .ha_get(paste0(HOST, "/api/all-players"), list(teamShortName = "AIK")),
      class = "fastRhockey_asset_fetch", regexp = "multi-page"
    )
  }
  body <- fx_json("api__all-players.json")
  body$pagination <- list(page = 1L, pageSize = 100L, pageCount = 1L, total = 3L)
  st <- mock_transport(ok_resp(body))
  .ha_get(paste0(HOST, "/api/all-players"), list(teamShortName = "AIK"))
  expect_identical(sent_body(st)$teamShortName, "AIK")
  expect_error(
    .ha_get(paste0(HOST, "/api/all-players"), list(teamShortName = "MORA")),
    class = "fastRhockey_asset_fetch", regexp = "outside teamShortName"
  )
})

test_that("play-by-play events of another team are refused", {
  skip_on_cran()
  req <- list(statNetGameNumber = "1", homeStatNetId = "AIK", awayStatNetId = "MIK", scheduledDateTime = "x")
  mock_transport(ok_resp(fx_text("api__play-by-play.json"))) # AIK v MODO
  expect_error(.ha_get(paste0(HOST, "/api/play-by-play"), req), class = "fastRhockey_asset_fetch", regexp = "outside")
  mock_transport(ok_resp(list(game_info = setNames(list(), character()), game_events = list(list(Period = "1", Events = list())))))
  expect_error(.ha_get(paste0(HOST, "/api/play-by-play"), req), class = "fastRhockey_no_data", regexp = "no play-by-play")
  mock_transport(fake_resp('{"error":"StatNet API error: 404"}', 404L))
  expect_error(.ha_get(paste0(HOST, "/api/play-by-play"), req), class = "fastRhockey_no_data")
})

test_that("game record refusals", {
  skip_on_cran()
  mock_transport(ok_resp(list(data = list(), meta = setNames(list(), character()))))
  expect_error(
    .ha_get(paste0(HOST, "/api/game"), list(slug = "20250101-aik-modo")),
    class = "fastRhockey_no_data", regexp = "2025/26"
  )
  mock_transport(ok_resp(fx_text("api__game.json")))
  expect_error(
    .ha_get(paste0(HOST, "/api/game"), list(slug = "20260919-mik-aik")),
    class = "fastRhockey_asset_fetch", regexp = "outside slug"
  )
})

# ---- the schedule page (HTML) ------------------------------------------------

# A Next.js page whose game-calendar element is split across two flight chunks.
page_with <- function(props_json, split = 40L) {
  text <- paste0('0:["$","$L4c","games.game-calendar-265-0",', props_json, "]\n")
  chunks <- c(substr(text, 1, split), substr(text, split + 1L, nchar(text)))
  pushes <- paste0("<script>self.__next_f.push([1,", vapply(chunks, to_json, ""), "])</script>", collapse = "")
  paste0("<html><body>", pushes, "</body></html>")
}
compact_props <- function(props = fx_json("pages__matcher.json")) to_json(props)
page_resp <- function(text, ctype = "text/html; charset=utf-8") fake_resp(text, ctype = ctype)

test_that("the schedule page decodes the split flight element", {
  skip_on_cran()
  props <- fx_json("pages__matcher.json")
  st <- mock_transport(page_resp(page_with(compact_props(props))))
  body <- .ha_get(paste0(HOST, "/pages/matcher"))
  expect_identical(length(body$games), length(props$games))
  expect_equal(body$games[[1]]$slug, props$games[[1]]$slug)
  expect_equal(nrow(.ha_parse_schedule(body)), 3L)
  expect_length(st$reqs, 1L)
})

test_that("the schedule page decodes through a multi-megabyte flight text", {
  skip_on_cran()
  props <- fx_json("pages__matcher.json")
  noise <- paste0('1:["$","div",null,{"className":"', strrep("x", 300000), '"}]\n')
  text <- paste0(
    '0:["$","$L4c","games.game-calendar-265-0",', compact_props(props), "]\n", strrep(noise, 8)
  )
  pushes <- paste0("<script>self.__next_f.push([1,", to_json(text), "])</script>")
  mock_transport(page_resp(paste0("<html>", pushes, "</html>")))
  body <- .ha_get(paste0(HOST, "/pages/matcher"))
  expect_length(body$games, 3L)
})

test_that("a props object longer than the first scan window is decoded", {
  skip_on_cran()
  props <- fx_json("pages__matcher.json")
  props$games <- rep(props$games, 40) # ~ 700 KB of props with quotes and brackets everywhere
  mock_transport(page_resp(page_with(compact_props(props))))
  expect_length(.ha_get(paste0(HOST, "/pages/matcher"))$games, 120L)
})

test_that("schedule page refusals", {
  skip_on_cran()
  other <- sub("games.game-calendar", "games.other", page_with(compact_props(list(x = 1L))), fixed = TRUE)
  cases <- list(
    list("<html>no flight</html>", "text/html", "fastRhockey_asset_fetch", "no self.__next_f"),
    list(other, "text/html", "fastRhockey_asset_fetch", "no games"),
    list(page_with(to_json(list(season = "current", games = "x"))), "text/html", "fastRhockey_asset_fetch", "no games list"),
    list(page_with(to_json(list(season = "current", games = list()))), "text/html", "fastRhockey_no_data", "no games"),
    list('{"games": []}', "application/json", "fastRhockey_asset_fetch", "not text/html")
  )
  for (case in cases) {
    mock_transport(page_resp(case[[1]], case[[2]]))
    expect_error(.ha_get(paste0(HOST, "/pages/matcher")), class = case[[3]], regexp = case[[4]])
  }
})

test_that("the schedule page refuses a mix of seasons", {
  skip_on_cran()
  props <- fx_json("pages__matcher.json")
  props$games[[2]]$season <- "season_2025"
  mock_transport(page_resp(page_with(compact_props(props))))
  expect_error(.ha_get(paste0(HOST, "/pages/matcher")), class = "fastRhockey_asset_fetch", regexp = "mixes seasons")
})

# ---- the exported wrappers, end to end through the mocked transport -----------

paged <- function(file, ...) {
  modifyList(fx_json(file), list(count = 3L, total_pages = 1L, ...))
}

test_that("every wrapper returns its documented tibble", {
  skip_on_cran()
  tm <- function(file, ...) paged(file, ...)
  cases <- list(
    list("ha_schedule", function() ha_schedule(), page_resp(page_with(compact_props()))),
    list("ha_game", function() ha_game(slug = "20260918-aik-modo"), ok_resp(fx_text("api__game.json"))),
    list("ha_pbp", function() {
      ha_pbp(
        statnet_game_number = 23401, home_statnet_id = "AIK", away_statnet_id = "MODO",
        scheduled_date_time = "2026-09-18T17:00:00.000Z"
      )
    }, ok_resp(fx_text("api__play-by-play.json"))),
    list("ha_games", function() ha_games(season = 2026), ok_resp(tm("api__all-time-games-stats.json"))),
    list("ha_standings", function() ha_standings(season = 2026), ok_resp(fx_text("api__league-standings-all-time.json"))),
    list("ha_skater_stats", function() ha_skater_stats(season = 2026), ok_resp(tm("api__all-time-skaters-stats.json"))),
    list("ha_goalie_stats", function() ha_goalie_stats(season = 2026), ok_resp(tm("api__all-time-goalkeepers-stats.json"))),
    list("ha_team_stats", function() ha_team_stats(season = 2026), ok_resp(fx_text("api__all-time-teams-stats.json"))),
    list("ha_teams", function() ha_teams(season = 2026), ok_resp(tm("api__all-time-goalkeepers-stats.json"))),
    list("ha_leaders", function() ha_leaders(metric = "TP"), ok_resp(paged("api__player-leaderboard.json", totalCount = 3L, totalPages = 1L))),
    list("ha_players", function() ha_players(team = "AIK"), ok_resp(
      modifyList(fx_json("api__all-players.json"), list(pagination = list(page = 1L, pageSize = 100L, pageCount = 1L, total = 3L)))
    ))
  )
  expect_length(cases, 11L)
  for (case in cases) {
    mock_transport(case[[3]])
    short <- sub("^ha_", "", case[[1]])
    tbl <- case[[2]]()
    expect_schema(tbl, short)
    expect_s3_class(tbl, "fastRhockey_data")
    expect_gt(nrow(tbl), 0L)
  }
})

test_that("return_parsed = FALSE returns the raw list, stamped where the body does not say", {
  skip_on_cran()
  mock_transport(ok_resp(paged("api__all-time-skaters-stats.json")))
  raw <- ha_skater_stats(season = 2026, return_parsed = FALSE)
  expect_type(raw, "list")
  expect_identical(raw[["_request"]], list(season = 2026L))
  expect_true(length(raw$results) > 0L)
  mock_transport(ok_resp(fx_text("api__game.json")))
  expect_false("_request" %in% names(ha_game(slug = "20260918-aik-modo", return_parsed = FALSE)))
})

test_that("wrapper arguments become the body fields, season END year to START year", {
  skip_on_cran()
  st <- mock_transport(ok_resp(paged("api__all-time-skaters-stats.json")))
  ha_skater_stats(
    season = 2026, phase = "HAPlayoff", category = "goals", position = "back", ages = "rookie",
    team_side = "home", team = "AIK", against_team = "MIK", all_results = "W",
    game_status = "played", decided_in = "ot"
  )
  b <- sent_body(st)
  expect_identical(b$season, "2025")
  expect_identical(b$categories, "goals")
  expect_identical(b[c("phase", "position", "ages", "team_side", "team", "against_team", "all_results", "game_status", "decided_in")],
                   list(phase = "HAPlayoff", position = "back", ages = "rookie", team_side = "home", team = "AIK",
                        against_team = "MIK", all_results = "W", game_status = "played", decided_in = "ot"))
  expect_identical(st$waited, paste0(HOST, "/api/all-time-skaters-stats"))
})

test_that("an omitted season is the season in progress", {
  skip_on_cran()
  st <- mock_transport(ok_resp(fx_text("api__league-standings-all-time.json")))
  raw <- ha_standings(return_parsed = FALSE)
  end <- .ha_current_season()
  expect_identical(sent_body(st)$season, as.character(end - 1L))
  expect_identical(raw[["_request"]]$season, end)
})

test_that("leaders and the directory send their optional fields only when given", {
  skip_on_cran()
  st <- mock_transport(ok_resp(paged("api__player-leaderboard.json", totalCount = 3L, totalPages = 1L)))
  ha_leaders(metric = "TP", team = "AIK", is_goalkeeper = TRUE)
  expect_identical(sent_body(st)$team, "AIK")
  expect_identical(sent_body(st)$isGoalkeeper, TRUE)
  pg <- list(page = 1L, pageSize = 100L, pageCount = 1L, total = 3L)
  st <- mock_transport(ok_resp(modifyList(fx_json("api__all-players.json"), list(pagination = pg))))
  ha_players(team = "AIK", order_by = "firstName", search_query = "Jo")
  expect_identical(
    sent_body(st)[c("teamShortName", "orderBy", "searchQuery")],
    list(teamShortName = "AIK", orderBy = "firstName", searchQuery = "Jo")
  )
})

test_that("an argument outside its set is a bad request, before any request", {
  skip_on_cran()
  st <- mock_transport(fake_resp("{}"))
  expect_error(ha_games(phase = "SHL"), class = "fastRhockey_bad_request", regexp = "phase")
  expect_error(ha_games(team_side = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_games(all_results = "w"), class = "fastRhockey_bad_request")
  expect_error(ha_games(game_status = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_games(decided_in = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_standings(location = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_skater_stats(category = "results"), class = "fastRhockey_bad_request", regexp = "category")
  expect_error(ha_goalie_stats(category = "points"), class = "fastRhockey_bad_request")
  expect_error(ha_team_stats(category = "summary"), class = "fastRhockey_bad_request")
  expect_error(ha_skater_stats(position = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_skater_stats(ages = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_leaders(metric = "TP", phase = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_players(team = "AIK", order_by = "x"), class = "fastRhockey_bad_request")
  expect_error(ha_leaders(metric = ""), class = "fastRhockey_bad_request", regexp = "metric")
  expect_error(ha_players(team = ""), class = "fastRhockey_bad_request", regexp = "team")
  expect_error(ha_players(team = NULL), class = "fastRhockey_bad_request")
  expect_length(st$reqs, 0L)
})

test_that("required arguments have no default", {
  skip_on_cran()
  for (f in list(ha_game, ha_pbp, ha_leaders, ha_players)) {
    expect_error(f(), "argument")
  }
  expect_identical(names(formals(ha_pbp))[1:5], c(
    "statnet_game_number", "home_statnet_id", "away_statnet_id", "scheduled_date_time", "played_date_time"
  ))
})

test_that("the wrappers keep sdv-py's names and signatures", {
  skip_on_cran()
  sigs <- list(
    ha_schedule = character(), ha_game = "slug",
    ha_pbp = c("statnet_game_number", "home_statnet_id", "away_statnet_id", "scheduled_date_time", "played_date_time"),
    ha_games = c("season", "phase", "team_side", "team", "against_team", "all_results", "game_status", "decided_in"),
    ha_standings = c("season", "phase", "location"),
    ha_skater_stats = c("season", "phase", "category", "position", "ages", "team_side", "team", "against_team", "all_results", "game_status", "decided_in"),
    ha_goalie_stats = c("season", "phase", "category", "team_side", "team", "against_team", "all_results", "game_status", "decided_in"),
    ha_team_stats = c("season", "phase", "category", "team_side", "team", "against_team", "all_results", "game_status", "decided_in"),
    ha_teams = c("season", "phase"),
    ha_leaders = c("metric", "phase", "team", "is_goalkeeper"),
    ha_players = c("team", "order_by", "search_query")
  )
  for (nm in names(sigs)) {
    f <- get(nm)
    expect_identical(names(formals(f)), c(sigs[[nm]], "return_parsed"), info = nm)
  }
  expect_identical(formals(ha_games)$phase, "HA")
  expect_null(formals(ha_games)$season)
  expect_identical(formals(ha_skater_stats)$category, "summary")
  expect_identical(formals(ha_team_stats)$category, "results")
  expect_identical(formals(ha_players)$order_by, "familyName")
  expect_identical(formals(ha_standings)$location, "all")
})

# ---- live (one request per route, behind skip_euro_test()) --------------------

test_that("live: the schedule page, a game and the games table", {
  skip_on_cran()
  skip_euro_test()
  skip_if_offline("hockeyallsvenskan.se")
  sched <- ha_schedule()
  expect_schema(sched, "schedule")
  expect_gt(nrow(sched), 0L)
  g <- ha_game(slug = sched$slug[1])
  expect_schema(g, "game")
  expect_identical(g$slug, sched$slug[1])
  games <- ha_games(season = .ha_current_season())
  expect_schema(games, "games")
})

test_that("live: play-by-play and the leaderboard", {
  skip_on_cran()
  skip_euro_test()
  skip_if_offline("hockeyallsvenskan.se")
  pbp <- ha_pbp(
    statnet_game_number = 23401, home_statnet_id = "AIK", away_statnet_id = "MODO",
    scheduled_date_time = "2026-09-18T17:00:00.000Z"
  )
  expect_schema(pbp, "pbp")
  expect_identical(unique(pbp$statnet_game_number), 23401L)
  leaders <- ha_leaders(metric = "TP")
  expect_schema(leaders, "leaders")
  expect_equal(nrow(leaders), 25L)
})

test_that("live: standings, the crosswalk and the directory", {
  skip_on_cran()
  skip_euro_test()
  skip_if_offline("hockeyallsvenskan.se")
  expect_schema(ha_standings(season = 2026), "standings")
  teams <- ha_teams(season = 2026)
  expect_schema(teams, "teams")
  expect_true(all(c("team_statnet_id", "team_code") %in% names(teams)))
  expect_schema(ha_players(team = "AIK"), "players")
})
