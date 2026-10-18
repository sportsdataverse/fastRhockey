# Offline tests for the VHL (Russia) family in R/vhl.R: the HTML parsers, the getter and
# the 13 wrappers. Mirrors sdv-py tests/hockey/vhl/test_vhl.py.
#
# Fixtures (tests/testthat/fixtures/vhl/) are copied from sdv-py tests/fixtures/vhl/ (cut-down
# real pages, the full report page, the season-table select and the refs capture records).
# parity/ holds sdv-py's frames on the SAME fixtures (one CSV per route + columns.json: the
# column order and dtypes), written with sdv-py origin/main 79dd939371; see parity/README.md.
# The getter is mocked at `.vhl_fetch` (HTML text per URL); `.vhl_fetch` itself is driven
# with httr2 mocked responses. No network.

fx_dir <- testthat::test_path("fixtures", "vhl")
fx_html <- function(name) {
  paste(readLines(file.path(fx_dir, name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
fx_capture <- function(name) {
  jsonlite::fromJSON(file.path(fx_dir, "captures", name), simplifyVector = FALSE)
}
REPORT <- "full/report_899223.html"
HOST <- "https://www.vhlru.ru"
CAL <- paste0(HOST, "/en/calendar/1374/0/")
REP <- paste0(HOST, "/en/report/1374/?idgame=899223")
REQ <- list(season = 2026L, stage = "regular", tn = 1374L)
GREQ <- c(REQ, list(game_id = 899223L))

record <- function(kind, page, req) {
  r <- .vhl_page_record(kind, fx_html(page))
  r[["_request"]] <- req
  r
}
# route -> list(parser, input) on the same inputs the parity CSVs were written from
parity_jobs <- function() {
  list(
    schedule = list(.vhl_parse_schedule, record("calendar", "calendar_1374.html", REQ)),
    game_info = list(.vhl_parse_game_info, record("report", REPORT, GREQ)),
    goals = list(.vhl_parse_goals, record("report", REPORT, GREQ)),
    penalties = list(.vhl_parse_penalties, record("report", REPORT, GREQ)),
    player_box = list(.vhl_parse_player_box, record("report", REPORT, GREQ)),
    goalie_box = list(.vhl_parse_goalie_box, record("report", REPORT, GREQ)),
    pbp = list(.vhl_parse_pbp, record("online", "online_899223.html", GREQ)),
    standings = list(.vhl_parse_standings, record("standings", "standings_1374.html", REQ)),
    skater_stats = list(.vhl_parse_skater_stats, record("stats_players", "stats_players_1374.html", REQ)),
    goalie_stats = list(.vhl_parse_goalie_stats, record("stats_players", "stats_players_1374.html", REQ)),
    player = list(.vhl_parse_player, record("player", "player_34418.html", list(player_id = 34418L))),
    player_career = list(.vhl_parse_player_career, record("player", "player_34418.html", list(player_id = 34418L))),
    roster = list(.vhl_parse_roster, record("team", "team_460279.html", list(team_page_id = 460279L))),
    player_career_goalie = list(.vhl_parse_player_career, fx_capture("en__players__player_id@40370.json"))
  )
}
sdv_cols <- function() jsonlite::fromJSON(file.path(fx_dir, "parity", "columns.json"), simplifyVector = FALSE)
sdv_csv <- function(name) {
  utils::read.csv(file.path(fx_dir, "parity", paste0(name, ".csv")), na.strings = "",
                  colClasses = "character", fileEncoding = "UTF-8", check.names = FALSE)
}
r_class <- function(col, name = "") {
  if (is.list(col)) {
    if (identical(name, "period_scores")) "list_character" else "list_integer"
  } else if (is.integer(col)) "integer" else if (is.double(col)) "double" else if (is.character(col)) "character" else class(col)[[1]]
}
# Cells where the R tibble and sdv-py's CSV disagree. An NA on one side only COUNTS as a mismatch.
n_mismatch <- function(df, csv, types) {
  stopifnot(identical(names(df), names(csv)), nrow(df) == nrow(csv))
  bad <- 0L
  for (nm in names(df)) {
    v <- df[[nm]]
    got <- if (is.list(v)) vapply(v, function(x) paste(x, collapse = " "), "") else as.character(v)
    if (is.list(v)) got[got == ""] <- NA
    want <- csv[[nm]]
    if (identical(types[[nm]], "double")) {
      a <- as.numeric(got)
      b <- as.numeric(want)
      ok <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-9)
    } else {
      ok <- (is.na(got) & is.na(want)) | (!is.na(got) & !is.na(want) & got == want)
    }
    bad <- bad + sum(!ok)
  }
  bad
}

# Mock the getter's fetch: `pages` maps a full URL (with its query) to a fixture file name,
# a list(html = ), a list(error = <function>) or an error condition; an unmapped URL is a 404.
mock_site <- function(pages = list(), env = parent.frame()) {
  .vhl_cache_clear()
  withr::defer(.vhl_cache_clear(), envir = env)
  site <- new.env()
  site$calls <- character()
  site$pages <- c(list(), pages)
  site$pages[[.VHL_CALENDAR_ROOT]] <- site$pages[[.VHL_CALENDAR_ROOT]] %||% "tournaments.html"
  testthat::local_mocked_bindings(
    .vhl_fetch = function(url, params = NULL) {
      params <- Filter(Negate(is.null), as.list(params))
      full <- if (length(params)) paste0(url, "?", paste(names(params), unlist(params), sep = "=", collapse = "&")) else url
      site$calls <- c(site$calls, full)
      p <- site$pages[[full]]
      if (is.null(p)) .abort_no_data("{full} answered HTTP 404.")
      if (is.character(p)) return(fx_html(p))
      if (inherits(p, "condition")) stop(p)
      p$html
    },
    .env = env
  )
  site
}

# ---- parity with sdv-py on the same fixtures ---------------------------------------------------

test_that("every route matches sdv-py's columns, order and dtypes", {
  skip_on_cran()
  cols <- sdv_cols()
  jobs <- parity_jobs()
  expect_setequal(names(jobs), names(cols))
  for (nm in names(jobs)) {
    df <- jobs[[nm]][[1]](jobs[[nm]][[2]])
    expect_s3_class(df, "tbl_df")
    expect_identical(names(df), names(cols[[nm]]), label = nm)
    expect_identical(unname(mapply(r_class, df, names(df))), unname(unlist(cols[[nm]])), label = nm)
    expect_true(all(grepl("^[a-z0-9]+(_[a-z0-9]+)*$", names(df))), label = nm)
  }
})

test_that("every route matches sdv-py's frame cell for cell (NA-vs-value is a mismatch)", {
  skip_on_cran()
  cols <- sdv_cols()
  jobs <- parity_jobs()
  for (nm in names(jobs)) {
    df <- jobs[[nm]][[1]](jobs[[nm]][[2]])
    csv <- sdv_csv(nm)
    expect_equal(nrow(df), nrow(csv), label = nm)
    expect_gt(nrow(df), 0L)
    expect_identical(n_mismatch(df, csv, cols[[nm]]), 0L, label = nm)
  }
})

test_that("the parity check would catch a drifted cell and an NA-vs-value", {
  skip_on_cran()
  cols <- sdv_cols()
  df <- .vhl_parse_goals(record("report", REPORT, GREQ))
  csv <- sdv_csv("goals")
  expect_identical(n_mismatch(df, csv, cols$goals), 0L)
  drifted <- csv
  drifted$scorer[[2]] <- "Somebody Else"
  expect_identical(n_mismatch(df, drifted, cols$goals), 1L)
  na_side <- csv
  na_side$assist2[[1]] <- NA
  expect_identical(n_mismatch(df, na_side, cols$goals), 1L)
  list_drift <- csv
  list_drift$home_on_ice[[1]] <- "1 2 3"
  expect_identical(n_mismatch(df, list_drift, cols$goals), 1L)
})

test_that("ids are integer (never a float or a string) in every frame", {
  skip_on_cran()
  jobs <- parity_jobs()
  for (nm in names(jobs)) {
    df <- jobs[[nm]][[1]](jobs[[nm]][[2]])
    id_cols <- grep("(^|_)id$|^tn$|^calendar_tn$", names(df), value = TRUE)
    expect_true(all(vapply(df[id_cols], is.integer, NA)), label = nm)
    if ("season" %in% names(df)) {
      expect_true(is.integer(df$season) && !anyNA(df$season), label = nm)
    }
  }
})

test_that("HTML text and the parsed record give the same frame", {
  skip_on_cran()
  page <- fx_html(REPORT)
  expect_identical(.vhl_parse_goals(page), .vhl_parse_goals(.vhl_page_record("report", page)))
  expect_identical(.vhl_parse_player_box(page), .vhl_parse_player_box(.vhl_page_record("report", page)))
})

# ---- empty and malformed payloads ------------------------------------------------------------------

parsers <- function() {
  list(schedule = .vhl_parse_schedule, game_info = .vhl_parse_game_info, goals = .vhl_parse_goals,
       penalties = .vhl_parse_penalties, player_box = .vhl_parse_player_box, goalie_box = .vhl_parse_goalie_box,
       pbp = .vhl_parse_pbp, standings = .vhl_parse_standings, skater_stats = .vhl_parse_skater_stats,
       goalie_stats = .vhl_parse_goalie_stats, player = .vhl_parse_player, player_career = .vhl_parse_player_career,
       roster = .vhl_parse_roster)
}

test_that("an empty or malformed payload is a zero-row tibble with the documented columns", {
  skip_on_cran()
  cols <- sdv_cols()
  payloads <- list(list(), NULL, "x", 3, "<html><body><h1>Game summary</h1></body></html>")
  for (nm in names(parsers())) {
    for (p in payloads) {
      df <- parsers()[[nm]](p)
      expect_s3_class(df, "tbl_df")
      expect_identical(nrow(df), 0L)
      expect_identical(names(df), names(cols[[nm]]), label = nm)
      expect_identical(unname(mapply(r_class, df, names(df))), unname(unlist(cols[[nm]])), label = nm)
    }
  }
})

# ---- dtypes and shapes from the full report page ----------------------------------------------------

test_that("rates are double even when the page prints them whole; toi stays mm:ss text", {
  skip_on_cran()
  skaters <- .vhl_parse_player_box(fx_html(REPORT))
  # a game report prints Sft/G as a whole number ('22'); the season tables print '22.8'
  expect_type(skaters$shifts_per_game, "double")
  expect_true(22 %in% skaters$shifts_per_game)
  expect_type(skaters$faceoff_pct, "double")
  expect_type(skaters$shot_pct, "double")
  expect_type(skaters$toi_per_game, "character")
  goalies <- .vhl_parse_goalie_box(fx_html(REPORT))
  expect_type(goalies$save_pct, "double")
  expect_type(goalies$gaa, "double")
  expect_identical(goalies$save_pct, c(100, 60, 93.8, NA))  # '-' for the goalie who did not play
})

test_that("the full report is complete: 34 skaters, 4 goalies (one dressed, no play)", {
  skip_on_cran()
  expect_identical(nrow(.vhl_parse_player_box(fx_html(REPORT))), 34L)
  expect_identical(.vhl_parse_goalie_box(fx_html(REPORT))$games_played, c(1L, 1L, 1L, 0L))
})

test_that("goals carry on-ice jerseys as integer lists and the scoring side", {
  skip_on_cran()
  goals <- .vhl_parse_goals(fx_html(REPORT))
  expect_type(goals$home_on_ice, "list")
  expect_true(all(vapply(goals$home_on_ice, is.integer, NA)))
  expect_identical(goals$team_side, c("away", "away", "home", "home", "away"))
  expect_identical(goals$team_page_id, c(505612L, 505612L, 390L, 390L, 505612L))
  shootout <- goals[nrow(goals), ]  # the winner: the goalie alone on ice, period 'RB' (Cyrillic)
  expect_identical(shootout$period, "РБ")
  expect_length(shootout$away_on_ice[[1]], 0L)
  expect_length(shootout$home_on_ice[[1]], 1L)
})

test_that("report frames share the game key from the request stamp", {
  skip_on_cran()
  rec <- record("report", REPORT, GREQ)
  for (f in list(.vhl_parse_game_info, .vhl_parse_goals, .vhl_parse_penalties, .vhl_parse_player_box)) {
    key <- unique(as.data.frame(f(rec))[c("game_id", "season", "stage", "game_number")])
    expect_identical(nrow(key), 1L)
    expect_identical(as.list(key), list(game_id = 899223L, season = 2026L, stage = "regular", game_number = 984L))
  }
})

test_that("bare report HTML has no game id but keeps its season", {
  skip_on_cran()
  info <- .vhl_parse_game_info(fx_html(REPORT))
  expect_identical(info$game_id, NA_integer_)
  expect_identical(info$season, 2026L)
})

test_that("club and team page ids are different key spaces that bridge only through game_id", {
  skip_on_cran()
  sched <- .vhl_parse_schedule(record("calendar", "calendar_1374.html", REQ))
  info <- .vhl_parse_game_info(record("report", REPORT, GREQ))
  expect_identical(typeof(sched$game_id), typeof(info$game_id))
  row <- merge(sched, info[c("game_id", "home_team_page_id", "away_team_page_id")], by = "game_id")
  # Toros: club id 192 (logo) and team page id 390 are different spaces of the same team
  expect_identical(c(row$home_club_id, row$home_team_page_id), c(192L, 390L))
  expect_identical(c(row$away_club_id, row$away_team_page_id), c(892L, 505612L))
})

test_that("schedule dates take the season's year (August-December = START year)", {
  skip_on_cran()
  sched <- .vhl_parse_schedule(record("calendar", "calendar_1374.html", REQ))
  expect_identical(unique(sched$game_date), "2026-03-18")
  expect_identical(.vhl_iso_day("20 October", 2026L), "2025-10-20")
  expect_identical(.vhl_iso_day("20 March", 2026L), "2026-03-20")
  expect_identical(.vhl_iso_day("garbage", 2026L), NA_character_)
})

test_that("career labels become END year and stage; goalie and skater share columns", {
  skip_on_cran()
  career <- .vhl_parse_player_career(fx_html("player_34418.html"))
  expect_identical(career$season, c(2027L, 2026L))
  expect_identical(career$stage, c("regular", "playoff"))
  expect_identical(career$tn, c(1430L, 1375L))
  expect_identical(career$team_season_id, c(15524L, 15429L))
  goalie <- .vhl_parse_player_career(fx_capture("en__players__player_id@40370.json"))
  expect_identical(names(goalie), names(career))
  expect_lt(sum(is.na(goalie$save_pct)), nrow(goalie))
})

test_that("standings and stats rows carry their ids and position groups", {
  skip_on_cran()
  st <- .vhl_parse_standings(fx_html("standings_1374.html"))
  expect_true(is.integer(st$team_season_id) && !anyNA(st$team_season_id))
  expect_true(is.integer(st$goals_for))
  expect_identical(st$rank, 1:3)
  sk <- .vhl_parse_skater_stats(fx_html("stats_players_1374.html"))
  expect_setequal(unique(sk$position_group), c("defense", "forward"))
  expect_type(.vhl_parse_goalie_stats(fx_html("stats_players_1374.html"))$save_pct, "double")
})

test_that("the broadcast is Russian with integer player-id lists", {
  skip_on_cran()
  pbp <- .vhl_parse_pbp(fx_html("online_899223.html"))
  expect_type(pbp$player_ids, "list")
  expect_true(all(vapply(pbp$player_ids, is.integer, NA)))
  expect_identical(pbp$event_index, seq_len(nrow(pbp)) - 1L)
  expect_true(all(c("goal", "penalty", "shootout_attempt") %in% pbp$kind))
  expect_identical(pbp$home_team[[1]], "Торос")
})

test_that("rosters are Cyrillic and the fixture birth dates are masked", {
  skip_on_cran()
  roster <- .vhl_parse_roster(fx_html("team_460279.html"))
  expect_identical(unique(roster$born), "01.01.2000")
  expect_true(all(grepl("[А-я]", roster$player)))
  expect_identical(.vhl_parse_player(fx_html("player_34418.html"))$born, "01.01.2000")
})

test_that("the tournament table lists every season stage (the KHL even/odd rule does not hold)", {
  skip_on_cran()
  rows <- .vhl_tournament_table(fx_html("tournaments.html"))
  expect_length(rows, 33L)
  tn <- stats::setNames(vapply(rows, function(r) r$tn, 1L), vapply(rows, function(r) paste(r$season, r$stage), ""))
  expect_identical(c(tn[["2011 regular"]], tn[["2011 playoff"]], tn[["2027 regular"]]), c(189L, 190L, 1430L))
  expect_identical(c(tn[["2020 regular"]], tn[["2020 playoff"]]), c(872L, 875L))
  expect_identical(c(tn[["2018 regular"]], tn[["2018 playoff"]]), c(492L, 496L))
})

# ---- the HTML layer's refusals ------------------------------------------------------------------

table_of <- function(html) rvest::html_elements(.vhl_html(html), "table")[[1]]

test_that("the table reader refuses header drift, short rows and zero rows", {
  skip_on_cran()
  good <- "<table><thead><tr><th>Time</th><th>Player</th><th></th><th>Penalty</th></tr></thead><tbody>%s</tbody></table>"
  row <- "<tr><td>01:00</td><td>1. A B</td><td>2</td><td>hooking</td></tr>"
  rows <- .vhl_rows(table_of(sprintf(good, row)), .VHL_REPORT_PENALTIES, "p")
  expect_identical(rows[[1]]$minutes, 2L)
  expect_error(
    .vhl_rows(table_of(sprintf(good, "<tr><td>01:00</td><td>x</td><td>2</td></tr>")), .VHL_REPORT_PENALTIES, "p"),
    "spans", class = "vhl_not_parsed"
  )
  expect_error(.vhl_rows(table_of(sprintf(good, "")), .VHL_REPORT_PENALTIES, "p"), "zero rows", class = "vhl_not_parsed")
  expect_error(
    .vhl_rows(table_of(sub("Penalty", "Infraction", sprintf(good, row))), .VHL_REPORT_PENALTIES, "p"),
    "headers", class = "vhl_not_parsed"
  )
  expect_error(.vhl_rows(NULL, .VHL_REPORT_PENALTIES, "p"), "table missing", class = "vhl_not_parsed")
})

test_that("display:none cells are dropped from the header and the body", {
  skip_on_cran()
  html <- "<table><tr><th>#</th><th style='display: none'>x</th></tr><tr><td>7</td><td style='display:none'>9</td></tr></table>"
  parts <- .vhl_table_parts(table_of(html))
  expect_identical(vapply(parts$head, function(c) c$text, ""), "#")
  expect_identical(vapply(parts$body[[1]], function(c) c$text, ""), "7")
})

test_that("a header colspan covers several body columns", {
  skip_on_cran()
  html <- "<table><thead><tr><th colspan='2'>Name</th><th>#</th></tr></thead><tbody><tr><td>A</td><td>B</td><td>9</td></tr></tbody></table>"
  spec <- list(.vs("Name", "name", "S"), .vs("#", "jersey", "N"))
  expect_identical(.vhl_rows(table_of(html), spec, "t")[[1]], list(name = "A", jersey = 9L))
})

test_that("page record identity checks are type-strict", {
  skip_on_cran()
  expect_error(.vhl_page_record("calendar", fx_html("calendar_1374.html"), list(tn = 1430L)),
               "tournament", class = "vhl_not_parsed")
  expect_error(.vhl_page_record("calendar", fx_html("calendar_1374.html"), list(tn = 1374)),
               "tournament", class = "vhl_not_parsed")  # double 1374 is not integer 1374
  want <- list(season = "2025/2026", stage = "Regular Season", game_number = 985L)
  expect_error(.vhl_page_record("report", fx_html(REPORT), want), "game number", class = "vhl_not_parsed")
  want$game_number <- 984L
  expect_type(.vhl_page_record("report", fx_html(REPORT), want), "list")
  expect_error(.vhl_page_record("report", fx_html(REPORT), list(season = 2026L, stage = "Regular Season", game_number = 984L)),
               "season", class = "vhl_not_parsed")
  expect_error(.vhl_page_record("player", fx_html("player_34418.html"), list(player_id = 34526L)),
               "player", class = "vhl_not_parsed")
  shell <- expect_error(.vhl_page_record("report", "<html><body><h1>Game summary</h1></body></html>"),
                        class = "vhl_not_parsed")
  expect_true(shell$empty)
  not_empty <- expect_error(.vhl_page_record("standings", "<html><body><table></table></body></html>"),
                            class = "vhl_not_parsed")
  expect_false(isTRUE(not_empty$empty))
})

# ---- the getter through a mocked fetch -----------------------------------------------------------------

test_that("season maps to tn once and the path segment is rewritten", {
  skip_on_cran()
  site <- mock_site(setNames(list("calendar_1374.html"), CAL))
  rec <- .vhl_get(paste0(HOST, "/en/calendar/2026/0/"), list(stage = "regular"))
  expect_identical(rec[["_request"]], list(season = 2026L, stage = "regular", tn = 1374L))
  expect_identical(site$calls, c(.VHL_CALENDAR_ROOT, CAL))
  expect_identical(vhl_tn(2026, "playoff"), 1375L)
  expect_identical(vhl_tn(2011), 189L)
  expect_identical(sum(site$calls == .VHL_CALENDAR_ROOT), 1L)  # cached
})

test_that("vhl_tn refuses unknown seasons and stages and bad input", {
  skip_on_cran()
  mock_site()
  expect_error(vhl_tn(2027, "playoff"), "no playoff tournament for season 2027", class = "fastRhockey_no_data")
  expect_error(vhl_tn(2026, "playoffs"), "stage", class = "fastRhockey_bad_request")
  expect_error(vhl_tn(TRUE), "season", class = "fastRhockey_bad_request")
  expect_error(vhl_tn("abc"), "season", class = "fastRhockey_bad_request")
  expect_error(vhl_tn(c(2026, 2025)), class = "fastRhockey_bad_request")
  expect_identical(vhl_tn("2026"), 1374L)
  expect_identical(vhl_tn(2026L), 1374L)
})

test_that("a calendar page without the tournament select is refused", {
  skip_on_cran()
  mock_site(setNames(list(list(html = "<html><body>nothing</body></html>")), .VHL_CALENDAR_ROOT))
  expect_error(vhl_tn(2026), "no tournament select", class = "fastRhockey_asset_fetch")
})

test_that("a report is checked against the calendar and stamped with its game", {
  skip_on_cran()
  site <- mock_site(setNames(list("calendar_1374.html", REPORT), c(CAL, REP)))
  rec <- .vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = 899223, stage = "regular"))
  expect_identical(rec[["_request"]], list(season = 2026L, stage = "regular", tn = 1374L, game_id = 899223L))
  expect_identical(unique(.vhl_parse_goals(rec)$game_id), 899223L)
  .vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = "899223"))  # a second report: cached calendar
  expect_identical(sum(site$calls == CAL), 1L)
})

test_that("a report of another game number is refused", {
  skip_on_cran()
  # the calendar says 899225 is game 986; the page served is game 984
  mock_site(setNames(list("calendar_1374.html", REPORT), c(CAL, paste0(HOST, "/en/report/1374/?idgame=899225"))))
  expect_error(.vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = 899225)),
               "game number 984 != 986", class = "fastRhockey_asset_fetch")
})

test_that("a game outside the calendar is no_data without fetching the report", {
  skip_on_cran()
  site <- mock_site(setNames(list("calendar_1374.html"), CAL))
  expect_error(.vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = 1)), "not in the calendar",
               class = "fastRhockey_no_data")
  expect_false(any(grepl("/report/", site$calls)))
})

test_that("the empty report shell is no_data", {
  skip_on_cran()
  shell <- "<html><head><title>Supreme Hockey League - Game summary</title></head><body><h1>Game summary</h1></body></html>"
  mock_site(setNames(list("calendar_1374.html", list(html = shell)), c(CAL, REP)))
  expect_error(.vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = 899223)), "match-card",
               class = "fastRhockey_no_data")
})

test_that("an unknown tn that serves the current season is refused", {
  skip_on_cran()
  # season 2027 maps to tn 1430; the site answers with another season's calendar
  mock_site(setNames(list("calendar_1374.html"), paste0(HOST, "/en/calendar/1430/0/")))
  expect_error(.vhl_get(paste0(HOST, "/en/calendar/2027/0/"), list()), "tournament 1374, asked 1430",
               class = "fastRhockey_asset_fetch")
})

test_that("standings read the league route, check the tn, and are regular season only", {
  skip_on_cran()
  mock_site(setNames(list("standings_1374.html", "standings_1374.html"),
                     paste0(HOST, c("/en/standings/league/1374/", "/en/standings/league/1430/"))))
  expect_gt(length(.vhl_get(paste0(HOST, "/en/standings/league/2026/"))[["rows"]]), 0L)
  expect_error(.vhl_get(paste0(HOST, "/en/standings/league/2027/")), "asked 1430", class = "fastRhockey_asset_fetch")
  expect_error(.vhl_get(paste0(HOST, "/en/standings/league/2026/"), list(stage = "playoff")),
               "regular season only", class = "fastRhockey_bad_request")
})

test_that("the broadcast needs its season and its game number", {
  skip_on_cran()
  online <- "https://online.vhlru.ru/online/%d.html"
  mock_site(setNames(list("calendar_1374.html", "online_899223.html", "online_899223.html"),
                     c(CAL, sprintf(online, 899223L), sprintf(online, 899225L))))
  rec <- .vhl_get(sprintf(online, 899223L), list(season = 2026))
  expect_identical(rec[["_request"]]$game_id, 899223L)
  expect_identical(.vhl_parse_pbp(rec)$game_id[[1]], 899223L)
  expect_error(.vhl_get(sprintf(online, 899225L), list(season = 2026)), "game number 984 != 986",
               class = "fastRhockey_asset_fetch")
  expect_error(.vhl_get(sprintf(online, 899223L)), "season", class = "fastRhockey_bad_request")
})

test_that("a player page must link its own id", {
  skip_on_cran()
  mock_site(setNames(list("player_34418.html", "player_34418.html"),
                     paste0(HOST, c("/en/players/34418/", "/en/players/34526/"))))
  expect_identical(.vhl_get(paste0(HOST, "/en/players/34418/"))[["_request"]], list(player_id = 34418L))
  expect_error(.vhl_get(paste0(HOST, "/en/players/34526/")), "page is player 34418",
               class = "fastRhockey_asset_fetch")
})

test_that("a team shell is no_data and a team page is stamped with its id", {
  skip_on_cran()
  mock_site(setNames(list("team_460279.html", list(html = "<html><body><h1>Teams</h1></body></html>")),
                     paste0(HOST, c("/en/teams/460279/", "/en/teams/1/"))))
  rec <- .vhl_get(paste0(HOST, "/en/teams/460279/"))
  expect_identical(unique(.vhl_parse_roster(rec)$team_page_id), 460279L)
  expect_error(.vhl_get(paste0(HOST, "/en/teams/1/")), "not a team page", class = "fastRhockey_no_data")
})

test_that("a truncated page is refused", {
  skip_on_cran()
  page <- fx_html(REPORT)
  mock_site(setNames(list("calendar_1374.html", list(html = substr(page, 1L, nchar(page) %/% 2L))), c(CAL, REP)))
  expect_error(.vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = 899223)), "not the page asked for",
               class = "fastRhockey_asset_fetch")
})

test_that("bad arguments are refused before any request", {
  skip_on_cran()
  site <- mock_site()
  expect_error(.vhl_get(paste0(HOST, "/en/report/2026/")), "idgame", class = "fastRhockey_bad_request")
  expect_error(.vhl_get(paste0(HOST, "/en/report/2026/"), list(idgame = "x")), "idgame", class = "fastRhockey_bad_request")
  expect_error(.vhl_get(paste0(HOST, "/en/standings/regular/2026/")), "Not a VHL route", class = "fastRhockey_bad_request")
  expect_error(.vhl_get(paste0(HOST, "/en/players/1/"), list(stage = "all")), "stage", class = "fastRhockey_bad_request")
  expect_length(site$calls, 0L)
})

# ---- .vhl_fetch through mocked responses ----------------------------------------------------------------

html_resp <- function(body = "<html><body>ok</body></html>", status = 200L, url = "https://www.vhlru.ru/en/players/34418/",
                      ctype = "text/html; charset=UTF-8", headers = list()) {
  body <- if (is.character(body)) charToRaw(enc2utf8(body)) else body
  httr2::response(status_code = status, url = url, body = body,
                  headers = c(list(`Content-Type` = ctype), headers))
}
PLAYER <- paste0(HOST, "/en/players/34418/")

fetch_env <- function(env = parent.frame()) {
  waits <- new.env()
  waits$urls <- character()
  waits$slept <- numeric()
  # the shared .polite_perform() asks the host clock for 1 s first, then the retry delay
  testthat::local_mocked_bindings(
    .polite_wait = function(url, min_interval = 1) {
      waits$urls <- c(waits$urls, url)
      waits$slept <- c(waits$slept, min_interval)
    },
    .env = env
  )
  waits
}

test_that(".vhl_fetch refuses every wrong answer under the error vocabulary", {
  skip_on_cran()
  fetch_env()
  cases <- list(
    list(html_resp("nope", 404L), "fastRhockey_no_data", "404"),
    list(html_resp("bad", 400L), "fastRhockey_bad_request", "400"),
    list(html_resp("bad", 422L), "fastRhockey_bad_request", "422"),
    list(html_resp("denied", 403L), "fastRhockey_asset_fetch", "403"),
    list(html_resp("", 204L), "fastRhockey_asset_fetch", "not 200"),
    list(html_resp('{"status": "error"}', ctype = "application/json"), "fastRhockey_asset_fetch", "non-HTML"),
    list(html_resp("   "), "fastRhockey_asset_fetch", "empty body"),
    list(html_resp(iconv("Торос", "UTF-8", "CP1251", toRaw = TRUE)[[1]]),
         "fastRhockey_asset_fetch", "not valid"),
    list(html_resp("x", ctype = "text/html; charset=nonsense"), "fastRhockey_asset_fetch", "not valid"),
    list(html_resp("ok �"), "fastRhockey_asset_fetch", "replacement"),
    list(html_resp("<html></html>", url = paste0(HOST, "/404.php")), "fastRhockey_no_data", "redirected"),
    list(html_resp("<html></html>", url = paste0(HOST, "/en/")), "fastRhockey_asset_fetch", "redirected")
  )
  for (cs in cases) {
    resp <- cs[[1]]
    httr2::local_mocked_responses(function(req) resp)
    expect_error(.vhl_fetch(PLAYER), cs[[3]], class = cs[[2]])
  }
})

test_that(".vhl_fetch refuses a broadcast redirected to the online index with no_data", {
  skip_on_cran()
  fetch_env()
  httr2::local_mocked_responses(function(req) {
    html_resp("<html></html>", url = "https://online.vhlru.ru/", headers = list())
  })
  expect_error(.vhl_fetch("https://online.vhlru.ru/online/899223.html"), "redirected", class = "fastRhockey_no_data")
})

test_that(".vhl_fetch returns the decoded text of a clean page (Cyrillic intact)", {
  skip_on_cran()
  fetch_env()
  httr2::local_mocked_responses(function(req) html_resp("<html><body>Торос</body></html>", url = req$url))
  expect_identical(.vhl_fetch(PLAYER), "<html><body>Торос</body></html>")
  # a declared windows-1251 page is decoded in that charset
  httr2::local_mocked_responses(function(req) {
    html_resp(iconv("<html>Торос</html>", "UTF-8", "CP1251", toRaw = TRUE)[[1]],
              url = req$url, ctype = "text/html; charset=windows-1251")
  })
  expect_identical(.vhl_fetch(PLAYER), "<html>Торос</html>")
})

test_that(".vhl_fetch pins only www.vhlru.ru to the working IP and sends the query", {
  skip_on_cran()
  fetch_env()
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- req
    html_resp(url = req$url)
  })
  .vhl_fetch(paste0(HOST, "/en/report/1374/"), list(idgame = 899223L))
  .vhl_fetch("https://online.vhlru.ru/online/1.html")
  expect_identical(seen[[1]]$options$resolve, "www.vhlru.ru:443:188.128.49.29")
  expect_identical(seen[[1]]$url, paste0(HOST, "/en/report/1374/?idgame=899223"))
  expect_null(seen[[2]]$options$resolve)
  expect_true("resolve" %in% names(curl::curl_options()))  # CURLOPT_RESOLVE exists in this curl
})

test_that(".vhl_fetch retries 5xx and transport errors, waiting the host slot on every attempt", {
  skip_on_cran()
  w <- fetch_env()
  good <- html_resp("<html><body>ok</body></html>")
  httr2::local_mocked_responses(list(html_resp("busy", 503L), html_resp("busy", 500L), good))
  got <- .vhl_fetch(PLAYER)
  expect_match(got, "ok")
  expect_length(w$urls, 3L)
  expect_identical(w$slept, c(1, 2, 4))  # first slot, then 2 s and 4 s
  # three dead connections: asset_fetch after 3 attempts
  w$urls <- character()
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    rlang::abort("connection reset", class = "httr2_failure")
  })
  expect_error(.vhl_fetch(PLAYER), "after 3 attempts", class = "fastRhockey_asset_fetch")
  expect_identical(calls, 3L)
  expect_length(w$urls, 3L)
  # a persistent 502 is asset_fetch carrying the status
  httr2::local_mocked_responses(function(req) html_resp("bad gateway", 502L))
  expect_error(.vhl_fetch(PLAYER), "502", class = "fastRhockey_asset_fetch")
})

test_that(".vhl_fetch honours Retry-After (seconds, capped at 120 s) through the shared retry", {
  skip_on_cran()
  w <- fetch_env()
  good <- html_resp()
  for (status in c(429L, 503L)) {
    httr2::local_mocked_responses(list(html_resp("busy", status, headers = list(`Retry-After` = "30")), good))
    w$slept <- numeric()
    .vhl_fetch(PLAYER)
    expect_identical(w$slept, c(1, 30))  # Retry-After over the 2 s backoff
    httr2::local_mocked_responses(list(html_resp("busy", status, headers = list(`Retry-After` = "86400")), good))
    w$slept <- numeric()
    .vhl_fetch(PLAYER)
    expect_identical(w$slept, c(1, 120))
  }
  # no Retry-After: the 2 s backoff
  httr2::local_mocked_responses(list(html_resp("busy", 500L), good))
  w$slept <- numeric()
  .vhl_fetch(PLAYER)
  expect_identical(w$slept, c(1, 2))
})

# ---- the exported wrappers ------------------------------------------------------------------------------

all_pages <- function() {
  online <- "https://online.vhlru.ru/online/899223.html"
  setNames(
    list("calendar_1374.html", REPORT, "online_899223.html", "standings_1374.html", "stats_players_1374.html",
         "player_34418.html", "team_460279.html"),
    c(CAL, REP, online, paste0(HOST, "/en/standings/league/1374/"), paste0(HOST, "/en/stats/players/1374/"),
      paste0(HOST, "/en/players/34418/"), paste0(HOST, "/en/teams/460279/"))
  )
}

test_that("each wrapper returns a fastRhockey_data tibble with sdv-py's columns", {
  skip_on_cran()
  mock_site(all_pages())
  cols <- sdv_cols()
  out <- list(
    schedule = vhl_schedule(2026),
    game_info = vhl_game_info(2026, 899223),
    goals = vhl_goals(2026, 899223),
    penalties = vhl_penalties(2026, 899223),
    player_box = vhl_player_box(2026, 899223),
    goalie_box = vhl_goalie_box(2026, 899223),
    pbp = vhl_pbp(899223, 2026),
    standings = vhl_standings(2026),
    skater_stats = vhl_skater_stats(2026),
    goalie_stats = vhl_goalie_stats(2026),
    player = vhl_player(34418),
    player_career = vhl_player_career(34418),
    roster = vhl_roster(460279)
  )
  expect_length(out, 13L)
  for (nm in names(out)) {
    df <- out[[nm]]
    expect_s3_class(df, "fastRhockey_data")
    expect_s3_class(df, "tbl_df")
    expect_identical(names(df), names(cols[[nm]]), label = nm)
    expect_gt(nrow(df), 0L)
    expect_match(attr(df, "fastRhockey_type"), "^VHL .* from vhlru.ru$")
  }
  # the request stamp reaches the frames: ids from the request, not from the page
  expect_identical(unique(out$goals$game_id), 899223L)
  expect_identical(unique(out$pbp$game_id), 899223L)
  expect_identical(unique(out$roster$team_page_id), 460279L)
  expect_identical(unique(out$schedule$tn), 1374L)
  expect_identical(out$player_career$player_id[[1]], 34418L)
})

test_that("return_parsed = FALSE returns the page's record with the request stamped", {
  skip_on_cran()
  mock_site(all_pages())
  rec <- vhl_goals(2026, 899223, return_parsed = FALSE)
  expect_type(rec, "list")
  expect_identical(rec[["_request"]], list(season = 2026L, stage = "regular", tn = 1374L, game_id = 899223L))
  expect_identical(.vhl_parse_goals(rec), .vhl_parse_goals(record("report", REPORT, GREQ)))
})

test_that("wrappers validate their arguments before any request", {
  skip_on_cran()
  site <- mock_site()
  expect_error(vhl_schedule("abc"), "season", class = "fastRhockey_bad_request")
  expect_error(vhl_schedule(2026, stage = "all"), "stage", class = "fastRhockey_bad_request")
  expect_error(vhl_schedule(2026, stage = NULL), "stage", class = "fastRhockey_bad_request")
  expect_error(vhl_goals(2026, game_id = NULL), "idgame", class = "fastRhockey_bad_request")
  expect_error(vhl_pbp(game_id = "x", season = 2026), "game_id", class = "fastRhockey_bad_request")
  expect_error(vhl_player(player_id = -1), "player_id", class = "fastRhockey_bad_request")
  expect_error(vhl_roster(team_page_id = NA), "team_page_id", class = "fastRhockey_bad_request")
  expect_error(vhl_standings(2026, stage = "playoff"), "unused argument")
  expect_length(site$calls, 0L)
})

test_that("playoff stages map to their own tn and the standings refuse them", {
  skip_on_cran()
  site <- mock_site(setNames(list("calendar_1374.html"), paste0(HOST, "/en/calendar/1375/0/")))
  expect_error(vhl_schedule(2026, "playoff"), "tournament 1374, asked 1375", class = "fastRhockey_asset_fetch")
  expect_true(paste0(HOST, "/en/calendar/1375/0/") %in% site$calls)
})

test_that("wrappers scope options and restore them", {
  skip_on_cran()
  mock_site(all_pages())
  old <- options(scipen = 0, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  vhl_player(34418)
  expect_equal(getOption("scipen"), 0)
})

# ---- live (off by default; RUN_EURO_TESTS=true) ------------------------------------------------------------

test_that("live: the season table, a calendar and a report agree with sdv-py", {
  skip_on_cran()
  skip_euro_test()
  .vhl_cache_clear()
  expect_identical(vhl_tn(2026), 1374L)
  expect_identical(vhl_tn(2026, "playoff"), 1375L)
  info <- vhl_game_info(2026, 899223)
  expect_identical(info$game_number, 984L)
  expect_identical(info$home_team_page_id, 390L)
})
