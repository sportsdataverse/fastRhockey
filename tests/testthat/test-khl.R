# Offline tests for the KHL family (R/khl.R): 14 khl.ru REST wrappers + 9 webcaster wrappers.
#
# Fixtures (tests/testthat/fixtures/khl/) are sdv-py's committed real captures
# (tests/fixtures/khl_rest, tests/fixtures/khl_webcaster, refs captures of 2026-10-12;
# every array truncated to 3 rows except full/game__text.json and full/data.json).
# fixtures/khl/parity/<fn>.csv is sdv-py's frame for the SAME fixture (the sdv-py
# parsers on sdv-py main, nulls written as <NA>); parity/_schema.csv is its column list.
# No network: HTTP is mocked with httr2::local_mocked_responses() or the family getters.

khl_fx_path <- function(...) testthat::test_path("fixtures", "khl", ...)
khl_fx <- function(path) jsonlite::fromJSON(khl_fx_path(path), simplifyVector = FALSE)
khl_fx_text <- function(path) paste(readLines(khl_fx_path(path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

KHL_SID <- strrep("0123456789abcdef", 2)
KHL_SID2 <- strrep("fedcba9876543210", 2)
KHL_REST <- "https://www.khl.ru/rest"
KHL_WC <- "https://khl.api.webcaster.pro/api/khl_mobile"
KHL_HEADER_FORM <- list(page = "resume", `values[gameid]` = 898236, `values[tournament]` = 1369)

khl_reset <- function() {
  .khl_state$sessions <- NULL
  .khl_state$season_table <- NULL
}

# ---- sdv-py parity -----------------------------------------------------------------

# fn -> (fixture, parser(raw))
khl_jobs <- list(
  khl_schedule = list("rest/calendar__list.json", .khl_parse_schedule),
  khl_roster = list("rest/clubs__team.json", .khl_parse_roster),
  khl_stat_filters = list("rest/filters__statplayers.json", .khl_parse_stat_filters),
  khl_game_header = list("rest/game__header.json", .khl_parse_game_header),
  khl_game_protocol = list("rest/game__protocol.json", .khl_parse_game_protocol),
  khl_game_info = list("rest/full/game__text.json", .khl_parse_game_info),
  khl_player_box = list("rest/full/game__text.json", .khl_parse_player_box),
  khl_pbp = list("rest/full/game__text.json", .khl_parse_pbp),
  khl_shots = list("rest/full/game__text.json", .khl_parse_shots),
  khl_player_profile = list("rest/players__profile.json", .khl_parse_player_profile),
  khl_player_career = list("rest/players__stats.json", .khl_parse_player_career),
  khl_playoff_bracket = list("rest/standings__playoff.json", .khl_parse_playoff_bracket),
  khl_standings = list("rest/standings__regular.json", .khl_parse_standings),
  khl_player_stats = list("rest/stat__players.json", .khl_parse_player_stats),
  khl_webcaster_stages = list("webcaster/full/data.json", .khl_parse_stages),
  khl_webcaster_event = list("webcaster/event_v2.json", function(x) .khl_parse_webcaster(x, "khl_webcaster_event")),
  khl_webcaster_events = list("webcaster/events_v2.json", function(x) .khl_parse_webcaster(x, "khl_webcaster_events")),
  khl_webcaster_leaders = list("webcaster/leaders_v2.json", .khl_parse_leaders),
  khl_webcaster_players = list("webcaster/players_v2.json", function(x) .khl_parse_webcaster(x, "khl_webcaster_players")),
  khl_webcaster_players_light = list("webcaster/players_v2_light.json",
                                     function(x) .khl_parse_webcaster(x, "khl_webcaster_players_light")),
  khl_webcaster_tables = list("webcaster/tables_v2.json", .khl_parse_tables),
  khl_webcaster_team = list("webcaster/team_v2.json", function(x) .khl_parse_webcaster(x, "khl_webcaster_team")),
  khl_webcaster_teams = list("webcaster/teams_v2.json", function(x) .khl_parse_webcaster(x, "khl_webcaster_teams"))
)

khl_parity_schema <- function() {
  utils::read.csv(khl_fx_path("parity", "_schema.csv"), stringsAsFactors = FALSE)
}
khl_parity_frame <- function(fn) {
  utils::read.csv(khl_fx_path("parity", paste0(fn, ".csv")), colClasses = "character", na.strings = "<NA>",
                  check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
}
# TRUE where the R cell and the sdv-py cell disagree; NA-vs-value is a mismatch.
khl_cell_mismatch <- function(r, py, type) {
  differs <- function(a, b) (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
  switch(type,
    integer = differs(as.character(r), py),
    double = {
      x <- as.numeric(py)
      (is.na(r) != is.na(x)) | (!is.na(r) & !is.na(x) & abs(r - x) > 1e-9 * pmax(1, abs(x)))
    },
    logical = differs(ifelse(is.na(r), NA_character_, ifelse(r, "true", "false")), py),
    character = differs(r, py)
  )
}

test_that("every KHL route matches sdv-py on the same fixture (NA-vs-value counts as a mismatch)", {
  skip_on_cran()
  schema <- khl_parity_schema()
  expect_setequal(unique(schema$fn), names(khl_jobs))
  expect_length(khl_jobs, 23)
  for (fn in names(khl_jobs)) {
    got <- khl_jobs[[fn]][[2]](khl_fx(khl_jobs[[fn]][[1]]))
    py <- khl_parity_frame(fn)
    want <- schema[schema$fn == fn, ]
    # the committed sdv-py column list: names, order and every documented column
    expect_identical(names(got), want$col, info = fn)
    expect_true(all(want$documented == 1), info = fn)
    expect_equal(nrow(got), nrow(py), info = fn)
    expect_gt(nrow(got), 0, label = paste(fn, "rows"))
    for (i in seq_len(nrow(want))) {
      cn <- want$col[[i]]
      # Int64 -> integer, Float64 -> double, Utf8 -> character, Boolean -> logical
      r_class <- c(integer = "integer", double = "numeric", character = "character", logical = "logical")[[want$type[[i]]]]
      # an Int64 column is an R integer; one holding values past 2^31 (epoch milliseconds) is a whole double
      is_type <- inherits(got[[cn]], r_class) ||
        (want$type[[i]] == "integer" && is.double(got[[cn]]) && all(is.na(got[[cn]]) | got[[cn]] == trunc(got[[cn]])))
      expect_true(is_type, info = paste(fn, cn, "type"))
      bad <- khl_cell_mismatch(got[[cn]], py[[cn]], want$type[[i]])
      expect_false(any(bad), info = paste0(fn, "$", cn, ": ", sum(bad), " mismatched cells"))
    }
  }
})

test_that("the R schema table is the sdv-py column list, in order", {
  skip_on_cran()
  schema <- khl_parity_schema()
  codes <- c(integer = "i", double = "f", character = "s", logical = "b")
  for (fn in names(khl_jobs)) {
    want <- schema[schema$fn == fn, ]
    expect_identical(names(.KHL_SCHEMAS[[fn]]), want$col, info = fn)
    expect_identical(unname(.KHL_SCHEMAS[[fn]]), unname(codes[want$type]), info = fn)
  }
  expect_length(.KHL_SCHEMAS, 23)
})

test_that("ids are integers, never strings or floats; a text id stays text", {
  skip_on_cran()
  info <- khl_jobs$khl_game_info[[2]](khl_fx("rest/full/game__text.json"))
  expect_identical(c(info$game_id, info$tournament_id), c(898236L, 1369L))
  for (cn in c("home_club_id", "away_club_id", "home_team_season_id", "away_team_season_id", "arena_id",
               "gk_a_player_id", "gk_b_player_id")) {
    expect_true(is.integer(info[[cn]]), info = cn)
  }
  box <- khl_jobs$khl_player_box[[2]](khl_fx("rest/full/game__text.json"))
  expect_true(is.integer(box$player_id) && is.integer(box$jn) && is.integer(box$game_id))
  career <- .khl_parse_player_career(khl_fx("rest/players__stats.json"))
  expect_true(all(vapply(career[c("player_id", "tournament_id", "team_season_id", "club_id")], is.integer, NA)))
  ev <- khl_jobs$khl_webcaster_events[[2]](khl_fx("webcaster/events_v2.json"))
  expect_true(is.integer(ev$id) && is.integer(ev$khl_id) && is.integer(ev$stage_id))
  expect_true(is.character(ev$yandex_id))  # a text id is never cast
  all_ids <- unlist(lapply(names(khl_jobs), function(fn) {
    got <- khl_jobs[[fn]][[2]](khl_fx(khl_jobs[[fn]][[1]]))
    unlist(lapply(got[vapply(got, is.character, NA)], function(x) grep("^[0-9]+\\.0$", x, value = TRUE)))
  }))
  expect_length(all_ids, 0)  # no float-origin "123.0" id strings anywhere
})

test_that("an empty or malformed payload is a zero-row tibble with the documented columns", {
  skip_on_cran()
  schema <- khl_parity_schema()
  payloads <- list(
    list(), setNames(list(), character(0)), NULL, "x",
    list(status = "error", data = NULL, errors = list(list(message = "x"))),
    list(status = "success", data = list())
  )
  # the generic webcaster shaper rows any non-empty object (an error-shaped body is a one-row page)
  generic <- c("khl_webcaster_event", "khl_webcaster_events", "khl_webcaster_players",
               "khl_webcaster_players_light", "khl_webcaster_team", "khl_webcaster_teams")
  for (fn in names(khl_jobs)) {
    for (p in if (fn %in% generic) payloads[1:4] else payloads) {
      got <- khl_jobs[[fn]][[2]](p)
      want <- schema[schema$fn == fn, ]
      expect_s3_class(got, "tbl_df")
      expect_equal(nrow(got), 0, info = fn)
      expect_identical(names(got), want$col, info = fn)
      expect_identical(unname(vapply(got, function(x) class(x)[1], "")),
                       unname(c(integer = "integer", double = "numeric", character = "character",
                                logical = "logical")[want$type]), info = fn)
    }
  }
})

test_that("an empty frame binds with a populated one (same dtypes)", {
  skip_on_cran()
  for (fn in c("khl_schedule", "khl_pbp", "khl_player_box", "khl_roster", "khl_webcaster_event")) {
    full <- khl_jobs[[fn]][[2]](khl_fx(khl_jobs[[fn]][[1]]))
    empty <- khl_jobs[[fn]][[2]](list())
    expect_equal(nrow(dplyr::bind_rows(empty, full)), nrow(full), info = fn)
  }
})

test_that("a sparse body is backfilled with the documented dtypes", {
  skip_on_cran()
  body <- khl_fx("rest/clubs__team.json")
  for (group in names(body$data$players)) {
    for (i in seq_along(body$data$players[[group]])) {
      body$data$players[[group]][[i]]["age"] <- list(NULL)
      body$data$players[[group]][[i]]["favorite"] <- list(NULL)
    }
  }
  sparse <- .khl_parse_roster(body)
  full <- .khl_parse_roster(khl_fx("rest/clubs__team.json"))
  expect_identical(vapply(sparse, function(x) class(x)[1], ""), vapply(full, function(x) class(x)[1], ""))
  expect_true(all(is.na(sparse$age)))
})

test_that("the All-Star games are dropped from the schedule and the id traps stay separate", {
  skip_on_cran()
  body <- khl_fx("rest/calendar__list.json")
  week <- body$data$GAMES[[1]]
  day <- names(week)[[1]]
  star <- modifyList(week[[day]][[1]], list(id = 901626, tnId = 1423))
  body$data$GAMES[[1]][[day]] <- c(week[[day]], list(star))
  got <- .khl_parse_schedule(body)
  expect_false(901626L %in% got$game_id)
  expect_true(all(got$tournament_id == 1369L))
  hdr <- .khl_parse_game_header(khl_fx("rest/game__header.json"))
  expect_false(identical(hdr$home_club_id, hdr$home_team_season_id))
})

test_that("game info resolves the starting goalie jerseys to player ids", {
  skip_on_cran()
  info <- .khl_parse_game_info(khl_fx("rest/full/game__text.json"))
  expect_false(is.na(info$gk_a_jn))
  expect_false(is.na(info$gk_a_player_id))
  expect_false(is.na(info$gk_b_player_id))
})

# ---- khl.ru runtime: session, POST form, refusals ---------------------------------------

khl_home <- function(sid = KHL_SID) {
  function(req) httr2::response(status_code = 200L, url = req$url,
                                headers = list(`Content-Type` = "text/html"),
                                body = charToRaw(sprintf("<html>\"bitrix_sessid\":\"%s\"</html>", sid)))
}
khl_text <- function(text, status = 200L) {
  function(req) httr2::response(status_code = status, url = req$url,
                                headers = list(`Content-Type` = "application/json"),
                                body = charToRaw(text))
}
khl_body <- function(x) khl_text(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)))
khl_file <- function(path) khl_text(khl_fx_text(path))

# Mock the whole transport: GETs pop `homes`, POSTs pop `posts`; no sleeping. Returns the log.
khl_transport <- function(posts = list(), homes = NULL, frame = parent.frame()) {
  log <- new.env()
  log$calls <- list()
  log$posts <- posts
  log$homes <- homes %||% list(khl_home(KHL_SID), khl_home(KHL_SID2), khl_home(KHL_SID))
  log$waits <- character()
  log$sleeps <- numeric()
  testthat::local_mocked_bindings(
    .polite_wait = function(url, min_interval = 1) {
      log$waits <- c(log$waits, url)
      # an interval above the 1 s default is a retry backoff (2 s, 4 s) or a Retry-After
      if (min_interval > 1) log$sleeps <- c(log$sleeps, min_interval)
      invisible(0)
    },
    .env = frame
  )
  httr2::local_mocked_responses(function(req) {
    post <- !is.null(req$body)
    log$calls[[length(log$calls) + 1L]] <- list(
      method = if (post) "POST" else "GET", url = req$url, form = if (post) req$body$data,
      headers = req$headers, follow = req$options$followlocation
    )
    key <- if (post) "posts" else "homes"
    nxt <- log[[key]][[1]]
    log[[key]] <- log[[key]][-1]
    nxt(req)
  }, env = frame)
  log
}
khl_methods <- function(log) vapply(log$calls, function(x) x$method, "")

khl_season_table_mock <- function(frame = parent.frame()) {
  fetched <- new.env()
  fetched$urls <- character()
  testthat::local_mocked_bindings(
    .khl_wc_fetch = function(url, query = list()) {
      fetched$urls <- c(fetched$urls, url)
      khl_fx("webcaster/full/data.json")
    },
    .env = frame
  )
  fetched
}

test_that("POST sends the form with the session and the ajax headers; the session is primed once", {
  skip_on_cran()
  khl_reset()
  log <- khl_transport(list(khl_file("rest/game__header.json"), khl_file("rest/game__header.json")))
  raw <- .khl_get(paste0(KHL_REST, "/game/header/"), c(KHL_HEADER_FORM, list(season = NULL)))
  expect_equal(raw$data$game$id, 898236L)
  post <- log$calls[[2]]
  expect_equal(c(post$method, post$url), c("POST", paste0(KHL_REST, "/game/header/")))
  expect_false(post$follow)
  form <- lapply(post$form, as.character)
  expect_equal(form, list(sessid = KHL_SID, page = "resume", `values[gameid]` = "898236",
                          `values[tournament]` = "1369"))
  expect_equal(post$headers[["X-Requested-With"]], "XMLHttpRequest")
  expect_equal(post$headers[["Origin"]], "https://www.khl.ru")
  expect_equal(post$headers[["Referer"]], "https://www.khl.ru/")
  .khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM, headers = list(`Accept-Language` = "en"))
  expect_equal(khl_methods(log), c("GET", "POST", "POST"))
  expect_equal(log$calls[[3]]$headers[["Accept-Language"]], "en")
  # every attempt waits its host slot first
  expect_equal(log$waits, c("https://www.khl.ru/", rep(paste0(KHL_REST, "/game/header/"), 2)))
})

test_that("season and stage become the tournament id through the season table, fetched once", {
  skip_on_cran()
  khl_reset()
  fetched <- khl_season_table_mock()
  log <- khl_transport(list(khl_file("rest/standings__playoff.json"), khl_file("rest/calendar__list.json")))
  .khl_get(paste0(KHL_REST, "/standings/playoff/"),
           list(`values[type]` = "playoff", stage = "playoff", season = 2026))
  expect_equal(as.character(log$calls[[2]]$form[["values[tournament]"]]), "1370")
  .khl_get(paste0(KHL_REST, "/calendar/list/"), list(season = 2026, stage = "regular"))
  expect_equal(lapply(log$calls[[3]]$form, as.character), list(sessid = KHL_SID, `values[tournament]` = "1369"))
  expect_equal(fetched$urls, paste0(KHL_WC, "/data.json"))
})

test_that("an explicit tournament wins over the season; a game route without one is a bad request", {
  skip_on_cran()
  khl_reset()
  log <- khl_transport(list(khl_file("rest/calendar__list.json")))
  .khl_get(paste0(KHL_REST, "/calendar/list/"), list(season = 1990, `values[tournament]` = 1369))
  expect_equal(as.character(log$calls[[2]]$form[["values[tournament]"]]), "1369")
  khl_reset()
  log <- khl_transport()
  expect_error(.khl_get(paste0(KHL_REST, "/game/text/"), list(`values[gameid]` = 898236)),
               class = "fastRhockey_bad_request")
  expect_length(log$calls, 0)
})

test_that("a 307 re-primes once and retries; a second 307 is a failed fetch", {
  skip_on_cran()
  khl_reset()
  log <- khl_transport(list(khl_text("", 307L), khl_file("rest/game__header.json")))
  .khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM)
  expect_equal(khl_methods(log), c("GET", "POST", "GET", "POST"))
  expect_equal(c(log$calls[[2]]$form$sessid, log$calls[[4]]$form$sessid), c(KHL_SID, KHL_SID2))
  expect_equal(log$waits[[3]], "https://www.khl.ru/")
  khl_reset()
  khl_transport(list(khl_text("", 307L), khl_text("", 307L)))
  expect_error(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM), "307 again",
               class = "fastRhockey_asset_fetch")
})

test_that("an error envelope is a failed fetch", {
  skip_on_cran()
  khl_reset()
  khl_transport(list(khl_body(list(status = "error", data = NULL, errors = list(list(message = "Undefined index: gameid"))))))
  expect_error(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM), "Undefined index",
               class = "fastRhockey_asset_fetch")
})

test_that("a stale sessid re-primes a fresh session and retries once; a second one drops the session", {
  skip_on_cran()
  stale <- list(status = "error", data = NULL, errors = list(list(code = "invalid_csrf", message = "x")))
  khl_reset()
  log <- khl_transport(list(khl_body(stale), khl_file("rest/game__header.json")))
  raw <- .khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM)
  expect_equal(raw$data$game$id, 898236L)
  expect_equal(khl_methods(log), c("GET", "POST", "GET", "POST"))
  expect_equal(c(log$calls[[2]]$form$sessid, log$calls[[4]]$form$sessid), c(KHL_SID, KHL_SID2))
  expect_equal(.khl_state$sessions[["https://www.khl.ru"]]$sessid, KHL_SID2)

  khl_reset()
  log <- khl_transport(list(khl_body(stale), khl_body(stale), khl_file("rest/game__header.json")))
  expect_error(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM), "invalid_csrf",
               class = "fastRhockey_asset_fetch")
  expect_null(.khl_state$sessions[["https://www.khl.ru"]])  # dropped: the next call primes afresh
  .khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM)
  expect_equal(khl_methods(log), c("GET", "POST", "GET", "POST", "GET", "POST"))
})

test_that("a 5xx POST is retried 2 s then 4 s apart, and the retries are bounded", {
  skip_on_cran()
  khl_reset()
  log <- khl_transport(list(khl_text("busy", 503L), khl_text("bad", 502L), khl_file("rest/game__header.json")))
  # both are retried by the shared .polite_perform(backoff = 2)
  expect_equal(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM)$status, "success")
  expect_equal(sum(khl_methods(log) == "POST"), 3)
  expect_equal(log$sleeps, c(2, 4))

  khl_reset()
  log <- khl_transport(rep(list(khl_text("boom", 500L)), 3))
  expect_error(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM), "500",
               class = "fastRhockey_asset_fetch")
  expect_equal(sum(khl_methods(log) == "POST"), 3)
  expect_equal(log$sleeps, c(2, 4))
})

test_that("an empty success is no data", {
  skip_on_cran()
  khl_reset()
  khl_transport(list(khl_body(list(status = "success", data = list(), errors = list()))))
  expect_error(.khl_get(paste0(KHL_REST, "/players/stats/"), list(`values[id]` = 41928)),
               class = "fastRhockey_no_data")
  khl_transport(list(khl_body(list(status = "success", data = list(players = setNames(list(), character(0))),
                                   errors = list()))))
  expect_error(.khl_get(paste0(KHL_REST, "/clubs/team/"), list(`values[club_id]` = 7)),
               class = "fastRhockey_no_data")
})

test_that("an answer for another game, player or tournament is a failed fetch", {
  skip_on_cran()
  cases <- list(
    list("/game/header/", c(KHL_HEADER_FORM[-2], list(`values[gameid]` = 1)), "rest/game__header.json", "asked for game 1"),
    list("/game/text/", modifyList(KHL_HEADER_FORM, list(`values[tournament]` = 1370)), "rest/game__text.json",
         "of tournament 1370"),
    list("/players/stats/", list(`values[id]` = 1), "rest/players__stats.json", "asked for player 1"),
    list("/players/profile/", list(`values[id]` = 1), "rest/players__profile.json", "asked for player 1"),
    list("/calendar/list/", list(`values[tournament]` = 1423), "rest/calendar__list.json", "tournament 1423")
  )
  for (case in cases) {
    khl_reset()
    khl_transport(list(khl_file(case[[3]])))
    expect_error(.khl_get(paste0(KHL_REST, case[[1]]), case[[2]]), case[[4]], class = "fastRhockey_asset_fetch",
                 info = case[[1]])
  }
})

test_that("a schedule without games is no data; All-Star rows do not fail the scope check", {
  skip_on_cran()
  body <- khl_fx("rest/calendar__list.json")
  body$data$GAMES <- list(NULL, list(`2026-01-01` = NULL))
  khl_reset()
  khl_transport(list(khl_body(body)))
  expect_error(.khl_get(paste0(KHL_REST, "/calendar/list/"), list(`values[tournament]` = 1369)), "no games",
               class = "fastRhockey_no_data")
  body <- khl_fx("rest/calendar__list.json")
  day <- names(body$data$GAMES[[1]])[[1]]
  star <- modifyList(body$data$GAMES[[1]][[day]][[1]], list(id = 901626, tnId = 1423))
  body$data$GAMES[[1]][[day]] <- c(body$data$GAMES[[1]][[day]], list(star))
  khl_reset()
  khl_transport(list(khl_body(body)))
  expect_equal(.khl_get(paste0(KHL_REST, "/calendar/list/"), list(`values[tournament]` = 1369))$status, "success")
})

test_that("status and body follow the error vocabulary", {
  skip_on_cran()
  cases <- list(
    list(list(khl_text("not found", 404L)), "fastRhockey_no_data"),
    list(list(khl_text("bad", 400L)), "fastRhockey_bad_request"),
    list(rep(list(khl_text("boom", 500L)), 3), "fastRhockey_asset_fetch"),
    list(list(khl_text("{\"status\": \"success\", \"data\": {\"ga\"")), "fastRhockey_asset_fetch"),  # cut off
    list(list(khl_text("<html>anti-bot</html>")), "fastRhockey_asset_fetch"),
    list(list(khl_text("")), "fastRhockey_asset_fetch")
  )
  for (case in cases) {
    khl_reset()
    khl_transport(case[[1]])
    expect_error(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM), class = case[[2]])
  }
})

test_that("a session that cannot be primed is a failed fetch", {
  skip_on_cran()
  for (home in list(khl_text("<html>no token</html>"), khl_text("access_deny", 403L))) {
    khl_reset()
    khl_transport(homes = rep(list(home), 3))
    expect_error(.khl_get(paste0(KHL_REST, "/game/header/"), KHL_HEADER_FORM), class = "fastRhockey_asset_fetch")
    expect_null(.khl_state$sessions[["https://www.khl.ru"]])
  }
})

test_that("betting fields are stripped from khl.ru bodies", {
  skip_on_cran()
  body <- khl_fx("rest/game__text.json")
  body$data$game$fonbet <- TRUE
  body$data$game$bets <- list(url = "https://bookmaker/?aff=1")
  khl_reset()
  khl_transport(list(khl_body(body)))
  got <- .khl_get(paste0(KHL_REST, "/game/text/"), KHL_HEADER_FORM)
  expect_false(any(c("fonbet", "bets") %in% names(got$data$game)))
})

# ---- webcaster runtime --------------------------------------------------------------------

test_that("the season table maps an END year to a tournament id and a stage id", {
  skip_on_cran()
  khl_reset()
  fetched <- khl_season_table_mock()
  expect_equal(c(.khl_stage_for(2026)$stage_id, .khl_stage_for(2026)$tournament_id), c(370L, 1369L))
  po <- .khl_stage_for(2026, "playoff")
  expect_equal(c(po$tournament_id, po$stage_id), c(1370L, 395L))
  expect_equal(.khl_stage_for("2010")$tournament_id, 167L)  # the odd/even pattern is not used
  expect_equal(.khl_stage_for(2009)$tournament_id, 160L)
  expect_equal(.khl_stage_for(2009, "playoff")$tournament_id, 165L)
  expect_equal(.khl_stage_for(2026, NULL)$type, "regular")
  expect_equal(fetched$urls, paste0(KHL_WC, "/data.json"))  # fetched once, cached
})

test_that("the season table refuses what it lacks", {
  skip_on_cran()
  khl_reset()
  khl_season_table_mock()
  expect_error(.khl_stage_for(1990), class = "fastRhockey_no_data")
  expect_error(.khl_stage_for(2027, "playoff"), class = "fastRhockey_no_data")
  expect_error(.khl_stage_for(2026, "final"), class = "fastRhockey_bad_request")
  expect_error(.khl_stage_for("2025-26"), class = "fastRhockey_bad_request")
  khl_reset()
  testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) list(stages_v2 = list()))
  expect_error(.khl_season_table(), class = "fastRhockey_no_data")
})

test_that("a webcaster season becomes the stage id and never reaches the wire", {
  skip_on_cran()
  khl_reset()
  khl_season_table_mock()
  .khl_season_table()
  seen <- NULL
  testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) {
    seen <<- list(url = url, query = query)
    khl_fx("webcaster/teams_v2.json")
  })
  .khl_wc_get("teams_v2.json", list(season = 2026, stage = "regular", stage_id = NULL, locale = "en"))
  expect_equal(seen$url, paste0(KHL_WC, "/teams_v2.json"))
  expect_equal(seen$query, list(locale = "en", stage_id = 370L))
  # an explicit stage_id wins over the season
  .khl_wc_get("teams_v2.json", list(season = 1990, stage_id = 407, locale = "en"))
  expect_equal(seen$query[["stage_id"]], 407)
})

test_that("webcaster error envelopes follow the error vocabulary", {
  skip_on_cran()
  cases <- list(
    list(list(error = list(code = 404, message = "Not found")), "fastRhockey_no_data"),
    list(list(error = list(code = 500, message = "x")), "fastRhockey_asset_fetch"),
    list(setNames(list(), character(0)), "fastRhockey_no_data")
  )
  for (case in cases) {
    testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) case[[1]])
    expect_error(.khl_wc_get("event_v2.json", list(id = 2786714)), class = case[[2]])
  }
})

test_that("a webcaster answer for another id, stage or player is a failed fetch", {
  skip_on_cran()
  cases <- list(
    list("event_v2.json", list(id = 1), "webcaster/event_v2.json"),
    list("team_v2.json", list(id = 1), "webcaster/team_v2.json"),
    list("events_v2.json", list(stage_id = 407), "webcaster/events_v2.json"),
    list("players_v2.json", list(`q[id_in][]` = 33405), "webcaster/players_v2.json")
  )
  for (case in cases) {
    body <- khl_fx(case[[3]])
    testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) body)
    expect_error(.khl_wc_get(case[[1]], case[[2]]), "asked for", class = "fastRhockey_asset_fetch", info = case[[1]])
  }
  # a numeric id the caller typed as a double is the same id (never "18.0")
  body <- khl_fx("webcaster/team_v2.json")
  testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) body)
  expect_type(.khl_wc_get("team_v2.json", list(id = 18)), "list")
})

test_that("an empty events page ends the stage; redactions are applied to every body", {
  skip_on_cran()
  khl_reset()
  khl_season_table_mock()
  testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) list())
  expect_equal(.khl_wc_get("events_v2.json", list(stage_id = 370, page = 48)), list())
  body <- list(remote_ip = "203.0.113.9", stages_v2 = list(list(id = 1, bets = list(url = "https://bookmaker/?aff=1"))))
  testthat::local_mocked_bindings(.khl_wc_fetch = function(url, query = list()) body)
  expect_equal(.khl_wc_get("data.json"), list(stages_v2 = list(list(id = 1))))
})

test_that("webcaster requests are HTTP/1.1, repeat a vector query key, and honour the proxy option", {
  skip_on_cran()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response(status_code = 200L, url = req$url, headers = list(`Content-Type` = "application/json"),
                    body = charToRaw("[]"))
  })
  testthat::local_mocked_bindings(.polite_wait = function(url, min_interval = 1) invisible(0))
  .khl_wc_fetch(paste0(KHL_WC, "/events_v2.json"), list(`q[team_a_or_team_b_in][]` = c(18, 27), page = 2))
  expect_equal(seen$options$http_version, 2L)
  expect_equal(lengths(regmatches(seen$url, gregexpr("q%5Bteam_a_or_team_b_in%5D%5B%5D=", seen$url))), 2)
  expect_match(seen$url, "page=2")
  withr::local_options(fastRhockey.proxy = "http://proxy.example:8000")
  .khl_wc_fetch(paste0(KHL_WC, "/data.json"), list(locale = "en"))
  expect_equal(seen$options$proxy, "http://proxy.example:8000")
})

test_that("a failed proxied fetch is reported without its cause (the proxy URL can carry credentials)", {
  skip_on_cran()
  secret <- "http://user:s3cret@proxy.example:8000"
  withr::local_options(fastRhockey.proxy = secret)
  testthat::local_mocked_bindings(.polite_perform = function(req, ...) {
    rlang::abort(paste("ProxyError: cannot connect to", secret), class = c("fastRhockey_asset_fetch", "fastRhockey_error"),
                 parent = simpleError(paste("cause", secret)))
  })
  err <- tryCatch(.khl_wc_fetch(paste0(KHL_WC, "/tables_v2.json"), list(locale = "en")), error = function(e) e)
  expect_s3_class(err, "fastRhockey_asset_fetch")
  expect_false(grepl("s3cret", conditionMessage(err)))
  expect_false(grepl("s3cret", paste(format(err), collapse = " ")))
  expect_null(err$parent)
})

# ---- exported wrappers --------------------------------------------------------------------

test_that("all 23 wrappers exist with sdv-py's names and no 'return_as_pandas'", {
  skip_on_cran()
  rest <- c("schedule", "roster", "stat_filters", "game_header", "game_protocol", "game_info", "player_box",
            "pbp", "shots", "player_profile", "player_career", "playoff_bracket", "standings", "player_stats")
  wc <- c("stages", "event", "events", "leaders", "players", "players_light", "tables", "team", "teams")
  ns <- asNamespace("fastRhockey")
  fns <- c(paste0("khl_", rest), paste0("khl_webcaster_", wc))
  expect_length(fns, 23)
  for (f in fns) expect_true(is.function(get0(f, envir = ns)), info = f)
  expect_identical(names(formals(khl_schedule)), c("season", "stage", "headers", "tournament_id", "return_parsed"))
  expect_identical(names(formals(khl_player_stats)),
                   c("season", "stage", "position", "club", "page", "headers", "tournament_id", "per_page",
                     "return_parsed"))
  expect_identical(formals(khl_player_stats)$per_page, 20)
  expect_identical(names(formals(khl_webcaster_events)),
                   c("season", "stage", "page", "stage_id", "team_ids", "start_after", "start_before", "locale",
                     "return_parsed"))
  expect_identical(formals(khl_webcaster_events)$page, 1)
})

test_that("each khl.ru wrapper sends sdv-py's route and form and returns the documented columns", {
  skip_on_cran()
  game <- list(game_id = 898236, season = 2026)
  specs <- list(
    list("khl_schedule", list(season = 2026), "/calendar/list/", "rest/calendar__list.json",
         c(`values[tournament]` = NA, season = "2026", stage = "regular")),
    list("khl_roster", list(club_id = 7), "/clubs/team/", "rest/clubs__team.json",
         c(page = "team", `values[club_id]` = "7")),
    list("khl_stat_filters", list(season = 2026), "/filters/statplayers/", "rest/filters__statplayers.json",
         c(season = "2026")),
    list("khl_game_header", game, "/game/header/", "rest/game__header.json",
         c(page = "resume", `values[gameid]` = "898236")),
    list("khl_game_protocol", game, "/game/protocol/", "rest/game__protocol.json", c(`values[gameid]` = "898236")),
    list("khl_game_info", game, "/game/text/", "rest/full/game__text.json", c(`values[gameid]` = "898236")),
    list("khl_player_box", game, "/game/text/", "rest/full/game__text.json", c(`values[gameid]` = "898236")),
    list("khl_pbp", game, "/game/text/", "rest/full/game__text.json", c(`values[gameid]` = "898236")),
    list("khl_shots", game, "/game/text/", "rest/full/game__text.json", c(`values[gameid]` = "898236")),
    list("khl_player_profile", list(player_id = 41928), "/players/profile/", "rest/players__profile.json",
         c(`values[id]` = "41928")),
    list("khl_player_career", list(player_id = 41928), "/players/stats/", "rest/players__stats.json",
         c(`values[id]` = "41928")),
    list("khl_playoff_bracket", list(season = 2026), "/standings/playoff/", "rest/standings__playoff.json",
         c(stage = "playoff", `values[type]` = "playoff", season = "2026")),
    list("khl_standings", list(season = 2026), "/standings/regular/", "rest/standings__regular.json",
         c(`values[type]` = "regular", season = "2026")),
    list("khl_player_stats", list(season = 2026, position = "forwards"), "/stat/players/", "rest/stat__players.json",
         c(`values[amplua_full]` = "forwards", `nav[page]` = "1", `nav[count]` = "20"))
  )
  schema <- khl_parity_schema()
  expect_length(specs, 14)
  for (s in specs) {
    seen <- NULL
    body <- khl_fx(s[[4]])
    testthat::local_mocked_bindings(.khl_get = function(url, params = list(), headers = NULL, timeout = 60) {
      seen <<- list(url = url, params = Filter(Negate(is.null), params))
      body
    })
    got <- do.call(s[[1]], s[[2]])
    expect_equal(seen$url, paste0(KHL_REST, s[[3]]), info = s[[1]])
    want <- s[[5]][!is.na(s[[5]])]
    sent <- vapply(seen$params, function(x) as.character(x)[[1]], "")
    expect_equal(sent[names(want)], want, info = s[[1]])
    expect_s3_class(got, "fastRhockey_data")
    expect_identical(names(got), schema$col[schema$fn == s[[1]]], info = s[[1]])
    expect_identical(do.call(s[[1]], c(s[[2]], list(return_parsed = FALSE))), body, info = s[[1]])
  }
})

test_that("each webcaster wrapper sends sdv-py's route and query and returns the documented columns", {
  skip_on_cran()
  specs <- list(
    list("khl_webcaster_stages", list(), "data.json", "webcaster/full/data.json", c(locale = "en")),
    list("khl_webcaster_event", list(event_id = 2786714), "event_v2.json", "webcaster/event_v2.json",
         c(id = "2786714", locale = "en")),
    list("khl_webcaster_events", list(season = 2026, page = 2, team_ids = 18), "events_v2.json",
         "webcaster/events_v2.json",
         c(season = "2026", stage = "regular", page = "2", `q[team_a_or_team_b_in][]` = "18", locale = "en")),
    list("khl_webcaster_leaders", list(stage_id = 370), "leaders_v2.json", "webcaster/leaders_v2.json",
         c(stage_id = "370", locale = "en")),
    list("khl_webcaster_players", list(player_ids = 33405), "players_v2.json", "webcaster/players_v2.json",
         c(`q[id_in][]` = "33405", locale = "en")),
    list("khl_webcaster_players_light", list(locale = "ru"), "players_v2_light.json",
         "webcaster/players_v2_light.json", c(locale = "ru")),
    list("khl_webcaster_tables", list(), "tables_v2.json", "webcaster/tables_v2.json", c(locale = "en")),
    list("khl_webcaster_team", list(team_id = 18, season = 2026), "team_v2.json", "webcaster/team_v2.json",
         c(id = "18", season = "2026", stage = "regular", locale = "en")),
    list("khl_webcaster_teams", list(season = 2026), "teams_v2.json", "webcaster/teams_v2.json",
         c(season = "2026", stage = "regular", locale = "en"))
  )
  schema <- khl_parity_schema()
  expect_length(specs, 9)
  for (s in specs) {
    seen <- NULL
    body <- khl_fx(s[[4]])
    testthat::local_mocked_bindings(.khl_wc_get = function(route, params = list()) {
      seen <<- list(route = route, params = Filter(Negate(is.null), params))
      body
    })
    got <- do.call(s[[1]], s[[2]])
    expect_equal(seen$route, s[[3]], info = s[[1]])
    sent <- vapply(seen$params, function(x) paste(as.character(x), collapse = ","), "")
    expect_equal(sent[names(s[[5]])], s[[5]], info = s[[1]])
    expect_s3_class(got, "fastRhockey_data")
    expect_identical(names(got), schema$col[schema$fn == s[[1]]], info = s[[1]])
    expect_identical(do.call(s[[1]], c(s[[2]], list(return_parsed = FALSE))), body, info = s[[1]])
  }
})

test_that("bad arguments are refused before any request", {
  skip_on_cran()
  khl_reset()
  log <- khl_transport()
  expect_error(khl_schedule(season = 2026, stage = "final"), class = "fastRhockey_bad_request")
  expect_error(khl_player_stats(season = 2026, position = "wingers"), class = "fastRhockey_bad_request")
  expect_error(khl_game_info(game_id = NULL, season = 2026), class = "fastRhockey_bad_request")
  expect_error(khl_roster(club_id = NA), class = "fastRhockey_bad_request")
  expect_error(khl_webcaster_teams(season = 2026, stage = "final"), class = "fastRhockey_bad_request")
  expect_error(khl_webcaster_teams(season = 2026, locale = "de"), class = "fastRhockey_bad_request")
  expect_error(khl_pbp(game_id = 898236), class = "fastRhockey_bad_request")  # no season / tournament
  expect_length(log$calls, 0)
})

test_that("a wrapper end to end: mocked transport, season table, parsed frame", {
  skip_on_cran()
  khl_reset()
  khl_season_table_mock()
  log <- khl_transport(list(khl_file("rest/full/game__text.json")))
  pbp <- khl_pbp(game_id = 898236, season = 2026)
  expect_s3_class(pbp, "fastRhockey_data")
  expect_equal(unique(pbp$game_id), 898236L)
  expect_equal(as.character(log$calls[[2]]$form[["values[tournament]"]]), "1369")
  # the wrapper scopes scipen/stringsAsFactors and restores them
  old <- options(scipen = 7)
  on.exit(options(old), add = TRUE)
  khl_transport(list(khl_file("rest/full/game__text.json")))
  khl_shots(game_id = 898236, tournament_id = 1369)
  expect_equal(getOption("scipen"), 7)
})

# ---- live (off by default; one request per second per host) -----------------------------------

test_that("live: the season table and a primed khl.ru schedule", {
  skip_on_cran()
  skip_euro_test()
  khl_reset()
  stages <- khl_webcaster_stages()
  expect_true(2026L %in% stages$season)
  sched <- khl_schedule(season = 2026)
  expect_gt(nrow(sched), 100)
  expect_true(all(sched$tournament_id == 1369L))
})

test_that(".khl_stage_for rejects non-whole seasons", {
  expect_error(.khl_stage_for(2026.9), class = "fastRhockey_bad_request")
  expect_error(.khl_stage_for("2026x"), class = "fastRhockey_bad_request")
})
