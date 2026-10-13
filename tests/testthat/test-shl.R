# Offline tests for the SHL family (R/shl.R), mirroring sdv-py's tests/hockey/shl.
#
# Bodies are the real captures sdv-py committed (tests/testthat/fixtures/shl/, 3-row samples
# plus a few full bodies in full/). HTTP is mocked with httr2::local_mocked_responses().
# Parity: fixtures/shl/parity/<case>.csv is the sdv-py frame (origin/main, #826) built from the
# SAME body, with fixtures/shl/parity/_dtypes.csv holding its dtypes; a null-vs-value
# disagreement counts as a mismatch. fixtures/shl/schemas.csv is sdv-py's column list.

shl_fx <- function(...) testthat::test_path("fixtures", "shl", ...)
shl_raw <- function(f) readBin(f, "raw", file.size(f))
shl_text <- function(f) rawToChar(shl_raw(f))
SHL_NULL <- "<<NA>>"
SHL_LAYOUT <- shl_fx("full", "statistics-v2__layout-info__directory.json")
SHL_GAME <- "bdhvuc5tex"
SHL_TEAM <- "087a-087aTQv9u"
SHL_AWAY <- "41c4-41c4BiYZU"
SHL_ATHLETE <- "qQ9-349c7WDv6"
SHL_SSGT <- "iuzqg7dqk9"

# A mocked SHL host: `routes` maps a path below /api to a raw/character body or to
# list(body =, status =). Records every request in the returned environment.
local_shl <- function(routes = list(), .env = parent.frame()) {
  rec <- new.env()
  rec$calls <- list()
  rec$waits <- 0L
  .shl_state$directory <- NULL
  withr::defer(.shl_state$directory <- NULL, envir = .env)
  routes <- utils::modifyList(list("/statistics-v2/layout-info" = shl_raw(SHL_LAYOUT)), routes)
  testthat::local_mocked_bindings(
    .polite_wait = function(...) {
      rec$waits <- rec$waits + 1L
      invisible(0)
    },
    .env = .env
  )
  httr2::local_mocked_responses(function(req) {
    u <- httr2::url_parse(req$url)
    path <- sub("^/api", "", u$path)
    rec$calls[[length(rec$calls) + 1L]] <- list(path = path, query = u$query)
    hit <- routes[[path]]
    if (is.null(hit)) stop("unexpected request ", path, call. = FALSE)
    status <- 200L
    if (is.list(hit)) {
      status <- hit$status %||% 200L
      hit <- hit$body
    }
    if (is.character(hit)) hit <- charToRaw(enc2utf8(hit))
    httr2::response(status_code = status, url = req$url,
                    headers = list(`Content-Type` = "application/json"), body = hit)
  }, env = .env)
  rec
}

shl_json <- function(f) jsonlite::fromJSON(f, simplifyVector = FALSE)
shl_dump <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
# A 3-row stats-info sample says totalCount = 473; the runtime refuses a cut page, so the
# sample's totalCount is set to its row count (as sdv-py's test does).
shl_stats_body <- function(f) {
  txt <- shl_text(f)
  n <- length(jsonlite::fromJSON(txt, simplifyVector = FALSE)[[1]]$stats)
  sub("\"totalCount\"\\s*:\\s*[0-9]+", paste0("\"totalCount\": ", n), txt)
}

shl_code <- function(x) {
  if (is.list(x)) return("list[int]")
  if (is.logical(x)) return("bool")
  if (is.integer(x)) return("int")
  if (is.double(x)) return("float")
  "str"
}

# Compare an R tibble with the sdv-py frame written for `case`: column order, dtype and
# every value. NA-vs-value is a mismatch.
expect_shl_parity <- function(tbl, case) {
  dts <- utils::read.csv(shl_fx("parity", "_dtypes.csv"), stringsAsFactors = FALSE)
  dts <- dts[dts$case == case, ]
  exp <- utils::read.csv(shl_fx("parity", paste0(case, ".csv")), colClasses = "character",
                         na.strings = SHL_NULL, check.names = FALSE, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8")
  testthat::expect_identical(names(tbl), dts$column)
  testthat::expect_identical(names(tbl), names(exp))
  testthat::expect_identical(nrow(tbl), nrow(exp))
  bad <- character()
  for (col in names(exp)) {
    r <- tbl[[col]]
    code <- dts$dtype[dts$column == col]
    if (!identical(shl_code(r), code)) {
      bad <- c(bad, paste0(col, ": dtype ", shl_code(r), " != ", code))
      next
    }
    e <- exp[[col]]
    mism <- switch(code,
      # read.csv folds CRLF inside a quoted cell to LF; compare without CR
      str = {
        rr <- gsub("\r", "", r, fixed = TRUE)
        (is.na(rr) != is.na(e)) | (!is.na(rr) & !is.na(e) & rr != gsub("\r", "", e, fixed = TRUE))
      },
      int = (is.na(r) != is.na(e)) | (!is.na(r) & !is.na(e) & r != suppressWarnings(as.numeric(e))),
      float = {
        ev <- suppressWarnings(as.numeric(e))
        (is.na(r) != is.na(ev)) |
          (!is.na(r) & !is.na(ev) & abs(r - ev) > 1e-9 * pmax(1, abs(ev)))
      },
      bool = {
        ev <- ifelse(e == "true", TRUE, ifelse(e == "false", FALSE, NA))
        (is.na(r) != is.na(ev)) | (!is.na(r) & !is.na(ev) & r != ev)
      },
      `list[int]` = {
        rv <- vapply(r, function(v) {
          if (is.null(v)) NA_character_ else paste(ifelse(is.na(v), "None", v), collapse = ";")
        }, "")
        (is.na(rv) != is.na(e)) | (!is.na(rv) & !is.na(e) & rv != e)
      }
    )
    if (any(mism)) bad <- c(bad, paste0(col, ": ", sum(mism), " mismatch(es)"))
  }
  testthat::expect_identical(bad, character(), info = case)
}

SHL_EXPORTS <- c(
  "shl_athlete_career", "shl_athlete_details", "shl_athlete_game_log", "shl_athlete_profile",
  "shl_athlete_seasons", "shl_boxscore", "shl_game_header", "shl_game_overview",
  "shl_game_summary", "shl_last_game_day", "shl_layout_info", "shl_next_game_day", "shl_pbp",
  "shl_player_box", "shl_player_h2h", "shl_post_game_team_stats", "shl_roster", "shl_schedule",
  "shl_season_filter", "shl_sites", "shl_ssgt_provider", "shl_standings", "shl_stats_info",
  "shl_team", "shl_team_played_games", "shl_team_stats", "shl_team_upcoming_games"
)

# ---------------------------------------------------------------------------
# Surface and schemas
# ---------------------------------------------------------------------------

test_that("the 27 sdv-py wrappers and the two public helpers exist with sdv-py's arguments", {
  skip_on_cran()
  for (fn in c(SHL_EXPORTS, "shl_ssgt_uuid", "shl_player_crosswalk")) {
    expect_true(is.function(get(fn)), info = fn)
  }
  expect_named(formals(shl_stats_info), c("module", "season", "game_type", "return_parsed"))
  expect_identical(formals(shl_stats_info)$game_type, "regular")
  expect_named(formals(shl_schedule), c("season", "game_type", "return_parsed"))
  expect_true(is.symbol(formals(shl_schedule)$season))
  expect_null(formals(shl_last_game_day)$season)
  expect_named(formals(shl_player_h2h),
               c("ssgt_uuid", "home_team_uuid", "away_team_uuid", "game_uuid", "return_parsed"))
  expect_named(formals(shl_post_game_team_stats),
               c("game_uuid", "ssgt_uuid", "home_team_uuid", "away_team_uuid", "return_parsed"))
  expect_named(formals(shl_athlete_game_log),
               c("athlete_uuid", "master_site_instance_id", "season", "return_parsed"))
  expect_named(formals(shl_team_upcoming_games), c("team_uuid", "game_place", "return_parsed"))
  expect_named(formals(shl_season_filter), c("series_uuid", "season_uuid", "return_parsed"))
  expect_named(formals(shl_layout_info), "return_parsed")
})

test_that("the embedded schemas equal sdv-py's committed column list", {
  skip_on_cran()
  ref <- utils::read.csv(shl_fx("schemas.csv"), stringsAsFactors = FALSE)
  expect_setequal(names(.SHL_SCHEMAS), unique(ref$endpoint))
  expect_length(.SHL_SCHEMAS, 27L)
  for (ep in names(.SHL_SCHEMAS)) {
    r <- ref[ref$endpoint == ep, ]
    expect_identical(names(.SHL_SCHEMAS[[ep]]), r$column, info = ep)
    expect_identical(unname(.SHL_SCHEMAS[[ep]]), r$dtype, info = ep)
  }
  expect_identical(nrow(ref), 961L)
})

# ---------------------------------------------------------------------------
# Parity: every wrapper, on the same body, against the sdv-py frame
# ---------------------------------------------------------------------------

SHL_ROUTES <- list(
  list("athlete_career", quote(shl_athlete_career(SHL_ATHLETE)),
       "/statistics-v2/athlete/playerProfile_gameLog", "statistics-v2__athlete__playerProfile_gameLog.json"),
  list("athlete_details", quote(shl_athlete_details(SHL_ATHLETE)),
       paste0("/sports-v2/athlete-details/", SHL_ATHLETE), "sports-v2__athlete-details__athlete_uuid.json"),
  list("athlete_game_log", quote(shl_athlete_game_log(SHL_ATHLETE, season = 2026)),
       "/statistics-v2/athlete/playerProfile_seasonGameLog", "statistics-v2__athlete__playerProfile_seasonGameLog.json"),
  list("athlete_profile", quote(shl_athlete_profile(SHL_ATHLETE)),
       "/statistics-v2/athlete/profile-page", "statistics-v2__athlete__profile-page.json"),
  list("athlete_seasons", quote(shl_athlete_seasons(SHL_ATHLETE)),
       "/statistics-v2/athlete/seasonList", "statistics-v2__athlete__seasonList.json"),
  list("boxscore", quote(shl_boxscore(SHL_GAME)),
       paste0("/gameday/boxscore/", SHL_GAME), "gameday__boxscore__game_uuid.json"),
  list("game_header", quote(shl_game_header(SHL_GAME)),
       paste0("/sports-v2/game-info/", SHL_GAME), "sports-v2__game-info__game_uuid.json"),
  list("game_overview", quote(shl_game_overview(SHL_GAME)),
       paste0("/gameday/game-overview/", SHL_GAME), "gameday__game-overview__game_uuid.json"),
  list("game_summary", quote(shl_game_summary(SHL_GAME)),
       paste0("/gameday/game-info/", SHL_GAME), "gameday__game-info__game_uuid.json"),
  list("last_game_day", quote(shl_last_game_day()),
       "/sports-v2/last-game-day", "sports-v2__last-game-day.json"),
  list("layout_info", quote(shl_layout_info()),
       "/statistics-v2/layout-info", "statistics-v2__layout-info.json"),
  list("next_game_day", quote(shl_next_game_day()),
       "/sports-v2/next-game-day", "sports-v2__next-game-day.json"),
  list("pbp", quote(shl_pbp(SHL_GAME)),
       paste0("/gameday/play-by-play/", SHL_GAME), "gameday__play-by-play__game_uuid.json"),
  list("player_box", quote(shl_player_box(SHL_GAME)),
       paste0("/gameday/player-stats/", SHL_GAME), "gameday__player-stats__game_uuid.json"),
  list("player_h2h", quote(shl_player_h2h(SHL_SSGT, SHL_TEAM, SHL_AWAY, game_uuid = SHL_GAME)),
       paste0("/statistics-v2/player-h2h-v2/", SHL_SSGT, "/", SHL_TEAM, "/", SHL_AWAY),
       "statistics-v2__player-h2h-v2__ssgt__home__away.json"),
  list("post_game_team_stats", quote(shl_post_game_team_stats(SHL_GAME, SHL_SSGT, SHL_TEAM, SHL_AWAY)),
       paste0("/gameday/post-game-data/team-stats/", SHL_GAME), "gameday__post-game-data__team-stats__game_uuid.json"),
  list("roster", quote(shl_roster(SHL_TEAM)),
       paste0("/sports-v2/athletes/by-team-uuid/", SHL_TEAM), "sports-v2__athletes__by-team-uuid__team_uuid.json"),
  list("schedule", quote(shl_schedule(2026)),
       "/sports-v2/game-schedule", "sports-v2__game-schedule.json"),
  list("season_filter", quote(shl_season_filter()),
       "/sports-v2/season-series-game-types-filter", "sports-v2__season-series-game-types-filter.json"),
  list("sites", quote(shl_sites(SHL_SSGT)),
       paste0("/sports-v2/all-sites/", SHL_SSGT), "sports-v2__all-sites__ssgt_uuid.json"),
  list("ssgt_provider", quote(shl_ssgt_provider(SHL_SSGT)),
       paste0("/sports-v2/ssgt/", SHL_SSGT, "/provider"), "sports-v2__ssgt__ssgt_uuid__provider.json"),
  list("standings", quote(shl_standings(SHL_SSGT)),
       paste0("/gameday/team-standings/", SHL_SSGT), "gameday__team-standings__ssgt_uuid.json"),
  list("team", quote(shl_team(SHL_TEAM)),
       paste0("/sports-v2/teams/", SHL_TEAM), "sports-v2__teams__team_uuid.json"),
  list("team_played_games", quote(shl_team_played_games(SHL_TEAM)),
       paste0("/sports-v2/played-games/", SHL_TEAM), "sports-v2__played-games__team_uuid.json"),
  list("team_stats", quote(shl_team_stats(SHL_GAME)),
       paste0("/gameday/team-stats/", SHL_GAME), "gameday__team-stats__game_uuid.json"),
  list("team_upcoming_games", quote(shl_team_upcoming_games(SHL_TEAM)),
       paste0("/sports-v2/upcoming-games/", SHL_TEAM), "sports-v2__upcoming-games__team_uuid.json")
)

test_that("each of the 26 single-table wrappers matches the sdv-py frame on the same body", {
  skip_on_cran()
  expect_length(SHL_ROUTES, 26L)
  for (r in SHL_ROUTES) {
    case <- r[[1]]
    body <- if (r[[4]] == "sports-v2__ssgt__ssgt_uuid__provider.json") "statnet" else shl_raw(shl_fx(r[[4]]))
    routes <- stats::setNames(list(body), r[[3]])
    if (case == "layout_info") routes[[r[[3]]]] <- shl_raw(shl_fx(r[[4]]))
    local_shl(routes)
    out <- eval(r[[2]])
    expect_s3_class(out, "fastRhockey_data")
    expect_shl_parity(out, case)
  }
})

test_that("shl_stats_info matches the sdv-py frame for all 34 modules and the two full bodies", {
  skip_on_cran()
  mods <- sub("^stats-info__module__(.*)\\.json$", "\\1",
              list.files(shl_fx(), pattern = "^stats-info__module__"))
  expect_length(mods, 34L)
  expect_setequal(mods, .SHL_MODULES)
  for (m in mods) {
    f <- shl_fx(paste0("stats-info__module__", m, ".json"))
    local_shl(stats::setNames(list(charToRaw(shl_stats_body(f))), paste0("/statistics-v2/stats-info/", m)))
    expect_shl_parity(shl_stats_info(m, 2026), paste0("stats_info__", m))
  }
  for (m in c("teams_result", "goalkeepers_summary")) {
    f <- shl_fx("full", paste0("stats-info__", m, "__", SHL_SSGT, ".json"))
    local_shl(stats::setNames(list(shl_raw(f)), paste0("/statistics-v2/stats-info/", m)))
    expect_shl_parity(shl_stats_info(m, 2026), paste0("stats_info__full_", m))
  }
})

test_that("full play-by-play and box score bodies match sdv-py (ids, coordinates, on-ice lists)", {
  skip_on_cran()
  local_shl(stats::setNames(list(shl_raw(shl_fx("full", "gameday__play-by-play__bdhvuc5tex.json"))),
                            paste0("/gameday/play-by-play/", SHL_GAME)))
  pbp <- shl_pbp(SHL_GAME)
  expect_shl_parity(pbp, "pbp__full")
  expect_identical(nrow(pbp), 131L)
  expect_type(pbp$player_id, "integer") # a STRING in this body
  expect_type(pbp$game_id, "integer")
  expect_type(pbp$event_id, "integer")
  expect_type(pbp$location_x, "integer")
  expect_type(pbp$pop_player_ids, "list")
  goals <- pbp[pbp$type == "goal", ]
  expect_identical(nrow(goals), 3L)
  expect_false(anyNA(goals$goal_section))
  expect_true(all(vapply(seq_len(nrow(goals)), function(i) goals$player_id[i] %in% goals$pop_player_ids[[i]], TRUE)))
  expect_true(all(lengths(goals$pop_player_ids) >= 5L))
  expect_true(all(lengths(goals$nep_player_ids) >= 4L))

  local_shl(stats::setNames(list(shl_raw(shl_fx("full", "gameday__boxscore__bdhvuc5tex.json"))),
                            paste0("/gameday/boxscore/", SHL_GAME)))
  box <- shl_boxscore(SHL_GAME)
  expect_shl_parity(box, "boxscore__full")
  expect_setequal(box$team_side, c("home", "away"))
  expect_setequal(box$position_group, c("skater", "goalie"))
  expect_false(anyNA(box$full_name))
  expect_type(box$svs_perc, "double")
  expect_type(box$toi, "character")
  expect_equal(box$svs_perc[box$player_id == 2427], 28 / 29)
})

test_that("the layout directory and the player crosswalk match sdv-py", {
  skip_on_cran()
  parsed <- .shl_parse_layout(shl_json(SHL_LAYOUT), .SHL_SCHEMAS$layout_info)
  expect_shl_parity(parsed, "layout_info__directory")
  expect_identical(nrow(parsed), 110L)
  expect_identical(sort(unique(parsed$game_type)), c("playin", "playoff", "qualdown", "regular"))

  f <- shl_fx("stats-info__module__players_summary.json")
  local_shl(list("/statistics-v2/stats-info/players_summary" = charToRaw(shl_stats_body(f))))
  xw <- shl_player_crosswalk(2026)
  expect_s3_class(xw, "fastRhockey_data")
  expect_shl_parity(xw, "player_crosswalk")
  expect_type(xw$player_id, "integer")
  expect_type(xw$athlete_uuid, "character")
})

# ---------------------------------------------------------------------------
# Season -> ssgt
# ---------------------------------------------------------------------------

test_that("stats-info maps the END year to its ssgt and always sends count = 1000", {
  skip_on_cran()
  f <- shl_fx("stats-info__module__standings_standings.json")
  rec <- local_shl(list("/statistics-v2/stats-info/standings_standings" = charToRaw(shl_stats_body(f))))
  shl_stats_info("standings_standings", 2026)
  q <- rec$calls[[length(rec$calls)]]$query
  expect_identical(q$ssgtUuid, SHL_SSGT)
  expect_identical(q$count, "1000")
  expect_null(q$season)
  expect_null(q$gameType)
  expect_length(rec$calls, 2L) # layout-info + the table
  expect_identical(rec$waits, 2L) # each over the polite clock
})

test_that("layout-info is fetched once per session", {
  skip_on_cran()
  f <- shl_fx("full", paste0("stats-info__teams_result__", SHL_SSGT, ".json"))
  rec <- local_shl(list("/statistics-v2/stats-info/teams_result" = shl_raw(f)))
  for (i in 1:3) shl_stats_info("teams_result", 2026)
  paths <- vapply(rec$calls, function(x) x$path, "")
  expect_identical(sum(paths == "/statistics-v2/layout-info"), 1L)
})

test_that("game-schedule gets all three uuids and is checked against the resolved ssgt", {
  skip_on_cran()
  rec <- local_shl(list("/sports-v2/game-schedule" = shl_raw(shl_fx("sports-v2__game-schedule.json"))))
  shl_schedule(2026)
  q <- rec$calls[[length(rec$calls)]]$query
  expect_identical(q$seasonUuid, "xs4m9qupsi")
  expect_identical(q$seriesUuid, "qQ9-bb0bzEWUk")
  expect_identical(q$gameTypeUuid, "qQ9-af37Ti40B")
})

test_that("a schedule of another season is refused (wrong answer)", {
  skip_on_cran()
  other <- shl_json(shl_fx("sports-v2__game-schedule.json"))
  for (i in seq_along(other$gameInfo)) other$gameInfo[[i]]$ssgtUuid <- "qeb-73qtP8d19"
  local_shl(list("/sports-v2/game-schedule" = shl_dump(other)))
  expect_error(shl_schedule(2026), class = "fastRhockey_asset_fetch", regexp = "not the data asked for")
})

test_that("match-day routes without a season use the active ssgt; with one they map it", {
  skip_on_cran()
  rec <- local_shl(list("/sports-v2/last-game-day" = shl_raw(shl_fx("sports-v2__last-game-day.json"))))
  shl_last_game_day()
  q <- rec$calls[[length(rec$calls)]]$query
  expect_identical(q, list(seasonSeriesGameType = "qa98unlbd6"))
  expect_identical(shl_ssgt_uuid(2027), "qa98unlbd6")
})

test_that("the athlete season game log sends the START year", {
  skip_on_cran()
  rec <- local_shl(list("/statistics-v2/athlete/playerProfile_seasonGameLog" =
                          shl_raw(shl_fx("statistics-v2__athlete__playerProfile_seasonGameLog.json"))))
  shl_athlete_game_log(SHL_ATHLETE, season = 2026)
  q <- rec$calls[[length(rec$calls)]]$query
  expect_identical(q$season, "2025")
  expect_identical(q$playerUuid, SHL_ATHLETE)
})

test_that("the filter route passes its season uuid through untouched", {
  skip_on_cran()
  rec <- local_shl(list("/sports-v2/season-series-game-types-filter" =
                          shl_raw(shl_fx("sports-v2__season-series-game-types-filter.json"))))
  shl_season_filter(season_uuid = "xs4m9qupsi")
  expect_identical(rec$calls[[length(rec$calls)]]$query, list(season = "xs4m9qupsi"))
})

test_that("an unknown season or game type is no data; shl_ssgt_uuid resolves the rest", {
  skip_on_cran()
  local_shl()
  for (a in list(list(2028, "regular"), list(1975, "regular"), list(2026, "allstar"))) {
    expect_error(shl_ssgt_uuid(a[[1]], a[[2]]), class = "fastRhockey_no_data",
                 regexp = "layout-info covers 1976-2027")
  }
  expect_identical(shl_ssgt_uuid(2026), SHL_SSGT)
  expect_identical(shl_ssgt_uuid(2025), "qeb-73qtP8d19")
  expect_error(shl_ssgt_uuid("twenty"), class = "fastRhockey_bad_request", regexp = "END year")
})

test_that("arguments outside their choices are refused before any request", {
  skip_on_cran()
  rec <- local_shl()
  expect_error(shl_stats_info("players_nope", 2026), class = "fastRhockey_bad_request")
  expect_error(shl_stats_info("players_summary", 2026, game_type = "allstar"), class = "fastRhockey_bad_request")
  expect_error(shl_schedule(2026, game_type = NULL), class = "fastRhockey_bad_request")
  expect_length(rec$calls, 0L)
})

# ---------------------------------------------------------------------------
# Refusals
# ---------------------------------------------------------------------------

test_that("a truncated stats page is refused", {
  skip_on_cran()
  body <- shl_json(shl_fx("stats-info__module__players_summary.json"))
  body[[1]]$stats <- rep(body[[1]]$stats[1], 1000L)
  local_shl(list("/statistics-v2/stats-info/players_summary" = shl_dump(body)))
  expect_error(shl_stats_info("players_summary", 2026), class = "fastRhockey_asset_fetch", regexp = "may be cut")
})

test_that("a stats page short of its totalCount is refused", {
  skip_on_cran()
  local_shl(list("/statistics-v2/stats-info/players_summary" =
                   shl_raw(shl_fx("stats-info__module__players_summary.json"))))
  expect_error(shl_stats_info("players_summary", 2026), class = "fastRhockey_asset_fetch",
               regexp = "3 of totalCount=473")
})

test_that("a stats page matching its totalCount passes", {
  skip_on_cran()
  f <- shl_fx("full", paste0("stats-info__goalkeepers_summary__", SHL_SSGT, ".json"))
  local_shl(list("/statistics-v2/stats-info/goalkeepers_summary" = shl_raw(f)))
  expect_identical(nrow(shl_stats_info("goalkeepers_summary", 2026)), 36L)
})

test_that("a team page for another uuid is a failed fetch, not nothing", {
  skip_on_cran()
  local_shl(list("/sports-v2/teams/zzzz-zzzzzzzzz" = shl_raw(shl_fx("sports-v2__teams__team_uuid.json"))))
  expect_error(shl_team("zzzz-zzzzzzzzz"), class = "fastRhockey_asset_fetch", regexp = "not the data asked for")
})

test_that("an empty gameday body (games before 2023/24) is no data", {
  skip_on_cran()
  local_shl(list("/gameday/play-by-play/qeb-old0game" = ""))
  expect_error(shl_pbp("qeb-old0game"), class = "fastRhockey_no_data", regexp = "2023/24")
})

test_that("an empty body from a non-gameday route is a failed fetch", {
  skip_on_cran()
  local_shl(list("/sports-v2/teams/087a-087aTQv9u" = ""))
  expect_error(shl_team(SHL_TEAM), class = "fastRhockey_asset_fetch")
})

test_that("an empty stub and a zero-row table are no data", {
  skip_on_cran()
  for (stub in c("{}", "[]")) {
    local_shl(list("/sports-v2/teams/087a-087aTQv9u" = stub))
    expect_error(shl_team(SHL_TEAM), class = "fastRhockey_no_data", regexp = "empty body")
  }
  local_shl(list("/sports-v2/upcoming-games/087a-087aTQv9u" = '{"upcomingGames": []}'))
  expect_error(shl_team_upcoming_games(SHL_TEAM), class = "fastRhockey_no_data", regexp = "no rows")
})

test_that("an error envelope and a non-JSON body are failed fetches", {
  skip_on_cran()
  local_shl(list("/gameday/boxscore/bdhvuc5tex" = '{"statusCode": 500, "message": "Internal server error"}'))
  expect_error(shl_boxscore(SHL_GAME), class = "fastRhockey_asset_fetch", regexp = "error envelope")
  local_shl(list("/gameday/boxscore/bdhvuc5tex" = "<!doctype html><html></html>"))
  expect_error(shl_boxscore(SHL_GAME), class = "fastRhockey_asset_fetch", regexp = "non-JSON")
})

test_that("a body for another game is refused, and so is one that cannot show its scope", {
  skip_on_cran()
  local_shl(list("/gameday/play-by-play/zzzzzzzzzz" = shl_raw(shl_fx("gameday__play-by-play__game_uuid.json"))))
  expect_error(shl_pbp("zzzzzzzzzz"), class = "fastRhockey_asset_fetch", regexp = "gameUuid='bdhvuc5tex'")
  body <- shl_json(shl_fx("gameday__game-overview__game_uuid.json"))
  body$gameUuid <- NULL
  local_shl(list("/gameday/game-overview/bdhvuc5tex" = shl_dump(body)))
  expect_error(shl_game_overview(SHL_GAME), class = "fastRhockey_asset_fetch", regexp = "cannot be verified")
})

test_that("a body for the asked-for game passes the scope check", {
  skip_on_cran()
  local_shl(list("/gameday/play-by-play/bdhvuc5tex" = shl_raw(shl_fx("gameday__play-by-play__game_uuid.json"))))
  expect_identical(nrow(shl_pbp(SHL_GAME)), 6L)
})

test_that("non-200 answers follow the error vocabulary", {
  skip_on_cran()
  for (cs in list(list(404L, "fastRhockey_no_data"), list(400L, "fastRhockey_bad_request"),
                  list(422L, "fastRhockey_bad_request"), list(500L, "fastRhockey_asset_fetch"),
                  list(403L, "fastRhockey_asset_fetch"))) {
    local_shl(list("/sports-v2/teams/087a-087aTQv9u" = list(body = '{"message": "x"}', status = cs[[1]])))
    expect_error(shl_team(SHL_TEAM), class = cs[[2]])
    expect_error(shl_team(SHL_TEAM), class = "fastRhockey_error")
  }
})

test_that("the provider text route returns a bare word and refuses an app shell", {
  skip_on_cran()
  path <- paste0("/sports-v2/ssgt/", SHL_SSGT, "/provider")
  rec <- local_shl(stats::setNames(list("statnet"), path))
  expect_identical(.shl_get(paste0(.SHL_BASE, path)), "statnet")
  local_shl(stats::setNames(list("<!doctype html><html>app shell</html>"), path))
  expect_error(shl_ssgt_provider(SHL_SSGT), class = "fastRhockey_asset_fetch", regexp = "not a provider name")
})

test_that("return_parsed = FALSE returns the decoded JSON", {
  skip_on_cran()
  local_shl(list("/sports-v2/teams/087a-087aTQv9u" = shl_raw(shl_fx("sports-v2__teams__team_uuid.json"))))
  raw <- shl_team(SHL_TEAM, return_parsed = FALSE)
  expect_type(raw, "list")
  expect_identical(raw$uuid, SHL_TEAM)
})

test_that("every attempt, retries included, waits its turn on the host clock", {
  skip_on_cran()
  rec <- local_shl()
  n <- 0L
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1L
    if (n == 1L) {
      return(httr2::response(status_code = 503L, url = req$url, body = charToRaw("{}")))
    }
    httr2::response(status_code = 200L, url = req$url, headers = list(`Content-Type` = "application/json"),
                    body = shl_raw(shl_fx("sports-v2__teams__team_uuid.json")))
  })
  expect_identical(shl_team(SHL_TEAM)$uuid, SHL_TEAM)
  expect_identical(rec$waits, 2L)
})

# ---------------------------------------------------------------------------
# Parsers: empty payloads, id types
# ---------------------------------------------------------------------------

SHL_PARSERS <- c(
  athlete_career = ".shl_parse_stats_info", athlete_details = ".shl_parse_page",
  athlete_game_log = ".shl_parse_stats_info", athlete_profile = ".shl_parse_page",
  athlete_seasons = ".shl_parse_generic", boxscore = ".shl_parse_player_box",
  game_header = ".shl_parse_page", game_overview = ".shl_parse_page", game_summary = ".shl_parse_page",
  last_game_day = ".shl_parse_schedule", layout_info = ".shl_parse_layout",
  next_game_day = ".shl_parse_schedule", pbp = ".shl_parse_pbp", player_box = ".shl_parse_player_box",
  player_h2h = ".shl_parse_h2h", post_game_team_stats = ".shl_parse_generic",
  roster = ".shl_parse_roster", schedule = ".shl_parse_schedule", season_filter = ".shl_parse_filter",
  sites = ".shl_parse_generic", ssgt_provider = ".shl_parse_generic", standings = ".shl_parse_standings",
  stats_info = ".shl_parse_stats_info", team = ".shl_parse_page", team_played_games = ".shl_parse_schedule",
  team_stats = ".shl_parse_team_stats", team_upcoming_games = ".shl_parse_schedule"
)

test_that("an empty or malformed payload is a zero-row tibble with the documented columns and types", {
  skip_on_cran()
  expect_setequal(names(SHL_PARSERS), names(.SHL_SCHEMAS))
  type <- c(str = "character", int = "integer", float = "double", bool = "logical", `list[int]` = "list")
  payloads <- list(list(), setNames(list(), character()), NULL, "<html>", 3, list(NULL), list(stats = NULL))
  for (ep in names(SHL_PARSERS)) {
    schema <- .SHL_SCHEMAS[[ep]]
    for (p in payloads) {
      out <- get(SHL_PARSERS[[ep]])(p, schema)
      expect_identical(nrow(out), 0L, info = ep)
      expect_identical(names(out), names(schema), info = ep)
      expect_identical(vapply(out, typeof, ""), stats::setNames(unname(type[schema]), names(schema)),
                       info = ep)
    }
  }
  expect_identical(nrow(.shl_parse_crosswalk(list())), 0L)
})

test_that("a zero-row frame concatenates with a populated one", {
  skip_on_cran()
  full <- .shl_parse_pbp(shl_json(shl_fx("gameday__play-by-play__game_uuid.json")), .SHL_SCHEMAS$pbp)
  empty <- .shl_parse_pbp(list(), .SHL_SCHEMAS$pbp)
  expect_no_error(dplyr::bind_rows(empty, full[, names(.SHL_SCHEMAS$pbp)]))
})

test_that("ids have one type everywhere: isa/StatNet ids integer, uuids and team codes character", {
  skip_on_cran()
  id_re <- "(^|_)(player_id|game_id|event_id|ext_id)$"
  for (r in SHL_ROUTES) {
    case <- r[[1]]
    schema <- .SHL_SCHEMAS[[case]]
    expect_true(all(schema[grepl(id_re, names(schema))] == "int"), info = case)
    expect_true(all(schema[grepl("_uuid$", names(schema))] == "str"), info = case)
  }
  expect_identical(.SHL_SCHEMAS$stats_info[["player_id"]], "int")
  # a STRING isa id in a pbp body becomes an integer; a non-numeric one becomes NA
  pbp <- .shl_parse_pbp(list(list(type = "shot", player = list(playerId = "3622"), locationX = 12L),
                             list(type = "shot", player = list(playerId = "x"))),
                        .SHL_SCHEMAS$pbp)
  expect_identical(pbp$player_id, c(3622L, NA))
  expect_identical(pbp$location_x, c(12L, NA))
})

test_that("column names follow sdv-py's snake_case rule", {
  skip_on_cran()
  expect_identical(.shl_column("SVS%"), "svs_perc")
  expect_identical(.shl_column("+/-"), "plus_minus")
  expect_identical(.shl_column("homeTeam_names_code"), "home_team_names_code")
  expect_identical(.shl_column("TPPG"), "tppg")
  expect_identical(.shl_column("playerId"), "player_id")
  expect_identical(.shl_column("assist1_playerId"), "assist1_player_id")
  expect_identical(.shl_column("\001"), "")
})

test_that("list and object cells are JSON text exactly as Python's json.dumps(sort_keys=True)", {
  skip_on_cran()
  expect_identical(.shl_json(list(b = 1L, a = list(1, NULL, "x\"y"))), "{\"a\": [1.0, null, \"x\\\"y\"], \"b\": 1}")
  expect_identical(.shl_json(list()), "[]")
  expect_identical(.shl_json(setNames(list(), character())), "{}")
  expect_identical(.shl_float_repr(c(0.1)), "0.1")
  expect_identical(.shl_float_repr(2), "2.0")
  expect_identical(.shl_float_repr(1e22), "1e+22")
  expect_identical(.shl_float_repr(1.5e-7), "1.5e-07")
})

# ---------------------------------------------------------------------------
# Live (one request; off by default)
# ---------------------------------------------------------------------------

test_that("live: the 2025/26 regular season schedule", {
  skip_on_cran()
  skip_euro_test()
  out <- shl_schedule(2026)
  expect_gt(nrow(out), 300L)
  expect_identical(names(out)[seq_along(.SHL_SCHEMAS$schedule)], names(.SHL_SCHEMAS$schedule))
})
