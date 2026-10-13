# Ligue Magnus (liguemagnus.com) wrappers: fixtures copied from sdv-py
# tests/fixtures/magnus/, parity CSVs written by sdv-py's parsers on the SAME
# fixtures (fixtures/magnus/parity/), the transport mocked. Live tests (nine
# requests) sit behind skip_euro_test().

fx <- function(...) testthat::test_path("fixtures", "magnus", ...)
fx_json <- function(...) jsonlite::fromJSON(fx(...), simplifyVector = FALSE)

COMPETITIONS <- "ajax__get_competition_by_type.json"
PHASES <- "ajax__get_phases.json"
TEAMS <- "ajax__get_equipes.json"
SCHEDULE <- "ajax__get_rencontres.json"
ROUND2 <- "ajax__get_rencontres@round2.json"
ROUND44 <- "ajax__get_rencontres@round44.json"
ROUNDS <- "ajax__get_liste_tours.json"
ROUND_GAMES <- "ajax__get_rencontrestours.json"
STANDINGS <- "ajax__get_classementsphase.json"
# fixture key -> (file, game id the sheet is for)
SHEETS <- list(
  g77779 = list(file = "rencontre__id.json", id = 77779L),
  g68838 = list(file = "rencontre__id@68838.json", id = 68838L),
  g865 = list(file = "rencontre__id@865.json", id = 865L),
  full = list(file = "full/rencontre__77779.json", id = 77779L)
)

# The documented columns/types of sdv-py (the parsers' zero-row frames):
# polars dtype -> R class.
sdv_schema <- function(route) jsonlite::fromJSON(fx("parity", "schemas.json"))[[route]]
r_class <- function(dtype) {
  if (grepl("^Datetime", dtype)) return("POSIXct")
  if (grepl("^List", dtype)) return("list")
  c(Int64 = "integer", String = "character", Boolean = "logical", Date = "Date")[[dtype]]
}

expect_schema <- function(tbl, route) {
  s <- sdv_schema(route)
  expect_s3_class(tbl, "tbl_df")
  expect_identical(names(tbl), names(s))
  for (nm in names(s)) {
    want <- r_class(s[[nm]])
    expect_true(inherits(tbl[[nm]], want) || (want == "list" && is.list(tbl[[nm]])),
                info = sprintf("%s: %s is not %s", route, nm, want))
    if (want == "integer") expect_type(tbl[[nm]], "integer")
    if (want == "POSIXct") expect_identical(attr(tbl[[nm]], "tzone"), "Europe/Paris")
  }
}

# A column as the strings sdv-py's parity CSV carries (NA = null).
cells <- function(x) {
  if (inherits(x, "POSIXct")) return(format(x, "%Y-%m-%d %H:%M:%S%z", tz = "Europe/Paris"))
  if (inherits(x, "Date")) return(as.character(x))
  if (is.list(x)) {
    return(vapply(x, function(v) {
      if (is.null(v)) return(NA_character_)
      paste0("[", paste(ifelse(is.na(v), "null", format(v, scientific = FALSE, trim = TRUE)), collapse = ", "), "]")
    }, character(1)))
  }
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x, "true", "false")))
  if (is.character(x)) return(enc2utf8(x))
  ifelse(is.na(x), NA_character_, format(x, scientific = FALSE, trim = TRUE))
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
    g <- as.character(cells(tbl[[nm]]))
    e <- exp[[nm]]
    info <- sprintf("%s/%s column %s", route, fixture, nm)
    expect_identical(is.na(g), is.na(e), info = info)
    expect_identical(g[!is.na(g)], enc2utf8(e[!is.na(e)]), info = info)
  }
}

# Mock the shared JSON transport; capture what it is asked for.
mock_ajax <- function(body, env = parent.frame()) {
  seen <- new.env()
  seen$n <- 0L
  testthat::local_mocked_bindings(
    .euro_get = function(url, query = list(), headers = NULL, ...) {
      seen$n <- seen$n + 1L
      seen$url <- url
      seen$query <- Filter(Negate(is.null), query)
      seen$dots <- list(...)
      body
    },
    .env = env
  )
  seen
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#x27;", x, fixed = TRUE)
}

# A game page: the sheet in the :data attribute of <live-rencontre-container>,
# optionally behind a decoy :data on another element.
game_page <- function(sheet, decoy = TRUE) {
  json <- as.character(jsonlite::toJSON(sheet, auto_unbox = TRUE, null = "null", digits = NA))
  head <- if (decoy) "<div :data=\"{&quot;evenements&quot;: []}\"></div>" else ""
  enc2utf8(sprintf(
    "<html>%s<live-rencontre-container id=\"x\" :data=\"%s\"></live-rencontre-container></html>",
    head, html_escape(json)
  ))
}

# Mock the game page transport with a sheet (or raw HTML text).
mock_page <- function(sheet = NULL, html = NULL, decoy = TRUE, env = parent.frame()) {
  seen <- new.env()
  seen$n <- 0L
  page <- if (is.null(html)) game_page(sheet, decoy) else html
  testthat::local_mocked_bindings(
    .magnus_page_text = function(url) {
      seen$n <- seen$n + 1L
      seen$url <- url
      page
    },
    .env = env
  )
  seen
}

# "Aucune donnee disponible" with its accent (kept out of the file as an escape-free ASCII source).
NO_DATA_MSG <- paste0("Aucune donn", intToUtf8(0xE9), "e disponible")

sheet_full <- function() fx_json("full", "rencontre__77779.json")

# ---------------------------------------------------------------- season mapping

test_that("season is an END year mapped to the season's competition_id without a request", {
  skip_on_cran()
  seen <- mock_ajax(fx_json(PHASES))
  for (pair in list(c(2021, 1), c(2022, 34), c(2026, 197), c(2027, 240), c("2027", 240))) {
    expect_identical(.magnus_competition_id(pair[[1]]), as.integer(pair[[2]]))
  }
  expect_identical(.magnus_competition_id(2027L), 240L)
  expect_null(.magnus_competition_id(NULL))
  expect_equal(seen$n, 0L)
  magnus_phases(2027)
  expect_equal(seen$query$competition_id, 240L)
  expect_equal(seen$query$action, "get_phases")
  expect_match(seen$url, "^https://liguemagnus.com/wp-admin/admin-ajax.php$")
  magnus_phases(2026)
  expect_equal(seen$query$competition_id, 197L)
})

test_that("a season before 2020-21 is no_data before any request", {
  skip_on_cran()
  seen <- mock_ajax(fx_json(PHASES))
  expect_error(magnus_phases(2020), class = "fastRhockey_no_data")
  expect_error(magnus_schedule(1999), class = "fastRhockey_no_data")
  expect_equal(seen$n, 0L)
})

test_that("a non-year season is a bad request before any request", {
  skip_on_cran()
  seen <- mock_ajax(fx_json(PHASES))
  for (bad in list("2026-27", TRUE, 2026.5, NA, c(2026, 2027), list(2027))) {
    expect_error(magnus_phases(bad), class = "fastRhockey_bad_request")
  }
  expect_equal(seen$n, 0L)
})

test_that("an unseeded season is looked up once per session", {
  skip_on_cran()
  old <- list(seasons = .magnus_state$seasons, live = .magnus_state$live)
  .magnus_state$live <- NULL
  tryCatch({
    body <- fx_json(COMPETITIONS)
    extra <- body$data$data[[1]]
    extra$id <- 281L
    extra$saison <- 2028L
    body$data$data[[length(body$data$data) + 1]] <- extra
    seen <- mock_ajax(body)
    expect_identical(.magnus_competition_id(2028), 281L)
    expect_identical(.magnus_competition_id(2028), 281L)
    expect_equal(seen$n, 1L)
    expect_equal(seen$query, list(action = "get_competition_by_type", id_type = 1L))
    expect_error(.magnus_competition_id(2031), class = "fastRhockey_no_data", regexp = "2031")
    expect_equal(seen$n, 1L) # the per-session lookup is not repeated
  }, finally = {
    .magnus_state$seasons <- old$seasons
    .magnus_state$live <- old$live
  })
})

# ---------------------------------------------------------------- requests

test_that("every wrapper sends its action and query (NULL values dropped), JSON unsimplified", {
  skip_on_cran()
  seen <- mock_ajax(fx_json(COMPETITIONS))
  magnus_competitions()
  expect_equal(seen$query, list(action = "get_competition_by_type", id_type = 1L))
  expect_false(seen$dots$simplifyVector)
  seen <- mock_ajax(fx_json(ROUND2))
  magnus_schedule(2027, phase_id = 714, team_id = 366, start_date = as.Date("2026-09-01"),
                  end_date = "2026-09-30", round = 1)
  expect_equal(seen$query, list(
    action = "get_rencontres", par_page = 700L, competition_id = 240L, phase_id = 714L,
    equipe_id = 366L, date_min = "2026-09-01", date_max = "2026-09-30", journee = 1L
  ))
  seen <- mock_ajax(fx_json(ROUND2))
  magnus_schedule(season = 2027)
  expect_equal(names(seen$query), c("action", "par_page", "competition_id"))
  seen <- mock_ajax(fx_json(ROUND_GAMES))
  magnus_round_games(714, 1)
  expect_equal(seen$query, list(action = "get_rencontrestours", phase_id = 714L, journee = 1L))
  seen <- mock_ajax(fx_json(ROUNDS))
  magnus_rounds(714)
  expect_equal(seen$query, list(action = "get_liste_tours", phase_id = 714L))
  seen <- mock_ajax(fx_json(STANDINGS))
  magnus_standings(714)
  expect_equal(seen$query, list(action = "get_classementsphase", phase_id = 714L))
  seen <- mock_ajax(fx_json(TEAMS))
  magnus_teams(714)
  expect_equal(seen$query, list(action = "get_equipes", phase_id = 714L))
})

test_that("the game routes fetch /rencontre/{id}/", {
  skip_on_cran()
  seen <- mock_page(fx_json(SHEETS$g77779$file))
  magnus_game(77779)
  expect_equal(seen$url, "https://liguemagnus.com/rencontre/77779/")
  magnus_pbp("77779")
  magnus_lineups(77779L)
  magnus_shootout(77779)
  expect_equal(seen$n, 4L)
})

test_that("bad arguments are a bad request before any request", {
  skip_on_cran()
  ajax <- mock_ajax(fx_json(ROUNDS))
  page <- mock_page(fx_json(SHEETS$g77779$file))
  for (bad in list("abc", 0, -1, 1.5, NA, c(1, 2), NULL, TRUE, 2^31)) {
    expect_error(magnus_game(bad), class = "fastRhockey_bad_request")
    expect_error(magnus_pbp(bad), class = "fastRhockey_bad_request")
    expect_error(magnus_lineups(bad), class = "fastRhockey_bad_request")
    expect_error(magnus_shootout(bad), class = "fastRhockey_bad_request")
  }
  for (bad in list("abc", -1, 1.5, NA, c(1, 2), NULL, TRUE)) {
    expect_error(magnus_rounds(bad), class = "fastRhockey_bad_request")
    expect_error(magnus_standings(bad), class = "fastRhockey_bad_request")
    expect_error(magnus_teams(bad), class = "fastRhockey_bad_request")
    expect_error(magnus_round_games(bad, 1), class = "fastRhockey_bad_request")
    expect_error(magnus_round_games(714, bad), class = "fastRhockey_bad_request")
  }
  for (bad in list("abc", -1, 1.5, NA, c(1, 2), TRUE)) {
    expect_error(magnus_schedule(2027, round = bad), class = "fastRhockey_bad_request")
    expect_error(magnus_schedule(2027, phase_id = bad), class = "fastRhockey_bad_request")
    expect_error(magnus_schedule(2027, team_id = bad), class = "fastRhockey_bad_request")
  }
  for (bad in list(20261011, NA, c("2026-01-01", "2026-02-01"), ""))
    expect_error(magnus_schedule(2027, start_date = bad), class = "fastRhockey_bad_request")
  expect_error(magnus_competitions("abc"), class = "fastRhockey_bad_request")
  expect_equal(ajax$n, 0L)
  expect_equal(page$n, 0L)
})

# ---------------------------------------------------------------- columns, types, parity

test_that("magnus_competitions: sdv-py columns, id types and parity", {
  skip_on_cran()
  mock_ajax(fx_json(COMPETITIONS))
  got <- magnus_competitions()
  expect_s3_class(got, "fastRhockey_data")
  expect_schema(got, "competitions")
  expect_parity(got, "competitions", "main")
  expect_identical(got$season, c(2021L, 2022L, 2023L))
  expect_type(got$competition_id, "integer")
})

test_that("magnus_phases, magnus_teams, magnus_rounds, magnus_standings: columns and parity", {
  skip_on_cran()
  mock_ajax(fx_json(PHASES))
  got <- magnus_phases(2027)
  expect_schema(got, "phases")
  expect_parity(got, "phases", "main")
  expect_identical(got$phase_id, 714L)

  mock_ajax(fx_json(TEAMS))
  got <- magnus_teams(714)
  expect_schema(got, "teams")
  expect_parity(got, "teams", "main")
  expect_type(got$team_id, "integer")
  expect_true(all(vapply(got$competition_ids, is.integer, logical(1))))

  mock_ajax(fx_json(ROUNDS))
  got <- magnus_rounds(714)
  expect_schema(got, "rounds")
  expect_parity(got, "rounds", "main")
  expect_s3_class(got$start_date, "Date") # DD/MM/YYYY text

  mock_ajax(fx_json(STANDINGS))
  got <- magnus_standings(714)
  expect_schema(got, "standings")
  expect_parity(got, "standings", "main")
  expect_match(got$last_results[!is.na(got$last_results)], "^[WLT]+$")
})

test_that("magnus_schedule: columns and parity, scores matched by team id", {
  skip_on_cran()
  for (fixture in c("round2", "round44")) {
    mock_ajax(fx_json(c(round2 = ROUND2, round44 = ROUND44)[[fixture]]))
    got <- magnus_schedule(2027, phase_id = 714)
    expect_schema(got, "schedule")
    expect_parity(got, "schedule", fixture)
  }
  # the 3-of-264-row sample is not the whole list: the wrapper refuses it, the parser reads it
  mock_ajax(fx_json(SCHEDULE))
  expect_error(magnus_schedule(2027, phase_id = 714), class = "fastRhockey_asset_fetch", regexp = "partial schedule")
  got <- .magnus_parse_schedule(fx_json(SCHEDULE))
  expect_schema(got, "schedule")
  expect_parity(got, "schedule", "main")
  # round 2 trap: games 77751 / 77753 / 77754 carry extra 0-goal score rows for
  # teams that did not play, and the score order is not home first
  mock_ajax(fx_json(ROUND2))
  got <- magnus_schedule(2027, phase_id = 714, round = 2)
  trap <- got[got$game_id %in% c(77751L, 77753L, 77754L), ]
  expect_equal(nrow(trap), 3)
  expect_false(anyNA(trap$home_score))
  expect_false(anyNA(trap$away_score))
  expect_false(anyNA(trap$home_team_participant_id))
  g53 <- got[got$game_id == 77753L, ]
  expect_equal(c(g53$home_score, g53$away_score), c(3L, 2L))
  # round 44 is unplayed: a 0-0 placeholder is NA, not 0
  mock_ajax(fx_json(ROUND44))
  got <- magnus_schedule(2027, phase_id = 714, round = 44)
  expect_true(all(got$status == "scheduled"))
  expect_true(all(is.na(got$home_score)) && all(is.na(got$away_score)))
  expect_true(all(is.na(got$decision)))
})

test_that("magnus_round_games: UTC round times land as Europe/Paris, no decision", {
  skip_on_cran()
  body <- fx_json(ROUND_GAMES)
  expect_match(body$data$data$rencontres[[1]]$date_rencontre, "Z$") # rencontrestours dates are UTC
  mock_ajax(body)
  got <- magnus_round_games(714, 1)
  expect_schema(got, "round_games")
  expect_parity(got, "round_games", "main")
  expect_identical(attr(got$game_datetime, "tzone"), "Europe/Paris")
  expect_equal(format(got$game_datetime[[1]], "%H:%M", tz = "Europe/Paris"), "20:00") # 18:00Z in September
  expect_true(all(is.na(got$decision)))
  expect_false(anyNA(got$season))
  expect_false(anyNA(got$phase_id))
  expect_false(anyNA(got$round))
})

test_that("game routes: columns and parity on all four sheets", {
  skip_on_cran()
  for (key in names(SHEETS)) {
    s <- SHEETS[[key]]
    mock_page(fx_json(s$file))
    for (route in c("game", "pbp", "lineups", "shootout")) {
      got <- get(paste0("magnus_", route))(s$id)
      expect_schema(got, route)
      expect_parity(got, route, key)
    }
  }
})

test_that("game routes on the full sheet: counts, ids and the string ids", {
  skip_on_cran()
  mock_page(sheet_full())
  game <- magnus_game(77779)
  expect_equal(nrow(game), 1)
  expect_identical(game$game_id, 77779L)
  expect_true(game$has_shootout)
  expect_identical(attr(game$game_datetime, "tzone"), "Europe/Paris")
  pbp <- magnus_pbp(77779)
  expect_equal(nrow(pbp), 159)
  expect_type(pbp$event_id, "character") # BUT99238
  expect_true(all(pbp$game_id == 77779L))
  expect_true(all(pbp$period >= 1L, na.rm = TRUE))
  lineups <- magnus_lineups(77779)
  expect_equal(nrow(lineups), 41)
  expect_type(lineups$licence_number, "character") # leading zeros survive
  expect_type(lineups$jersey_number, "character")
  expect_type(lineups$player_id, "integer")
  expect_true(any(lineups$is_mvp))
  expect_true(all(lineups$home_away %in% c("home", "away")))
  shootout <- magnus_shootout(77779)
  expect_equal(nrow(shootout), 10)
  expect_identical(shootout$shootout_round, sort(shootout$shootout_round))
})

test_that("a game without a shootout is zero rows with the documented columns", {
  skip_on_cran()
  mock_page(fx_json(SHEETS$g68838$file))
  got <- magnus_shootout(68838)
  expect_equal(nrow(got), 0)
  expect_schema(got, "shootout")
})

test_that("a period boundary: a faceoff at 1200 opens period 2, any other event closes period 1", {
  skip_on_cran()
  sheet <- list(
    scores = list(list(sportif_rencontre_id = 5L)),
    evenements = list(
      list(id = "ENGA9", type = "ENGA", temps = 1200L),
      list(id = "TIR3", type = "TIR", temps = 1200L),
      list(id = "BUT1", type = "BUT", temps = 1200L),
      list(id = "ENGA1", type = "ENGA", temps = 0L)
    )
  )
  got <- .magnus_parse_pbp(sheet)
  expect_identical(got$event_type_raw, c("ENGA", "TIR", "BUT", "ENGA"))
  expect_identical(got$period, c(1L, 1L, 1L, 2L))
  expect_identical(got$period_seconds, c(0L, 1200L, 1200L, 0L))
})

# ---------------------------------------------------------------- zero-row and malformed payloads

test_that("parsers return the documented zero-row frame on an empty or malformed payload", {
  skip_on_cran()
  empties <- list(list(), NULL, "x", list(success = TRUE, data = list(data = list())), list(data = list()))
  for (e in empties) {
    expect_schema(.magnus_parse_competitions(e), "competitions")
    expect_schema(.magnus_parse_phases(e), "phases")
    expect_schema(.magnus_parse_teams(e), "teams")
    expect_schema(.magnus_parse_schedule(e), "schedule")
    expect_schema(.magnus_parse_rounds(e), "rounds")
    expect_schema(.magnus_parse_standings(e), "standings")
    expect_schema(.magnus_parse_game(e), "game")
    expect_schema(.magnus_parse_pbp(e), "pbp")
    expect_schema(.magnus_parse_lineups(e), "lineups")
    expect_schema(.magnus_parse_shootout(e), "shootout")
    expect_equal(nrow(.magnus_parse_schedule(e)), 0)
  }
  # an envelope or an error object is not a game
  expect_equal(nrow(.magnus_parse_game(fx_json(PHASES))), 0)
  expect_equal(nrow(.magnus_parse_game(list(message = "Rencontre introuvable"))), 0)
  # a leaf of the wrong type is NA, never an error
  odd <- .magnus_parse_competitions(list(data = list(list(id = "x", saison = 2.5, libelle = TRUE,
                                                          type = list(id = list()), parametres = list(publier_sur_site = "oui")))))
  expect_equal(nrow(odd), 1)
  expect_true(all(is.na(odd)))
  # a digit-string id is an integer, never "123.0"
  expect_identical(.magnus_parse_phases(list(data = list(list(id = "714"))))$phase_id, 714L)
})

# ---------------------------------------------------------------- admin-ajax refusals

test_that("error envelopes are refused with the right class", {
  skip_on_cran()
  cases <- list(
    list(list(success = FALSE, data = NO_DATA_MSG), "fastRhockey_no_data"),
    list(list(success = FALSE, data = "  AUCUNE   DONNEE\tdisponible. "), "fastRhockey_no_data"),
    list(list(success = FALSE, data = paste(tolower(NO_DATA_MSG), "pour cette phase")), "fastRhockey_no_data"),
    list(list(success = FALSE, data = paste0("Aucune donn", intToUtf8(c(0x65, 0x301)), "e disponible")), "fastRhockey_no_data"),
    list(list(success = FALSE, data = "Erreur interne du serveur"), "fastRhockey_asset_fetch"),
    list(list(success = FALSE, data = list(code = 500L)), "fastRhockey_asset_fetch"),
    list(list(success = FALSE, data = ""), "fastRhockey_asset_fetch"),
    list(list(success = TRUE, data = ""), "fastRhockey_no_data"),
    list(list(success = TRUE, data = NULL), "fastRhockey_no_data"),
    list(list(success = TRUE, data = list()), "fastRhockey_no_data"),
    list(list(success = TRUE, data = list(competition_id = list("Le champ competition id est invalide."))),
         "fastRhockey_bad_request"),
    list(list(success = TRUE, data = list(data = list(), time = "1")), "fastRhockey_no_data"),
    list(list(success = TRUE, data = list(data = list(1L, 2L), time = "1")), "fastRhockey_asset_fetch"),
    list(list(success = TRUE, data = list(time = "1")), "fastRhockey_asset_fetch"),
    list(list(success = TRUE, data = list(data = list(list(id = 1L)), time = "1")), NA),
    list(list(data = list(data = list(list(id = 1L)))), "fastRhockey_asset_fetch"),
    list(list(success = "true", data = list(data = list(list(id = 1L)))), "fastRhockey_asset_fetch"),
    list(list(list(id = 1L)), "fastRhockey_asset_fetch")
  )
  for (case in cases) {
    mock_ajax(case[[1]])
    if (is.na(case[[2]])) {
      expect_no_error(magnus_phases(2027, return_parsed = FALSE))
    } else {
      expect_error(magnus_phases(2027), class = case[[2]])
    }
  }
})

test_that("success=false 'no data' is matched ignoring accents, case and whitespace only", {
  skip_on_cran()
  expect_true(.magnus_is_no_data_message(NO_DATA_MSG))
  expect_true(.magnus_is_no_data_message(paste0(" AUCUNE", intToUtf8(0xA0), "DONN", intToUtf8(0xC9), "E   DISPONIBLE ")))
  expect_false(.magnus_is_no_data_message("Aucune donnee"))
  expect_false(.magnus_is_no_data_message(list("Aucune donnee disponible")))
  expect_false(.magnus_is_no_data_message(NULL))
})

test_that("rows outside the requested scope are refused, a missing scope field too", {
  skip_on_cran()
  body <- fx_json(ROUND2)
  body$data$data[[1]]$phase_id <- 560L
  mock_ajax(body)
  expect_error(magnus_schedule(2027, phase_id = 714), class = "fastRhockey_asset_fetch", regexp = "outside phase_id=714")

  body <- fx_json(ROUND2)
  body$data$data[[3]]$competition$id <- 197L
  mock_ajax(body)
  expect_error(magnus_schedule(2027), class = "fastRhockey_asset_fetch", regexp = "outside competition_id=240")

  body <- fx_json(ROUND2)
  body$data$data[[1]]$phase_id <- NULL # refused, never skipped
  mock_ajax(body)
  expect_error(magnus_schedule(2027, phase_id = 714), class = "fastRhockey_asset_fetch", regexp = "outside phase_id")

  mock_ajax(fx_json(COMPETITIONS))
  expect_error(magnus_competitions(competition_type = 2), class = "fastRhockey_asset_fetch", regexp = "outside id_type=2")

  mock_ajax(fx_json(ROUNDS))
  expect_error(magnus_rounds(560), class = "fastRhockey_asset_fetch", regexp = "outside phase_id=560")
})

test_that("a round of another number or phase is refused", {
  skip_on_cran()
  mock_ajax(fx_json(ROUND_GAMES))
  expect_no_error(magnus_round_games(714, 1))
  expect_error(magnus_round_games(714, 2), class = "fastRhockey_asset_fetch", regexp = "phase/round")
  expect_error(magnus_round_games(560, 1), class = "fastRhockey_asset_fetch", regexp = "phase/round")
})

test_that("the schedule must be the whole list: paging meta is required and type-strict", {
  skip_on_cran()
  body <- fx_json(ROUND2)
  expect_identical(body$data$meta$last_page, 1L)
  expect_identical(body$data$meta$total, 6L)
  mock_ajax(body)
  expect_equal(nrow(magnus_schedule(2027)), 6)

  body$data$meta <- NULL
  mock_ajax(body)
  expect_error(magnus_schedule(2027), class = "fastRhockey_asset_fetch", regexp = "no paging meta")

  partial <- list(
    list(last_page = 14L, total = 264L, current_page = 1L),
    list(last_page = 2L, total = 6L), # rows match the total, but it is not the only page
    list(last_page = 1L, total = 7L), # one page that does not hold every game
    list(last_page = TRUE, total = 6L), # a logical is not a page count
    list(last_page = "1", total = 6L),
    list(last_page = 1, total = 6L), # a float is not a page count
    list(last_page = 1L, total = "6"),
    list(last_page = 1L, total = NULL)
  )
  for (meta in partial) {
    body <- fx_json(ROUND2)
    body$data$meta <- c(meta[!vapply(meta, is.null, logical(1))], body$data$meta[setdiff(names(body$data$meta), names(meta))])
    mock_ajax(body)
    expect_error(magnus_schedule(2027), class = "fastRhockey_asset_fetch", regexp = "partial schedule")
  }
})

# ---------------------------------------------------------------- game sheet page

test_that("the game sheet is extracted from its own element, ahead of a decoy :data", {
  skip_on_cran()
  page <- game_page(sheet_full())
  sheet <- .magnus_extract_sheet(page)
  expect_length(sheet$evenements, 159)
  expect_null(.magnus_extract_sheet("<html><div :data=\"{&quot;x&quot;: 1}\"></div></html>"))
  expect_null(.magnus_extract_sheet(NA_character_))
  # a :data that is not a JSON object is never handed to jsonlite (it could be a path or URL)
  expect_null(.magnus_extract_sheet("<live-rencontre-container :data=\"https://example.test/x.json\"></live-rencontre-container>"))
})

test_that("html entities in the attribute are unescaped", {
  skip_on_cran()
  sheet <- sheet_full()
  rink <- paste0("L'", intToUtf8(0xCE), "le & \"Glace\" <b>")
  sheet$patinoire <- rink
  mock_page(sheet)
  expect_identical(magnus_game(77779, return_parsed = FALSE)$patinoire, rink)
})

test_that("staff birth dates are never returned", {
  skip_on_cran()
  sheet <- sheet_full()
  expect_true(all(vapply(sheet$staffs, function(s) "ddn" %in% names(s), logical(1))))
  mock_page(sheet)
  out <- magnus_pbp(77779, return_parsed = FALSE)
  expect_true(length(out$staffs) > 0)
  expect_false(any(vapply(out$staffs, function(s) "ddn" %in% names(s), logical(1))))
})

test_that("a page without the game element cannot be read", {
  skip_on_cran()
  mock_page(html = "<html><div :data=\"{&quot;x&quot;: 1}\"></div></html>")
  expect_error(magnus_game(77779), class = "fastRhockey_asset_fetch", regexp = "no <live-rencontre-container")
})

test_that("an unknown game id and an unplayed game are no_data", {
  skip_on_cran()
  mock_page(list(message = "Rencontre introuvable"), decoy = FALSE)
  expect_error(magnus_game(77779), class = "fastRhockey_no_data", regexp = "no game sheet")
  sheet <- sheet_full()
  sheet$evenements <- list()
  mock_page(sheet)
  expect_error(magnus_pbp(77779), class = "fastRhockey_no_data", regexp = "unplayed")
  sheet$evenements <- NULL
  mock_page(sheet)
  expect_error(magnus_pbp(77779), class = "fastRhockey_no_data")
  sheet <- sheet_full()
  sheet$evenements <- "oops"
  mock_page(sheet)
  expect_error(magnus_pbp(77779), class = "fastRhockey_asset_fetch", regexp = "not a list")
})

test_that("a sheet for another game is refused", {
  skip_on_cran()
  for (ids in list(c(77779L, 77780L), c(77780L, 77780L), integer())) {
    sheet <- sheet_full()
    template <- sheet$scores[[1]]
    sheet$scores <- lapply(ids, function(i) {
      template$sportif_rencontre_id <- i
      template
    })
    mock_page(sheet)
    expect_error(magnus_game(77779), class = "fastRhockey_asset_fetch", regexp = "asked 77779")
  }
  mock_page(sheet_full())
  expect_error(magnus_game(77781), class = "fastRhockey_asset_fetch", regexp = "asked 77781")
})

test_that("return_parsed = FALSE hands back the raw envelope or sheet", {
  skip_on_cran()
  mock_ajax(fx_json(ROUNDS))
  raw <- magnus_rounds(714, return_parsed = FALSE)
  expect_true(raw$success)
  expect_false(inherits(raw, "tbl_df"))
  mock_page(sheet_full())
  expect_length(magnus_game(77779, return_parsed = FALSE)$evenements, 159)
})

# ---------------------------------------------------------------- transport (the shared runtime)

json_resp <- function(body, status = 200L, url = "https://liguemagnus.com/wp-admin/admin-ajax.php",
                      type = "application/json") {
  httr2::response(status_code = status, url = url, headers = list(`Content-Type` = type),
                  body = if (length(body)) charToRaw(body) else raw())
}

# Count the host-throttle slots taken instead of sleeping.
count_waits <- function(env = parent.frame()) {
  seen <- new.env()
  seen$urls <- character()
  testthat::local_mocked_bindings(
    .polite_wait = function(url, min_interval = 1) {
      seen$urls <- c(seen$urls, url)
      invisible(0)
    },
    .env = env
  )
  seen
}

test_that("every request, ajax and game page, waits for the host throttle", {
  skip_on_cran()
  waits <- count_waits()
  sheet_html <- game_page(sheet_full())
  httr2::local_mocked_responses(function(req) {
    if (grepl("rencontre", req$url, fixed = TRUE)) {
      json_resp(sheet_html, url = req$url, type = "text/html")
    } else {
      json_resp(paste(readLines(fx(PHASES), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), url = req$url)
    }
  })
  expect_equal(nrow(magnus_phases(2027)), 1)
  expect_equal(nrow(magnus_game(77779)), 1)
  expect_equal(waits$urls, c(
    "https://liguemagnus.com/wp-admin/admin-ajax.php?action=get_phases&competition_id=240",
    "https://liguemagnus.com/rencontre/77779/"
  ))
})

test_that("transport errors map to the package vocabulary (ajax and game page)", {
  skip_on_cran()
  count_waits()
  answer <- NULL
  httr2::local_mocked_responses(function(req) {
    json_resp(answer$body, answer$status, req$url, answer$type %||% "application/json")
  })
  cases <- list(
    list(status = 404L, body = "", cls = "fastRhockey_no_data"),
    list(status = 400L, body = "0", type = "text/html", cls = "fastRhockey_bad_request"), # an unknown action
    list(status = 422L, body = "{}", cls = "fastRhockey_bad_request"),
    list(status = 500L, body = "oops", cls = "fastRhockey_asset_fetch"),
    list(status = 403L, body = "Just a moment...", type = "text/html", cls = "fastRhockey_asset_fetch"),
    list(status = 200L, body = "<html>challenge</html>", type = "text/html", cls = "fastRhockey_asset_fetch")
  )
  for (case in cases) {
    answer <- case
    expect_error(magnus_phases(2027), class = case$cls)
    expect_error(magnus_game(77779), class = case$cls)
  }
  # a 2xx game page that is empty is a block, not "nothing"
  answer <- list(status = 200L, body = "", type = "text/html")
  expect_error(magnus_game(77779), class = "fastRhockey_asset_fetch", regexp = "empty body")
})

test_that("an empty 200 ajax body is asset_fetch", {
  skip_on_cran()
  count_waits()
  httr2::local_mocked_responses(function(req) json_resp("", 200L, req$url))
  expect_error(magnus_phases(2027), class = "fastRhockey_asset_fetch", regexp = "empty body")
})

# ---------------------------------------------------------------- live (one request per route, nine in all)

test_that("live: the admin-ajax routes answer with the documented columns", {
  skip_on_cran()
  skip_euro_test()
  phases <- magnus_phases(2027)
  expect_schema(phases, "phases")
  pid <- phases$phase_id[[1]]
  expect_schema(magnus_competitions(), "competitions")
  expect_schema(magnus_teams(pid), "teams")
  expect_schema(magnus_rounds(pid), "rounds")
  expect_schema(magnus_standings(pid), "standings")
  expect_schema(magnus_round_games(pid, 1), "round_games")
  schedule <- magnus_schedule(2027, phase_id = pid, round = 2)
  expect_schema(schedule, "schedule")
  expect_equal(nrow(schedule), 6)
})

test_that("live: the game sheet routes answer with the documented columns", {
  skip_on_cran()
  skip_euro_test()
  expect_schema(magnus_game(77779), "game")
  pbp <- magnus_pbp(77779)
  expect_schema(pbp, "pbp")
  expect_equal(nrow(pbp), 159)
})
