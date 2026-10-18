# F1: mch/wch argument parity. Every call is answered by a mocked getter that
# records (url, params); no network.

json_body <- function(body = "{}", status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

capture_req <- function(expr_fn) {
  seen <- new.env()
  seen$urls <- character()
  seen$params <- list()
  testthat::local_mocked_bindings(
    .retry_request = function(url, params = list(), ...) {
      seen$urls <- c(seen$urls, url)
      seen$params[[length(seen$params) + 1L]] <- params
      json_body()
    },
    .package = "fastRhockey"
  )
  suppressWarnings(suppressMessages(try(expr_fn(), silent = TRUE)))
  list(url = seen$urls[1], params = seen$params[[1]])
}

SITE <- "https://site.api.espn.com/apis/site/v2/sports/hockey"
SITE_ALT <- "https://site.api.espn.com/apis/v2/sports/hockey"
CORE <- "https://sports.core.api.espn.com/v2/sports/hockey/leagues"
WEB <- "https://site.web.api.espn.com/apis/common/v3/sports/hockey"

test_that("NHL requests are unchanged by the new helper args (URL + query)", {
  chk <- function(r, url, params) {
    expect_identical(r$url, url)
    expect_identical(r$params, params)
  }
  chk(capture_req(function() espn_nhl_scoreboard()), paste0(SITE, "/nhl/scoreboard"), list())
  chk(capture_req(function() espn_nhl_scoreboard(dates = "20250110")),
      paste0(SITE, "/nhl/scoreboard"), list(dates = "20250110"))
  chk(capture_req(function() espn_nhl_standings(season = 2025)),
      paste0(SITE_ALT, "/nhl/standings"), list(season = 2025))
  chk(capture_req(function() espn_nhl_team_roster(team_id = 10)),
      paste0(SITE, "/nhl/teams/10/roster"), list())
  chk(capture_req(function() espn_nhl_teams()), paste0(SITE, "/nhl/teams"), list())
  chk(capture_req(function() espn_nhl_transactions()), paste0(SITE, "/nhl/transactions"), list())
  chk(capture_req(function() espn_nhl_season_awards(season = 2025)),
      paste0(CORE, "/nhl/seasons/2025/awards"), list())
  chk(capture_req(function() espn_nhl_coach_record(coach_id = 7)),
      paste0(CORE, "/nhl/coaches/7/record"), list())
  chk(capture_req(function() espn_nhl_leaders(limit = 25)),
      paste0(WEB, "/nhl/statistics/byathlete"), list(limit = 25, page = 1L))
})

test_that("mch/wch wrappers with no new args hit the expected URLs", {
  for (lg in c(mch = "mens-college-hockey", wch = "womens-college-hockey")) {
    pre <- if (lg == "mens-college-hockey") "espn_mch_" else "espn_wch_"
    r <- capture_req(function() get(paste0(pre, "scoreboard"))())
    expect_identical(r$url, paste0(SITE, "/", lg, "/scoreboard"))
    expect_identical(r$params, list(limit = 500))  # sdv-py default
    for (rt in list(c("teams_site", 1000), c("transactions", 500), c("season_awards", 200))) {
      r <- capture_req(function() get(paste0(pre, rt[[1]]))())
      expect_identical(r$params$limit, as.numeric(rt[[2]]))
    }
    r <- capture_req(function() get(paste0(pre, "team_roster"))(team_id = 10))
    expect_identical(r$params$limit, 500)
    # F2: college coach_record defaults to sdv-py's record/0
    r <- capture_req(function() get(paste0(pre, "coach_record"))(coach_id = 7))
    expect_identical(r$url, paste0(CORE, "/", lg, "/coaches/7/record/0"))
    r <- capture_req(function() get(paste0(pre, "coach_record"))(coach_id = 7, record_type = NULL))
    expect_identical(r$url, paste0(CORE, "/", lg, "/coaches/7/record"))
  }
})

test_that("new mch/wch args reach the ESPN query / path", {
  for (lg in c(mch = "mens-college-hockey", wch = "womens-college-hockey")) {
    pre <- if (lg == "mens-college-hockey") "espn_mch_" else "espn_wch_"
    f <- function(n) get(paste0(pre, n))

    r <- capture_req(function() f("scoreboard")(dates = "20250110", week = 3,
                                                season_type = 2, groups = 50, limit = 100))
    expect_identical(r$url, paste0(SITE, "/", lg, "/scoreboard"))
    expect_identical(r$params, list(dates = "20250110", week = 3, seasontype = 2,
                                    groups = 50, limit = 100))

    r <- capture_req(function() f("standings")(season = 2025, group = 80,
                                               standings_type = "by-division"))
    expect_identical(r$url, paste0(SITE_ALT, "/", lg, "/standings"))
    expect_identical(r$params, list(season = 2025, group = 80, type = "by-division"))

    r <- capture_req(function() f("leaders")(limit = 10, sort = "offensive.goals:desc"))
    expect_identical(r$url, paste0(WEB, "/", lg, "/statistics/byathlete"))
    expect_identical(r$params, list(limit = 10, page = 1L, sort = "offensive.goals:desc"))

    r <- capture_req(function() f("coach_record")(coach_id = 7, record_type = 0))
    expect_identical(r$url, paste0(CORE, "/", lg, "/coaches/7/record/0"))

    r <- capture_req(function() f("teams_site")(limit = 99))
    expect_identical(r$url, paste0(SITE, "/", lg, "/teams"))
    expect_identical(r$params, list(limit = 99))

    r <- capture_req(function() f("team_roster")(team_id = 10, limit = 60))
    expect_identical(r$url, paste0(SITE, "/", lg, "/teams/10/roster"))
    expect_identical(r$params, list(limit = 60))

    r <- capture_req(function() f("transactions")(limit = 20))
    expect_identical(r$url, paste0(SITE, "/", lg, "/transactions"))
    expect_identical(r$params, list(limit = 20))

    r <- capture_req(function() f("season_awards")(season = 2025, limit = 5))
    expect_identical(r$url, paste0(CORE, "/", lg, "/seasons/2025/awards"))
    expect_identical(r$params, list(limit = 5))
  }
})
