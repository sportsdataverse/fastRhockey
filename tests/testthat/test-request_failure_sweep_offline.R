# Offline sweep of the failure contract: every NHL / ESPN NHL / Fox / PWHL / AHL /
# OHL / WHL / QMJHL wrapper is called with placeholder arguments while every httr2
# request either fails outright or warns. NHL and PWHL wrappers message and return
# NULL, ESPN, Fox and the HockeyTech leagues return an empty frame: none may
# error. A few wrappers let the failure propagate on purpose (listed below).

sweep_fns <- function() {
  fns <- getNamespaceExports("fastRhockey")
  sort(fns[grepl("^(espn_nhl|nhl|pwhl|fox|ahl|ohl|whl|qmjhl)_", fns)])
}

placeholder_args <- function(f) {
  fm <- formals(getExportedValue("fastRhockey", f))
  req <- setdiff(names(fm)[vapply(fm, function(x) identical(x, quote(expr = )), TRUE)], "...")
  val <- function(nm) {
    if (grepl("season", nm)) return(2025)
    if (grepl("date|day", nm)) return("2025-01-10")
    if (grepl("abbr", nm)) return("TOR")
    if (grepl("id$|_ids?$|^id", nm)) return(1)
    if (grepl("name|term|key|handle|category|sort|position|attribute|endpoint|type|report", nm)) return("x")
    1
  }
  stats::setNames(lapply(req, val), req)
}

sweep <- function(responder, skip = character()) {
  for (f in setdiff(sweep_fns(), skip)) {
    out <- httr2::with_mocked_responses(responder, tryCatch(
      suppressMessages(suppressWarnings(do.call(f, placeholder_args(f)))),
      error = function(e) e
    ))
    expect_false(inherits(out, "error"), label = paste0(f, "() erred: ",
      if (inherits(out, "error")) conditionMessage(out) else ""))
  }
}

test_that("wrappers survive a failed request", {
  skip_on_cran()
  # These let the connection error propagate (the pbp/boxscore/shift builders and
  # the PWHL season/team lookups they depend on).
  propagate <- c("nhl_game_boxscore", "nhl_game_feed", "nhl_game_pbp", "nhl_game_shifts",
                 "pwhl_game_info", "pwhl_standings", "pwhl_team_roster", "pwhl_teams")
  sweep(function(req) stop("simulated connection failure"), skip = propagate)
})

test_that("wrappers survive a request that warns and returns an empty object", {
  skip_on_cran()
  sweep(function(req) {
    warning("simulated warning")
    httr2::response(status_code = 200L, url = req$url,
                    headers = list(`Content-Type` = "application/json"),
                    body = charToRaw("{}"))
  }, skip = c("pwhl_game_info", "pwhl_standings", "pwhl_team_roster"))
})
