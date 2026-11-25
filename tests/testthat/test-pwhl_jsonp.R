# pwhl_game_info(), pwhl_player_box(), pwhl_team_roster() and pwhl_teams()
# stripped the statviewfeed JSONP with their own regex on the callback tail
# ("}}})", "}]})", ...). They now go through .pwhl_api() -> .hockeytech_api().
#
# fixtures/pwhl_jsonp/statviewfeed_old_output.rds is the output of the old
# wrappers (origin/main 0e7e305b8) on the recorded replies in fixtures/http,
# with the fastRhockey_timestamp attribute dropped.

drop_timestamp <- function(x) {
  if (is.data.frame(x)) attr(x, "fastRhockey_timestamp") <- NULL
  if (is.list(x) && !is.data.frame(x)) x <- lapply(x, drop_timestamp)
  x
}

run_statviewfeed_wrappers <- function() {
  suppressMessages(list(
    pwhl_game_info = pwhl_game_info(game_id = 27),
    pwhl_player_box = pwhl_player_box(game_id = 27),
    pwhl_team_roster = pwhl_team_roster(season = 2023, team = "Toronto"),
    pwhl_teams = pwhl_teams()
  ))
}

old_output <- function() {
  readRDS(testthat::test_path("fixtures", "pwhl_jsonp", "statviewfeed_old_output.rds"))
}

test_that("PWHL statviewfeed wrappers give the same output as before on recorded replies", {
  skip_on_cran()
  local_http_fixtures()
  new <- lapply(run_statviewfeed_wrappers(), drop_timestamp)
  expect_identical(new, old_output())
})

test_that("PWHL statviewfeed wrappers parse a reply whose JSONP tail differs", {
  skip_on_cran()
  local_http_fixtures() # skips without the recorded replies
  index <- http_fixture_index()
  dir <- testthat::test_path("fixtures", "http")
  # The same recorded replies, each ending in an extra key, so the tail before
  # ")" is `[]}` rather than the `}}}` / `}]}` / `}}]}]}]}` the old regex
  # expected. The wrappers ignore the extra key.
  httr2::local_mocked_responses(function(req) {
    hit <- index[[req$url]]
    if (is.null(hit)) return(httr2::response(status_code = 404L, url = req$url))
    path <- file.path(dir, hit$file)
    body <- rawToChar(memDecompress(readBin(path, "raw", file.size(path)), "gzip"))
    Encoding(body) <- "UTF-8"
    if (grepl("feed=statviewfeed", req$url, fixed = TRUE)) {
      body <- sub("\\}\\)\\s*$", ",\"zz_tail\":[]})", body)
    }
    httr2::response(
      status_code = hit$status, url = req$url,
      headers = list(`Content-Type` = hit$type), body = charToRaw(enc2utf8(body))
    )
  })
  new <- lapply(run_statviewfeed_wrappers(), drop_timestamp)
  expect_identical(new, old_output())
})
