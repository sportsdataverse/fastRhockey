# A slow release download used to hit R's default 60 s timeout, and the
# loader returned the other seasons with only a vague warning.

# A reader that fails `fails` times, then returns `value`; records each call.
flaky <- function(fails, value = data.frame(x = 1)) {
  calls <- 0L
  timeouts <- numeric()
  f <- function(url) {
    calls <<- calls + 1L
    timeouts <<- c(timeouts, getOption("timeout"))
    if (calls <= fails) stop("Timeout of 60 seconds was reached")
    value
  }
  list(read = f, calls = function() calls, timeouts = function() timeouts)
}

test_that("a transient failure is retried with a raised timeout, then restored", {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  r <- flaky(2)
  expect_no_warning(out <- .read_release_asset("https://x/a_2024.rds", r$read))
  expect_equal(out, data.frame(x = 1))
  expect_equal(r$calls(), 3L)
  expect_equal(r$timeouts(), rep(150, 3))
  expect_equal(getOption("timeout"), 60)
})

test_that("a larger user timeout is kept", {
  old <- options(timeout = 3600)
  on.exit(options(old), add = TRUE)
  r <- flaky(0)
  .read_release_asset("https://x/a_2024.rds", r$read)
  expect_equal(r$timeouts(), 3600)
  expect_equal(getOption("timeout"), 3600)
})

test_that("a persistent failure warns with the url and returns a try-error", {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  r <- flaky(Inf)
  expect_warning(
    out <- .read_release_asset("https://x/a_2024.rds", r$read),
    "a_2024.rds.*incomplete"
  )
  expect_s3_class(out, "try-error")
  expect_equal(r$calls(), 3L)
  expect_equal(getOption("timeout"), 60)
})

test_that("a 404 is not retried", {
  calls <- 0L
  read <- function(url) {
    calls <<- calls + 1L
    warning("cannot open URL 'u': HTTP status was '404 Not Found'")
    stop("cannot open the connection")
  }
  expect_warning(.read_release_asset("https://x/a_1800.rds", read), "a_1800.rds")
  expect_equal(calls, 1L)
})

test_that("warnings from a read that succeeds are passed on", {
  read <- function(url) {
    warning("a note from the reader")
    1
  }
  expect_warning(out <- .read_release_asset("https://x/a_2024.rds", read), "a note from the reader")
  expect_equal(out, 1)
})

test_that("a loader keeps the seasons that load and names the one that failed", {
  old <- options(timeout = 60)
  on.exit(options(old), add = TRUE)
  calls <- 0L
  local_mocked_bindings(.read_rds_url = function(url) {
    calls <<- calls + 1L
    if (endsWith(url, "_2024.rds")) stop("Timeout of 60 seconds was reached")
    data.frame(season = 2023L, game_id = 1L)
  })
  expect_warning(
    out <- load_nhl_pbp(2023:2024),
    "play_by_play_2024.rds", fixed = TRUE
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$season, 2023L)
  expect_setequal(names(out), c("season", "game_id"))
  expect_equal(calls, 4L)
  expect_s3_class(out, "fastRhockey_data")
  expect_s3_class(out, "data.frame")
  expect_false("data.table" %in% class(out))
  expect_equal(getOption("timeout"), 60)
})

test_that("rds_from_url reads a local file", {
  src <- tempfile(fileext = ".rds")
  on.exit(unlink(src), add = TRUE)
  saveRDS(data.frame(x = 1:2), src, compress = FALSE)
  path <- normalizePath(src, winslash = "/")
  url <- paste0("file://", if (!startsWith(path, "/")) "/", path)
  expect_equal(.read_rds_url(url), data.frame(x = 1:2))
  expect_equal(rds_from_url(url)$x, 1:2)
})

test_that("a group loader retries a failed csv read", {
  calls <- 0L
  local_mocked_bindings(csv_from_url = function(input, ...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("Timeout of 150 seconds was reached")
    data.table::fread(text = "league,group_id,level,first_season,last_season,notes
nhl,nhl:eastern,conference,1993,,x", ...)
  })
  expect_no_warning(x <- load_nhl_groups())
  expect_equal(calls, 2L)
  expect_equal(nrow(x), 1L)
  expect_s3_class(x, "fastRhockey_data")
})

test_that("a group loader warns with the url when the csv keeps failing", {
  local_mocked_bindings(csv_from_url = function(input, ...) stop("Timeout of 150 seconds was reached"))
  expect_warning(x <- load_nhl_groups(), "nhl_groups.csv", fixed = TRUE)
  expect_equal(nrow(x), 0L)
})

# Stand-in for utils::download.file(): records the url and method, writes `body`.
fake_download <- function(body = "season,game_id\n2024,1\n", status = 0L) {
  seen <- list()
  f <- function(url, destfile, method, ...) {
    seen[[length(seen) + 1L]] <<- list(url = url, method = method)
    writeLines(body, destfile)
    status
  }
  list(download = f, seen = function() seen)
}

test_that("the games csv read is retried and passes its arguments on", {
  withr::local_options(timeout = 60, download.file.method = "curl")
  dl <- fake_download()
  local_mocked_bindings(download.file = dl$download, .package = "utils")
  calls <- 0L
  local_mocked_bindings(csv_from_url = function(file, ...) {
    calls <<- calls + 1L
    expect_equal(getOption("timeout"), 150)
    expect_true(file.exists(file))
    if (calls == 1L) stop("Timeout of 60 seconds was reached")
    data.table::data.table(file = file, ...)
  })
  expect_no_warning(out <- load_phf_games())
  expect_equal(calls, 2L)
  expect_match(dl$seen()[[2]]$url, "phf_games_in_data_repo.csv", fixed = TRUE)
  # libcurl even when the user's default method is curl; the temp file is removed.
  expect_equal(dl$seen()[[2]]$method, "libcurl")
  expect_false(file.exists(out$file))
  expect_equal(.release_csv_from_url("https://x/a.csv", sep = ",")$sep, ",")
  expect_equal(getOption("timeout"), 60)
})

test_that("a games csv that keeps failing warns with the url, then raises", {
  local_mocked_bindings(download.file = fake_download()$download, .package = "utils")
  calls <- 0L
  local_mocked_bindings(csv_from_url = function(file, ...) {
    calls <<- calls + 1L
    stop("Timeout of 150 seconds was reached")
  })
  expect_error(
    expect_warning(load_phf_games(), "phf_games_in_data_repo.csv", fixed = TRUE),
    "Timeout of 150 seconds"
  )
  expect_equal(calls, 3L)
})

test_that("a non-zero download status is a failed read", {
  local_mocked_bindings(download.file = fake_download(status = 22L)$download, .package = "utils")
  local_mocked_bindings(csv_from_url = function(...) stop("not reached"))
  expect_error(
    suppressWarnings(.release_csv_from_url("https://x/a.csv")),
    "Download failed (status 22)", fixed = TRUE
  )
})

test_that("a strict read turns a partial-read warning into an error", {
  local_mocked_bindings(download.file = fake_download()$download, .package = "utils")
  calls <- 0L
  local_mocked_bindings(csv_from_url = function(...) {
    calls <<- calls + 1L
    warning("Stopped early on line 3. Expected 2 fields but found 3.")
    data.table::data.table(season = 2024L, game_id = 1L)
  })
  # Not strict: the warning is passed on with the partial table, as before.
  expect_warning(out <- .release_csv_from_url("https://x/a.csv"), "Stopped early")
  expect_equal(nrow(out), 1L)
  expect_error(
    suppressWarnings(.release_csv_from_url("https://x/a.csv", strict = TRUE)),
    "fread() warned, so the table may be partial: Stopped early", fixed = TRUE
  )
  # The partial read is not retried: one read for each of the two calls.
  expect_equal(calls, 2L)
})

test_that("a strict partial read leaves fread() clean for the next call", {
  # real fread() on a malformed file: one row has an extra field
  local_mocked_bindings(.download_url = function(url, dest, ...) {
    writeLines(c("season,game_id", "2024,1", "2025,2,extra", "2026,3"), dest)
  })
  # only the warning naming the url: fread()'s own warning is muffled
  expect_error(
    expect_warning(
      .release_csv_from_url("https://x/games.csv", strict = TRUE),
      "Failed to read <https://x/games.csv> after 1 attempt", fixed = TRUE
    ),
    "may be partial", class = "release_csv_partial"
  )
  # fread() finished its own cleanup, so the next read is clean
  expect_no_warning(data.table::fread(text = "a,b\n1,2"))
})

test_that("a release csv is read from a local file", {
  skip_if_not("file" %in% attr(libcurlVersion(), "protocols"), "libcurl has no FILE support")
  src <- withr::local_tempfile(fileext = ".csv")
  data.table::fwrite(data.frame(season = 2024L, game_id = 1:2), src)
  path <- normalizePath(src, winslash = "/")
  out <- .release_csv_from_url(paste0("file://", if (!startsWith(path, "/")) "/", path))
  expect_equal(out$game_id, 1:2)
})
