test_that(".fastRhockey_with_warnings() names the wrapper and keeps the result", {
  f <- function() {
    df <- data.frame()
    tryCatch(
      expr = .fastRhockey_with_warnings("my_wrapper", {
        x <- as.integer("not a number") # warns: NAs introduced by coercion
        df <- data.frame(x = x, y = 1L)
      }),
      error = function(e) NULL
    )
    df
  }
  expect_warning(out <- f(), class = "fastRhockey_warning", regexp = "my_wrapper\\(\\): .*coercion")
  expect_equal(nrow(out), 1L)
  expect_identical(out$y, 1L)
})

test_that("nested .fastRhockey_with_warnings() raises one warning, named by the inner wrapper", {
  w <- testthat::capture_warnings(
    .fastRhockey_with_warnings("outer", .fastRhockey_with_warnings("inner", warning("boom")))
  )
  expect_length(w, 1L)
  expect_match(w, "inner\\(\\): boom")
  expect_no_match(w, "outer")
})

test_that(".fastRhockey_with_warnings() keeps return(), error handling and the original class", {
  early <- function() {
    tryCatch(expr = .fastRhockey_with_warnings("early", {
      return("returned")
      "not reached"
    }), error = function(e) "error")
  }
  expect_identical(early(), "returned")

  expect_warning(caught <- tryCatch(
    expr = .fastRhockey_with_warnings("caught", {
      warning("first")
      stop("then failed")
    }),
    error = function(e) conditionMessage(e)
  ), class = "fastRhockey_warning")
  expect_identical(caught, "then failed")

  custom <- function() warning(warningCondition("deprecated thing", class = "my_deprecation"))
  expect_no_warning(suppressWarnings(.fastRhockey_with_warnings("cls", custom()), classes = "my_deprecation"))
})

test_that(".espn_fn() names the exported wrapper behind a league-generic helper", {
  expect_identical(.espn_fn("nhl", "game"), "espn_nhl_game")
  expect_identical(.espn_fn("mens-college-hockey", "team_roster"), "espn_mch_team_roster")
  expect_identical(.espn_fn("womens-college-hockey", "game"), "espn_wch_game")
})

test_that("an ESPN wrapper returns its rows when parsing warns", {
  # Before 1.1.0 a tryCatch(warning =) handler (here one that reported the
  # warning) stopped the parse at the first warning and returned an empty
  # frame. clean_names() warns here, ahead of the assignment, so the old
  # behaviour returns 0 rows.
  body <- paste(readLines(testthat::test_path(
    "fixtures", "espn_ncaa_hockey", "bodies", "mch_team_roster.json"
  ), warn = FALSE), collapse = "\n")
  local_mocked_bindings(
    .retry_request = function(...) {
      httr2::response(
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(body)
      )
    }
  )
  clean <- janitor::clean_names
  local_mocked_bindings(
    clean_names = function(dat, ...) {
      warning("synthetic parse warning")
      clean(dat, ...)
    },
    .package = "janitor"
  )
  expect_warning(out <- espn_mch_team_roster(team_id = "104"), class = "fastRhockey_warning",
                 regexp = "espn_mch_team_roster\\(\\): synthetic parse warning")
  expect_equal(nrow(out), 2L)
  expect_identical(out$first_name[1], "Tristan")
})

test_that("no wrapper's tryCatch() has a warning = handler", {
  # A warning = handler stops `expr` at the first warning (reported or not);
  # use .fastRhockey_with_warnings() instead. withCallingHandlers() collectors
  # that muffle and carry on are fine.
  ns <- asNamespace("fastRhockey")
  bad <- Filter(function(nm) {
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) return(FALSE)
    found <- FALSE
    walk <- function(x) {
      if (found || !is.call(x)) return(invisible())
      if (identical(x[[1]], as.name("tryCatch")) && "warning" %in% names(x)) found <<- TRUE
      for (a in as.list(x)[-1]) if (!missing(a)) walk(a)
    }
    walk(body(f))
    found
  }, ls(ns, all.names = TRUE))
  expect_identical(bad, character())
})
