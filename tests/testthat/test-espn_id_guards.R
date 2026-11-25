# An ESPN helper that pastes a NULL, NA or "" id into its path asks for the
# collection endpoint instead (events/ rather than events/{id}) and parses
# whatever comes back. Every helper that builds a URL from an id argument checks
# it with .espn_check_id() first.

test_that(".espn_check_id() accepts one id and rejects NULL, NA, empty and vectors", {
  expect_identical(.espn_check_id("401715296"), "401715296")
  expect_identical(.espn_check_id(1L), 1L)
  event_id <- NULL
  expect_error(.espn_check_id(event_id), "`event_id` must be a single, non-missing ESPN id")
  expect_error(.espn_check_id(NA_character_), "single, non-missing ESPN id")
  expect_error(.espn_check_id(""), "single, non-missing ESPN id")
  expect_error(.espn_check_id(c("1", "2")), "single, non-missing ESPN id")
})

test_that("ESPN helpers reject a NULL id before making any request", {
  skip_on_cran() # offline, but calls ~400 helper/id pairs
  ns <- asNamespace("fastRhockey")
  is_id <- function(x) grepl("(_id$|^cid$)", x)
  # an id whose default is NULL is an optional filter (season_powerindex's
  # team_id), so NULL is a valid value there
  required_ids <- function(f) {
    fm <- formals(f)
    ids <- names(fm)[is_id(names(fm))]
    ids[!vapply(ids, function(i) !identical(fm[[i]], quote(expr = )) && is.null(fm[[i]]), logical(1))]
  }
  fns <- Filter(function(nm) {
    f <- get(nm, envir = ns)
    is.function(f) && length(required_ids(f)) > 0
  }, grep("^(\\.espn_hockey_|espn_(nhl|mch|wch)_)", ls(ns, all.names = TRUE), value = TRUE))
  expect_gt(length(fns), 250L)

  ok <- list(event_id = "401715296", game_id = "401715296", cid = "401715296",
             athlete_id = "4024981", opponent_id = "1", opp_id = "1", team_id = "1",
             coach_id = "1", award_id = "1", franchise_id = "1", official_id = "1",
             position_id = "1", venue_id = "1", group_id = "1", play_id = "1")
  requests <- 0L
  local_mocked_bindings(
    req_perform = function(...) {
      requests <<- requests + 1L
      stop("a request was made")
    },
    .package = "httr2"
  )

  requested <- character()
  unreported <- character()
  for (nm in fns) {
    f <- get(nm, envir = ns)
    for (id in required_ids(f)) {
      args <- ok[intersect(names(formals(f)), names(ok))]
      args[id] <- list(NULL)
      requests <- 0L
      msgs <- character()
      withCallingHandlers(
        tryCatch(do.call(f, args), error = function(e) msgs <<- c(msgs, conditionMessage(e))),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        },
        warning = function(w) invokeRestart("muffleWarning")
      )
      label <- paste0(nm, "(", id, " = NULL)")
      if (requests > 0L) requested <- c(requested, label)
      # names the helper's own argument, which a college wrapper may call
      # differently (espn_mch_summary(event_id) passes it on as game_id)
      if (!any(grepl("` must be a single, non-missing ESPN id", msgs, fixed = TRUE))) {
        unreported <- c(unreported, label)
      }
    }
  }
  expect_identical(requested, character())
  expect_identical(unreported, character())
})
