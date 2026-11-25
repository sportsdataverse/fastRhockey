# A wrapper's tryCatch() error handler must name the cause of a failure, not
# only "no data available": a 429, a 403 and a parse bug otherwise read like an
# empty result. Every request is made to fail offline, and every function with
# a block-bodied tryCatch() error handler must pass that cause on, without the
# error escaping.

# Functions this setup cannot exercise, with why.
.handler_cause_skip <- c(
  .build_pbp = "parses a feed it is given; no request",
  .euro_resp_json = "parses a response it is given; no request",
  helper_nhl_calculate_xg = "scores a pbp frame it is given; the model download is covered by test-xg",
  refresh_xg_models = "replaces the user's xG model cache",
  .vhl_page_record = "parses page HTML it is given; a parse failure is raised (euro contract)",
  phf_game_all = "defunct: stops with lifecycle::deprecate_stop() before any request",
  phf_leaders = "defunct", phf_league_info = "defunct", phf_pbp = "defunct",
  phf_player_box = "defunct", phf_player_stats = "defunct", phf_schedule = "defunct",
  phf_standings = "defunct", phf_team_box = "defunct", phf_team_roster = "defunct",
  phf_team_stats = "defunct"
)

.handler_cause_args <- list(
  game_id = 2023020001L, event_id = "401715296", athlete_id = "4024981", team_id = "1",
  player_id = 8478402L, team_abbr = "TOR", season = 2024L, coach_id = "1",
  start_season = 2024L, end_season = 2024L, game_type = 2L, group_id = "1",
  league = "nhl", team = "TOR", year = 2024L, attribute = "goals", event_number = 1L,
  play_id = "1", week = 1L, api_season = "20232024", award_id = "1",
  conference_name = "Eastern", division_name = "Atlantic", franchise_id = "1",
  official_id = "1", opponent_id = "1", position_id = "1", postal_code = "10001",
  search_term = "Crosby", season_id = 5L, series_letter = "A", venue_id = "1",
  label = "items", kind = "fpi", page = 1L, player_type = "skater", resource = "teams",
  path = "teams", sport = "hockey", template_key = "x", include_shifts = FALSE
)

# Where one shared value does not fit.
.handler_cause_override <- list(
  .fox_nhl_resource = list(sport = "nhl", resource = "roster"),
  .nhl_edge_api = list(base = "skater-detail/8478402"),
  nhl_schedule = list(game_type = NULL),
  pwhl_stats = list(team = "all")
)

test_that("error handlers report the cause of a failed request", {
  skip_on_cran() # offline, but calls ~220 internals with one shared argument list
  ns <- asNamespace("fastRhockey")
  # Selected by handler shape, not by whether it already reports the cause,
  # so a handler that loses conditionMessage() fails here instead of
  # silently dropping out of the list.
  has_block_handler <- function(nm) {
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) return(FALSE)
    b <- paste(deparse(body(f)), collapse = "\n")
    grepl("tryCatch", b, fixed = TRUE) && grepl("error = function(e) {", b, fixed = TRUE)
  }
  fns <- Filter(has_block_handler, ls(ns, all.names = TRUE))
  expect_true(all(names(.handler_cause_skip) %in% fns))
  fns <- setdiff(fns, names(.handler_cause_skip))
  expect_gt(length(fns), 200L)

  local_mocked_bindings(
    .polite_wait = function(...) invisible(0),
    .download_url = function(...) stop("mock outage")
  )
  local_mocked_bindings(
    req_perform = function(...) stop("mock outage"),
    .package = "httr2"
  )

  silent <- character()
  escaped <- character()
  for (nm in fns) {
    f <- get(nm, envir = ns)
    args <- .handler_cause_args[intersect(names(formals(f)), names(.handler_cause_args))]
    args[names(.handler_cause_override[[nm]])] <- .handler_cause_override[[nm]]
    msgs <- character()
    withCallingHandlers(
      tryCatch(do.call(f, args), error = function(e) escaped <<- c(escaped, nm)),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    if (!any(grepl("mock outage", msgs, fixed = TRUE))) silent <- c(silent, nm)
  }
  expect_identical(escaped, character())
  expect_identical(setdiff(silent, escaped), character())
})
