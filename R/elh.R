# elh.R -- Czech Extraliga (hokej.cz) wrappers.
#
# Port of sdv-py sportsdataverse/hockey/elh/ (elh_parsers.py, elh_runtime.py and
# the generated elh.py). Two keyless hosts, both throttled to one request a
# second per host through the shared European hockey transport
# (euro_hockey_runtime.R):
#
# * s3-eu-west-1.amazonaws.com/hokej.cz -- a public, listable S3 bucket: a
#   ListObjects V1 XML page at `/`, play-by-play at match/{START year}/{full,
#   short,mobile}/{id}.json, shots and per-minute events at visualization/.
# * json.esports.cz/hokejcz/scoreboard/{YYYY-MM-DD}.json -- one day, every
#   league, keyed by league id (ELH = 101), a ROLLING window of recent days.
#
# Season key: the public argument is the season's END year (2027 = 2026-27);
# the S3 folder is the START year, mapped at the boundary by
# `.season_start_year()` (= sdv-py `elh_start_year`).
#
# Refusals (sdv-py `_check_json` / `_get_listing`): a 2xx error envelope, a
# wrong-scope body (shots of another game, a scoreboard dated another day), a
# listing that is not a ListBucketResult and a truncated listing page raise
# `fastRhockey_asset_fetch`; a stub body (no comments, no shots, no minutes, no
# Extraliga game that day) raises `fastRhockey_no_data`. Transport errors come
# from the shared runtime (404 -> no_data, 400/422 -> bad_request, other
# non-2xx / empty / non-JSON -> asset_fetch).

.ELH_S3 <- "https://s3-eu-west-1.amazonaws.com/hokej.cz"
.ELH_SCOREBOARD <- "https://json.esports.cz/hokejcz"
# Scoreboard league key of the Tipsport extraliga.
.ELH_LEAGUE_ID <- 101L
# A 2xx body whose keys are all in here is an error envelope, not data.
.ELH_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status")
# Hard stop for elh_game_ids() paging (1,000 keys a page; a season folder holds < 7,000).
.ELH_MAX_PAGES <- 50L
# match_shot_resutl_id of a goal (the only verified code).
.ELH_GOAL_RESULT_ID <- 4L
# Seconds in a regulation period.
.ELH_PERIOD_SECONDS <- 1200L

# Documented column -> type of every frame (sdv-py ELH_SCHEMAS). Zero-row
# results carry these columns; populated frames are built to the same types.
.ELH_SCHEMAS <- list(
  pbp = c(
    comment_id = "integer",
    sort_order = "double",
    event_type = "character",
    label2 = "character",
    time_type = "character",
    phase = "character",
    clock = "character",
    game_seconds = "integer",
    period = "integer",
    home_score = "integer",
    away_score = "integer",
    home_shootout_score = "integer",
    away_shootout_score = "integer",
    event_id = "integer",
    onlajny_team_id = "integer",
    onlajny_team_global_id = "integer",
    team_code = "character",
    onlajny_player1_id = "integer",
    onlajny_player1_global_id = "integer",
    player1_name = "character",
    player1_first_name = "character",
    player1_last_name = "character",
    onlajny_player2_id = "integer",
    onlajny_player2_global_id = "integer",
    player2_name = "character",
    player2_first_name = "character",
    player2_last_name = "character",
    onlajny_player3_id = "integer",
    onlajny_player3_global_id = "integer",
    player3_name = "character",
    player3_first_name = "character",
    player3_last_name = "character",
    no_assist = "integer",
    penalty_minutes = "integer",
    penalty_offense = "character",
    strength = "character",
    message = "character",
    written = "character",
    last_change = "character"
  ),
  pbp_goals = c(
    comment_id = "integer",
    event_id = "integer",
    clock = "character",
    game_seconds = "integer",
    period = "integer",
    strength = "character",
    home_score = "integer",
    away_score = "integer",
    scoring_onlajny_team_id = "integer",
    scoring_team_code = "character",
    on_ice_onlajny_team_id = "integer",
    on_ice_team_code = "character",
    is_scoring_team = "logical",
    slot = "integer",
    onlajny_player_id = "integer",
    onlajny_player_global_id = "integer",
    player_name = "character"
  ),
  pbp_mobile = c(
    comment_id = "integer",
    event_type = "character",
    game_seconds = "integer",
    content = "character"
  ),
  shots = c(
    hokejcz_game_id = "integer",
    game_date = "character",
    hokejcz_home_team_id = "integer",
    home_team_name = "character",
    home_team_code = "character",
    hokejcz_away_team_id = "integer",
    away_team_name = "character",
    away_team_code = "character",
    shot_id = "integer",
    hokejcz_player_id = "integer",
    jersey = "integer",
    first_name = "character",
    last_name = "character",
    hokejcz_team_id = "integer",
    team_side = "character",
    match_shot_resutl_id = "integer",
    shot_result_id = "integer",
    is_goal = "logical",
    time = "integer",
    period = "integer",
    coordinate_x = "double",
    coordinate_y = "double",
    real_date = "character"
  ),
  events = c(
    start_date = "character",
    team_side = "character",
    team_name = "character",
    team_score = "integer",
    minute = "integer",
    shots = "integer",
    shots_on_goal = "integer"
  ),
  scoreboard = c(
    league_id = "integer",
    league_name = "character",
    hokejcz_game_id = "integer",
    onlajny_game_id = "integer",
    game_date = "Date",
    start_time = "character",
    status_code = "character",
    status_name = "character",
    match_status = "character",
    home_score = "integer",
    away_score = "integer",
    score_periods = "character",
    hokejcz_home_team_id = "integer",
    onlajny_home_team_id = "integer",
    home_team_name = "character",
    home_team_short_name = "character",
    home_team_code = "character",
    hokejcz_away_team_id = "integer",
    onlajny_away_team_id = "integer",
    away_team_name = "character",
    away_team_short_name = "character",
    away_team_code = "character",
    stream_url = "character"
  ),
  pbp_latest = c(
    comment_id = "integer",
    sort_order = "double",
    event_type = "character",
    label2 = "character",
    time_type = "character",
    phase = "character",
    clock = "character",
    game_seconds = "integer",
    period = "integer",
    home_score = "integer",
    away_score = "integer",
    home_shootout_score = "integer",
    away_shootout_score = "integer",
    event_id = "integer",
    onlajny_team_id = "integer",
    onlajny_team_global_id = "integer",
    team_code = "character",
    onlajny_player1_id = "integer",
    onlajny_player1_global_id = "integer",
    player1_name = "character",
    player1_first_name = "character",
    player1_last_name = "character",
    onlajny_player2_id = "integer",
    onlajny_player2_global_id = "integer",
    player2_name = "character",
    player2_first_name = "character",
    player2_last_name = "character",
    onlajny_player3_id = "integer",
    onlajny_player3_global_id = "integer",
    player3_name = "character",
    player3_first_name = "character",
    player3_last_name = "character",
    no_assist = "integer",
    penalty_minutes = "integer",
    penalty_offense = "character",
    strength = "character",
    message = "character",
    written = "character",
    last_change = "character"
  ),
  list_objects = c(
    hokejcz_game_id = "integer",
    key = "character",
    last_modified = "character",
    size = "integer"
  )
)

# ---------------------------------------------------------------------------
# Cell helpers (sdv-py _int/_id/_float/_str/_dict/_attrs/_many)
# ---------------------------------------------------------------------------
# The XML-to-JSON feed writes every number as a string, an empty element as
# `[]`, and a lone child element as an object while repeated ones become an
# array. jsonlite (simplifyVector = FALSE) turns an object into a NAMED list
# and an array into an UNNAMED list; `{}` is a named empty list, `[]` an
# unnamed one. Every helper returns NA, never raises, for a value it cannot
# read.

#' @noRd
.elh_is_obj <- function(x) is.list(x) && !is.null(names(x))

#' @noRd
.elh_is_arr <- function(x) is.list(x) && is.null(names(x))

#' @noRd
.elh_dict <- function(x) if (.elh_is_obj(x)) x else list()

# dict.get(): NULL for a missing key or a non-object. `[[` is exact-match.
#' @noRd
.elh_get1 <- function(x, key) if (.elh_is_obj(x)) x[[key]] else NULL

#' @noRd
.elh_dig <- function(x, ...) {
  for (key in c(...)) x <- .elh_get1(x, key)
  x
}

#' @noRd
.elh_attrs <- function(node) .elh_dict(.elh_get1(.elh_dict(node), "@attributes"))

#' @noRd
.elh_many <- function(x) {
  if (.elh_is_obj(x)) {
    if (length(x)) list(x) else list()
  } else if (is.list(x)) {
    x
  } else {
    list()
  }
}

# An integer-valued number from a number or a numeric string; else NA. Held as
# double until the column is built (so an out-of-range id is caught, not wrapped).
#' @noRd
.elh_int <- function(x) {
  if (length(x) != 1 || is.logical(x) || is.list(x)) return(NA_real_)
  if (is.numeric(x)) return(if (is.finite(x) && x == round(x)) as.numeric(x) else NA_real_)
  if (is.character(x) && !is.na(x)) {
    s <- trimws(x)
    if (grepl("^-?[0-9]+$", s)) return(as.numeric(s))
  }
  NA_real_
}

# An id; the feed's `0` placeholder (an unused player slot) is NA.
#' @noRd
.elh_id <- function(x) {
  v <- .elh_int(x)
  if (!is.na(v) && v == 0) NA_real_ else v
}

#' @noRd
.elh_dbl <- function(x) {
  if (length(x) != 1 || is.logical(x) || is.list(x)) return(NA_real_)
  if (is.numeric(x)) return(as.numeric(x))
  if (is.character(x) && !is.na(x)) return(suppressWarnings(as.numeric(trimws(x))))
  NA_real_
}

# A non-empty string, else NA (the converter writes an absent value as "" or []).
#' @noRd
.elh_str <- function(x) {
  if (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)) x else NA_character_
}

# Seconds of an elapsed game clock "mm:ss"; NA otherwise.
#' @noRd
.elh_clock_seconds <- function(clock) {
  if (is.na(clock)) return(NA_real_)
  m <- regmatches(clock, regexec("^([0-9]+):([0-9]{2})$", clock))[[1]]
  if (length(m) != 3) return(NA_real_)
  as.numeric(m[2]) * 60 + as.numeric(m[3])
}

# Period of an elapsed-clock second (20-minute periods; 20:00 closes the first).
#' @noRd
.elh_period <- function(seconds) {
  if (is.na(seconds)) NA_real_ else max(seconds - 1, 0) %/% .ELH_PERIOD_SECONDS + 1
}

# Numbered children `<stem>1`, `<stem>2`, ... in number order: list(list(n, node)).
#' @noRd
.elh_numbered <- function(node, stem) {
  nms <- names(node)
  if (is.null(nms) || !length(node)) return(list())
  tail <- substring(nms, nchar(stem) + 1)
  keep <- which(startsWith(nms, stem) & grepl("^[0-9]+$", tail) &
                  vapply(node, .elh_is_obj, logical(1)))
  if (!length(keep)) return(list())
  keep <- keep[order(as.numeric(tail[keep]))]
  lapply(keep, function(i) list(n = as.numeric(tail[i]), node = node[[i]]))
}

# Rows (lists of atomic length-1 cells) -> tibble of the documented schema.
#' @noRd
.elh_frame <- function(rows, schema) {
  if (!length(rows)) return(.empty_tibble(schema))
  num <- function(vals) vapply(vals, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
  cols <- lapply(names(schema), function(nm) {
    vals <- lapply(rows, function(r) r[[nm]])
    switch(schema[[nm]],
      integer = {
        v <- num(vals)
        if (any(abs(v) > .Machine$integer.max, na.rm = TRUE)) {
          .abort_asset_fetch("Column {.field {nm}} holds a value beyond the integer range.")
        }
        as.integer(v)
      },
      double = num(vals),
      character = vapply(vals, function(x) if (is.null(x)) NA_character_ else as.character(x), character(1)),
      logical = vapply(vals, function(x) if (is.null(x)) NA else as.logical(x), logical(1)),
      Date = as.Date(num(vals), origin = "1970-01-01")
    )
  })
  names(cols) <- names(schema)
  tibble::as_tibble(cols)
}

# ---------------------------------------------------------------------------
# Parsers (sdv-py elh_parsers.py). Each returns the documented schema, zero
# rows for an empty or malformed payload -- never an error.
# ---------------------------------------------------------------------------

#' @noRd
.elh_comments <- function(raw) {
  cm <- .elh_many(.elh_dig(raw, "comments", "comment"))
  Filter(.elh_is_obj, cm)
}

#' @noRd
.elh_penalty <- function(player) {
  pens <- lapply(.elh_many(.elh_dig(player, "penalties", "penalty")), .elh_attrs)
  if (!length(pens)) return(list(minutes = NA_real_, offense = NA_character_))
  mins <- vapply(pens, function(p) .elh_int(p[["length"]]), numeric(1))
  offs <- vapply(pens, function(p) .elh_str(p[["offense"]]), character(1))
  offs <- offs[!is.na(offs)]
  list(
    minutes = if (any(!is.na(mins))) sum(mins, na.rm = TRUE) else NA_real_,
    offense = if (length(offs)) paste(offs, collapse = ",") else NA_character_
  )
}

#' @noRd
.elh_pbp_row <- function(entry) {
  a <- .elh_attrs(entry)
  tm <- entry[["time"]]
  if (is.character(tm)) {
    clock <- .elh_str(tm)
    phase <- NA_character_
  } else {
    clock <- NA_character_
    phase <- .elh_str(.elh_attrs(tm)[["period"]])
  }
  seconds <- .elh_clock_seconds(clock)
  details <- .elh_dict(entry[["details"]])
  detail <- .elh_dict(details[["detail"]])
  team <- .elh_attrs(detail[["opponent"]])
  row <- list(
    comment_id = .elh_id(a[["id"]]),
    sort_order = .elh_int(a[["order"]]),
    event_type = .elh_str(a[["label"]]),
    label2 = .elh_str(a[["label2"]]),
    time_type = .elh_str(a[["type"]]),
    phase = phase,
    clock = clock,
    game_seconds = seconds,
    period = .elh_period(seconds),
    home_score = .elh_int(a[["score1"]]),
    away_score = .elh_int(a[["score2"]]),
    home_shootout_score = .elh_int(a[["shootout_score1"]]),
    away_shootout_score = .elh_int(a[["shootout_score2"]]),
    event_id = .elh_id(.elh_attrs(detail)[["id"]]),
    onlajny_team_id = .elh_id(team[["id"]]),
    onlajny_team_global_id = .elh_id(team[["global_id"]]),
    team_code = .elh_str(team[["code"]])
  )
  no_assist <- NA_real_
  pen <- list(minutes = NA_real_, offense = NA_character_)
  for (n in 1:3) {
    player <- .elh_dict(detail[[paste0("player", n)]])
    p <- .elh_attrs(player)
    cells <- list(
      .elh_id(p[["id"]]), .elh_id(p[["global_id"]]), .elh_str(p[["name"]]),
      .elh_str(p[["firstname"]]), .elh_str(p[["surname"]])
    )
    names(cells) <- c(
      sprintf("onlajny_player%d_id", n), sprintf("onlajny_player%d_global_id", n),
      sprintf("player%d_name", n), sprintf("player%d_first_name", n),
      sprintf("player%d_last_name", n)
    )
    row <- c(row, cells)
    if (n == 1) {
      no_assist <- .elh_int(p[["no_assist"]])
      pen <- .elh_penalty(player)
    }
  }
  row$no_assist <- no_assist
  row$penalty_minutes <- pen$minutes
  row$penalty_offense <- pen$offense
  row$strength <- .elh_str(.elh_attrs(details[["on_field"]])[["balance"]])
  row$message <- .elh_str(entry[["message"]])
  row$written <- .elh_str(a[["written"]])
  row$last_change <- .elh_str(a[["last_change"]])
  row
}

# Chronological: ascending sort_order (the file is newest first); rows without
# one last, in file order.
#' @noRd
.elh_sort_index <- function(rows) {
  so <- vapply(rows, function(r) r[["sort_order"]], numeric(1))
  order(is.na(so), ifelse(is.na(so), 0, so), seq_along(rows))
}

#' @noRd
.elh_parse_pbp <- function(raw) {
  rows <- lapply(.elh_comments(raw), .elh_pbp_row)
  if (length(rows)) rows <- rows[.elh_sort_index(rows)]
  .elh_frame(rows, .ELH_SCHEMAS$pbp)
}

#' @noRd
.elh_parse_pbp_goals <- function(raw) {
  comments <- .elh_comments(raw)
  rows <- lapply(comments, .elh_pbp_row)
  ord <- if (length(rows)) .elh_sort_index(rows) else integer()
  empty <- as.list(stats::setNames(rep(NA, 7), names(.ELH_SCHEMAS$pbp_goals)[11:17]))
  out <- list()
  for (i in ord) {
    entry <- rows[[i]]
    if (!identical(entry$event_type, "goal")) next
    details <- .elh_dict(comments[[i]][["details"]])
    base <- entry[c(
      "comment_id", "event_id", "clock", "game_seconds", "period", "strength",
      "home_score", "away_score"
    )]
    base$scoring_onlajny_team_id <- entry$onlajny_team_id
    base$scoring_team_code <- entry$team_code
    players <- list()
    for (team_n in .elh_numbered(.elh_dict(details[["on_field"]]), "opponent")) {
      t <- .elh_attrs(team_n$node)
      team_id <- .elh_id(t[["id"]])
      for (slot_n in .elh_numbered(team_n$node, "player")) {
        p <- .elh_attrs(slot_n$node)
        players[[length(players) + 1]] <- c(base, list(
          on_ice_onlajny_team_id = team_id,
          on_ice_team_code = .elh_str(t[["code"]]),
          is_scoring_team = if (is.na(team_id)) NA else if (is.na(base$scoring_onlajny_team_id)) FALSE else team_id == base$scoring_onlajny_team_id,
          slot = slot_n$n,
          onlajny_player_id = .elh_id(p[["id"]]),
          onlajny_player_global_id = .elh_id(p[["global_id"]]),
          player_name = .elh_str(p[["name"]])
        ))
      }
    }
    # A goal without the on-ice block is ONE row with NA on-ice columns, so no goal is dropped.
    out <- c(out, if (length(players)) players else list(c(base, empty)))
  }
  .elh_frame(out, .ELH_SCHEMAS$pbp_goals)
}

#' @noRd
.elh_parse_pbp_mobile <- function(raw) {
  events <- .elh_get1(raw, "commentEvents")
  rows <- if (.elh_is_arr(events)) {
    lapply(Filter(.elh_is_obj, events), function(e) list(
      comment_id = .elh_id(e[["id"]]),
      event_type = .elh_str(e[["type"]]),
      game_seconds = .elh_int(e[["time"]]),
      content = .elh_str(e[["content"]])
    ))
  } else {
    list()
  }
  .elh_frame(rows, .ELH_SCHEMAS$pbp_mobile)
}

#' @noRd
.elh_parse_shots <- function(raw) {
  match <- .elh_dict(.elh_get1(raw, "match"))
  home_id <- .elh_id(match[["home_id"]])
  away_id <- .elh_id(match[["visitor_id"]])
  game <- list(
    hokejcz_game_id = .elh_id(match[["id"]]),
    game_date = .elh_str(match[["date"]]),
    hokejcz_home_team_id = home_id,
    home_team_name = .elh_str(match[["home_name"]]),
    home_team_code = .elh_str(match[["home_shortcut"]]),
    hokejcz_away_team_id = away_id,
    away_team_name = .elh_str(match[["visitor_name"]]),
    away_team_code = .elh_str(match[["visitor_shortcut"]])
  )
  shots <- match[["shots"]]
  rows <- if (.elh_is_arr(shots)) {
    lapply(Filter(.elh_is_obj, shots), function(s) {
      team_id <- .elh_id(s[["team_id"]])
      result <- .elh_int(s[["match_shot_resutl_id"]])
      time <- .elh_int(s[["time"]])
      side <- NA_character_
      if (!is.na(team_id) && !is.na(home_id) && team_id == home_id) side <- "home"
      if (!is.na(team_id) && !is.na(away_id) && team_id == away_id) side <- "away"
      c(game, list(
        shot_id = .elh_id(s[["id"]]),
        hokejcz_player_id = .elh_id(s[["player_id"]]),
        jersey = .elh_int(s[["jersey"]]),
        first_name = .elh_str(s[["first_name"]]),
        last_name = .elh_str(s[["last_name"]]),
        hokejcz_team_id = team_id,
        team_side = side,
        match_shot_resutl_id = result,
        shot_result_id = result,
        is_goal = if (is.na(result)) NA else result == .ELH_GOAL_RESULT_ID,
        time = time,
        period = if (is.na(time) || time < 0) NA_real_ else time %/% .ELH_PERIOD_SECONDS + 1,
        coordinate_x = .elh_dbl(s[["coordinate_x"]]),
        coordinate_y = .elh_dbl(s[["coordinate_y"]]),
        real_date = .elh_str(s[["real_date"]])
      ))
    })
  } else {
    list()
  }
  .elh_frame(rows, .ELH_SCHEMAS$shots)
}

#' @noRd
.elh_parse_events <- function(raw) {
  body <- .elh_dict(raw)
  rows <- list()
  for (sd in list(c("home", "home"), c("visitor", "away"))) {
    team <- .elh_dict(body[[sd[1]]])
    shots <- .elh_dict(team[["shots"]])
    on_goal <- .elh_dict(team[["shotsOnGoal"]])
    minutes <- vapply(c(names(shots), names(on_goal)), .elh_int, numeric(1), USE.NAMES = FALSE)
    for (minute in sort(unique(minutes[!is.na(minutes)]))) {
      key <- as.character(minute)
      rows[[length(rows) + 1]] <- list(
        start_date = .elh_str(body[["startDate"]]),
        team_side = sd[2],
        team_name = .elh_str(team[["teamName"]]),
        team_score = .elh_int(team[["score"]]),
        minute = minute,
        shots = .elh_int(shots[[key]]),
        shots_on_goal = .elh_int(on_goal[[key]])
      )
    }
  }
  .elh_frame(rows, .ELH_SCHEMAS$events)
}

# "DD-MM-YYYY" -> days since the epoch (a valid calendar day), else NA.
#' @noRd
.elh_iso_date <- function(dmy) {
  if (!is.character(dmy) || length(dmy) != 1 || is.na(dmy)) return(NA_real_)
  m <- regmatches(dmy, regexec("^([0-9]{2})-([0-9]{2})-([0-9]{4})$", dmy))[[1]]
  if (length(m) != 4) return(NA_real_)
  d <- as.Date(sprintf("%s-%s-%s", m[4], m[3], m[2]), format = "%Y-%m-%d")
  if (is.na(d)) NA_real_ else as.numeric(d)
}

# `league_id`: keep only this league (101 = Tipsport extraliga); NULL keeps all.
#' @noRd
.elh_parse_scoreboard <- function(raw, league_id = .ELH_LEAGUE_ID) {
  rows <- list()
  leagues <- .elh_dict(raw)
  for (key in names(leagues)) {
    lid <- .elh_int(key)
    if (is.na(lid) || (!is.null(league_id) && lid != league_id)) next
    league <- leagues[[key]]
    for (g in .elh_many(.elh_get1(league, "matches"))) {
      if (!.elh_is_obj(g)) next
      periods <- Filter(function(p) is.character(p) && length(p) == 1, .elh_many(g[["score_periods"]]))
      row <- list(
        league_id = lid,
        league_name = .elh_str(.elh_get1(league, "league_name")),
        hokejcz_game_id = .elh_id(g[["hokejcz_id"]]),
        onlajny_game_id = .elh_id(g[["onlajny_id"]]),
        game_date = .elh_iso_date(g[["date"]]),
        start_time = .elh_str(g[["time"]]),
        status_code = .elh_str(g[["match_actual_time_alias"]]),
        status_name = .elh_str(g[["match_actual_time_name"]]),
        match_status = .elh_str(g[["match_status"]]),
        home_score = .elh_int(g[["score_home"]]),
        away_score = .elh_int(g[["score_visitor"]]),
        score_periods = if (length(periods)) paste(unlist(periods), collapse = ",") else NA_character_,
        stream_url = .elh_str(g[["stream_url"]])
      )
      for (sd in list(c("home", "home"), c("visitor", "away"))) {
        team <- .elh_dict(g[[sd[1]]])
        side <- sd[2]
        row[[sprintf("hokejcz_%s_team_id", side)]] <- .elh_id(team[["hokejcz_id"]])
        row[[sprintf("onlajny_%s_team_id", side)]] <- .elh_id(team[["onlajny_id"]])
        row[[sprintf("%s_team_name", side)]] <- .elh_str(team[["name"]])
        row[[sprintf("%s_team_short_name", side)]] <- .elh_str(team[["short_name"]])
        row[[sprintf("%s_team_code", side)]] <- .elh_str(team[["shortcut"]])
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  .elh_frame(rows, .ELH_SCHEMAS$scoreboard)
}

# ---------------------------------------------------------------------------
# S3 listing (ListObjects V1 XML)
# ---------------------------------------------------------------------------

# Parse XML text, NULL when it is not XML. The text is passed as raw bytes:
# xml2::read_xml() on a character string that does not start with "<" would
# treat it as a file path or URL.
#' @noRd
.elh_read_xml <- function(text) {
  rlang::check_installed("xml2")
  tryCatch(xml2::read_xml(charToRaw(enc2utf8(text))), error = function(e) NULL)
}

#' @noRd
.elh_localname <- function(nodes) sub("^.*:", "", xml2::xml_name(nodes))

# Contents of each listing page -> one row per key.
#' @noRd
.elh_parse_game_ids <- function(pages) {
  if (is.character(pages)) pages <- as.list(pages)
  rows <- list()
  for (page in pages) {
    if (!is.character(page) || length(page) != 1 || is.na(page)) next
    root <- .elh_read_xml(page)
    if (is.null(root)) next
    for (el in xml2::xml_children(root)) {
      if (.elh_localname(el) != "Contents") next
      kids <- xml2::xml_children(el)
      cell <- as.list(stats::setNames(xml2::xml_text(kids), .elh_localname(kids)))
      cell <- cell[!duplicated(names(cell), fromLast = TRUE)]
      key <- .elh_str(cell[["Key"]])
      m <- if (is.na(key)) character() else regmatches(key, regexec("^match/[0-9]+/full/([0-9]+)\\.json$", key))[[1]]
      rows[[length(rows) + 1]] <- list(
        hokejcz_game_id = if (length(m) == 2) as.numeric(m[2]) else NA_real_,
        key = key,
        last_modified = .elh_str(cell[["LastModified"]]),
        size = .elh_int(cell[["Size"]])
      )
    }
  }
  .elh_frame(rows, .ELH_SCHEMAS$list_objects)
}

# Parse a listing page, refusing anything but a ListBucketResult.
#' @noRd
.elh_listing_root <- function(url, text) {
  where <- .euro_where(url)
  root <- .elh_read_xml(text)
  if (is.null(root)) {
    excerpt <- substr(gsub("\\s+", " ", text), 1, 200)
    .abort_asset_fetch("{where}: the listing is not XML: {excerpt}")
  }
  if (.elh_localname(root) != "ListBucketResult") {
    .abort_asset_fetch("{where}: the listing root is not ListBucketResult")
  }
  root
}

# list(truncated, last_key) of a listing page.
#' @noRd
.elh_page_state <- function(root) {
  truncated <- FALSE
  last <- NA_character_
  for (el in xml2::xml_children(root)) {
    tag <- .elh_localname(el)
    if (tag == "IsTruncated") {
      truncated <- identical(tolower(trimws(xml2::xml_text(el))), "true")
    } else if (tag == "Contents") {
      kids <- xml2::xml_children(el)
      keys <- kids[.elh_localname(kids) == "Key"]
      if (length(keys)) last <- xml2::xml_text(keys[[length(keys)]])
    }
  }
  list(truncated = truncated, last = last)
}

# ---------------------------------------------------------------------------
# Transport and scope checks (sdv-py elh_runtime.py)
# ---------------------------------------------------------------------------

# GET a text body (the listing) under the shared error vocabulary: 404 ->
# no_data, 400/422 -> bad_request, other non-2xx or an empty body ->
# asset_fetch. The mock seam for the listing in tests.
#' @noRd
.elh_get_text <- function(url, query = list()) {
  query <- Filter(Negate(is.null), query)
  resp <- .build_request(url, query) |> .polite_perform()
  .euro_check_status(resp)
  text <- if (httr2::resp_has_body(resp)) .resp_text(resp) else ""
  if (!nzchar(trimws(text))) {
    where <- .euro_where(url)
    status <- httr2::resp_status(resp)
    .abort_asset_fetch("{where} answered HTTP {status} with an empty body.")
  }
  text
}

# One listing page as XML text, scope-checked.
#' @noRd
.elh_get_listing <- function(url, query = list()) {
  text <- .elh_get_text(url, query)
  state <- .elh_page_state(.elh_listing_root(url, text))
  if (state$truncated && is.na(state$last)) {
    where <- .euro_where(url)
    .abort_asset_fetch("{where}: a truncated listing page carries no key to page on")
  }
  text
}

# Raise unless a JSON body is a real, in-scope answer for `url`.
#' @noRd
.elh_check_json <- function(url, raw) {
  where <- .euro_where(url)
  if (.elh_is_obj(raw) && length(raw) && all(names(raw) %in% .ELH_ERROR_KEYS)) {
    keys <- sort(names(raw))
    .abort_asset_fetch("{where}: answered an error envelope {keys}")
  }
  path <- httr2::url_parse(url)$path
  rows_ok <- function(x) length(x) > 0 && all(vapply(x, .elh_is_obj, logical(1))) &&
    all(vapply(x, length, integer(1)) > 0)
  if (grepl("/match/[0-9]+/(full|short)/[0-9]+\\.json$", path)) {
    rows <- .elh_many(.elh_dig(raw, "comments", "comment"))
    if (!rows_ok(rows)) .abort_no_data("{where}: the play-by-play holds no comments")
  } else if (grepl("/match/[0-9]+/mobile/[0-9]+\\.json$", path)) {
    rows <- .elh_dig(raw, "commentEvents")
    if (!(.elh_is_arr(rows) && rows_ok(rows))) {
      .abort_no_data("{where}: the mobile play-by-play holds no comments")
    }
  } else if (grepl("/visualization/shots/[0-9]+\\.json$", path)) {
    want <- sub("^.*/visualization/shots/([0-9]+)\\.json$", "\\1", path)
    shots <- .elh_dig(raw, "match", "shots")
    if (!(.elh_is_arr(shots) && rows_ok(shots))) .abort_no_data("{where}: the shot file holds no shots")
    got <- .elh_dig(raw, "match", "id")
    got_chr <- if (is.numeric(got) && length(got) == 1) format(got, scientific = FALSE, trim = TRUE) else as.character(got)
    if (!identical(got_chr, want)) {
      got_lbl <- if (length(got_chr)) got_chr else "none"
      .abort_asset_fetch("{where}: answered game {got_lbl} for game {want}")
    }
  } else if (grepl("/visualization/events/[0-9]+\\.json$", path)) {
    for (side in c("home", "visitor")) {
      minutes <- .elh_dig(raw, side, "shots")
      if (!(.elh_is_obj(minutes) && length(minutes))) {
        .abort_no_data("{where}: the per-minute file holds no {side} minutes")
      }
    }
  } else if (grepl("/scoreboard/[0-9]{4}-[0-9]{2}-[0-9]{2}\\.json$", path)) {
    ymd <- regmatches(path, regexec("/scoreboard/([0-9]{4})-([0-9]{2})-([0-9]{2})\\.json$", path))[[1]]
    day <- sprintf("%s-%s-%s", ymd[4], ymd[3], ymd[2]) # the body dates games DD-MM-YYYY
    leagues <- .elh_dict(raw)
    dates <- unique(unlist(lapply(leagues, function(lg) {
      lapply(Filter(.elh_is_obj, .elh_many(.elh_get1(lg, "matches"))), function(g) {
        d <- g[["date"]]
        if (is.null(d)) NA_character_ else as.character(d)[1]
      })
    })))
    if (length(dates) && !identical(dates, day)) {
      found <- sort(dates)
      .abort_asset_fetch("{where}: answered games dated {found} for {day}")
    }
    if (!length(.elh_many(.elh_dig(leagues, as.character(.ELH_LEAGUE_ID), "matches")))) {
      lid <- .ELH_LEAGUE_ID
      .abort_no_data("{where}: no Extraliga (league {lid}) game that day")
    }
  }
  invisible(raw)
}

# GET an ELH route: the listing as XML text, everything else as checked JSON.
# The mock seam for the JSON routes in tests is `.euro_get()`.
#' @noRd
.elh_get <- function(url, query = list()) {
  if (sub("/+$", "", httr2::url_parse(url)$path) == httr2::url_parse(.ELH_S3)$path) {
    return(.elh_get_listing(url, query))
  }
  raw <- .euro_get(url, query, simplifyVector = FALSE)
  .elh_check_json(url, raw)
  raw
}

# A hokej.cz game id for a URL path: one positive whole number.
#' @noRd
.elh_match_id <- function(match_id) {
  v <- if (length(match_id) == 1 && !is.na(match_id)) suppressWarnings(as.numeric(match_id)) else NA_real_
  if (is.na(v) || v <= 0 || v != round(v)) {
    .abort_bad_request("{.arg match_id} must be one hokej.cz game id (a positive whole number), not {.val {match_id}}.")
  }
  format(v, scientific = FALSE, trim = TRUE)
}

# The scoreboard day as YYYY-MM-DD.
#' @noRd
.elh_date <- function(date) {
  d <- if (inherits(date, "Date") && length(date) == 1 && !is.na(date)) format(date, "%Y-%m-%d") else date
  if (!is.character(d) || length(d) != 1 || is.na(d) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)) {
    .abort_bad_request("{.arg date} must be one day as YYYY-MM-DD, not {.val {date}}.")
  }
  d
}

# ---------------------------------------------------------------------------
# Exported wrappers
# ---------------------------------------------------------------------------

#' @title **Czech Extraliga (ELH) play-by-play commentary**
#' @description Play-by-play commentary of one Tipsport Extraliga game from
#'   hokej.cz's S3 bucket (`match/{season - 1}/full/{match_id}.json`), every
#'   entry, oldest first: goals and penalties with team and players.
#'
#'   Ids are **onlajny** ids (the commentary system), not the hokej.cz ids of
#'   [elh_shots()]: join the two through [elh_scoreboard()], which maps games and
#'   teams between the id spaces (only for days still in its rolling window).
#'   `clock` is the elapsed game clock `mm:ss`; a clockless entry names only its
#'   `phase` (`0` pre-game, `1INT`/`2INT` intermissions, `3PER`, `ERT` after the
#'   game; meanings inferred, the provider documents none). `period` is derived
#'   from the clock (20-minute periods). `strength` (`EQ` = even strength,
#'   inferred, the only value observed) is set only on goals that carry the
#'   on-ice block (not in 2016-17).
#' @param season Season by its END year (2027 = 2026-27); the bucket files it
#'   under the START year (`match/2026/`), mapped here. Play-by-play starts in
#'   2014-15 (season 2015).
#' @param match_id hokej.cz game id (the S3 file name; `hokejcz_game_id` of
#'   [elh_game_ids()] / [elh_scoreboard()]), e.g. 2928321. Not the onlajny game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON as a list (XML-converted: every attribute a string).
#' @return A tibble (`return_parsed = TRUE`; zero rows for a malformed body),
#'   else the raw JSON list:
#'
#'    |col_name                 |type     |description|
#'    |:------------------------|:--------|:----------|
#'    |comment_id               |integer  |Onlajny commentary entry id (integer); equals the mobile file's comment id.|
#'    |sort_order               |numeric  |Provider sort key (integer-valued double: values exceed the 32-bit integer range); ascending = chronological, the order rows are returned in.|
#'    |event_type               |character|Entry kind: goal, penalty, time (period start/end), important, normal, expertComment.|
#'    |label2                   |character|Secondary label; empty (null) in every sample.|
#'    |time_type                |character|For time entries: begin or end of a period; null otherwise.|
#'    |phase                    |character|Phase of a clockless entry: 0 pre-game, 1INT / 2INT intermissions, 3PER third period, ERT after the game (meanings inferred from the entries, undocumented); null when the entry has a clock.|
#'    |clock                    |character|Elapsed game clock mm:ss (00:01 = game start); null on a clockless entry.|
#'    |game_seconds             |integer  |Seconds from the start of the game.|
#'    |period                   |integer  |Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry.|
#'    |home_score               |integer  |Home goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |away_score               |integer  |Away goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |home_shootout_score      |integer  |Home shootout goals after the entry (0 without a shootout).|
#'    |away_shootout_score      |integer  |Away shootout goals after the entry (0 without a shootout).|
#'    |event_id                 |integer  |Onlajny event id of a goal or penalty (integer); null on other entries.|
#'    |onlajny_team_id          |integer  |Onlajny team id of the team credited with the goal or penalty (Sparta = 156); NOT the hokej.cz team id. The scoreboard maps it.|
#'    |onlajny_team_global_id   |integer  |Onlajny global team id (equal to onlajny_team_id in every sample).|
#'    |team_code                |character|Team code of the credited team, e.g. SPA (same as the shots / scoreboard codes).|
#'    |onlajny_player1_id       |integer  |Onlajny player id of slot 1 (goal scorer / penalised player); NOT the hokej.cz id of the shots; null for an unused slot.|
#'    |onlajny_player1_global_id|integer  |Onlajny GLOBAL player id of slot 1 (goal scorer / penalised player); can differ from onlajny_player1_id (Hradec 48287 / 76559).|
#'    |player1_name             |character|Display name of slot 1 (goal scorer / penalised player).|
#'    |player1_first_name       |character|Given name of slot 1 (goal scorer / penalised player); null in 2016-17.|
#'    |player1_last_name        |character|Family name of slot 1 (goal scorer / penalised player); null in 2016-17.|
#'    |onlajny_player2_id       |integer  |Onlajny player id of slot 2 (first assist); NOT the hokej.cz id of the shots; null for an unused slot.|
#'    |onlajny_player2_global_id|integer  |Onlajny GLOBAL player id of slot 2 (first assist); can differ from onlajny_player2_id (Hradec 48287 / 76559).|
#'    |player2_name             |character|Display name of slot 2 (first assist).|
#'    |player2_first_name       |character|Given name of slot 2 (first assist); null in 2016-17.|
#'    |player2_last_name        |character|Family name of slot 2 (first assist); null in 2016-17.|
#'    |onlajny_player3_id       |integer  |Onlajny player id of slot 3 (second assist); NOT the hokej.cz id of the shots; null for an unused slot.|
#'    |onlajny_player3_global_id|integer  |Onlajny GLOBAL player id of slot 3 (second assist); can differ from onlajny_player3_id (Hradec 48287 / 76559).|
#'    |player3_name             |character|Display name of slot 3 (second assist).|
#'    |player3_first_name       |character|Given name of slot 3 (second assist); null in 2016-17.|
#'    |player3_last_name        |character|Family name of slot 3 (second assist); null in 2016-17.|
#'    |no_assist                |integer  |Unassisted flag on slot 1 (0 in every sample; 1 = unassisted is inferred, unobserved). Absent before 2026-27 samples.|
#'    |penalty_minutes          |integer  |Penalty minutes of the penalised player (player1); two penalties are summed. Null in 2016-17.|
#'    |penalty_offense          |character|Czech offense code(s), e.g. VYSH (high sticking), PODR (tripping); comma-joined for two penalties. Null in 2016-17.|
#'    |strength                 |character|Strength of a goal: EQ (read as even strength; inferred, the only value observed). Null where the on-ice block is absent (all of 2016-17).|
#'    |message                  |character|Czech commentary text (may contain <br />).|
#'    |written                  |character|When the entry was written, local time YYYY-MM-DD HH:MM:SS.|
#'    |last_change              |character|Last edit of the entry, local time YYYY-MM-DD HH:MM:SS.|
#' @section Errors:
#' `fastRhockey_no_data`: a missing file (404 `NoSuchKey`) or a stub body without
#' comments. `fastRhockey_asset_fetch`: an error envelope, an empty or non-JSON
#' 200, or any other non-2xx. `fastRhockey_bad_request`: HTTP 400/422 or a bad
#' `season` / `match_id`.
#' @keywords ELH Play-by-play
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_pbp(season = 2027, match_id = 2928321))
#' }
elh_pbp <- function(season, match_id, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/match/%d/full/%s.json", .ELH_S3, .season_start_year(season), .elh_match_id(match_id))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_pbp(raw) else raw
}

#' @title **Czech Extraliga (ELH) players on the ice at each goal**
#' @description Players on the ice at each goal of one game (onlajny ids), from
#'   the full play-by-play (`match/{season - 1}/full/{match_id}.json`). A goal
#'   entry's on-ice block lists both teams' skaters and goalies (up to six
#'   players each) and the strength. Every player is a row tagged with the scoring
#'   team, so plus/minus and on-ice counts fall out of a group-by. A goal without
#'   that block (every 2016-17 goal) is ONE row with `NA` on-ice columns, so no
#'   goal is dropped.
#' @inheritParams elh_pbp
#' @return A tibble (`return_parsed = TRUE`; goals oldest first, players in team
#'   then slot order; zero rows for a malformed body), else the raw JSON list:
#'
#'    |col_name                |type     |description|
#'    |:-----------------------|:--------|:----------|
#'    |comment_id              |integer  |Onlajny commentary entry id (integer); equals the mobile file's comment id.|
#'    |event_id                |integer  |Onlajny event id of a goal or penalty (integer); null on other entries.|
#'    |clock                   |character|Elapsed game clock mm:ss (00:01 = game start); null on a clockless entry.|
#'    |game_seconds            |integer  |Seconds from the start of the game.|
#'    |period                  |integer  |Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry.|
#'    |strength                |character|Strength of a goal: EQ (read as even strength; inferred, the only value observed). Null where the on-ice block is absent (all of 2016-17).|
#'    |home_score              |integer  |Home goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |away_score              |integer  |Away goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |scoring_onlajny_team_id |integer  |Onlajny team id of the scoring team.|
#'    |scoring_team_code       |character|Team code of the scoring team, e.g. SPA.|
#'    |on_ice_onlajny_team_id  |integer  |Onlajny team id of the on-ice player's team; null for a goal without the on-ice block (2016-17).|
#'    |on_ice_team_code        |character|Team code of the on-ice player's team.|
#'    |is_scoring_team         |logical  |True when the on-ice player is on the scoring team.|
#'    |slot                    |integer  |Player slot within the team's on-ice block (1-6).|
#'    |onlajny_player_id       |integer  |Onlajny player id of the on-ice player (per competition, inferred); NOT the hokej.cz player id of the shots.|
#'    |onlajny_player_global_id|integer  |Onlajny GLOBAL player id of the on-ice player; can differ from onlajny_player_id for the same player.|
#'    |player_name             |character|On-ice player's display name (surname).|
#' @section Errors:
#' As [elh_pbp()].
#' @keywords ELH Play-by-play
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_pbp_goals(season = 2027, match_id = 2928321))
#' }
elh_pbp_goals <- function(season, match_id, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/match/%d/full/%s.json", .ELH_S3, .season_start_year(season), .elh_match_id(match_id))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_pbp_goals(raw) else raw
}

#' @title **Czech Extraliga (ELH) latest play-by-play entries**
#' @description The newest 20 play-by-play entries of one game
#'   (`match/{season - 1}/short/{match_id}.json`); same columns as [elh_pbp()].
#' @inheritParams elh_pbp
#' @return A tibble (`return_parsed = TRUE`; zero rows for a malformed body),
#'   else the raw JSON list:
#'
#'    |col_name                 |type     |description|
#'    |:------------------------|:--------|:----------|
#'    |comment_id               |integer  |Onlajny commentary entry id (integer); equals the mobile file's comment id.|
#'    |sort_order               |numeric  |Provider sort key (integer-valued double: values exceed the 32-bit integer range); ascending = chronological, the order rows are returned in.|
#'    |event_type               |character|Entry kind: goal, penalty, time (period start/end), important, normal, expertComment.|
#'    |label2                   |character|Secondary label; empty (null) in every sample.|
#'    |time_type                |character|For time entries: begin or end of a period; null otherwise.|
#'    |phase                    |character|Phase of a clockless entry: 0 pre-game, 1INT / 2INT intermissions, 3PER third period, ERT after the game (meanings inferred from the entries, undocumented); null when the entry has a clock.|
#'    |clock                    |character|Elapsed game clock mm:ss (00:01 = game start); null on a clockless entry.|
#'    |game_seconds             |integer  |Seconds from the start of the game.|
#'    |period                   |integer  |Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry.|
#'    |home_score               |integer  |Home goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |away_score               |integer  |Away goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |home_shootout_score      |integer  |Home shootout goals after the entry (0 without a shootout).|
#'    |away_shootout_score      |integer  |Away shootout goals after the entry (0 without a shootout).|
#'    |event_id                 |integer  |Onlajny event id of a goal or penalty (integer); null on other entries.|
#'    |onlajny_team_id          |integer  |Onlajny team id of the team credited with the goal or penalty (Sparta = 156); NOT the hokej.cz team id. The scoreboard maps it.|
#'    |onlajny_team_global_id   |integer  |Onlajny global team id (equal to onlajny_team_id in every sample).|
#'    |team_code                |character|Team code of the credited team, e.g. SPA (same as the shots / scoreboard codes).|
#'    |onlajny_player1_id       |integer  |Onlajny player id of slot 1 (goal scorer / penalised player); NOT the hokej.cz id of the shots; null for an unused slot.|
#'    |onlajny_player1_global_id|integer  |Onlajny GLOBAL player id of slot 1 (goal scorer / penalised player); can differ from onlajny_player1_id (Hradec 48287 / 76559).|
#'    |player1_name             |character|Display name of slot 1 (goal scorer / penalised player).|
#'    |player1_first_name       |character|Given name of slot 1 (goal scorer / penalised player); null in 2016-17.|
#'    |player1_last_name        |character|Family name of slot 1 (goal scorer / penalised player); null in 2016-17.|
#'    |onlajny_player2_id       |integer  |Onlajny player id of slot 2 (first assist); NOT the hokej.cz id of the shots; null for an unused slot.|
#'    |onlajny_player2_global_id|integer  |Onlajny GLOBAL player id of slot 2 (first assist); can differ from onlajny_player2_id (Hradec 48287 / 76559).|
#'    |player2_name             |character|Display name of slot 2 (first assist).|
#'    |player2_first_name       |character|Given name of slot 2 (first assist); null in 2016-17.|
#'    |player2_last_name        |character|Family name of slot 2 (first assist); null in 2016-17.|
#'    |onlajny_player3_id       |integer  |Onlajny player id of slot 3 (second assist); NOT the hokej.cz id of the shots; null for an unused slot.|
#'    |onlajny_player3_global_id|integer  |Onlajny GLOBAL player id of slot 3 (second assist); can differ from onlajny_player3_id (Hradec 48287 / 76559).|
#'    |player3_name             |character|Display name of slot 3 (second assist).|
#'    |player3_first_name       |character|Given name of slot 3 (second assist); null in 2016-17.|
#'    |player3_last_name        |character|Family name of slot 3 (second assist); null in 2016-17.|
#'    |no_assist                |integer  |Unassisted flag on slot 1 (0 in every sample; 1 = unassisted is inferred, unobserved). Absent before 2026-27 samples.|
#'    |penalty_minutes          |integer  |Penalty minutes of the penalised player (player1); two penalties are summed. Null in 2016-17.|
#'    |penalty_offense          |character|Czech offense code(s), e.g. VYSH (high sticking), PODR (tripping); comma-joined for two penalties. Null in 2016-17.|
#'    |strength                 |character|Strength of a goal: EQ (read as even strength; inferred, the only value observed). Null where the on-ice block is absent (all of 2016-17).|
#'    |message                  |character|Czech commentary text (may contain <br />).|
#'    |written                  |character|When the entry was written, local time YYYY-MM-DD HH:MM:SS.|
#'    |last_change              |character|Last edit of the entry, local time YYYY-MM-DD HH:MM:SS.|
#' @section Errors:
#' As [elh_pbp()].
#' @keywords ELH Play-by-play
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_pbp_latest(season = 2027, match_id = 2928321))
#' }
elh_pbp_latest <- function(season, match_id, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/match/%d/short/%s.json", .ELH_S3, .season_start_year(season), .elh_match_id(match_id))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_pbp(raw) else raw
}

#' @title **Czech Extraliga (ELH) mobile play-by-play**
#' @description Play-by-play of one game as plain text, oldest first
#'   (`match/{season - 1}/mobile/{match_id}.json`): the same entries as
#'   [elh_pbp()], flattened to text with no team, player or game ids.
#'   `comment_id` equals the full file's `comment_id`; `game_seconds` counts from
#'   the start of the game (`0` before it).
#' @inheritParams elh_pbp
#' @return A tibble (`return_parsed = TRUE`; zero rows for a malformed body),
#'   else the raw JSON list:
#'
#'    |col_name    |type     |description|
#'    |:-----------|:--------|:----------|
#'    |comment_id  |integer  |Onlajny commentary entry id (integer); equals the mobile file's comment id.|
#'    |event_type  |character|Entry kind: goal, penalty, time (period start/end), important, normal, expertComment.|
#'    |game_seconds|integer  |Seconds from the start of the game.|
#'    |content     |character|Czech commentary text (plain, newline line breaks).|
#' @section Errors:
#' As [elh_pbp()].
#' @keywords ELH Play-by-play
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_pbp_mobile(season = 2027, match_id = 2928321))
#' }
elh_pbp_mobile <- function(season, match_id, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/match/%d/mobile/%s.json", .ELH_S3, .season_start_year(season), .elh_match_id(match_id))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_pbp_mobile(raw) else raw
}

#' @title **Czech Extraliga (ELH) shot attempts**
#' @description Shot attempts of one game with rink coordinates and result code
#'   (`visualization/shots/{match_id}.json`).
#'
#'   Ids are **hokej.cz** ids (`hokejcz_player_id`, `hokejcz_team_id`), not the
#'   onlajny ids of [elh_pbp()]. The provider's `match_shot_resutl_id` (sic) is
#'   kept and aliased `shot_result_id`: `4` = goal (`is_goal`, verified against
#'   the play-by-play); `1`/`2`/`3`/`5` are unknown. `time` is seconds from the
#'   start of the game, EXCEPT in 2016-17, where it is only the period's start
#'   offset (0/1200/2400; use `real_date`). `period` is derived from `time` and
#'   holds in both eras. The faceoff win percentage block of the file is left out
#'   (`return_parsed = FALSE` keeps it).
#' @details **Coordinate caveat (unverified).** `coordinate_x` runs along the rink
#'   LENGTH (goal to goal), centred at 0 and normalised so the home team shoots at
#'   +x and the away team at -x; `coordinate_y` runs across the WIDTH; both are
#'   about -100..100. That scaling and orientation are inferred from the data: the
#'   provider documents neither, and they have not been verified against a rink
#'   diagram or an independent source. Treat the coordinates as relative positions,
#'   not rink feet, until verified.
#' @param match_id hokej.cz game id (the S3 file name; `hokejcz_game_id` of
#'   [elh_game_ids()] / [elh_scoreboard()]), e.g. 2928321. Not the onlajny game id.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON as a list.
#' @return A tibble (`return_parsed = TRUE`; zero rows for a malformed body),
#'   else the raw JSON list:
#'
#'    |col_name            |type     |description|
#'    |:-------------------|:--------|:----------|
#'    |hokejcz_game_id     |integer  |hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer.|
#'    |game_date           |character|Game date (shots: YYYY-MM-DD string; scoreboard: Date).|
#'    |hokejcz_home_team_id|integer  |hokej.cz team id of the home team (Sparta = 8); NOT the onlajny id of the play-by-play.|
#'    |home_team_name      |character|Home team full name.|
#'    |home_team_code      |character|Home team code, e.g. SPA.|
#'    |hokejcz_away_team_id|integer  |hokej.cz team id of the away team (Vitkovice = 7); NOT the onlajny id of the play-by-play.|
#'    |away_team_name      |character|Away team full name.|
#'    |away_team_code      |character|Away team code, e.g. VIT.|
#'    |shot_id             |integer  |Feed id of the shot event.|
#'    |hokejcz_player_id   |integer  |hokej.cz player id of the shooter (Hradec = 10093378); NOT an onlajny id. No player crosswalk is published.|
#'    |jersey              |integer  |Shooter's jersey number (the join key for a per-game crosswalk with the play-by-play names).|
#'    |first_name          |character|Shooter's given name.|
#'    |last_name           |character|Shooter's family name.|
#'    |hokejcz_team_id     |integer  |hokej.cz team id of the shooting team (equals the home or away team id).|
#'    |team_side           |character|The team's side: home or away.|
#'    |match_shot_resutl_id|integer  |Shot result code, provider's typo kept: 4 = goal (verified against the play-by-play); 1/2/3/5 unknown.|
#'    |shot_result_id      |integer  |Alias of match_shot_resutl_id: 4 = goal; 1/2/3/5 unknown.|
#'    |is_goal             |logical  |True when the result code is 4 (goal).|
#'    |time                |integer  |Seconds from the start of the game; in 2016-17 only the period's start offset (0 / 1200 / 2400), use real_date.|
#'    |period              |integer  |Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry.|
#'    |coordinate_x        |numeric  |Shot position along the rink LENGTH (goal to goal), centred at 0, normalised so the home team shoots at +x and the away team at -x; about -100..100. The scaling (units) and the orientation are inferred from the data, not documented by the provider.|
#'    |coordinate_y        |numeric  |Shot position across the rink WIDTH (board to board), centred at 0; about -100..100. The scaling (each axis looks independently scaled to -100..100) and which board is positive are inferred from the data, not documented by the provider.|
#'    |real_date           |character|Wall-clock time of the shot, local YYYY-MM-DD HH:MM:SS.|
#' @section Errors:
#' `fastRhockey_no_data`: a missing file (404) or a file without shots.
#' `fastRhockey_asset_fetch`: an error envelope, a body for another game, an
#' empty or non-JSON 200, or any other non-2xx. `fastRhockey_bad_request`: HTTP
#' 400/422 or a bad `match_id`.
#' @keywords ELH Shots
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_shots(match_id = 2928321))
#' }
elh_shots <- function(match_id, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/visualization/shots/%s.json", .ELH_S3, .elh_match_id(match_id))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_shots(raw) else raw
}

#' @title **Czech Extraliga (ELH) per-minute shot totals**
#' @description Cumulative shot attempts and shots on goal of one game, per team
#'   and game minute (`visualization/events/{match_id}.json`). `shots` (attempts)
#'   and `shots_on_goal` are running totals up to the end of the minute;
#'   `team_score` is the team's final score. The file's goal list and power plays
#'   are left out (`return_parsed = FALSE` keeps them; goals are in [elh_pbp()]).
#'   The body names no game id: it is the requested `match_id`. The file starts in
#'   2017-18.
#' @inheritParams elh_shots
#' @return A tibble (`return_parsed = TRUE`; home rows, then away, by minute; zero
#'   rows for a malformed body), else the raw JSON list:
#'
#'    |col_name     |type     |description|
#'    |:------------|:--------|:----------|
#'    |start_date   |character|Scheduled start, local YYYY-MM-DD HH:MM.|
#'    |team_side    |character|The team's side: home or away.|
#'    |team_name    |character|Team full (official) name.|
#'    |team_score   |integer  |The team's final goals.|
#'    |minute       |integer  |Game minute (1-60), the end of which the totals run to.|
#'    |shots        |integer  |Cumulative shot attempts up to the end of the minute.|
#'    |shots_on_goal|integer  |Cumulative shots on goal up to the end of the minute.|
#' @section Errors:
#' `fastRhockey_no_data`: a missing file (404) or a file without minutes.
#' `fastRhockey_asset_fetch`: an error envelope, an empty or non-JSON 200, or any
#' other non-2xx. `fastRhockey_bad_request`: HTTP 400/422 or a bad `match_id`.
#' @keywords ELH Shots
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_events(match_id = 2928321))
#' }
elh_events <- function(match_id, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/visualization/events/%s.json", .ELH_S3, .elh_match_id(match_id))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_events(raw) else raw
}

#' @title **Czech Extraliga (ELH) scoreboard**
#' @description Extraliga games of one recent day with both id spaces (hokej.cz
#'   and onlajny) for games and teams (`json.esports.cz/hokejcz/scoreboard/{date}.json`).
#'   The scoreboard is the only place hokej.cz ids meet onlajny ids, which is how
#'   [elh_pbp()] joins [elh_shots()]. The window is ROLLING (recent days only), so
#'   it cannot map past seasons: an older day (and today) answers 404. Only the
#'   Tipsport extraliga (league 101) is kept; betting odds are left out
#'   (`return_parsed = FALSE` keeps every league and the odds).
#' @param date Day, `YYYY-MM-DD` (a character string or a `Date`).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name            |type     |description|
#'    |:-------------------|:--------|:----------|
#'    |league_id           |integer  |Scoreboard league key: 101 Tipsport extraliga (ELH), 82 Maxa liga, 130 para hockey.|
#'    |league_name         |character|League name, e.g. Tipsport extraliga.|
#'    |hokejcz_game_id     |integer  |hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer.|
#'    |onlajny_game_id     |integer  |Onlajny game id; the scoreboard is the only place it meets the hokej.cz game id.|
#'    |game_date           |Date     |Game date (shots: YYYY-MM-DD string; scoreboard: Date).|
#'    |start_time          |character|Start time, local HH:MM.|
#'    |status_code         |character|Game state code: K = finished (others unobserved).|
#'    |status_name         |character|Game state in Czech, e.g. konec zapasu (end of game).|
#'    |match_status        |character|Status in Czech, e.g. po zapase (after the game).|
#'    |home_score          |integer  |Home goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |away_score          |integer  |Away goals (after the entry on play-by-play rows; final on the scoreboard).|
#'    |score_periods       |character|Per-period scores home:away, comma-joined, e.g. 2:0,0:0,1:0.|
#'    |hokejcz_home_team_id|integer  |hokej.cz team id of the home team (Sparta = 8); NOT the onlajny id of the play-by-play.|
#'    |onlajny_home_team_id|integer  |Onlajny team id of the home team (Sparta = 156); joins the play-by-play onlajny_team_id.|
#'    |home_team_name      |character|Home team full name.|
#'    |home_team_short_name|character|Home team short name, e.g. Sparta.|
#'    |home_team_code      |character|Home team code, e.g. SPA.|
#'    |hokejcz_away_team_id|integer  |hokej.cz team id of the away team (Vitkovice = 7); NOT the onlajny id of the play-by-play.|
#'    |onlajny_away_team_id|integer  |Onlajny team id of the away team (Vitkovice = 168); joins the play-by-play onlajny_team_id.|
#'    |away_team_name      |character|Away team full name.|
#'    |away_team_short_name|character|Away team short name.|
#'    |away_team_code      |character|Away team code, e.g. VIT.|
#'    |stream_url          |character|Stream code; md in every sample.|
#' @section Errors:
#' `fastRhockey_no_data`: a day outside the rolling window (404) or a day without
#' an Extraliga game. `fastRhockey_asset_fetch`: an error envelope, a body dated
#' another day, an empty or non-JSON 200, or any other non-2xx.
#' `fastRhockey_bad_request`: HTTP 400/422 or a malformed `date`.
#' @keywords ELH Scoreboard
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_scoreboard(date = format(Sys.Date() - 1)))
#' }
elh_scoreboard <- function(date, return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  url <- sprintf("%s/scoreboard/%s.json", .ELH_SCOREBOARD, .elh_date(date))
  raw <- .elh_get(url)
  if (return_parsed) .elh_parse_scoreboard(raw) else raw
}

#' @title **Czech Extraliga (ELH) S3 bucket listing**
#' @description One page (up to 1,000 keys) of the hokej.cz S3 bucket listing
#'   (ListObjects V1); match keys carry the hokej.cz game id. The bucket is NOT
#'   Extraliga-only, and the listing has no league field: see [elh_game_ids()].
#'   Keys of a few hundred bytes are files written ahead of a game.
#' @param prefix Key prefix, e.g. `match/2026/full/` or `visualization/shots/`.
#' @param delimiter `/` groups keys below the next slash into CommonPrefixes
#'   (folder view).
#' @param marker ListObjects V1 paging: the last `Key` of the previous page; repeat
#'   until `IsTruncated` is false (V1 sends `NextMarker` only with a delimiter).
#' @param max_keys Page size, at most 1000 (the default).
#' @param list_type `2` switches to ListObjectsV2 (`KeyCount`,
#'   `NextContinuationToken`); omitted = V1 as documented here.
#' @param continuation_token V2 only (`list_type = 2`): `NextContinuationToken` of
#'   the previous page.
#' @param start_after V2 only (`list_type = 2`): list keys after this key.
#' @param return_parsed Parse the page into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw ListObjects XML page as text.
#' @return A tibble (`return_parsed = TRUE`; `hokejcz_game_id` is `NA` for any key
#'   that is not `match/{season}/full/{id}.json`), else the raw XML text:
#'
#'    |col_name       |type     |description|
#'    |:--------------|:--------|:----------|
#'    |hokejcz_game_id|integer  |hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer.|
#'    |key            |character|S3 object key, e.g. match/2026/full/2928255.json.|
#'    |last_modified  |character|Last write of the object, ISO-8601 UTC.|
#'    |size           |integer  |Object size in bytes (a few hundred bytes = a file written ahead of the game).|
#' @section Errors:
#' `fastRhockey_no_data`: HTTP 404. `fastRhockey_asset_fetch`: a body that is not a
#' ListBucketResult, a truncated page without a key to page on, an empty 200, or
#' any other non-2xx. `fastRhockey_bad_request`: HTTP 400/422.
#' @keywords ELH Listing
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_list_objects(prefix = "match/2026/full/", max_keys = "3"))
#' }
elh_list_objects <- function(prefix = NULL, delimiter = NULL, marker = NULL,
                             max_keys = NULL, list_type = NULL,
                             continuation_token = NULL, start_after = NULL,
                             return_parsed = TRUE) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  query <- list(
    "prefix" = prefix, "delimiter" = delimiter, "marker" = marker,
    "max-keys" = max_keys, "list-type" = list_type,
    "continuation-token" = continuation_token, "start-after" = start_after
  )
  text <- .elh_get(paste0(.ELH_S3, "/"), query)
  if (return_parsed) .elh_parse_game_ids(text) else text
}

#' @title **Czech Extraliga (ELH) game ids of a season**
#' @description Every hokej.cz game id with a play-by-play file in a season,
#'   paging the S3 listing. Lists `match/{season - 1}/full/` (the bucket files a
#'   season under its START year) a thousand keys at a time, following `marker`
#'   until the listing is complete.
#' @details **The bucket is NOT Extraliga-only.** A season folder holds every
#'   competition hokej.cz shows (Maxa liga, lower leagues, juniors, the national
#'   team): about 2,000 to 6,000 ids against about 400 ELH games, plus files
#'   written ahead of a game (a few hundred bytes; see `size`). The listing
#'   carries no league, so these ids are NOT filtered: check a game's teams with
#'   [elh_shots()] / [elh_pbp()], or, for a day still in the scoreboard's rolling
#'   window, keep the ids [elh_scoreboard()] lists.
#' @param season Season by its END year (2027 = 2026-27). Play-by-play starts in
#'   2014-15 (season 2015).
#' @return A tibble, one row per file, ordered by game id:
#'
#'    |col_name       |type     |description|
#'    |:--------------|:--------|:----------|
#'    |hokejcz_game_id|integer  |hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer.|
#'    |season         |integer  |Season END year as requested (2027 = 2026-27).|
#'    |key            |character|S3 object key, e.g. match/2026/full/2928255.json.|
#'    |last_modified  |character|Last write of the object, ISO-8601 UTC.|
#'    |size           |integer  |Object size in bytes (a few hundred bytes = a file written ahead of the game).|
#' @section Errors:
#' `fastRhockey_no_data`: the season folder holds no play-by-play file.
#' `fastRhockey_asset_fetch`: a page failed, is not a listing, or the listing does
#' not advance. `fastRhockey_bad_request`: `season` is not a year.
#' @keywords ELH Listing
#' @family ELH Functions
#' @export
#' @examples
#' \donttest{
#'   try(elh_game_ids(season = 2027))
#' }
elh_game_ids <- function(season) {
  withr::local_options(scipen = 999, stringsAsFactors = FALSE)
  start <- .season_start_year(season)
  prefix <- sprintf("match/%d/full/", start)
  url <- paste0(.ELH_S3, "/")
  pages <- list()
  marker <- NULL
  done <- FALSE
  for (i in seq_len(.ELH_MAX_PAGES)) {
    text <- .elh_get_listing(url, list(prefix = prefix, marker = marker))
    pages[[length(pages) + 1]] <- text
    state <- .elh_page_state(.elh_listing_root(url, text))
    if (!state$truncated) {
      done <- TRUE
      break
    }
    if (is.na(state$last) || identical(state$last, marker)) {
      where <- .euro_where(.ELH_S3)
      .abort_asset_fetch("{where}: the listing for {prefix} does not advance past {marker}")
    }
    marker <- state$last
  }
  if (!done) {
    where <- .euro_where(.ELH_S3)
    max_pages <- .ELH_MAX_PAGES
    .abort_asset_fetch("{where}: the listing for {prefix} did not end within {max_pages} pages")
  }
  df <- .elh_parse_game_ids(pages)
  df <- df[!is.na(df$hokejcz_game_id), , drop = FALSE]
  if (nrow(df) == 0) {
    where <- .euro_where(.ELH_S3)
    .abort_no_data("{where}: no play-by-play files under {prefix} (season {season})")
  }
  df <- df[!duplicated(df$hokejcz_game_id), , drop = FALSE]
  df$season <- as.integer(season)
  df <- df[order(df$hokejcz_game_id), c("hokejcz_game_id", "season", "key", "last_modified", "size")]
  tibble::as_tibble(df)
}
