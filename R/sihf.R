# sihf.R -- Swiss Ice Hockey Federation (SIHF) statistics wrappers.
#
# Mirrors sdv-py sportsdataverse/hockey/sihf/ (sihf_runtime.py,
# sihf_api_web_parsers.py, the generated sihf_api_web.py) and
# tools/codegen/endpoints/sihf_api_web.yaml: 33 wrappers over the keyless JSON API
# https://data.sihf.ch/statistic/api/cms (/table, /gameoverview). Community PR
# sdv-py #779 (the first 11) plus the 22 detail tables added in #826.
#
# Transport: .euro_get() (throttled to one request per second per host, retries
# included). Season: SIHF keys a season by its END year (2026 = 2025-26), so no
# season mapping is needed; the results table needs the season-wide Date range
# (01.07.<end-1>-30.06.<end>), built here as in sdv-py.
#
# Contracts measured live by sdv-py (2026-10-08 .. 2026-10-12), all kept here:
#  * the league is a scope, not a filter: searchQuery = "1//<league>";
#  * the other filters are one composite pair (filterBy / filterQuery) in the
#    order Season, Phase, Date, ContentType;
#  * an unknown value is never an error upstream: SIHF answers another season /
#    phase / league and only the body's filters[].selected echo shows it, so every
#    table answer is checked against its echo (fastRhockey_no_data on a different
#    answer, fastRhockey_asset_fetch when the answer cannot be verified);
#  * a table announcing N rows in `pages` but serving fewer is a truncated answer;
#  * stub / redirect bodies (the error shape drifts: 200 stub, later empty 501) and
#    a gameoverview body for another game are fastRhockey_asset_fetch.
#
# Ids are integers (SIHF licence numbers, team / phase / league ids), except
# game_id (14 digits, e.g. 20261105000325), which exceeds R's 32-bit integer and
# is therefore numeric (double) everywhere.

.SIHF_BASE <- "https://data.sihf.ch/statistic/api/cms"
.SIHF_FILTER_KEYS <- c("Season", "Phase", "Date", "ContentType")
.SIHF_LANGUAGES <- c("en", "de", "fr", "it")
.SIHF_PSEUDO_TEAM_ID <- 100000
.SIHF_PSEUDO_TEAM_NAMES <- c(Durchschnitt = "average", Total = "total")
.SIHF_SUMMARY_STYLES <- c("average", "total")
# The thousands separator of attendance figures (3 458 381 written with U+2019).
.SIHF_THOUSANDS <- paste0("[", intToUtf8(8217L), "']")

# Table header aliases whose string cells are counts / rates (everything else stays
# character); a value that does not parse ("", "-") is NA.
.SIHF_INT_ALIASES <- c(
  "assists", "assists1", "assists2", "blockings", "eqAssists", "eqGoals", "evenGoalsAgainst",
  "evenGoalsFor", "faceoffs", "faceoffsDefensive", "faceoffsLost", "faceoffsLostDefensive",
  "faceoffsLostNeutral", "faceoffsLostOffensive", "faceoffsNeutral", "faceoffsOffensive",
  "faceoffsWon", "faceoffsWonDefensive", "faceoffsWonNeutral", "faceoffsWonOffensive",
  "firstKeeper", "gameMisconducts", "gameWinningGoals", "gamesLostExtraTime", "gamesLostInTime",
  "gamesLostPenalty", "gamesPlayed", "gamesWonExtraTime", "gamesWonInTime", "gamesWonPenalty",
  "goals", "goalsAgainst", "goalsDifference", "goalsFor", "goalsForDf", "goalsForFw",
  "goalsForFwDf", "goalsForIm", "hattricks", "homeGames", "losses", "lossesIntime",
  "lossesOvertime", "lossesPenalty", "matchPenalties", "overtimeGoals", "penShotGoals",
  "penShotGoalsAgainst", "penShotSaves", "penShotWinningGoals", "penShots", "penShotsAgainst",
  "penShotsMissed", "penaltyInMinutes", "pim10", "pim2", "pim5", "pimTotal", "pkAssists",
  "pkGoals", "pkGoalsAgainst", "pkSituations", "playerNumber", "plusMinus", "points",
  "ppAssists", "ppGoals", "ppGoalsAgainst", "ppGoalsFor", "ppOpportunities", "rank", "saves",
  "shGoals", "shifts", "shotsAgainst", "shotsOnGoal", "shutout", "sogMissed", "sogPost",
  "sogSog", "sogSogSlot", "sogaMissed", "sogaPost", "sogaSog", "sogaSogSlot", "sowGoalsAgainst",
  "sowGoalsFor", "spectators", "wins", "winsIntime", "winsOvertime", "winsPenalty"
)

.SIHF_FLOAT_ALIASES <- c(
  "blockingsPerGame", "evenGoalsAgainstQuotient", "evenGoalsForQuotient",
  "faceoffsDefensivePerGame", "faceoffsDefensivePercent", "faceoffsLostDefensivePerGame",
  "faceoffsLostNeutralPerGame", "faceoffsLostOffensivePerGame", "faceoffsLostPerGame",
  "faceoffsNeutralPerGame", "faceoffsNeutralPercent", "faceoffsOffensivePerGame",
  "faceoffsOffensivePercent", "faceoffsPerGame", "faceoffsPercent",
  "faceoffsWonDefensivePerGame", "faceoffsWonNeutralPerGame", "faceoffsWonOffensivePerGame",
  "faceoffsWonPerGame", "goalsAgainstAverage", "goalsAgainstQuotient", "goalsForDfPercent",
  "goalsForFwPercent", "goalsForImPercent", "goalsForQuotient", "penShotSavesPercent",
  "penShotsPercent", "pkGoalsAgainstQuotient", "pkPercent", "pkSituationsQuotient",
  "pointsPerGame", "pointsQuotient", "ppGoalsAgainstQuotient", "ppGoalsForQuotient",
  "ppOpportunitiesQuotient", "ppPercent", "savesPercentage", "savesSlotPercentage",
  "shGoalsQuotient", "shiftsPerGame", "sogMissedPerGame", "sogPostPerGame", "sogSogPercent",
  "sogSogQuotient", "sogSogSlotPerGame", "sogaMissedPerGame", "sogaPostPerGame",
  "sogaSogPerGame", "sogaSogSlotPerGame", "spectatorsQuotient", "stadiumCapacityPercent",
  "stadiumCapacityQuotient"
)

# ---- naming -----------------------------------------------------------------

# sdv-py dl_utils.underscore() + the replace() calls of _col().
.sihf_snake <- function(x) {
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("-", "_", x, fixed = TRUE)
  x <- tolower(x)
  x <- gsub(" ", "_", x, fixed = TRUE)
  gsub(".", "_", x, fixed = TRUE)
}
.SIHF_INT_COLS <- vapply(.SIHF_INT_ALIASES, .sihf_snake, character(1), USE.NAMES = FALSE)
.SIHF_FLOAT_COLS <- vapply(.SIHF_FLOAT_ALIASES, .sihf_snake, character(1), USE.NAMES = FALSE)

# Season-table columns whose dtype neither their header alias nor the *_id rule gives.
.SIHF_TABLE_DTYPES <- c(
  season = "integer", date = "Date", score_home = "integer", score_away = "integer",
  score_per_period_home = "list_integer", score_per_period_away = "list_integer",
  status_percent = "integer", status_canceled = "logical"
)

# The documented dtype of a season-table column (character when the parser does not type it).
.sihf_table_dtype <- function(col) {
  if (col %in% names(.SIHF_TABLE_DTYPES)) return(.SIHF_TABLE_DTYPES[[col]])
  if (identical(col, "game_id")) return("double")
  if (identical(col, "id") || endsWith(col, "_id") || col %in% .SIHF_INT_COLS) return("integer")
  if (col %in% .SIHF_FLOAT_COLS) "double" else "character"
}

# ---- scalar helpers (JSON as simplifyVector = FALSE) --------------------------

.sihf_is_obj <- function(x) is.list(x) && !is.null(names(x))
.sihf_is_array <- function(x) is.list(x) && is.null(names(x))
.sihf_obj <- function(x) if (.sihf_is_obj(x)) x else list()
.sihf_arr <- function(x) if (.sihf_is_array(x)) x else list()
.sihf_na <- function(x) if (is.null(x) || !length(x)) NA else x

# Python str() of a JSON scalar; NA for null / arrays / objects.
.sihf_str <- function(x) {
  if (is.null(x) || length(x) != 1L || is.list(x)) return(NA_character_)
  if (is.character(x)) return(x)
  if (is.logical(x)) return(if (is.na(x)) NA_character_ else if (x) "True" else "False")
  if (is.numeric(x)) {
    if (is.na(x)) return(NA_character_)
    return(if (x == round(x)) sprintf("%.0f", x) else format(x, digits = 15, scientific = FALSE, trim = TRUE))
  }
  as.character(x)
}

# _int(): a whole number or a digit string (thousands separator dropped); NA otherwise.
.sihf_int <- function(v) {
  if (is.null(v) || length(v) != 1L || is.list(v) || is.logical(v)) return(NA_real_)
  if (is.numeric(v)) return(if (is.finite(v) && v == round(v)) as.double(v) else NA_real_)
  if (is.character(v)) {
    t <- gsub(.SIHF_THOUSANDS, "", trimws(v))
    return(if (grepl("^-?[0-9]+$", t)) as.numeric(t) else NA_real_)
  }
  NA_real_
}

# _licence(): a SIHF licence number / id; 0 (the API's "nobody") is NA.
.sihf_licence <- function(v) {
  n <- .sihf_int(v)
  if (is.na(n) || n == 0) NA_real_ else n
}

# _blank(): "" and SIHF's "-" missing-value placeholder are NA.
.sihf_blank <- function(v) {
  if (is.character(v) && length(v) == 1L && !is.na(v) && trimws(v) %in% c("", "-")) NA else v
}

# _clock_seconds(): "65:56" -> 3956; NA when malformed.
.sihf_clock_seconds <- function(v) {
  s <- trimws(.sihf_str(v))
  if (is.na(s)) return(NA_real_)
  m <- regmatches(s, regexec("^([0-9]+):([0-9]{2})$", s))[[1]]
  if (length(m) == 3L) as.numeric(m[2]) * 60 + as.numeric(m[3]) else NA_real_
}

# _joined(): the present values joined by `sep`; NA when there are none.
.sihf_joined <- function(values, sep) {
  text <- vapply(values, .sihf_str, character(1), USE.NAMES = FALSE)
  text <- text[!is.na(text) & nzchar(trimws(text))]
  if (length(text)) paste(text, collapse = sep) else NA_character_
}

# Parse a value as a count (strict: NA unless it is all digits after the separator is dropped).
.sihf_cast_int <- function(v) {
  if (is.numeric(v) && length(v) == 1L) return(.sihf_int(v))
  s <- .sihf_str(v)
  if (is.na(s)) return(NA_real_)
  t <- gsub(.SIHF_THOUSANDS, "", s)
  if (grepl("^[+-]?[0-9]+$", t)) as.numeric(t) else NA_real_
}
.sihf_cast_float <- function(v) {
  if (is.numeric(v) && length(v) == 1L) return(as.double(v))
  s <- .sihf_str(v)
  if (is.na(s)) return(NA_real_)
  t <- gsub(.SIHF_THOUSANDS, "", s)
  if (grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", t)) as.numeric(t) else NA_real_
}
.sihf_cast_date <- function(v) {
  s <- .sihf_str(v)
  if (is.na(s) || !grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$", s)) return(as.Date(NA))
  as.Date(s, format = "%d.%m.%Y")
}

# ---- columns ----------------------------------------------------------------

.sihf_as_integer <- function(x, col) {
  # Widen to double past R's 32-bit integer (as khl/shl/liiga do) instead of aborting.
  if (any(abs(x) > .Machine$integer.max, na.rm = TRUE)) return(as.double(x))
  as.integer(x)
}

# One column from per-row values, typed by a dtype code: integer, double, character,
# logical, Date, list_integer or list_character (_coerce() + _typed()).
.sihf_column <- function(vals, type, col = "") {
  n <- length(vals)
  switch(
    type,
    integer = .sihf_as_integer(vapply(vals, .sihf_cast_int, numeric(1), USE.NAMES = FALSE), col),
    double = vapply(vals, .sihf_cast_float, numeric(1), USE.NAMES = FALSE),
    character = vapply(vals, function(v) {
      if (is.null(v) || is.list(v) || is.logical(v) || length(v) != 1L) NA_character_ else .sihf_str(v)
    }, character(1), USE.NAMES = FALSE),
    logical = vapply(vals, function(v) if (is.logical(v) && length(v) == 1L) v else NA, logical(1),
                     USE.NAMES = FALSE),
    Date = do.call(c, c(list(as.Date(character())), lapply(vals, .sihf_cast_date))),
    list_integer = lapply(vals, function(v) {
      if (is.list(v) || (is.numeric(v) && !(length(v) == 1L && is.na(v)))) {
        .sihf_as_integer(vapply(as.list(v), .sihf_int, numeric(1)), col)
      } else NULL
    }),
    list_character = lapply(vals, function(v) {
      if (is.list(v) || (is.character(v) && !(length(v) == 1L && is.na(v)))) {
        vapply(as.list(v), function(e) if (is.character(e) && length(e) == 1L) e else
          if (is.numeric(e) && length(e) == 1L) .sihf_str(e) else NA_character_, character(1))
      } else NULL
    }),
    .abort_bad_request("Unknown SIHF column type {.val {type}}.")
  )
}

# Zero-row tibble with the documented columns (list columns keep their position).
.sihf_empty <- function(types) {
  is_list <- startsWith(unname(types), "list_")
  base <- types
  base[is_list] <- "character"
  out <- .empty_tibble(base)
  for (nm in names(types)[is_list]) out[[nm]] <- list()
  out
}

.sihf_frame <- function(rows, schema) {
  if (!length(rows)) return(.sihf_empty(schema))
  out <- lapply(names(schema), function(cn) {
    .sihf_column(lapply(rows, function(r) r[[cn]]), schema[[cn]], cn)
  })
  names(out) <- names(schema)
  tibble::as_tibble(out)
}

# ---- table cells ------------------------------------------------------------

# One table cell -> named list of values; a typed object becomes several columns.
.sihf_expand_cell <- function(col, value) {
  if (!.sihf_is_obj(value)) {
    out <- list(.sihf_na(.sihf_blank(value)))
    names(out) <- col
    return(out)
  }
  kind <- .sihf_str(value[["type"]])
  out <- switch(
    if (is.na(kind)) "unknown" else kind,
    team = list(.sihf_licence(value[["id"]]), .sihf_na(value[["name"]]), .sihf_na(value[["acronym"]])),
    result = list(.sihf_int(value[["homeTeam"]]), .sihf_int(value[["awayTeam"]])),
    periods = list(vapply(.sihf_arr(value[["homeTeam"]]), .sihf_int, numeric(1)),
                   vapply(.sihf_arr(value[["awayTeam"]]), .sihf_int, numeric(1))),
    gameStatus = list(.sihf_int(value[["id"]]), .sihf_na(value[["name"]]), .sihf_int(value[["percent"]]),
                      .sihf_na(value[["canceled"]]), .sihf_na(value[["startDateTime"]]),
                      .sihf_na(value[["endDateTime"]]), .sihf_na(value[["updateDateTime"]])),
    linkToGameDetail = list(.sihf_int(value[["gameId"]])),
    broadcasts = list(.sihf_joined(.sihf_arr(value[["entries"]]), ",")),
    list(.sihf_licence(value[["id"]]), .sihf_na(value[["name"]]))
  )
  names(out) <- switch(
    if (is.na(kind)) "unknown" else kind,
    team = paste0(col, c("_id", "_name", "_acronym")),
    result = paste0(col, c("_home", "_away")),
    periods = paste0(col, c("_home", "_away")),
    gameStatus = c(paste0(col, c("_id", "_name", "_percent", "_canceled")),
                   "start_date_time", "end_date_time", "update_date_time"),
    linkToGameDetail = "game_id",
    broadcasts = col,
    paste0(col, c("_id", "_name"))
  )
  out
}

.sihf_merge <- function(row, add) {
  for (nm in names(add)) row[[nm]] <- .sihf_na_keep(add[[nm]])
  row
}
# Keep zero-length vectors (a period list) but never NULL (which would delete the element).
.sihf_na_keep <- function(x) if (is.null(x)) NA else x

# The filter echo (season / phase_id) of a table body; NA when not echoed.
.sihf_echoed <- function(raw) {
  names_map <- c(Season = "season", Phase = "phase_id")
  out <- list(season = NA_real_, phase_id = NA_real_)
  for (f in .sihf_arr(if (.sihf_is_obj(raw)) raw[["filters"]] else NULL)) {
    alias <- if (.sihf_is_obj(f)) f[["alias"]] else NULL
    if (is.character(alias) && length(alias) == 1L && alias %in% names(names_map)) {
      out[[names_map[[alias]]]] <- .sihf_int(f[["selected"]])
    }
  }
  out
}

# A SIHF table object -> list of row lists (NULL when unusable).
.sihf_table_rows <- function(raw, extra = list()) {
  if (!.sihf_is_obj(raw)) return(NULL)
  header <- Filter(.sihf_is_obj, .sihf_arr(raw[["header"]]))
  data <- raw[["data"]]
  if (!length(header) || !.sihf_is_array(data)) return(NULL)
  aliases <- vapply(header, function(h) {
    a <- .sihf_str(h[["alias"]])
    if (is.na(a)) "None" else a
  }, character(1))
  cols <- vapply(aliases, .sihf_snake, character(1), USE.NAMES = FALSE)
  summary <- character()
  for (hp in .sihf_arr(raw[["highlightedPositions"]])) {
    style <- if (.sihf_is_obj(hp)) hp[["style"]] else NULL
    if (is.character(style) && length(style) == 1L && style %in% .SIHF_SUMMARY_STYLES) {
      for (r in .sihf_arr(hp[["rows"]])) {
        if (is.numeric(r) && length(r) == 1L && !is.na(r)) summary[as.character(r - 1)] <- style  # 1-based
      }
    }
  }
  rows <- list()
  for (i in seq_along(data)) {
    cells <- data[[i]]
    if (!.sihf_is_array(cells)) next
    row <- extra
    for (j in seq_len(min(length(header), length(cells)))) {
      row <- .sihf_merge(row, .sihf_expand_cell(cols[[j]], cells[[j]]))
    }
    # A pseudo-team row is a summary row even when highlightedPositions does not tag it, and its
    # team id (100000) and acronym are not a team's: null them so they never join to one.
    tn <- row[["team_name"]]
    name <- if (is.character(tn) && length(tn) == 1L && !is.na(tn)) tn else ""
    tid <- row[["team_id"]]
    pseudo <- (is.numeric(tid) && length(tid) == 1L && !is.na(tid) && tid == .SIHF_PSEUDO_TEAM_ID) ||
      name %in% names(.SIHF_PSEUDO_TEAM_NAMES)
    tagged <- summary[as.character(i - 1)]
    row[["row_type"]] <- if (!is.na(tagged)) unname(tagged) else if (pseudo) {
      if (name %in% names(.SIHF_PSEUDO_TEAM_NAMES)) unname(.SIHF_PSEUDO_TEAM_NAMES[[name]]) else "average"
    } else "data"
    if (pseudo) {
      for (cn in c("team_id", "team_acronym")) if (!is.null(row[[cn]])) row[[cn]] <- NA
    }
    rows[[length(rows) + 1L]] <- row
  }
  rows
}

# List of row lists -> tibble, each column typed by .sihf_table_dtype() (NULL for no rows).
.sihf_assemble <- function(rows) {
  if (!length(rows)) return(NULL)
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  out <- lapply(cols, function(cn) {
    .sihf_column(lapply(rows, function(r) r[[cn]]), .sihf_table_dtype(cn), cn)
  })
  names(out) <- cols
  tibble::as_tibble(out)
}

# parse_sihf_table(): one row per table row; a zero-row tibble with the `columns`
# schema when the payload is empty or malformed (no columns when `columns` is NULL).
.sihf_parse_table <- function(raw, columns = NULL) {
  df <- .sihf_assemble(.sihf_table_rows(raw, .sihf_echoed(raw)))
  if (is.null(df)) {
    if (is.null(columns)) return(tibble::tibble())
    return(.sihf_empty(stats::setNames(vapply(columns, .sihf_table_dtype, character(1)), columns)))
  }
  df
}

# ---- phases -----------------------------------------------------------------

.SIHF_PHASES_SCHEMA <- c(season = "integer", phase_id = "integer", phase_name = "character", n_games = "integer")

.sihf_parse_phases <- function(raw) {
  season <- .sihf_echoed(raw)$season
  # Only the results table (the one with home / away team columns) counts games per phase.
  counts_games <- .sihf_is_obj(raw) && any(vapply(.sihf_arr(raw[["header"]]), function(h) {
    .sihf_is_obj(h) && identical(h[["alias"]], "homeTeam")
  }, logical(1)))
  rows <- list()
  for (f in .sihf_arr(if (.sihf_is_obj(raw)) raw[["filters"]] else NULL)) {
    if (!(.sihf_is_obj(f) && identical(f[["alias"]], "Phase"))) next
    for (e in .sihf_arr(f[["entries"]])) {
      phase_id <- if (.sihf_is_obj(e)) .sihf_int(e[["alias"]]) else NA_real_
      if (is.na(phase_id)) next  # the "all" pseudo-entry
      nm <- e[["name"]]
      name <- if (is.character(nm) && length(nm) == 1L && !is.na(nm) && nzchar(trimws(nm))) trimws(nm) else NA_character_
      m <- if (is.na(name)) character() else regmatches(name, regexec("^(.*?)\\s*\\(([0-9]+)\\)$", name, perl = TRUE))[[1]]
      rows[[length(rows) + 1L]] <- list(
        season = season, phase_id = phase_id,
        phase_name = if (length(m)) m[2] else name,
        n_games = if (length(m) && counts_games) as.numeric(m[3]) else NA_real_
      )
    }
  }
  .sihf_frame(rows, .SIHF_PHASES_SCHEMA)
}

# ---- the game page ----------------------------------------------------------

.sihf_game <- function(raw) {
  gid <- if (.sihf_is_obj(raw)) raw[["gameId"]] else NULL
  ok <- !is.null(gid) && length(gid) == 1L && !is.list(gid) && !is.na(gid) && !identical(gid, "") &&
    !identical(gid, 0L) && !identical(gid, 0) && !identical(gid, FALSE)
  if (ok) raw else list()
}

# {team_id: "home" | "away"} from details$homeTeam / awayTeam.
.sihf_sides <- function(game) {
  details <- .sihf_obj(game[["details"]])
  out <- list()
  for (side in c("home", "away")) {
    tid <- .sihf_licence(.sihf_obj(details[[paste0(side, "Team")]])[["id"]])
    if (!is.na(tid)) out[[.sihf_str(tid)]] <- side
  }
  out
}

# {licence: player} from the `players` list.
.sihf_roster <- function(game) {
  out <- list()
  for (p in .sihf_arr(game[["players"]])) {
    if (!.sihf_is_obj(p)) next
    pid <- .sihf_licence(p[["id"]])
    if (!is.na(pid)) out[[.sihf_str(pid)]] <- p
  }
  out
}

# <prefix>_id / <prefix>_name (+ player_jersey for the player) for a licence number.
.sihf_person <- function(roster, licence, prefix = "player") {
  pid <- .sihf_licence(licence)
  p <- if (is.na(pid)) list() else .sihf_obj(roster[[.sihf_str(pid)]])
  out <- list(pid, .sihf_na(p[["fullName"]]))
  names(out) <- paste0(prefix, c("_id", "_name"))
  if (identical(prefix, "player")) out[["player_jersey"]] <- .sihf_int(p[["jerseyNumber"]])
  out
}

.sihf_stats_table <- function(game, alias) {
  for (t in .sihf_arr(game[["stats"]])) {
    if (.sihf_is_obj(t) && identical(t[["alias"]], alias)) return(t)
  }
  NULL
}

.SIHF_PBP_SCHEMA <- c(
  game_id = "double", event_index = "integer", period_number = "integer", period = "character",
  event_type = "character", time = "character", game_seconds = "integer", team_id = "integer",
  team_side = "character", player_id = "integer", player_name = "character",
  player_jersey = "integer", assist1_player_id = "integer", assist1_player_name = "character",
  assist2_player_id = "integer", assist2_player_name = "character", goalie_id = "integer",
  goalie_name = "character", strength = "character", goal_tags = "character",
  score_home = "integer", score_away = "integer", plus_player_ids = "list_integer",
  minus_player_ids = "list_integer", penalty_id = "integer", penalty_name = "character",
  penalty_minutes = "integer", penalty_start = "character", penalty_end = "character",
  goalie_change = "character", revoke_reason = "character", shootout_number = "integer",
  shootout_scored = "logical", text = "character"
)

.SIHF_GOAL_HEAD <- "^\\*\\*([^*]*)\\*\\*\\s*/\\s*\\*\\*([0-9]+):([0-9]+)\\*\\*"
.SIHF_FIRST_BOLD <- "^\\*\\*([^*]+)\\*\\*"
.SIHF_GOALIE_CHANGE <- "\\((IN|OUT)\\)\\s*$"
# Same-clock tie-break: the order SIHF's own timeline prints events in.
.SIHF_TYPE_ORDER <- c(penalty = 0, goal = 1, revoked_goal = 2, goalie = 3)

.sihf_parse_pbp <- function(raw) {
  game <- .sihf_game(raw)
  if (!length(game)) return(.sihf_frame(list(), .SIHF_PBP_SCHEMA))
  gid <- .sihf_int(game[["gameId"]])
  sides <- .sihf_sides(game)
  roster <- .sihf_roster(game)
  who <- function(prefix, licence) .sihf_person(roster, licence, prefix)
  side_of <- function(tid) if (is.na(tid)) NA else .sihf_na(sides[[.sihf_str(tid)]])
  events <- list()
  periods <- Filter(.sihf_is_obj, .sihf_arr(.sihf_obj(game[["summary"]])[["periods"]]))
  kinds <- list(c("penalty", "fouls"), c("goal", "goals"), c("revoked_goal", "revokedGoals"),
                c("goalie", "goalkeepers"))
  for (number in seq_along(periods)) {
    period <- periods[[number]]
    for (kk in kinds) {
      kind <- kk[[1]]
      for (e in .sihf_arr(period[[kk[[2]]]])) {
        if (!.sihf_is_obj(e)) next
        tid <- .sihf_licence(e[["teamId"]])
        text <- if (is.character(e[["text"]]) && length(e[["text"]]) == 1L && !is.na(e[["text"]])) e[["text"]] else ""
        sort_order <- .sihf_int(e[["sortOrder"]])
        row <- list(
          period_number = number, period = .sihf_na(period[["name"]]), event_type = kind,
          time = .sihf_na(e[["time"]]), team_id = tid, team_side = side_of(tid), text = text,
          .order_type = .SIHF_TYPE_ORDER[[kind]], .order_sort = if (is.na(sort_order)) 0 else sort_order
        )
        if (kind %in% c("goal", "revoked_goal")) {
          row <- .sihf_merge(row, who("player", e[["scorerLicenceNr"]]))
          row <- .sihf_merge(row, who("assist1_player", e[["assist1LicenceNr"]]))
          row <- .sihf_merge(row, who("assist2_player", e[["assist2LicenceNr"]]))
        }
        if (identical(kind, "goal")) {
          m <- regmatches(text, regexec(.SIHF_GOAL_HEAD, text, perl = TRUE))[[1]]
          if (length(m)) {
            tags <- trimws(m[2])
            strength <- trimws(strsplit(tags, ",", fixed = TRUE)[[1]][1])
            row <- .sihf_merge(row, list(
              goal_tags = if (nzchar(tags)) tags else NA_character_,
              strength = if (!is.na(strength) && nzchar(strength)) strength else NA_character_,
              score_home = as.numeric(m[3]), score_away = as.numeric(m[4])
            ))
          }
          licences <- function(prefix) {
            v <- vapply(1:6, function(i) .sihf_licence(e[[sprintf("%s%dLicenceNr", prefix, i)]]), numeric(1))
            v[!is.na(v)]
          }
          row[["plus_player_ids"]] <- licences("p")
          row[["minus_player_ids"]] <- licences("m")
        } else if (identical(kind, "revoked_goal")) {
          m <- regmatches(text, regexec(.SIHF_FIRST_BOLD, text, perl = TRUE))[[1]]
          row[["revoke_reason"]] <- if (length(m)) trimws(m[2]) else NA_character_
        } else if (identical(kind, "penalty")) {
          row <- .sihf_merge(row, who("player", e[["playerLicenceNr"]]))
          row <- .sihf_merge(row, list(
            penalty_id = .sihf_int(e[["id"]]), penalty_name = .sihf_na(e[["name"]]),
            penalty_minutes = .sihf_int(e[["minutes"]]), penalty_start = .sihf_na(e[["startTime"]]),
            penalty_end = .sihf_na(e[["endTime"]])
          ))
        } else {  # goalie
          row <- .sihf_merge(row, who("player", e[["playerLicenceNr"]]))
          m <- regmatches(text, regexec(.SIHF_GOALIE_CHANGE, text, perl = TRUE))[[1]]
          row[["goalie_change"]] <- if (length(m)) tolower(m[2]) else NA_character_
        }
        events[[length(events) + 1L]] <- row
      }
    }
  }
  if (length(events)) {
    clock <- vapply(events, function(r) .sihf_clock_seconds(r[["time"]]), numeric(1))
    ord <- order(is.na(clock), ifelse(is.na(clock), 0, clock),
                 vapply(events, function(r) r[[".order_type"]], numeric(1)),
                 vapply(events, function(r) r[[".order_sort"]], numeric(1)))
    events <- events[ord]
  }
  shootout <- .sihf_obj(.sihf_obj(game[["summary"]])[["shootout"]])
  for (s in .sihf_arr(shootout[["shoots"]])) {
    if (!.sihf_is_obj(s)) next
    shooter <- who("player", s[["scorerLicenceNr"]])
    tid <- if (!is.na(shooter$player_id)) {
      .sihf_licence(.sihf_obj(roster[[.sihf_str(shooter$player_id)]])[["teamId"]])
    } else NA_real_
    # `goalkeeperLiceneNr` (sic) is the API's spelling.
    goalie_licence <- if ("goalkeeperLiceneNr" %in% names(s)) s[["goalkeeperLiceneNr"]] else s[["goalkeeperLicenceNr"]]
    row <- list(
      period_number = length(periods) + 1, period = .sihf_na(shootout[["name"]]), event_type = "shootout",
      team_id = tid, team_side = side_of(tid), shootout_number = .sihf_int(s[["number"]]),
      shootout_scored = if (is.logical(s[["scored"]]) && length(s[["scored"]]) == 1L) s[["scored"]] else NA,
      text = .sihf_na(s[["text"]])
    )
    row <- .sihf_merge(row, shooter)
    events[[length(events) + 1L]] <- .sihf_merge(row, who("goalie", goalie_licence))
  }
  rows <- lapply(seq_along(events), function(i) {
    e <- events[[i]]
    e[[".order_type"]] <- NULL
    e[[".order_sort"]] <- NULL
    c(list(game_id = gid, event_index = i, game_seconds = .sihf_clock_seconds(e[["time"]])), e)
  })
  .sihf_frame(rows, .SIHF_PBP_SCHEMA)
}

# The per-side stats tables {alias}Home + {alias}Away as row lists with side / team / player ids.
.sihf_side_rows <- function(game, alias) {
  details <- .sihf_obj(game[["details"]])
  # (team, jersey) -> licence; a jersey two roster players share maps to NA rather than a guess.
  by_jersey <- list()
  for (p in .sihf_roster(game)) {
    tid <- .sihf_licence(p[["teamId"]])
    jersey <- .sihf_int(p[["jerseyNumber"]])
    pid <- .sihf_licence(p[["id"]])
    if (!is.na(tid) && !is.na(jersey)) {
      key <- paste(.sihf_str(tid), .sihf_str(jersey))
      by_jersey[[key]] <- if (key %in% names(by_jersey)) NA_real_ else pid
    }
  }
  out <- list()
  for (side in c("home", "away")) {
    table <- .sihf_stats_table(game, paste0(alias, toupper(substring(side, 1, 1)), substring(side, 2)))
    team <- .sihf_obj(details[[paste0(side, "Team")]])
    tid <- .sihf_licence(team[["id"]])
    rows <- .sihf_table_rows(table, list(
      game_id = .sihf_int(game[["gameId"]]), team_side = side, team_id = tid, team_name = .sihf_na(team[["name"]])
    ))
    if (!length(rows)) next
    if (any(vapply(rows, function(r) "player_number" %in% names(r), logical(1)))) {
      rows <- lapply(rows, function(r) {
        n <- .sihf_int(r[["player_number"]])
        r[["player_id"]] <- if (is.na(n) || is.na(tid)) NA_real_ else .sihf_na(by_jersey[[paste(.sihf_str(tid), .sihf_str(n))]])
        r
      })
    }
    out <- c(out, rows)
  }
  out
}

# The side tables in the documented column order and dtypes; unexpected extra columns are kept at the end.
.sihf_conform <- function(df, schema) {
  if (is.null(df)) return(.sihf_empty(schema))
  out <- lapply(names(schema), function(cn) {
    if (cn %in% names(df)) {
      x <- df[[cn]]
      if (!identical(.sihf_class_code(x), schema[[cn]])) x <- .sihf_column(as.list(x), schema[[cn]], cn)
      x
    } else .sihf_column(as.list(rep(NA, nrow(df))), schema[[cn]], cn)
  })
  names(out) <- names(schema)
  extra <- setdiff(names(df), names(schema))
  tibble::as_tibble(c(out, as.list(df[extra])))
}
.sihf_class_code <- function(x) {
  if (is.integer(x)) "integer" else if (is.double(x) && !inherits(x, "Date")) "double"
  else if (is.character(x)) "character" else if (is.logical(x)) "logical" else if (inherits(x, "Date")) "Date"
  else "other"
}

.SIHF_GAME_PLAYER_SCHEMA <- c(
  game_id = "double", team_side = "character", team_id = "integer", team_name = "character",
  player_number = "integer", player = "character", player_position = "character",
  goals = "integer", assists = "integer", assists1 = "integer", assists2 = "integer",
  points = "integer", pim_total = "integer", plus_minus = "integer", shots_on_goal = "integer",
  sog_missed = "integer", sog_post = "integer", blockings = "integer", faceoffs_won = "integer",
  faceoffs_lost = "integer", faceoffs_percent = "double", time_on_ice = "character",
  time_on_ice_pp = "character", time_on_ice_pk = "character", row_type = "character",
  player_id = "integer"
)

.SIHF_GAME_GOALIE_SCHEMA <- c(
  game_id = "double", team_side = "character", team_id = "integer", team_name = "character",
  player_number = "integer", player = "character", goals_against = "integer", saves = "integer",
  shots_against = "integer", saves_percentage = "double", seconds_played = "character",
  goals_against_average = "double", penalty_in_minutes = "integer", row_type = "character",
  player_id = "integer"
)

.sihf_parse_game_player_stats <- function(raw) {
  .sihf_conform(.sihf_assemble(.sihf_side_rows(.sihf_game(raw), "gamePlayerStats")), .SIHF_GAME_PLAYER_SCHEMA)
}
.sihf_parse_game_goalie_stats <- function(raw) {
  .sihf_conform(.sihf_assemble(.sihf_side_rows(.sihf_game(raw), "gameGoalieStats")), .SIHF_GAME_GOALIE_SCHEMA)
}

.SIHF_TEAM_STATS_SCHEMA <- c(game_id = "double", statistic = "character", home_value = "character",
                             away_value = "character")

.sihf_parse_game_team_stats <- function(raw) {
  game <- .sihf_game(raw)
  rows <- list()
  for (cells in .sihf_arr(.sihf_obj(.sihf_stats_table(game, "gameTeamStats"))[["data"]])) {
    if (.sihf_is_array(cells) && length(cells) >= 3L) {
      rows[[length(rows) + 1L]] <- list(
        game_id = .sihf_int(game[["gameId"]]), statistic = .sihf_na(cells[[1]]),
        home_value = .sihf_na(.sihf_blank(cells[[2]])), away_value = .sihf_na(.sihf_blank(cells[[3]]))
      )
    }
  }
  .sihf_frame(rows, .SIHF_TEAM_STATS_SCHEMA)
}

.SIHF_INFO_SCHEMA <- c(
  game_id = "double", season = "integer", round = "integer", league_id = "integer",
  league_name = "character", region_id = "integer", region_name = "character",
  group_id = "integer", group_name = "character", tournament_id = "integer",
  tournament_name = "character", qualification_id = "integer", qualification_name = "character",
  phase_id = "integer", phase_name = "character", start_date_time = "character",
  end_date_time = "character", update_date_time = "character", status_id = "integer",
  status_name = "character", status_percent = "integer", status_canceled = "logical",
  home_team_id = "integer", home_team_name = "character", home_team_acronym = "character",
  away_team_id = "integer", away_team_name = "character", away_team_acronym = "character",
  home_score = "integer", away_score = "integer", home_score_by_period = "list_integer",
  away_score_by_period = "list_integer", home_shots_by_period = "list_integer",
  away_shots_by_period = "list_integer", period_indicators = "list_character",
  venue_name = "character", venue_city = "character", spectators = "integer",
  referees = "character", linesmen = "character", broadcasts = "character"
)

.sihf_parse_game_info <- function(raw) {
  game <- .sihf_game(raw)
  if (!length(game)) return(.sihf_frame(list(), .SIHF_INFO_SCHEMA))
  details <- .sihf_obj(game[["details"]])
  result <- .sihf_obj(game[["result"]])
  status <- .sihf_obj(game[["status"]])
  venue <- .sihf_obj(details[["venue"]])
  row <- list(game_id = .sihf_int(game[["gameId"]]), season = .sihf_int(game[["season"]]),
              round = .sihf_int(game[["round"]]))
  for (key in c("league", "region", "group", "tournament", "qualification", "phase")) {
    block <- .sihf_obj(game[[key]])
    row[[paste0(key, "_id")]] <- .sihf_int(block[["id"]])
    row[[paste0(key, "_name")]] <- .sihf_na(block[["name"]])
  }
  row <- .sihf_merge(row, list(
    start_date_time = .sihf_na(game[["startDateTime"]]), end_date_time = .sihf_na(game[["endDateTime"]]),
    update_date_time = .sihf_na(game[["updateDateTime"]]), status_id = .sihf_int(status[["id"]]),
    status_name = .sihf_na(status[["name"]]), status_percent = .sihf_int(status[["percent"]]),
    status_canceled = if (is.logical(status[["canceled"]]) && length(status[["canceled"]]) == 1L) status[["canceled"]] else NA
  ))
  scores <- Filter(.sihf_is_obj, .sihf_arr(result[["scores"]]))
  sogs <- Filter(.sihf_is_obj, .sihf_arr(result[["sogs"]]))
  for (side in c("home", "away")) {
    team <- .sihf_obj(details[[paste0(side, "Team")]])
    row <- .sihf_merge(row, stats::setNames(list(
      .sihf_licence(team[["id"]]), .sihf_na(team[["name"]]), .sihf_na(team[["acronym"]]),
      .sihf_int(result[[paste0(side, "Team")]]),
      vapply(scores, function(s) .sihf_int(s[[paste0(side, "Team")]]), numeric(1)),
      vapply(sogs, function(s) .sihf_int(s[[paste0(side, "Team")]]), numeric(1))
    ), paste0(side, c("_team_id", "_team_name", "_team_acronym", "_score", "_score_by_period",
                      "_shots_by_period"))))
  }
  # aligned with the per-period lists, so a missing indicator stays an NA element (never "None")
  row[["period_indicators"]] <- vapply(scores, function(s) .sihf_str(s[["indicator"]]), character(1))
  row <- .sihf_merge(row, list(
    venue_name = .sihf_na(venue[["name"]]), venue_city = .sihf_na(venue[["city"]]),
    spectators = .sihf_int(venue[["spectators"]]),
    referees = .sihf_joined(lapply(.sihf_arr(details[["referees"]]), function(o) .sihf_obj(o)[["fullName"]]), ", "),
    linesmen = .sihf_joined(lapply(.sihf_arr(details[["linesmen"]]), function(o) .sihf_obj(o)[["fullName"]]), ", "),
    broadcasts = .sihf_joined(.sihf_arr(.sihf_obj(game[["broadcasts"]])[["entries"]]), ",")
  ))
  .sihf_frame(list(row), .SIHF_INFO_SCHEMA)
}

.SIHF_LINEUP_SCHEMA <- c(
  game_id = "double", team_side = "character", team_id = "integer", unit = "character",
  slot = "integer", player_id = "integer", player_name = "character", player_jersey = "integer",
  is_captain = "logical", is_first_goalkeeper = "logical", absence_code = "character",
  absence_reason = "character"
)

# lineUps keys -> unit label, in the order the page lists them.
.SIHF_UNITS <- list(
  list(c("goalkeepers"), "goalkeeper"),
  list(c("defenders", "left"), "defense_left"),
  list(c("defenders", "right"), "defense_right"),
  list(c("forwarders", "left"), "forward_left"),
  list(c("forwarders", "center"), "forward_center"),
  list(c("forwarders", "right"), "forward_right"),
  list(c("otherPlayers"), "other")
)

.sihf_parse_game_lineups <- function(raw) {
  game <- .sihf_game(raw)
  gid <- .sihf_int(game[["gameId"]])
  roster <- .sihf_roster(game)
  details <- .sihf_obj(game[["details"]])
  lineups <- .sihf_obj(game[["lineUps"]])
  rows <- list()
  for (side in c("home", "away")) {
    tid <- .sihf_licence(.sihf_obj(details[[paste0(side, "Team")]])[["id"]])
    team <- lineups[[paste0(side, "Team")]]
    if (.sihf_is_obj(team)) {
      tags <- Filter(.sihf_is_obj, .sihf_arr(team[["taggedPlayers"]]))
      tagged <- function(type) {
        v <- vapply(Filter(function(t) identical(t[["type"]], type), tags), function(t) .sihf_licence(t[["id"]]), numeric(1))
        v
      }
      captains <- tagged("captain")
      first_goalies <- tagged("goalkeeper")
      for (u in .SIHF_UNITS) {
        node <- team
        for (key in u[[1]]) node <- if (.sihf_is_obj(node)) node[[key]] else NULL
        ids <- if (.sihf_is_array(node)) node else list()
        for (slot in seq_along(ids)) {
          who <- .sihf_person(roster, ids[[slot]])
          if (is.na(who$player_id)) next
          rows[[length(rows) + 1L]] <- c(
            list(game_id = gid, team_side = side, team_id = tid, unit = u[[2]], slot = slot), who,
            list(is_captain = who$player_id %in% captains[!is.na(captains)],
                 is_first_goalkeeper = who$player_id %in% first_goalies[!is.na(first_goalies)])
          )
        }
      }
    }
    for (group in .sihf_arr(details[[paste0(side, "Absentees")]])) {
      if (!.sihf_is_obj(group)) next
      for (licence in .sihf_arr(group[["absentees"]])) {
        who <- .sihf_person(roster, licence)
        if (is.na(who$player_id)) next
        rows[[length(rows) + 1L]] <- c(
          list(game_id = gid, team_side = side, team_id = tid), who,
          list(absence_code = .sihf_na(group[["acronym"]]), absence_reason = .sihf_na(group[["name"]]))
        )
      }
    }
  }
  .sihf_frame(rows, .SIHF_LINEUP_SCHEMA)
}

# ---- request + refusal ------------------------------------------------------

# A season END year as the string SIHF takes; anything else is a bad request.
.sihf_season <- function(season) {
  n <- suppressWarnings(as.numeric(season))
  if (length(season) != 1L || is.na(n) || n != round(n) || n < 1000 || n > 9999) {
    .abort_bad_request("{.arg season} must be a four-digit END year (2026 = 2025-26), not {.val {season}}.")
  }
  sprintf("%.0f", n)
}

# A value of a query parameter as the string SIHF takes (NULL stays NULL).
.sihf_query_value <- function(x) if (is.null(x)) NULL else .sihf_str(x)

.sihf_check_choice <- function(value, choices, arg) {
  if (length(value) != 1L || is.na(value) || !value %in% choices) {
    .abort_bad_request("{.arg {arg}} must be one of {.val {choices}}; got {.val {value}}.")
  }
  invisible(value)
}

# 01.07.<end-1>-30.06.<end>: every date of a season keyed by its END year.
.sihf_season_date_range <- function(season) {
  end <- as.integer(as.numeric(season))
  sprintf("01.07.%d-30.06.%d", end - 1L, end)
}

# The query to send and the (name, value) filters folded into it.
.sihf_build_query <- function(params) {
  clean <- Filter(Negate(is.null), params)
  league <- clean[["League"]]
  clean[["League"]] <- NULL
  if (!is.null(league)) clean[["searchQuery"]] <- paste0(clean[["searchQuery"]] %||% "1//", league)
  if (identical(clean[["alias"]], "results") && !is.null(clean[["Season"]]) && is.null(clean[["Date"]])) {
    clean[["Date"]] <- .sihf_season_date_range(clean[["Season"]])
  }
  keys <- intersect(.SIHF_FILTER_KEYS, names(clean))
  filters <- lapply(keys, function(k) list(name = k, value = as.character(clean[[k]])))
  clean[keys] <- NULL
  if (length(keys)) {
    clean[["filterBy"]] <- paste(keys, collapse = ",")
    clean[["filterQuery"]] <- paste(vapply(filters, function(f) f$value, character(1)), collapse = "/")
  }
  list(query = clean, filters = filters)
}

# Raise unless the table body serves exactly what was asked (see the header comment).
.sihf_check_table <- function(url, raw, filters) {
  if (!length(filters)) return(invisible())
  where <- .euro_where(url)
  echoed <- if (.sihf_is_obj(raw)) raw[["filters"]] else NULL
  if (!.sihf_is_array(echoed)) {
    .abort_asset_fetch("{where}: the body carries no filter echo, so the answer cannot be verified")
  }
  by_alias <- list()
  for (f in echoed) {
    a <- if (.sihf_is_obj(f)) .sihf_str(f[["alias"]]) else NA_character_
    if (!is.na(a)) by_alias[[a]] <- f
  }
  for (f in filters) {
    name <- f$name
    value <- f$value
    entry <- by_alias[[name]]
    if (identical(name, "Phase") && identical(value, "")) {
      if (!.sihf_is_array(.sihf_obj(entry)[["entries"]])) {
        .abort_asset_fetch("{where}: the body carries no Phase entries to list")
      }
      next
    }
    # `selected: null` echoes nothing, exactly like a dropped filter.
    got <- if (is.null(entry)) NA_character_ else .sihf_str(entry[["selected"]])
    if (is.na(got) || !identical(got, value)) {
      answered <- if (!is.na(got)) paste0("answered ", name, "=", got) else paste0("dropped the ", name, " filter")
      if (identical(name, "ContentType") || is.na(got)) {
        # A filter missing from the echo was ignored, not answered: unverifiable. And every season has an
        # All / Home / Away table, so a swapped ContentType is SIHF breaking its own contract.
        .abort_asset_fetch("{where}: SIHF {answered} for {name}={value}, so the table cannot be verified")
      }
      .abort_no_data(paste0(
        "{where}: SIHF has no data for {name}={value} with the other filters given; it {answered} instead"
      ))
    }
    if (identical(name, "Phase")) {
      entries <- entry[["entries"]]
      if (!.sihf_is_array(entries)) {
        .abort_asset_fetch("{where}: the Phase echo lists no entries, so Phase={value} cannot be verified")
      }
      listed <- vapply(Filter(.sihf_is_obj, entries), function(e) .sihf_str(e[["alias"]]), character(1))
      if (!value %in% listed) {
        .abort_no_data(paste0(
          "{where}: SIHF lists no Phase={value} for this table; it echoed the id back but serves another ",
          "phase's rows"
        ))
      }
    }
  }
  rows <- if (.sihf_is_obj(raw)) raw[["data"]] else NULL
  if (.sihf_is_array(rows) && length(rows) &&
      all(vapply(rows, function(r) .sihf_is_array(r) && length(r) == 1L, logical(1)))) {
    sentinel <- .sihf_str(rows[[1]][[1]])
    .abort_no_data("{where}: SIHF answered {.val {sentinel}} (no table for that league and season)")
  }
  invisible()
}

# Raise unless a table body holds every row it announces. `pages` is the table's total ROW
# count (0 on standing, which has no paging): a positive `pages` that disagrees with the rows
# served means a truncated answer.
.sihf_check_complete <- function(url, raw) {
  if (!.sihf_is_obj(raw)) return(invisible())
  pages <- raw[["pages"]]
  rows <- raw[["data"]]
  if (is.numeric(pages) && length(pages) == 1L && !is.na(pages) && pages > 0 && .sihf_is_array(rows) &&
      length(rows) != pages) {
    where <- .euro_where(url)
    n_rows <- length(rows)
    .abort_asset_fetch(paste0(
      "{where}: the table announces {pages} rows but serves {n_rows}; a partial table is not returned ",
      "as a complete one"
    ))
  }
  invisible()
}

# Raise on a 200 that is not the requested answer: a stub, a redirect, or another game.
.sihf_check_body <- function(url, raw, query) {
  where <- .euro_where(url)
  if (.sihf_is_obj(raw) && identical(raw[["stub"]], TRUE)) {
    .abort_asset_fetch("{where}: SIHF answered a stub body (an unknown alias)")
  }
  if (.sihf_is_obj(raw) && identical(raw[["action"]], "Redirect")) {
    target <- .sihf_str(.sihf_obj(raw[["redirectTo"]])[["alias"]])
    .abort_asset_fetch("{where}: SIHF answered a redirect to alias {.val {target}}, not a table")
  }
  if (endsWith(sub("/+$", "", url), "/gameoverview")) {
    got <- if (.sihf_is_obj(raw)) .sihf_str(raw[["gameId"]]) else NA_character_
    asked <- .sihf_str(query[["searchQuery"]])
    if (is.na(got) || !identical(got, asked)) {
      .abort_asset_fetch("{where}: asked for game {asked}, SIHF answered gameId={.val {got}}")
    }
  }
  invisible()
}

# GET a SIHF route as JSON: build the scoped query, fetch, then verify a table answer.
# Error vocabulary from .euro_get(): 404 -> fastRhockey_no_data, 400/422 ->
# fastRhockey_bad_request, any other non-2xx / empty or non-JSON 200 / dead connection ->
# fastRhockey_asset_fetch; the checks above add the SIHF-specific refusals.
.sihf_get <- function(path, params) {
  url <- paste0(.SIHF_BASE, "/", path)
  built <- .sihf_build_query(params)
  raw <- .euro_get(url, built$query, simplifyVector = FALSE)
  .sihf_check_body(url, raw, built$query)
  .sihf_check_table(url, raw, built$filters)
  .sihf_check_complete(url, raw)
  raw
}

# ---- route cores (shared by the exported wrappers) ---------------------------

.sihf_table_route <- function(short, alias, season, league_id, phase_id, language, label, venue = NULL) {
  .sihf_check_choice(language, .SIHF_LANGUAGES, "language")
  params <- list(
    alias = alias, searchQuery = "1//", Season = .sihf_season(season),
    League = .sihf_query_value(league_id), Phase = .sihf_query_value(phase_id),
    ContentType = venue, language = language
  )
  raw <- .sihf_get("table", params)
  make_fastRhockey_data(.sihf_parse_table(raw, .SIHF_COLUMNS[[short]]),
                        paste0("SIHF ", label, " Information from data.sihf.ch"), Sys.time())
}

.sihf_game_route <- function(short, parser, game_id, language, label) {
  .sihf_check_choice(language, .SIHF_LANGUAGES, "language")
  if (length(game_id) != 1L || is.na(.sihf_str(game_id)) || !nzchar(.sihf_str(game_id))) {
    .abort_bad_request("{.arg game_id} must be one SIHF game id (e.g. 20261105000325), not {.val {game_id}}.")
  }
  raw <- .sihf_get("gameoverview", list(alias = "gameDetail", searchQuery = .sihf_str(game_id), language = language))
  make_fastRhockey_data(parser(raw), paste0("SIHF ", label, " Information from data.sihf.ch"), Sys.time())
}

.SIHF_COLUMNS <- list(
  results = c(
    "season", "phase_id", "day", "date", "time", "home_team_id", "home_team_name",
    "home_team_acronym", "away_team_id", "away_team_name", "away_team_acronym", "score_home",
    "score_away", "score_per_period_home", "score_per_period_away", "decision", "status_id",
    "status_name", "status_percent", "status_canceled", "start_date_time", "end_date_time",
    "update_date_time", "game_id", "broadcasts", "row_type"
  ),
  standings = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played", "points",
    "points_quotient", "games_won_in_time", "games_won_extra_time", "games_won_penalty",
    "games_lost_penalty", "games_lost_extra_time", "games_lost_in_time", "goals_for",
    "goals_against", "goals_difference", "streak", "row_type"
  ),
  player_stats = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "goals", "assists", "points", "points_per_game", "pim_total", "plus_minus",
    "row_type"
  ),
  goalie_stats = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "games_played",
    "first_keeper", "goals_against", "goals_against_average", "shots_against", "saves",
    "saves_percentage", "seconds_played", "penalty_in_minutes", "goals", "assists", "row_type"
  ),
  phases = c(
    "season", "phase_id", "phase_name", "n_games"
  ),
  pbp = c(
    "game_id", "event_index", "period_number", "period", "event_type", "time", "game_seconds",
    "team_id", "team_side", "player_id", "player_name", "player_jersey", "assist1_player_id",
    "assist1_player_name", "assist2_player_id", "assist2_player_name", "goalie_id", "goalie_name",
    "strength", "goal_tags", "score_home", "score_away", "plus_player_ids", "minus_player_ids",
    "penalty_id", "penalty_name", "penalty_minutes", "penalty_start", "penalty_end",
    "goalie_change", "revoke_reason", "shootout_number", "shootout_scored", "text"
  ),
  game_info = c(
    "game_id", "season", "round", "league_id", "league_name", "region_id", "region_name",
    "group_id", "group_name", "tournament_id", "tournament_name", "qualification_id",
    "qualification_name", "phase_id", "phase_name", "start_date_time", "end_date_time",
    "update_date_time", "status_id", "status_name", "status_percent", "status_canceled",
    "home_team_id", "home_team_name", "home_team_acronym", "away_team_id", "away_team_name",
    "away_team_acronym", "home_score", "away_score", "home_score_by_period",
    "away_score_by_period", "home_shots_by_period", "away_shots_by_period", "period_indicators",
    "venue_name", "venue_city", "spectators", "referees", "linesmen", "broadcasts"
  ),
  game_player_stats = c(
    "game_id", "team_side", "team_id", "team_name", "player_number", "player", "player_position",
    "goals", "assists", "assists1", "assists2", "points", "pim_total", "plus_minus",
    "shots_on_goal", "sog_missed", "sog_post", "blockings", "faceoffs_won", "faceoffs_lost",
    "faceoffs_percent", "time_on_ice", "time_on_ice_pp", "time_on_ice_pk", "row_type", "player_id"
  ),
  game_goalie_stats = c(
    "game_id", "team_side", "team_id", "team_name", "player_number", "player", "goals_against",
    "saves", "shots_against", "saves_percentage", "seconds_played", "goals_against_average",
    "penalty_in_minutes", "row_type", "player_id"
  ),
  game_team_stats = c(
    "game_id", "statistic", "home_value", "away_value"
  ),
  game_lineups = c(
    "game_id", "team_side", "team_id", "unit", "slot", "player_id", "player_name", "player_jersey",
    "is_captain", "is_first_goalkeeper", "absence_code", "absence_reason"
  ),
  team_goals = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "goals_for", "goals_for_quotient", "goals_against", "goals_against_quotient", "even_goals_for",
    "even_goals_for_quotient", "even_goals_against", "even_goals_against_quotient", "pp_goals_for",
    "pp_goals_for_quotient", "pp_goals_against", "pp_goals_against_quotient", "sh_goals",
    "sh_goals_quotient", "pk_goals_against", "pk_goals_against_quotient", "row_type"
  ),
  team_goals_by_position = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "goals_for_fw_df", "goals_for_fw", "goals_for_fw_percent", "goals_for_df",
    "goals_for_df_percent", "goals_for_im", "goals_for_im_percent", "sow_goals_for",
    "sow_goals_against", "row_type"
  ),
  team_shots = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "sog_sog", "sog_sog_percent", "sog_sog_quotient", "sog_sog_slot", "sog_sog_slot_per_game",
    "sog_missed", "sog_missed_per_game", "sog_post", "sog_post_per_game", "blockings",
    "blockings_per_game", "soga_sog", "soga_sog_per_game", "soga_sog_slot",
    "soga_sog_slot_per_game", "row_type"
  ),
  team_power_play = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "pp_opportunities", "pp_opportunities_quotient", "pp_goals_for", "pp_goals_for_quotient",
    "pp_percent", "power_play_time", "power_play_time_quotient", "pp_goals_against",
    "pp_goals_against_quotient", "row_type"
  ),
  team_penalty_kill = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "pk_situations", "pk_situations_quotient", "pk_goals_against", "pk_goals_against_quotient",
    "pk_percent", "penalty_kill_time", "penalty_kill_time_quotient", "sh_goals",
    "sh_goals_quotient", "row_type"
  ),
  team_penalties = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played", "pim2",
    "pim5", "pim10", "game_misconducts", "match_penalties", "pim_total", "row_type"
  ),
  team_shootouts = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "games_won_penalty", "games_lost_penalty", "pen_shots", "pen_shot_goals", "pen_shots_missed",
    "pen_shots_percent", "pen_shots_against", "pen_shot_goals_against", "pen_shot_saves",
    "pen_shot_saves_percent", "row_type"
  ),
  team_faceoffs = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "faceoffs", "faceoffs_per_game", "faceoffs_percent", "faceoffs_won", "faceoffs_lost",
    "faceoffs_won_per_game", "faceoffs_lost_per_game", "faceoffs_offensive_percent",
    "faceoffs_neutral_percent", "faceoffs_defensive_percent", "row_type"
  ),
  team_faceoffs_by_zone = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "faceoffs_offensive", "faceoffs_offensive_percent", "faceoffs_won_offensive",
    "faceoffs_lost_offensive", "faceoffs_neutral", "faceoffs_neutral_percent",
    "faceoffs_won_neutral", "faceoffs_lost_neutral", "faceoffs_defensive",
    "faceoffs_defensive_percent", "faceoffs_won_defensive", "faceoffs_lost_defensive", "row_type"
  ),
  team_faceoffs_by_zone_per_game = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "games_played",
    "faceoffs_offensive_per_game", "faceoffs_won_offensive_per_game",
    "faceoffs_lost_offensive_per_game", "faceoffs_neutral_per_game",
    "faceoffs_won_neutral_per_game", "faceoffs_lost_neutral_per_game",
    "faceoffs_defensive_per_game", "faceoffs_won_defensive_per_game",
    "faceoffs_lost_defensive_per_game", "row_type"
  ),
  team_attendance = c(
    "season", "phase_id", "rank", "team_id", "team_name", "team_acronym", "home_games",
    "spectators", "stadium_capacity_quotient", "spectators_quotient", "stadium_capacity_percent",
    "row_type"
  ),
  player_goals_assists = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "goals", "assists", "assists1", "assists2", "game_winning_goals", "eq_goals",
    "eq_assists", "pp_goals", "pp_assists", "pk_goals", "pk_assists", "overtime_goals",
    "hattricks", "row_type"
  ),
  player_shots = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "sog_sog", "sog_sog_percent", "sog_sog_quotient", "sog_sog_slot",
    "sog_sog_slot_per_game", "sog_missed", "sog_missed_per_game", "sog_post", "sog_post_per_game",
    "blockings", "blockings_per_game", "row_type"
  ),
  player_penalties = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "pim_total", "pim2", "pim5", "pim10", "game_misconducts", "match_penalties",
    "row_type"
  ),
  player_shootouts = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "pen_shots", "pen_shot_goals", "pen_shots_missed", "pen_shots_percent",
    "pen_shot_winning_goals", "row_type"
  ),
  player_faceoffs = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "faceoffs", "faceoffs_per_game", "faceoffs_percent", "faceoffs_won",
    "faceoffs_lost", "faceoffs_won_per_game", "faceoffs_lost_per_game",
    "faceoffs_offensive_percent", "faceoffs_neutral_percent", "faceoffs_defensive_percent",
    "row_type"
  ),
  player_faceoffs_by_zone = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "faceoffs_offensive", "faceoffs_offensive_percent", "faceoffs_won_offensive",
    "faceoffs_lost_offensive", "faceoffs_neutral", "faceoffs_neutral_percent",
    "faceoffs_won_neutral", "faceoffs_lost_neutral", "faceoffs_defensive",
    "faceoffs_defensive_percent", "faceoffs_won_defensive", "faceoffs_lost_defensive", "row_type"
  ),
  player_faceoffs_by_zone_per_game = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "faceoffs_offensive_per_game", "faceoffs_won_offensive_per_game",
    "faceoffs_lost_offensive_per_game", "faceoffs_neutral_per_game",
    "faceoffs_won_neutral_per_game", "faceoffs_lost_neutral_per_game",
    "faceoffs_defensive_per_game", "faceoffs_won_defensive_per_game",
    "faceoffs_lost_defensive_per_game", "row_type"
  ),
  player_time_on_ice = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "position",
    "games_played", "time_on_ice_eq", "time_on_ice_eq_per_game", "time_on_ice_pp",
    "time_on_ice_pp_per_game", "time_on_ice_pk", "time_on_ice_pk_per_game", "time_on_ice",
    "time_on_ice_per_game", "shifts", "time_on_ice_per_shift", "shifts_per_game", "row_type"
  ),
  goalie_shots = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "games_played",
    "soga_sog", "soga_sog_per_game", "soga_sog_slot", "soga_sog_slot_per_game", "soga_missed",
    "soga_missed_per_game", "soga_post", "soga_post_per_game", "saves_slot_percentage", "row_type"
  ),
  goalie_wins = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "games_played",
    "first_keeper", "wins", "wins_intime", "wins_overtime", "wins_penalty", "losses",
    "losses_intime", "losses_overtime", "losses_penalty", "shutout", "row_type"
  ),
  goalie_shootouts = c(
    "season", "phase_id", "rank", "player", "team_id", "team_name", "team_acronym", "games_played",
    "pen_shots_against", "pen_shot_goals_against", "pen_shot_saves", "pen_shot_saves_percent",
    "row_type"
  )
)

#' **SIHF Games of a league season**
#'
#' @name sihf_results
#' @description
#' Games of a league season: date, teams, score, per-period score, decision, status and game_id.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase (player / goalie tables), every game (results), the regular-season table (standings).
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 26 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | day | character | Weekday abbreviation, German (e.g., "Di"). |
#'   | date | Date | Game date (local). |
#'   | time | character | Scheduled local start time, "hh:mm". |
#'   | home_team_id | integer | SIHF team id of the home team. |
#'   | home_team_name | character | Home team display name. |
#'   | home_team_acronym | character | Home team abbreviation. |
#'   | away_team_id | integer | SIHF team id of the away team. |
#'   | away_team_name | character | Away team display name. |
#'   | away_team_acronym | character | Away team abbreviation. |
#'   | score_home | integer | Final home score. |
#'   | score_away | integer | Final away score. |
#'   | score_per_period_home | list | Home goals per period (overtime included as its own period). |
#'   | score_per_period_away | list | Away goals per period (overtime included as its own period). |
#'   | decision | character | How the game was decided beyond regulation: "OT" or "SO"; null in regulation. |
#'   | status_id | integer | SIHF game-status id (12 = finished). |
#'   | status_name | character | Game-status name; German whatever the language (e.g., "Ende"). |
#'   | status_percent | integer | Game progress, 0-100 (100 once the game is over). |
#'   | status_canceled | logical | Whether the game was cancelled. |
#'   | start_date_time | character | Scheduled start, ISO-8601 with UTC offset, as a string. |
#'   | end_date_time | character | End of the game, ISO-8601 with UTC offset, as a string. |
#'   | update_date_time | character | When SIHF last updated the record, ISO-8601 with UTC offset, as a string. |
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | broadcasts | character | Broadcasters, comma-joined (e.g., "MYSPORTS,TV24"). |
#'   | row_type | character | "data" for a regular row; "average" or "total" for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_results(season = 2026, league_id = 1))
#' }
sihf_results <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("results", "results", season, league_id, phase_id, language, "Results")
}

#' **SIHF League table of a season (every game, or home / away games only)**
#'
#' @name sihf_standings
#' @description
#' League table of a season (every game, or home / away games only): rank, games, points, regulation / overtime / shootout wins and losses, goals, streak.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the regular-season table.
#' @param venue Which games the table counts: All (the full table), Home or Away. Always sent: without it SIHF answers the away-games table (26 of the 52 National League games).
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 20 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row. |
#'   | team_id | integer | SIHF team id (integer join key). |
#'   | team_name | character | Team display name as SIHF publishes it. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"). |
#'   | games_played | integer | Games the team played in the selection (every game, or home / away games only, per venue). |
#'   | points | integer | Table points earned in the standings. |
#'   | points_quotient | numeric | Points per game. |
#'   | games_won_in_time | integer | Wins in regulation. |
#'   | games_won_extra_time | integer | Wins in overtime. |
#'   | games_won_penalty | integer | Wins in a shootout. |
#'   | games_lost_penalty | integer | Losses in a shootout. |
#'   | games_lost_extra_time | integer | Losses in overtime. |
#'   | games_lost_in_time | integer | Losses in regulation. |
#'   | goals_for | integer | Goals scored by the team. |
#'   | goals_against | integer | Goals allowed by the team. |
#'   | goals_difference | integer | Goal difference. |
#'   | streak | character | Recent results, newest first, as SIHF prints them (e.g., "W+L-"; "+" regulation, "-" after regulation). |
#'   | row_type | character | "data" for a regular row; "average" or "total" for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_standings(season = 2026, league_id = 1))
#' }
sihf_standings <- function(season, league_id = 1L, phase_id = NULL, venue = "All", language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  .sihf_check_choice(venue, c("All", "Home", "Away"), "venue")
  on.exit(options(old), add = TRUE)
  .sihf_table_route("standings", "standing", season, league_id, phase_id, language, "Standings", venue = venue)
}

#' **SIHF Skater season totals**
#'
#' @name sihf_player_stats
#' @description
#' Skater season totals: games, goals, assists, points, penalty minutes, plus-minus.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase (player / goalie tables), every game (results), the regular-season table (standings).
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 16 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
#'   | team_id | integer | SIHF team id (integer join key). |
#'   | team_name | character | Team display name as SIHF publishes it. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"). |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"). |
#'   | games_played | integer | Games the player played in the selection. |
#'   | goals | integer | Goals scored by the player. |
#'   | assists | integer | Assists (primary + secondary). |
#'   | points | integer | Points (goals + assists). |
#'   | points_per_game | numeric | Points per game. |
#'   | pim_total | integer | Penalty minutes. |
#'   | plus_minus | integer | Plus-minus: even-strength and shorthanded goals for, minus against, while on ice. |
#'   | row_type | character | "data" for a regular row; "average" or "total" for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_stats(season = 2026, league_id = 1))
#' }
sihf_player_stats <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_stats", "player", season, league_id, phase_id, language, "Player Stats")
}

#' **SIHF Goaltender season totals**
#'
#' @name sihf_goalie_stats
#' @description
#' Goaltender season totals: games, starts, goals against, GAA, shots, saves, save percentage, ice time.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase (player / goalie tables), every game (results), the regular-season table (standings).
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 19 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
#'   | team_id | integer | SIHF team id (integer join key). |
#'   | team_name | character | Team display name as SIHF publishes it. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"). |
#'   | games_played | integer | Games the goaltender dressed for. |
#'   | first_keeper | integer | Games listed as the starting goaltender. |
#'   | goals_against | integer | Goals allowed by the goaltender. |
#'   | goals_against_average | numeric | Goals against per 60 minutes. |
#'   | shots_against | integer | Shots on goal faced. |
#'   | saves | integer | Shots on goal saved by the goaltender. |
#'   | saves_percentage | numeric | Save percentage on a 0-100 scale (91.25 = .9125). |
#'   | seconds_played | character | Ice time as "mm:ss" (a string, despite the upstream name). Minutes are not capped at 59: an overtime game reads "65:00", a season total "3615:02". |
#'   | penalty_in_minutes | integer | Penalty minutes. |
#'   | goals | integer | Goals scored by the goaltender (almost always 0). |
#'   | assists | integer | Assists by the goaltender. |
#'   | row_type | character | "data" for a regular row; "average" or "total" for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_goalie_stats(season = 2026, league_id = 1))
#' }
sihf_goalie_stats <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("goalie_stats", "goalkeeper", season, league_id, phase_id, language, "Goalie Stats")
}

#' **SIHF Phases of a league season (regular season, play-in and playoff rounds, playout)**
#'
#' @name sihf_phases
#' @description
#' Phases of a league season (regular season, play-in and playoff rounds, playout): the phase_id values the table wrappers take, with each phase's name and game count.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param table SIHF table whose phase list to read; each table lists its own. results (sihf_results) lists every round with its game count; player / goalkeeper (sihf_player_stats / sihf_goalie_stats) add the Playoff Overall and Play-In Overall totals; standing (sihf_standings) lists only the phases played as a table (the regular season, in some seasons a playout round).
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 4 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (integer), the value the phase_id argument of sihf_results, sihf_standings, sihf_player_stats and sihf_goalie_stats takes. Ids differ by league, season and table. |
#'   | phase_name | character | Phase name as SIHF publishes it, without the trailing count (e.g., "Regular Season", "Playoff 1/4 Final", "Playoff Overall"). |
#'   | n_games | integer | Games played in the phase, from the results table's phase list; null for the other tables and for a phase still in progress. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_phases(season = 2026, league_id = 1))
#' }
sihf_phases <- function(season, league_id = 1L, table = "results", language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_check_choice(language, .SIHF_LANGUAGES, "language")
  .sihf_check_choice(table, c("results", "standing", "player", "goalkeeper"), "table")
  raw <- .sihf_get("table", list(
    searchQuery = "1//", Phase = "", Season = .sihf_season(season),
    League = .sihf_query_value(league_id), alias = table, language = language
  ))
  make_fastRhockey_data(.sihf_parse_phases(raw), "SIHF Phases Information from data.sihf.ch", Sys.time())
}

#' **SIHF Event timeline of one game**
#'
#' @name sihf_pbp
#' @description
#' Event timeline of one game: goals, penalties, goaltender changes, revoked goals and shootout attempts.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param game_id Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 34 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | event_index | integer | 1-based event order within the game. |
#'   | period_number | integer | 1-based period (4 = first overtime); the shootout follows the last period. |
#'   | period | character | Period name as SIHF prints it, German (e.g., "1. Drittel", "Shootout"). |
#'   | event_type | character | Event kind: "goal", "penalty", "goalie", "revoked_goal" or "shootout". |
#'   | time | character | Elapsed game time of the event, "mm:ss" (null on shootout rows). |
#'   | game_seconds | integer | Elapsed game time of the event in seconds. |
#'   | team_id | integer | SIHF team id of the team the event belongs to. |
#'   | team_side | character | Side of the game the row belongs to: "home" or "away". |
#'   | player_id | integer | Licence number of the scorer, penalized player, goaltender (goalie rows) or shooter. |
#'   | player_name | character | Name of the scorer, penalized player, goaltender (goalie rows) or shooter. |
#'   | player_jersey | integer | Jersey number of the player in player_id. |
#'   | assist1_player_id | integer | Licence number of the first assist. |
#'   | assist1_player_name | character | Name of the first assist. |
#'   | assist2_player_id | integer | Licence number of the second assist. |
#'   | assist2_player_name | character | Name of the second assist. |
#'   | goalie_id | integer | Licence number of the goaltender faced (shootout rows). |
#'   | goalie_name | character | Name of the goaltender faced (shootout rows). |
#'   | strength | character | First goal tag: "EQ" even strength, "PP1" power play, "SH1" shorthanded, ... |
#'   | goal_tags | character | Every goal tag, comma-joined as SIHF prints them (e.g., "PP1,GWG"). |
#'   | score_home | integer | Home score after the goal. |
#'   | score_away | integer | Away score after the goal. |
#'   | plus_player_ids | list | Licence numbers credited +1 on the goal (listed only where plus-minus applies). |
#'   | minus_player_ids | list | Licence numbers charged -1 on the goal (listed only where plus-minus applies). |
#'   | penalty_id | integer | SIHF penalty-type id. |
#'   | penalty_name | character | Penalty name, German whatever the language (e.g., "Haken"). |
#'   | penalty_minutes | integer | Penalty length in minutes. |
#'   | penalty_start | character | Game time the penalty starts, "mm:ss". |
#'   | penalty_end | character | Game time the penalty ended, "mm:ss" (earlier than its full length when a goal ended it). |
#'   | goalie_change | character | Goaltender change direction: "in" or "out". |
#'   | revoke_reason | character | Why the goal was disallowed, as SIHF prints it (e.g., "CC-Offiside"). |
#'   | shootout_number | integer | 1-based shootout attempt number. |
#'   | shootout_scored | logical | Whether the shootout attempt scored. |
#'   | text | character | Event text exactly as SIHF prints it (markdown bold included). |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_pbp(game_id = 20261105000325))
#' }
sihf_pbp <- function(game_id, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_game_route("pbp", .sihf_parse_pbp, game_id, language, "PBP")
}

#' **SIHF Header of one game**
#'
#' @name sihf_game_info
#' @description
#' Header of one game: league, phase, teams, final and per-period score and shots, venue, attendance, officials.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param game_id Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 41 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | season | integer | Season by its END year (2026 = 2025-26). |
#'   | round | integer | Round number within the season. |
#'   | league_id | integer | SIHF league id of the game (1 = National League). |
#'   | league_name | character | League name (e.g., "National League"). |
#'   | region_id | integer | SIHF region id of the game (1 = national). |
#'   | region_name | character | Region name (e.g., "National"). |
#'   | group_id | integer | SIHF group id (0 = no group). |
#'   | group_name | character | Name of the league group the game belongs to ('Keine' = none). |
#'   | tournament_id | integer | SIHF tournament id (1 = championship). |
#'   | tournament_name | character | Tournament name, German (e.g., 'Meisterschaft' = championship). |
#'   | qualification_id | integer | SIHF id of the stage of the season (1 = regular season). |
#'   | qualification_name | character | Stage of the season (e.g., "Regular Season", "Playoff Final"). |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...). |
#'   | phase_name | character | Phase name (e.g., "NL Playoff Final"). |
#'   | start_date_time | character | Scheduled start, ISO-8601 with UTC offset, as a string. |
#'   | end_date_time | character | End of the game, ISO-8601 with UTC offset, as a string. |
#'   | update_date_time | character | When SIHF last updated the record, ISO-8601 with UTC offset, as a string. |
#'   | status_id | integer | SIHF game-status id (12 = finished). |
#'   | status_name | character | Game-status name; German whatever the language (e.g., "Ende"). |
#'   | status_percent | integer | Game progress, 0-100 (100 once the game is over). |
#'   | status_canceled | logical | Whether the game was cancelled. |
#'   | home_team_id | integer | SIHF team id of the home team. |
#'   | home_team_name | character | Home team display name. |
#'   | home_team_acronym | character | Home team abbreviation. |
#'   | away_team_id | integer | SIHF team id of the away team. |
#'   | away_team_name | character | Away team display name. |
#'   | away_team_acronym | character | Away team abbreviation. |
#'   | home_score | integer | Final home score. |
#'   | away_score | integer | Final away score. |
#'   | home_score_by_period | list | Home goals per period, aligned with period_indicators. |
#'   | away_score_by_period | list | Away goals per period, aligned with period_indicators. |
#'   | home_shots_by_period | list | Home shots on goal per period, aligned with period_indicators. |
#'   | away_shots_by_period | list | Away shots on goal per period, aligned with period_indicators. |
#'   | period_indicators | list | Period labels: "1", "2", "3", then "OT1", ... |
#'   | venue_name | character | Name of the arena the game was played in. |
#'   | venue_city | character | City of the arena with its canton code (e.g., 'Davos-Platz GR'). |
#'   | spectators | integer | Attendance as SIHF reports it. |
#'   | referees | character | Referee names, comma-joined. |
#'   | linesmen | character | Linesman names, comma-joined. |
#'   | broadcasts | character | Broadcasters, comma-joined (e.g., "MYSPORTS,TV24"). |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_game_info(game_id = 20261105000325))
#' }
sihf_game_info <- function(game_id, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_game_route("game_info", .sihf_parse_game_info, game_id, language, "Game Info")
}

#' **SIHF Skater box score of one game, both sides, with ice time and faceoffs**
#'
#' @name sihf_game_player_stats
#' @description
#' Skater box score of one game, both sides, with ice time and faceoffs.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param game_id Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 26 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | team_side | character | Side of the game the row belongs to: "home" or "away". |
#'   | team_id | integer | SIHF team id of the row's side. |
#'   | team_name | character | Team name of the row's side. |
#'   | player_number | integer | Jersey number worn in this game. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
#'   | player_position | character | Position, German whatever the language (e.g., "Verteidiger"). |
#'   | goals | integer | Goals scored by the player. |
#'   | assists | integer | Assists (primary + secondary). |
#'   | assists1 | integer | Primary assists. |
#'   | assists2 | integer | Secondary assists. |
#'   | points | integer | Points (goals + assists). |
#'   | pim_total | integer | Penalty minutes. |
#'   | plus_minus | integer | Plus-minus: even-strength and shorthanded goals for, minus against, while on ice. |
#'   | shots_on_goal | integer | Shots on goal taken by the player. |
#'   | sog_missed | integer | Shots that missed the net. |
#'   | sog_post | integer | Shots off the post or crossbar. |
#'   | blockings | integer | Opponent shots blocked by the player. |
#'   | faceoffs_won | integer | Faceoffs won by the player. |
#'   | faceoffs_lost | integer | Faceoffs lost by the player. |
#'   | faceoffs_percent | numeric | Faceoff win percentage, 0-100; null with no faceoffs. |
#'   | time_on_ice | character | Ice time, "mm:ss". |
#'   | time_on_ice_pp | character | Power-play ice time, "mm:ss". |
#'   | time_on_ice_pk | character | Penalty-kill ice time, "mm:ss". |
#'   | row_type | character | "data" per player, "total" for the side's total row. |
#'   | player_id | integer | SIHF licence number of the player (integer join key), resolved from the game's roster. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_game_player_stats(game_id = 20261105000325))
#' }
sihf_game_player_stats <- function(game_id, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_game_route("game_player_stats", .sihf_parse_game_player_stats, game_id, language, "Game Player Stats")
}

#' **SIHF Goaltender box score of one game, both sides**
#'
#' @name sihf_game_goalie_stats
#' @description
#' Goaltender box score of one game, both sides.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param game_id Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 15 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | team_side | character | Side of the game the row belongs to: "home" or "away". |
#'   | team_id | integer | SIHF team id of the row's side. |
#'   | team_name | character | Team name of the row's side. |
#'   | player_number | integer | Jersey number worn in this game. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"). |
#'   | goals_against | integer | Goals allowed by the goaltender. |
#'   | saves | integer | Shots on goal saved by the goaltender. |
#'   | shots_against | integer | Shots on goal faced. |
#'   | saves_percentage | numeric | Save percentage on a 0-100 scale (91.25 = .9125). |
#'   | seconds_played | character | Ice time as "mm:ss" (a string, despite the upstream name). Minutes are not capped at 59: an overtime game reads "65:00", a season total "3615:02". |
#'   | goals_against_average | numeric | Goals against per 60 minutes in this game. |
#'   | penalty_in_minutes | integer | Penalty minutes. |
#'   | row_type | character | "data" for every row: a game goalie box has no summary rows. |
#'   | player_id | integer | SIHF licence number of the player (integer join key), resolved from the game's roster. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_game_goalie_stats(game_id = 20261105000325))
#' }
sihf_game_goalie_stats <- function(game_id, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_game_route("game_goalie_stats", .sihf_parse_game_goalie_stats, game_id, language, "Game Goalie Stats")
}

#' **SIHF Team stats of one game**
#'
#' @name sihf_game_team_stats
#' @description
#' Team stats of one game: shots by period, faceoffs by zone, penalty minutes, power play and penalty kill.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param game_id Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 4 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | statistic | character | Statistic label as SIHF prints it (e.g., "SOG Total", "FO% 1P", "PPT"). |
#'   | home_value | character | Home team's value, as a string (counts, percentages and mm:ss times share the column). |
#'   | away_value | character | Away team's value, as a string. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_game_team_stats(game_id = 20261105000325))
#' }
sihf_game_team_stats <- function(game_id, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_game_route("game_team_stats", .sihf_parse_game_team_stats, game_id, language, "Game Team Stats")
}

#' **SIHF Lineups of one game by unit and slot, plus the absentees**
#'
#' @name sihf_game_lineups
#' @description
#' Lineups of one game by unit and slot, plus the absentees.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param game_id Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 12 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
#'   | team_side | character | Side of the game the row belongs to: "home" or "away". |
#'   | team_id | integer | SIHF team id (integer join key). |
#'   | unit | character | Lineup unit: "goalkeeper", "defense_left" / "_right", "forward_left" / "_center" / "_right", "other"; null for an absentee. |
#'   | slot | integer | 1-based position in SIHF's listing of the unit. |
#'   | player_id | integer | SIHF licence number of the player (integer join key), resolved from the game's roster. |
#'   | player_name | character | Player name from the game's roster, last name first. |
#'   | player_jersey | integer | Jersey number from the game's roster. |
#'   | is_captain | logical | Whether the lineup tags the player as captain. |
#'   | is_first_goalkeeper | logical | Whether the lineup tags the player as the starting goaltender. |
#'   | absence_code | character | Absence code for an absentee ("SU", "IN", "AB"). |
#'   | absence_reason | character | Absence reason, German (e.g., "Überzählig", "Verletzt", "Abwesend"). |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_game_lineups(game_id = 20261105000325))
#' }
sihf_game_lineups <- function(game_id, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_game_route("game_lineups", .sihf_parse_game_lineups, game_id, language, "Game Lineups")
}

#' **SIHF Team goals for and against per season**
#'
#' @name sihf_team_goals
#' @description
#' Team goals for and against per season: totals and per game, even strength, power play and short-handed.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 24 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | goals_for | integer | Goals scored (goals for). |
#'   | goals_for_quotient | numeric | Goals for per game. |
#'   | goals_against | integer | Goals conceded (goals against). |
#'   | goals_against_quotient | numeric | Goals against per game. |
#'   | even_goals_for | integer | Even-strength goals for. |
#'   | even_goals_for_quotient | numeric | Even-strength goals for per game. |
#'   | even_goals_against | integer | Even-strength goals against. |
#'   | even_goals_against_quotient | numeric | Even-strength goals against per game. |
#'   | pp_goals_for | integer | Power-play goals for. |
#'   | pp_goals_for_quotient | numeric | Power-play goals for per game. |
#'   | pp_goals_against | integer | Goals against while on the power play. |
#'   | pp_goals_against_quotient | numeric | Goals against while on the power play, per game. |
#'   | sh_goals | integer | Short-handed goals for. |
#'   | sh_goals_quotient | numeric | Short-handed goals for per game. |
#'   | pk_goals_against | integer | Goals against while short-handed. |
#'   | pk_goals_against_quotient | numeric | Goals against while short-handed, per game. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_goals(season = 2026, league_id = 1))
#' }
sihf_team_goals <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_goals", "teamGoal", season, league_id, phase_id, language, "Team Goals")
}

#' **SIHF Team goals by scorer**
#'
#' @name sihf_team_goals_by_position
#' @description
#' Team goals by scorer: forwards, defensemen and import players (totals and shares), plus shootout-winning goals for and against.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 17 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | goals_for_fw_df | integer | Goals by forwards and defencemen. |
#'   | goals_for_fw | integer | Goals by forwards. |
#'   | goals_for_fw_percent | numeric | Share of goals by forwards, percent. |
#'   | goals_for_df | integer | Goals by defencemen. |
#'   | goals_for_df_percent | numeric | Share of goals by defencemen, percent. |
#'   | goals_for_im | integer | Goals by import (foreign-licence) players. |
#'   | goals_for_im_percent | numeric | Share of goals by import players, percent. |
#'   | sow_goals_for | integer | Deciding shootout goals scored. |
#'   | sow_goals_against | integer | Deciding shootout goals conceded. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_goals_by_position(season = 2026, league_id = 1))
#' }
sihf_team_goals_by_position <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_goals_by_position", "teamGoalPosNation", season, league_id, phase_id, language, "Team Goals By Position")
}

#' **SIHF Team shots per season**
#'
#' @name sihf_team_shots
#' @description
#' Team shots per season: on goal (total, slot, shooting percentage), missed, post, blocked, and shots against, with per-game rates.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 23 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | sog_sog | integer | Shots on goal (saved or scored). |
#'   | sog_sog_percent | numeric | Shooting percentage (goals / shots on goal). |
#'   | sog_sog_quotient | numeric | Shots on goal per game. |
#'   | sog_sog_slot | integer | Slot shots on goal. |
#'   | sog_sog_slot_per_game | numeric | Slot shots on goal per game. |
#'   | sog_missed | integer | Missed shots, total. |
#'   | sog_missed_per_game | numeric | Missed shots per game. |
#'   | sog_post | integer | Post/crossbar shots, total. |
#'   | sog_post_per_game | numeric | Post/crossbar shots per game. |
#'   | blockings | integer | Number of shots blocked. |
#'   | blockings_per_game | numeric | Blocked shots per game. |
#'   | soga_sog | integer | Shots on goal against, total. |
#'   | soga_sog_per_game | numeric | Shots on goal against per game. |
#'   | soga_sog_slot | integer | Slot shots on goal against. |
#'   | soga_sog_slot_per_game | numeric | Slot shots on goal against per game. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_shots(season = 2026, league_id = 1))
#' }
sihf_team_shots <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_shots", "teamShotDetail", season, league_id, phase_id, language, "Team Shots")
}

#' **SIHF Team power play per season**
#'
#' @name sihf_team_power_play
#' @description
#' Team power play per season: opportunities, goals, conversion percentage, power-play time, and short-handed goals against.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 17 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | pp_opportunities | integer | Power-play opportunities. |
#'   | pp_opportunities_quotient | numeric | Power-play opportunities per game. |
#'   | pp_goals_for | integer | Power-play goals for. |
#'   | pp_goals_for_quotient | numeric | Power-play goals for per game. |
#'   | pp_percent | numeric | Power-play percentage. |
#'   | power_play_time | character | Time on the power play, mm:ss string (minutes may exceed 59), not seconds. |
#'   | power_play_time_quotient | character | Time on the power play per game, mm:ss string (minutes may exceed 59), not seconds. |
#'   | pp_goals_against | integer | Goals against while on the power play. |
#'   | pp_goals_against_quotient | numeric | Goals against while on the power play, per game. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_power_play(season = 2026, league_id = 1))
#' }
sihf_team_power_play <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_power_play", "teamPp", season, league_id, phase_id, language, "Team Power Play")
}

#' **SIHF Team penalty kill per season**
#'
#' @name sihf_team_penalty_kill
#' @description
#' Team penalty kill per season: times short-handed, goals against, kill percentage, penalty-kill time, and short-handed goals for.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 17 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | pk_situations | integer | Short-handed situations. |
#'   | pk_situations_quotient | numeric | Short-handed situations per game. |
#'   | pk_goals_against | integer | Goals against while short-handed. |
#'   | pk_goals_against_quotient | numeric | Goals against while short-handed, per game. |
#'   | pk_percent | numeric | Penalty-kill percentage. |
#'   | penalty_kill_time | character | Time short-handed, mm:ss string (minutes may exceed 59), not seconds. |
#'   | penalty_kill_time_quotient | character | Time short-handed per game, mm:ss string (minutes may exceed 59), not seconds. |
#'   | sh_goals | integer | Short-handed goals for. |
#'   | sh_goals_quotient | numeric | Short-handed goals for per game. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_penalty_kill(season = 2026, league_id = 1))
#' }
sihf_team_penalty_kill <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_penalty_kill", "teamPk", season, league_id, phase_id, language, "Team Penalty Kill")
}

#' **SIHF Team penalties per season**
#'
#' @name sihf_team_penalties
#' @description
#' Team penalties per season: 2-, 5- and 10-minute penalties, game misconducts, match penalties and penalty minutes.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 14 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | pim2 | integer | Number of 2-minute minor penalties. |
#'   | pim5 | integer | Number of 5-minute major penalties. |
#'   | pim10 | integer | Number of 10-minute misconducts. |
#'   | game_misconducts | integer | Number of game misconducts. |
#'   | match_penalties | integer | Number of match penalties. |
#'   | pim_total | integer | Penalty minutes, total. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_penalties(season = 2026, league_id = 1))
#' }
sihf_team_penalties <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_penalties", "teamFoul", season, league_id, phase_id, language, "Team Penalties")
}

#' **SIHF Team shootouts per season**
#'
#' @name sihf_team_shootouts
#' @description
#' Team shootouts per season: shootout wins and losses, attempts, goals and percentage, and attempts, goals against and saves.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 18 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | games_won_penalty | integer | Games won in a shootout. |
#'   | games_lost_penalty | integer | Shootout losses. |
#'   | pen_shots | integer | Shootout/penalty shots taken. |
#'   | pen_shot_goals | integer | Shootout/penalty goals. |
#'   | pen_shots_missed | integer | Shootout/penalty shots missed. |
#'   | pen_shots_percent | numeric | Shootout/penalty conversion percentage. |
#'   | pen_shots_against | integer | Shootout/penalty shots faced. |
#'   | pen_shot_goals_against | integer | Shootout/penalty goals allowed. |
#'   | pen_shot_saves | integer | Shootout/penalty shots saved. |
#'   | pen_shot_saves_percent | numeric | Shootout/penalty save percentage. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_shootouts(season = 2026, league_id = 1))
#' }
sihf_team_shootouts <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_shootouts", "teamShootout", season, league_id, phase_id, language, "Team Shootouts")
}

#' **SIHF Team faceoffs per season**
#'
#' @name sihf_team_faceoffs
#' @description
#' Team faceoffs per season: taken, won, lost, per game, and win percentage overall and by zone.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 18 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | faceoffs | integer | Faceoffs taken, total. |
#'   | faceoffs_per_game | numeric | Faceoffs taken per game. |
#'   | faceoffs_percent | numeric | Faceoff win percentage; '-' with no faceoffs. |
#'   | faceoffs_won | integer | Number of faceoffs won. |
#'   | faceoffs_lost | integer | Number of faceoffs lost. |
#'   | faceoffs_won_per_game | numeric | Faceoffs won per game. |
#'   | faceoffs_lost_per_game | numeric | Faceoffs lost per game. |
#'   | faceoffs_offensive_percent | numeric | Faceoff win percentage, offensive zone. |
#'   | faceoffs_neutral_percent | numeric | Faceoff win percentage, neutral zone. |
#'   | faceoffs_defensive_percent | numeric | Faceoff win percentage, defensive zone. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_faceoffs(season = 2026, league_id = 1))
#' }
sihf_team_faceoffs <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_faceoffs", "teamFaceoff", season, league_id, phase_id, language, "Team Faceoffs")
}

#' **SIHF Team faceoffs by zone (offensive, neutral, defensive) per season**
#'
#' @name sihf_team_faceoffs_by_zone
#' @description
#' Team faceoffs by zone (offensive, neutral, defensive) per season: taken, won, lost and win percentage.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 20 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | faceoffs_offensive | integer | Faceoffs taken, offensive zone. |
#'   | faceoffs_offensive_percent | numeric | Faceoff win percentage, offensive zone. |
#'   | faceoffs_won_offensive | integer | Faceoffs won, offensive zone. |
#'   | faceoffs_lost_offensive | integer | Faceoffs lost, offensive zone. |
#'   | faceoffs_neutral | integer | Faceoffs taken, neutral zone. |
#'   | faceoffs_neutral_percent | numeric | Faceoff win percentage, neutral zone. |
#'   | faceoffs_won_neutral | integer | Faceoffs won, neutral zone. |
#'   | faceoffs_lost_neutral | integer | Faceoffs lost, neutral zone. |
#'   | faceoffs_defensive | integer | Faceoffs taken, defensive zone. |
#'   | faceoffs_defensive_percent | numeric | Faceoff win percentage, defensive zone. |
#'   | faceoffs_won_defensive | integer | Faceoffs won, defensive zone. |
#'   | faceoffs_lost_defensive | integer | Faceoffs lost, defensive zone. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_faceoffs_by_zone(season = 2026, league_id = 1))
#' }
sihf_team_faceoffs_by_zone <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_faceoffs_by_zone", "teamFaceoffZone", season, league_id, phase_id, language, "Team Faceoffs By Zone")
}

#' **SIHF Team faceoffs by zone (offensive, neutral, defensive) per game**
#'
#' @name sihf_team_faceoffs_by_zone_per_game
#' @description
#' Team faceoffs by zone (offensive, neutral, defensive) per game: taken, won and lost.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 17 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | faceoffs_offensive_per_game | numeric | Faceoffs taken per game, offensive zone. |
#'   | faceoffs_won_offensive_per_game | numeric | Faceoffs won per game, offensive zone. |
#'   | faceoffs_lost_offensive_per_game | numeric | Faceoffs lost per game, offensive zone. |
#'   | faceoffs_neutral_per_game | numeric | Faceoffs taken per game, neutral zone. |
#'   | faceoffs_won_neutral_per_game | numeric | Faceoffs won per game, neutral zone. |
#'   | faceoffs_lost_neutral_per_game | numeric | Faceoffs lost per game, neutral zone. |
#'   | faceoffs_defensive_per_game | numeric | Faceoffs taken per game, defensive zone. |
#'   | faceoffs_won_defensive_per_game | numeric | Faceoffs won per game, defensive zone. |
#'   | faceoffs_lost_defensive_per_game | numeric | Faceoffs lost per game, defensive zone. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_faceoffs_by_zone_per_game(season = 2026, league_id = 1))
#' }
sihf_team_faceoffs_by_zone_per_game <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_faceoffs_by_zone_per_game", "teamFaceoffZoneGame", season, league_id, phase_id, language, "Team Faceoffs By Zone Per Game")
}

#' **SIHF Team home attendance per season**
#'
#' @name sihf_team_attendance
#' @description
#' Team home attendance per season: home games, total and average spectators, average arena capacity and utilisation.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 12 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | home_games | integer | Number of home games. |
#'   | spectators | integer | Total home attendance (SIHF prints an apostrophe thousands separator; dropped here). |
#'   | stadium_capacity_quotient | numeric | Average arena capacity. |
#'   | spectators_quotient | numeric | Average home attendance per game. |
#'   | stadium_capacity_percent | numeric | Average arena utilisation, percent. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_team_attendance(season = 2026, league_id = 1))
#' }
sihf_team_attendance <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("team_attendance", "teamSpectator", season, league_id, phase_id, language, "Team Attendance")
}

#' **SIHF Skater goals and assists by situation**
#'
#' @name sihf_player_goals_assists
#' @description
#' Skater goals and assists by situation: even strength, power play, short-handed, game-winning and overtime goals, hat tricks.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 23 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | goals | integer | Goals scored (total). |
#'   | assists | integer | Assists credited. |
#'   | assists1 | integer | Primary assists. |
#'   | assists2 | integer | Secondary assists. |
#'   | game_winning_goals | integer | Game-winning goals. |
#'   | eq_goals | integer | Even-strength goals. |
#'   | eq_assists | integer | Even-strength assists. |
#'   | pp_goals | integer | Power-play goals. |
#'   | pp_assists | integer | Power-play assists. |
#'   | pk_goals | integer | Short-handed goals. |
#'   | pk_assists | integer | Short-handed assists. |
#'   | overtime_goals | integer | Overtime goals. |
#'   | hattricks | integer | Hat tricks (games with three or more goals). |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_goals_assists(season = 2026, league_id = 1))
#' }
sihf_player_goals_assists <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_goals_assists", "playerGoalAssist", season, league_id, phase_id, language, "Player Goals Assists")
}

#' **SIHF Skater shots**
#'
#' @name sihf_player_shots
#' @description
#' Skater shots: on goal (total, slot, shooting percentage), missed, post and blocked, with per-game rates.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 21 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | sog_sog | integer | Shots on goal (saved or scored). |
#'   | sog_sog_percent | numeric | Shooting percentage (goals / shots on goal). |
#'   | sog_sog_quotient | numeric | Shots on goal per game. |
#'   | sog_sog_slot | integer | Slot shots on goal. |
#'   | sog_sog_slot_per_game | numeric | Slot shots on goal per game. |
#'   | sog_missed | integer | Missed shots, total. |
#'   | sog_missed_per_game | numeric | Missed shots per game. |
#'   | sog_post | integer | Post/crossbar shots, total. |
#'   | sog_post_per_game | numeric | Post/crossbar shots per game. |
#'   | blockings | integer | Number of shots blocked. |
#'   | blockings_per_game | numeric | Blocked shots per game. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_shots(season = 2026, league_id = 1))
#' }
sihf_player_shots <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_shots", "playerShotDetail", season, league_id, phase_id, language, "Player Shots")
}

#' **SIHF Skater penalties**
#'
#' @name sihf_player_penalties
#' @description
#' Skater penalties: penalty minutes, 2-, 5- and 10-minute penalties, game misconducts and match penalties.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 16 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | pim_total | integer | Penalty minutes, total. |
#'   | pim2 | integer | Number of 2-minute minor penalties. |
#'   | pim5 | integer | Number of 5-minute major penalties. |
#'   | pim10 | integer | Number of 10-minute misconducts. |
#'   | game_misconducts | integer | Number of game misconducts. |
#'   | match_penalties | integer | Number of match penalties. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_penalties(season = 2026, league_id = 1))
#' }
sihf_player_penalties <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_penalties", "playerFoul", season, league_id, phase_id, language, "Player Penalties")
}

#' **SIHF Skater shootouts**
#'
#' @name sihf_player_shootouts
#' @description
#' Skater shootouts: attempts, goals, misses, percentage and shootout-winning goals.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 15 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | pen_shots | integer | Shootout/penalty shots taken. |
#'   | pen_shot_goals | integer | Shootout/penalty goals. |
#'   | pen_shots_missed | integer | Shootout/penalty shots missed. |
#'   | pen_shots_percent | numeric | Shootout/penalty conversion percentage. |
#'   | pen_shot_winning_goals | integer | Deciding shootout goals. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_shootouts(season = 2026, league_id = 1))
#' }
sihf_player_shootouts <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_shootouts", "playerShootout", season, league_id, phase_id, language, "Player Shootouts")
}

#' **SIHF Skater faceoffs**
#'
#' @name sihf_player_faceoffs
#' @description
#' Skater faceoffs: taken, won, lost, per game, and win percentage overall and by zone.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 20 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | faceoffs | integer | Faceoffs taken, total. |
#'   | faceoffs_per_game | numeric | Faceoffs taken per game. |
#'   | faceoffs_percent | numeric | Faceoff win percentage; '-' with no faceoffs. |
#'   | faceoffs_won | integer | Number of faceoffs won. |
#'   | faceoffs_lost | integer | Number of faceoffs lost. |
#'   | faceoffs_won_per_game | numeric | Faceoffs won per game. |
#'   | faceoffs_lost_per_game | numeric | Faceoffs lost per game. |
#'   | faceoffs_offensive_percent | numeric | Faceoff win percentage, offensive zone. |
#'   | faceoffs_neutral_percent | numeric | Faceoff win percentage, neutral zone. |
#'   | faceoffs_defensive_percent | numeric | Faceoff win percentage, defensive zone. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_faceoffs(season = 2026, league_id = 1))
#' }
sihf_player_faceoffs <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_faceoffs", "playerFaceoff", season, league_id, phase_id, language, "Player Faceoffs")
}

#' **SIHF Skater faceoffs by zone (offensive, neutral, defensive)**
#'
#' @name sihf_player_faceoffs_by_zone
#' @description
#' Skater faceoffs by zone (offensive, neutral, defensive): taken, won, lost and win percentage.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 22 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | faceoffs_offensive | integer | Faceoffs taken, offensive zone. |
#'   | faceoffs_offensive_percent | numeric | Faceoff win percentage, offensive zone. |
#'   | faceoffs_won_offensive | integer | Faceoffs won, offensive zone. |
#'   | faceoffs_lost_offensive | integer | Faceoffs lost, offensive zone. |
#'   | faceoffs_neutral | integer | Faceoffs taken, neutral zone. |
#'   | faceoffs_neutral_percent | numeric | Faceoff win percentage, neutral zone. |
#'   | faceoffs_won_neutral | integer | Faceoffs won, neutral zone. |
#'   | faceoffs_lost_neutral | integer | Faceoffs lost, neutral zone. |
#'   | faceoffs_defensive | integer | Faceoffs taken, defensive zone. |
#'   | faceoffs_defensive_percent | numeric | Faceoff win percentage, defensive zone. |
#'   | faceoffs_won_defensive | integer | Faceoffs won, defensive zone. |
#'   | faceoffs_lost_defensive | integer | Faceoffs lost, defensive zone. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_faceoffs_by_zone(season = 2026, league_id = 1))
#' }
sihf_player_faceoffs_by_zone <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_faceoffs_by_zone", "playerFaceoffZone", season, league_id, phase_id, language, "Player Faceoffs By Zone")
}

#' **SIHF Skater faceoffs by zone (offensive, neutral, defensive) per game**
#'
#' @name sihf_player_faceoffs_by_zone_per_game
#' @description
#' Skater faceoffs by zone (offensive, neutral, defensive) per game: taken, won and lost.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 19 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | faceoffs_offensive_per_game | numeric | Faceoffs taken per game, offensive zone. |
#'   | faceoffs_won_offensive_per_game | numeric | Faceoffs won per game, offensive zone. |
#'   | faceoffs_lost_offensive_per_game | numeric | Faceoffs lost per game, offensive zone. |
#'   | faceoffs_neutral_per_game | numeric | Faceoffs taken per game, neutral zone. |
#'   | faceoffs_won_neutral_per_game | numeric | Faceoffs won per game, neutral zone. |
#'   | faceoffs_lost_neutral_per_game | numeric | Faceoffs lost per game, neutral zone. |
#'   | faceoffs_defensive_per_game | numeric | Faceoffs taken per game, defensive zone. |
#'   | faceoffs_won_defensive_per_game | numeric | Faceoffs won per game, defensive zone. |
#'   | faceoffs_lost_defensive_per_game | numeric | Faceoffs lost per game, defensive zone. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_faceoffs_by_zone_per_game(season = 2026, league_id = 1))
#' }
sihf_player_faceoffs_by_zone_per_game <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_faceoffs_by_zone_per_game", "playerFaceoffZoneGame", season, league_id, phase_id, language, "Player Faceoffs By Zone Per Game")
}

#' **SIHF Skater ice time by situation (even strength, power play, penalty kill), totals and per game, plus shifts**
#'
#' @name sihf_player_time_on_ice
#' @description
#' Skater ice time by situation (even strength, power play, penalty kill), totals and per game, plus shifts.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 21 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | position | character | Position, German whatever the language (e.g., "Stürmer"); null when SIHF prints "-". |
#'   | games_played | integer | Games played in the table's scope. |
#'   | time_on_ice_eq | character | Even-strength time on ice, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice_eq_per_game | character | Even-strength time on ice per game, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice_pp | character | Power-play time on ice, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice_pp_per_game | character | Power-play time on ice per game, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice_pk | character | Short-handed time on ice, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice_pk_per_game | character | Short-handed time on ice per game, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice | character | Time on ice, total, mm:ss string (minutes may exceed 59), not seconds. |
#'   | time_on_ice_per_game | character | Time on ice per game, mm:ss string (minutes may exceed 59), not seconds. |
#'   | shifts | integer | Number of shifts on ice. |
#'   | time_on_ice_per_shift | character | Average time on ice per shift, mm:ss string (minutes may exceed 59), not seconds; '-' with no shifts. |
#'   | shifts_per_game | numeric | Shifts per game. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_player_time_on_ice(season = 2026, league_id = 1))
#' }
sihf_player_time_on_ice <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("player_time_on_ice", "playerTimeOnIce", season, league_id, phase_id, language, "Player Time On Ice")
}

#' **SIHF Goaltender shots against**
#'
#' @name sihf_goalie_shots
#' @description
#' Goaltender shots against: on goal (total, slot), missed, post, with per-game rates and the slot save percentage.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 18 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | soga_sog | integer | Shots on goal against, total. |
#'   | soga_sog_per_game | numeric | Shots on goal against per game. |
#'   | soga_sog_slot | integer | Slot shots on goal against. |
#'   | soga_sog_slot_per_game | numeric | Slot shots on goal against per game. |
#'   | soga_missed | integer | Missed shots against. |
#'   | soga_missed_per_game | numeric | Missed shots against per game. |
#'   | soga_post | integer | Post/crossbar shots against. |
#'   | soga_post_per_game | numeric | Post/crossbar shots against per game. |
#'   | saves_slot_percentage | numeric | Save percentage on slot shots. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_goalie_shots(season = 2026, league_id = 1))
#' }
sihf_goalie_shots <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("goalie_shots", "goalkeeperShotDetail", season, league_id, phase_id, language, "Goalie Shots")
}

#' **SIHF Goaltender decisions**
#'
#' @name sihf_goalie_wins
#' @description
#' Goaltender decisions: starts, wins and losses in regulation, overtime and shootout, and shutouts.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 19 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | first_keeper | integer | Games started as goalkeeper (GPI). |
#'   | wins | integer | Wins, all decision types. |
#'   | wins_intime | integer | Wins in regulation. |
#'   | wins_overtime | integer | Games won in overtime. |
#'   | wins_penalty | integer | Games won in a shootout. |
#'   | losses | integer | Losses, all decision types. |
#'   | losses_intime | integer | Losses in regulation. |
#'   | losses_overtime | integer | Overtime losses. |
#'   | losses_penalty | integer | Shootout losses. |
#'   | shutout | integer | Shutouts (games with no goal against). |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_goalie_wins(season = 2026, league_id = 1))
#' }
sihf_goalie_wins <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("goalie_wins", "goalkeeperWin", season, league_id, phase_id, language, "Goalie Wins")
}

#' **SIHF Goaltender shootouts**
#'
#' @name sihf_goalie_shootouts
#' @description
#' Goaltender shootouts: attempts faced, goals against, saves and save percentage.
#'
#' Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`, keyless JSON). SIHF keys a
#' season by its END year (2026 = 2025-26). An unknown `season`, `league_id` or `phase_id` is never an
#' error upstream (SIHF silently answers another one), so every table answer is checked against the
#' body's own filter echo and refused when it differs. Every request is spaced at least one second
#' from the last one to the host.
#'
#' @param season Required. Season by its END year (2026 = 2025-26). A season SIHF does not have raises `fastRhockey_no_data` error (the API itself silently answers another one).
#' @param league_id SIHF league id: 1 = National League, 2 = Swiss League, 100 = MyHockey League, 3 = 1. Liga, 43 = SWHL B, 113 = U21-Elit (the body's League filter lists them). An id SIHF does not have for the season raises `fastRhockey_no_data` error.
#' @param phase_id SIHF phase id (regular season, a playoff round, ...; the body's Phase filter lists them). `NULL` = no phase filter: the season summary over every phase.
#' @param language Response language. Only part of the body is translated: status, position and penalty names stay German.
#'
#' @section Errors:
#' - `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a requested season, league or
#'   phase and served another one.
#' - `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is invalid.
#' - `fastRhockey_asset_fetch`: any other non-2xx answer or a dead connection after retries, an empty or
#'   unreadable body, a stub or redirect body, a table without its filter echo, a truncated table, or a
#'   `gameoverview` body for another game.
#'
#' @return A `fastRhockey_data` tibble with 13 columns. A zero-row tibble with these columns when the
#'   payload holds no rows.
#'
#'   | col_name | type | description |
#'   |:---------|:-----|:------------|
#'   | season | integer | Season by its END year (2026 = 2025-26), from the body's echo of the Season filter. |
#'   | phase_id | integer | SIHF phase id (regular season, a playoff round, ...); null unless a phase filter is passed. |
#'   | rank | integer | Rank in the table; null on a summary row, and on a goaltender below the qualification line. |
#'   | player | character | Player name as SIHF prints it, last name first (e.g., "Dahlbeck Klas"); season tables carry no player id. |
#'   | team_id | integer | SIHF team id (integer join key); null on a summary row (the pseudo-team 100000). |
#'   | team_name | character | Team display name as SIHF publishes it; "Durchschnitt" on the league-average row, "Total" on the attendance total row. |
#'   | team_acronym | character | Team abbreviation as SIHF publishes it (e.g., "HCD"); null on a summary row. |
#'   | games_played | integer | Games played in the table's scope. |
#'   | pen_shots_against | integer | Shootout/penalty shots faced. |
#'   | pen_shot_goals_against | integer | Shootout/penalty goals allowed. |
#'   | pen_shot_saves | integer | Shootout/penalty shots saved. |
#'   | pen_shot_saves_percent | numeric | Shootout/penalty save percentage. |
#'   | row_type | character | "data" for a regular row; "average" (league average) or "total" (attendance total) for a summary row SIHF mixes into the table. |
#'
#' @family SIHF Functions
#' @keywords SIHF
#' @export
#' @examples
#' \donttest{
#'   try(sihf_goalie_shootouts(season = 2026, league_id = 1))
#' }
sihf_goalie_shootouts <- function(season, league_id = 1L, phase_id = NULL, language = "en") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .sihf_table_route("goalie_shootouts", "goalkeeperShootout", season, league_id, phase_id, language, "Goalie Shootouts")
}
