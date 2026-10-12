# shl.R -- SHL (Swedish Hockey League) wrappers over www.shl.se/api.
#
# Mirrors sdv-py sportsdataverse/hockey/shl/ (shl_runtime.py, shl_parsers.py and
# the generated shl.py, sdv-py #826). The SHL site API (Sportality platform,
# stats by StatNet) is keyless JSON. Unofficial use of a public source.
#
# * Seasons are END years (2026 = 2025/26). The SHL keys a season by an "ssgt"
#   uuid (season + series + game type); .shl_get() maps (END year, game type)
#   through /statistics-v2/layout-info (fetched once per session) to the ssgt
#   (stats-info, match-day routes) or the season/series/gameType uuid triple
#   (game-schedule). The athlete season game log takes the START year.
# * stats-info truncates silently: it is always asked for count = 1000 and a
#   page that reaches the count, or falls short of its totalCount, is refused.
# * gameday/* starts in 2023/24; older games answer 200 with an EMPTY body.
# * Three player id systems: isa (StatNet, Int) is what play-by-play, box
#   scores and stats tables key on; the platform athlete uuid keys rosters and
#   athlete routes; ramses_legacy (athlete-details only). shl_player_crosswalk()
#   joins isa to uuid.
# * Every request goes through .polite_perform(): >= 1 s between two requests to
#   the host, on every attempt.

.SHL_BASE <- "https://www.shl.se/api"
.SHL_GAME_TYPES <- c("regular", "playoff", "qualdown", "playin")
.SHL_STATS_COUNT <- 1000L
.SHL_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status", "statusCode")

# Route (path below /api; ids as [^/]+) -> pluck path of the rows that must be present.
.SHL_ROWS <- c(
  "/sports-v2/season-series-game-types-filter" = "season[]",
  "/statistics-v2/layout-info" = "[].seasonSeriesGameTypeTeams[]",
  "/sports-v2/game-schedule" = "gameInfo[]",
  "/sports-v2/game-info/[^/]+" = "gameInfo",
  "/sports-v2/teams/[^/]+" = "",
  "/sports-v2/athletes/by-team-uuid/[^/]+" = "[].players[]",
  "/sports-v2/athlete-details/[^/]+" = "athleteData",
  "/sports-v2/all-sites/[^/]+" = "[]",
  "/sports-v2/played-games/[^/]+" = "playedGames[]",
  "/sports-v2/upcoming-games/[^/]+" = "upcomingGames[]",
  "/sports-v2/last-game-day" = "games[]",
  "/sports-v2/next-game-day" = "games[]",
  "/gameday/play-by-play/[^/]+" = "[]",
  "/gameday/boxscore/[^/]+" = "stats.*[]",
  "/gameday/player-stats/[^/]+" = "stats.*[]",
  "/gameday/team-stats/[^/]+" = "home",
  "/gameday/game-info/[^/]+" = "teamHead2Head[]",
  "/gameday/game-overview/[^/]+" = "homeTeam",
  "/gameday/team-standings/[^/]+" = "leagueStandings[]",
  "/gameday/post-game-data/team-stats/[^/]+" = "stats[]",
  "/statistics-v2/stats-info/[^/]+" = "[].stats[]",
  "/statistics-v2/athlete/profile-page" = "",
  "/statistics-v2/athlete/seasonList" = "[]",
  "/statistics-v2/athlete/playerProfile_gameLog" = "[].stats[]",
  "/statistics-v2/athlete/playerProfile_seasonGameLog" = "[].stats[]",
  "/statistics-v2/player-h2h-v2/[^/]+/[^/]+/[^/]+" = "players[]"
)
# Route -> c(pluck path of the field that must echo the request, where the asked-for id is):
# "path" = last path segment, "ssgt" = the ssgt this call resolved, else a query key.
.SHL_SCOPE <- list(
  "/sports-v2/game-schedule" = c("gameInfo[].ssgtUuid", "ssgt"),
  "/sports-v2/game-info/[^/]+" = c("gameInfo.gameUuid", "path"),
  "/sports-v2/teams/[^/]+" = c("uuid", "path"),
  "/sports-v2/athlete-details/[^/]+" = c("athleteData.uuid", "path"),
  "/sports-v2/last-game-day" = c("games[].ssgtUuid", "ssgt"),
  "/sports-v2/next-game-day" = c("games[].ssgtUuid", "ssgt"),
  "/gameday/play-by-play/[^/]+" = c("[].gameUuid", "path"),
  "/gameday/team-stats/[^/]+" = c("home.gameUuid", "path"),
  "/gameday/game-overview/[^/]+" = c("gameUuid", "path"),
  "/gameday/team-standings/[^/]+" = c("ssgtUuid", "path"),
  "/statistics-v2/athlete/profile-page" = c("uuid", "playerUuid")
)
.SHL_TEXT_ROUTE <- "/sports-v2/ssgt/[^/]+/provider"
.SHL_STATS_ROUTE <- "/statistics-v2/stats-info/[^/]+"
# Routes whose `season` (END year) + `gameType` the runtime maps to the provider's ssgt keys.
.SHL_SEASON_ROUTES <- c(.SHL_STATS_ROUTE, "/sports-v2/game-schedule",
                        "/sports-v2/last-game-day", "/sports-v2/next-game-day")

# layout-info directory, fetched once per session
.shl_state <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# JSON value helpers (bodies are decoded with simplifyVector = FALSE: an object
# is a NAMED list, an array an unnamed list, null is NULL)
# ---------------------------------------------------------------------------

.shl_is_obj <- function(x) is.list(x) && !is.null(names(x))
.shl_is_arr <- function(x) is.list(x) && is.null(names(x))
.shl_dict <- function(x) if (.shl_is_obj(x)) x else list()
.shl_list <- function(x) if (.shl_is_arr(x)) x else list()
.shl_scalar <- function(x) is.atomic(x) && length(x) == 1L && !is.null(x)
.shl_is_num <- function(x) .shl_scalar(x) && is.numeric(x)
.shl_truthy <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) return(length(x) > 0)
  if (!.shl_scalar(x)) return(length(x) > 0)
  if (is.character(x)) return(nzchar(x))
  !is.na(x) && x != 0
}

# Python repr(float): shortest round-trip digits, fixed notation for 1e-4 <= |x| < 1e16
.shl_float_repr <- function(x) {
  if (is.na(x)) return("nan")
  if (is.infinite(x)) return(if (x > 0) "inf" else "-inf")
  if (x == 0) return(if (1 / x < 0) "-0.0" else "0.0")
  d <- 1L
  while (d < 17L && as.numeric(sprintf("%.*e", d - 1L, x)) != x) d <- d + 1L
  sci <- sprintf("%.*e", d - 1L, abs(x))
  mant <- sub("e.*$", "", sci)
  e <- as.integer(sub("^.*e", "", sci))
  digits <- sub("\\.", "", mant)
  sign <- if (x < 0) "-" else ""
  body <- if (e >= -4L && e < 16L) {
    if (e >= 0L) {
      if (nchar(digits) <= e + 1L) paste0(digits, strrep("0", e + 1L - nchar(digits)), ".0")
      else paste0(substr(digits, 1L, e + 1L), ".", substr(digits, e + 2L, nchar(digits)))
    } else {
      paste0("0.", strrep("0", -e - 1L), digits)
    }
  } else {
    m <- if (nchar(digits) > 1L) paste0(substr(digits, 1L, 1L), ".", substr(digits, 2L, nchar(digits))) else digits
    paste0(m, "e", if (e < 0L) "-" else "+", formatC(abs(e), width = 2L, flag = "0"))
  }
  paste0(sign, body)
}

# Python str(v) for a JSON scalar
.shl_pystr <- function(v) {
  if (is.null(v)) return("None")
  if (is.logical(v)) return(if (isTRUE(v)) "True" else "False")
  if (is.integer(v)) return(as.character(v))
  if (is.double(v)) return(.shl_float_repr(v))
  as.character(v)
}

.shl_jstr <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  s <- gsub("\b", "\\b", s, fixed = TRUE)
  s <- gsub("\f", "\\f", s, fixed = TRUE)
  paste0("\"", s, "\"")
}

# json.dumps(v, ensure_ascii=False, sort_keys=True)
.shl_json <- function(v) {
  if (is.null(v)) return("null")
  if (.shl_is_obj(v)) {
    if (!length(v)) return("{}")
    ord <- order(names(v), method = "radix")
    return(paste0("{", paste0(.shl_jstr(names(v)[ord]), ": ", vapply(v[ord], .shl_json, ""), collapse = ", "), "}"))
  }
  if (is.list(v)) {
    if (!length(v)) return("[]")
    return(paste0("[", paste(vapply(v, .shl_json, ""), collapse = ", "), "]"))
  }
  if (is.logical(v)) return(if (isTRUE(v)) "true" else "false")
  if (is.integer(v)) return(as.character(v))
  if (is.double(v)) return(.shl_float_repr(v))
  .shl_jstr(as.character(v))
}

# A cell as text (polars Utf8): NA for null, JSON for list/object cells
.shl_text <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (is.list(v)) return(.shl_json(v))
  if (is.logical(v)) return(if (isTRUE(v)) "true" else "false")
  .shl_pystr(v)
}

.shl_as_float <- function(v) {
  if (.shl_is_num(v)) return(as.numeric(v))
  if (is.character(v) && length(v) == 1L) {
    s <- trimws(v)
    if (grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", s)) return(as.numeric(s))
  }
  NA_real_
}

# ---------------------------------------------------------------------------
# Rows: a row is a named list; setting a key to NULL keeps the key (a null cell)
# ---------------------------------------------------------------------------

.shl_set <- function(row, key, value) {
  row[key] <- list(value)
  row
}
.shl_update <- function(row, other) {
  for (i in seq_along(other)) row[names(other)[i]] <- other[i]
  row
}
# Nested objects -> one flat row, keys joined by "_" (arrays stay as values)
.shl_flatten <- function(obj, prefix = "") {
  out <- list()
  nms <- names(obj)
  for (i in seq_along(obj)) {
    k <- nms[i]
    if (is.na(k) || !nzchar(k)) k <- "\001"
    v <- obj[[i]]
    key <- if (nzchar(prefix)) paste0(prefix, "_", k) else k
    if (.shl_is_obj(v) && length(v)) {
      out <- .shl_update(out, .shl_flatten(v, key))
    } else {
      out[key] <- list(if (.shl_is_obj(v)) NULL else v)
    }
  }
  out
}

# Wire key -> snake_case column ("SVS%" -> svs_perc, "+/-" -> plus_minus)
.shl_column <- function(key) {
  key <- gsub("+/-", "PlusMinus", key, fixed = TRUE)
  key <- gsub("%", "Perc", key, fixed = TRUE)
  w <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", key, perl = TRUE)
  w <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", w, perl = TRUE)
  w <- tolower(gsub("-", "_", w, fixed = TRUE))
  w <- gsub("[^0-9a-z_]+", "_", w, perl = TRUE)
  gsub("^_+|_+$", "", w)
}

# isa player ids, StatNet game / event ids are integers in every frame
.SHL_INT_ID <- "(^|_)(player_id|game_id|event_id|ext_id)$"
# Stat cells that are labels or placeholders, not measurements
.SHL_NOT_STATS <- c("Rank", "NR", "LINE", "POS")
# Stats-table display slots that hold a constant 0 (the site fills them from the info record)
.SHL_PLACEHOLDERS <- c("Player", "Team", "Season", "Series", "GameType")
.SHL_TEAM_PREFIX <- c(homeTeamInfo = "homeTeam", awayTeamInfo = "awayTeam",
                      venueInfo = "venue", seriesInfo = "series")
.SHL_SCHEDULE_KEYS <- c("gameInfo", "games", "playedGames", "upcomingGames")

# ---------------------------------------------------------------------------
# Column typing
# ---------------------------------------------------------------------------

# Whole-number values as integer; a value beyond 32 bits keeps the column double
.shl_int_or_dbl <- function(x) {
  if (all(is.na(x) | abs(x) <= .Machine$integer.max)) as.integer(x) else x
}

.shl_series <- function(values, stat) {
  present <- Filter(Negate(is.null), values)
  if (stat && length(present) && all(!is.na(vapply(present, .shl_as_float, 0)))) {
    return(vapply(values, function(v) if (is.null(v)) NA_real_ else .shl_as_float(v), 0))
  }
  if (length(present) && all(vapply(present, function(v) .shl_scalar(v) && is.logical(v), TRUE))) {
    return(vapply(values, function(v) if (is.null(v)) NA else v, TRUE))
  }
  if (length(present) && all(vapply(present, .shl_is_num, TRUE))) {
    if (all(vapply(present, is.integer, TRUE))) {
      return(vapply(values, function(v) if (is.null(v)) NA_integer_ else v, 1L))
    }
    return(vapply(values, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0))
  }
  vapply(values, .shl_text, "")
}

# Flat wire-keyed rows -> named list of equal-length column vectors (NULL when no rows)
.shl_frame <- function(rows, stat_keys = character()) {
  rows <- Filter(function(r) .shl_is_obj(r) && any(!vapply(r, is.null, TRUE)), rows)
  if (!length(rows)) return(NULL)
  keys <- unique(unlist(lapply(rows, names), use.names = FALSE))
  cols <- list()
  for (k in keys) {
    name <- .shl_column(k)
    if (!nzchar(name) || !is.null(cols[[name]])) next
    values <- lapply(rows, function(r) r[[k]])
    ids_ok <- endsWith(name, "_ids") && any(vapply(values, function(v) {
      .shl_is_arr(v) && length(v) > 0 && all(vapply(v, .shl_is_num, TRUE))
    }, TRUE))
    if (ids_ok) {
      cols[[name]] <- lapply(values, function(v) {
        if (is.null(v)) return(NULL)
        vapply(v, function(x) if (is.null(x)) NA_integer_ else as.integer(x), 1L)
      })
      next
    }
    cols[[name]] <- .shl_series(values, k %in% stat_keys || endsWith(name, "_perc"))
  }
  for (nm in grep(.SHL_INT_ID, names(cols), value = TRUE)) {
    if (!is.integer(cols[[nm]]) && !is.list(cols[[nm]])) cols[[nm]] <- .shl_as_int(cols[[nm]])
  }
  cols
}

# polars cast(strict = FALSE) of a column to the documented dtype codes
.shl_as_chr <- function(x) {
  if (is.list(x)) return(rep(NA_character_, length(x)))
  if (is.character(x)) return(x)
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x, "true", "false")))
  if (is.integer(x)) return(as.character(x))
  vapply(x, function(v) if (is.na(v)) NA_character_ else .shl_float_repr(v), "")
}
.shl_as_int <- function(x) {
  if (is.list(x)) return(rep(NA_integer_, length(x)))
  if (is.integer(x)) return(x)
  v <- if (is.character(x)) {
    ok <- grepl("^[+-]?[0-9]+$", trimws(x))
    out <- rep(NA_real_, length(x))
    out[ok] <- as.numeric(trimws(x)[ok])
    out
  } else {
    as.numeric(x)
  }
  v[!is.finite(v)] <- NA_real_
  .shl_int_or_dbl(trunc(v))
}
.shl_as_dbl <- function(x) {
  if (is.list(x)) return(rep(NA_real_, length(x)))
  if (is.character(x)) return(vapply(x, .shl_as_float, 0, USE.NAMES = FALSE))
  as.numeric(x)
}
.shl_as_lgl <- function(x) {
  if (is.list(x)) return(rep(NA, length(x)))
  if (is.logical(x)) return(x)
  if (is.character(x)) return(ifelse(x == "true", TRUE, ifelse(x == "false", FALSE, NA)))
  as.numeric(x) != 0
}
.shl_as_ids <- function(x) {
  if (is.list(x)) return(lapply(x, function(v) if (is.null(v)) NULL else as.integer(v)))
  lapply(seq_along(x), function(i) NULL)
}
.shl_cast <- function(x, code) {
  switch(code,
    str = .shl_as_chr(x), int = .shl_as_int(x), float = .shl_as_dbl(x),
    bool = .shl_as_lgl(x), `list[int]` = .shl_as_ids(x)
  )
}
.shl_null_col <- function(code, n) {
  switch(code,
    str = rep(NA_character_, n), int = rep(NA_integer_, n), float = rep(NA_real_, n),
    bool = rep(NA, n), `list[int]` = vector("list", n)
  )
}

# Zero-row tibble with the documented columns and types
.shl_empty <- function(schema) {
  type <- c(str = "character", int = "integer", float = "double", bool = "logical")
  atomic <- schema[schema != "list[int]"]
  out <- .empty_tibble(stats::setNames(unname(type[atomic]), names(atomic)))
  for (nm in names(schema)[schema == "list[int]"]) out[[nm]] <- list()
  out[names(schema)]
}

# Conform columns to the documented schema: documented columns first, in order and with
# the documented types (a missing one is a null column), anything the API added after them.
# An empty body gives the same columns with zero rows. schema = NULL: no conforming.
.shl_finish <- function(cols, schema = NULL) {
  if (is.null(schema)) {
    return(if (is.null(cols)) tibble::tibble() else tibble::as_tibble(cols))
  }
  if (is.null(cols)) return(.shl_empty(schema))
  n <- length(cols[[1]])
  out <- list()
  for (nm in names(schema)) {
    out[[nm]] <- if (nm %in% names(cols)) .shl_cast(cols[[nm]], schema[[nm]]) else .shl_null_col(schema[[nm]], n)
  }
  for (nm in setdiff(names(cols), names(schema))) out[[nm]] <- cols[[nm]]
  tibble::as_tibble(out)
}

# ---------------------------------------------------------------------------
# Parsers (one per body shape; each returns the conformed tibble)
# ---------------------------------------------------------------------------

# Generic row rule: a list -> one row per item; a dict holding exactly one list -> that
# list's items; any other non-empty dict -> one row; the provider route's bare word -> one row.
.shl_parse_generic <- function(raw, schema = NULL) {
  rows <- list()
  if (is.character(raw) && length(raw) == 1L && grepl("^[a-z_]{1,40}$", trimws(raw))) {
    rows <- list(list(provider = trimws(raw)))
  } else if (.shl_is_arr(raw)) {
    rows <- raw
  } else if (.shl_is_obj(raw) && length(raw)) {
    lists <- Filter(.shl_is_arr, raw)
    rows <- if (length(lists) == 1L) lists[[1]] else list(raw)
  }
  flat <- lapply(Filter(.shl_is_obj, rows), .shl_flatten)
  .shl_finish(.shl_frame(flat), schema)
}

# A page object (a team, a game header, an athlete profile): ONE row
.shl_parse_page <- function(raw, schema = NULL) {
  rows <- if (.shl_is_obj(raw) && length(raw)) list(.shl_flatten(raw)) else list()
  .shl_finish(.shl_frame(rows), schema)
}

# game-schedule, last|next-game-day, played|upcoming-games: one row per game
.shl_parse_schedule <- function(raw, schema = NULL) {
  games <- list()
  if (.shl_is_obj(raw)) {
    for (k in .SHL_SCHEDULE_KEYS) {
      if (.shl_is_arr(raw[[k]])) {
        games <- raw[[k]]
        break
      }
    }
  }
  rows <- list()
  for (g in games) {
    if (!.shl_is_obj(g)) next
    row <- list()
    for (i in seq_along(g)) {
      k <- names(g)[i]
      v <- g[[i]]
      if (k == "uuid") {
        row <- .shl_set(row, "gameUuid", v)
      } else if (k %in% names(.SHL_TEAM_PREFIX) && .shl_is_obj(v)) {
        row <- .shl_update(row, .shl_flatten(v, .SHL_TEAM_PREFIX[[k]]))
      } else {
        row <- .shl_update(row, .shl_flatten(stats::setNames(list(v), k)))
      }
    }
    rows[[length(rows) + 1L]] <- row
  }
  .shl_finish(.shl_frame(rows), schema)
}

# A pbp player record -> <prefix>playerId (isa) + its names; `statistics` dropped
.shl_player <- function(prefix, rec) {
  if (!.shl_is_obj(rec)) return(list())
  rec <- rec[names(rec) != "statistics"]
  out <- .shl_set(list(), paste0(prefix, "playerId"), rec[["playerId"]])
  rec <- rec[names(rec) != "playerId"]
  pre <- sub("_+$", "", prefix)
  .shl_update(out, .shl_flatten(rec, if (nzchar(pre)) pre else "player"))
}
.shl_isa_list <- function(recs) {
  if (!.shl_is_arr(recs)) return(NULL)
  lapply(recs, function(r) {
    pid <- .shl_pystr(.shl_dict(r)[["playerId"]])
    if (grepl("^[0-9]+$", pid)) as.integer(pid) else NULL
  })
}

.shl_parse_pbp <- function(raw, schema = NULL) {
  rows <- list()
  for (ev in if (.shl_is_arr(raw)) raw else list()) {
    if (!.shl_is_obj(ev)) next
    row <- list()
    for (i in seq_along(ev)) {
      k <- names(ev)[i]
      v <- ev[[i]]
      if (k %in% c("homeTeam", "awayTeam", "eventTeam") && .shl_is_obj(v)) {
        row <- .shl_update(row, .shl_flatten(v, substr(k, 1L, nchar(k) - 4L)))
      } else if (k == "player") {
        row <- .shl_update(row, .shl_player("", v))
      } else if (k == "assists" && .shl_is_obj(v)) {
        row <- .shl_update(row, .shl_player("assist1_", v[["first"]]))
        row <- .shl_update(row, .shl_player("assist2_", v[["second"]]))
      } else if (k %in% c("pop", "nep")) {
        row <- .shl_set(row, paste0(k, "PlayerIds"), .shl_isa_list(v))
      } else {
        row <- .shl_update(row, .shl_flatten(stats::setNames(list(v), k)))
      }
    }
    rows[[length(rows) + 1L]] <- row
  }
  .shl_finish(.shl_frame(rows), schema)
}

# boxscore / player-stats: one row per player and game (skaters from stats{}, goalies from gkStats{})
.shl_parse_player_box <- function(raw, schema = NULL) {
  rows <- list()
  stats <- character()
  body <- .shl_dict(raw)
  groups <- list(c("skater", "stats", "players"), c("goalie", "gkStats", "goalkeepers"))
  sides_def <- list(c("homeTeamValue", "home"), c("awayTeamValue", "away"))
  for (grp in groups) {
    sides <- body[[grp[2]]]
    names_map <- .shl_dict(body[[grp[3]]])
    for (sd in sides_def) {
      recs <- if (.shl_is_obj(sides)) sides[[sd[1]]] else NULL
      side_names <- .shl_dict(names_map[[sd[1]]])
      for (rec in if (.shl_is_arr(recs)) recs else list()) {
        if (!.shl_is_obj(rec)) next
        info <- .shl_dict(rec[["info"]])
        pid <- info[["playerId"]]
        nm <- .shl_dict(side_names[[.shl_pystr(pid)]])
        row <- list(teamSide = sd[2], positionGroup = grp[1])
        row <- .shl_set(row, "playerId", pid)
        row <- .shl_set(row, "teamId", info[["teamId"]])
        row <- .shl_set(row, "period", info[["period"]])
        row <- .shl_update(row, nm[names(nm) != "extIds"])
        cells <- rec[names(rec) != "info"]
        keep <- !names(cells) %in% .SHL_NOT_STATS & !vapply(cells, is.list, TRUE)
        stats <- union(stats, names(cells)[keep])
        row <- .shl_update(row, cells)
        rows[[length(rows) + 1L]] <- row
      }
    }
  }
  .shl_finish(.shl_frame(rows, stats), schema)
}

# team-stats: one row per side and period (period 0 = the game)
.shl_parse_team_stats <- function(raw, schema = NULL) {
  rows <- list()
  stat_keys <- character()
  body <- .shl_dict(raw)
  for (side in c("home", "away")) {
    team <- body[[side]]
    if (!.shl_is_obj(team)) next
    head <- .shl_update(list(teamSide = side), team[names(team) != "statistics"])
    for (per in .shl_list(team[["statistics"]])) {
      if (!.shl_is_obj(per)) next
      row <- .shl_set(head, "period", per[["period"]])
      for (kv in .shl_list(per[["parsedTotalStatistics"]])) {
        if (.shl_is_obj(kv) && .shl_truthy(kv[["key"]])) {
          key <- .shl_pystr(kv[["key"]])
          row <- .shl_set(row, key, kv[["value"]])
          stat_keys <- c(stat_keys, key)
        }
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  .shl_finish(.shl_frame(rows, stat_keys), schema)
}

# team-standings: one row per team; season is the END year
.shl_parse_standings <- function(raw, schema = NULL) {
  body <- .shl_dict(raw)
  start <- .shl_pystr(body[["season"]])
  rows <- list()
  for (rec in .shl_list(body[["leagueStandings"]])) {
    if (!.shl_is_obj(rec)) next
    info <- .shl_dict(rec[["info"]])
    team <- .shl_dict(info[["teamInfo"]])
    row <- list()
    row <- .shl_set(row, "season", if (grepl("^[0-9]+$", start)) as.integer(start) + 1L else NULL)
    row <- .shl_set(row, "ssgtUuid", body[["ssgtUuid"]])
    row <- .shl_set(row, "teamCode", rec[["id"]])
    row <- .shl_set(row, "teamUuid", team[["teamUuid"]])
    row <- .shl_set(row, "teamName", .shl_dict(team[["teamNames"]])[["long"]])
    row <- .shl_set(row, "teamShortName", info[["code"]])
    row <- .shl_update(row, rec[!names(rec) %in% c("id", "info")])
    rows[[length(rows) + 1L]] <- row
  }
  .shl_finish(.shl_frame(rows), schema)
}

# The one dict of a one-item-list body (stats-info, game logs); raw otherwise
.shl_body <- function(raw) {
  if (.shl_is_arr(raw) && length(raw) && .shl_is_obj(raw[[1]])) raw[[1]] else raw
}

# players{<isa>: {uuid}} -> {uuid: isa}, the first isa wins
.shl_isa_by_uuid <- function(players) {
  out <- list()
  if (.shl_is_obj(players)) {
    for (i in seq_along(players)) {
      k <- names(players)[i]
      v <- players[[i]]
      if (.shl_is_obj(v) && .shl_truthy(v[["uuid"]]) && grepl("^[0-9]+$", k)) {
        u <- .shl_pystr(v[["uuid"]])
        if (is.null(out[[u]])) out[[u]] <- as.integer(k)
      }
    }
  }
  out
}

# stats-info/{module} and the athlete game logs: one row per stats[] entry; player_id
# (isa) is looked up from the body's players{} map
.shl_parse_stats_info <- function(raw, schema = NULL) {
  body <- .shl_body(raw)
  if (!.shl_is_obj(body)) return(.shl_finish(NULL, schema))
  single <- identical(body[["type"]], "athlete_single")
  isa <- if (single) list() else .shl_isa_by_uuid(body[["players"]])
  rows <- list()
  stats <- character()
  for (rec in .shl_list(body[["stats"]])) {
    if (!.shl_is_obj(rec)) next
    info <- .shl_dict(rec[["info"]])
    row <- list()
    if (length(isa)) row <- .shl_set(row, "playerId", isa[[.shl_pystr(info[["uuid"]])]])
    cells <- rec[names(rec) != "info"]
    drop <- names(cells) %in% .SHL_PLACEHOLDERS & vapply(cells, .shl_is_num, TRUE)
    cells <- cells[!drop]
    keep <- !names(cells) %in% .SHL_NOT_STATS & !vapply(cells, is.list, TRUE)
    stats <- union(stats, names(cells)[keep])
    row <- .shl_update(row, .shl_flatten(cells))
    row <- .shl_update(row, .shl_flatten(info, "info"))
    rows[[length(rows) + 1L]] <- row
  }
  .shl_finish(.shl_frame(rows, stats), schema)
}

# players{} of a stats-info body: isa -> athlete uuid (no documented schema)
.shl_parse_crosswalk <- function(raw) {
  body <- .shl_body(raw)
  players <- if (.shl_is_obj(body)) body[["players"]] else NULL
  rows <- list()
  if (.shl_is_obj(players)) {
    for (i in seq_along(players)) {
      k <- names(players)[i]
      v <- players[[i]]
      if (!.shl_is_obj(v) || !grepl("^[0-9]+$", k)) next
      rec <- v[!names(v) %in% c("id", "uuid", "type", "playerMedia", "teamPlayers")]
      row <- .shl_set(list(), "playerId", as.integer(k))
      row <- .shl_set(row, "athleteUuid", v[["uuid"]])
      rows[[length(rows) + 1L]] <- .shl_update(row, .shl_flatten(rec))
    }
  }
  .shl_finish(.shl_frame(rows), NULL)
}

# layout-info: one row per ssgt (season + series + game type)
.shl_parse_layout <- function(raw, schema = NULL) {
  body <- .shl_body(raw)
  ssgts <- if (.shl_is_obj(body)) body[["seasonSeriesGameTypeTeams"]] else NULL
  rows <- list()
  for (s in if (.shl_is_arr(ssgts)) ssgts else list()) {
    if (!.shl_is_obj(s)) next
    season <- .shl_dict(s[["season"]])
    gtype <- .shl_dict(s[["gameType"]])
    series <- .shl_dict(s[["series"]])
    code <- if (.shl_truthy(season[["code"]])) .shl_pystr(season[["code"]]) else ""
    ok <- grepl("^[0-9]+$", code)
    row <- list()
    row <- .shl_set(row, "season", if (ok) as.integer(code) + 1L else NULL)
    row <- .shl_set(row, "seasonCode", if (nzchar(code)) code else NULL)
    row <- .shl_set(row, "seasonName", season[["name"]])
    row <- .shl_set(row, "gameType", gtype[["code"]])
    row <- .shl_set(row, "gameTypeName", gtype[["name"]])
    row <- .shl_set(row, "seriesCode", series[["code"]])
    row <- .shl_set(row, "ssgtUuid", s[["ssgtUuid"]])
    row <- .shl_set(row, "state", s[["state"]])
    row <- .shl_set(row, "seasonUuid", season[["uuid"]])
    row <- .shl_set(row, "seriesUuid", series[["uuid"]])
    row <- .shl_set(row, "gameTypeUuid", gtype[["uuid"]])
    rows[[length(rows) + 1L]] <- row
  }
  .shl_finish(.shl_frame(rows), schema)
}

# season-series-game-types-filter: one row per series, season and game type
.shl_parse_filter <- function(raw, schema = NULL) {
  body <- .shl_dict(raw)
  rows <- list()
  for (def in list(c("series", "series"), c("season", "season"), c("gameType", "game_type"))) {
    for (rec in .shl_list(body[[def[1]]])) {
      if (!.shl_is_obj(rec)) next
      nm <- list()
      for (n in .shl_list(rec[["names"]])) {
        if (.shl_is_obj(n)) nm[.shl_pystr(n[["language"]])] <- list(n[["translation"]])
      }
      name <- if (.shl_truthy(nm[["en"]])) nm[["en"]] else if (length(nm)) nm[[1]] else NULL
      row <- list()
      row <- .shl_set(row, "kind", def[2])
      row <- .shl_set(row, "code", rec[["code"]])
      row <- .shl_set(row, "name", name)
      row <- .shl_set(row, "uuid", rec[["uuid"]])
      row <- .shl_set(row, "deleted", rec[["deleted"]])
      rows[[length(rows) + 1L]] <- row
    }
  }
  .shl_finish(.shl_frame(rows), schema)
}

# athletes/by-team-uuid: one row per player of the current roster
.shl_parse_roster <- function(raw, schema = NULL) {
  rows <- list()
  for (group in if (.shl_is_arr(raw)) raw else list()) {
    if (!.shl_is_obj(group)) next
    for (p in .shl_list(group[["players"]])) {
      if (!.shl_is_obj(p)) next
      portrait <- .shl_dict(p[["renderedLatestPortrait"]])
      rec <- p[!names(p) %in% c("portraitList", "renderedLatestPortrait", "globalPortraitFallback")]
      row <- .shl_set(list(), "position", group[["position"]])
      row <- .shl_set(row, "positionCode", group[["positionCode"]])
      row <- .shl_update(row, .shl_flatten(rec))
      row <- .shl_set(row, "portraitUrl", portrait[["url"]])
      rows[[length(rows) + 1L]] <- row
    }
  }
  .shl_finish(.shl_frame(rows), schema)
}

# player-h2h-v2: one row per player of the head-to-head
.shl_parse_h2h <- function(raw, schema = NULL) {
  body <- .shl_dict(raw)
  rows <- list()
  for (p in .shl_list(body[["players"]])) {
    if (!.shl_is_obj(p)) next
    rows[[length(rows) + 1L]] <- .shl_set(.shl_flatten(p), "effectiveSsgtUuid", body[["effectiveSsgtUuid"]])
  }
  .shl_finish(.shl_frame(rows), schema)
}

# ---------------------------------------------------------------------------
# Runtime: season -> ssgt, transport, answer checks
# ---------------------------------------------------------------------------

# Pluck path -> values: "[]" walks an array, "*" an object's values, "" = the body; nulls skipped
.shl_pluck <- function(body, path) {
  parts <- regmatches(path, gregexpr("\\[\\]|[^.\\[\\]]+", path, perl = TRUE))[[1]]
  cur <- list(body)
  for (part in parts) {
    nxt <- list()
    for (cc in cur) {
      if (part == "[]") {
        if (.shl_is_arr(cc)) nxt <- c(nxt, cc)
      } else if (part == "*") {
        if (.shl_is_obj(cc)) nxt <- c(nxt, unname(cc))
      } else if (.shl_is_obj(cc) && !is.null(cc[[part]])) {
        nxt <- c(nxt, list(cc[[part]]))
      }
    }
    cur <- nxt
  }
  cur
}

# The table key whose pattern matches the URL's path below /api (NULL if none)
.shl_route <- function(url, table) {
  path <- httr2::url_parse(url)$path
  pos <- regexpr("/api", path, fixed = TRUE)
  if (pos > 0) path <- substr(path, pos + 4L, nchar(path))
  path <- sub("/+$", "", path)
  hit <- Filter(function(k) grepl(paste0("^(?:", k, ")$"), path, perl = TRUE), names(table))
  if (length(hit)) hit[[1]] else NULL
}

# Response body as text; "" for a response without a body (resp_body_string() raises on one)
.shl_resp_text <- function(resp) {
  if (!httr2::resp_has_body(resp)) "" else .resp_text(resp)
}

# One polite GET decoded as JSON. An empty 200 from a gameday route is "no data" (games before
# 2023/24); an empty 200 anywhere else is a failed fetch.
.shl_fetch <- function(url, query = list()) {
  query <- Filter(Negate(is.null), query)
  resp <- .build_request(url, query) |> .polite_perform()
  status <- httr2::resp_status(resp)
  if (status == 200L && !nzchar(trimws(.shl_resp_text(resp)))) {
    where <- .euro_where(url)
    if (grepl("/gameday/", url, fixed = TRUE)) {
      .abort_no_data("{where} answered an empty body: gameday data starts in 2023/24")
    }
    .abort_asset_fetch("{where} answered HTTP 200 with an empty body")
  }
  .euro_resp_json(resp, simplifyVector = FALSE)
}

# ({(END season, game type): uuids}, active ssgt) from one layout-info call (cached per session)
.shl_directory <- function() {
  if (!is.null(.shl_state$directory)) return(.shl_state$directory)
  url <- paste0(.SHL_BASE, "/statistics-v2/layout-info")
  raw <- .shl_fetch(url)
  body <- if (.shl_is_arr(raw) && length(raw) && .shl_is_obj(raw[[1]])) raw[[1]] else list()
  out <- list()
  for (s in .shl_list(body[["seasonSeriesGameTypeTeams"]])) {
    if (!.shl_is_obj(s)) next
    code <- if (.shl_truthy(.shl_dict(s[["season"]])[["code"]])) .shl_pystr(s[["season"]][["code"]]) else ""
    gtype <- .shl_dict(s[["gameType"]])[["code"]]
    if (grepl("^[0-9]+$", code) && .shl_truthy(gtype) && .shl_truthy(s[["ssgtUuid"]])) {
      key <- paste(as.integer(code) + 1L, .shl_pystr(gtype), sep = "|")
      out[[key]] <- c(
        ssgt = .shl_pystr(s[["ssgtUuid"]]),
        season = .shl_pystr(.shl_dict(s[["season"]])[["uuid"]] %||% ""),
        series = .shl_pystr(.shl_dict(s[["series"]])[["uuid"]] %||% ""),
        game_type = .shl_pystr(.shl_dict(s[["gameType"]])[["uuid"]] %||% "")
      )
    }
  }
  if (!length(out)) {
    where <- .euro_where(url)
    .abort_asset_fetch("{where}: no ssgt directory in the body")
  }
  active <- .shl_dict(body[["ssgtDefaultValue"]])[["ssgtUuid"]]
  .shl_state$directory <- list(map = out, active = if (.shl_truthy(active)) .shl_pystr(active) else "")
  .shl_state$directory
}

# The uuids of (END-year season, game type); fastRhockey_no_data when layout-info has none
.shl_resolve <- function(season, game_type) {
  gtype <- if (.shl_truthy(game_type)) as.character(game_type) else "regular"
  yr <- suppressWarnings(as.integer(season))
  if (length(yr) != 1L || is.na(yr)) {
    .abort_bad_request("season must be an END year such as 2026; got {.val {season}}")
  }
  dir <- .shl_directory()$map
  hit <- dir[[paste(yr, gtype, sep = "|")]]
  if (is.null(hit)) {
    keys <- strsplit(names(dir), "|", fixed = TRUE)
    seasons <- as.integer(vapply(keys, `[`, "", 1L))
    types <- sort(unique(vapply(keys, `[`, "", 2L)))
    .abort_no_data(paste0(
      "SHL lists no {gtype} ssgt for season {season} (END year); layout-info covers ",
      "{min(seasons)}-{max(seasons)} with game types {.val {types}}"
    ))
  }
  hit
}

# Rewrite the wrappers' `season` / `gameType` into the provider's keys; also return the ssgt
.shl_map_query <- function(route, params) {
  q <- params
  if (identical(route, "/statistics-v2/athlete/playerProfile_seasonGameLog")) {
    if (!is.null(q[["season"]])) q[["season"]] <- as.integer(q[["season"]]) - 1L # START year
    return(list(query = q, ssgt = NULL))
  }
  if (!route %in% .SHL_SEASON_ROUTES) return(list(query = q, ssgt = NULL))
  season <- q[["season"]]
  gtype <- q[["gameType"]]
  q[["season"]] <- NULL
  q[["gameType"]] <- NULL
  if (route == .SHL_STATS_ROUTE) {
    if (!is.null(season)) q[["ssgtUuid"]] <- .shl_resolve(season, gtype)[["ssgt"]]
    q[["count"]] <- .SHL_STATS_COUNT
    return(list(query = q, ssgt = q[["ssgtUuid"]]))
  }
  if (route == "/sports-v2/game-schedule") {
    if (!is.null(season)) {
      ids <- .shl_resolve(season, gtype)
      q[["seasonUuid"]] <- ids[["season"]]
      q[["seriesUuid"]] <- ids[["series"]]
      q[["gameTypeUuid"]] <- ids[["game_type"]]
      return(list(query = q, ssgt = ids[["ssgt"]]))
    }
    return(list(query = q, ssgt = NULL))
  }
  if (!is.null(season)) {
    q[["seasonSeriesGameType"]] <- .shl_resolve(season, gtype)[["ssgt"]]
  } else if (!.shl_truthy(q[["seasonSeriesGameType"]])) {
    q[["seasonSeriesGameType"]] <- .shl_directory()$active # the active ssgt
  }
  list(query = q, ssgt = q[["seasonSeriesGameType"]])
}

# Refuse an unusable answer: error envelope, empty body, no rows, a cut stats page, or
# a body for another game / team / athlete / ssgt (or one that cannot show its scope)
.shl_check <- function(url, route, raw, query, ssgt) {
  where <- .euro_where(url)
  if (.shl_is_obj(raw) && length(raw) && all(names(raw) %in% .SHL_ERROR_KEYS)) {
    keys <- sort(names(raw))
    .abort_asset_fetch("{where} answered an error envelope {.val {keys}}")
  }
  if (is.null(raw) || (is.list(raw) && !length(raw)) || identical(raw, "")) {
    .abort_no_data("{where} answered an empty body")
  }
  if (is.null(route)) return(invisible(raw))
  rows <- .shl_pluck(raw, .SHL_ROWS[[route]])
  if (!any(vapply(rows, function(r) .shl_is_obj(r) && length(r) > 0, TRUE))) {
    path <- .SHL_ROWS[[route]]
    .abort_no_data("{where} answered no rows at {.val {path}}")
  }
  if (route == .SHL_STATS_ROUTE) {
    n <- length(rows)
    cnt <- .SHL_STATS_COUNT
    if (n >= cnt) {
      .abort_asset_fetch("{where} answered {n} rows = count={cnt}: the table may be cut")
    }
    total <- .shl_pluck(raw, "[].totalCount")
    total <- if (length(total)) total[[1]] else NULL
    if (.shl_is_num(total) && n < total) {
      .abort_asset_fetch("{where} answered {n} of totalCount={total} rows: the table was cut")
    }
  }
  scope <- .SHL_SCOPE[[route]]
  if (is.null(scope)) return(invisible(raw))
  field <- scope[1]
  source <- scope[2]
  expected <- if (source == "path") {
    sub("^.*/", "", sub("/+$", "", httr2::url_parse(url)$path))
  } else if (source == "ssgt") {
    ssgt
  } else {
    query[[source]]
  }
  if (!.shl_truthy(expected)) return(invisible(raw))
  values <- .shl_pluck(raw, field)
  if (!length(values) ||
      (grepl("[]", field, fixed = TRUE) &&
       length(values) != length(.shl_pluck(raw, sub("\\.[^.]*$", "", field))))) {
    .abort_asset_fetch("{where}: the body lacks {.val {field}}, so {.val {expected}} cannot be verified")
  }
  wrong <- Filter(function(v) !(is.character(v) && length(v) == 1L && v == expected), values)
  if (length(wrong)) {
    got <- .shl_pystr(wrong[[1]])
    .abort_asset_fetch("{where} answered {field}='{got}' for '{expected}': not the data asked for")
  }
  invisible(raw)
}

# GET an SHL route: map the season, fetch politely, then refuse an unusable answer.
# `params`: query parameters; `season` (END year) and `gameType` are mapped, NULLs dropped.
.shl_get <- function(url, params = list()) {
  if (grepl(.SHL_TEXT_ROUTE, httr2::url_parse(url)$path)) {
    resp <- .build_request(url, list()) |> .polite_perform()
    .euro_check_status(resp)
    status <- httr2::resp_status(resp)
    text <- trimws(.shl_resp_text(resp))
    if (!nzchar(text) && !status %in% c(204L, 205L)) {
      where <- .euro_where(url)
      .abort_asset_fetch("{where} answered HTTP {status} with an empty body")
    }
    if (!grepl("^[a-z_]{1,40}$", text)) {
      where <- .euro_where(url)
      head <- substr(text, 1L, 40L)
      .abort_asset_fetch("{where} answered '{head}', not a provider name")
    }
    return(text)
  }
  route <- .shl_route(url, .SHL_ROWS)
  mapped <- .shl_map_query(route %||% "", Filter(Negate(is.null), params))
  raw <- .shl_fetch(url, mapped$query)
  .shl_check(url, route, raw, mapped$query, mapped$ssgt)
  raw
}

.shl_check_choice <- function(arg, value, choices) {
  if (!(is.character(value) && length(value) == 1L && value %in% choices)) {
    .abort_bad_request("{.arg {arg}} must be one of {.val {choices}}; got {.val {value}}")
  }
  invisible(value)
}

# ---------------------------------------------------------------------------
# Documented columns per endpoint (sdv-py shl_schemas.SCHEMAS): an empty payload parses to a
# zero-row tibble with these columns and types. str=character, int=integer, float=double,
# bool=logical, list[int]=list of integer vectors.
# ---------------------------------------------------------------------------

.SHL_SCHEMAS <- list(
  athlete_career = c(
    gp = "float", gpi = "float", ga = "float", gaa = "float", svs = "float", svs_perc = "float",
    w = "float", t = "float", l = "float", so = "float", info_uuid = "str", info_full_name =
    "str", info_first_name = "str", info_last_name = "str", info_birth_date = "str",
    info_nationality = "str", info_gender = "str", info_age_value = "int", info_age_format =
    "str", info_weight_value = "int", info_weight_format = "str", info_height_value = "int",
    info_height_format = "str", info_jersey_number = "str", info_position = "str",
    info_position_code = "str", info_shoots = "str", info_team_uuid = "str",
    info_team_instance_id = "str", info_team_name = "str", info_team_code = "str",
    info_season_stats = "str", info_career_stats = "str", info_statistics_provider = "str",
    info_is_in_squad = "bool", info_player_id = "int", info_ssgt_uuid = "str", info_team_id =
    "str"
  ),
  athlete_details = c(
    athlete_data_uuid = "str", athlete_data_contact_info = "str", athlete_data_created_at =
    "str", athlete_data_created_by = "str", athlete_data_creator_site = "str",
    athlete_data_height = "int", athlete_data_date_of_birth = "str", athlete_data_deleted =
    "bool", athlete_data_first_name = "str", athlete_data_gender = "str",
    athlete_data_last_modified = "str", athlete_data_last_name = "str", athlete_data_metadata =
    "str", athlete_data_nationality = "str", athlete_data_shoots = "str", athlete_data_weight =
    "int", athlete_data_player_ext_ids = "str"
  ),
  athlete_game_log = c(
    date = "str", series = "str", home_team = "str", home_team_icon = "str", guest_team = "str",
    guest_team_icon = "str", gp = "float", gpi = "float", gs = "float", w = "float", t =
    "float", l = "float", so = "float", ga = "float", gaa = "float", soga = "float", svs =
    "float", svs_perc = "float", info_full_name = "str", info_first_name = "str", info_last_name
    = "str", info_birth_date = "str", info_nationality = "str", info_gender = "str",
    info_jersey_number = "str", info_position = "str", info_position_code = "str", info_shoots =
    "str", info_team_name = "str", info_season_stats = "str", info_career_stats = "str",
    info_statistics_provider = "str", info_is_in_squad = "bool", info_player_id = "int",
    info_ssgt_uuid = "str"
  ),
  athlete_profile = c(
    uuid = "str", full_name = "str", first_name = "str", last_name = "str", birth_date = "str",
    nationality = "str", gender = "str", age_value = "int", age_format = "str", weight_value =
    "int", weight_format = "str", height_value = "int", height_format = "str", jersey_number =
    "int", position = "str", position_code = "str", shoots = "str", team_uuid = "str",
    team_instance_id = "str", team_name = "str", team_code = "str", team_media = "str",
    season_stats = "str", career_stats = "str", is_in_squad = "bool", media_url = "str",
    media_alt = "str", media_srcset = "str", media_url_img_original_proportion = "str",
    media_url_placeholder_image = "str", media_videourl = "str", media_photographer = "str",
    media_focal_point = "str"
  ),
  athlete_seasons = c(
    value = "str", name = "str", label = "str"
  ),
  boxscore = c(
    team_side = "str", position_group = "str", player_id = "int", team_id = "str", period =
    "int", full_name = "str", first_name = "str", last_name = "str", nr = "int", pos = "str",
    line = "int", g = "float", a = "float", ppg = "float", sw = "float", pim = "float", sog =
    "float", ppsog = "float", plus_minus = "float", toi = "str", hits = "float", fow = "float",
    fol = "float", fo_perc = "float", ga = "float", soga = "float", spga = "float", svs =
    "float", svs_perc = "float"
  ),
  game_header = c(
    instance_context_site_team_is_in_game = "bool", instance_context_site_team_is_home_team =
    "bool", instance_context_site_team_is_away_team = "bool", game_info_game_uuid = "str",
    game_info_ext_id = "int", game_info_start_date_time = "str", game_info_arena_name = "str",
    game_info_state = "str", game_info_overtime = "bool", game_info_shootout = "bool",
    game_info_series_code = "str", game_info_series_name = "str", game_info_round_number =
    "int", game_info_round_label = "str", game_info_series_display_name = "str",
    home_team_names_code = "str", home_team_names_short = "str", home_team_names_long = "str",
    home_team_names_full = "str", home_team_names_code_site = "str", home_team_names_short_site
    = "str", home_team_names_full_site = "str", home_team_names_long_site = "str",
    home_team_uuid = "str", home_team_instance_id = "str", home_team_founded_on = "str",
    home_team_address = "str", home_team_email = "str", home_team_icon = "str", home_team_score
    = "int", away_team_names_code = "str", away_team_names_short = "str", away_team_names_long =
    "str", away_team_names_full = "str", away_team_names_code_site = "str",
    away_team_names_short_site = "str", away_team_names_full_site = "str",
    away_team_names_long_site = "str", away_team_uuid = "str", away_team_instance_id = "str",
    away_team_founded_on = "str", away_team_address = "str", away_team_email = "str",
    away_team_icon = "str", away_team_score = "int", ssgt_uuid = "str", series_uuid = "str"
  ),
  game_overview = c(
    game_uuid = "str", home_team_game_source_id = "str", home_team_game_id = "int",
    home_team_place = "str", home_team_score = "int", home_team_team_id = "str",
    home_team_team_name = "str", home_team_team_code = "str", home_team_game_uuid = "str",
    away_team_game_source_id = "str", away_team_game_id = "int", away_team_place = "str",
    away_team_score = "int", away_team_team_id = "str", away_team_team_name = "str",
    away_team_team_code = "str", away_team_game_uuid = "str", home_goals = "int", away_goals =
    "int", state = "str", time_period = "int", time_period_time = "str"
  ),
  game_summary = c(
    team_head2_head = "str", team_period_caption = "str", team_period_overtime = "bool",
    team_period_shootout = "bool", team_period_stats = "str", best_scorers = "str",
    statistics_provider = "str"
  ),
  last_game_day = c(
    game_uuid = "str", start_date_time = "str", state = "str", overtime = "bool", shootout =
    "bool", ssgt_uuid = "str", home_team_status = "str", home_team_uuid = "str",
    home_team_owner_instance_id = "str", home_team_code = "str", home_team_names_code = "str",
    home_team_names_short = "str", home_team_names_long = "str", home_team_names_full = "str",
    home_team_names_code_site = "str", home_team_names_short_site = "str",
    home_team_names_full_site = "str", home_team_names_long_site = "str", home_team_score =
    "int", home_team_icon = "str", away_team_status = "str", away_team_uuid = "str",
    away_team_owner_instance_id = "str", away_team_code = "str", away_team_names_code = "str",
    away_team_names_short = "str", away_team_names_long = "str", away_team_names_full = "str",
    away_team_names_code_site = "str", away_team_names_short_site = "str",
    away_team_names_full_site = "str", away_team_names_long_site = "str", away_team_score =
    "int", away_team_icon = "str", venue_uuid = "str", venue_name = "str", series_uuid = "str",
    series_code = "str", series_display_name = "str"
  ),
  layout_info = c(
    season = "int", season_code = "str", season_name = "str", game_type = "str", game_type_name
    = "str", series_code = "str", ssgt_uuid = "str", state = "str", season_uuid = "str",
    series_uuid = "str", game_type_uuid = "str"
  ),
  next_game_day = c(
    game_uuid = "str", start_date_time = "str", state = "str", overtime = "bool", shootout =
    "bool", ssgt_uuid = "str", home_team_status = "str", home_team_uuid = "str",
    home_team_owner_instance_id = "str", home_team_code = "str", home_team_names_code = "str",
    home_team_names_short = "str", home_team_names_long = "str", home_team_names_full = "str",
    home_team_names_code_site = "str", home_team_names_short_site = "str",
    home_team_names_full_site = "str", home_team_names_long_site = "str", home_team_score =
    "int", home_team_icon = "str", away_team_status = "str", away_team_uuid = "str",
    away_team_owner_instance_id = "str", away_team_code = "str", away_team_names_code = "str",
    away_team_names_short = "str", away_team_names_long = "str", away_team_names_full = "str",
    away_team_score = "int", away_team_icon = "str", venue_uuid = "str", venue_name = "str",
    series_uuid = "str", series_code = "str", series_display_name = "str",
    away_team_names_code_site = "str", away_team_names_short_site = "str",
    away_team_names_full_site = "str", away_team_names_long_site = "str"
  ),
  pbp = c(
    game_source_id = "str", game_id = "int", event_id = "int", event_uuid = "str", round =
    "int", game_type = "str", arena = "str", attendance = "int", start_date_and_time = "str",
    period = "int", time = "str", game_state = "str", revision = "int", type = "str",
    real_world_time = "str", updated_time = "str", home_team_id = "str", home_team_name = "str",
    home_team_code = "str", home_score = "int", away_team_id = "str", away_team_name = "str",
    away_team_code = "str", away_score = "int", event_team_id = "str", event_place = "str",
    event_team_code = "str", event_team_name = "str", player_id = "int", player_first_name =
    "str", player_family_name = "str", player_jersey_today = "str", is_entering = "bool",
    game_uuid = "str", location_x = "int", location_y = "int", goal_section = "int",
    is_penalty_shot = "bool", variant_short_name = "str", variant_minor_time = "str",
    variant_double_minor_time = "str", variant_bench_time = "str", variant_major_time = "str",
    variant_misconduct_time = "str", variant_g_m_time = "str", variant_m_p_time = "str",
    variant_description = "str", offence = "str", did_render_in_penalty_shot = "bool",
    home_goals = "int", away_goals = "int", is_empty_net_goal = "bool", pop_player_ids =
    "list[int]", nep_player_ids = "list[int]", assist1_player_id = "int", assist1_first_name =
    "str", assist1_family_name = "str", assist1_jersey_today = "str", assist2_player_id = "int",
    assist2_first_name = "str", assist2_family_name = "str", assist2_jersey_today = "str",
    goal_status = "str", started = "bool", started_at = "str", finished = "bool", finished_at =
    "str"
  ),
  player_box = c(
    team_side = "str", position_group = "str", player_id = "int", team_id = "str", period =
    "int", full_name = "str", first_name = "str", last_name = "str", nr = "int", pos = "str",
    line = "int", g = "float", a = "float", ppg = "float", sw = "float", pim = "float", sog =
    "float", ppsog = "float", plus_minus = "float", toi = "str", hits = "float", fow = "float",
    fol = "float", fo_perc = "float", ga = "float", soga = "float", spga = "float", svs =
    "float", svs_perc = "float", tp = "float", shg = "float", nep = "float", pop = "float",
    fo_tot = "float", num_pen = "float", ms = "float", bk_s = "float"
  ),
  player_h2h = c(
    uuid = "str", first_name = "str", last_name = "str", full_name = "str", birth_date = "str",
    nationality = "str", gender = "str", jersey_number = "str", position = "str", position_code
    = "str", age_value = "int", age_format = "str", height_value = "int", height_format = "str",
    weight_value = "int", weight_format = "str", team_uuid = "str", team_instance_id = "str",
    team_name = "str", team_code = "str", team_media = "str", media_url = "str", media_alt =
    "str", media_srcset = "str", season_stats = "str", career_stats = "str", match_stats_nr =
    "int", match_stats_pos = "str", match_stats_line = "int", match_stats_g = "int",
    match_stats_a = "int", match_stats_tp = "int", match_stats_shg = "int", match_stats_ppg =
    "int", match_stats_sw = "int", match_stats_pim = "int", match_stats_sog = "int",
    match_stats_ppsog = "int", match_stats_nep = "int", match_stats_pop = "int", match_stats_toi
    = "str", match_stats_hits = "int", match_stats_fo_tot = "int", match_stats_fow = "int",
    match_stats_fol = "int", match_stats_num_pen = "int", match_stats_fo_perc = "float",
    match_stats_ms = "int", match_stats_bk_s = "int", match_stats_plus_minus = "int",
    season_avg_stats = "str", statistics_provider = "str", is_in_squad = "bool",
    effective_ssgt_uuid = "str"
  ),
  post_game_team_stats = c(
    home_team_side_translate_key = "str", home_team_left_type = "str", home_team_left_value =
    "int", home_team_right_type = "str", home_team_right_value = "int", home_team_center_type =
    "str", home_team_center_value = "str", away_team_side_translate_key = "str",
    away_team_left_type = "str", away_team_left_value = "int", away_team_right_type = "str",
    away_team_right_value = "int", away_team_center_type = "str", away_team_center_value = "str"
  ),
  roster = c(
    position = "str", position_code = "str", player_type = "str", gender = "str", uuid = "str",
    first_name = "str", last_name = "str", full_name = "str", nationality = "str", jersey_number
    = "int", portrait_url = "str"
  ),
  schedule = c(
    game_uuid = "str", raw_start_date_time = "str", start_date_time = "str", state = "str",
    overtime = "bool", shootout = "bool", ssgt_uuid = "str", home_team_status = "str",
    home_team_uuid = "str", home_team_owner_instance_id = "str", home_team_code = "str",
    home_team_names_code = "str", home_team_names_short = "str", home_team_names_long = "str",
    home_team_names_full = "str", home_team_names_code_site = "str", home_team_names_short_site
    = "str", home_team_names_full_site = "str", home_team_names_long_site = "str",
    home_team_score = "int", home_team_icon = "str", away_team_status = "str", away_team_uuid =
    "str", away_team_owner_instance_id = "str", away_team_code = "str", away_team_names_code =
    "str", away_team_names_short = "str", away_team_names_long = "str", away_team_names_full =
    "str", away_team_names_code_site = "str", away_team_names_short_site = "str",
    away_team_names_full_site = "str", away_team_names_long_site = "str", away_team_score =
    "int", away_team_icon = "str", venue_uuid = "str", venue_name = "str", series_uuid = "str",
    series_code = "str", series_display_name = "str", round_number = "int", round_label = "str"
  ),
  season_filter = c(
    kind = "str", code = "str", name = "str", uuid = "str", deleted = "bool"
  ),
  sites = c(
    id = "str", instance_id = "str", public_url = "str", name = "str", icon = "str"
  ),
  ssgt_provider = c(
    provider = "str"
  ),
  standings = c(
    season = "int", ssgt_uuid = "str", team_code = "str", team_uuid = "str", team_name = "str",
    team_short_name = "str", rank = "int", rank_diff = "int", gp = "int", w = "int", t = "int",
    l = "int", otw = "int", otl = "int", p = "int", gf = "int", ga = "int", diff = "int"
  ),
  stats_info = c(
    rank = "int", gp = "float", w = "float", l = "float", leading = "float", trailing = "float",
    g = "float", ga = "float", gfg = "float", otw = "float", otl = "float", sow = "float", sol =
    "float", p_gp_format = "float", gpg = "float", gapg = "float", points = "float", info_type =
    "str", info_id = "str", info_uuid = "str", info_created_at = "str", info_created_by = "str",
    info_deleted = "bool", info_gender = "str", info_graphics_library = "str",
    info_last_modified = "str", info_nationality = "str", info_owner_instance_id = "str",
    info_ownersite = "str", info_team_code = "str", info_series = "str", info_sport = "str",
    info_team_media = "str", info_team_names_code = "str", info_team_names_short = "str",
    info_team_names_long = "str", info_team_names_full = "str", info_team_names_code_site =
    "str", info_team_names_short_site = "str", info_team_names_full_site = "str",
    info_team_names_long_site = "str", info_club_name_code = "str", info_club_name_short =
    "str", info_club_name_long = "str", info_club_name_full = "str", info_team_info_founded =
    "str", info_team_info_golds = "str", info_team_info_finals = "str", info_team_info_address =
    "str", info_team_info_phone = "str", info_team_info_fax = "str", info_team_info_email =
    "str", info_team_info_chairman = "str", info_team_info_manager = "str",
    info_team_info_accreditation = "str", info_team_info_facebook = "str",
    info_team_info_instagram = "str", info_team_info_twitter = "str", info_team_ext_ids = "str",
    info_ext_ids = "str", info_logo = "str", info_social_media = "str", info_display_name =
    "str", info_code = "str", info_site_display_name = "str", info_team_info_retired_numbers =
    "str", info_team_info_contact = "str", info_club_name = "str", player_id = "int", svs =
    "float", svs_perc = "float", gpi = "float", gaa = "float", t = "float", so = "float", mip =
    "str", valid_ranking = "bool", info_full_name = "str", info_first_name = "str",
    info_last_name = "str", info_birth_date = "str", info_number = "int", info_position = "str",
    info_shoots = "str", info_weight_value = "int", info_weight_format = "str",
    info_height_value = "int", info_height_format = "str", info_player_media_id = "int",
    info_player_media_media_string = "str", info_player_media_type = "str",
    info_player_media_sort_order = "int", info_team_uuid = "str", info_team_name = "str",
    info_team_owner_instance_id = "str", info_team_site_display_name = "str", info_team_id =
    "str", tp = "float", a = "float", pim = "float", gwg = "float", ppg = "float", sog =
    "float", hits = "float", bk_s = "float", plus = "float", minus = "float", plus_minus =
    "float", toi_gp = "str", tppg = "float", a_firsts = "float", a_seconds = "float", ppa =
    "float", pptp = "float", eqg = "float", eqa = "float", eqtp = "float", shg = "float", sha =
    "float", shtp = "float", trailing2 = "float", trailing1 = "float", tiedg = "float", leading1
    = "float", leading2 = "float", eng = "float", toi = "str", pptoi_gp = "str", shtoi_gp =
    "str", pptoi_perc = "float", shtoi_perc = "float", pptoi_pp_plus = "str", ppsog = "float",
    ppsw = "float", ppsg_perc = "float", pp_plus = "float", pp_minus = "float", pp_plus_minus =
    "float", shsog = "float", shsw = "float", sh_plus = "float", sh_minus = "float",
    sh_plus_minus = "float", shtoi_sh_minus = "str", esg = "float", esa = "float", estp =
    "float", essog = "float", essw = "float", es_plus = "float", es_minus = "float",
    es_plus_minus = "float", estoi_gp = "str", eff_pim = "float", pers_pim = "float", off_pim =
    "float", minor = "float", major = "float", misc = "float", gm = "float", mp = "float",
    fo_perc = "float", fo = "float", fow = "float", fol = "float", ofo_perc = "float", ofo =
    "float", ofow = "float", ofol = "float", dfo_perc = "float", dfo = "float", dfow = "float",
    dfol = "float", fo_violation_perc = "float", fo_violations = "float", sg_perc = "float", ms
    = "float", sog_perc = "float", ps = "float", psg = "float", ps_perc = "float", so_ps =
    "float", so_psg = "float", so_ps_perc = "float", cf = "float", ca = "float", cf_perc =
    "float", close_cf_perc = "float", ff = "float", fa = "float", ff_perc = "float",
    close_ff_perc = "float", on_ice_goal_perc = "float", on_ice_svs_perc = "float", pdo =
    "float", info_player_media = "str", shga = "float", ppga = "float", enga = "float", pp_opp =
    "float", pp_perc = "float", pp_time = "str", st_perc = "float", pk_opp = "float", pk_perc =
    "float", ppsoga = "float", pk_time = "str", gp_home = "float", att_avg_home = "float",
    att_total_home = "float", att_avg_overall = "float", att_total_overall = "float", gp_road =
    "float", att_avg_road = "float", att_total_road = "float", team_pen = "float", s_eff =
    "float", pps_eff = "float", sw = "float", sib = "float", ss = "float", soga = "float", msa =
    "float", swa = "float", sfa = "float", ssa = "float", fo_violations_perc = "float", psa =
    "float", pssvs = "float", so_psa = "float", so_pssvs = "float", so_pssvs_perc = "float",
    freezes = "float", freezes_against = "float", icings = "float", icing_against = "float",
    offsides = "float", offsides_against = "float", ssga = "float", ssoga = "float", sssvs_perc
    = "float", ppsvs_perc = "float", shsoga = "float", shsvs_perc = "float", esga = "float",
    essoga = "float", essvs_perc = "float", psga = "float", pssvs_perc = "float", so_psga =
    "float", date_time = "str", home_team = "str", guest_team = "str", game_score_home_score =
    "int", game_score_away_score = "int", game_score_decided_in = "str", venue = "str",
    attendance = "float", info_game_id = "int", info_home_team_id = "str", info_away_team_id =
    "str", diff = "float"
  ),
  team = c(
    names_code = "str", names_short = "str", names_long = "str", names_full = "str",
    names_code_site = "str", names_short_site = "str", names_full_site = "str", names_long_site
    = "str", info_founded = "str", info_golds = "str", info_finals = "str", info_retired_numbers
    = "str", info_address = "str", info_phone = "str", info_fax = "str", info_email = "str",
    info_chairman = "str", info_manager = "str", info_accreditation = "str", info_facebook =
    "str", info_instagram = "str", info_twitter = "str", contact = "str", uuid = "str",
    nationality = "str", arena_name = "str", public_url = "str", icon = "str", owner_instance_id
    = "str", site_display_name = "str"
  ),
  team_played_games = c(
    date = "str", time = "str", game_uuid = "str", start_date_time = "str", state = "str",
    overtime = "bool", shootout = "bool", ssgt_uuid = "str", home_team_status = "str",
    home_team_uuid = "str", home_team_owner_instance_id = "str", home_team_code = "str",
    home_team_names_code = "str", home_team_names_short = "str", home_team_names_long = "str",
    home_team_names_full = "str", home_team_names_code_site = "str", home_team_names_short_site
    = "str", home_team_names_full_site = "str", home_team_names_long_site = "str",
    home_team_score = "int", home_team_icon = "str", away_team_status = "str", away_team_uuid =
    "str", away_team_owner_instance_id = "str", away_team_code = "str", away_team_names_code =
    "str", away_team_names_short = "str", away_team_names_long = "str", away_team_names_full =
    "str", away_team_names_code_site = "str", away_team_names_short_site = "str",
    away_team_names_full_site = "str", away_team_names_long_site = "str", away_team_score =
    "int", away_team_icon = "str", venue_uuid = "str", venue_name = "str", series_uuid = "str",
    series_code = "str", series_display_name = "str"
  ),
  team_stats = c(
    team_side = "str", game_source_id = "str", game_id = "int", place = "str", score = "int",
    team_id = "str", team_name = "str", team_code = "str", game_uuid = "str", period = "int", g
    = "float", pim = "float", fow = "float"
  ),
  team_upcoming_games = c(
    date = "str", time = "str", game_uuid = "str", start_date_time = "str", state = "str",
    overtime = "bool", shootout = "bool", ssgt_uuid = "str", home_team_status = "str",
    home_team_uuid = "str", home_team_owner_instance_id = "str", home_team_code = "str",
    home_team_names_code = "str", home_team_names_short = "str", home_team_names_long = "str",
    home_team_names_full = "str", home_team_names_code_site = "str", home_team_names_short_site
    = "str", home_team_names_full_site = "str", home_team_names_long_site = "str",
    home_team_score = "int", home_team_icon = "str", away_team_status = "str", away_team_uuid =
    "str", away_team_owner_instance_id = "str", away_team_code = "str", away_team_names_code =
    "str", away_team_names_short = "str", away_team_names_long = "str", away_team_names_full =
    "str", away_team_names_code_site = "str", away_team_names_short_site = "str",
    away_team_names_full_site = "str", away_team_names_long_site = "str", away_team_score =
    "int", away_team_icon = "str", venue_uuid = "str", venue_name = "str", series_uuid = "str",
    series_code = "str", series_display_name = "str"
  )
)

.SHL_MODULES <- c(
  "players_summary", "players_point", "players_strength", "players_goal", "players_timeOnIce",
  "players_powerplay", "players_penaltykilling", "players_equalStrength", "players_penalty",
  "players_plusMinus", "players_faceoff", "players_shot", "players_defensive",
  "players_penaltyShot", "players_extendedMetrics", "teams_result", "teams_goal",
  "teams_powerplay", "teams_penaltyKilling", "teams_attendance", "teams_penalty",
  "teams_shotsForward", "teams_shotsAgainst", "teams_faceoff", "teams_penaltyShot",
  "teams_extendedMetrics", "teams_stoppage", "goalkeepers_summary", "goalkeepers_shotAgainst",
  "goalkeepers_result", "goalkeepers_strength", "goalkeepers_penaltyShot", "games_games",
  "standings_standings"
)

#' Player career totals by season
#'
#' Wraps `GET https://www.shl.se/api/statistics-v2/athlete/playerProfile_gameLog` of the SHL site
#'   API (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and
#'   are mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param athlete_uuid Required. Athlete uuid.
#' @param master_site_instance_id Site instance (shl1_shl). No effect in the drop-one probe.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | gp | numeric | Number of games played. |
#' | gpi | numeric | Games played with ice time. |
#' | ga | numeric | Number of goals conceded (goals against). |
#' | gaa | numeric | Goals against average (per 60 minutes). Numeric, but sent as a string in some rows and as a number in others. |
#' | svs | numeric | Number of saves made by the goalie. |
#' | svs_perc | numeric | Save %. Numeric, but sent as a string in some rows and as a number in others. |
#' | w | numeric | Goalie wins: games the goalie was credited with the win, overtime and shootout wins included. |
#' | t | numeric | Goalie ties: games that ended level with the goalie credited a tie; 0 in every sampled row, as overtime and shootouts now decide every game. |
#' | l | numeric | Goalie losses: games the goalie was charged with the loss, overtime and shootout losses included. |
#' | so | numeric | Number of shutouts (games without a goal against). |
#' | info_uuid | character | Athlete uuid (platform key space) in player and goalie modules, team uuid in team modules; joins to the athlete routes or `/sports-v2/teams/{team_uuid}`. |
#' | info_full_name | character | Given name and surname. |
#' | info_first_name | character | Given (first) name of the player. |
#' | info_last_name | character | Surname (family name) of the player. |
#' | info_birth_date | character | Date of birth (YYYY-MM-DD). |
#' | info_nationality | character | Nationality (ISO country code). |
#' | info_gender | character | Sex of the athlete or team (male / female). |
#' | info_age_value | integer | Age of the player in whole years. |
#' | info_age_format | character | Unit of value: years. |
#' | info_weight_value | integer | Weight of the player in kilograms. |
#' | info_weight_format | character | Unit of value: kg. |
#' | info_height_value | integer | Height of the player in centimetres. |
#' | info_height_format | character | Unit of value: cm. |
#' | info_jersey_number | character | Jersey (shirt) number of the player. |
#' | info_position | character | Position code (CE = centre, LW, RW, D, GK, F); N/A in the athlete game logs. |
#' | info_position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
#' | info_shoots | character | Shooting side (L / R; Left / Right). |
#' | info_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | info_team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
#' | info_team_name | character | Team name (e.g. Frölunda HC); empty in some rows. |
#' | info_team_code | character | Team code of the player's team (e.g. FHC). |
#' | info_season_stats | character | Current-season stat rows of the player (empty in the sample). |
#' | info_career_stats | character | Career stat rows of the player (empty in the sample). |
#' | info_statistics_provider | character | Stats provider of the data (statnet seen). |
#' | info_is_in_squad | logical | Whether the player is in the current squad. |
#' | info_player_id | integer | isa (StatNet) player id as an INTEGER. Join trap: play-by-play sends the same id as a STRING (player.playerId, pop/nep, assists) and map keys are strings; cast before joining. |
#' | info_ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | info_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_athlete_career(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl")
#' }
#' @export
#' @family SHL Functions
shl_athlete_career <- function(athlete_uuid, master_site_instance_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/athlete/playerProfile_gameLog"),
    list(playerUuid = athlete_uuid, masterSiteInstanceId = master_site_instance_id)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_stats_info(raw, .SHL_SCHEMAS[["athlete_career"]])
  make_fastRhockey_data(out, "SHL Athlete Career Information from www.shl.se", Sys.time())
}

#' Player bio plus playerExtIds (isa, ramses_legacy, ...)
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/athlete-details/{athlete_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param athlete_uuid Athlete uuid (stats-info stats[].info.uuid, roster rows, or athlete-details).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | athlete_data_uuid | character | Athlete uuid (platform key space; qQ9-... or 10-char form); joins to the athlete routes and stats-info stats[].info.uuid. Not the isa or ramses_legacy id. |
#' | athlete_data_contact_info | character | Contact info (null in captures). |
#' | athlete_data_created_at | character | Record creation time (ISO 8601; often null). |
#' | athlete_data_created_by | character | Record creator (site instance id; often null). |
#' | athlete_data_creator_site | character | Site that created the record (null in captures). |
#' | athlete_data_height | integer | Height of the player in centimetres. |
#' | athlete_data_date_of_birth | character | Date of birth (YYYY-MM-DD). |
#' | athlete_data_deleted | logical | Soft-delete flag of the record (false in every sample). |
#' | athlete_data_first_name | character | Given (first) name of the player. |
#' | athlete_data_gender | character | Sex of the athlete or team (male / female). |
#' | athlete_data_last_modified | character | Record last-modified time (ISO 8601). |
#' | athlete_data_last_name | character | Surname (family name) of the player. |
#' | athlete_data_metadata | character | Free-form metadata. |
#' | athlete_data_nationality | character | Nationality (ISO country code). |
#' | athlete_data_shoots | character | Shooting side (L / R; Left / Right). |
#' | athlete_data_weight | integer | Weight of the player in kilograms. |
#' | athlete_data_player_ext_ids | character | External ids of the athlete (isa, ramses_legacy, ramses_generic_legacy, ramses_merged). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_athlete_details(athlete_uuid = "qQ9-349c7WDv6")
#' }
#' @export
#' @family SHL Functions
shl_athlete_details <- function(athlete_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/athlete-details/", athlete_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_page(raw, .SHL_SCHEMAS[["athlete_details"]])
  make_fastRhockey_data(out, "SHL Athlete Details Information from www.shl.se", Sys.time())
}

#' Player game log of one season
#'
#' Wraps `GET https://www.shl.se/api/statistics-v2/athlete/playerProfile_seasonGameLog` of the SHL
#'   site API (www.shl.se). Unofficial use of a public source; seasons are END years (2026 =
#'   2025/26) and are mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param athlete_uuid Required. Athlete uuid.
#' @param master_site_instance_id Site instance (shl1_shl). No effect in the drop-one probe.
#' @param season Season END year (2026 = 2025/26); the runtime sends the START year this route
#'   takes. None = the player's default (shorter) log.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | date | character | Date of the game (YYYY-MM-DD). |
#' | series | character | Series id in the career log. |
#' | home_team | character | Home team SHORT NAME (e.g. Frölunda; may carry a trailing space), not a code. |
#' | home_team_icon | character | Home team logo URL. |
#' | guest_team | character | Away team SHORT NAME (e.g. Luleå), not a code. |
#' | guest_team_icon | character | Away team logo URL. |
#' | gp | numeric | Number of games played. |
#' | gpi | numeric | Games played with ice time. |
#' | gs | numeric | Games started (goalie log). |
#' | w | numeric | 1 when the goalie was credited with the win in this game (overtime and shootout included), else 0. |
#' | t | numeric | 1 when the goalie was credited a tie in this game, else 0; 0 in every sampled row, as overtime and shootouts now decide every game. |
#' | l | numeric | 1 when the goalie was charged with the loss in this game (overtime and shootout included), else 0. |
#' | so | numeric | Number of shutouts (games without a goal against). |
#' | ga | numeric | Number of goals conceded (goals against). |
#' | gaa | numeric | Goals against average (per 60 minutes). Numeric, but sent as a string in some rows and as a number in others. |
#' | soga | numeric | Shots on goal against. |
#' | svs | numeric | Number of saves made by the goalie. |
#' | svs_perc | numeric | Save %. Numeric, but sent as a string in some rows and as a number in others. |
#' | info_full_name | character | Given name and surname. |
#' | info_first_name | character | Given (first) name of the player. |
#' | info_last_name | character | Surname (family name) of the player. |
#' | info_birth_date | character | Date of birth (YYYY-MM-DD). |
#' | info_nationality | character | Nationality (ISO country code). |
#' | info_gender | character | Sex of the athlete or team (male / female). |
#' | info_jersey_number | character | Jersey (shirt) number of the player. |
#' | info_position | character | Position code (CE = centre, LW, RW, D, GK, F); N/A in the athlete game logs. |
#' | info_position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
#' | info_shoots | character | Shooting side (L / R; Left / Right). '-' placeholder in every sample. |
#' | info_team_name | character | Team name (e.g. Frölunda HC); empty in some rows. Empty string in every sample (placeholder). |
#' | info_season_stats | character | Current-season stat rows of the player (empty in the sample). |
#' | info_career_stats | character | Career stat rows of the player (empty in the sample). |
#' | info_statistics_provider | character | Stats provider of the data (statnet seen). |
#' | info_is_in_squad | logical | Whether the player is in the current squad. |
#' | info_player_id | integer | isa (StatNet) player id as an INTEGER. Join trap: play-by-play sends the same id as a STRING (player.playerId, pop/nep, assists) and map keys are strings; cast before joining. |
#' | info_ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_athlete_game_log(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl", season = 2026)
#' }
#' @export
#' @family SHL Functions
shl_athlete_game_log <- function(athlete_uuid, master_site_instance_id = NULL, season = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/athlete/playerProfile_seasonGameLog"),
    list(playerUuid = athlete_uuid, masterSiteInstanceId = master_site_instance_id, season = season)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_stats_info(raw, .SHL_SCHEMAS[["athlete_game_log"]])
  make_fastRhockey_data(out, "SHL Athlete Game Log Information from www.shl.se", Sys.time())
}

#' Player profile: age, height, weight, position
#'
#' Wraps `GET https://www.shl.se/api/statistics-v2/athlete/profile-page` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param athlete_uuid Required. Athlete uuid.
#' @param master_site_instance_id Site instance (shl1_shl). No effect in the drop-one probe.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | uuid | character | Athlete uuid (platform key space, qQ9-... or 10-char form); joins to the athlete routes and stats-info stats[].info.uuid. Not the isa or ramses_legacy id. |
#' | full_name | character | Given name and surname. |
#' | first_name | character | Given (first) name of the player. |
#' | last_name | character | Surname (family name) of the player. |
#' | birth_date | character | Date of birth (YYYY-MM-DD). |
#' | nationality | character | Nationality (ISO country code). |
#' | gender | character | Sex of the athlete or team (male / female). |
#' | age_value | integer | Age of the player in whole years. |
#' | age_format | character | Unit of value: years. |
#' | weight_value | integer | Weight of the player in kilograms. |
#' | weight_format | character | Unit of value: kg. |
#' | height_value | integer | Height of the player in centimetres. |
#' | height_format | character | Unit of value: cm. |
#' | jersey_number | integer | Jersey (shirt) number of the player. |
#' | position | character | Position label in Swedish (Målvakt = goalkeeper). |
#' | position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
#' | shoots | character | Shooting side (L / R; Left / Right). |
#' | team_uuid | character | Platform team uuid. |
#' | team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
#' | team_name | character | Name of the team. |
#' | team_code | character | StatNet team code (SAIK, FHC). |
#' | team_media | character | Media URL or string. |
#' | season_stats | character | Current-season stat rows of the player (empty in the sample). |
#' | career_stats | character | Career stat rows of the player (empty in the sample). |
#' | is_in_squad | logical | Whether the player is in the current squad. |
#' | media_url | character | Portrait URL on the imgix CDN. |
#' | media_alt | character | Image alt text. Empty string in every sample (placeholder). |
#' | media_srcset | character | Responsive image srcset. |
#' | media_url_img_original_proportion | character | Image URL, original proportion. |
#' | media_url_placeholder_image | character | Placeholder image URL. Empty string in every sample (placeholder). |
#' | media_videourl | character | Link to a club video. Empty string in every sample (placeholder). |
#' | media_photographer | character | Image credit. Empty string in every sample (placeholder). |
#' | media_focal_point | character | Image crop focal point. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_athlete_profile(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl")
#' }
#' @export
#' @family SHL Functions
shl_athlete_profile <- function(athlete_uuid, master_site_instance_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/athlete/profile-page"),
    list(playerUuid = athlete_uuid, masterSiteInstanceId = master_site_instance_id)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_page(raw, .SHL_SCHEMAS[["athlete_profile"]])
  make_fastRhockey_data(out, "SHL Athlete Profile Information from www.shl.se", Sys.time())
}

#' Seasons a player has stats for
#'
#' Wraps `GET https://www.shl.se/api/statistics-v2/athlete/seasonList` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param athlete_uuid Required. Athlete uuid.
#' @param master_site_instance_id Site instance (shl1_shl). No effect in the drop-one probe.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | value | character | Season START year (2025 = 2025/2026); the season= param of playerProfile_seasonGameLog. Numeric value sent as a string. |
#' | name | character | Season label (e.g. 2025/2026). |
#' | label | character | Display label (season list: 2025/2026). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_athlete_seasons(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl")
#' }
#' @export
#' @family SHL Functions
shl_athlete_seasons <- function(athlete_uuid, master_site_instance_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/athlete/seasonList"),
    list(playerUuid = athlete_uuid, masterSiteInstanceId = master_site_instance_id)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_generic(raw, .SHL_SCHEMAS[["athlete_seasons"]])
  make_fastRhockey_data(out, "SHL Athlete Seasons Information from www.shl.se", Sys.time())
}

#' Skater and goalie box score per team (2023/24 on)
#'
#' Wraps `GET https://www.shl.se/api/gameday/boxscore/{game_uuid}` of the SHL site API (www.shl.se).
#'   Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the
#'   site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | team_side | character | Side of the game: home or away. |
#' | position_group | character | skater or goalie (goalies come from the body's gkStats rows). |
#' | player_id | integer | isa (StatNet) player id, integer. Play-by-play sends it as a string, every other route as an integer; it is cast here so the frames join. |
#' | team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | period | integer | Period: 0 = the whole game, 1-3 the periods, 4 and up overtime. |
#' | full_name | character | Given name and surname. |
#' | first_name | character | Given (first) name of the player. |
#' | last_name | character | Surname (family name) of the player. |
#' | nr | integer | Jersey (shirt) number of the player. |
#' | pos | character | Position code in this game (e.g. CE, LW, RW, D, GK). |
#' | line | integer | Line (unit) number the player was listed on. |
#' | g | numeric | Number of goals scored. |
#' | a | numeric | Number of assists credited. |
#' | ppg | numeric | Power-play goals. |
#' | sw | numeric | Number of shots that missed the net. |
#' | pim | numeric | Penalty minutes. |
#' | sog | numeric | Number of shots on goal. |
#' | ppsog | numeric | Power-play shots on goal. |
#' | plus_minus | numeric | Plus/minus in the game. |
#' | toi | character | Time on ice (mm:ss). Clock/duration as an mm:ss string. |
#' | hits | numeric | Body checks delivered. |
#' | fow | numeric | Number of face-offs won. |
#' | fol | numeric | Face-offs lost. |
#' | fo_perc | numeric | Face-off win %. |
#' | ga | numeric | Number of goals conceded (goals against). |
#' | soga | numeric | Shots on goal against. |
#' | spga | numeric | Unverified goalie column (box score). |
#' | svs | numeric | Number of saves made by the goalie. |
#' | svs_perc | numeric | Save percentage of the game as a FRACTION 0-1 (e.g. '0.9655...'); integer 0 for a goalie without shots. Numeric, but sent as a string in some rows and as a number in others. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_boxscore(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_boxscore <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/boxscore/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_player_box(raw, .SHL_SCHEMAS[["boxscore"]])
  make_fastRhockey_data(out, "SHL Boxscore Information from www.shl.se", Sys.time())
}

#' Game header: StatNet extId, arena, state, teams, scores
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/game-info/{game_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | instance_context_site_team_is_in_game | logical | Whether the site's team plays in the game. |
#' | instance_context_site_team_is_home_team | logical | Whether the site's team is the home team. |
#' | instance_context_site_team_is_away_team | logical | Whether the site's team is the away team. |
#' | game_info_game_uuid | character | Game uuid (platform key space, e.g. bdhvuc5tex); joins to schedule gameInfo[].uuid and every `gameday/{game_uuid}` route. The StatNet number of the same game is gameId. |
#' | game_info_ext_id | integer | StatNet game number as a string (StatNet key space); equals gameday gameId. Numeric value sent as a string. |
#' | game_info_start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
#' | game_info_arena_name | character | Name of the arena hosting the game. |
#' | game_info_state | character | Game state with an underscore (post_game seen); the schedule spells it post-game. |
#' | game_info_overtime | logical | Whether the game went to overtime. |
#' | game_info_shootout | logical | Whether the game went to a shootout. |
#' | game_info_series_code | character | Series code of the game (SHL). |
#' | game_info_series_name | character | Series label of the game (SHL). |
#' | game_info_round_number | integer | Schedule round (1 = first game day of the season). |
#' | game_info_round_label | character | Round label (often empty). Empty string in every sample (placeholder). |
#' | game_info_series_display_name | character | Series label shown on the site (SHL). |
#' | home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | home_team_names_short | character | Short club name ('Herr' = men's team). |
#' | home_team_names_long | character | Long club name ('Herr' = men's team). |
#' | home_team_names_full | character | Full club name ('Herr' = men's team). |
#' | home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | home_team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
#' | home_team_founded_on | character | Club founding date. Numeric value sent as a string. |
#' | home_team_address | character | Club postal address. |
#' | home_team_email | character | Contact email address of the club. |
#' | home_team_icon | character | URL of the team logo image. |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | away_team_names_short | character | Short club name ('Herr' = men's team). |
#' | away_team_names_long | character | Long club name ('Herr' = men's team). |
#' | away_team_names_full | character | Full club name ('Herr' = men's team). |
#' | away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | away_team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
#' | away_team_founded_on | character | Club founding date. Numeric value sent as a string. |
#' | away_team_address | character | Club postal address. |
#' | away_team_email | character | Contact email address of the club. |
#' | away_team_icon | character | URL of the team logo image. |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk); the seriesUuid param of game-schedule. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_game_header(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_game_header <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/game-info/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_page(raw, .SHL_SCHEMAS[["game_header"]])
  make_fastRhockey_data(out, "SHL Game Header Information from www.shl.se", Sys.time())
}

#' Score, state, clock, StatNet gameId and gameSourceId
#'
#' Wraps `GET https://www.shl.se/api/gameday/game-overview/{game_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | home_team_game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
#' | home_team_game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games{} and equals sports-v2 game-info extId. Not the game uuid. |
#' | home_team_place | character | Side of the game: home or away. |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | home_team_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | home_team_team_name | character | Team display name (e.g. Frölunda HC). |
#' | home_team_team_code | character | Team code (FHC, LHC, ...). |
#' | home_team_game_uuid | character | Game uuid (platform key space, e.g. bdhvuc5tex); joins to schedule gameInfo[].uuid and every `gameday/{game_uuid}` route. The StatNet number of the same game is gameId. |
#' | away_team_game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
#' | away_team_game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games{} and equals sports-v2 game-info extId. Not the game uuid. |
#' | away_team_place | character | Side of the game: home or away. |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | away_team_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | away_team_team_name | character | Team display name (e.g. Frölunda HC). |
#' | away_team_team_code | character | Team code (FHC, LHC, ...). |
#' | away_team_game_uuid | character | Game uuid (platform key space, e.g. bdhvuc5tex); joins to schedule gameInfo[].uuid and every `gameday/{game_uuid}` route. The StatNet number of the same game is gameId. |
#' | home_goals | integer | Home goals after this event (running score). |
#' | away_goals | integer | Away goals after this event (running score). |
#' | state | character | StatNet game state (GameEnded seen). |
#' | time_period | integer | Period (1-3, 4 = overtime, 5 = shootout). |
#' | time_period_time | character | Time in period (MM:SS). Clock/duration as an mm:ss string. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_game_overview(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_game_overview <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/game-overview/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_page(raw, .SHL_SCHEMAS[["game_overview"]])
  make_fastRhockey_data(out, "SHL Game Overview Information from www.shl.se", Sys.time())
}

#' Goals by period, head-to-head summary, best scorers
#'
#' Wraps `GET https://www.shl.se/api/gameday/game-info/{game_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | team_head2_head | character | Previous meetings of the two teams. |
#' | team_period_caption | character | Caption text of the image. |
#' | team_period_overtime | logical | Whether the game went to overtime. |
#' | team_period_shootout | logical | Whether the game went to a shootout. |
#' | team_period_stats | character | Team stats per period: rows with home and away values. |
#' | best_scorers | character | Top scorers of the game. |
#' | statistics_provider | character | Stats provider of the data (statnet seen). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_game_summary(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_game_summary <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/game-info/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_page(raw, .SHL_SCHEMAS[["game_summary"]])
  make_fastRhockey_data(out, "SHL Game Summary Information from www.shl.se", Sys.time())
}

#' Games of the last played match day of an ssgt
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/last-game-day` of the SHL site API (www.shl.se).
#'   Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the
#'   site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param season Season END year (2026 = 2025/26), mapped to its ssgt through layout-info. None =
#'   the active ssgt (these match-day views follow the current season).
#' @param game_type Game type: regular (Grundserie), playoff (Slutspel), qualdown (relegation
#'   qualification) or playin (2023/24 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
#' | state | character | Game state: pre-game or post-game (hyphenated; a live value was not observed). |
#' | overtime | logical | Whether the game went to overtime. |
#' | shootout | logical | Whether the game went to a shootout. |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | home_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | home_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | home_team_code | character | Home team code (e.g. FHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | home_team_names_short | character | Short club name ('Herr' = men's team). |
#' | home_team_names_long | character | Long club name ('Herr' = men's team). |
#' | home_team_names_full | character | Full club name ('Herr' = men's team). |
#' | home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | home_team_icon | character | URL of the team logo image. |
#' | away_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | away_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | away_team_code | character | Away team code (e.g. LHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | away_team_names_short | character | Short club name ('Herr' = men's team). |
#' | away_team_names_long | character | Long club name ('Herr' = men's team). |
#' | away_team_names_full | character | Full club name ('Herr' = men's team). |
#' | away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | away_team_icon | character | URL of the team logo image. |
#' | venue_uuid | character | Arena uuid (platform key space). |
#' | venue_name | character | Name of the arena (venue). |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
#' | series_code | character | Series code (SHL, CHL). |
#' | series_display_name | character | Series label shown on the site (SHL, CHL). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_last_game_day()
#' }
#' @export
#' @family SHL Functions
shl_last_game_day <- function(season = NULL, game_type = "regular", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .shl_check_choice("game_type", game_type, .SHL_GAME_TYPES)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/last-game-day"),
    list(season = season, gameType = game_type)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_schedule(raw, .SHL_SCHEMAS[["last_game_day"]])
  make_fastRhockey_data(out, "SHL Last Game Day Information from www.shl.se", Sys.time())
}

#' Directory of every SHL ssgt (season + series + game type) with teams and the stats modules
#'
#' Wraps `GET https://www.shl.se/api/statistics-v2/layout-info` of the SHL site API (www.shl.se).
#'   Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the
#'   site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | season | integer | Season END year (2026 = 2025/26). |
#' | season_code | character | Provider season code: the START year as a string (2025 = 2025/26). |
#' | season_name | character | Season label (e.g. 2025/2026). |
#' | game_type | character | Game type record (uuid, code regular / playoff / qualdown, Swedish name). |
#' | game_type_name | character | Game type label in Swedish: Grundserie = regular season, Slutspel = playoffs, Kvalspel nedflyttning = relegation qualification, Kvalserien = the old promotion/relegation series. |
#' | series_code | character | Series code (SHL, CHL; HA and Kvalserien in old career logs). |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | state | character | ssgt state: active (the current season) or closed. |
#' | season_uuid | character | Season uuid (platform key space; code 2025 = 2025/2026); the seasonUuid param of game-schedule. |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
#' | game_type_uuid | character | Game type uuid (platform key space; regular qQ9-af37Ti40B); the gameTypeUuid param of game-schedule. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_layout_info()
#' }
#' @export
#' @family SHL Functions
shl_layout_info <- function(return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/layout-info"),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_layout(raw, .SHL_SCHEMAS[["layout_info"]])
  make_fastRhockey_data(out, "SHL Layout Info Information from www.shl.se", Sys.time())
}

#' Games of the next match day of an ssgt
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/next-game-day` of the SHL site API (www.shl.se).
#'   Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the
#'   site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param season Season END year (2026 = 2025/26), mapped to its ssgt through layout-info. None =
#'   the active ssgt (these match-day views follow the current season).
#' @param game_type Game type: regular (Grundserie), playoff (Slutspel), qualdown (relegation
#'   qualification) or playin (2023/24 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
#' | state | character | Game state: pre-game or post-game (hyphenated; a live value was not observed). |
#' | overtime | logical | Whether the game went to overtime. |
#' | shootout | logical | Whether the game went to a shootout. |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | home_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | home_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | home_team_code | character | Home team code (e.g. FHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | home_team_names_short | character | Short club name ('Herr' = men's team). |
#' | home_team_names_long | character | Long club name ('Herr' = men's team). |
#' | home_team_names_full | character | Full club name ('Herr' = men's team). |
#' | home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | home_team_icon | character | URL of the team logo image. |
#' | away_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | away_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | away_team_code | character | Away team code (e.g. LHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | away_team_names_short | character | Short club name ('Herr' = men's team). |
#' | away_team_names_long | character | Long club name ('Herr' = men's team). |
#' | away_team_names_full | character | Full club name ('Herr' = men's team). |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | away_team_icon | character | URL of the team logo image. |
#' | venue_uuid | character | Arena uuid (platform key space). |
#' | venue_name | character | Name of the arena (venue). |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
#' | series_code | character | Series code (SHL, CHL). |
#' | series_display_name | character | Series label shown on the site (SHL, CHL). |
#' | away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_next_game_day()
#' }
#' @export
#' @family SHL Functions
shl_next_game_day <- function(season = NULL, game_type = "regular", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .shl_check_choice("game_type", game_type, .SHL_GAME_TYPES)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/next-game-day"),
    list(season = season, gameType = game_type)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_schedule(raw, .SHL_SCHEMAS[["next_game_day"]])
  make_fastRhockey_data(out, "SHL Next Game Day Information from www.shl.se", Sys.time())
}

#' Play-by-play events with shot coordinates (2023/24 on)
#'
#' Wraps `GET https://www.shl.se/api/gameday/play-by-play/{game_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
#' | game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games{} and equals sports-v2 game-info extId. Not the game uuid. |
#' | event_id | integer | StatNet event sequence number; unique within one game only (pair it with gameId or gameUuid). |
#' | event_uuid | character | Event uuid (platform key space), unique across games. eventId is the StatNet per-game sequence number. |
#' | round | integer | Schedule round of the game (1 = first game day). |
#' | game_type | character | StatNet league label: Elitserien (SHL's name until 2013). |
#' | arena | character | Name of the arena hosting the game. |
#' | attendance | integer | Spectators at the game. |
#' | start_date_and_time | character | Scheduled start (local). |
#' | period | integer | Period (1-3, 4 = overtime, 5 = shootout). |
#' | time | character | Game clock in the period (MM:SS, counting up from 00:00). Clock/duration as an mm:ss string. |
#' | game_state | character | Game state (e.g. GameEnded). |
#' | revision | integer | Event revision counter. |
#' | type | character | Event type: goal, shot, penalty, goalkeeper (goalie in / out), period (start / end), timeout. |
#' | real_world_time | character | Wall-clock time of the event (local). |
#' | updated_time | character | Last update time of the event. |
#' | home_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | home_team_name | character | Team display name (e.g. Frölunda HC). |
#' | home_team_code | character | Team code (FHC, LHC, ...). |
#' | home_score | integer | Goals scored by the team in the game. |
#' | away_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | away_team_name | character | Team display name (e.g. Frölunda HC). |
#' | away_team_code | character | Team code (FHC, LHC, ...). |
#' | away_score | integer | Goals scored by the team in the game. |
#' | event_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | event_place | character | Side of the game: home or away. |
#' | event_team_code | character | Team code (FHC, LHC, ...). |
#' | event_team_name | character | Team display name (e.g. Frölunda HC). |
#' | player_id | integer | isa (StatNet) player id, integer. Play-by-play sends it as a string, every other route as an integer; it is cast here so the frames join. |
#' | player_first_name | character | Given (first) name of the player. |
#' | player_family_name | character | Surname (family name) of the player. |
#' | player_jersey_today | character | Jersey number in this game. Numeric value sent as a string. |
#' | is_entering | logical | Goalie event, true when the goalie enters the ice and false when leaving. |
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | location_x | integer | Shot x coordinate. Units, origin and orientation unverified; range -29..481 over the 118 located events of game bdhvuc5tex (discovery download, 2026-10-12). |
#' | location_y | integer | Shot y coordinate. Units, origin and orientation unverified; range -140..129 over the 118 located events of game bdhvuc5tex (discovery download, 2026-10-12). |
#' | goal_section | integer | Goal-mouth section of the shot/goal (-3 lines up with blocked shots; unverified). |
#' | is_penalty_shot | logical | Whether the shot/goal was a penalty shot. |
#' | variant_short_name | character | Penalty type (Minor, Major, ...). |
#' | variant_minor_time | character | Minor-penalty minutes. Numeric value sent as a string. |
#' | variant_double_minor_time | character | Double-minor minutes in the penalty. Numeric value sent as a string. |
#' | variant_bench_time | character | Bench minor minutes in the penalty. Numeric value sent as a string. |
#' | variant_major_time | character | Major-penalty minutes. Numeric value sent as a string. |
#' | variant_misconduct_time | character | Misconduct minutes. Numeric value sent as a string. |
#' | variant_g_m_time | character | Game-misconduct minutes in the penalty. Numeric value sent as a string. |
#' | variant_m_p_time | character | Match-penalty minutes. Numeric value sent as a string. |
#' | variant_description | character | Text description (penalty, media). |
#' | offence | character | Penalty offence code (e.g. CROSS, HOOK, TRIP). |
#' | did_render_in_penalty_shot | logical | Whether the penalty led to a penalty shot. |
#' | home_goals | integer | Home goals after this event (running score). |
#' | away_goals | integer | Away goals after this event (running score). |
#' | is_empty_net_goal | logical | Whether the goal went into an empty net. |
#' | pop_player_ids | list | isa ids of the scoring side's players on the ice for the goal (list of integers). |
#' | nep_player_ids | list | isa ids of the conceding side's players on the ice for the goal (list of integers). |
#' | assist1_player_id | integer | isa id of the first assist. |
#' | assist1_first_name | character | First assist: given name. |
#' | assist1_family_name | character | First assist: surname. |
#' | assist1_jersey_today | character | First assist: jersey number in this game (string). |
#' | assist2_player_id | integer | isa id of the second assist. |
#' | assist2_first_name | character | Second assist: given name. |
#' | assist2_family_name | character | Second assist: surname. |
#' | assist2_jersey_today | character | Second assist: jersey number in this game (string). |
#' | goal_status | character | Goal strength (e.g. EQ, PP1, SH1). |
#' | started | logical | Whether the period has started. |
#' | started_at | character | Period start time (ISO 8601 UTC). |
#' | finished | logical | Whether the period is finished. |
#' | finished_at | character | Period end time (ISO 8601 UTC). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_pbp(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_pbp <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/play-by-play/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_pbp(raw, .SHL_SCHEMAS[["pbp"]])
  make_fastRhockey_data(out, "SHL Pbp Information from www.shl.se", Sys.time())
}

#' Extended skater and goalie game stats per team (2023/24 on)
#'
#' Wraps `GET https://www.shl.se/api/gameday/player-stats/{game_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | team_side | character | Side of the game: home or away. |
#' | position_group | character | skater or goalie (goalies come from the body's gkStats rows). |
#' | player_id | integer | isa (StatNet) player id, integer. Play-by-play sends it as a string, every other route as an integer; it is cast here so the frames join. |
#' | team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | period | integer | Period: 0 = the whole game, 1-3 the periods, 4 and up overtime. |
#' | full_name | character | Given name and surname. |
#' | first_name | character | Given (first) name of the player. |
#' | last_name | character | Surname (family name) of the player. |
#' | nr | integer | Jersey (shirt) number of the player. |
#' | pos | character | Position code in this game (e.g. CE, LW, RW, D, GK). |
#' | line | integer | Line (unit) number the player was listed on. |
#' | g | numeric | Number of goals scored. |
#' | a | numeric | Number of assists credited. |
#' | ppg | numeric | Power-play goals. |
#' | sw | numeric | Number of shots that missed the net. |
#' | pim | numeric | Penalty minutes. |
#' | sog | numeric | Number of shots on goal. |
#' | ppsog | numeric | Power-play shots on goal. |
#' | plus_minus | numeric | Plus/minus in the game. |
#' | toi | character | Time on ice (mm:ss). Clock/duration as an mm:ss string. |
#' | hits | numeric | Body checks delivered. |
#' | fow | numeric | Number of face-offs won. |
#' | fol | numeric | Face-offs lost. |
#' | fo_perc | numeric | Face-off win %. |
#' | ga | numeric | Number of goals conceded (goals against). |
#' | soga | numeric | Shots on goal against. |
#' | spga | numeric | Unverified goalie column (box score). |
#' | svs | numeric | Number of saves made by the goalie. |
#' | svs_perc | numeric | Save percentage of the game as a FRACTION 0-1 (e.g. '0.9655...'); integer 0 for a goalie without shots. Numeric, but sent as a string in some rows and as a number in others. |
#' | tp | numeric | Points (goals + assists). |
#' | shg | numeric | Shorthanded goals. |
#' | nep | numeric | On ice for goals against (count). |
#' | pop | numeric | On ice for goals for (count). |
#' | fo_tot | numeric | Face-offs taken. |
#' | num_pen | numeric | Number of penalties. |
#' | ms | numeric | Missed shots (wide + blocked). |
#' | bk_s | numeric | Number of blocked shots. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_player_box(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_player_box <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/player-stats/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_player_box(raw, .SHL_SCHEMAS[["player_box"]])
  make_fastRhockey_data(out, "SHL Player Box Information from www.shl.se", Sys.time())
}

#' Player head-to-head for a matchup
#'
#' Wraps `GET
#'   https://www.shl.se/api/statistics-v2/player-h2h-
#'   v2/{ssgt_uuid}/{home_team_uuid}/{away_team_uuid}` of the SHL site API (www.shl.se). Unofficial
#'   use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the site's
#'   ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param ssgt_uuid ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular season).
#'   Directory: /statistics-v2/layout-info seasonSeriesGameTypeTeams[].ssgtUuid.
#' @param home_team_uuid Home team uuid of the game.
#' @param away_team_uuid Away team uuid of the game.
#' @param game_uuid Game uuid; narrows the head-to-head to that game's line-ups.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | uuid | character | Athlete uuid (platform key space; qQ9-... or 10-char form); joins to the athlete routes and stats-info stats[].info.uuid. Not the isa or ramses_legacy id. |
#' | first_name | character | Given (first) name of the player. |
#' | last_name | character | Surname (family name) of the player. |
#' | full_name | character | Given name and surname. |
#' | birth_date | character | Date of birth (YYYY-MM-DD). |
#' | nationality | character | Nationality (ISO country code). |
#' | gender | character | Sex of the athlete or team (male / female). |
#' | jersey_number | character | Jersey (shirt) number of the player. |
#' | position | character | Position label in English (e.g. Forward); N/A when unknown. |
#' | position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
#' | age_value | integer | Age of the player in whole years. |
#' | age_format | character | Unit of value: years. |
#' | height_value | integer | Height of the player in centimetres. |
#' | height_format | character | Unit of value: cm. |
#' | weight_value | integer | Weight of the player in kilograms. |
#' | weight_format | character | Unit of value: kg. |
#' | team_uuid | character | Platform team uuid. |
#' | team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
#' | team_name | character | Name of the team. |
#' | team_code | character | StatNet team code (SAIK, FHC). |
#' | team_media | character | Media URL or string. |
#' | media_url | character | Portrait URL on the imgix CDN. |
#' | media_alt | character | Image alt text. Empty string in every sample (placeholder). |
#' | media_srcset | character | Responsive image srcset. |
#' | season_stats | character | Current-season stat rows of the player (empty in the sample). |
#' | career_stats | character | Career stat rows of the player (empty in the sample). |
#' | match_stats_nr | integer | Jersey (shirt) number of the player. |
#' | match_stats_pos | character | Position code in this game (e.g. CE, LW, RW, D, GK). |
#' | match_stats_line | integer | Line (unit) number the player was listed on. |
#' | match_stats_g | integer | Number of goals scored. |
#' | match_stats_a | integer | Number of assists credited. |
#' | match_stats_tp | integer | Points (goals + assists). |
#' | match_stats_shg | integer | Shorthanded goals. |
#' | match_stats_ppg | integer | Power-play goals. |
#' | match_stats_sw | integer | Number of shots that missed the net. |
#' | match_stats_pim | integer | Penalty minutes. |
#' | match_stats_sog | integer | Number of shots on goal. |
#' | match_stats_ppsog | integer | Power-play shots on goal. |
#' | match_stats_nep | integer | On ice for goals against (count). |
#' | match_stats_pop | integer | On ice for goals for (count). |
#' | match_stats_toi | character | Time on ice (mm:ss). Clock/duration as an mm:ss string. |
#' | match_stats_hits | integer | Body checks delivered. |
#' | match_stats_fo_tot | integer | Face-offs taken. |
#' | match_stats_fow | integer | Number of face-offs won. |
#' | match_stats_fol | integer | Face-offs lost. |
#' | match_stats_num_pen | integer | Number of penalties. |
#' | match_stats_fo_perc | numeric | Face-off win %. |
#' | match_stats_ms | integer | Missed shots (wide + blocked). |
#' | match_stats_bk_s | integer | Number of blocked shots. |
#' | match_stats_plus_minus | integer | Goals for minus goals against while on ice (+/-). |
#' | season_avg_stats | character | Head-to-head season averages. |
#' | statistics_provider | character | Stats provider of the data (statnet seen). |
#' | is_in_squad | logical | Whether the player is in the current squad. |
#' | effective_ssgt_uuid | character | The ssgt the head-to-head was computed for: the CURRENT season, whatever the path asked. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_player_h2h(ssgt_uuid = "iuzqg7dqk9", home_team_uuid = "087a-087aTQv9u", away_team_uuid = "41c4-41c4BiYZU", game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_player_h2h <- function(ssgt_uuid, home_team_uuid, away_team_uuid, game_uuid = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/player-h2h-v2/", ssgt_uuid, "/", home_team_uuid, "/", away_team_uuid),
    list(gameUuid = game_uuid)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_h2h(raw, .SHL_SCHEMAS[["player_h2h"]])
  make_fastRhockey_data(out, "SHL Player H2h Information from www.shl.se", Sys.time())
}

#' Post-game team comparison
#'
#' Wraps `GET https://www.shl.se/api/gameday/post-game-data/team-stats/{game_uuid}` of the SHL site
#'   API (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and
#'   are mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param ssgt_uuid Required. ssgt uuid.
#' @param home_team_uuid Required. Home team uuid; without it the home side comes back blank.
#' @param away_team_uuid Required. Away team uuid; without it the away side comes back blank.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | home_team_side_translate_key | character | Translation key of the side. |
#' | home_team_left_type | character | Stat code compared on the left (e.g. G, PPG). |
#' | home_team_left_value | integer | Value (string-encoded). |
#' | home_team_right_type | character | Stat code compared on the right (e.g. SOG, PPOpp). |
#' | home_team_right_value | integer | Value (string-encoded). |
#' | home_team_center_type | character | Stat code compared in the middle (e.g. SEff, PPPerc). |
#' | home_team_center_value | character | Value (string-encoded). Numeric, but sent as a string in some rows and as a number in others. |
#' | away_team_side_translate_key | character | Translation key of the side. |
#' | away_team_left_type | character | Stat code compared on the left (e.g. G, PPG). |
#' | away_team_left_value | integer | Value (string-encoded). |
#' | away_team_right_type | character | Stat code compared on the right (e.g. SOG, PPOpp). |
#' | away_team_right_value | integer | Value (string-encoded). |
#' | away_team_center_type | character | Stat code compared in the middle (e.g. SEff, PPPerc). |
#' | away_team_center_value | character | Value (string-encoded). Numeric, but sent as a string in some rows and as a number in others. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_post_game_team_stats(game_uuid = "bdhvuc5tex", ssgt_uuid = "iuzqg7dqk9", home_team_uuid = "087a-087aTQv9u", away_team_uuid = "41c4-41c4BiYZU")
#' }
#' @export
#' @family SHL Functions
shl_post_game_team_stats <- function(game_uuid, ssgt_uuid, home_team_uuid, away_team_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/post-game-data/team-stats/", game_uuid),
    list(ssgtUuid = ssgt_uuid, homeTeamUuid = home_team_uuid, awayTeamUuid = away_team_uuid)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_generic(raw, .SHL_SCHEMAS[["post_game_team_stats"]])
  make_fastRhockey_data(out, "SHL Post Game Team Stats Information from www.shl.se", Sys.time())
}

#' Current roster of a team, grouped by position
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/athletes/by-team-uuid/{team_uuid}` of the SHL site
#'   API (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and
#'   are mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param team_uuid Team uuid (homeTeamInfo.uuid / awayTeamInfo.uuid in the schedule, or
#'   teamList[].uuid).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | position | character | Position group label: Målvakter = goalkeepers, Backar = defencemen, Forwards. |
#' | position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
#' | player_type | character | Roster role of the player (unverified). |
#' | gender | character | Sex of the athlete or team (male / female). |
#' | uuid | character | Athlete uuid (platform key space; qQ9-... or 10-char form); joins to the athlete routes and stats-info stats[].info.uuid. Not the isa or ramses_legacy id. |
#' | first_name | character | Given (first) name of the player. |
#' | last_name | character | Surname (family name) of the player. |
#' | full_name | character | Given name and surname. |
#' | nationality | character | Nationality (ISO country code). |
#' | jersey_number | integer | Jersey (shirt) number of the player. |
#' | portrait_url | character | URL of the player's latest rendered portrait. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_roster(team_uuid = "087a-087aTQv9u")
#' }
#' @export
#' @family SHL Functions
shl_roster <- function(team_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/athletes/by-team-uuid/", team_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_roster(raw, .SHL_SCHEMAS[["roster"]])
  make_fastRhockey_data(out, "SHL Roster Information from www.shl.se", Sys.time())
}

#' Schedule and results of one season + series + game type
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/game-schedule` of the SHL site API (www.shl.se).
#'   Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the
#'   site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param season Required. Season END year (2026 = 2025/26). Mapped to the provider's ssgt through
#'   /statistics-v2/layout-info (SHL 1975/76 on); a season it does not list raises NoDataError.
#' @param game_type Game type: regular (Grundserie), playoff (Slutspel), qualdown (relegation
#'   qualification) or playin (2023/24 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | raw_start_date_time | character | Start time, UTC ISO 8601. |
#' | start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
#' | state | character | Game state: pre-game or post-game (hyphenated; a live value was not observed). |
#' | overtime | logical | Whether the game went to overtime. |
#' | shootout | logical | Whether the game went to a shootout. |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | home_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | home_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | home_team_code | character | Home team code (e.g. FHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | home_team_names_short | character | Short club name ('Herr' = men's team). |
#' | home_team_names_long | character | Long club name ('Herr' = men's team). |
#' | home_team_names_full | character | Full club name ('Herr' = men's team). |
#' | home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | home_team_icon | character | URL of the team logo image. |
#' | away_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | away_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | away_team_code | character | Away team code (e.g. LHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | away_team_names_short | character | Short club name ('Herr' = men's team). |
#' | away_team_names_long | character | Long club name ('Herr' = men's team). |
#' | away_team_names_full | character | Full club name ('Herr' = men's team). |
#' | away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | away_team_icon | character | URL of the team logo image. |
#' | venue_uuid | character | Arena uuid (platform key space). |
#' | venue_name | character | Name of the arena (venue). |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
#' | series_code | character | Series code (SHL, CHL). |
#' | series_display_name | character | Series label shown on the site (SHL, CHL). |
#' | round_number | integer | Schedule round (1 = first game day of the season). |
#' | round_label | character | Round label (often empty). Empty string in every sample (placeholder). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_schedule(season = 2026)
#' }
#' @export
#' @family SHL Functions
shl_schedule <- function(season, game_type = "regular", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .shl_check_choice("game_type", game_type, .SHL_GAME_TYPES)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/game-schedule"),
    list(season = season, gameType = game_type)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_schedule(raw, .SHL_SCHEMAS[["schedule"]])
  make_fastRhockey_data(out, "SHL Schedule Information from www.shl.se", Sys.time())
}

#' Series, seasons (1975/76 on) and game types with their uuids, plus the default ssgt
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/season-series-game-types-filter` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param series_uuid Series uuid (SHL qQ9-bb0bzEWUk, CHL qQ9-9397drVEh). No effect on the body in
#'   the drop-one probe.
#' @param season_uuid Season uuid; picks the default ssgt returned for that season (omit for the
#'   current season).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | kind | character | Directory entry kind: series, season or game_type. |
#' | code | character | Game type code: regular, playoff, qualdown (legacy career logs: qualification). |
#' | name | character | Game type label in Swedish: Grundserie = regular season, Slutspel = playoffs, Kvalspel nedflyttning = relegation qualification, Kvalserien = the old promotion/relegation series. |
#' | uuid | character | Game type uuid (platform key space; regular qQ9-af37Ti40B); the gameTypeUuid param of game-schedule. |
#' | deleted | logical | Soft-delete flag of the record (false in every sample). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_season_filter(series_uuid = "qQ9-bb0bzEWUk", season_uuid = "xs4m9qupsi")
#' }
#' @export
#' @family SHL Functions
shl_season_filter <- function(series_uuid = NULL, season_uuid = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/season-series-game-types-filter"),
    list(series = series_uuid, season = season_uuid)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_filter(raw, .SHL_SCHEMAS[["season_filter"]])
  make_fastRhockey_data(out, "SHL Season Filter Information from www.shl.se", Sys.time())
}

#' Teams (sites) taking part in an ssgt
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/all-sites/{ssgt_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param ssgt_uuid ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular season).
#'   Directory: /statistics-v2/layout-info seasonSeriesGameTypeTeams[].ssgtUuid.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | id | character | Club site instance id (e.g. bif1_bif); equals instanceId. Not a team uuid or code. |
#' | instance_id | character | Club site instance id (e.g. fhc1_fhc). |
#' | public_url | character | Public image URL. |
#' | name | character | Full name of the club. |
#' | icon | character | URL of the team logo image. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_sites(ssgt_uuid = "iuzqg7dqk9")
#' }
#' @export
#' @family SHL Functions
shl_sites <- function(ssgt_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/all-sites/", ssgt_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_generic(raw, .SHL_SCHEMAS[["sites"]])
  make_fastRhockey_data(out, "SHL Sites Information from www.shl.se", Sys.time())
}

#' Stats provider of an ssgt, a bare string such as statnet
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/ssgt/{ssgt_uuid}/provider` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param ssgt_uuid ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular season).
#'   Directory: /statistics-v2/layout-info seasonSeriesGameTypeTeams[].ssgtUuid.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | provider | character | Stats provider of the ssgt (statnet, impleo). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_ssgt_provider(ssgt_uuid = "iuzqg7dqk9")
#' }
#' @export
#' @family SHL Functions
shl_ssgt_provider <- function(ssgt_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/ssgt/", ssgt_uuid, "/provider"),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_generic(raw, .SHL_SCHEMAS[["ssgt_provider"]])
  make_fastRhockey_data(out, "SHL Ssgt Provider Information from www.shl.se", Sys.time())
}

#' Standings of an ssgt (gameday widget)
#'
#' Wraps `GET https://www.shl.se/api/gameday/team-standings/{ssgt_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param ssgt_uuid ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular season).
#'   Directory: /statistics-v2/layout-info seasonSeriesGameTypeTeams[].ssgtUuid.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | season | integer | Season END year (2026 = 2025/26). |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | team_code | character | StatNet team code (SAIK, FHC). |
#' | team_uuid | character | Platform team uuid. |
#' | team_name | character | Name of the team. |
#' | team_short_name | character | Short display name of the team. |
#' | rank | integer | Position in the table (1 = top). |
#' | rank_diff | integer | Change in standings rank, in places (the reference point is unverified). |
#' | gp | integer | Number of games played. |
#' | w | integer | Regulation wins. |
#' | t | integer | Games tied after regulation, decided in overtime or a shootout (= OTW + OTL). |
#' | l | integer | Regulation losses. |
#' | otw | integer | Overtime and shootout wins. |
#' | otl | integer | Overtime and shootout losses. |
#' | p | integer | Standings points (3 per regulation win, 2 per OT/SO win, 1 per OT/SO loss). |
#' | gf | integer | Number of goals scored (goals for). |
#' | ga | integer | Number of goals conceded (goals against). |
#' | diff | integer | Goal difference. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_standings(ssgt_uuid = "iuzqg7dqk9")
#' }
#' @export
#' @family SHL Functions
shl_standings <- function(ssgt_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/team-standings/", ssgt_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_standings(raw, .SHL_SCHEMAS[["standings"]])
  make_fastRhockey_data(out, "SHL Standings Information from www.shl.se", Sys.time())
}

#' Stats table of one module for an ssgt (standings, players, goalies, teams, games)
#'
#' Wraps `GET https://www.shl.se/api/statistics-v2/stats-info/{module}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param module Stats module: <group>_<table>. Every name under layout-info
#'   [].filters.<group>.modules.
#' @param season Required. Season END year (2026 = 2025/26). Mapped to the provider's ssgt through
#'   /statistics-v2/layout-info (SHL 1975/76 on); a season it does not list raises NoDataError.
#' @param game_type Game type: regular (Grundserie), playoff (Slutspel), qualdown (relegation
#'   qualification) or playin (2023/24 on).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | rank | integer | Rank in the table. |
#' | gp | numeric | Number of games played. |
#' | w | numeric | Wins. Team modules (teams_result): total wins, regulation + overtime + shootout; goalkeeper modules: wins credited to the goalie; standings_standings: regulation wins only. |
#' | l | numeric | Losses. Team modules (teams_result): total losses, regulation + overtime + shootout; goalkeeper modules: losses charged to the goalie; standings_standings: regulation losses only. |
#' | leading | numeric | Times leading: how often the team was ahead (exact definition unverified). |
#' | trailing | numeric | Times trailing. |
#' | g | numeric | Number of goals scored. |
#' | ga | numeric | Number of goals conceded (goals against). |
#' | gfg | numeric | First goal of the game. |
#' | otw | numeric | Overtime wins, shootouts excluded (see SOW). |
#' | otl | numeric | Overtime losses, shootouts excluded (see SOL). |
#' | sow | numeric | Number of shootout wins. |
#' | sol | numeric | Shootout losses. |
#' | p_gp_format | numeric | Points per game (formatted). Numeric value sent as a string. |
#' | gpg | numeric | Goals per game. Numeric value sent as a string. |
#' | gapg | numeric | Goals against per game. Numeric value sent as a string. |
#' | points | numeric | Standings points (3 per regulation win, 2 per OT/SO win, 1 per OT/SO loss). |
#' | info_type | character | Record type of the row's subject: team in team modules, athlete in player and goalie modules. |
#' | info_id | character | Team code (e.g. SAIK) in team and standings modules; null in player, goalie and games modules (use info.uuid). |
#' | info_uuid | character | Athlete uuid (platform key space) in player and goalie modules, team uuid in team modules; joins to the athlete routes or `/sports-v2/teams/{team_uuid}`. |
#' | info_created_at | character | Record creation time (ISO 8601; often null). |
#' | info_created_by | character | Record creator (site instance id; often null). |
#' | info_deleted | logical | Soft-delete flag of the record (false in every sample). |
#' | info_gender | character | Sex of the athlete or team (male / female). |
#' | info_graphics_library | character | Graphics asset key. |
#' | info_last_modified | character | Record last-modified time (ISO 8601). |
#' | info_nationality | character | Nationality (ISO country code). |
#' | info_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | info_ownersite | character | Owning site code. |
#' | info_team_code | character | Team code of the player's team (e.g. FHC). |
#' | info_series | character | Series with their uuids (SHL, CHL). |
#' | info_sport | character | Sport records the club belongs to (ice hockey). |
#' | info_team_media | character | Media URL or string. |
#' | info_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | info_team_names_short | character | Short club name ('Herr' = men's team). |
#' | info_team_names_long | character | Long club name ('Herr' = men's team). |
#' | info_team_names_full | character | Full club name ('Herr' = men's team). |
#' | info_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | info_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | info_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | info_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | info_club_name_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | info_club_name_short | character | Short club name ('Herr' = men's team). |
#' | info_club_name_long | character | Long club name ('Herr' = men's team). |
#' | info_club_name_full | character | Full club name ('Herr' = men's team). |
#' | info_team_info_founded | character | Club founding year (text). Numeric value sent as a string. |
#' | info_team_info_golds | character | Club's championships (text). |
#' | info_team_info_finals | character | Club's final appearances (text). |
#' | info_team_info_address | character | Club postal address. |
#' | info_team_info_phone | character | Telephone number of the club. |
#' | info_team_info_fax | character | Fax number of the club. |
#' | info_team_info_email | character | Contact email address of the club. |
#' | info_team_info_chairman | character | Name of the club chairman. |
#' | info_team_info_manager | character | Club general manager. |
#' | info_team_info_accreditation | character | Press-accreditation contact, free Swedish text (e.g. 'Senast dagen före match till …' = 'no later than the day before the game, to …'). |
#' | info_team_info_facebook | character | Club Facebook URL. |
#' | info_team_info_instagram | character | Club Instagram URL. |
#' | info_team_info_twitter | character | Club X/Twitter URL. |
#' | info_team_ext_ids | character | External ids of the team. |
#' | info_ext_ids | character | External ids of the record. |
#' | info_logo | character | URL of the team logo image. |
#' | info_social_media | character | Club social links. |
#' | info_display_name | character | Team display-name override; unused. Empty string in every sample (placeholder). |
#' | info_code | character | Team code (e.g. SAIK) in team and standings modules; absent for players. |
#' | info_site_display_name | character | Club name as shown on its own site (e.g. Frölunda HC). |
#' | info_team_info_retired_numbers | character | Retired jersey numbers (text). |
#' | info_team_info_contact | character | Club contact text. |
#' | info_club_name | character | Club names by length. |
#' | player_id | integer | isa (StatNet) player id, integer. Play-by-play sends it as a string, every other route as an integer; it is cast here so the frames join. |
#' | svs | numeric | Number of saves made by the goalie. |
#' | svs_perc | numeric | Save %. Numeric value sent as a string. |
#' | gpi | numeric | Games played with ice time. |
#' | gaa | numeric | Goals against average (per 60 minutes). Numeric value sent as a string. |
#' | t | numeric | Goalie ties (goalkeeper modules only); 0 in every sampled row, as overtime and shootouts now decide every game. |
#' | so | numeric | Number of shutouts (games without a goal against). |
#' | mip | character | Minutes played (mm:ss). Clock/duration as an mm:ss string. |
#' | valid_ranking | logical | Whether the row qualifies for the ranking (minimum games / minutes). |
#' | info_full_name | character | Given name and surname. |
#' | info_first_name | character | Given (first) name of the player. |
#' | info_last_name | character | Surname (family name) of the player. |
#' | info_birth_date | character | Date of birth (YYYY-MM-DD). |
#' | info_number | integer | Jersey (shirt) number of the player. |
#' | info_position | character | Position code (CE = centre, LW, RW, D, GK, F); N/A in the athlete game logs. |
#' | info_shoots | character | Shooting side (L / R; Left / Right). |
#' | info_weight_value | integer | Weight of the player in kilograms. |
#' | info_weight_format | character | Unit of value: kg. |
#' | info_height_value | integer | Height of the player in centimetres. |
#' | info_height_format | character | Unit of value: cm. |
#' | info_player_media_id | integer | Media-library id of the portrait (joins to /api/media; not a player id). |
#' | info_player_media_media_string | character | Media reference (type/source/id). |
#' | info_player_media_type | character | Image kind (portrait). |
#' | info_player_media_sort_order | integer | Ordering index for display. |
#' | info_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | info_team_name | character | Team name (e.g. Frölunda HC); empty in some rows. |
#' | info_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | info_team_site_display_name | character | Club name as shown on its own site (e.g. Frölunda HC). |
#' | info_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | tp | numeric | Points (goals + assists). |
#' | a | numeric | Number of assists credited. |
#' | pim | numeric | Penalty minutes. |
#' | gwg | numeric | Game-winning goals. |
#' | ppg | numeric | Power-play goals. |
#' | sog | numeric | Number of shots on goal. |
#' | hits | numeric | Body checks delivered. |
#' | bk_s | numeric | Number of blocked shots. |
#' | plus | numeric | Goals for while on ice. |
#' | minus | numeric | Goals against while on ice. |
#' | plus_minus | numeric | Goals for minus goals against while on ice (+/-). |
#' | toi_gp | character | Time on ice per game (mm:ss). Clock/duration as an mm:ss string. |
#' | tppg | numeric | Points per game. Numeric value sent as a string. |
#' | a_firsts | numeric | Primary assists. |
#' | a_seconds | numeric | Secondary assists. |
#' | ppa | numeric | Power-play assists. |
#' | pptp | numeric | Power-play points. |
#' | eqg | numeric | Even-strength goals. |
#' | eqa | numeric | Even-strength assists. |
#' | eqtp | numeric | Even-strength points. |
#' | shg | numeric | Shorthanded goals. |
#' | sha | numeric | Shorthanded assists. |
#' | shtp | numeric | Shorthanded points. |
#' | trailing2 | numeric | Games trailing after period 2 (unverified). |
#' | trailing1 | numeric | Games trailing after period 1 (unverified). |
#' | tiedg | numeric | Times tied (unverified). |
#' | leading1 | numeric | Games leading after period 1 (unverified). |
#' | leading2 | numeric | Games leading after period 2 (unverified). |
#' | eng | numeric | Empty-net goals. |
#' | toi | character | Time on ice (mm:ss). Duration as an hh:mm:ss string. |
#' | pptoi_gp | character | Power-play time on ice per game (mm:ss). Clock/duration as an mm:ss string. |
#' | shtoi_gp | character | Shorthanded time on ice per game (mm:ss). Clock/duration as an mm:ss string. |
#' | pptoi_perc | numeric | Share of time on ice on the power play (unverified). Numeric value sent as a string. |
#' | shtoi_perc | numeric | Share of time on ice shorthanded (unverified). Numeric value sent as a string. |
#' | pptoi_pp_plus | character | Power-play time on ice per power-play goal for (unverified). Clock/duration as an mm:ss string. |
#' | ppsog | numeric | Power-play shots on goal. |
#' | ppsw | numeric | Power-play shots wide. |
#' | ppsg_perc | numeric | Power-play shooting % (unverified). Numeric value sent as a string. |
#' | pp_plus | numeric | Goals for while on ice on the power play. |
#' | pp_minus | numeric | Goals against while on ice on the power play. |
#' | pp_plus_minus | numeric | Power-play plus/minus. |
#' | shsog | numeric | Shorthanded shots on goal. |
#' | shsw | numeric | Shorthanded shots wide. |
#' | sh_plus | numeric | Goals for while on ice shorthanded. |
#' | sh_minus | numeric | Goals against while on ice shorthanded. |
#' | sh_plus_minus | numeric | Shorthanded plus/minus. |
#' | shtoi_sh_minus | character | Shorthanded time on ice per shorthanded goal against (unverified). Clock/duration as an mm:ss string. |
#' | esg | numeric | Even-strength goals. |
#' | esa | numeric | Even-strength assists. |
#' | estp | numeric | Even-strength points. |
#' | essog | numeric | Even-strength shots on goal. |
#' | essw | numeric | Even-strength shots wide. |
#' | es_plus | numeric | Even-strength goals for while on ice. |
#' | es_minus | numeric | Even-strength goals against while on ice. |
#' | es_plus_minus | numeric | Even-strength plus/minus. |
#' | estoi_gp | character | Even-strength time on ice per game (mm:ss). Clock/duration as an mm:ss string. |
#' | eff_pim | numeric | Effective penalty minutes (served). |
#' | pers_pim | numeric | Personal penalty minutes. |
#' | off_pim | numeric | Offensive penalty minutes. |
#' | minor | numeric | Minor penalties. |
#' | major | numeric | Major penalties. |
#' | misc | numeric | Misconduct penalties. |
#' | gm | numeric | Game misconducts. |
#' | mp | numeric | Match penalties. |
#' | fo_perc | numeric | Face-off win %. Numeric value sent as a string. |
#' | fo | numeric | Face-offs taken. |
#' | fow | numeric | Number of face-offs won. |
#' | fol | numeric | Face-offs lost. |
#' | ofo_perc | numeric | Offensive-zone face-offs won %. Numeric value sent as a string. |
#' | ofo | numeric | Offensive-zone face-offs. |
#' | ofow | numeric | Offensive-zone face-offs won. |
#' | ofol | numeric | Offensive-zone face-offs lost. |
#' | dfo_perc | numeric | Defensive-zone face-offs won %. Numeric value sent as a string. |
#' | dfo | numeric | Defensive-zone face-offs. |
#' | dfow | numeric | Defensive-zone face-offs won. |
#' | dfol | numeric | Defensive-zone face-offs lost. |
#' | fo_violation_perc | numeric | Face-off violation %. Numeric value sent as a string. |
#' | fo_violations | numeric | Face-off violations. |
#' | sg_perc | numeric | Shooting %. Numeric value sent as a string. |
#' | ms | numeric | Missed shots (wide + blocked). |
#' | sog_perc | numeric | Shots on goal % of attempts. Numeric value sent as a string. |
#' | ps | numeric | Number of penalty shots. |
#' | psg | numeric | Penalty-shot goals. |
#' | ps_perc | numeric | Penalty-shot conversion %. Numeric value sent as a string. |
#' | so_ps | numeric | Shootout shots. |
#' | so_psg | numeric | Shootout goals. |
#' | so_ps_perc | numeric | Shootout shooting %. Numeric value sent as a string. |
#' | cf | numeric | Corsi for (shot attempts for). |
#' | ca | numeric | Corsi against (shot attempts against). |
#' | cf_perc | numeric | Corsi for percentage. Numeric value sent as a string. |
#' | close_cf_perc | numeric | Corsi for percentage in close-score situations. Numeric, but sent as a string in some rows and as a number in others. |
#' | ff | numeric | Fenwick for (unblocked shot attempts for). |
#' | fa | numeric | Fenwick against (unblocked shot attempts against). |
#' | ff_perc | numeric | Fenwick for percentage. Numeric, but sent as a string in some rows and as a number in others. |
#' | close_ff_perc | numeric | Fenwick for percentage in close-score situations. Numeric, but sent as a string in some rows and as a number in others. |
#' | on_ice_goal_perc | numeric | On-ice shooting % (unverified). |
#' | on_ice_svs_perc | numeric | On-ice save % (unverified). Numeric, but sent as a string in some rows and as a number in others. |
#' | pdo | numeric | PDO (on-ice shooting % + on-ice save %). Numeric, but sent as a string in some rows and as a number in others. |
#' | info_player_media | character | Portrait image reference (mediaString, type). |
#' | shga | numeric | Shorthanded goals against (conceded while on the power play). |
#' | ppga | numeric | Power-play goals against (conceded while shorthanded). |
#' | enga | numeric | Empty-net goals against. |
#' | pp_opp | numeric | Power-play opportunities. |
#' | pp_perc | numeric | Power-play %. Numeric value sent as a string. |
#' | pp_time | character | Time on the power play (hh:mm:ss). Duration as an hh:mm:ss string. |
#' | st_perc | numeric | Special-teams % (power-play % + penalty-kill %). Numeric value sent as a string. |
#' | pk_opp | numeric | Times shorthanded. |
#' | pk_perc | numeric | Penalty-kill %. Numeric value sent as a string. |
#' | ppsoga | numeric | Power-play shots on goal against. |
#' | pk_time | character | Time shorthanded (hh:mm:ss). Duration as an hh:mm:ss string. |
#' | gp_home | numeric | Home games played. |
#' | att_avg_home | numeric | Average home attendance per game. |
#' | att_total_home | numeric | Total home attendance. |
#' | att_avg_overall | numeric | Average attendance per game. |
#' | att_total_overall | numeric | Total attendance. |
#' | gp_road | numeric | Away games played. |
#' | att_avg_road | numeric | Average away attendance per game. |
#' | att_total_road | numeric | Total away attendance. |
#' | team_pen | numeric | Team (bench) penalties. |
#' | s_eff | numeric | Shooting %. Numeric value sent as a string. |
#' | pps_eff | numeric | Power-play shooting %. Numeric value sent as a string. |
#' | sw | numeric | Number of shots that missed the net. |
#' | sib | numeric | Shots blocked (blocked attempts). |
#' | ss | numeric | Scoring-chance shots (site label; unverified). |
#' | soga | numeric | Shots on goal against. |
#' | msa | numeric | Missed shots against (wide + blocked). |
#' | swa | numeric | Shots wide against. |
#' | sfa | numeric | Shots on goal against (unverified). |
#' | ssa | numeric | Scoring-chance shots against (site label; unverified). |
#' | fo_violations_perc | numeric | Face-off violation %. |
#' | psa | numeric | Penalty-shot attempts. |
#' | pssvs | numeric | Penalty shots saved. |
#' | so_psa | numeric | Shootout attempts against. |
#' | so_pssvs | numeric | Shootout saves. |
#' | so_pssvs_perc | numeric | Shootout save %. Numeric value sent as a string. |
#' | freezes | numeric | Puck freezes (stoppages caused). |
#' | freezes_against | numeric | Puck freezes by the opponent. |
#' | icings | numeric | Icing calls against the team. |
#' | icing_against | numeric | Icings by the opponent. |
#' | offsides | numeric | Offside calls against the team. |
#' | offsides_against | numeric | Offsides by the opponent. |
#' | ssga | numeric | Scoring-chance goals against (unverified). |
#' | ssoga | numeric | Scoring-chance shots on goal against (unverified). |
#' | sssvs_perc | numeric | Save % on scoring-chance shots. Numeric value sent as a string. |
#' | ppsvs_perc | numeric | Save % against the power play (unverified). Numeric value sent as a string. |
#' | shsoga | numeric | Shorthanded shots on goal against. |
#' | shsvs_perc | numeric | Shorthanded save % (unverified). Numeric value sent as a string. |
#' | esga | numeric | Even-strength goals against. |
#' | essoga | numeric | Even-strength shots on goal against. |
#' | essvs_perc | numeric | Even-strength save %. Numeric value sent as a string. |
#' | psga | numeric | Penalty-shot goals against. |
#' | pssvs_perc | numeric | Penalty-shot save %. |
#' | so_psga | numeric | Shootout goals against. |
#' | date_time | character | Game date and time. |
#' | home_team | character | Home team code (e.g. BIF). |
#' | guest_team | character | Away team code (e.g. HV71). |
#' | game_score_home_score | integer | Goals scored by the home team. |
#' | game_score_away_score | integer | Goals scored by the away team. |
#' | game_score_decided_in | character | Filter values for how a game was decided. |
#' | venue | character | Name of the arena the game is played in. |
#' | attendance | numeric | Spectators at the game. |
#' | info_game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games{} and equals sports-v2 game-info extId. Not the game uuid. |
#' | info_home_team_id | character | Home team code (e.g. FHC); not a uuid. |
#' | info_away_team_id | character | Away team code (e.g. LHC); not a uuid. |
#' | diff | numeric | Goal difference. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_stats_info(module = "players_summary", season = 2026)
#' }
#' @export
#' @family SHL Functions
shl_stats_info <- function(module, season, game_type = "regular", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .shl_check_choice("module", module, .SHL_MODULES)
  .shl_check_choice("game_type", game_type, .SHL_GAME_TYPES)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/stats-info/", module),
    list(count = 1000L, season = season, gameType = game_type)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_stats_info(raw, .SHL_SCHEMAS[["stats_info"]])
  make_fastRhockey_data(out, "SHL Stats Info Information from www.shl.se", Sys.time())
}

#' One team
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/teams/{team_uuid}` of the SHL site API (www.shl.se).
#'   Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are mapped to the
#'   site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param team_uuid Team uuid (homeTeamInfo.uuid / awayTeamInfo.uuid in the schedule, or
#'   teamList[].uuid).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | names_short | character | Short club name ('Herr' = men's team). |
#' | names_long | character | Long club name ('Herr' = men's team). |
#' | names_full | character | Full club name ('Herr' = men's team). |
#' | names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | info_founded | character | Club founding year (text). Numeric value sent as a string. |
#' | info_golds | character | Club's championships (text). |
#' | info_finals | character | Club's final appearances (text). |
#' | info_retired_numbers | character | Retired jersey numbers (text). |
#' | info_address | character | Club postal address. |
#' | info_phone | character | Telephone number of the club. |
#' | info_fax | character | Fax number of the club. |
#' | info_email | character | Contact email address of the club. |
#' | info_chairman | character | Name of the club chairman. |
#' | info_manager | character | Club general manager. |
#' | info_accreditation | character | Press-accreditation contact, free Swedish text (e.g. 'Senast dagen före match till …' = 'no later than the day before the game, to …'). |
#' | info_facebook | character | Club Facebook URL. |
#' | info_instagram | character | Club Instagram URL. |
#' | info_twitter | character | Club X/Twitter URL. |
#' | contact | character | Club contact text. |
#' | uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | nationality | character | Nationality (ISO country code). |
#' | arena_name | character | Name of the arena hosting the game. Empty string in every sample (placeholder). |
#' | public_url | character | Public image URL. |
#' | icon | character | URL of the team logo image. |
#' | owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | site_display_name | character | Club name as shown on its own site (e.g. Frölunda HC). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_team(team_uuid = "087a-087aTQv9u")
#' }
#' @export
#' @family SHL Functions
shl_team <- function(team_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/teams/", team_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_page(raw, .SHL_SCHEMAS[["team"]])
  make_fastRhockey_data(out, "SHL Team Information from www.shl.se", Sys.time())
}

#' A team's recent games
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/played-games/{team_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param team_uuid Team uuid (homeTeamInfo.uuid / awayTeamInfo.uuid in the schedule, or
#'   teamList[].uuid).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | date | character | Date (YYYY-MM-DD). |
#' | time | character | Game clock in the period (MM:SS, counting up from 00:00). Clock/duration as an mm:ss string. |
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
#' | state | character | Game state: pre-game or post-game (hyphenated; a live value was not observed). |
#' | overtime | logical | Whether the game went to overtime. |
#' | shootout | logical | Whether the game went to a shootout. |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | home_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | home_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | home_team_code | character | Home team code (e.g. FHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | home_team_names_short | character | Short club name ('Herr' = men's team). |
#' | home_team_names_long | character | Long club name ('Herr' = men's team). |
#' | home_team_names_full | character | Full club name ('Herr' = men's team). |
#' | home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | home_team_icon | character | URL of the team logo image. |
#' | away_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | away_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | away_team_code | character | Away team code (e.g. LHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | away_team_names_short | character | Short club name ('Herr' = men's team). |
#' | away_team_names_long | character | Long club name ('Herr' = men's team). |
#' | away_team_names_full | character | Full club name ('Herr' = men's team). |
#' | away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | away_team_icon | character | URL of the team logo image. |
#' | venue_uuid | character | Arena uuid (platform key space). |
#' | venue_name | character | Name of the arena (venue). |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
#' | series_code | character | Series code (SHL, CHL). |
#' | series_display_name | character | Series label shown on the site (SHL, CHL). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_team_played_games(team_uuid = "087a-087aTQv9u")
#' }
#' @export
#' @family SHL Functions
shl_team_played_games <- function(team_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/played-games/", team_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_schedule(raw, .SHL_SCHEMAS[["team_played_games"]])
  make_fastRhockey_data(out, "SHL Team Played Games Information from www.shl.se", Sys.time())
}

#' Team stats per period and total (2023/24 on)
#'
#' Wraps `GET https://www.shl.se/api/gameday/team-stats/{game_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param game_uuid Game uuid (gameInfo[].uuid from /sports-v2/game-schedule). gameday/* has data
#'   from 2023/24 on; older games answer 200 with an empty body.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | team_side | character | Side of the game: home or away. |
#' | game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
#' | game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games{} and equals sports-v2 game-info extId. Not the game uuid. |
#' | place | character | Side of the game: home or away. |
#' | score | integer | Goals scored by the team in the game. |
#' | team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
#' | team_name | character | Name of the team. |
#' | team_code | character | StatNet team code (SAIK, FHC). |
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | period | integer | Period: 0 = the whole game, 1-3 the periods, 4 and up overtime. |
#' | g | numeric | Number of goals scored. |
#' | pim | numeric | Penalty minutes. |
#' | fow | numeric | Number of face-offs won. |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_team_stats(game_uuid = "bdhvuc5tex")
#' }
#' @export
#' @family SHL Functions
shl_team_stats <- function(game_uuid, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/gameday/team-stats/", game_uuid),
    list()
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_team_stats(raw, .SHL_SCHEMAS[["team_stats"]])
  make_fastRhockey_data(out, "SHL Team Stats Information from www.shl.se", Sys.time())
}

#' A team's next games
#'
#' Wraps `GET https://www.shl.se/api/sports-v2/upcoming-games/{team_uuid}` of the SHL site API
#'   (www.shl.se). Unofficial use of a public source; seasons are END years (2026 = 2025/26) and are
#'   mapped to the site's ssgt uuids through `/statistics-v2/layout-info`.
#'
#' @param team_uuid Team uuid (homeTeamInfo.uuid / awayTeamInfo.uuid in the schedule, or
#'   teamList[].uuid).
#' @param game_place Home/away filter; empty means all. No effect in the drop-one probe.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`); `FALSE` returns the raw
#'   decoded JSON as a list.
#' @return A `fastRhockey_data` tibble (the raw JSON list when `return_parsed = FALSE`) with the following columns:
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | date | character | Date (YYYY-MM-DD). |
#' | time | character | Game clock in the period (MM:SS, counting up from 00:00). Clock/duration as an mm:ss string. |
#' | game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
#' | start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
#' | state | character | Game state: pre-game or post-game (hyphenated; a live value was not observed). |
#' | overtime | logical | Whether the game went to overtime. |
#' | shootout | logical | Whether the game went to a shootout. |
#' | ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams[]. |
#' | home_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | home_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | home_team_code | character | Home team code (e.g. FHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | home_team_names_short | character | Short club name ('Herr' = men's team). |
#' | home_team_names_long | character | Long club name ('Herr' = men's team). |
#' | home_team_names_full | character | Full club name ('Herr' = men's team). |
#' | home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | home_team_score | integer | Goals scored by the team in the game. |
#' | home_team_icon | character | URL of the team logo image. |
#' | away_team_status | character | Result for the team (WIN, LOSE) or record status. |
#' | away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
#' | away_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
#' | away_team_code | character | Away team code (e.g. LHC); joins to gameday teamId and the code-keyed teams{} maps. |
#' | away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
#' | away_team_names_short | character | Short club name ('Herr' = men's team). |
#' | away_team_names_long | character | Long club name ('Herr' = men's team). |
#' | away_team_names_full | character | Full club name ('Herr' = men's team). |
#' | away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
#' | away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
#' | away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
#' | away_team_score | integer | Goals scored by the team in the game. |
#' | away_team_icon | character | URL of the team logo image. |
#' | venue_uuid | character | Arena uuid (platform key space). |
#' | venue_name | character | Name of the arena (venue). |
#' | series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
#' | series_code | character | Series code (SHL, CHL). |
#' | series_display_name | character | Series label shown on the site (SHL, CHL). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: the host answered 404, an empty body (gameday routes before 2023/24), a
#' table with no rows, or the season and game type are not in layout-info. `fastRhockey_asset_fetch`:
#' any other non-2xx answer or a dead connection after retries, a non-JSON or error-envelope 200, a
#' stats-info page that reaches `count` or falls short of its `totalCount`, or a body for another
#' game, team, athlete or ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
#' season that is not a year, or an argument outside its choices.
#'
#' @examples
#' \donttest{
#'   shl_team_upcoming_games(team_uuid = "087a-087aTQv9u")
#' }
#' @export
#' @family SHL Functions
shl_team_upcoming_games <- function(team_uuid, game_place = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/sports-v2/upcoming-games/", team_uuid),
    list(gamePlace = game_place)
  )
  if (!isTRUE(return_parsed)) {
    return(raw)
  }
  out <- .shl_parse_schedule(raw, .SHL_SCHEMAS[["team_upcoming_games"]])
  make_fastRhockey_data(out, "SHL Team Upcoming Games Information from www.shl.se", Sys.time())
}

#' SHL ssgt uuid of a season and game type
#'
#' The SHL keys a season by an "ssgt" uuid (season + series + game type). This maps a season
#' (END year) and game type to it through `/statistics-v2/layout-info`, fetched once per R
#' session. Use it for the routes that take an ssgt uuid directly ([shl_standings()],
#' [shl_sites()], [shl_ssgt_provider()], [shl_post_game_team_stats()], [shl_player_h2h()]).
#'
#' @param season Season END year (2026 = 2025/26).
#' @param game_type Game type: `"regular"` (Grundserie), `"playoff"` (Slutspel), `"qualdown"`
#'   (relegation qualification) or `"playin"` (2023/24 on).
#' @return A character string, the ssgt uuid (`"iuzqg7dqk9"` for the 2025/26 regular season).
#' @section Errors:
#' `fastRhockey_no_data`: layout-info lists no ssgt for that season and game type.
#' `fastRhockey_bad_request`: `season` is not a year. `fastRhockey_asset_fetch`: layout-info
#' could not be fetched or read.
#' @examples
#' \donttest{
#'   shl_ssgt_uuid(2026)
#'   shl_ssgt_uuid(2025, "playoff")
#' }
#' @export
#' @family SHL Functions
shl_ssgt_uuid <- function(season, game_type = "regular") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  unname(.shl_resolve(season, game_type)[["ssgt"]])
}

#' SHL player id crosswalk: isa id to athlete uuid
#'
#' The SHL has three player id systems: the **isa** (StatNet) id that play-by-play, box scores
#' and stats tables key on, the platform **athlete uuid** that rosters and athlete routes key
#' on, and the old `ramses_legacy` CMS id (athlete-details only). The `players_summary` table's
#' `players{}` map is keyed by isa and carries each player's uuid, skaters and goalies alike, so
#' one stats-info call per season gives the whole crosswalk.
#'
#' @param season Season END year (2026 = 2025/26).
#' @param game_type Game type: `"regular"`, `"playoff"`, `"qualdown"` or `"playin"`.
#' @return A `fastRhockey_data` tibble, one row per player, with the following columns (the
#'   API may add more, after these):
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | player_id | integer | isa (StatNet) player id: the map key. The id play-by-play, box scores and stats tables key on. |
#' | athlete_uuid | character | Platform athlete uuid (qQ9-... or 10-char form); joins to the athlete routes and the roster. Null where SHL has none, common before about 2005. |
#' | first_name | character | Given (first) name of the player. |
#' | last_name | character | Surname (family name) of the player. |
#' | jersey_number | integer | Jersey (shirt) number of the player. |
#' | position | character | Position as the players map carries it (F, D, GK or a Swedish label). |
#' | shoots | character | Shooting side (L / R; Left / Right). |
#' | full_name | character | Given name and surname. |
#' | date_of_birth | character | Date of birth (YYYY-MM-DD). |
#' | nationality | character | Nationality (ISO country code). |
#' | gender | character | Sex of the athlete (male / female). |
#' | weight | integer | Weight of the player in kilograms. |
#' | height | integer | Height of the player in centimetres. |
#' | ep_url | character | External profile URL of the player (empty in the sample). |
#'
#' @section Errors:
#' `fastRhockey_no_data`: layout-info lists no such ssgt, or the table has no rows.
#' `fastRhockey_asset_fetch`: the fetch failed or the table came back cut.
#' @examples
#' \donttest{
#'   xw <- shl_player_crosswalk(2026)
#'   xw[xw$player_id == 2427, ]
#' }
#' @export
#' @family SHL Functions
shl_player_crosswalk <- function(season, game_type = "regular") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .shl_check_choice("game_type", game_type, .SHL_GAME_TYPES)
  raw <- .shl_get(
    paste0(.SHL_BASE, "/statistics-v2/stats-info/players_summary"),
    list(season = season, gameType = game_type)
  )
  out <- .shl_parse_crosswalk(raw)
  make_fastRhockey_data(out, "SHL Player Crosswalk Information from www.shl.se", Sys.time())
}

