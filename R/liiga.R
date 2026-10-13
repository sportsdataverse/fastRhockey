# liiga.R -- Liiga (Finnish top league) wrappers over liiga.fi/api/v2 (keyless JSON).
#
# Mirrors sdv-py sportsdataverse/hockey/liiga/ (liiga_runtime.py, liiga_parsers.py,
# liiga_dtypes.py and the generated liiga.py): the same 23 wrappers, arguments, refusal
# rules and column names. Seasons are END years natively (2026 = 2025-26), so no mapping
# happens. Every request goes through .euro_get(), i.e. at least one second between two
# requests to liiga.fi on every attempt.
#
# Layout of this file: runtime (.liiga_get and its refusal rules), parsers (the sdv-py
# row/flatten/dtype rules, ported), dtype and column tables (generated from sdv-py's
# returns-schemas), then the 23 exported wrappers.

# ---- runtime ---------------------------------------------------------------------------

.LIIGA_HOST <- "https://liiga.fi/api/v2"

# The `tournament` values Liiga accepts; anything else answers HTTP 500.
.LIIGA_TOURNAMENTS <- c(
  "runkosarja", "playoffs", "playout", "qualifications", "valmistavat_ottelut", "chl"
)

# Key the runtime stamps the request under, where the body does not say which
# season / game / player it answers (see .liiga_stamp()).
.LIIGA_REQUEST_KEY <- "_request"

.LIIGA_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status")

.lg_is_obj <- function(x) is.list(x) && !is.null(names(x))
.lg_is_arr <- function(x) is.list(x) && is.null(names(x))

# dict.get(key, default): the default only when the key is missing, not when it is null.
.lg_get <- function(obj, key, default = NULL) {
  if (.lg_is_obj(obj) && key %in% names(obj)) obj[[key]] else default
}

# Type-strict equality (JSON `true` must not pass for game 1, a float must not pass for an int).
.lg_is_int <- function(value, want) {
  is.integer(value) && length(value) == 1L && !is.na(value) && value == want
}

# Add / overwrite entries of a named list, keeping NULL values (a key sent as null is a column).
.lg_put <- function(out, new) {
  for (nm in names(new)) out[nm] <- list(new[[nm]])
  out
}

.liiga_request_of <- function(url, query) {
  path <- httr2::url_parse(url)$path
  patterns <- list(
    list("/games/stats/(\\d+)/(\\d+)/?$", c("season", "game_id")),
    list("/games/(\\d+)/(\\d+)/?$", c("season", "game_id")),
    list("/shotmap/(\\d+)/(\\d+)/?$", c("season", "game_id")),
    list("/players/info/(\\d+)/games/(\\d+)/?$", c("player_id", "season"))
  )
  for (p in patterns) {
    m <- regmatches(path, regexec(p[[1]], path, perl = TRUE))[[1]]
    if (length(m)) {
      out <- as.list(as.integer(m[-1]))
      names(out) <- p[[2]]
      return(out)
    }
  }
  season <- query[["season"]]
  if (grepl("/standings$", sub("/+$", "", path)) && !is.null(season)) {
    return(list(season = as.integer(season)))
  }
  list()
}

# Add the request's ids under `_request` (on each row of a list body).
.liiga_stamp <- function(url, query, body) {
  req <- .liiga_request_of(url, query)
  if (!length(req)) return(body)
  if (.lg_is_obj(body)) {
    body[[.LIIGA_REQUEST_KEY]] <- req
  } else if (.lg_is_arr(body)) {
    for (i in seq_along(body)) {
      if (.lg_is_obj(body[[i]])) body[[i]][[.LIIGA_REQUEST_KEY]] <- req
    }
  }
  body
}

# Raise on a 200 whose body is one of Liiga's error shapes.
.liiga_error <- function(url, body) {
  where <- .euro_where(url)
  if (is.character(body)) {
    shown <- substr(body[[1]], 1, 80)
    .abort_asset_fetch("{where} answered an error string: {shown}")
  }
  if (!.lg_is_obj(body) || !length(body)) return(invisible())
  if (all(names(body) %in% .LIIGA_ERROR_KEYS)) {
    shown <- substr(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"), 1, 120)
    .abort_asset_fetch("{where} answered an error envelope: {shown}")
  }
  for (key in names(body)) {
    value <- body[[key]]
    if (is.character(value) && length(value) == 1L && grepl("\\berror\\b", value, ignore.case = TRUE, perl = TRUE)) {
      if (key == "stats") .abort_no_data("{where}: Liiga has no such game ({.val {value}})")
      .abort_asset_fetch("{where} answered an error body: {key} = {value}")
    }
    if (.lg_is_obj(value) && length(value) && all(names(value) %in% .LIIGA_ERROR_KEYS)) {
      shown <- substr(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"), 1, 120)
      .abort_asset_fetch("{where} answered an error envelope under {key}: {shown}")
    }
  }
  invisible()
}

# No rows: a scalar, a list without one non-empty object, an all-null object, or one whose lists are all empty.
.liiga_is_empty <- function(body) {
  if (.lg_is_arr(body)) return(!any(vapply(body, function(r) .lg_is_obj(r) && length(r) > 0, NA)))
  if (!.lg_is_obj(body)) return(TRUE)
  if (!length(body) || all(vapply(body, is.null, NA))) return(TRUE)
  lists <- Filter(.lg_is_arr, body)
  length(lists) > 0 && !any(vapply(lists, function(l) length(l) > 0, NA))
}

# a <= b on strings, by C-locale (byte) order, so ISO timestamps compare the same everywhere.
.lg_le <- function(a, b) order(c(a, b), method = "radix")[[1]] == 1L

# A body that names its season or game must be the one asked for.
.liiga_scope <- function(url, query, body) {
  where <- .euro_where(url)
  path <- httr2::url_parse(url)$path
  game <- regmatches(path, regexec("/games/(\\d+)/(\\d+)/?$", path, perl = TRUE))[[1]]
  if (length(game) && !grepl("/games/stats/", path, fixed = TRUE) && is.null(query[["dataType"]])) {
    want_season <- as.integer(game[[2]])
    want_game <- as.integer(game[[3]])
    g <- if (.lg_is_obj(body)) body[["game"]] else NULL
    if (!.lg_is_obj(g) || !(.lg_is_int(g[["season"]], want_season) && .lg_is_int(g[["id"]], want_game))) {
      got <- if (.lg_is_obj(g)) paste0("(", .lg_json(g[["season"]]), ", ", .lg_json(g[["id"]]), ")") else "none"
      .abort_asset_fetch("{where}: asked for game {want_season}/{want_game}, the body is {got}")
    }
    return(invisible())
  }
  season <- query[["season"]]
  if (!is.null(season) && (grepl("/(games|schedule)/?$", path) || grepl("/gameweeks$", sub("/+$", "", path)))) {
    rows <- if (.lg_is_arr(body)) body else list(body)
    bad <- Filter(function(r) !(.lg_is_obj(r) && .lg_is_int(r[["season"]], as.integer(season))), rows)
    if (length(bad)) {
      got <- if (.lg_is_obj(bad[[1]])) .lg_json(bad[[1]][["season"]]) else .lg_json(bad[[1]])
      .abort_asset_fetch("{where}: asked for season {season}, the body has season {got}")
    }
    return(invisible())
  }
  log <- regmatches(path, regexec("/players/info/\\d+/games/(\\d+)/?$", path, perl = TRUE))[[1]]
  if (length(log) && .lg_is_obj(body)) {
    end <- as.integer(log[[2]])
    lo <- sprintf("%d-07-01", end - 1L)
    hi <- sprintf("%d-06-30T23:59:59Z", end)
    for (rows in body) {
      if (!.lg_is_arr(rows)) next
      for (r in rows) {
        date <- if (.lg_is_obj(r)) r[["date"]] else NULL
        if (!is.character(date) || !(.lg_le(lo, date) && .lg_le(date, hi))) {
          shown <- if (is.character(date)) date else "none"
          .abort_asset_fetch("{where}: game date {shown} is outside season {end} ({lo}..{hi})")
        }
      }
    }
  }
  invisible()
}

# GET a Liiga route as JSON, refuse the error / empty / out-of-scope answers, stamp the request.
#   404 -> fastRhockey_no_data; 400 / 422 -> fastRhockey_bad_request; any other non-2xx
#   (a 500 at once, unretried), a dead connection, an empty / non-JSON / error-shaped 200,
#   or a body for another season or game -> fastRhockey_asset_fetch;
#   {"stats": "Remote server error"} or a body without rows -> fastRhockey_no_data.
#' @noRd
.liiga_get <- function(path, query = list()) {
  query <- Filter(Negate(is.null), query)
  tournament <- query[["tournament"]]
  if (!is.null(tournament) && !(length(tournament) == 1L && tournament %in% .LIIGA_TOURNAMENTS)) {
    valid <- .LIIGA_TOURNAMENTS
    .abort_bad_request("{.arg tournament} must be one of {.val {valid}}; got {.val {tournament}}.")
  }
  url <- paste0(.LIIGA_HOST, path)
  # a 500 is the provider's answer to a bad request, so only 429 / 503 are retried
  body <- .euro_get(url, query, simplifyVector = FALSE, retry_statuses = c(429L, 503L))
  .liiga_error(url, body)
  if (.liiga_is_empty(body)) {
    where <- .euro_where(url)
    .abort_no_data("{where}: Liiga answered no rows")
  }
  .liiga_scope(url, query, body)
  .liiga_stamp(url, query, body)
}

# ---- argument checks -------------------------------------------------------------------

.lg_int <- function(x, name, nullable = FALSE) {
  if (is.null(x) && nullable) return(NULL)
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x == round(x)
  if (!ok) .abort_bad_request("{.arg {name}} must be a single whole number, not {.val {x}}.", .envir = environment())
  as.integer(x)
}

.lg_choice <- function(x, choices, name) {
  if (!(is.character(x) && length(x) == 1L && x %in% choices)) {
    .abort_bad_request("{.arg {name}} must be one of {.val {choices}}; got {.val {x}}.", .envir = environment())
  }
  x
}

# ---- parsers: JSON serialisation (json.dumps(..., ensure_ascii=False) layout) -------------

.lg_jstr <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  esc <- c("\n" = "\\n", "\r" = "\\r", "\t" = "\\t", "\b" = "\\b", "\f" = "\\f")
  for (ch in names(esc)) s <- gsub(ch, esc[[ch]], s, fixed = TRUE)
  for (i in setdiff(1:31, c(8L, 9L, 10L, 12L, 13L))) {
    s <- gsub(intToUtf8(i), sprintf("\\u%04x", i), s, fixed = TRUE)
  }
  paste0("\"", s, "\"")
}

# A double the way Python prints a float: shortest round trip, whole numbers as `3.0`.
.lg_num <- function(x) {
  if (is.na(x)) return("null")
  if (x == round(x) && abs(x) < 1e16) return(paste0(formatC(x, format = "f", digits = 0), ".0"))
  for (d in 1:17) {
    s <- formatC(x, digits = d, format = "g")
    if (as.numeric(s) == x) break
  }
  s
}

.lg_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (.lg_is_obj(x)) {
      if (!length(x)) return("{}")
      return(paste0("{", paste0(vapply(names(x), .lg_jstr, ""), ": ", vapply(x, .lg_json, ""), collapse = ", "), "}"))
    }
    return(paste0("[", paste(vapply(x, .lg_json, ""), collapse = ", "), "]"))
  }
  if (is.logical(x)) return(if (is.na(x)) "null" else if (x) "true" else "false")
  if (is.character(x)) return(.lg_jstr(x))
  if (is.integer(x)) return(as.character(x))
  .lg_num(x)
}

# ---- parsers: names, ids, dtypes -------------------------------------------------------

# dl_utils.underscore
.lg_underscore <- function(word) {
  word <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", word, perl = TRUE)
  word <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", word, perl = TRUE)
  tolower(gsub("-", "_", word, fixed = TRUE))
}

# Wire keys that hold a team id, in any of its three forms.
.LG_TEAM_KEYS <- c(
  "teamId", "homeTeamId", "awayTeamId", "shootingTeamId", "blockingTeamId", "leftTeam", "rightTeam"
)
# Maps keyed by a team NAME ({"HIFK": [...]}), kept as JSON like the id-keyed ones.
.LG_NAME_KEYED <- "teamList"
# Per-team event lists of a game; their own frames, never columns of the game row.
.LG_EVENT_KEYS <- c("goalEvents", "penaltyEvents", "goalKeeperEvents", "goalKeeperChanges")
.LG_SIDES <- list(c("home", "homeTeam"), c("away", "awayTeam"))
.LG_STANDINGS_TABLES <- c(
  season = "runkosarja", playoffs = "playoffs", valmistavat_ottelut = "valmistavat_ottelut",
  playout = "playout", qualifications = "qualifications"
)
.LG_ID_COL <- "(^|_)id$"
.LG_FLOAT_COL <- "percent|per_game|expected|average|speed|distance|duration|hardest"
.LG_COUNT_COL <- "(^|_)(season|period|total|won|lost|goals|assists|points|shots|passes|saves|games|wins|losses|ranking)$"
.LG_THOUSANDS <- "^-?[0-9]{1,3}(,[0-9]{3})+$"

# An integer as R integer when it fits, else double (R has no int64).
.lg_as_int <- function(x) {
  x <- as.numeric(x)
  if (!is.na(x) && abs(x) <= .Machine$integer.max) as.integer(x) else x
}

# Normalise any of Liiga's three team-id forms to list(num, slug): "55786244:hpk", 55786244, "55786244".
.lg_split_team_id <- function(value) {
  none <- list(num = NULL, slug = NULL)
  if (is.logical(value)) return(none)
  if (is.numeric(value) && length(value) == 1L && !is.na(value) && value == round(value)) {
    return(list(num = .lg_as_int(value), slug = NULL))
  }
  if (is.character(value) && length(value) == 1L) {
    m <- regmatches(value, regexec("^\\s*([0-9]+)(?::(.*))?\\s*$", value, perl = TRUE))[[1]]
    if (length(m)) {
      slug <- if (length(m) >= 3L && nzchar(m[[3]])) m[[3]] else NULL
      return(list(num = .lg_as_int(m[[2]]), slug = slug))
    }
  }
  none
}

# `teamId` -> team_id (+ team_slug when the wire value carries one).
.lg_team <- function(name, value) {
  col <- if (grepl("_id$", name)) name else paste0(name, "_id")
  s <- .lg_split_team_id(value)
  out <- list()
  out[col] <- list(s$num)
  if (is.character(value) && length(value) == 1L && grepl(":", value, fixed = TRUE)) {
    out[paste0(substr(col, 1L, nchar(col) - 3L), "_slug")] <- list(s$slug)
  }
  out
}

.lg_is_map <- function(value) {
  .lg_is_obj(value) && length(value) > 0 && all(grepl("^[0-9]+(:|$)", names(value)))
}

# One flat row: nested objects as <parent>_<child>, lists and id maps as JSON.
.lg_flatten <- function(obj, prefix = "") {
  out <- list()
  keys <- names(obj)
  for (i in seq_along(obj)) {
    key <- keys[[i]]
    if (identical(key, .LIIGA_REQUEST_KEY)) next
    value <- obj[[i]]
    name <- paste0(prefix, .lg_underscore(key))
    if (key %in% .LG_TEAM_KEYS) {
      out <- .lg_put(out, .lg_team(name, value))
    } else if (.lg_is_arr(value) || .lg_is_map(value) || (key %in% .LG_NAME_KEYED && .lg_is_obj(value))) {
      out[name] <- list(.lg_json(value))
    } else if (.lg_is_obj(value)) {
      out <- .lg_put(out, .lg_flatten(value, paste0(name, "_")))
    } else {
      out[name] <- list(value)
    }
  }
  out
}

.lg_request <- function(obj) {
  req <- .lg_get(obj, .LIIGA_REQUEST_KEY)
  if (.lg_is_obj(req)) req else list()
}
.lg_obj <- function(value) if (.lg_is_obj(value)) value else list()
.lg_list <- function(value) if (.lg_is_arr(value)) Filter(.lg_is_obj, value) else list()
.lg_key <- function(value) {
  if ((is.numeric(value) || is.character(value)) && length(value) == 1L && !is.na(value)) as.character(value) else NULL
}

.lg_rename <- function(row, old, new) {
  names(row)[names(row) == old] <- new
  row
}

# Request ids first (season, game_id, player_id), never overriding the body.
.lg_stamp_row <- function(row, req) {
  head <- list()
  for (k in c("season", "game_id", "player_id")) {
    if (k %in% names(req) && !(k %in% names(row))) head[k] <- list(req[[k]])
  }
  c(head, row)
}

# A numeric string -> double (unparseable -> NA). A comma is a thousands separator when the
# string also has a `.` or every comma is followed by exactly three digits ("4,583" -> 4583);
# otherwise it is the decimal mark ("12,5" -> 12.5, "0,00" -> 0).
.lg_number <- function(x) {
  text <- trimws(x)
  grouped <- !is.na(text) & (grepl(".", text, fixed = TRUE) | grepl(.LG_THOUSANDS, text))
  fixed <- ifelse(grouped, gsub(",", "", text, fixed = TRUE), sub(",", ".", text, fixed = TRUE))
  ok <- !is.na(fixed) & grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", fixed)
  out <- rep(NA_real_, length(fixed))
  out[ok] <- as.numeric(fixed[ok])
  out
}

# dtype codes: i Int64 (R integer), f Float64 (double), b Boolean, s String.
.lg_dtype_for <- function(col, schema = NULL) {
  code <- if (!is.null(schema) && col %in% names(schema)) schema[[col]] else NA_character_
  if (!is.na(code)) return(code)
  if (grepl(.LG_ID_COL, col)) return("i")
  if (grepl(.LG_FLOAT_COL, col)) return("f")
  if (grepl(.LG_COUNT_COL, col)) return("i")
  "s"
}

.lg_code <- function(x) {
  if (is.integer(x)) "i" else if (is.double(x)) "f" else if (is.logical(x)) "b" else "s"
}

.lg_cast <- function(x, code) {
  switch(code,
    i = if (is.character(x)) .lg_as_int_vec(.lg_number(x)) else .lg_as_int_vec(as.numeric(x)),
    f = if (is.character(x)) .lg_number(x) else as.numeric(x),
    b = as.logical(x),
    s = as.character(x)
  )
}

.lg_as_int_vec <- function(x) {
  if (all(is.na(x) | abs(x) <= .Machine$integer.max)) as.integer(x) else x
}

# One column from the values of every row (NULL = null): the polars from_dicts inference.
.lg_column <- function(vals) {
  nn <- !vapply(vals, is.null, NA)
  v <- vals[nn]
  if (!length(v)) return(list(kind = "null", x = rep(NA, length(vals))))
  out_na <- function(type) { x <- rep(NA, length(vals)); mode(x) <- type; x }
  flat <- unlist(v, use.names = FALSE)
  if (is.character(flat)) {
    x <- out_na("character")
    x[nn] <- vapply(v, function(e) if (is.logical(e)) tolower(as.character(e)) else as.character(e), "")
    return(list(kind = "s", x = x))
  }
  if (is.logical(flat)) {
    x <- out_na("logical")
    x[nn] <- flat
    return(list(kind = "b", x = x))
  }
  # numeric: Int64 when every value is a JSON integer, else Float64
  is_int <- vapply(v, function(e) is.integer(e) || (is.double(e) && e == round(e) && abs(e) >= 2^31), NA)
  if (all(is_int)) {
    x <- rep(NA_real_, length(vals))
    x[nn] <- as.numeric(flat)
    return(list(kind = "i", x = .lg_as_int_vec(x)))
  }
  x <- rep(NA_real_, length(vals))
  x[nn] <- as.numeric(flat)
  list(kind = "f", x = x)
}

# Ids -> integer (lossless only); every non-null value must survive the cast and round-trip.
.lg_cast_ids <- function(cols, kinds) {
  for (col in names(cols)) {
    if (!grepl(.LG_ID_COL, col) || kinds[[col]] %in% c("i", "null", "b")) next
    x <- cols[[col]]
    if (kinds[[col]] == "s") {
      ok <- is.na(x) | grepl("^[+-]?[0-9]+$", x)
      if (!all(ok)) next
      num <- suppressWarnings(as.numeric(x))
      back <- ifelse(is.na(num), NA_character_, formatC(num, format = "f", digits = 0))
      if (!all(is.na(x) | x == back)) next
    } else {
      num <- x
      if (!all(is.na(num) | num == round(num))) next
    }
    cols[[col]] <- .lg_as_int_vec(num)
    kinds[[col]] <- "i"
  }
  list(cols = cols, kinds = kinds)
}

# Rows (named lists of scalars) -> typed columns; NULL when there are no rows.
.lg_frame <- function(rows) {
  rows <- Filter(length, rows)
  if (!length(rows)) return(NULL)
  names_all <- unique(unlist(lapply(rows, names), use.names = FALSE))
  built <- lapply(names_all, function(cn) .lg_column(lapply(rows, function(r) r[[cn]])))
  names(built) <- names_all
  cols <- lapply(built, `[[`, "x")
  kinds <- vapply(built, `[[`, "", "kind")
  ids <- .lg_cast_ids(cols, kinds)
  cols <- ids$cols
  kinds <- ids$kinds
  for (col in names(cols)) {
    if (kinds[[col]] == "null") {
      cols[[col]] <- .lg_cast(cols[[col]], .lg_dtype_for(col))
      next
    }
    if (!grepl(.LG_FLOAT_COL, col) || grepl(.LG_ID_COL, col)) next
    if (kinds[[col]] == "s") cols[[col]] <- .lg_number(cols[[col]])
    else if (kinds[[col]] == "i") cols[[col]] <- as.numeric(cols[[col]])
  }
  tibble::as_tibble(cols, .name_repair = "minimal")
}

# Apply the parser's dtype table and the documented columns: an all-null column (and a
# whole-number rate) takes the schema dtype; an empty frame keeps the documented columns
# and a missing one is added as nulls, all typed like a populated frame.
.lg_finish <- function(df, columns, parser) {
  schema <- .LG_DTYPES[[parser]]
  type_name <- c(i = "integer", f = "double", b = "logical", s = "character")
  if (!is.null(df) && length(schema)) {
    for (col in names(df)) {
      cur <- .lg_code(df[[col]])
      want <- if (col %in% names(schema)) .lg_dtype_for(col, schema) else cur
      if (cur != want && (all(is.na(df[[col]])) || (want == "f" && cur %in% c("i", "f")))) {
        df[[col]] <- .lg_cast(df[[col]], want)
      }
    }
  }
  if (length(columns)) {
    if (is.null(df)) {
      return(.empty_tibble(stats::setNames(
        type_name[vapply(names(columns), .lg_dtype_for, "", schema = schema)], names(columns)
      )))
    }
    for (col in setdiff(names(columns), names(df))) {
      df[[col]] <- .lg_cast(rep(NA, nrow(df)), .lg_dtype_for(col, schema))
    }
  }
  if (is.null(df)) tibble::tibble() else df
}

# ---- parsers -----------------------------------------------------------------------------

# Generic body -> rows: a list is rows; a dict whose values are all objects keyed by an id
# ("123" / "123:slug") is rows; a dict with one list-valued key (/teams/stats) is that list;
# any other dict is one row.
.lg_rows <- function(raw) {
  if (.lg_is_arr(raw)) return(.lg_list(raw))
  if (!.lg_is_obj(raw) || !length(raw)) return(list())
  body <- raw[names(raw) != .LIIGA_REQUEST_KEY]
  if (length(body) >= 2L && all(vapply(body, .lg_is_obj, NA)) && all(grepl("^[0-9]+(:|$)", names(body)))) {
    return(lapply(names(body), function(k) .lg_put(list(id = k), body[[k]])))
  }
  lists <- names(body)[vapply(body, .lg_is_arr, NA)]
  if (length(lists) == 1L &&
      all(!vapply(body[names(body) != lists], is.list, NA))) {
    return(.lg_list(body[[lists]]))
  }
  list(body)
}

.lg_parse_generic <- function(raw, columns, parser = "parse_liiga") {
  .lg_finish(.lg_frame(lapply(.lg_rows(raw), .lg_flatten)), columns, parser)
}

# A team block of a game as <prefix>_* columns (teamName -> <prefix>_name).
.lg_side <- function(prefix, block) {
  out <- list()
  for (key in names(block)) {
    if (key %in% .LG_EVENT_KEYS) next
    value <- block[[key]]
    if (key == "teamId") {
      out <- .lg_put(out, .lg_team(paste0(prefix, "_team_id"), value))
      next
    }
    name <- if (startsWith(key, "team") && grepl("^[A-Z]", substr(key, 5L, 5L))) {
      paste0(tolower(substr(key, 5L, 5L)), substr(key, 6L, nchar(key)))
    } else {
      key
    }
    one <- list()
    one[name] <- list(value)
    out <- .lg_put(out, .lg_flatten(one, paste0(prefix, "_")))
  }
  out
}

.lg_game_row <- function(game) {
  head <- game[!(names(game) %in% c("homeTeam", "awayTeam", "winningShotCompetitionEvents"))]
  row <- .lg_rename(.lg_flatten(head), "id", "game_id")
  for (s in .LG_SIDES) row <- .lg_put(row, .lg_side(s[[1]], .lg_obj(.lg_get(game, s[[2]]))))
  row
}

.lg_goal_rows <- function(game, req) {
  out <- list()
  base <- list(season = .lg_get(game, "season", .lg_get(req, "season")),
               game_id = .lg_get(game, "id", .lg_get(req, "game_id")))
  for (s in .LG_SIDES) {
    team <- .lg_obj(.lg_get(game, s[[2]]))
    tid <- .lg_team("team_id", .lg_get(team, "teamId"))
    for (ev in .lg_list(.lg_get(team, "goalEvents"))) {
      pop <- function(k) { v <- .lg_get(ev, k); ev[k] <<- NULL; v }
      scorer <- .lg_obj(pop("scorerPlayer"))
      ids <- pop("assistantPlayerIds")
      assists <- .lg_list(pop("assistantPlayers"))
      so_far <- .lg_obj(pop("assistsSoFarInSeason"))
      types <- pop("goalTypes")
      plus <- pop("plusPlayerIds")
      minus <- pop("minusPlayerIds")
      scorer_id <- .lg_get(ev, "scorerPlayerId", .lg_get(scorer, "playerId"))
      row <- c(base, list(side = s[[1]]))
      row <- .lg_put(row, tid)
      row <- .lg_put(row, .lg_flatten(ev[names(ev) != "scorerPlayerId"]))
      row["scorer_player_id"] <- list(scorer_id)
      row["scorer_first_name"] <- list(.lg_get(scorer, "firstName"))
      row["scorer_last_name"] <- list(.lg_get(scorer, "lastName"))
      ids <- if (.lg_is_arr(ids)) ids else lapply(assists, function(a) .lg_get(a, "playerId"))
      for (n in 1:2) {
        pid <- if (length(ids) >= n) ids[[n]] else NULL
        who <- list()
        if (!is.null(pid)) {
          hit <- Filter(function(a) identical(.lg_get(a, "playerId"), pid), assists)
          if (length(hit)) who <- hit[[1]]
        }
        row[paste0("assist", n, "_player_id")] <- list(pid)
        row[paste0("assist", n, "_first_name")] <- list(.lg_get(who, "firstName"))
        row[paste0("assist", n, "_last_name")] <- list(.lg_get(who, "lastName"))
        row[paste0("assist", n, "_season_assists")] <- list(
          if (is.null(pid)) NULL else .lg_get(so_far, as.character(pid))
        )
      }
      row["goal_types"] <- list(if (.lg_is_arr(types)) paste(vapply(types, as.character, ""), collapse = ",") else NULL)
      row["plus_jerseys"] <- list(plus)
      row["minus_jerseys"] <- list(minus)
      out[[length(out) + 1L]] <- row
    }
  }
  out
}

.lg_parse_games <- function(raw, columns) {
  .lg_finish(.lg_frame(lapply(.lg_list(raw), .lg_game_row)), columns, "parse_liiga_games")
}

.lg_parse_goals <- function(raw, columns) {
  rows <- unlist(lapply(.lg_list(raw), .lg_goal_rows, req = list()), recursive = FALSE)
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_goals")
}

.lg_parse_schedule <- function(raw, columns) {
  rows <- lapply(.lg_list(raw), function(r) .lg_rename(.lg_rename(.lg_flatten(r), "id", "game_id"), "serie", "serie_id"))
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_schedule")
}

# (game block, list(season, game_id)) of a /games/{season}/{id} body.
.lg_game_of <- function(raw) {
  game <- .lg_obj(.lg_get(raw, "game"))
  req <- .lg_request(raw)
  list(game = game, ids = list(season = .lg_get(game, "season", .lg_get(req, "season")),
                               game_id = .lg_get(game, "id", .lg_get(req, "game_id"))))
}

.lg_parse_game <- function(raw, columns) {
  g <- .lg_game_of(raw)$game
  .lg_finish(.lg_frame(if (length(g)) list(.lg_game_row(g)) else list()), columns, "parse_liiga_game")
}

.lg_parse_game_goals <- function(raw, columns) {
  g <- .lg_game_of(raw)
  rows <- if (length(g$game)) .lg_goal_rows(g$game, g$ids) else list()
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_game_goals")
}

.lg_parse_game_penalties <- function(raw, columns) {
  g <- .lg_game_of(raw)
  rows <- list()
  for (s in .LG_SIDES) {
    team <- .lg_obj(.lg_get(g$game, s[[2]]))
    tid <- .lg_team("team_id", .lg_get(team, "teamId"))
    for (ev in .lg_list(.lg_get(team, "penaltyEvents"))) {
      rows[[length(rows) + 1L]] <- .lg_put(.lg_put(c(g$ids, list(side = s[[1]])), tid), .lg_flatten(ev))
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_game_penalties")
}

.lg_parse_game_shootout <- function(raw, columns) {
  g <- .lg_game_of(raw)
  rows <- lapply(.lg_list(.lg_get(g$game, "winningShotCompetitionEvents")), function(ev) .lg_put(g$ids, .lg_flatten(ev)))
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_game_shootout")
}

.lg_parse_game_lineups <- function(raw, columns) {
  ids <- .lg_game_of(raw)$ids
  rows <- list()
  for (s in list(c("home", "homeTeamPlayers"), c("away", "awayTeamPlayers"))) {
    for (p in .lg_list(.lg_get(raw, s[[2]]))) {
      rows[[length(rows) + 1L]] <- .lg_put(c(ids, list(side = s[[1]])), .lg_rename(.lg_flatten(p), "id", "player_id"))
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_game_lineups")
}

.lg_parse_game_players_stats <- function(raw, columns) {
  req <- .lg_request(raw)
  ids <- list(season = .lg_get(req, "season"), game_id = .lg_get(req, "game_id"))
  rows <- list()
  for (s in list(c("home", "homeTeamPlayers"), c("away", "awayTeamPlayers"))) {
    for (p in .lg_list(.lg_get(raw, s[[2]]))) {
      row <- .lg_put(c(ids, list(side = s[[1]])),
                     .lg_rename(.lg_flatten(.lg_obj(.lg_get(p, "playerDTO"))), "id", "player_id"))
      rows[[length(rows) + 1L]] <- .lg_put(row, .lg_flatten(p[names(p) != "playerDTO"]))
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_game_players_stats")
}

.lg_parse_game_stats <- function(raw, columns) {
  req <- .lg_request(raw)
  ids <- list(season = .lg_get(req, "season"), game_id = .lg_get(req, "game_id"))
  rows <- list()
  for (s in .LG_SIDES) {
    for (tp in .lg_list(.lg_get(raw, s[[2]]))) {
      head <- .lg_put(c(ids, list(side = s[[1]])), .lg_team("team_id", .lg_get(tp, "teamId")))
      head["period"] <- list(.lg_get(tp, "period"))
      for (grp in list(list(FALSE, "periodPlayerStats"), list(TRUE, "goaliePeriodStats"))) {
        for (ps in .lg_list(.lg_get(tp, grp[[2]]))) {
          per <- .lg_obj(.lg_get(ps, "period"))
          stats <- per[names(per) != "period"]
          rest <- ps[!(names(ps) %in% c("period", "jerseyId", "playerId"))]
          row <- head
          row["player_id"] <- list(.lg_get(ps, "playerId"))
          row["jersey"] <- list(.lg_get(ps, "jerseyId"))
          row["is_goalie"] <- list(grp[[1]])
          row <- .lg_put(row, .lg_flatten(stats))
          rows[[length(rows) + 1L]] <- .lg_put(row, .lg_flatten(rest))
        }
      }
    }
  }
  df <- .lg_frame(rows)
  tracking <- .lg_frame(unlist(lapply(c("homeTeamGamePlayerStats", "awayTeamGamePlayerStats"), function(key) {
    lapply(.lg_list(.lg_get(raw, key)), .lg_flatten)
  }), recursive = FALSE))
  if (!is.null(df) && !is.null(tracking) && "player_id" %in% names(tracking)) {
    tracking <- tracking[!duplicated(tracking$player_id), , drop = FALSE]
    if (!identical(class(df$player_id), class(tracking$player_id))) {
      .abort_bad_request("player_id dtypes differ between the box score and the tracking rows.")
    }
    idx <- match(df$player_id, tracking$player_id)
    idx[is.na(df$player_id)] <- NA_integer_
    for (col in setdiff(names(tracking), "player_id")) {
      name <- if (col %in% names(df)) paste0(col, "_game") else col
      df[[name]] <- tracking[[col]][idx]
    }
  }
  .lg_finish(df, columns, "parse_liiga_game_stats")
}

.lg_parse_game_team_stats <- function(raw, columns) {
  req <- .lg_request(raw)
  ids <- list(season = .lg_get(req, "season"), game_id = .lg_get(req, "game_id"))
  puck <- list()
  for (p in .lg_list(.lg_get(raw, "puckStats"))) {
    k <- .lg_key(.lg_get(p, "periodNumber"))
    if (!is.null(k)) puck[[k]] <- p
  }
  rows <- list()
  for (s in .LG_SIDES) {
    other <- if (s[[1]] == "home") "away" else "home"
    for (tp in .lg_list(.lg_get(raw, s[[2]]))) {
      row <- c(ids, list(side = s[[1]]))
      row["period"] <- list(.lg_get(tp, "period"))
      row <- .lg_put(row, .lg_flatten(tp[!(names(tp) %in% c("periodPlayerStats", "goaliePeriodStats"))]))
      k <- .lg_key(.lg_get(tp, "period"))
      p <- if (!is.null(k) && k %in% names(puck)) puck[[k]] else list()
      row["puck_control_duration"] <- list(.lg_get(p, paste0(s[[1]], "TeamControlDuration")))
      row["opponent_puck_control_duration"] <- list(.lg_get(p, paste0(other, "TeamControlDuration")))
      row["contested_control_duration"] <- list(.lg_get(p, "contestedControlDuration"))
      rows[[length(rows) + 1L]] <- row
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_game_team_stats")
}

.lg_parse_shotmap <- function(raw, columns) {
  rows <- lapply(.lg_list(raw), function(r) .lg_stamp_row(.lg_flatten(r), .lg_request(r)))
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_shotmap")
}

.lg_parse_standings <- function(raw, columns) {
  req <- .lg_request(raw)
  rows <- list()
  for (key in names(.LG_STANDINGS_TABLES)) {
    for (r in .lg_list(.lg_get(raw, key))) {
      rows[[length(rows) + 1L]] <- .lg_put(
        list(season = .lg_get(req, "season"), tournament = .LG_STANDINGS_TABLES[[key]]), .lg_flatten(r)
      )
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_standings")
}

.lg_parse_teams <- function(raw, columns) {
  teams <- .lg_obj(.lg_get(raw, "teams"))
  rows <- list()
  for (key in names(teams)) {
    team <- teams[[key]]
    if (!.lg_is_obj(team)) next
    from_key <- .lg_split_team_id(key)
    flat <- .lg_flatten(team)
    flat <- flat[!(names(flat) %in% c("id", "slug"))]
    num <- .lg_split_team_id(.lg_get(team, "id"))$num
    if (is.null(num) || num == 0) num <- from_key$num
    slug <- .lg_get(team, "slug")
    if (is.null(slug) || identical(slug, "")) slug <- from_key$slug
    head <- list()
    head["team_id"] <- list(num)
    head["team_slug"] <- list(slug)
    rows[[length(rows) + 1L]] <- .lg_put(head, flat)
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_teams")
}

.lg_parse_players_info <- function(raw, columns) {
  rows <- lapply(.lg_list(raw), function(r) .lg_rename(.lg_flatten(r), "id", "player_id"))
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_players_info")
}

.lg_parse_player <- function(raw, columns) {
  body <- if (.lg_is_obj(raw)) raw[names(raw) != "historical"] else list()
  row <- .lg_rename(.lg_flatten(body), "fiha_id", "player_id")
  .lg_finish(.lg_frame(if (length(body)) list(row) else list()), columns, "parse_liiga_player")
}

.lg_parse_player_career <- function(raw, columns) {
  pid <- .lg_get(raw, "fihaId")
  rows <- list()
  for (v in .lg_obj(.lg_get(raw, "historical"))) {
    for (r in .lg_list(v)) {
      head <- list()
      head["player_id"] <- list(pid)
      rows[[length(rows) + 1L]] <- .lg_put(head, .lg_flatten(r))
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_player_career")
}

.lg_parse_player_game_log <- function(raw, columns) {
  req <- .lg_request(raw)
  ids <- list(player_id = .lg_get(req, "player_id"), season = .lg_get(req, "season"))
  rows <- list()
  if (.lg_is_obj(raw)) {
    for (key in setdiff(names(raw), .LIIGA_REQUEST_KEY)) {
      for (r in .lg_list(raw[[key]])) {
        rows[[length(rows) + 1L]] <- .lg_put(c(ids, list(tournament = key)), .lg_flatten(r))
      }
    }
  }
  .lg_finish(.lg_frame(rows), columns, "parse_liiga_player_game_log")
}

# ---- generated from sdv-py's returns-schemas (liiga_dtypes.py / liiga.py _PARSER_COLUMNS) ----

# Column dtype of each parser (i integer, f double, b logical, s character): an empty frame, a
# gap-filled column and an all-null column are typed from it, like the populated frame.
.LG_DTYPES <- list(
  parse_liiga = c(
    alumni_points = "i",
    assists = "i",
    away_games = "i",
    away_losses = "i",
    away_ties = "i",
    away_wins = "i",
    away_wins_in_extra_time = "i",
    cachebust = "b",
    contest_lost = "i",
    contest_won = "i",
    contest_won_percentage = "f",
    current = "b",
    even_strength_goals = "i",
    even_strength_goals_against = "i",
    expected_goals = "f",
    first_name = "s",
    game_week = "i",
    games = "i",
    goalkeeper = "b",
    goals = "i",
    goals_against = "i",
    goals_scored = "i",
    goals_to_empty_goal = "i",
    golden_helmets = "i",
    home_games = "i",
    home_losses = "i",
    home_ties = "i",
    home_wins = "i",
    home_wins_in_extra_time = "i",
    id = "i",
    injured = "b",
    is_u20 = "b",
    jersey = "i",
    last_name = "s",
    last_season = "i",
    league_id = "i",
    losses = "i",
    max_game_week = "i",
    minus = "i",
    nationality = "s",
    nb_weeks = "i",
    non_blocked_shots = "i",
    non_blocked_shots_against = "i",
    non_blocked_shots_against_even_strength = "i",
    non_blocked_shots_against_penalty_kill = "i",
    non_blocked_shots_against_powerplay = "i",
    non_blocked_shots_difference = "i",
    non_blocked_shots_difference_even_strength = "i",
    non_blocked_shots_difference_penalty_kill = "i",
    non_blocked_shots_difference_powerplay = "i",
    non_blocked_shots_even_strength = "i",
    non_blocked_shots_for_shots_against_percentage = "f",
    non_blocked_shots_for_shots_against_percentage_even_strength = "f",
    non_blocked_shots_for_shots_against_percentage_penalty_kill = "f",
    non_blocked_shots_for_shots_against_percentage_powerplay = "f",
    non_blocked_shots_penalty_kill = "i",
    non_blocked_shots_powerplay = "i",
    pdo = "f",
    pdo_even_strength = "f",
    pdo_penalty_kill = "f",
    pdo_powerplay = "f",
    penalty_kill_goals = "i",
    penalty_kill_goals_against = "i",
    penalty_minutes = "i",
    penaltykill_goals = "i",
    picture_url = "s",
    played_games = "i",
    player_id = "i",
    playoff_rank = "i",
    plus = "i",
    plus_minus = "i",
    points = "i",
    powerplay_goals = "i",
    powerplay_goals_against = "i",
    previous_game_day_goals = "i",
    previous_game_day_ranking = "i",
    previous_game_day_shots = "i",
    previous_game_day_shots_percentage = "f",
    previous_teams_for_tournament = "s",
    ranking = "i",
    regular_season_rank = "i",
    removed = "b",
    role = "s",
    rookie = "b",
    save_percentage = "f",
    save_percentage_even_strength = "f",
    save_percentage_penalty_kill = "f",
    save_percentage_powerplay = "f",
    season = "i",
    serie = "s",
    serie_id = "i",
    shot_percentage = "f",
    shots = "i",
    shots_against = "i",
    shots_against_even_strength = "i",
    shots_against_penalty_kill = "i",
    shots_against_powerplay = "i",
    shots_difference = "i",
    shots_difference_even_strength = "i",
    shots_difference_penalty_kill = "i",
    shots_difference_powerplay = "i",
    shots_even_strength = "i",
    shots_for_shots_against_percentage = "f",
    shots_for_shots_against_percentage_even_strength = "f",
    shots_for_shots_against_percentage_penalty_kill = "f",
    shots_for_shots_against_percentage_powerplay = "f",
    shots_into_goal = "i",
    shots_penalty_kill = "i",
    shots_percentage = "f",
    shots_percentage_even_strength = "f",
    shots_percentage_penalty_kill = "f",
    shots_percentage_powerplay = "f",
    shots_powerplay = "i",
    shots_saved = "i",
    sort_desc = "b",
    suspended = "b",
    team_id = "i",
    team_logos_dark_bg = "s",
    team_logos_dark_bg_original = "s",
    team_logos_light_bg = "s",
    team_logos_light_bg_original = "s",
    team_name = "s",
    team_short_name = "s",
    ties = "i",
    time_on_ice = "i",
    time_on_ice_avg = "f",
    tournament = "s",
    winning_goals = "i",
    wins = "i",
    wins_in_extra_time = "i",
    xge = "f"
  ),
  parse_liiga_game = c(
    away_expected_goals = "f",
    away_game_start_date_time = "s",
    away_goals = "i",
    away_logos_dark_bg = "s",
    away_logos_dark_bg_original = "s",
    away_logos_light_bg = "s",
    away_logos_light_bg_original = "s",
    away_name = "s",
    away_placeholder = "s",
    away_powerplay_goals = "i",
    away_powerplay_instances = "i",
    away_ranking = "i",
    away_short_handed_goals = "i",
    away_short_handed_instances = "i",
    away_team_id = "i",
    away_team_slug = "s",
    away_time_out = "i",
    current_period = "i",
    ended = "b",
    finished_type = "s",
    game_id = "i",
    game_time = "i",
    game_week = "i",
    home_expected_goals = "f",
    home_game_start_date_time = "s",
    home_goals = "i",
    home_logos_dark_bg = "s",
    home_logos_dark_bg_original = "s",
    home_logos_light_bg = "s",
    home_logos_light_bg_original = "s",
    home_name = "s",
    home_placeholder = "s",
    home_powerplay_goals = "i",
    home_powerplay_instances = "i",
    home_ranking = "i",
    home_short_handed_goals = "i",
    home_short_handed_instances = "i",
    home_team_id = "i",
    home_team_slug = "s",
    home_time_out = "i",
    ice_rink_city = "s",
    ice_rink_id = "i",
    ice_rink_latitude = "f",
    ice_rink_longitude = "f",
    ice_rink_name = "s",
    ice_rink_street_address = "s",
    ice_rink_zip = "s",
    periods = "s",
    referees = "s",
    season = "i",
    serie = "s",
    spectators = "i",
    stale = "b",
    start = "s",
    started = "b"
  ),
  parse_liiga_game_goals = c(
    assist1_first_name = "s",
    assist1_last_name = "s",
    assist1_player_id = "i",
    assist1_season_assists = "i",
    assist2_first_name = "s",
    assist2_last_name = "s",
    assist2_player_id = "i",
    assist2_season_assists = "i",
    away_team_score = "i",
    event_id = "i",
    game_id = "i",
    game_time = "i",
    goal_types = "s",
    goals_so_far_in_season = "i",
    home_team_score = "i",
    log_time = "s",
    minus_jerseys = "s",
    period = "i",
    plus_jerseys = "s",
    scorer_first_name = "s",
    scorer_last_name = "s",
    scorer_player_id = "i",
    season = "i",
    side = "s",
    team_id = "i",
    team_slug = "s",
    video_clip_playlist_url = "s",
    video_clip_url = "s",
    video_thumbnail_url = "s",
    winning_goal = "b"
  ),
  parse_liiga_game_lineups = c(
    alternate_captain = "b",
    awards = "s",
    captain = "b",
    country_of_birth = "s",
    date_of_birth = "s",
    extra_description = "s",
    extra_rookie_picture = "b",
    extra_strengths = "s",
    first_name = "s",
    game_id = "i",
    handedness = "s",
    height = "i",
    injured = "b",
    jersey = "i",
    last_name = "s",
    line = "i",
    nationality = "s",
    picture_url = "s",
    place_of_birth = "s",
    player_id = "i",
    removed = "b",
    role = "s",
    role_code = "s",
    rookie = "b",
    season = "i",
    side = "s",
    sponsors = "s",
    suspended = "b",
    team_id = "i",
    team_name = "s",
    team_slug = "s",
    weight = "i"
  ),
  parse_liiga_game_penalties = c(
    event_id = "i",
    game_id = "i",
    game_time = "i",
    log_time = "s",
    penalty_begintime = "i",
    penalty_endtime = "i",
    penalty_fault_name = "s",
    penalty_fault_type = "s",
    penalty_info = "s",
    penalty_minutes = "i",
    period = "i",
    player_id = "i",
    season = "i",
    side = "s",
    sufferer_player_id = "i",
    team_id = "i",
    team_slug = "s"
  ),
  parse_liiga_game_players_stats = c(
    all_time_stats_assists = "i",
    all_time_stats_games = "i",
    all_time_stats_goals = "i",
    all_time_stats_minus = "i",
    all_time_stats_penalty_minutes = "i",
    all_time_stats_plus = "i",
    all_time_stats_plus_minus = "i",
    all_time_stats_points = "i",
    all_time_stats_shot_percentage = "f",
    all_time_stats_shots = "i",
    alternate_captain = "b",
    awards = "s",
    captain = "b",
    country_of_birth = "s",
    date_of_birth = "s",
    extra_description = "s",
    extra_rookie_picture = "b",
    extra_strengths = "s",
    first_name = "s",
    game_id = "i",
    handedness = "s",
    height = "i",
    injured = "b",
    jersey = "i",
    last_name = "s",
    line = "i",
    nationality = "s",
    picture_url = "s",
    place_of_birth = "s",
    player_id = "i",
    removed = "b",
    role = "s",
    role_code = "s",
    rookie = "b",
    season = "i",
    season_stats_assists = "i",
    season_stats_games = "i",
    season_stats_goals = "i",
    season_stats_in_team_assists = "i",
    season_stats_in_team_games = "i",
    season_stats_in_team_goals = "i",
    season_stats_in_team_minus = "i",
    season_stats_in_team_penalty_minutes = "i",
    season_stats_in_team_plus = "i",
    season_stats_in_team_plus_minus = "i",
    season_stats_in_team_points = "i",
    season_stats_in_team_shot_percentage = "f",
    season_stats_in_team_shots = "i",
    season_stats_minus = "i",
    season_stats_penalty_minutes = "i",
    season_stats_plus = "i",
    season_stats_plus_minus = "i",
    season_stats_points = "i",
    season_stats_shot_percentage = "f",
    season_stats_shots = "i",
    side = "s",
    sponsors = "s",
    suspended = "b",
    team_id = "i",
    team_name = "s",
    team_slug = "s",
    vs_team_stats_assists = "i",
    vs_team_stats_games = "i",
    vs_team_stats_goals = "i",
    vs_team_stats_minus = "i",
    vs_team_stats_penalty_minutes = "i",
    vs_team_stats_plus = "i",
    vs_team_stats_plus_minus = "i",
    vs_team_stats_points = "i",
    vs_team_stats_shot_percentage = "f",
    vs_team_stats_shots = "i",
    weight = "i"
  ),
  parse_liiga_game_shootout = c(
    blocking_player_id = "i",
    blocking_team_id = "i",
    blocking_team_slug = "s",
    game_id = "i",
    goal_scored = "b",
    season = "i",
    shooting_player_id = "i",
    shooting_team_id = "i",
    shooting_team_slug = "s",
    shot_number = "i",
    winning_goal = "b"
  ),
  parse_liiga_game_stats = c(
    assists = "i",
    blocked_shots = "i",
    corsi_against = "i",
    corsi_for = "i",
    distance = "f",
    expected_goals_against = "f",
    expected_goals_against_shot_on_goal = "f",
    expected_goals_player = "f",
    expected_goals_team = "f",
    faceoffs_center_total = "i",
    faceoffs_center_won = "i",
    faceoffs_defence_total = "i",
    faceoffs_defence_won = "i",
    faceoffs_offence_total = "i",
    faceoffs_offence_won = "i",
    faceoffs_total = "i",
    faceoffs_won = "i",
    fs_team_goals = "i",
    fs_team_goals_against = "i",
    fs_team_shots = "i",
    fs_team_shots_against = "i",
    fs_zone_starts_dz = "i",
    fs_zone_starts_oz = "i",
    game_id = "i",
    goals = "i",
    goals_allowed = "i",
    goals_to_empty_goal = "i",
    hardest_shot = "f",
    is_goalie = "b",
    jersey = "i",
    minus = "i",
    penaltykill2_goals = "i",
    penaltykill_assists = "i",
    penaltyminutes = "i",
    period = "i",
    player_id = "i",
    player_passes_under_high_pressure = "i",
    player_passes_under_pressure = "i",
    player_successful_passes_under_high_pressure = "i",
    player_successful_passes_under_pressure = "i",
    plus = "i",
    plusminus = "i",
    points = "i",
    powerplay2_goals = "i",
    powerplay_assists = "i",
    powerplay_goals = "i",
    saves = "i",
    saves_percentage = "f",
    season = "i",
    short_handed_goals = "i",
    shots = "i",
    shots_on_goal = "i",
    side = "s",
    successful_passes = "i",
    team_id = "i",
    team_slug = "s",
    timeofice = "i",
    top_speed = "f",
    total_passes = "i",
    winning_goal = "b"
  ),
  parse_liiga_game_team_stats = c(
    contested_control_duration = "f",
    face_off_wins = "i",
    five_minute_penalties = "i",
    game_id = "i",
    goals = "i",
    opponent_puck_control_duration = "f",
    penalty_minutes = "i",
    period = "i",
    power_play_goals = "i",
    power_play_instances = "s",
    puck_control_duration = "f",
    season = "i",
    short_handed_goals_against = "i",
    short_handed_instances = "s",
    shots = "i",
    side = "s",
    team_id = "i",
    team_slug = "s",
    ten_minute_penalties = "i",
    total_distance_travelled = "f",
    twenty_minute_penalties = "i",
    two_minute_penalties = "i"
  ),
  parse_liiga_games = c(
    away_expected_goals = "f",
    away_game_start_date_time = "s",
    away_goals = "i",
    away_logos_dark_bg = "s",
    away_logos_dark_bg_original = "s",
    away_logos_light_bg = "s",
    away_logos_light_bg_original = "s",
    away_name = "s",
    away_placeholder = "s",
    away_powerplay_goals = "i",
    away_powerplay_instances = "i",
    away_ranking = "i",
    away_short_handed_goals = "i",
    away_short_handed_instances = "i",
    away_team_id = "i",
    away_team_slug = "s",
    away_time_out = "i",
    buy_tickets_url = "s",
    current_period = "i",
    end = "s",
    ended = "b",
    finished_type = "s",
    game_id = "i",
    game_time = "i",
    game_week = "i",
    home_expected_goals = "f",
    home_game_start_date_time = "s",
    home_goals = "i",
    home_logos_dark_bg = "s",
    home_logos_dark_bg_original = "s",
    home_logos_light_bg = "s",
    home_logos_light_bg_original = "s",
    home_name = "s",
    home_placeholder = "s",
    home_powerplay_goals = "i",
    home_powerplay_instances = "i",
    home_ranking = "i",
    home_short_handed_goals = "i",
    home_short_handed_instances = "i",
    home_team_id = "i",
    home_team_slug = "s",
    home_time_out = "i",
    ice_rink_city = "s",
    ice_rink_id = "i",
    ice_rink_latitude = "f",
    ice_rink_longitude = "f",
    ice_rink_name = "s",
    ice_rink_street_address = "s",
    ice_rink_zip = "s",
    periods = "s",
    play_off_pair = "i",
    play_off_phase = "i",
    play_off_req_wins = "i",
    season = "i",
    serie = "s",
    spectators = "i",
    stale = "b",
    start = "s",
    started = "b"
  ),
  parse_liiga_goals = c(
    assist1_first_name = "s",
    assist1_last_name = "s",
    assist1_player_id = "i",
    assist1_season_assists = "i",
    assist2_first_name = "s",
    assist2_last_name = "s",
    assist2_player_id = "i",
    assist2_season_assists = "i",
    away_team_score = "i",
    event_id = "i",
    game_id = "i",
    game_time = "i",
    goal_types = "s",
    goals_so_far_in_season = "i",
    home_team_score = "i",
    log_time = "s",
    minus_jerseys = "s",
    period = "i",
    plus_jerseys = "s",
    scorer_first_name = "s",
    scorer_last_name = "s",
    scorer_player_id = "i",
    season = "i",
    side = "s",
    team_id = "i",
    team_slug = "s",
    video_clip_playlist_url = "s",
    video_clip_url = "s",
    video_thumbnail_url = "s",
    winning_goal = "b"
  ),
  parse_liiga_player = c(
    active_seasons = "s",
    birth_locality_country_code = "s",
    birth_locality_country_name = "s",
    birth_locality_name = "s",
    date_of_birth = "s",
    first_name = "s",
    handedness = "s",
    height = "i",
    is_removed = "b",
    is_suspended = "b",
    last_name = "s",
    nationality_code = "s",
    nationality_name = "s",
    player_id = "i",
    team_list = "s",
    team_seasons = "s",
    teams = "s",
    weight = "i"
  ),
  parse_liiga_player_career = c(
    assists = "i",
    end_time = "s",
    faceoff_total = "i",
    faceoffs_percentage = "f",
    faceoffs_won = "i",
    games = "i",
    gk_losses = "i",
    gk_ties = "i",
    gk_wins = "i",
    goals = "i",
    goals_against = "i",
    goals_against_average = "f",
    league_name = "s",
    losses = "i",
    minus = "i",
    penalty_minutes = "i",
    played_games = "i",
    player_id = "i",
    plus = "i",
    plus_minus = "i",
    points = "i",
    power_play_goals = "i",
    save_percentage = "f",
    saves = "i",
    season = "i",
    short_handed_goals = "i",
    shot_into_goal_percentage = "f",
    shot_percentage = "f",
    shots = "i",
    shots_into_goal = "i",
    shut_out = "s",
    team_name = "s",
    team_short_name = "s",
    ties = "i",
    time_on_ice = "i",
    tournament = "s",
    win_percentage = "f",
    winning_goals = "i",
    wins = "i"
  ),
  parse_liiga_player_game_log = c(
    assists = "i",
    away_team_name = "s",
    block_or_saved_shots_percentage = "f",
    blocked_or_saved_shots = "i",
    date = "s",
    faceoffs_percentage = "f",
    faceoffs_total = "i",
    faceoffs_won = "i",
    game_id = "i",
    game_str = "s",
    goalkeeper = "b",
    goals = "i",
    goals_against = "i",
    home_team_name = "s",
    lost = "i",
    minus = "i",
    penalty_minutes = "i",
    penaltykill_assists = "i",
    penaltykill_goals = "i",
    penaltykill_points = "i",
    player_id = "i",
    plus = "i",
    plus_minus = "i",
    powerplay_assists = "i",
    powerplay_goals = "i",
    powerplay_seconds = "i",
    save_percentage = "f",
    saves = "i",
    season = "i",
    shot_into_goal_percentage = "f",
    shot_percentage = "f",
    shots = "i",
    shots_into_goal = "i",
    shots_saved = "i",
    shots_saved_by_skater = "i",
    shut_out = "i",
    tied = "i",
    time_on_ice = "i",
    total_points = "i",
    tournament = "s",
    winning_goals = "i",
    won = "i"
  ),
  parse_liiga_players_info = c(
    alternate_captain = "b",
    captain = "b",
    country_of_birth = "s",
    date_of_birth = "s",
    first_name = "s",
    handedness = "s",
    height = "i",
    injured = "b",
    jersey = "i",
    last_name = "s",
    nationality = "s",
    picture_url = "s",
    place_of_birth = "s",
    player_id = "i",
    removed = "b",
    role = "s",
    role_code = "s",
    rookie = "b",
    suspended = "b",
    team_id = "i",
    team_name = "s",
    team_slug = "s",
    weight = "i"
  ),
  parse_liiga_schedule = c(
    away_team_goals = "i",
    away_team_id = "i",
    away_team_name = "s",
    away_team_placeholder = "s",
    away_team_ranking = "i",
    away_team_slug = "s",
    buy_tickets_url = "s",
    ended = "b",
    expected_away_team_goals = "f",
    expected_home_team_goals = "f",
    finished_type = "s",
    game_id = "i",
    game_time = "i",
    home_team_goals = "i",
    home_team_id = "i",
    home_team_name = "s",
    home_team_placeholder = "s",
    home_team_ranking = "i",
    home_team_slug = "s",
    home_team_winner = "b",
    ice_rink_city = "s",
    ice_rink_id = "i",
    ice_rink_latitude = "f",
    ice_rink_longitude = "f",
    ice_rink_name = "s",
    ice_rink_street_address = "s",
    ice_rink_zip = "s",
    play_off_pair = "i",
    play_off_phase = "i",
    play_off_req_wins = "i",
    season = "i",
    serie_id = "i",
    spectators = "i",
    start = "s",
    started = "b"
  ),
  parse_liiga_shotmap = c(
    blocker_id = "i",
    event_type = "s",
    game_id = "i",
    game_time = "i",
    left_team_id = "i",
    other_team_players_on_ice = "i",
    own_team_players_on_ice = "i",
    period = "i",
    right_team_id = "i",
    season = "i",
    shooter_id = "i",
    shooting_team_id = "i",
    shot_x = "i",
    shot_y = "i",
    type = "s"
  ),
  parse_liiga_standings = c(
    distance = "f",
    distance_per_game = "f",
    five_minute_penalties = "i",
    games = "i",
    goals = "i",
    goals_against = "i",
    internal_id = "i",
    live_games = "i",
    live_goals = "i",
    live_goals_against = "i",
    live_losses = "i",
    live_points = "i",
    live_points_per_game = "f",
    live_ranking = "i",
    live_ties = "i",
    live_wins = "i",
    losses = "i",
    overtime_losses = "i",
    overtime_wins = "i",
    penalty_minutes = "i",
    points = "i",
    points_per_game = "f",
    power_play_goals = "i",
    power_play_instances = "i",
    power_play_percentage = "f",
    power_play_time = "i",
    ranking = "i",
    season = "i",
    serie = "s",
    short_handed_goals_against = "i",
    short_handed_instances = "i",
    short_handed_percentage = "f",
    short_handed_time = "i",
    team_id = "i",
    team_logos_dark_bg = "s",
    team_logos_dark_bg_original = "s",
    team_logos_light_bg = "s",
    team_logos_light_bg_original = "s",
    team_name = "s",
    team_slug = "s",
    ten_minute_penalties = "i",
    ties = "i",
    total_penalties = "i",
    tournament = "s",
    twenty_five_minute_penalties = "i",
    twenty_minute_penalties = "i",
    two_minute_penalties = "i",
    win_percentage = "f",
    wins = "i"
  ),
  parse_liiga_teams = c(
    contact_info = "s",
    country_code = "s",
    country_name = "s",
    current_venue_capacity = "i",
    externals = "s",
    general_info = "s",
    locality = "s",
    logo = "s",
    name = "s",
    short_name = "s",
    team_id = "i",
    team_slug = "s",
    teamtournamentstats = "s",
    url = "s"
  )
)

# Documented columns of each wrapper (its returns-schema): name -> R type.
.LG_COLS <- list(
  tournament = c(
    cachebust = "logical",
    game_week = "integer",
    max_game_week = "integer"
  ),
  games = c(
    game_id = "integer",
    season = "integer",
    start = "character",
    end = "character",
    periods = "character",
    finished_type = "character",
    started = "logical",
    ended = "logical",
    game_time = "integer",
    spectators = "integer",
    play_off_pair = "integer",
    play_off_phase = "integer",
    play_off_req_wins = "integer",
    buy_tickets_url = "character",
    ice_rink_id = "integer",
    ice_rink_name = "character",
    ice_rink_latitude = "double",
    ice_rink_longitude = "double",
    ice_rink_street_address = "character",
    ice_rink_zip = "character",
    ice_rink_city = "character",
    current_period = "integer",
    stale = "logical",
    serie = "character",
    game_week = "integer",
    home_team_id = "integer",
    home_team_slug = "character",
    home_placeholder = "character",
    home_name = "character",
    home_goals = "integer",
    home_time_out = "integer",
    home_powerplay_instances = "integer",
    home_powerplay_goals = "integer",
    home_short_handed_instances = "integer",
    home_short_handed_goals = "integer",
    home_expected_goals = "double",
    home_ranking = "integer",
    home_game_start_date_time = "character",
    home_logos_dark_bg = "character",
    home_logos_light_bg = "character",
    home_logos_dark_bg_original = "character",
    home_logos_light_bg_original = "character",
    away_team_id = "integer",
    away_team_slug = "character",
    away_placeholder = "character",
    away_name = "character",
    away_goals = "integer",
    away_time_out = "integer",
    away_powerplay_instances = "integer",
    away_powerplay_goals = "integer",
    away_short_handed_instances = "integer",
    away_short_handed_goals = "integer",
    away_expected_goals = "double",
    away_ranking = "integer",
    away_game_start_date_time = "character",
    away_logos_dark_bg = "character",
    away_logos_light_bg = "character",
    away_logos_dark_bg_original = "character",
    away_logos_light_bg_original = "character"
  ),
  goals = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    team_id = "integer",
    team_slug = "character",
    log_time = "character",
    winning_goal = "logical",
    game_time = "integer",
    period = "integer",
    event_id = "integer",
    home_team_score = "integer",
    away_team_score = "integer",
    goals_so_far_in_season = "integer",
    video_clip_url = "character",
    video_clip_playlist_url = "character",
    video_thumbnail_url = "character",
    scorer_player_id = "integer",
    scorer_first_name = "character",
    scorer_last_name = "character",
    assist1_player_id = "integer",
    assist1_first_name = "character",
    assist1_last_name = "character",
    assist1_season_assists = "integer",
    assist2_player_id = "integer",
    assist2_first_name = "character",
    assist2_last_name = "character",
    assist2_season_assists = "integer",
    goal_types = "character",
    plus_jerseys = "character",
    minus_jerseys = "character"
  ),
  schedule = c(
    game_id = "integer",
    serie_id = "integer",
    season = "integer",
    home_team_name = "character",
    away_team_name = "character",
    home_team_id = "integer",
    home_team_slug = "character",
    away_team_id = "integer",
    away_team_slug = "character",
    home_team_placeholder = "character",
    away_team_placeholder = "character",
    home_team_goals = "integer",
    away_team_goals = "integer",
    expected_home_team_goals = "double",
    expected_away_team_goals = "double",
    game_time = "integer",
    started = "logical",
    ended = "logical",
    home_team_winner = "logical",
    spectators = "integer",
    home_team_ranking = "integer",
    away_team_ranking = "integer",
    play_off_pair = "integer",
    play_off_phase = "integer",
    play_off_req_wins = "integer",
    start = "character",
    buy_tickets_url = "character",
    finished_type = "character",
    ice_rink_id = "integer",
    ice_rink_name = "character",
    ice_rink_latitude = "double",
    ice_rink_longitude = "double",
    ice_rink_street_address = "character",
    ice_rink_zip = "character",
    ice_rink_city = "character"
  ),
  gameweeks = c(
    season = "integer",
    serie = "character",
    nb_weeks = "integer"
  ),
  game = c(
    game_id = "integer",
    season = "integer",
    start = "character",
    periods = "character",
    finished_type = "character",
    started = "logical",
    ended = "logical",
    game_time = "integer",
    spectators = "integer",
    ice_rink_id = "integer",
    ice_rink_name = "character",
    ice_rink_latitude = "double",
    ice_rink_longitude = "double",
    ice_rink_street_address = "character",
    ice_rink_zip = "character",
    ice_rink_city = "character",
    current_period = "integer",
    referees = "character",
    stale = "logical",
    serie = "character",
    game_week = "integer",
    home_team_id = "integer",
    home_team_slug = "character",
    home_placeholder = "character",
    home_name = "character",
    home_goals = "integer",
    home_time_out = "integer",
    home_powerplay_instances = "integer",
    home_powerplay_goals = "integer",
    home_short_handed_instances = "integer",
    home_short_handed_goals = "integer",
    home_expected_goals = "double",
    home_ranking = "integer",
    home_game_start_date_time = "character",
    home_logos_dark_bg = "character",
    home_logos_light_bg = "character",
    home_logos_dark_bg_original = "character",
    home_logos_light_bg_original = "character",
    away_team_id = "integer",
    away_team_slug = "character",
    away_placeholder = "character",
    away_name = "character",
    away_goals = "integer",
    away_time_out = "integer",
    away_powerplay_instances = "integer",
    away_powerplay_goals = "integer",
    away_short_handed_instances = "integer",
    away_short_handed_goals = "integer",
    away_expected_goals = "double",
    away_ranking = "integer",
    away_game_start_date_time = "character",
    away_logos_dark_bg = "character",
    away_logos_light_bg = "character",
    away_logos_dark_bg_original = "character",
    away_logos_light_bg_original = "character"
  ),
  game_goals = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    team_id = "integer",
    team_slug = "character",
    log_time = "character",
    winning_goal = "logical",
    game_time = "integer",
    period = "integer",
    event_id = "integer",
    home_team_score = "integer",
    away_team_score = "integer",
    goals_so_far_in_season = "integer",
    video_clip_url = "character",
    video_clip_playlist_url = "character",
    video_thumbnail_url = "character",
    scorer_player_id = "integer",
    scorer_first_name = "character",
    scorer_last_name = "character",
    assist1_player_id = "integer",
    assist1_first_name = "character",
    assist1_last_name = "character",
    assist1_season_assists = "integer",
    assist2_player_id = "integer",
    assist2_first_name = "character",
    assist2_last_name = "character",
    assist2_season_assists = "integer",
    goal_types = "character",
    plus_jerseys = "character",
    minus_jerseys = "character"
  ),
  game_penalties = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    team_id = "integer",
    team_slug = "character",
    player_id = "integer",
    sufferer_player_id = "integer",
    event_id = "integer",
    log_time = "character",
    game_time = "integer",
    period = "integer",
    penalty_begintime = "integer",
    penalty_endtime = "integer",
    penalty_fault_name = "character",
    penalty_fault_type = "character",
    penalty_info = "character",
    penalty_minutes = "integer"
  ),
  game_shootout = c(
    season = "integer",
    game_id = "integer",
    shooting_player_id = "integer",
    blocking_player_id = "integer",
    shot_number = "integer",
    shooting_team_id = "integer",
    shooting_team_slug = "character",
    blocking_team_id = "integer",
    blocking_team_slug = "character",
    goal_scored = "logical",
    winning_goal = "logical"
  ),
  game_lineups = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    player_id = "integer",
    team_id = "integer",
    team_slug = "character",
    team_name = "character",
    line = "integer",
    country_of_birth = "character",
    place_of_birth = "character",
    date_of_birth = "character",
    nationality = "character",
    first_name = "character",
    last_name = "character",
    role = "character",
    role_code = "character",
    handedness = "character",
    height = "integer",
    weight = "integer",
    captain = "logical",
    rookie = "logical",
    alternate_captain = "logical",
    jersey = "integer",
    picture_url = "character",
    injured = "logical",
    suspended = "logical",
    removed = "logical",
    awards = "character",
    sponsors = "character",
    extra_description = "character",
    extra_strengths = "character",
    extra_rookie_picture = "logical"
  ),
  game_players_stats = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    player_id = "integer",
    team_id = "integer",
    team_slug = "character",
    team_name = "character",
    line = "integer",
    country_of_birth = "character",
    place_of_birth = "character",
    date_of_birth = "character",
    nationality = "character",
    first_name = "character",
    last_name = "character",
    role = "character",
    role_code = "character",
    handedness = "character",
    height = "integer",
    weight = "integer",
    captain = "logical",
    rookie = "logical",
    alternate_captain = "logical",
    jersey = "integer",
    picture_url = "character",
    injured = "logical",
    suspended = "logical",
    removed = "logical",
    awards = "character",
    sponsors = "character",
    extra_description = "character",
    extra_strengths = "character",
    extra_rookie_picture = "logical",
    all_time_stats_games = "integer",
    all_time_stats_goals = "integer",
    all_time_stats_assists = "integer",
    all_time_stats_points = "integer",
    all_time_stats_plus = "integer",
    all_time_stats_minus = "integer",
    all_time_stats_plus_minus = "integer",
    all_time_stats_shots = "integer",
    all_time_stats_shot_percentage = "double",
    all_time_stats_penalty_minutes = "integer",
    season_stats_games = "integer",
    season_stats_goals = "integer",
    season_stats_assists = "integer",
    season_stats_points = "integer",
    season_stats_plus = "integer",
    season_stats_minus = "integer",
    season_stats_plus_minus = "integer",
    season_stats_shots = "integer",
    season_stats_shot_percentage = "double",
    season_stats_penalty_minutes = "integer",
    season_stats_in_team_games = "integer",
    season_stats_in_team_goals = "integer",
    season_stats_in_team_assists = "integer",
    season_stats_in_team_points = "integer",
    season_stats_in_team_plus = "integer",
    season_stats_in_team_minus = "integer",
    season_stats_in_team_plus_minus = "integer",
    season_stats_in_team_shots = "integer",
    season_stats_in_team_shot_percentage = "double",
    season_stats_in_team_penalty_minutes = "integer",
    vs_team_stats_games = "integer",
    vs_team_stats_goals = "integer",
    vs_team_stats_assists = "integer",
    vs_team_stats_points = "integer",
    vs_team_stats_plus = "integer",
    vs_team_stats_minus = "integer",
    vs_team_stats_plus_minus = "integer",
    vs_team_stats_shots = "integer",
    vs_team_stats_shot_percentage = "double",
    vs_team_stats_penalty_minutes = "integer"
  ),
  game_stats = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    team_id = "integer",
    team_slug = "character",
    period = "integer",
    player_id = "integer",
    jersey = "integer",
    is_goalie = "logical",
    points = "integer",
    assists = "integer",
    goals = "integer",
    plusminus = "integer",
    plus = "integer",
    minus = "integer",
    shots = "integer",
    penaltyminutes = "integer",
    timeofice = "integer",
    powerplay_goals = "integer",
    short_handed_goals = "integer",
    winning_goal = "logical",
    blocked_shots = "integer",
    faceoffs_total = "integer",
    faceoffs_won = "integer",
    corsi_for = "integer",
    corsi_against = "integer",
    faceoffs_center_total = "integer",
    faceoffs_center_won = "integer",
    faceoffs_defence_total = "integer",
    faceoffs_defence_won = "integer",
    faceoffs_offence_total = "integer",
    faceoffs_offence_won = "integer",
    fs_zone_starts_oz = "integer",
    fs_zone_starts_dz = "integer",
    powerplay2_goals = "integer",
    penaltykill2_goals = "integer",
    powerplay_assists = "integer",
    penaltykill_assists = "integer",
    goals_to_empty_goal = "integer",
    fs_team_shots = "integer",
    fs_team_goals = "integer",
    fs_team_shots_against = "integer",
    fs_team_goals_against = "integer",
    distance = "double",
    total_passes = "integer",
    successful_passes = "integer",
    player_passes_under_pressure = "integer",
    player_successful_passes_under_pressure = "integer",
    player_passes_under_high_pressure = "integer",
    player_successful_passes_under_high_pressure = "integer",
    expected_goals_player = "double",
    expected_goals_team = "double",
    expected_goals_against = "double",
    expected_goals_against_shot_on_goal = "double",
    saves = "integer",
    goals_allowed = "integer",
    saves_percentage = "double",
    shots_on_goal = "integer",
    top_speed = "double",
    hardest_shot = "double"
  ),
  game_team_stats = c(
    season = "integer",
    game_id = "integer",
    side = "character",
    period = "integer",
    team_id = "integer",
    team_slug = "character",
    goals = "integer",
    shots = "integer",
    power_play_instances = "character",
    power_play_goals = "integer",
    short_handed_instances = "character",
    short_handed_goals_against = "integer",
    penalty_minutes = "integer",
    face_off_wins = "integer",
    two_minute_penalties = "integer",
    five_minute_penalties = "integer",
    ten_minute_penalties = "integer",
    twenty_minute_penalties = "integer",
    total_distance_travelled = "double",
    puck_control_duration = "double",
    opponent_puck_control_duration = "double",
    contested_control_duration = "double"
  ),
  shotmap = c(
    season = "integer",
    game_id = "integer",
    type = "character",
    shooting_team_id = "integer",
    left_team_id = "integer",
    right_team_id = "integer",
    game_time = "integer",
    period = "integer",
    shot_x = "integer",
    shot_y = "integer",
    blocker_id = "integer",
    shooter_id = "integer",
    event_type = "character",
    own_team_players_on_ice = "integer",
    other_team_players_on_ice = "integer"
  ),
  standings = c(
    season = "integer",
    tournament = "character",
    internal_id = "integer",
    team_id = "integer",
    team_slug = "character",
    team_name = "character",
    team_logos_dark_bg = "character",
    team_logos_light_bg = "character",
    team_logos_dark_bg_original = "character",
    team_logos_light_bg_original = "character",
    ranking = "integer",
    live_ranking = "integer",
    games = "integer",
    live_games = "integer",
    wins = "integer",
    live_wins = "integer",
    overtime_wins = "integer",
    losses = "integer",
    live_losses = "integer",
    ties = "integer",
    live_ties = "integer",
    overtime_losses = "integer",
    points = "integer",
    live_points = "integer",
    goals = "integer",
    goals_against = "integer",
    live_goals = "integer",
    live_goals_against = "integer",
    power_play_instances = "integer",
    power_play_time = "integer",
    power_play_goals = "integer",
    short_handed_instances = "integer",
    short_handed_time = "integer",
    short_handed_goals_against = "integer",
    penalty_minutes = "integer",
    two_minute_penalties = "integer",
    five_minute_penalties = "integer",
    ten_minute_penalties = "integer",
    twenty_minute_penalties = "integer",
    twenty_five_minute_penalties = "integer",
    total_penalties = "integer",
    distance = "double",
    serie = "character",
    live_points_per_game = "double",
    power_play_percentage = "double",
    short_handed_percentage = "double",
    distance_per_game = "double",
    points_per_game = "double",
    win_percentage = "double"
  ),
  teams_history = c(
    id = "integer",
    season = "integer",
    league_id = "integer",
    serie_id = "integer",
    team_id = "integer",
    away_games = "integer",
    away_losses = "integer",
    away_ties = "integer",
    away_wins = "integer",
    away_wins_in_extra_time = "integer",
    games = "integer",
    goals_against = "integer",
    goals_scored = "integer",
    home_games = "integer",
    home_losses = "integer",
    home_ties = "integer",
    home_wins = "integer",
    home_wins_in_extra_time = "integer",
    losses = "integer",
    playoff_rank = "integer",
    points = "integer",
    regular_season_rank = "integer",
    ties = "integer",
    wins = "integer",
    wins_in_extra_time = "integer"
  ),
  teams = c(
    team_id = "integer",
    team_slug = "character",
    name = "character",
    contact_info = "character",
    externals = "character",
    country_code = "character",
    country_name = "character",
    current_venue_capacity = "integer",
    general_info = "character",
    url = "character",
    locality = "character",
    logo = "character",
    short_name = "character",
    teamtournamentstats = "character"
  ),
  team_stats = c(
    sort_desc = "logical",
    team_id = "integer",
    team_name = "character",
    team_short_name = "character",
    team_logos_dark_bg = "character",
    team_logos_light_bg = "character",
    team_logos_dark_bg_original = "character",
    team_logos_light_bg_original = "character",
    games = "integer",
    ranking = "integer",
    previous_game_day_ranking = "integer",
    shots = "integer",
    shots_saved = "integer",
    shots_even_strength = "integer",
    shots_powerplay = "integer",
    shots_penalty_kill = "integer",
    shots_against = "integer",
    goals = "integer",
    goals_against = "integer",
    shots_against_even_strength = "integer",
    shots_against_powerplay = "integer",
    shots_against_penalty_kill = "integer",
    non_blocked_shots = "integer",
    non_blocked_shots_even_strength = "integer",
    non_blocked_shots_powerplay = "integer",
    non_blocked_shots_penalty_kill = "integer",
    non_blocked_shots_against = "integer",
    non_blocked_shots_against_even_strength = "integer",
    non_blocked_shots_against_powerplay = "integer",
    non_blocked_shots_against_penalty_kill = "integer",
    powerplay_goals = "integer",
    powerplay_goals_against = "integer",
    penalty_kill_goals = "integer",
    penalty_kill_goals_against = "integer",
    even_strength_goals = "integer",
    even_strength_goals_against = "integer",
    previous_game_day_goals = "integer",
    previous_game_day_shots = "integer",
    previous_game_day_shots_percentage = "double",
    shots_percentage_even_strength = "double",
    shots_percentage_powerplay = "double",
    shots_percentage_penalty_kill = "double",
    shots_difference_even_strength = "integer",
    shots_difference_powerplay = "integer",
    shots_difference_penalty_kill = "integer",
    shots_for_shots_against_percentage = "double",
    non_blocked_shots_difference = "integer",
    save_percentage_even_strength = "double",
    save_percentage_powerplay = "double",
    save_percentage_penalty_kill = "double",
    shots_difference = "integer",
    pdo_penalty_kill = "double",
    pdo_even_strength = "double",
    pdo_powerplay = "double",
    shots_percentage = "double",
    non_blocked_shots_for_shots_against_percentage_even_strength = "double",
    non_blocked_shots_for_shots_against_percentage_powerplay = "double",
    non_blocked_shots_for_shots_against_percentage_penalty_kill = "double",
    shots_for_shots_against_percentage_even_strength = "double",
    shots_for_shots_against_percentage_powerplay = "double",
    shots_for_shots_against_percentage_penalty_kill = "double",
    non_blocked_shots_difference_powerplay = "integer",
    non_blocked_shots_difference_penalty_kill = "integer",
    non_blocked_shots_for_shots_against_percentage = "double",
    pdo = "double",
    non_blocked_shots_difference_even_strength = "integer",
    save_percentage = "double"
  ),
  players_info = c(
    player_id = "integer",
    team_id = "integer",
    team_slug = "character",
    team_name = "character",
    country_of_birth = "character",
    place_of_birth = "character",
    date_of_birth = "character",
    nationality = "character",
    first_name = "character",
    last_name = "character",
    role = "character",
    role_code = "character",
    handedness = "character",
    height = "integer",
    weight = "integer",
    rookie = "logical",
    jersey = "integer",
    injured = "logical",
    suspended = "logical",
    removed = "logical",
    captain = "logical",
    alternate_captain = "logical",
    picture_url = "character"
  ),
  player = c(
    active_seasons = "character",
    team_seasons = "character",
    team_list = "character",
    birth_locality_country_code = "character",
    birth_locality_country_name = "character",
    birth_locality_name = "character",
    date_of_birth = "character",
    player_id = "integer",
    first_name = "character",
    last_name = "character",
    handedness = "character",
    height = "integer",
    is_suspended = "logical",
    is_removed = "logical",
    nationality_code = "character",
    nationality_name = "character",
    weight = "integer",
    teams = "character"
  ),
  player_career = c(
    player_id = "integer",
    season = "integer",
    games = "integer",
    played_games = "integer",
    goals = "integer",
    assists = "integer",
    points = "integer",
    penalty_minutes = "integer",
    plus = "integer",
    minus = "integer",
    plus_minus = "integer",
    power_play_goals = "integer",
    short_handed_goals = "integer",
    winning_goals = "integer",
    shots = "integer",
    shots_into_goal = "integer",
    shot_percentage = "double",
    shot_into_goal_percentage = "double",
    tournament = "character",
    league_name = "character",
    team_name = "character",
    end_time = "character",
    team_short_name = "character",
    time_on_ice = "integer",
    save_percentage = "double",
    goals_against_average = "double",
    win_percentage = "double",
    shut_out = "character",
    faceoffs_won = "integer",
    faceoffs_percentage = "double",
    wins = "integer",
    losses = "integer",
    ties = "integer",
    gk_wins = "integer",
    gk_losses = "integer",
    gk_ties = "integer",
    saves = "integer",
    goals_against = "integer",
    faceoff_total = "integer"
  ),
  player_game_log = c(
    player_id = "integer",
    season = "integer",
    tournament = "character",
    game_str = "character",
    game_id = "integer",
    date = "character",
    assists = "integer",
    blocked_or_saved_shots = "integer",
    faceoffs_total = "integer",
    faceoffs_won = "integer",
    faceoffs_percentage = "double",
    goals = "integer",
    minus = "integer",
    total_points = "integer",
    home_team_name = "character",
    away_team_name = "character",
    penalty_minutes = "integer",
    penaltykill_assists = "integer",
    penaltykill_goals = "integer",
    penaltykill_points = "integer",
    plus = "integer",
    plus_minus = "integer",
    powerplay_assists = "integer",
    powerplay_goals = "integer",
    powerplay_seconds = "integer",
    time_on_ice = "integer",
    saves = "integer",
    save_percentage = "double",
    block_or_saved_shots_percentage = "double",
    shots = "integer",
    shots_into_goal = "integer",
    shut_out = "integer",
    shots_saved = "integer",
    shots_saved_by_skater = "integer",
    shot_percentage = "double",
    shot_into_goal_percentage = "double",
    goals_against = "integer",
    goalkeeper = "logical",
    winning_goals = "integer",
    won = "integer",
    tied = "integer",
    lost = "integer"
  ),
  player_stats = c(
    player_id = "integer",
    team_id = "integer",
    team_name = "character",
    team_short_name = "character",
    role = "character",
    first_name = "character",
    last_name = "character",
    nationality = "character",
    tournament = "character",
    picture_url = "character",
    previous_teams_for_tournament = "character",
    injured = "logical",
    jersey = "integer",
    last_season = "integer",
    goalkeeper = "logical",
    games = "integer",
    played_games = "integer",
    rookie = "logical",
    suspended = "logical",
    removed = "logical",
    time_on_ice = "integer",
    current = "logical",
    is_u20 = "logical",
    golden_helmets = "integer",
    alumni_points = "integer",
    goals = "integer",
    assists = "integer",
    points = "integer",
    plus = "integer",
    minus = "integer",
    plus_minus = "integer",
    penalty_minutes = "integer",
    powerplay_goals = "integer",
    penaltykill_goals = "integer",
    winning_goals = "integer",
    shots = "integer",
    shots_into_goal = "integer",
    expected_goals = "double",
    contest_won = "integer",
    contest_lost = "integer",
    goals_to_empty_goal = "integer",
    time_on_ice_avg = "double",
    contest_won_percentage = "double",
    xge = "double",
    shot_percentage = "double"
  )
)

#' **Liiga tournament**
#'
#' Current state of the league: game week of the season in progress and its last game week.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/tournament`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#'
#' @return A `fastRhockey_data` tibble (3 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `cachebust` | logical | Cache-busting flag for the site. |
#' | `game_week` | integer | Game week of the season. |
#' | `max_game_week` | integer | Last game week of the season. |
#'
#' @examples
#' \donttest{
#'   try(liiga_tournament())
#' }
#' @family Liiga Functions
#' @export
liiga_tournament <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .liiga_get("/tournament", list())
  df <- .lg_parse_generic(raw, .LG_COLS[["tournament"]])
  make_fastRhockey_data(df, "Liiga tournament Information from liiga.fi", Sys.time())
}

#' **Liiga games**
#'
#' Every game of a season and tournament: teams, score, xG, power plays, periods and rink.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Omitted = the season in progress.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League). Omitted = every tournament, preseason included.
#'
#' @return A `fastRhockey_data` tibble (59 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `start` | character | Scheduled start, ISO 8601 UTC. |
#' | `end` | character | Time the game ended, ISO 8601 UTC (absent from the refs captures; seen in the full 2026 playoff body). |
#' | `periods` | character | JSON list of the game's periods: index, home and away goals, category, start and end second. |
#' | `finished_type` | character | How the game ended: ENDED_DURING_REGULAR_GAME_TIME, ENDED_DURING_EXTENDED_GAME_TIME (overtime), ENDED_DURING_WINNING_SHOT_COMPETITION (shootout). |
#' | `started` | logical | Game has started. |
#' | `ended` | logical | Game has ended. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `spectators` | integer | Attendance (spectators) reported for the game. |
#' | `play_off_pair` | integer | Playoff pairing number, else null. |
#' | `play_off_phase` | integer | Playoff round, else null. |
#' | `play_off_req_wins` | integer | Wins needed to take the playoff series, else null. |
#' | `buy_tickets_url` | character | Ticket link for the game, else null. |
#' | `ice_rink_id` | integer | Liiga's id of the rink (ice arena). |
#' | `ice_rink_name` | character | Name of the rink (ice arena). |
#' | `ice_rink_latitude` | numeric | Rink latitude, decimal degrees. |
#' | `ice_rink_longitude` | numeric | Rink longitude, decimal degrees. |
#' | `ice_rink_street_address` | character | Street address of the rink. |
#' | `ice_rink_zip` | character | Postal code of the rink. |
#' | `ice_rink_city` | character | City where the rink is located. |
#' | `current_period` | integer | Last period played (3 = regulation; 4 and up = overtime periods or the shootout; 2026 values 3, 4, 5, 7). |
#' | `stale` | logical | Live data is stale. |
#' | `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
#' | `game_week` | integer | Game week of the season. |
#' | `home_team_id` | integer | Home team id (the number of the `<num>:<slug>` key). |
#' | `home_team_slug` | character | Home team slug (e.g. hpk). |
#' | `home_placeholder` | character | Home team: placeholder label for an undecided playoff team, else null. |
#' | `home_name` | character | Home team: display name of the team, e.g. HPK. |
#' | `home_goals` | integer | Home team: goals scored by the team in the game. |
#' | `home_time_out` | integer | Home team: second of the game (from game start) at which the team called its timeout. |
#' | `home_powerplay_instances` | integer | Home team: power plays the team had in the game. |
#' | `home_powerplay_goals` | integer | Home team: power-play goals the team scored in the game. |
#' | `home_short_handed_instances` | integer | Home team: times the team played short-handed in the game. |
#' | `home_short_handed_goals` | integer | Home team: short-handed goals the team scored in the game. |
#' | `home_expected_goals` | numeric | Home team: expected goals (xG) for the team in the game, from Liiga's undocumented model. |
#' | `home_ranking` | integer | Home team: table position. |
#' | `home_game_start_date_time` | character | Home team: scheduled start, ISO 8601 UTC. |
#' | `home_logos_dark_bg` | character | Home team: logo image URL for dark backgrounds. |
#' | `home_logos_light_bg` | character | Home team: logo image URL for light backgrounds. |
#' | `home_logos_dark_bg_original` | character | Home team: original-file logo URL for dark backgrounds; null in every capture. |
#' | `home_logos_light_bg_original` | character | Home team: original-file logo URL for light backgrounds; null in every capture. |
#' | `away_team_id` | integer | Away team id (the number of the `<num>:<slug>` key). |
#' | `away_team_slug` | character | Away team slug (e.g. tps). |
#' | `away_placeholder` | character | Away team: placeholder label for an undecided playoff team, else null. |
#' | `away_name` | character | Away team: display name of the team, e.g. HPK. |
#' | `away_goals` | integer | Away team: goals scored by the team in the game. |
#' | `away_time_out` | integer | Away team: second of the game (from game start) at which the team called its timeout. |
#' | `away_powerplay_instances` | integer | Away team: power plays the team had in the game. |
#' | `away_powerplay_goals` | integer | Away team: power-play goals the team scored in the game. |
#' | `away_short_handed_instances` | integer | Away team: times the team played short-handed in the game. |
#' | `away_short_handed_goals` | integer | Away team: short-handed goals the team scored in the game. |
#' | `away_expected_goals` | numeric | Away team: expected goals (xG) for the team in the game, from Liiga's undocumented model. |
#' | `away_ranking` | integer | Away team: table position. |
#' | `away_game_start_date_time` | character | Away team: scheduled start, ISO 8601 UTC. |
#' | `away_logos_dark_bg` | character | Away team: logo image URL for dark backgrounds. |
#' | `away_logos_light_bg` | character | Away team: logo image URL for light backgrounds. |
#' | `away_logos_dark_bg_original` | character | Away team: original-file logo URL for dark backgrounds; null in every capture. |
#' | `away_logos_light_bg_original` | character | Away team: original-file logo URL for light backgrounds; null in every capture. |
#'
#' @examples
#' \donttest{
#'   try(liiga_games(season = 2026, tournament = "playoffs"))
#' }
#' @family Liiga Functions
#' @export
liiga_games <- function(season = NULL, tournament = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = TRUE)
  raw <- .liiga_get("/games", list(season = season, tournament = tournament))
  df <- .lg_parse_games(raw, .LG_COLS[["games"]])
  make_fastRhockey_data(df, "Liiga games Information from liiga.fi", Sys.time())
}

#' **Liiga goals**
#'
#' Every goal of a season and tournament: scorer, assists, strength codes and skaters on ice.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Omitted = the season in progress.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League). Omitted = every tournament, preseason included.
#'
#' @return A `fastRhockey_data` tibble (30 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `log_time` | character | Wall-clock time the event was logged, ISO 8601 UTC. |
#' | `winning_goal` | logical | Game-winning goal. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
#' | `event_id` | integer | Event id within the game. |
#' | `home_team_score` | integer | Home score after the event. |
#' | `away_team_score` | integer | Away score after the event. |
#' | `goals_so_far_in_season` | integer | Scorer's season goal count after this goal. |
#' | `video_clip_url` | character | URL of the goal's video clip; null in every capture. |
#' | `video_clip_playlist_url` | character | URL of the goal's video clip playlist; null in every capture. |
#' | `video_thumbnail_url` | character | URL of the goal video's thumbnail image; null in every capture. |
#' | `scorer_player_id` | integer | Scorer's player id. |
#' | `scorer_first_name` | character | Scorer's first name. |
#' | `scorer_last_name` | character | Scorer's last name. |
#' | `assist1_player_id` | integer | First assist's player id. |
#' | `assist1_first_name` | character | First assist's first name. |
#' | `assist1_last_name` | character | First assist's last name. |
#' | `assist1_season_assists` | integer | First assister's season assist count after this goal. |
#' | `assist2_player_id` | integer | Second assist's player id. |
#' | `assist2_first_name` | character | Second assist's first name. |
#' | `assist2_last_name` | character | Second assist's last name. |
#' | `assist2_season_assists` | integer | Second assister's season assist count after this goal. |
#' | `goal_types` | character | Goal type codes, comma-separated Finnish abbreviations: YV power play, AV short-handed, TM empty net, RL penalty shot (others undocumented). |
#' | `plus_jerseys` | character | Jersey NUMBERS of this team's skaters on ice for the goal, space-separated. |
#' | `minus_jerseys` | character | Jersey NUMBERS of the other team's skaters on ice for the goal, space-separated. |
#'
#' @examples
#' \donttest{
#'   try(liiga_goals(season = 2026, tournament = "playoffs"))
#' }
#' @family Liiga Functions
#' @export
liiga_goals <- function(season = NULL, tournament = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = TRUE)
  raw <- .liiga_get("/games", list(season = season, tournament = tournament))
  df <- .lg_parse_goals(raw, .LG_COLS[["goals"]])
  make_fastRhockey_data(df, "Liiga goals Information from liiga.fi", Sys.time())
}

#' **Liiga schedule**
#'
#' Schedule of a season and tournament: game ids, teams, goals, xG, start and rink.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/schedule`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Omitted = the season in progress.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League). Omitted = runkosarja.
#'
#' @return A `fastRhockey_data` tibble (35 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `serie_id` | integer | Tournament id, the same every season: 1188 regular season, 1189 playoffs, 1194 preseason, 1890 CHL. |
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `home_team_name` | character | Display name of the home team, e.g. HPK. |
#' | `away_team_name` | character | Display name of the away team, e.g. TPS. |
#' | `home_team_id` | integer | Home team id (the number of the `<num>:<slug>` key). |
#' | `home_team_slug` | character | Home team slug (e.g. hpk). |
#' | `away_team_id` | integer | Away team id (the number of the `<num>:<slug>` key). |
#' | `away_team_slug` | character | Away team slug (e.g. tps). |
#' | `home_team_placeholder` | character | Placeholder label for an undecided playoff home team, else null. |
#' | `away_team_placeholder` | character | Placeholder label for an undecided playoff away team, else null. |
#' | `home_team_goals` | integer | Goals scored by the home team in the game. |
#' | `away_team_goals` | integer | Goals scored by the away team in the game. |
#' | `expected_home_team_goals` | numeric | Home team's expected goals (xG) for the game, provider-computed (same value as /games `homeTeam.expectedGoals`). |
#' | `expected_away_team_goals` | numeric | Away team's expected goals (xG) for the game, provider-computed. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `started` | logical | Game has started. |
#' | `ended` | logical | Game has ended. |
#' | `home_team_winner` | logical | True when the home team won the game. |
#' | `spectators` | integer | Attendance (spectators) reported for the game. |
#' | `home_team_ranking` | integer | Home team's table position; null in every capture. |
#' | `away_team_ranking` | integer | Away team's table position; null in every capture. |
#' | `play_off_pair` | integer | Playoff pairing number, else null. |
#' | `play_off_phase` | integer | Playoff round, else null. |
#' | `play_off_req_wins` | integer | Wins needed to take the series, else null. |
#' | `start` | character | Scheduled start, ISO 8601 UTC. |
#' | `buy_tickets_url` | character | Ticket link, else null. |
#' | `finished_type` | character | How the game ended: ENDED_DURING_REGULAR_GAME_TIME, ENDED_DURING_EXTENDED_GAME_TIME (overtime), ENDED_DURING_WINNING_SHOT_COMPETITION (shootout). |
#' | `ice_rink_id` | integer | Liiga's id of the rink (ice arena). |
#' | `ice_rink_name` | character | Name of the rink (ice arena). |
#' | `ice_rink_latitude` | numeric | Rink latitude, decimal degrees. |
#' | `ice_rink_longitude` | numeric | Rink longitude, decimal degrees. |
#' | `ice_rink_street_address` | character | Street address of the rink. |
#' | `ice_rink_zip` | character | Postal code of the rink. |
#' | `ice_rink_city` | character | City where the rink is located. |
#'
#' @examples
#' \donttest{
#'   try(liiga_schedule(season = 2026, tournament = "runkosarja"))
#' }
#' @family Liiga Functions
#' @export
liiga_schedule <- function(season = NULL, tournament = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = TRUE)
  raw <- .liiga_get("/schedule", list(season = season, tournament = tournament))
  df <- .lg_parse_schedule(raw, .LG_COLS[["schedule"]])
  make_fastRhockey_data(df, "Liiga schedule Information from liiga.fi", Sys.time())
}

#' **Liiga game weeks**
#'
#' Number of game weeks of a season and tournament.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/gameweeks`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Omitted = the season in progress.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League). Required by the API; defaults to runkosarja.
#'
#' @return A `fastRhockey_data` tibble (3 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
#' | `nb_weeks` | integer | Number of game weeks. |
#'
#' @examples
#' \donttest{
#'   try(liiga_gameweeks(season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_gameweeks <- function(season = NULL, tournament = "runkosarja") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = TRUE)
  tournament <- .lg_choice(tournament, .LIIGA_TOURNAMENTS, "tournament")
  raw <- .liiga_get("/gameweeks", list(season = season, tournament = tournament))
  df <- .lg_parse_generic(raw, .LG_COLS[["gameweeks"]])
  make_fastRhockey_data(df, "Liiga game weeks Information from liiga.fi", Sys.time())
}

#' **Liiga game header**
#'
#' One game's header: teams, score, xG, power plays, periods, rink, referees and attendance.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (55 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `start` | character | Scheduled start, ISO 8601 UTC. |
#' | `periods` | character | JSON list of the game's periods: index, home and away goals, category, start and end second. |
#' | `finished_type` | character | How the game ended: ENDED_DURING_REGULAR_GAME_TIME, ENDED_DURING_EXTENDED_GAME_TIME (overtime), ENDED_DURING_WINNING_SHOT_COMPETITION (shootout). |
#' | `started` | logical | Game has started. |
#' | `ended` | logical | Game has ended. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `spectators` | integer | Attendance (spectators) reported for the game. |
#' | `ice_rink_id` | integer | Liiga's id of the rink (ice arena). |
#' | `ice_rink_name` | character | Name of the rink (ice arena). |
#' | `ice_rink_latitude` | numeric | Rink latitude, decimal degrees. |
#' | `ice_rink_longitude` | numeric | Rink longitude, decimal degrees. |
#' | `ice_rink_street_address` | character | Street address of the rink. |
#' | `ice_rink_zip` | character | Postal code of the rink. |
#' | `ice_rink_city` | character | City where the rink is located. |
#' | `current_period` | integer | Last period played (3 = regulation; 4 and up = overtime periods or the shootout; 2026 values 3, 4, 5, 7). |
#' | `referees` | character | Game officials. |
#' | `stale` | logical | Live data is stale. |
#' | `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
#' | `game_week` | integer | Game week of the season. |
#' | `home_team_id` | integer | Home team id (the number of the `<num>:<slug>` key). |
#' | `home_team_slug` | character | Home team slug (e.g. hpk). |
#' | `home_placeholder` | character | Home team: placeholder label for an undecided playoff team, else null. |
#' | `home_name` | character | Home team: display name of the team, e.g. HPK. |
#' | `home_goals` | integer | Home team: goals scored by the team in the game. |
#' | `home_time_out` | integer | Home team: second of the game (from game start) at which the team called its timeout. |
#' | `home_powerplay_instances` | integer | Home team: power plays the team had in the game. |
#' | `home_powerplay_goals` | integer | Home team: power-play goals the team scored in the game. |
#' | `home_short_handed_instances` | integer | Home team: times the team played short-handed in the game. |
#' | `home_short_handed_goals` | integer | Home team: short-handed goals the team scored in the game. |
#' | `home_expected_goals` | numeric | Home team: expected goals (xG) for the team in the game, from Liiga's undocumented model. |
#' | `home_ranking` | integer | Home team: table position. |
#' | `home_game_start_date_time` | character | Home team: scheduled start, ISO 8601 UTC. |
#' | `home_logos_dark_bg` | character | Home team: logo image URL for dark backgrounds. |
#' | `home_logos_light_bg` | character | Home team: logo image URL for light backgrounds. |
#' | `home_logos_dark_bg_original` | character | Home team: original-file logo URL for dark backgrounds; null in every capture. |
#' | `home_logos_light_bg_original` | character | Home team: original-file logo URL for light backgrounds; null in every capture. |
#' | `away_team_id` | integer | Away team id (the number of the `<num>:<slug>` key). |
#' | `away_team_slug` | character | Away team slug (e.g. tps). |
#' | `away_placeholder` | character | Away team: placeholder label for an undecided playoff team, else null. |
#' | `away_name` | character | Away team: display name of the team, e.g. HPK. |
#' | `away_goals` | integer | Away team: goals scored by the team in the game. |
#' | `away_time_out` | integer | Away team: second of the game (from game start) at which the team called its timeout. |
#' | `away_powerplay_instances` | integer | Away team: power plays the team had in the game. |
#' | `away_powerplay_goals` | integer | Away team: power-play goals the team scored in the game. |
#' | `away_short_handed_instances` | integer | Away team: times the team played short-handed in the game. |
#' | `away_short_handed_goals` | integer | Away team: short-handed goals the team scored in the game. |
#' | `away_expected_goals` | numeric | Away team: expected goals (xG) for the team in the game, from Liiga's undocumented model. |
#' | `away_ranking` | integer | Away team: table position. |
#' | `away_game_start_date_time` | character | Away team: scheduled start, ISO 8601 UTC. |
#' | `away_logos_dark_bg` | character | Away team: logo image URL for dark backgrounds. |
#' | `away_logos_light_bg` | character | Away team: logo image URL for light backgrounds. |
#' | `away_logos_dark_bg_original` | character | Away team: original-file logo URL for dark backgrounds; null in every capture. |
#' | `away_logos_light_bg_original` | character | Away team: original-file logo URL for light backgrounds; null in every capture. |
#'
#' @examples
#' \donttest{
#'   try(liiga_game(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/", season, "/", game_id), list())
  df <- .lg_parse_game(raw, .LG_COLS[["game"]])
  make_fastRhockey_data(df, "Liiga game header Information from liiga.fi", Sys.time())
}

#' **Liiga game goals**
#'
#' One game's goals: scorer, assists, strength codes and skaters on ice.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (30 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `log_time` | character | Wall-clock time the event was logged, ISO 8601 UTC. |
#' | `winning_goal` | logical | Game-winning goal. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
#' | `event_id` | integer | Event id within the game. |
#' | `home_team_score` | integer | Home score after the event. |
#' | `away_team_score` | integer | Away score after the event. |
#' | `goals_so_far_in_season` | integer | Scorer's season goal count after this goal. |
#' | `video_clip_url` | character | URL of the goal's video clip; null in every capture. |
#' | `video_clip_playlist_url` | character | URL of the goal's video clip playlist; null in every capture. |
#' | `video_thumbnail_url` | character | URL of the goal video's thumbnail image; null in every capture. |
#' | `scorer_player_id` | integer | Scorer's player id. |
#' | `scorer_first_name` | character | Scorer's first name. |
#' | `scorer_last_name` | character | Scorer's last name. |
#' | `assist1_player_id` | integer | First assist's player id. |
#' | `assist1_first_name` | character | First assist's first name. |
#' | `assist1_last_name` | character | First assist's last name. |
#' | `assist1_season_assists` | integer | First assister's season assist count after this goal. |
#' | `assist2_player_id` | integer | Second assist's player id. |
#' | `assist2_first_name` | character | Second assist's first name. |
#' | `assist2_last_name` | character | Second assist's last name. |
#' | `assist2_season_assists` | integer | Second assister's season assist count after this goal. |
#' | `goal_types` | character | Goal type codes, comma-separated Finnish abbreviations: YV power play, AV short-handed, TM empty net, RL penalty shot (others undocumented). |
#' | `plus_jerseys` | character | Jersey NUMBERS of this team's skaters on ice for the goal, space-separated. |
#' | `minus_jerseys` | character | Jersey NUMBERS of the other team's skaters on ice for the goal, space-separated. |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_goals(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_goals <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/", season, "/", game_id), list())
  df <- .lg_parse_game_goals(raw, .LG_COLS[["game_goals"]])
  make_fastRhockey_data(df, "Liiga game goals Information from liiga.fi", Sys.time())
}

#' **Liiga game penalties**
#'
#' One game's penalties, both teams: player, fault, minutes, start and end.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (17 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `sufferer_player_id` | integer | Player who served the penalty. |
#' | `event_id` | integer | Event id within the game. |
#' | `log_time` | character | Wall-clock time the event was logged, ISO 8601 UTC. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
#' | `penalty_begintime` | integer | Penalty start, seconds from game start. |
#' | `penalty_endtime` | integer | Penalty end, seconds from game start. |
#' | `penalty_fault_name` | character | Infraction, in Finnish. |
#' | `penalty_fault_type` | character | Infraction code. |
#' | `penalty_info` | character | Extra penalty code ('' or a letter such as K; meaning unverified). |
#' | `penalty_minutes` | integer | Length of the penalty in minutes (2, 5, 10, 20, ...). |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_penalties(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_penalties <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/", season, "/", game_id), list())
  df <- .lg_parse_game_penalties(raw, .LG_COLS[["game_penalties"]])
  make_fastRhockey_data(df, "Liiga game penalties Information from liiga.fi", Sys.time())
}

#' **Liiga game shootout**
#'
#' One game's shootout attempts: shooter, goalkeeper and result (none for a game without one).
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (11 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `shooting_player_id` | integer | Shooter's player id. |
#' | `blocking_player_id` | integer | Goalkeeper's player id. |
#' | `shot_number` | integer | Attempt number in the shootout. |
#' | `shooting_team_id` | integer | Team id of the shooter's team (the number of the `<num>:<slug>` key). |
#' | `shooting_team_slug` | character | Slug of the shooter's team (e.g. hpk). |
#' | `blocking_team_id` | integer | Goalkeeper's team id. |
#' | `blocking_team_slug` | character | Goalkeeper's team slug. |
#' | `goal_scored` | logical | The attempt scored. |
#' | `winning_goal` | logical | Game-winning goal. |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_shootout(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_shootout <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/", season, "/", game_id), list())
  df <- .lg_parse_game_shootout(raw, .LG_COLS[["game_shootout"]])
  make_fastRhockey_data(df, "Liiga game shootout Information from liiga.fi", Sys.time())
}

#' **Liiga game lineups**
#'
#' One game's lineups, both teams: line, role, jersey, captaincy and bio.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (32 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `line` | integer | Line number 1-4; null for goalkeepers and extras. |
#' | `country_of_birth` | character | Country of birth, 3-letter code. |
#' | `place_of_birth` | character | Player's place of birth (town or city). |
#' | `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
#' | `nationality` | character | Nationality, 3-letter code. |
#' | `first_name` | character | Player's first name. |
#' | `last_name` | character | Player's last name. |
#' | `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
#' | `role_code` | character | Position code, Finnish: KH = centre, VL/OL = left/right wing, VP/OP = left/right defence, 7. P = seventh defenceman, MV = goalkeeper. |
#' | `handedness` | character | Shoots LEFT or RIGHT. |
#' | `height` | integer | Player's height in centimetres. |
#' | `weight` | integer | Player's weight in kilograms. |
#' | `captain` | logical | True when the player is the team captain (C). |
#' | `rookie` | logical | True when Liiga flags the player as a rookie. |
#' | `alternate_captain` | logical | True when the player is an alternate captain (A). |
#' | `jersey` | integer | Jersey (sweater) number the player wore. |
#' | `picture_url` | character | URL of the player's photo. |
#' | `injured` | logical | True when the player is listed as injured. |
#' | `suspended` | logical | True when the player is listed as suspended. |
#' | `removed` | logical | Removed from the roster. |
#' | `awards` | character | Player awards of the game (stars). |
#' | `sponsors` | character | Player's sponsors; null in every capture (unverified). |
#' | `extra_description` | character | Extra profile text for the player; null in every capture (meaning unverified). |
#' | `extra_strengths` | character | Extra profile field for the player's strengths; null in every capture (unverified). |
#' | `extra_rookie_picture` | logical | Flag for an extra rookie picture on the player's profile (meaning unverified). |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_lineups(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_lineups <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/", season, "/", game_id), list())
  df <- .lg_parse_game_lineups(raw, .LG_COLS[["game_lineups"]])
  make_fastRhockey_data(df, "Liiga game lineups Information from liiga.fi", Sys.time())
}

#' **Liiga game player stats**
#'
#' One game's lineups with each player's all-time, season, season-in-team and versus-team stats.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`
#' (with `dataType=playersWithStats`)
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (72 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `line` | integer | Line number 1-4; null for goalkeepers and extras. |
#' | `country_of_birth` | character | Country of birth, 3-letter code. |
#' | `place_of_birth` | character | Player's place of birth (town or city). |
#' | `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
#' | `nationality` | character | Nationality, 3-letter code. |
#' | `first_name` | character | Player's first name. |
#' | `last_name` | character | Player's last name. |
#' | `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
#' | `role_code` | character | Position code, Finnish: KH = centre, VL/OL = left/right wing, VP/OP = left/right defence, 7. P = seventh defenceman, MV = goalkeeper. |
#' | `handedness` | character | Shoots LEFT or RIGHT. |
#' | `height` | integer | Player's height in centimetres. |
#' | `weight` | integer | Player's weight in kilograms. |
#' | `captain` | logical | True when the player is the team captain (C). |
#' | `rookie` | logical | True when Liiga flags the player as a rookie. |
#' | `alternate_captain` | logical | True when the player is an alternate captain (A). |
#' | `jersey` | integer | Jersey (sweater) number the player wore. |
#' | `picture_url` | character | URL of the player's photo. |
#' | `injured` | logical | True when the player is listed as injured. |
#' | `suspended` | logical | True when the player is listed as suspended. |
#' | `removed` | logical | Removed from the roster. |
#' | `awards` | character | Player awards of the game (stars). |
#' | `sponsors` | character | Player's sponsors; null in every capture (unverified). |
#' | `extra_description` | character | Extra profile text for the player; null in every capture (meaning unverified). |
#' | `extra_strengths` | character | Extra profile field for the player's strengths; null in every capture (unverified). |
#' | `extra_rookie_picture` | logical | Flag for an extra rookie picture on the player's profile (meaning unverified). |
#' | `all_time_stats_games` | integer | Games (player, all-time). |
#' | `all_time_stats_goals` | integer | Goals (player, all-time). |
#' | `all_time_stats_assists` | integer | Assists (player, all-time). |
#' | `all_time_stats_points` | integer | Points (player, all-time). |
#' | `all_time_stats_plus` | integer | On ice for goals for, the + of plus/minus (all-time). |
#' | `all_time_stats_minus` | integer | On ice for goals against, the - of plus/minus (all-time). |
#' | `all_time_stats_plus_minus` | integer | Plus/minus, on-ice goals for minus against (all-time). |
#' | `all_time_stats_shots` | integer | Shots (player, all-time). |
#' | `all_time_stats_shot_percentage` | numeric | Shot percentage (player, all-time). Percent, 0-100. |
#' | `all_time_stats_penalty_minutes` | integer | Penalty minutes (player, all-time). |
#' | `season_stats_games` | integer | Games (player, this season). |
#' | `season_stats_goals` | integer | Goals (player, this season). |
#' | `season_stats_assists` | integer | Assists (player, this season). |
#' | `season_stats_points` | integer | Points (player, this season). |
#' | `season_stats_plus` | integer | On ice for goals for, the + of plus/minus (this season). |
#' | `season_stats_minus` | integer | On ice for goals against, the - of plus/minus (this season). |
#' | `season_stats_plus_minus` | integer | Plus/minus, on-ice goals for minus against (this season). |
#' | `season_stats_shots` | integer | Shots (player, this season). |
#' | `season_stats_shot_percentage` | numeric | Shot percentage (player, this season). Percent, 0-100. |
#' | `season_stats_penalty_minutes` | integer | Penalty minutes (player, this season). |
#' | `season_stats_in_team_games` | integer | Games (player, this season for this team). |
#' | `season_stats_in_team_goals` | integer | Goals (player, this season for this team). |
#' | `season_stats_in_team_assists` | integer | Assists (player, this season for this team). |
#' | `season_stats_in_team_points` | integer | Points (player, this season for this team). |
#' | `season_stats_in_team_plus` | integer | On ice for goals for, the + of plus/minus (this season with this team). |
#' | `season_stats_in_team_minus` | integer | On ice for goals against, the - of plus/minus (this season with this team). |
#' | `season_stats_in_team_plus_minus` | integer | Plus/minus, on-ice goals for minus against (this season with this team). |
#' | `season_stats_in_team_shots` | integer | Shots (player, this season for this team). |
#' | `season_stats_in_team_shot_percentage` | numeric | Shot percentage (player, this season for this team). Percent, 0-100. |
#' | `season_stats_in_team_penalty_minutes` | integer | Penalty minutes (player, this season for this team). |
#' | `vs_team_stats_games` | integer | Games played by the player against this game's opponent (span unverified). |
#' | `vs_team_stats_goals` | integer | Goals by the player against this game's opponent (span unverified). |
#' | `vs_team_stats_assists` | integer | Assists by the player against this game's opponent (span unverified). |
#' | `vs_team_stats_points` | integer | Points by the player against this game's opponent (span unverified). |
#' | `vs_team_stats_plus` | integer | On ice for goals for, the + of plus/minus (against this game's opponent; span unverified). |
#' | `vs_team_stats_minus` | integer | On ice for goals against, the - of plus/minus (against this game's opponent; span unverified). |
#' | `vs_team_stats_plus_minus` | integer | Plus/minus, on-ice goals for minus against (against this game's opponent; span unverified). |
#' | `vs_team_stats_shots` | integer | Shots by the player against this game's opponent (span unverified). |
#' | `vs_team_stats_shot_percentage` | numeric | Shot percentage against this game's opponent, 0-100 (span unverified). |
#' | `vs_team_stats_penalty_minutes` | integer | Penalty minutes by the player against this game's opponent (span unverified). |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_players_stats(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_players_stats <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/", season, "/", game_id), list(dataType = "playersWithStats"))
  df <- .lg_parse_game_players_stats(raw, .LG_COLS[["game_players_stats"]])
  make_fastRhockey_data(df, "Liiga game player stats Information from liiga.fi", Sys.time())
}

#' **Liiga game box score**
#'
#' One game's per-period player box score, skaters and goalies, with tracking where Liiga has it.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/stats/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (60 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `jersey` | integer | Jersey (sweater) number the player wore. |
#' | `is_goalie` | logical | Row is a goalkeeper's (goalie columns filled). |
#' | `points` | integer | Points (player, this period). |
#' | `assists` | integer | Assists (player, this period). |
#' | `goals` | integer | Goals (player, this period). |
#' | `plusminus` | integer | plus minus minus, this period. |
#' | `plus` | integer | On ice for goals for (the + of plus/minus). |
#' | `minus` | integer | On ice for goals against (the - of plus/minus). |
#' | `shots` | integer | Shots (player, this period). |
#' | `penaltyminutes` | integer | Penaltyminutes (player, this period). |
#' | `timeofice` | integer | Timeofice (player, this period). Seconds. |
#' | `powerplay_goals` | integer | Powerplay goals (player, this period). |
#' | `short_handed_goals` | integer | Short handed goals (player, this period). |
#' | `winning_goal` | logical | Game-winning goal. |
#' | `blocked_shots` | integer | Blocked shots (player, this period). |
#' | `faceoffs_total` | integer | Faceoffs the player took in the period; null for every player in the 2026 captures. |
#' | `faceoffs_won` | integer | Faceoffs the player won in the period; null for every player in the 2026 captures. |
#' | `corsi_for` | integer | Corsi (shot attempts) for (player, this period). |
#' | `corsi_against` | integer | Corsi (shot attempts) against (player, this period). |
#' | `faceoffs_center_total` | integer | Faceoffs the player took at centre ice in the period; null for every player in the 2026 captures. |
#' | `faceoffs_center_won` | integer | Faceoffs the player won at centre ice in the period; null for every player in the 2026 captures. |
#' | `faceoffs_defence_total` | integer | Faceoffs the player took in the defensive zone in the period; null for every player in the 2026 captures. |
#' | `faceoffs_defence_won` | integer | Faceoffs the player won in the defensive zone in the period; null for every player in the 2026 captures. |
#' | `faceoffs_offence_total` | integer | Faceoffs the player took in the offensive zone in the period; null for every player in the 2026 captures. |
#' | `faceoffs_offence_won` | integer | Faceoffs the player won in the offensive zone in the period; null for every player in the 2026 captures. |
#' | `fs_zone_starts_oz` | integer | Full-strength (fs, unverified) zone starts offensive zone (player, this period). |
#' | `fs_zone_starts_dz` | integer | Full-strength (fs, unverified) zone starts defensive zone (player, this period). |
#' | `powerplay2_goals` | integer | Powerplay 2 goals (player, this period). |
#' | `penaltykill2_goals` | integer | Penaltykill 2 goals (player, this period). |
#' | `powerplay_assists` | integer | Powerplay assists (player, this period). |
#' | `penaltykill_assists` | integer | Penaltykill assists (player, this period). |
#' | `goals_to_empty_goal` | integer | Goals to empty goal (player, this period). |
#' | `fs_team_shots` | integer | Full-strength (fs, unverified) team shots (player, this period). |
#' | `fs_team_goals` | integer | Full-strength (fs, unverified) team goals (player, this period). |
#' | `fs_team_shots_against` | integer | Full-strength (fs, unverified) team shots against (player, this period). |
#' | `fs_team_goals_against` | integer | Full-strength (fs, unverified) team goals against (player, this period). |
#' | `distance` | numeric | Distance (player, this period). Distance; unit not stated (unverified). |
#' | `total_passes` | integer | Total passes (player, this period). |
#' | `successful_passes` | integer | Successful passes (player, this period). |
#' | `player_passes_under_pressure` | integer | Player passes under pressure (player, this period). |
#' | `player_successful_passes_under_pressure` | integer | Player successful passes under pressure (player, this period). |
#' | `player_passes_under_high_pressure` | integer | Player passes under high pressure (player, this period). |
#' | `player_successful_passes_under_high_pressure` | integer | Player successful passes under high pressure (player, this period). |
#' | `expected_goals_player` | numeric | Expected goals (xG) of the player's own shots in the period, provider-computed (unverified scope). |
#' | `expected_goals_team` | numeric | Team expected goals (xG) while the player was on ice in the period, provider-computed (unverified scope). |
#' | `expected_goals_against` | numeric | Expected goals against (xG) faced in the period, provider-computed. |
#' | `expected_goals_against_shot_on_goal` | numeric | Expected goals against (xG) of the shots on goal only, provider-computed. |
#' | `saves` | integer | Saves (goalkeeper, this period). |
#' | `goals_allowed` | integer | Goals allowed (goalkeeper, this period). |
#' | `saves_percentage` | numeric | Save percentage, 0-100, sent as a STRING (e.g. "92.86"; "" when no shots), this period. |
#' | `shots_on_goal` | integer | Shots on goal (goalkeeper, this period). |
#' | `top_speed` | numeric | Player's top skating speed in the game (unit not stated; values near 9 fit m/s). |
#' | `hardest_shot` | numeric | Player's hardest shot in the game (unit not stated; values near 23 fit m/s). |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_stats(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_stats <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/stats/", season, "/", game_id), list())
  df <- .lg_parse_game_stats(raw, .LG_COLS[["game_stats"]])
  make_fastRhockey_data(df, "Liiga game box score Information from liiga.fi", Sys.time())
}

#' **Liiga game team stats**
#'
#' One game's per-period team stats with puck possession.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/games/stats/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (22 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `side` | character | Which team of the game the row belongs to: home or away. |
#' | `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `goals` | integer | Goals (team, this period). |
#' | `shots` | integer | Shots (team, this period). |
#' | `power_play_instances` | character | JSON list of Liiga's codes for the team's power plays in the period, one per power play (code meaning undocumented). |
#' | `power_play_goals` | integer | Power play goals (team, this period). |
#' | `short_handed_instances` | character | JSON list of Liiga's codes for the team's short-handed spells in the period, one per spell (code meaning undocumented). |
#' | `short_handed_goals_against` | integer | Short handed goals against (team, this period). |
#' | `penalty_minutes` | integer | Penalty minutes (team, this period). |
#' | `face_off_wins` | integer | Face off wins (team, this period). |
#' | `two_minute_penalties` | integer | Two minute penalties (team, this period). |
#' | `five_minute_penalties` | integer | Five minute penalties (team, this period). |
#' | `ten_minute_penalties` | integer | Ten minute penalties (team, this period). |
#' | `twenty_minute_penalties` | integer | Twenty minute penalties (team, this period). |
#' | `total_distance_travelled` | numeric | Distance skated by the team in the period (unit not stated). |
#' | `puck_control_duration` | numeric | Seconds this team controlled the puck in the period. |
#' | `opponent_puck_control_duration` | numeric | Seconds the other team controlled the puck in the period. |
#' | `contested_control_duration` | numeric | Seconds the puck was contested in the period. |
#'
#' @examples
#' \donttest{
#'   try(liiga_game_team_stats(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_game_team_stats <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/games/stats/", season, "/", game_id), list())
  df <- .lg_parse_game_team_stats(raw, .LG_COLS[["game_team_stats"]])
  make_fastRhockey_data(df, "Liiga game team stats Information from liiga.fi", Sys.time())
}

#' **Liiga shotmap**
#'
#' One game's shots with rink-image coordinates (pixels), shooter, blocker and skaters on ice; from 2016.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/shotmap/{season}/{game_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Game ids repeat across seasons, so a game is (season, game_id).
#' @param game_id Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique only within a season.
#'
#' @return A `fastRhockey_data` tibble (15 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `type` | character | Strength of the shot, e.g. EvenStrengthShot, PowerplayShot. |
#' | `shooting_team_id` | integer | Team id of the shooter's team (the number of the `<num>:<slug>` key). |
#' | `left_team_id` | integer | Team whose shots are drawn at the right end of the rink image (x grows toward it). |
#' | `right_team_id` | integer | Team whose shots are drawn at the left end of the rink image. |
#' | `game_time` | integer | Seconds from the start of the game. Regular season: 3900 (65:00) = the end of the 5-minute overtime; playoff overtime runs past it. |
#' | `period` | integer | Period number: 1-3 are regulation, 4 and up overtime (or the shootout). |
#' | `shot_x` | integer | Shot x on the site's rink drawing. Game 1 of 2026: x 90-932; the leftTeam's shots lie at x 188-932 and the rightTeam's at 90-379, so x grows toward the right end. Origin, unit and image width are not stated (unverified). |
#' | `shot_y` | integer | Shot y on the site's rink drawing. Game 1 of 2026: y 9-514. Origin, direction and image height are not stated (unverified). |
#' | `blocker_id` | integer | Player id of the goalkeeper or skater who stopped the shot, else null. |
#' | `shooter_id` | integer | Shooter's player id. |
#' | `event_type` | character | Shot outcome: GOAL, GOALIE_BLOCKED (saved), MISSED, PLAYER_BLOCKED. |
#' | `own_team_players_on_ice` | integer | Shooting team's skaters on ice. |
#' | `other_team_players_on_ice` | integer | Defending team's skaters on ice. |
#'
#' @examples
#' \donttest{
#'   try(liiga_shotmap(game_id = 1, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_shotmap <- function(season, game_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = FALSE)
  game_id <- .lg_int(game_id, "game_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/shotmap/", season, "/", game_id), list())
  df <- .lg_parse_shotmap(raw, .LG_COLS[["shotmap"]])
  make_fastRhockey_data(df, "Liiga shotmap Information from liiga.fi", Sys.time())
}

#' **Liiga standings**
#'
#' Standings of a season: regular season, playoffs, preseason, playout and qualifications tables.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/standings/`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season Season END year (2026 = 2025-26). Omitted = the season in progress.
#'
#' @return A `fastRhockey_data` tibble (49 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `tournament` | character | Table the row belongs to, in the request vocabulary: runkosarja, playoffs, valmistavat_ottelut, playout or qualifications. |
#' | `internal_id` | integer | Team id (same number as team_id). |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `team_logos_dark_bg` | character | Logo image URL for dark backgrounds. |
#' | `team_logos_light_bg` | character | Logo image URL for light backgrounds. |
#' | `team_logos_dark_bg_original` | character | Original-file logo URL for dark backgrounds; null in every capture. |
#' | `team_logos_light_bg_original` | character | Original-file logo URL for light backgrounds; null in every capture. |
#' | `ranking` | integer | Table position. |
#' | `live_ranking` | integer | Table position counting games in progress. |
#' | `games` | integer | Games (team, season to date in that table's tournament). |
#' | `live_games` | integer | Games counting games in progress (team, season to date in that table's tournament). |
#' | `wins` | integer | Wins (team, season to date in that table's tournament). |
#' | `live_wins` | integer | Wins counting games in progress (team, season to date in that table's tournament). |
#' | `overtime_wins` | integer | Overtime wins (team, season to date in that table's tournament). |
#' | `losses` | integer | Losses (team, season to date in that table's tournament). |
#' | `live_losses` | integer | Losses counting games in progress (team, season to date in that table's tournament). |
#' | `ties` | integer | Ties (team, season to date in that table's tournament). |
#' | `live_ties` | integer | Ties counting games in progress (team, season to date in that table's tournament). |
#' | `overtime_losses` | integer | Overtime losses (team, season to date in that table's tournament). |
#' | `points` | integer | Points (team, season to date in that table's tournament). |
#' | `live_points` | integer | Points counting games in progress (team, season to date in that table's tournament). |
#' | `goals` | integer | Goals (team, season to date in that table's tournament). |
#' | `goals_against` | integer | Goals against (team, season to date in that table's tournament). |
#' | `live_goals` | integer | Goals counting games in progress (team, season to date in that table's tournament). |
#' | `live_goals_against` | integer | Goals against counting games in progress (team, season to date in that table's tournament). |
#' | `power_play_instances` | integer | Power play instances (team, season to date in that table's tournament). |
#' | `power_play_time` | integer | Seconds on the power play, season to date. |
#' | `power_play_goals` | integer | Power play goals (team, season to date in that table's tournament). |
#' | `short_handed_instances` | integer | Short handed instances (team, season to date in that table's tournament). |
#' | `short_handed_time` | integer | Seconds short-handed, season to date. |
#' | `short_handed_goals_against` | integer | Short handed goals against (team, season to date in that table's tournament). |
#' | `penalty_minutes` | integer | Penalty minutes (team, season to date in that table's tournament). |
#' | `two_minute_penalties` | integer | Two minute penalties (team, season to date in that table's tournament). |
#' | `five_minute_penalties` | integer | Five minute penalties (team, season to date in that table's tournament). |
#' | `ten_minute_penalties` | integer | Ten minute penalties (team, season to date in that table's tournament). |
#' | `twenty_minute_penalties` | integer | Twenty minute penalties (team, season to date in that table's tournament). |
#' | `twenty_five_minute_penalties` | integer | Twenty five minute penalties (team, season to date in that table's tournament). |
#' | `total_penalties` | integer | Total penalties (team, season to date in that table's tournament). |
#' | `distance` | numeric | Distance (team, season to date in that table's tournament). Distance; unit not stated (unverified). |
#' | `serie` | character | Tournament code, uppercase (RUNKOSARJA, PLAYOFFS, PRACTICE, ...); in /schedule the integer tournament id instead (see serieId and Divergence notes). |
#' | `live_points_per_game` | numeric | Points per game counting games in progress (team, season to date in that table's tournament). |
#' | `power_play_percentage` | numeric | Power-play conversion, a 0-1 FRACTION sent as a STRING (e.g. "0.277"), season to date. |
#' | `short_handed_percentage` | numeric | Penalty-kill success, a 0-1 FRACTION sent as a STRING (e.g. "0.815"), season to date. |
#' | `distance_per_game` | numeric | Distance per game (team, season to date in that table's tournament). Distance; unit not stated (unverified). |
#' | `points_per_game` | numeric | Points per game (team, season to date in that table's tournament). |
#' | `win_percentage` | numeric | Share of games won, a 0-1 FRACTION sent as a STRING (e.g. "0.583"), season to date. |
#'
#' @examples
#' \donttest{
#'   try(liiga_standings(season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_standings <- function(season = NULL) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season <- .lg_int(season, "season", nullable = TRUE)
  raw <- .liiga_get("/standings/", list(season = season))
  df <- .lg_parse_standings(raw, .LG_COLS[["standings"]])
  make_fastRhockey_data(df, "Liiga standings Information from liiga.fi", Sys.time())
}

#' **Liiga team history**
#'
#' Every team's regular-season record, one row per team and season since 1976.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/teams/info`
#' (with `team=all`)
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#'
#' @return A `fastRhockey_data` tibble (25 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `id` | integer | Row id of this team-season row (NOT a team id; `teamId` is the team). |
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `league_id` | integer | League id: 1 = Liiga (SM-liiga) on every /teams/info row 1976-2027. |
#' | `serie_id` | integer | Tournament id, the same every season: 1188 regular season, 1189 playoffs, 1194 preseason, 1890 CHL. |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `away_games` | integer | Away games (team, that regular season). |
#' | `away_losses` | integer | Away losses (team, that regular season). |
#' | `away_ties` | integer | Away ties (team, that regular season). |
#' | `away_wins` | integer | Away wins (team, that regular season). |
#' | `away_wins_in_extra_time` | integer | Away wins in extra time (team, that regular season). |
#' | `games` | integer | Games (team, that regular season). |
#' | `goals_against` | integer | Goals against (team, that regular season). |
#' | `goals_scored` | integer | Goals scored (team, that regular season). |
#' | `home_games` | integer | Home games (team, that regular season). |
#' | `home_losses` | integer | Home losses (team, that regular season). |
#' | `home_ties` | integer | Home ties (team, that regular season). |
#' | `home_wins` | integer | Home wins (team, that regular season). |
#' | `home_wins_in_extra_time` | integer | Home wins in extra time (team, that regular season). |
#' | `losses` | integer | Losses (team, that regular season). |
#' | `playoff_rank` | integer | Playoff rank (team, that regular season). |
#' | `points` | integer | Points (team, that regular season). |
#' | `regular_season_rank` | integer | Regular season rank (team, that regular season). |
#' | `ties` | integer | Ties (team, that regular season). |
#' | `wins` | integer | Wins (team, that regular season). |
#' | `wins_in_extra_time` | integer | Wins in extra time (team, that regular season). |
#'
#' @examples
#' \donttest{
#'   try(liiga_teams_history())
#' }
#' @family Liiga Functions
#' @export
liiga_teams_history <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .liiga_get("/teams/info", list(team = "all"))
  df <- .lg_parse_generic(raw, .LG_COLS[["teams_history"]])
  make_fastRhockey_data(df, "Liiga team history Information from liiga.fi", Sys.time())
}

#' **Liiga teams**
#'
#' Team directory: every current and past team with logos, venue capacity and club info.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/teams/info`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#'
#' @return A `fastRhockey_data` tibble (14 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `name` | character | Club name as Liiga lists it, e.g. HIFK or K-Espoo. |
#' | `contact_info` | character | Club contact details as HTML, in Finnish (company, address, phone). |
#' | `externals` | character | JSON list of the club's external links (url, label), e.g. its ticket shops. |
#' | `country_code` | character | Club's country, 3-letter code (e.g. FIN). |
#' | `country_name` | character | Club's country name, e.g. Finland. |
#' | `current_venue_capacity` | integer | Capacity of the team's current home arena (spectators). |
#' | `general_info` | character | Club history and facts as an HTML table, in Finnish (founding year, titles, ...). |
#' | `url` | character | Club website URL. |
#' | `locality` | character | Club's home locality; null in every capture (unverified). |
#' | `logo` | character | Team logo image URL (old.liiga.fi media). |
#' | `short_name` | character | Team abbreviation. |
#' | `teamtournamentstats` | character | Team totals per season and tournament, snake_case, percentages and averages sent as STRINGS. |
#'
#' @examples
#' \donttest{
#'   try(liiga_teams())
#' }
#' @family Liiga Functions
#' @export
liiga_teams <- function() {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .liiga_get("/teams/info", list())
  df <- .lg_parse_teams(raw, .LG_COLS[["teams"]])
  make_fastRhockey_data(df, "Liiga teams Information from liiga.fi", Sys.time())
}

#' **Liiga team stat tables**
#'
#' Team stat table of a season range, by data_type.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/teams/stats`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' The columns below are those of the default `data_type`; the other tables carry their own
#' stat columns next to `team_id` / `player_id`.
#'
#' @param data_type Stat table: shots, passes, faceoffs, penalties, powerplay, penalty_kill, even_strength.
#' @param season_from First season (END year) of the range.
#' @param season_to Last season (END year) of the range.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League). Required by the API; defaults to runkosarja.
#'
#' @return A `fastRhockey_data` tibble (67 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `sort_desc` | logical | Default sort is descending. |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `team_short_name` | character | Team abbreviation. |
#' | `team_logos_dark_bg` | character | Logo image URL for dark backgrounds. |
#' | `team_logos_light_bg` | character | Logo image URL for light backgrounds. |
#' | `team_logos_dark_bg_original` | character | Original-file logo URL for dark backgrounds; null in every capture. |
#' | `team_logos_light_bg_original` | character | Original-file logo URL for light backgrounds; null in every capture. |
#' | `games` | integer | Games (team, over the season range). |
#' | `ranking` | integer | Table position. |
#' | `previous_game_day_ranking` | integer | Table position after the previous game day. |
#' | `shots` | integer | Shots (team, over the season range). |
#' | `shots_saved` | integer | Shots saved (team, over the season range). |
#' | `shots_even_strength` | integer | Shots at even strength (team, over the season range). |
#' | `shots_powerplay` | integer | Shots on the power play (team, over the season range). |
#' | `shots_penalty_kill` | integer | Shots on the penalty kill (team, over the season range). |
#' | `shots_against` | integer | Shots against (team, over the season range). |
#' | `goals` | integer | Goals (team, over the season range). |
#' | `goals_against` | integer | Goals against (team, over the season range). |
#' | `shots_against_even_strength` | integer | Shots against at even strength (team, over the season range). |
#' | `shots_against_powerplay` | integer | Shots against on the power play (team, over the season range). |
#' | `shots_against_penalty_kill` | integer | Shots against on the penalty kill (team, over the season range). |
#' | `non_blocked_shots` | integer | Non blocked shots (team, over the season range). |
#' | `non_blocked_shots_even_strength` | integer | Non blocked shots at even strength (team, over the season range). |
#' | `non_blocked_shots_powerplay` | integer | Non blocked shots on the power play (team, over the season range). |
#' | `non_blocked_shots_penalty_kill` | integer | Non blocked shots on the penalty kill (team, over the season range). |
#' | `non_blocked_shots_against` | integer | Non blocked shots against (team, over the season range). |
#' | `non_blocked_shots_against_even_strength` | integer | Non blocked shots against at even strength (team, over the season range). |
#' | `non_blocked_shots_against_powerplay` | integer | Non blocked shots against on the power play (team, over the season range). |
#' | `non_blocked_shots_against_penalty_kill` | integer | Non blocked shots against on the penalty kill (team, over the season range). |
#' | `powerplay_goals` | integer | Powerplay goals (team, over the season range). |
#' | `powerplay_goals_against` | integer | Powerplay goals against (team, over the season range). |
#' | `penalty_kill_goals` | integer | Penalty kill goals (team, over the season range). |
#' | `penalty_kill_goals_against` | integer | Penalty kill goals against (team, over the season range). |
#' | `even_strength_goals` | integer | Even strength goals (team, over the season range). |
#' | `even_strength_goals_against` | integer | Even strength goals against (team, over the season range). |
#' | `previous_game_day_goals` | integer | Goals as of the previous game day (team, over the season range). |
#' | `previous_game_day_shots` | integer | Shots as of the previous game day (team, over the season range). |
#' | `previous_game_day_shots_percentage` | numeric | Shots percentage as of the previous game day (team, over the season range). Percent, 0-100. |
#' | `shots_percentage_even_strength` | numeric | Shots percentage at even strength (team, over the season range). Percent, 0-100. |
#' | `shots_percentage_powerplay` | numeric | Shots percentage on the power play (team, over the season range). Percent, 0-100. |
#' | `shots_percentage_penalty_kill` | numeric | Shots percentage on the penalty kill (team, over the season range). Percent, 0-100. |
#' | `shots_difference_even_strength` | integer | Shots difference at even strength (team, over the season range). |
#' | `shots_difference_powerplay` | integer | Shots difference on the power play (team, over the season range). |
#' | `shots_difference_penalty_kill` | integer | Shots difference on the penalty kill (team, over the season range). |
#' | `shots_for_shots_against_percentage` | numeric | Shots for shots against percentage (team, over the season range). Percent, 0-100. |
#' | `non_blocked_shots_difference` | integer | Non blocked shots difference (team, over the season range). |
#' | `save_percentage_even_strength` | numeric | Save percentage at even strength (team, over the season range). Percent, 0-100. |
#' | `save_percentage_powerplay` | numeric | Save percentage on the power play (team, over the season range). Percent, 0-100. |
#' | `save_percentage_penalty_kill` | numeric | Save percentage on the penalty kill (team, over the season range). Percent, 0-100. |
#' | `shots_difference` | integer | Shots difference (team, over the season range). |
#' | `pdo_penalty_kill` | numeric | PDO (on-ice shooting % + save %) on the penalty kill (team, over the season range). Percent, 0-100. |
#' | `pdo_even_strength` | numeric | PDO (on-ice shooting % + save %) at even strength (team, over the season range). Percent, 0-100. |
#' | `pdo_powerplay` | numeric | PDO (on-ice shooting % + save %) on the power play (team, over the season range). Percent, 0-100. |
#' | `shots_percentage` | numeric | Shots percentage (team, over the season range). Percent, 0-100. |
#' | `non_blocked_shots_for_shots_against_percentage_even_strength` | numeric | Non blocked shots for shots against percentage at even strength (team, over the season range). Percent, 0-100. |
#' | `non_blocked_shots_for_shots_against_percentage_powerplay` | numeric | Non blocked shots for shots against percentage on the power play (team, over the season range). Percent, 0-100. |
#' | `non_blocked_shots_for_shots_against_percentage_penalty_kill` | numeric | Non blocked shots for shots against percentage on the penalty kill (team, over the season range). Percent, 0-100. |
#' | `shots_for_shots_against_percentage_even_strength` | numeric | Shots for shots against percentage at even strength (team, over the season range). Percent, 0-100. |
#' | `shots_for_shots_against_percentage_powerplay` | numeric | Shots for shots against percentage on the power play (team, over the season range). Percent, 0-100. |
#' | `shots_for_shots_against_percentage_penalty_kill` | numeric | Shots for shots against percentage on the penalty kill (team, over the season range). Percent, 0-100. |
#' | `non_blocked_shots_difference_powerplay` | integer | Non blocked shots difference on the power play (team, over the season range). |
#' | `non_blocked_shots_difference_penalty_kill` | integer | Non blocked shots difference on the penalty kill (team, over the season range). |
#' | `non_blocked_shots_for_shots_against_percentage` | numeric | Non blocked shots for shots against percentage (team, over the season range). Percent, 0-100. |
#' | `pdo` | numeric | PDO (on-ice shooting % + save %) (team, over the season range). Percent, 0-100. |
#' | `non_blocked_shots_difference_even_strength` | integer | Non blocked shots difference at even strength (team, over the season range). |
#' | `save_percentage` | numeric | Save percentage (team, over the season range). Percent, 0-100. |
#'
#' @examples
#' \donttest{
#'   try(liiga_team_stats(data_type = "shots", season_from = 2026, season_to = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_team_stats <- function(data_type, season_from = NULL, season_to = NULL, tournament = "runkosarja") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  data_type <- .lg_choice(data_type, c("shots", "passes", "faceoffs", "penalties", "powerplay", "penalty_kill", "even_strength"), "data_type")
  season_from <- .lg_int(season_from, "season_from", nullable = TRUE)
  season_to <- .lg_int(season_to, "season_to", nullable = TRUE)
  tournament <- .lg_choice(tournament, .LIIGA_TOURNAMENTS, "tournament")
  raw <- .liiga_get("/teams/stats", list(dataType = data_type, seasonFrom = season_from, seasonTo = season_to, tournament = tournament))
  df <- .lg_parse_generic(raw, .LG_COLS[["team_stats"]])
  make_fastRhockey_data(df, "Liiga team stat tables Information from liiga.fi", Sys.time())
}

#' **Liiga roster**
#'
#' Roster and bio of every player in a season range.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/players/info`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param season_from First season (END year) of the range.
#' @param season_to Last season (END year) of the range.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League). Required by the API; defaults to runkosarja.
#'
#' @return A `fastRhockey_data` tibble (23 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `country_of_birth` | character | Country of birth, 3-letter code. |
#' | `place_of_birth` | character | Player's place of birth (town or city). |
#' | `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
#' | `nationality` | character | Nationality, 3-letter code. |
#' | `first_name` | character | Player's first name. |
#' | `last_name` | character | Player's last name. |
#' | `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
#' | `role_code` | character | Position code, Finnish: KH = centre, VL/OL = left/right wing, VP/OP = left/right defence, 7. P = seventh defenceman, MV = goalkeeper. |
#' | `handedness` | character | Shoots LEFT or RIGHT. |
#' | `height` | integer | Player's height in centimetres. |
#' | `weight` | integer | Player's weight in kilograms. |
#' | `rookie` | logical | True when Liiga flags the player as a rookie. |
#' | `jersey` | integer | Jersey (sweater) number the player wore. |
#' | `injured` | logical | True when the player is listed as injured. |
#' | `suspended` | logical | True when the player is listed as suspended. |
#' | `removed` | logical | Removed from the roster. |
#' | `captain` | logical | True when the player is the team captain (C). |
#' | `alternate_captain` | logical | True when the player is an alternate captain (A). |
#' | `picture_url` | character | URL of the player's photo. |
#'
#' @examples
#' \donttest{
#'   try(liiga_players_info(season_from = 2026, season_to = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_players_info <- function(season_from = NULL, season_to = NULL, tournament = "runkosarja") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season_from <- .lg_int(season_from, "season_from", nullable = TRUE)
  season_to <- .lg_int(season_to, "season_to", nullable = TRUE)
  tournament <- .lg_choice(tournament, .LIIGA_TOURNAMENTS, "tournament")
  raw <- .liiga_get("/players/info", list(fromSeason = season_from, toSeason = season_to, tournament = tournament))
  df <- .lg_parse_players_info(raw, .LG_COLS[["players_info"]])
  make_fastRhockey_data(df, "Liiga roster Information from liiga.fi", Sys.time())
}

#' **Liiga player**
#'
#' One player's bio, teams by season and active seasons.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/players/info/{player_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param player_id Player id (player_id in liiga_players_info or a lineup; equals fihaId), e.g. 21870028.
#'
#' @return A `fastRhockey_data` tibble (18 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `active_seasons` | character | Seasons (END year) the player appears in. |
#' | `team_seasons` | character | Map team id -> seasons (END year) with that team. |
#' | `team_list` | character | Map team name -> team as `<num>:<slug>`. |
#' | `birth_locality_country_code` | character | Player's birth country, 3-letter code (e.g. FIN). |
#' | `birth_locality_country_name` | character | Player's birth country name, in Finnish (e.g. Suomi). |
#' | `birth_locality_name` | character | Player's birth place (town or city). |
#' | `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `first_name` | character | Player's first name. |
#' | `last_name` | character | Player's last name. |
#' | `handedness` | character | Shoots LEFT or RIGHT. |
#' | `height` | integer | Player's height in centimetres. |
#' | `is_suspended` | logical | True when the player is listed as suspended. |
#' | `is_removed` | logical | Removed from the roster. |
#' | `nationality_code` | character | Player's nationality, 3-letter country code (e.g. FIN). |
#' | `nationality_name` | character | Player's nationality as a country name, in Finnish (e.g. Suomi). |
#' | `weight` | integer | Player's weight in kilograms. |
#' | `teams` | character | Map keyed by an id -> one of the player's team seasons (season, jersey, teamId, slug). |
#'
#' @examples
#' \donttest{
#'   try(liiga_player(player_id = 21870028))
#' }
#' @family Liiga Functions
#' @export
liiga_player <- function(player_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  player_id <- .lg_int(player_id, "player_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/players/info/", player_id), list())
  df <- .lg_parse_player(raw, .LG_COLS[["player"]])
  make_fastRhockey_data(df, "Liiga player Information from liiga.fi", Sys.time())
}

#' **Liiga player career**
#'
#' One player's career: season totals by tournament.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/players/info/{player_id}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param player_id Player id (player_id in liiga_players_info or a lineup; equals fihaId), e.g. 21870028.
#'
#' @return A `fastRhockey_data` tibble (39 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `games` | integer | Games (player, that season and tournament). |
#' | `played_games` | integer | Played games (player, that season and tournament). |
#' | `goals` | integer | Goals (player, that season and tournament). |
#' | `assists` | integer | Assists (player, that season and tournament). |
#' | `points` | integer | Points (player, that season and tournament). |
#' | `penalty_minutes` | integer | Penalty minutes (player, that season and tournament). |
#' | `plus` | integer | On ice for goals for (the + of plus/minus). |
#' | `minus` | integer | On ice for goals against (the - of plus/minus). |
#' | `plus_minus` | integer | Plus/minus: on-ice goals for minus on-ice goals against. |
#' | `power_play_goals` | integer | Power play goals (player, that season and tournament). |
#' | `short_handed_goals` | integer | Short handed goals (player, that season and tournament). |
#' | `winning_goals` | integer | Winning goals (player, that season and tournament). |
#' | `shots` | integer | Shots (player, that season and tournament). |
#' | `shots_into_goal` | integer | Shots into goal (player, that season and tournament). |
#' | `shot_percentage` | numeric | Shot percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
#' | `shot_into_goal_percentage` | numeric | Shot into goal percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
#' | `tournament` | character | Tournament of the career row, uppercase: RUNKOSARJA (regular season), PLAYOFFS, PRACTICE (preseason), ... |
#' | `league_name` | character | Name of the league of that career row, e.g. Liiga. |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `end_time` | character | ISO 8601 UTC timestamp, not game seconds: the end of that season-tournament for finished seasons (2026-03-17T23:59:59Z for the 2025-26 regular season) and the request time for the season in progress. Looks like the end of the stats window; meaning unverified. |
#' | `team_short_name` | character | Team abbreviation. |
#' | `time_on_ice` | integer | Time on ice (player, that season and tournament). Seconds. |
#' | `save_percentage` | numeric | Save percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
#' | `goals_against_average` | numeric | Goals against per game (goalkeeper), that season and tournament; sent as a string. |
#' | `win_percentage` | numeric | Win percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
#' | `shut_out` | character | Shutouts (goalkeeper), that season and tournament; sent as a string (e.g. "0"). |
#' | `faceoffs_won` | integer | Faceoffs won (player, that season and tournament). |
#' | `faceoffs_percentage` | numeric | Faceoffs percentage (player, that season and tournament). Percent, 0-100, sent as a STRING. |
#' | `wins` | integer | Wins (player, that season and tournament). |
#' | `losses` | integer | Losses (player, that season and tournament). |
#' | `ties` | integer | Ties (player, that season and tournament). |
#' | `gk_wins` | integer | Goalkeeper wins (player, that season and tournament). |
#' | `gk_losses` | integer | Goalkeeper losses (player, that season and tournament). |
#' | `gk_ties` | integer | Goalkeeper ties (player, that season and tournament). |
#' | `saves` | integer | Saves (player, that season and tournament). |
#' | `goals_against` | integer | Goals against (player, that season and tournament). |
#' | `faceoff_total` | integer | Faceoff total (player, that season and tournament). |
#'
#' @examples
#' \donttest{
#'   try(liiga_player_career(player_id = 21870028))
#' }
#' @family Liiga Functions
#' @export
liiga_player_career <- function(player_id) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  player_id <- .lg_int(player_id, "player_id", nullable = FALSE)
  raw <- .liiga_get(paste0("/players/info/", player_id), list())
  df <- .lg_parse_player_career(raw, .LG_COLS[["player_career"]])
  make_fastRhockey_data(df, "Liiga player career Information from liiga.fi", Sys.time())
}

#' **Liiga player game log**
#'
#' One player's game log for a season, every tournament.
#'
#' Endpoint: `GET https://liiga.fi/api/v2/players/info/{player_id}/games/{season}`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' @param player_id Player id (player_id in liiga_players_info or a lineup; equals fihaId), e.g. 21870028.
#' @param season Season END year (2026 = 2025-26).
#'
#' @return A `fastRhockey_data` tibble (42 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `season` | integer | Season END year (2026 = 2025-26). |
#' | `tournament` | character | Tournament of the game (the body's list key): regular, playoffs, practice, qualifications, playout, chl or pitsiturnaus. |
#' | `game_str` | character | Matchup label, e.g. `HPK - LUK`. |
#' | `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
#' | `date` | character | Game start, ISO 8601 UTC. |
#' | `assists` | integer | Assists (player, this game). |
#' | `blocked_or_saved_shots` | integer | Blocked or saved shots (player, this game). |
#' | `faceoffs_total` | integer | Faceoffs total (player, this game). |
#' | `faceoffs_won` | integer | Faceoffs won (player, this game). |
#' | `faceoffs_percentage` | numeric | Faceoffs percentage (player, this game). Percent, 0-100, sent as a STRING. |
#' | `goals` | integer | Goals (player, this game). |
#' | `minus` | integer | On ice for goals against (the - of plus/minus). |
#' | `total_points` | integer | Total points (player, this game). |
#' | `home_team_name` | character | Display name of the home team of the game, e.g. HPK. |
#' | `away_team_name` | character | Display name of the away team of the game, e.g. Lukko. |
#' | `penalty_minutes` | integer | Penalty minutes (player, this game). |
#' | `penaltykill_assists` | integer | Penaltykill assists (player, this game). |
#' | `penaltykill_goals` | integer | Penaltykill goals (player, this game). |
#' | `penaltykill_points` | integer | Penaltykill points (player, this game). |
#' | `plus` | integer | On ice for goals for (the + of plus/minus). |
#' | `plus_minus` | integer | Plus/minus: on-ice goals for minus on-ice goals against. |
#' | `powerplay_assists` | integer | Powerplay assists (player, this game). |
#' | `powerplay_goals` | integer | Powerplay goals (player, this game). |
#' | `powerplay_seconds` | integer | Powerplay seconds (player, this game). Seconds. |
#' | `time_on_ice` | integer | Time on ice (player, this game). Seconds. |
#' | `saves` | integer | Saves (player, this game). |
#' | `save_percentage` | numeric | Save percentage (player, this game). Percent, 0-100, sent as a STRING. |
#' | `block_or_saved_shots_percentage` | numeric | Block or saved shots percentage (player, this game). Percent, 0-100, sent as a STRING. |
#' | `shots` | integer | Shots (player, this game). |
#' | `shots_into_goal` | integer | Shots into goal (player, this game). |
#' | `shut_out` | integer | 1 for a goalkeeper's shutout in this game, else 0. |
#' | `shots_saved` | integer | Shots saved (player, this game). |
#' | `shots_saved_by_skater` | integer | Shots saved by skater (player, this game). |
#' | `shot_percentage` | numeric | Shot percentage (player, this game). Percent, 0-100, sent as a STRING. |
#' | `shot_into_goal_percentage` | numeric | Shot into goal percentage (player, this game). Percent, 0-100, sent as a STRING. |
#' | `goals_against` | integer | Goals against (player, this game). |
#' | `goalkeeper` | logical | True when the player is a goalkeeper. |
#' | `winning_goals` | integer | Winning goals (player, this game). |
#' | `won` | integer | 1 when the player's team won the game, else 0. |
#' | `tied` | integer | 1 when Liiga counts the game as a tie for the player's team, else 0 (overtime handling unverified). |
#' | `lost` | integer | 1 when the player's team lost the game, else 0. |
#'
#' @examples
#' \donttest{
#'   try(liiga_player_game_log(player_id = 21870028, season = 2026))
#' }
#' @family Liiga Functions
#' @export
liiga_player_game_log <- function(player_id, season) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  player_id <- .lg_int(player_id, "player_id", nullable = FALSE)
  season <- .lg_int(season, "season", nullable = FALSE)
  raw <- .liiga_get(paste0("/players/info/", player_id, "/games/", season), list())
  df <- .lg_parse_player_game_log(raw, .LG_COLS[["player_game_log"]])
  make_fastRhockey_data(df, "Liiga player game log Information from liiga.fi", Sys.time())
}

#' **Liiga player stat tables**
#'
#' Player stat table summed over a season range, by data_type (the whole league, unfiltered).
#'
#' Endpoint: `GET https://liiga.fi/api/v2/players/stats/summed/{season_from}/{season_to}/{tournament}/true`
#'
#' Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests to liiga.fi are
#' spaced at least one second apart. A request Liiga answers with nothing (an unknown game, a
#' tournament not played that season, a shot map before 2016) raises
#' `fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
#' `fastRhockey_asset_fetch`; a bad argument raises `fastRhockey_bad_request`.
#'
#' The columns below are those of the default `data_type`; the other tables carry their own
#' stat columns next to `team_id` / `player_id`.
#'
#' @param season_from First season (END year) of the range, e.g. 2026.
#' @param season_to Last season (END year) of the range, e.g. 2026.
#' @param tournament Tournament: runkosarja (regular season), playoffs, playout, qualifications, valmistavat_ottelut (preseason) or chl (Champions Hockey League).
#' @param data_type Stat table: basicStats, basicStatsGk, advancedStats, goalStats, shotStats, skatingStats, faceOffStats, penaltyStats, powerplayPenaltykillStats, passes, winningShotComp, winningShotCompGk, gameTime.
#'
#' @return A `fastRhockey_data` tibble (45 columns; zero rows when a valid payload has none).
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
#' | `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
#' | `team_name` | character | Team's display name, e.g. HPK or Tappara. |
#' | `team_short_name` | character | Team abbreviation. |
#' | `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
#' | `first_name` | character | Player's first name. |
#' | `last_name` | character | Player's last name. |
#' | `nationality` | character | Nationality, 3-letter code. |
#' | `tournament` | character | Tournament of the summed row as Liiga labels it, e.g. regular. |
#' | `picture_url` | character | URL of the player's photo. |
#' | `previous_teams_for_tournament` | character | Earlier teams of a player traded during the tournament. |
#' | `injured` | logical | True when the player is listed as injured. |
#' | `jersey` | integer | Jersey (sweater) number the player wore. |
#' | `last_season` | integer | Last season (END year) of the range in which the player appears. |
#' | `goalkeeper` | logical | True when the player is a goalkeeper. |
#' | `games` | integer | Games (player, summed over the season range). |
#' | `played_games` | integer | Played games (player, summed over the season range). |
#' | `rookie` | logical | True when Liiga flags the player as a rookie. |
#' | `suspended` | logical | True when the player is listed as suspended. |
#' | `removed` | logical | Removed from the roster. |
#' | `time_on_ice` | integer | Time on ice (player, summed over the season range). Seconds. |
#' | `current` | logical | Player is on the team now. |
#' | `is_u20` | logical | True when Liiga flags the player as under 20 (U20). |
#' | `golden_helmets` | integer | Golden helmets (Liiga's team points-leader helmet; reading unverified), summed over the season range. |
#' | `alumni_points` | integer | Alumni points (meaning undocumented by Liiga), summed over the season range. |
#' | `goals` | integer | Goals (player, summed over the season range). |
#' | `assists` | integer | Assists (player, summed over the season range). |
#' | `points` | integer | Points (player, summed over the season range). |
#' | `plus` | integer | On ice for goals for (the + of plus/minus). |
#' | `minus` | integer | On ice for goals against (the - of plus/minus). |
#' | `plus_minus` | integer | Plus/minus: on-ice goals for minus on-ice goals against. |
#' | `penalty_minutes` | integer | Penalty minutes (player, summed over the season range). |
#' | `powerplay_goals` | integer | Powerplay goals (player, summed over the season range). |
#' | `penaltykill_goals` | integer | Penaltykill goals (player, summed over the season range). |
#' | `winning_goals` | integer | Winning goals (player, summed over the season range). |
#' | `shots` | integer | Shots (player, summed over the season range). |
#' | `shots_into_goal` | integer | Shots into goal (player, summed over the season range). |
#' | `expected_goals` | numeric | Expected goals (xG) of the player's shots summed over the season range, from Liiga's undocumented model. |
#' | `contest_won` | integer | Contests (puck battles; definition unverified) the player won, summed over the season range. |
#' | `contest_lost` | integer | Contests (puck battles; definition unverified) the player lost, summed over the season range. |
#' | `goals_to_empty_goal` | integer | Goals to empty goal (player, summed over the season range). |
#' | `time_on_ice_avg` | numeric | Time on ice (player, summed over the season range). Seconds per game. |
#' | `contest_won_percentage` | numeric | Share of contests won, 0-100, over the season range (definition unverified). |
#' | `xge` | numeric | Goals minus expectedGoals over the season range (4 goals on 5.0956 xG = -1.0956). |
#' | `shot_percentage` | numeric | Shot percentage (player, summed over the season range). Percent, 0-100. |
#'
#' @examples
#' \donttest{
#'   try(liiga_player_stats(season_from = 2026, season_to = 2026, tournament = "runkosarja"))
#' }
#' @family Liiga Functions
#' @export
liiga_player_stats <- function(season_from, season_to, tournament, data_type = "basicStats") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  season_from <- .lg_int(season_from, "season_from", nullable = FALSE)
  season_to <- .lg_int(season_to, "season_to", nullable = FALSE)
  tournament <- .lg_choice(tournament, .LIIGA_TOURNAMENTS, "tournament")
  data_type <- .lg_choice(data_type, c("basicStats", "basicStatsGk", "advancedStats", "goalStats", "shotStats", "skatingStats", "faceOffStats", "penaltyStats", "powerplayPenaltykillStats", "passes", "winningShotComp", "winningShotCompGk", "gameTime"), "data_type")
  raw <- .liiga_get(paste0("/players/stats/summed/", season_from, "/", season_to, "/", tournament, "/true"), list(dataType = data_type))
  df <- .lg_parse_generic(raw, .LG_COLS[["player_stats"]])
  make_fastRhockey_data(df, "Liiga player stat tables Information from liiga.fi", Sys.time())
}
