# magnus.R -- Ligue Magnus (liguemagnus.com) wrappers.
#
# Port of sdv-py sportsdataverse/hockey/magnus/ (magnus_runtime.py,
# magnus_parsers.py and the generated magnus.py). One keyless host, throttled
# to one request a second through the shared European hockey transport
# (euro_hockey_runtime.R), two sources on it:
#
# * WordPress admin-ajax JSON: GET /wp-admin/admin-ajax.php?action=<x>&...,
#   one physical endpoint dispatched by `action`. Every answer is
#   {"success": true, "data": ...} with HTTP 200, errors included, so the
#   envelope is read (`.magnus_check_ajax()`).
# * The game sheet page: GET /rencontre/{id}/ is text/html; the whole sheet is
#   the HTML-escaped JSON value of the Vue `:data` attribute on
#   <live-rencontre-container> (`.magnus_extract_sheet()`).
#
# Season key: the public argument is the season's END year (2027 = 2026-27);
# the API keys a season by `competition_id`, mapped by `.magnus_competition_id()`
# (seeded ids, one live lookup per session for an unseeded season). Coverage
# starts in 2020-21 (season 2021).
#
# Refusals (sdv-py `_check_ajax` / `_game_sheet`): `success: false` with
# "Aucune donnee disponible" -> fastRhockey_no_data, with any other message ->
# fastRhockey_asset_fetch; an empty `data` stub or zero rows -> no_data; a
# Laravel validation dict naming a parameter -> fastRhockey_bad_request; rows
# outside the requested scope, non-object rows, a schedule page that is not the
# whole list (type-strict `last_page` == 1 and `total` == rows served) or a game
# page without its embedded sheet -> asset_fetch; an unknown or unplayed game ->
# no_data. Transport errors come from the shared runtime (404 -> no_data,
# 400/422 -> bad_request, other non-2xx / empty / non-JSON -> asset_fetch).
#
# Ids are integers (never a float, never a "123.0" string); licence_number and
# event_id are strings. Staff birth dates (staffs[].ddn) are dropped from every
# sheet.

.MAGNUS_HOST <- "https://liguemagnus.com"
.MAGNUS_AJAX <- "https://liguemagnus.com/wp-admin/admin-ajax.php"
# get_competition_by_type id_type of the Ligue Magnus (2 = Division 1).
.MAGNUS_TYPE <- 1L
# The first season (END year) the backend holds: 2020-21.
.MAGNUS_FIRST_SEASON <- 2021L
# Game times are published in French local time.
.MAGNUS_TZ <- "Europe/Paris"
# Regulation period length in seconds; `temps` is elapsed game seconds.
.MAGNUS_PERIOD_SECONDS <- 1200L
# A game sheet whose keys are all in here is an error object, not a sheet.
.MAGNUS_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status")
# The success = false message that means "no data" (accent-, case- and whitespace-folded).
.MAGNUS_NO_DATA_MESSAGE <- "aucune donnee disponible"

# season (END year) -> competition_id (get_competition_by_type&id_type=1,
# 2026-10-12); `live` caches one lookup for an unseeded season.
.magnus_state <- new.env(parent = emptyenv())
.magnus_state$seasons <- c(
  `2021` = 1L, `2022` = 34L, `2023` = 79L, `2024` = 120L, `2025` = 157L,
  `2026` = 197L, `2027` = 240L
)
.magnus_state$live <- NULL

# Raw event `type` -> English event_type.
.MAGNUS_EVENT_TYPES <- c(
  BUT = "goal", SANCTION = "penalty", TIR = "shot", ENGA = "faceoff",
  REMPLACANT = "goalie_change", TEMPS_MORT = "timeout"
)
# Order of events that share a second (a heuristic tie-break only: the sheet
# groups events by type and carries no sequence number).
.MAGNUS_TYPE_ORDER <- c(TIR = 0L, BUT = 1L, SANCTION = 2L, REMPLACANT = 3L, TEMPS_MORT = 4L, ENGA = 5L)
.MAGNUS_DECISIONS <- c(S = "regulation", PRL = "overtime", TAB = "shootout")
.MAGNUS_POSITIONS <- c(A = "forward", D = "defence", GB = "goalie")

# ---------------------------------------------------------------------------
# Value helpers (sdv-py `_at`, `_int`, `_dicts`, `_coerce`)
# ---------------------------------------------------------------------------

# A JSON object (named list) / a JSON array (unnamed list), as jsonlite returns
# them with simplifyVector = FALSE.
.magnus_is_obj <- function(x) is.list(x) && !is.null(names(x))
.magnus_is_arr <- function(x) is.list(x) && is.null(names(x))

# obj[[k1]][[k2]]...; NULL as soon as a level is not an object.
.magnus_at <- function(obj, ...) {
  for (key in c(...)) obj <- if (.magnus_is_obj(obj)) obj[[key]] else NULL
  obj
}

.magnus_dicts <- function(x) if (.magnus_is_arr(x)) Filter(.magnus_is_obj, x) else list()

# Python truthiness of a parsed JSON value (`a or b`).
.magnus_truthy <- function(x) {
  !is.null(x) && length(x) > 0 && !identical(x, FALSE) && !identical(x, "")
}

# An integer-valued number, or NA_integer_. Accepts a JSON integer, a digit
# string, and a whole double beyond the 32-bit range (jsonlite's overflow);
# refuses a logical, a fractional or float-valued number, an object. In-range
# values come back as R integers (so a computed integer passes through again);
# `.magnus_col_integer()` refuses the out-of-range remainder.
.magnus_int <- function(x) {
  if (is.null(x) || length(x) != 1L || is.list(x) || is.logical(x)) return(NA_integer_)
  if (is.integer(x)) return(x)
  if (is.double(x)) {
    return(if (is.finite(x) && x == trunc(x) && abs(x) >= 2^31) x else NA_integer_)
  }
  if (is.character(x)) {
    s <- trimws(x)
    if (is.na(s) || !grepl("^-?[0-9]+$", s)) return(NA_integer_)
    v <- as.numeric(s)
    return(if (abs(v) < 2^31) as.integer(v) else v)
  }
  NA_integer_
}

# A user-supplied id / year: any whole number or digit string; else NA.
.magnus_arg_int <- function(x) {
  if (is.null(x) || length(x) != 1L || is.list(x) || is.logical(x)) return(NA_real_)
  if (is.numeric(x)) return(if (is.finite(x) && x == trunc(x)) as.numeric(x) else NA_real_)
  if (is.character(x)) {
    s <- trimws(x)
    return(if (!is.na(s) && grepl("^-?[0-9]+$", s)) as.numeric(s) else NA_real_)
  }
  NA_real_
}

# Validate a required/optional id argument; NULL passes through when optional.
.magnus_id_arg <- function(x, name, optional = FALSE) {
  if (is.null(x) && optional) return(NULL)
  v <- .magnus_arg_int(x)
  if (is.na(v) || v < 0 || v >= 2^31) {
    .abort_bad_request("{.arg {name}} must be a whole number id, not {.val {x}}.")
  }
  as.integer(v)
}

.magnus_game_id_arg <- function(game_id) {
  v <- .magnus_arg_int(game_id)
  if (is.na(v) || v < 1 || v >= 2^31) {
    .abort_bad_request("{.arg game_id} must be a positive whole number game id, not {.val {game_id}}.")
  }
  as.integer(v)
}

# A YYYY-MM-DD string (or a Date) the server filters games by; NULL passes through.
.magnus_date_arg <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date") && length(x) == 1L && !is.na(x)) return(format(x, "%Y-%m-%d"))
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))) return(trimws(x))
  .abort_bad_request("{.arg {name}} must be a YYYY-MM-DD string or a Date, not {.val {x}}.")
}

.magnus_excerpt <- function(x) {
  substr(gsub("\\s+", " ", paste(x, collapse = " ")), 1, 200)
}

.magnus_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

# Python `str(x)` for a scalar raw value (NULL -> "None").
.magnus_str <- function(x) {
  if (is.null(x) || length(x) != 1L || is.list(x) || is.na(x)) return("None")
  if (is.logical(x)) return(if (x) "True" else "False")
  as.character(x)
}

# Accents, case and whitespace folded away (sdv-py `_fold`). Decomposed
# accents are dropped as combining marks; precomposed Latin-1 letters are
# mapped by table.
.magnus_fold <- function(text) {
  from <- intToUtf8(c(224:229, 231, 232:235, 236:239, 241, 242:246, 249:252, 253, 255))
  to <- "aaaaaaceeeeiiiinooooouuuuyy"
  x <- tolower(enc2utf8(text))
  x <- gsub(paste0("[", intToUtf8(0x300:0x36F), "]"), "", x, perl = TRUE)
  x <- chartr(from, to, x)
  trimws(gsub(paste0("[[:space:]", intToUtf8(c(0xA0, 0x202F)), "]+"), " ", x))
}

.magnus_is_no_data_message <- function(data) {
  is.character(data) && length(data) == 1L && !is.na(data) &&
    grepl(.MAGNUS_NO_DATA_MESSAGE, .magnus_fold(data), fixed = TRUE)
}

# ---------------------------------------------------------------------------
# Season -> competition_id (sdv-py `_competition_id`)
# ---------------------------------------------------------------------------

# {season: competition_id} from one get_competition_by_type request, cached
# for the session after a successful lookup.
.magnus_live_seasons <- function() {
  if (!is.null(.magnus_state$live)) return(.magnus_state$live)
  body <- .magnus_get(.MAGNUS_AJAX, list(action = "get_competition_by_type", id_type = .MAGNUS_TYPE))
  out <- integer()
  for (row in .magnus_dicts(.magnus_at(body, "data", "data"))) {
    season <- .magnus_int(row[["saison"]])
    cid <- .magnus_int(row[["id"]])
    if (!is.na(season) && !is.na(cid)) out[[as.character(season)]] <- as.integer(cid)
  }
  .magnus_state$live <- out
  out
}

# The competition_id of a season given by its END year (2027 = 2026-27); NULL
# passes through (no competition filter).
.magnus_competition_id <- function(season) {
  if (is.null(season)) return(NULL)
  end <- .magnus_arg_int(season)
  if (is.na(end)) {
    .abort_bad_request("{.arg season} must be a year (END year, 2027 = 2026-27); got {.val {season}}.")
  }
  if (end < .MAGNUS_FIRST_SEASON) {
    first <- .MAGNUS_FIRST_SEASON
    .abort_no_data("Ligue Magnus data on liguemagnus.com starts in 2020-21 (season {first}); got {end}.")
  }
  key <- as.character(as.integer(end))
  if (is.na(.magnus_state$seasons[key])) {
    live <- .magnus_live_seasons()
    .magnus_state$seasons[names(live)] <- live
  }
  if (is.na(.magnus_state$seasons[key])) {
    .abort_no_data("liguemagnus.com lists no Ligue Magnus competition for season {end}.")
  }
  unname(.magnus_state$seasons[[key]])
}

# ---------------------------------------------------------------------------
# Transport and refusals (sdv-py magnus_runtime.py)
# ---------------------------------------------------------------------------

# `_rows`: the row list of an admin-ajax `data` object (NULL when missing).
.magnus_rows <- function(action, data) {
  inner <- .magnus_at(data, "data")
  if (identical(action, "get_rencontrestours")) return(.magnus_at(inner, "rencontres"))
  if (identical(action, "get_classementsphase")) return(.magnus_at(inner, "positions"))
  inner
}

# action -> list(request param, row -> the value that param scopes).
.MAGNUS_SCOPE <- list(
  get_competition_by_type = list(list("id_type", function(r) .magnus_at(r, "type", "id"))),
  get_rencontres = list(
    list("phase_id", function(r) r[["phase_id"]]),
    list("competition_id", function(r) .magnus_at(r, "competition", "id"))
  ),
  get_liste_tours = list(list("phase_id", function(r) r[["phase_id"]]))
)

# Return `body` when it is a complete, in-scope admin-ajax answer; raise otherwise.
.magnus_check_ajax <- function(url, body, params) {
  where <- .euro_where(url)
  action <- params[["action"]]
  ok_success <- .magnus_is_obj(body) && is.logical(body[["success"]]) &&
    length(body[["success"]]) == 1L && !is.na(body[["success"]])
  if (!ok_success) {
    .abort_asset_fetch("{where} ({action}): the body is not a {{success, data}} envelope.")
  }
  data <- body[["data"]]
  if (!isTRUE(body[["success"]])) {
    detail <- .magnus_excerpt(if (is.character(data) && length(data) == 1L) data else .magnus_json(data))
    if (.magnus_is_no_data_message(data)) {
      .abort_no_data("{where} ({action}) answered success=false: {detail}")
    }
    .abort_asset_fetch("{where} ({action}) answered success=false with an unknown message: {detail}")
  }
  if (is.null(data) || identical(data, "") || (is.list(data) && length(data) == 0L)) {
    .abort_no_data("{where} ({action}) answered an empty data stub (a required parameter missing or unknown).")
  }
  if (.magnus_is_obj(data) && all(names(data) %in% names(params)) &&
      all(vapply(data, .magnus_is_arr, logical(1)))) {
    detail <- .magnus_excerpt(.magnus_json(data))
    .abort_bad_request("{where} ({action}) rejected the request: {detail}")
  }
  rows <- .magnus_rows(action, data)
  if (!.magnus_is_arr(rows)) .abort_asset_fetch("{where} ({action}): the body carries no row list.")
  if (length(rows) == 0L) .abort_no_data("{where} ({action}) answered zero rows.")
  if (!all(vapply(rows, .magnus_is_obj, logical(1)))) {
    .abort_asset_fetch("{where} ({action}): the body has non-object rows.")
  }
  act <- if (is.character(action) && length(action) == 1L && !is.na(action)) action else ""
  for (sc in .MAGNUS_SCOPE[[act]]) {
    param <- sc[[1]]
    if (is.null(params[[param]])) next
    want <- .magnus_int(params[[param]])
    bad <- vapply(rows, function(r) {
      got <- .magnus_int(sc[[2]](r))
      is.na(got) || is.na(want) || got != want
    }, logical(1))
    if (any(bad)) {
      n_bad <- sum(bad)
      n_rows <- length(rows)
      .abort_asset_fetch("{where} ({action}): {n_bad} of {n_rows} rows are outside {param}={want}.")
    }
  }
  if (identical(action, "get_rencontrestours")) {
    rnd <- .magnus_at(data, "data")
    got <- c(.magnus_int(.magnus_at(rnd, "phase_id")), .magnus_int(.magnus_at(rnd, "tour")))
    want <- c(.magnus_int(params[["phase_id"]]), .magnus_int(params[["journee"]]))
    if (anyNA(got) || anyNA(want) || !all(got == want)) {
      got_s <- paste(got, collapse = "/")
      want_s <- paste(want, collapse = "/")
      .abort_asset_fetch("{where} ({action}): served phase/round {got_s}, asked {want_s}.")
    }
  }
  if (identical(action, "get_rencontres")) {
    meta <- .magnus_at(data, "meta")
    if (!.magnus_is_obj(meta)) {
      .abort_asset_fetch("{where} ({action}): no paging meta, so the list cannot be verified complete.")
    }
    last_page <- meta[["last_page"]]
    total <- meta[["total"]]
    # type-strict: a logical, a float or a digit string is not a page count.
    one_page <- is.integer(last_page) && length(last_page) == 1L && !is.na(last_page) && last_page == 1L &&
      is.integer(total) && length(total) == 1L && !is.na(total) && total == length(rows)
    if (!one_page) {
      cur <- .magnus_json(meta[["current_page"]])
      lp <- .magnus_json(last_page)
      tot <- .magnus_json(total)
      n_rows <- length(rows)
      .abort_asset_fetch(paste0(
        "{where} ({action}): page {cur}/{lp} of {tot} games served {n_rows}; ",
        "a partial schedule is not returned as a whole one."
      ))
    }
  }
  body
}

# The page text of a game sheet URL, throttled per host. An empty 2xx body is
# a block, not "nothing".
.magnus_page_text <- function(url) {
  resp <- .build_request(url) |> .polite_perform()
  .euro_check_status(resp)
  if (!httr2::resp_has_body(resp)) {
    where <- .euro_where(url)
    status <- httr2::resp_status(resp)
    .abort_asset_fetch("{where} answered HTTP {status} with an empty body.")
  }
  .resp_text(resp)
}

# HTML-unescape an attribute value (named and numeric entities) with libxml2.
.magnus_unescape <- function(x) {
  doc <- rvest::read_html(paste0("<html><body><p>", x, "</p></body></html>"), encoding = "UTF-8")
  rvest::html_text(rvest::html_element(doc, "p"), trim = FALSE)
}

# The first JSON object in a <live-rencontre-container :data="..."> attribute,
# unescaped; NULL when the page has none. Anchored on the custom element so a
# :data attribute elsewhere is never taken for the game sheet.
.magnus_extract_sheet <- function(page) {
  if (!is.character(page) || length(page) != 1L || is.na(page)) return(NULL)
  pat <- "<live-rencontre-container\\b[^>]*?\\s:data=\"[^\"]*\""
  hits <- regmatches(page, gregexpr(pat, page, perl = TRUE))[[1]]
  for (hit in hits) {
    attr <- sub("(?s)^.*?\\s:data=\"([^\"]*)\"$", "\\1", hit, perl = TRUE)
    text <- trimws(.magnus_unescape(attr))
    if (!startsWith(text, "{")) next # a JSON object only (never a path or URL for jsonlite to open)
    body <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
    if (.magnus_is_obj(body)) return(body)
  }
  NULL
}

# Fetch /rencontre/{id}/ and return its checked game sheet.
.magnus_game_sheet <- function(url, game_id) {
  where <- .euro_where(url)
  sheet <- .magnus_extract_sheet(.magnus_page_text(url))
  if (is.null(sheet)) {
    .abort_asset_fetch("{where}: no <live-rencontre-container :data> game sheet in the page.")
  }
  keys <- names(sheet)
  if ((length(sheet) > 0L && all(keys %in% .MAGNUS_ERROR_KEYS)) || !"evenements" %in% keys) {
    detail <- .magnus_excerpt(.magnus_json(sheet))
    .abort_no_data("{where}: no game sheet for game {game_id}: {detail}")
  }
  events <- sheet[["evenements"]]
  if (!.magnus_is_arr(events)) .abort_asset_fetch("{where}: the game sheet's evenements is not a list.")
  if (length(events) == 0L) .abort_no_data("{where}: game {game_id} has no events yet (unplayed).")
  scores <- sheet[["scores"]]
  ids <- if (.magnus_is_arr(scores)) {
    unique(vapply(scores, function(s) .magnus_int(.magnus_at(s, "sportif_rencontre_id")), numeric(1)))
  } else {
    numeric()
  }
  if (!(length(ids) == 1L && !is.na(ids) && ids == as.numeric(game_id))) {
    got <- paste(sort(ids), collapse = ", ")
    .abort_asset_fetch("{where}: the sheet's scores are for games [{got}], asked {game_id}.")
  }
  staffs <- sheet[["staffs"]]
  if (.magnus_is_arr(staffs)) {
    for (i in seq_along(staffs)) {
      if (.magnus_is_obj(staffs[[i]])) staffs[[i]][["ddn"]] <- NULL # personal data, never returned
    }
    sheet[["staffs"]] <- staffs
  }
  sheet
}

# GET a Ligue Magnus route: an admin-ajax action as checked JSON, or a game
# page's game sheet (sdv-py `_get`).
.magnus_get <- function(url, params = list()) {
  clean <- Filter(Negate(is.null), params)
  m <- regmatches(httr2::url_parse(url)$path, regexec("^/rencontre/([0-9]+)/?$", httr2::url_parse(url)$path))[[1]]
  if (length(m) == 2L) return(.magnus_game_sheet(url, m[[2]]))
  .magnus_check_ajax(url, .euro_get(url, clean, simplifyVector = FALSE), clean)
}

# ---------------------------------------------------------------------------
# Frame builder (sdv-py `_frame` / `_coerce`)
# ---------------------------------------------------------------------------

.magnus_spec_col <- function(type, get) list(type = type, get = get)
.magnus_g <- function(...) {
  keys <- c(...)
  function(row) do.call(.magnus_at, c(list(row), as.list(keys)))
}

.magnus_col_integer <- function(vals) {
  v <- vapply(vals, .magnus_int, numeric(1))
  if (any(abs(v) >= 2^31, na.rm = TRUE)) {
    .abort_asset_fetch("A numeric column holds a value beyond the 32-bit integer range.")
  }
  as.integer(v)
}

.magnus_col_character <- function(vals) {
  vapply(vals, function(v) {
    if (is.null(v) || length(v) != 1L || is.list(v) || is.logical(v) || is.na(v)) return(NA_character_)
    if (is.character(v)) return(if (nzchar(trimws(v))) enc2utf8(v) else NA_character_)
    if (is.numeric(v)) as.character(v) else NA_character_
  }, character(1))
}

.magnus_col_logical <- function(vals) {
  vapply(vals, function(v) if (is.logical(v) && length(v) == 1L && !is.na(v)) v else NA, logical(1))
}

.magnus_col_intlist <- function(vals) {
  lapply(vals, function(v) {
    if (!.magnus_is_arr(v)) return(NULL)
    out <- vapply(v, .magnus_int, numeric(1))
    if (any(abs(out) >= 2^31, na.rm = TRUE)) .abort_asset_fetch("An id list holds a value beyond the integer range.")
    as.integer(out)
  })
}

# DD/MM/YYYY -> Date (NA otherwise).
.magnus_col_date <- function(vals) {
  s <- .magnus_col_character(vals)
  ok <- !is.na(s) & grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", s)
  out <- as.Date(rep(NA_character_, length(s)))
  out[ok] <- as.Date(s[ok], format = "%d/%m/%Y")
  out
}

# A published game time as a Europe/Paris POSIXct: local `YYYY-MM-DD HH:MM[:SS]`
# or UTC ISO `...Z` (a round body).
.magnus_col_datetime <- function(vals) {
  s <- .magnus_col_character(vals)
  secs <- rep(NA_real_, length(s))
  local <- !is.na(s) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}(:[0-9]{2})?$", s)
  if (any(local)) {
    txt <- ifelse(nchar(s[local]) == 16L, paste0(s[local], ":00"), s[local])
    secs[local] <- as.numeric(as.POSIXct(txt, format = "%Y-%m-%d %H:%M:%S", tz = .MAGNUS_TZ))
  }
  utc <- !is.na(s) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$", s)
  if (any(utc)) {
    base <- as.numeric(as.POSIXct(substr(s[utc], 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    frac <- suppressWarnings(as.numeric(sub("^.{19}(\\.[0-9]+)?Z$", "\\1", s[utc])))
    secs[utc] <- base + ifelse(is.na(frac), 0, frac)
  }
  structure(secs, class = c("POSIXct", "POSIXt"), tzone = .MAGNUS_TZ)
}

.magnus_empty <- function(spec) {
  base <- vapply(spec, function(s) {
    switch(s$type, integer = "integer", character = "character", logical = "logical",
           date = "Date", datetime = "POSIXct", intlist = "logical")
  }, character(1))
  out <- .empty_tibble(base)
  for (nm in names(spec)) {
    if (spec[[nm]]$type == "datetime") {
      col <- out[[nm]]
      attr(col, "tzone") <- .MAGNUS_TZ
      out[[nm]] <- col
    } else if (spec[[nm]]$type == "intlist") {
      out[[nm]] <- list()
    }
  }
  out
}

# One row per mapping, each column read by its getter and coerced to its type;
# a leaf of the wrong type becomes NA rather than raising.
.magnus_frame <- function(rows, spec) {
  if (length(rows) == 0L) return(.magnus_empty(spec))
  cols <- lapply(spec, function(s) {
    vals <- lapply(rows, s$get)
    switch(s$type,
      integer = .magnus_col_integer(vals),
      character = .magnus_col_character(vals),
      logical = .magnus_col_logical(vals),
      date = .magnus_col_date(vals),
      datetime = .magnus_col_datetime(vals),
      intlist = .magnus_col_intlist(vals)
    )
  })
  tibble::as_tibble(cols)
}

# The admin-ajax data.data of an envelope, or of its inner data object.
.magnus_payload <- function(raw) {
  body <- if (.magnus_is_obj(raw)) raw else list()
  if ("success" %in% names(body)) .magnus_at(body, "data", "data") else body[["data"]]
}

# ---------------------------------------------------------------------------
# Parsers: admin-ajax (sdv-py magnus_parsers.py)
# ---------------------------------------------------------------------------

.MAGNUS_COMPETITIONS <- list(
  competition_id = .magnus_spec_col("integer", .magnus_g("id")),
  season = .magnus_spec_col("integer", .magnus_g("saison")),
  competition = .magnus_spec_col("character", .magnus_g("libelle")),
  competition_type_id = .magnus_spec_col("integer", .magnus_g("type", "id")),
  competition_type_code = .magnus_spec_col("character", .magnus_g("type", "code")),
  competition_type = .magnus_spec_col("character", .magnus_g("type", "libelle")),
  external_url = .magnus_spec_col("character", .magnus_g("parametres", "url_externe")),
  published = .magnus_spec_col("logical", .magnus_g("parametres", "publier_sur_site"))
)

.magnus_parse_competitions <- function(raw) {
  .magnus_frame(.magnus_dicts(.magnus_payload(raw)), .MAGNUS_COMPETITIONS)
}

.MAGNUS_PHASES <- list(
  phase_id = .magnus_spec_col("integer", .magnus_g("id")),
  phase = .magnus_spec_col("character", .magnus_g("libelle")),
  phase_type_code = .magnus_spec_col("character", .magnus_g("type_phase")),
  visible = .magnus_spec_col("logical", .magnus_g("visible")),
  competition = .magnus_spec_col("character", .magnus_g("competition_libelle")),
  n_teams = .magnus_spec_col("integer", .magnus_g("parametrage", "nombre_participant")),
  overtime = .magnus_spec_col("logical", .magnus_g("parametrage", "prolongation")),
  games_per_matchup = .magnus_spec_col("integer", .magnus_g("parametrage", "nombre_match_par_confrontation"))
)

.magnus_parse_phases <- function(raw) {
  .magnus_frame(.magnus_dicts(.magnus_payload(raw)), .MAGNUS_PHASES)
}

# The team columns of a team object at `path` (a get_equipes row when empty).
.magnus_team_spec <- function(prefix, ...) {
  out <- list(
    .magnus_spec_col("integer", .magnus_g(..., "id")),
    .magnus_spec_col("character", .magnus_g(..., "libelle_complet")),
    .magnus_spec_col("character", .magnus_g(..., "abreviation"))
  )
  names(out) <- paste0(prefix, c("team_id", "team", "team_abbreviation"))
  out
}

.MAGNUS_TEAMS <- c(
  .magnus_team_spec(""),
  list(
    team_short_name = .magnus_spec_col("character", .magnus_g("libelle_court")),
    team_label = .magnus_spec_col("character", .magnus_g("libelle")),
    gender = .magnus_spec_col("character", .magnus_g("sexe")),
    logo_url = .magnus_spec_col("character", .magnus_g("logo")),
    club_id = .magnus_spec_col("integer", .magnus_g("structure", "id")),
    club = .magnus_spec_col("character", .magnus_g("structure", "nom")),
    club_code = .magnus_spec_col("character", .magnus_g("structure", "code")),
    competition_ids = .magnus_spec_col("intlist", .magnus_g("id_compet"))
  )
)

.magnus_parse_teams <- function(raw) {
  .magnus_frame(.magnus_dicts(.magnus_payload(raw)), .MAGNUS_TEAMS)
}

# The score row of `side`'s team, matched by team id. score[] is not
# positional: it can list the away team first, and some finished games carry
# extra 0-goal rows for teams that did not play (2026-27 games 77751 / 77753 /
# 77754).
.magnus_score <- function(game, side, key) {
  team <- .magnus_int(.magnus_at(game, side, "id"))
  sc <- game[["score"]]
  if (!.magnus_truthy(sc)) sc <- game[["scores"]]
  if (is.na(team)) return(NULL)
  hits <- Filter(function(s) isTRUE(.magnus_int(s[["equipe_id"]]) == team), .magnus_dicts(sc))
  if (length(hits) == 1L) hits[[1]][[key]] else NULL
}

# A side's goals once the game is final or live; unplayed games publish a placeholder 0-0.
.magnus_result <- function(game, side) {
  played <- identical(game[["etat"]], "T") || isTRUE(game[["en_cours"]])
  if (played) .magnus_int(.magnus_score(game, side, "score")) else NA_real_
}

.magnus_status <- function(game) {
  if (identical(game[["etat"]], "T")) return("final")
  if (isTRUE(game[["en_cours"]])) "in_progress" else "scheduled"
}

.MAGNUS_SCHEDULE <- c(
  list(
    game_id = .magnus_spec_col("integer", .magnus_g("id")),
    season = .magnus_spec_col("integer", .magnus_g("competition", "saison")),
    competition_id = .magnus_spec_col("integer", .magnus_g("competition", "id")),
    phase_id = .magnus_spec_col("integer", function(g) {
      if (!is.null(g[["phase_id"]])) g[["phase_id"]] else .magnus_at(g, "phase", "id")
    }),
    phase = .magnus_spec_col("character", .magnus_g("phase", "libelle")),
    round = .magnus_spec_col("integer", .magnus_g("tour")),
    game_datetime = .magnus_spec_col("datetime", .magnus_g("date_rencontre")),
    status = .magnus_spec_col("character", .magnus_status),
    decision = .magnus_spec_col("character", function(g) {
      if (!identical(g[["etat"]], "T")) return(NULL)
      key <- .magnus_str(g[["victoire_par"]])
      if (key %in% names(.MAGNUS_DECISIONS)) .MAGNUS_DECISIONS[[key]] else NULL
    })
  ),
  .magnus_team_spec("home_", "receveur"),
  .magnus_team_spec("away_", "visiteur"),
  list(
    home_score = .magnus_spec_col("integer", function(g) .magnus_result(g, "receveur")),
    away_score = .magnus_spec_col("integer", function(g) .magnus_result(g, "visiteur")),
    home_team_participant_id = .magnus_spec_col("integer", function(g) .magnus_score(g, "receveur", "participant_id")),
    away_team_participant_id = .magnus_spec_col("integer", function(g) .magnus_score(g, "visiteur", "participant_id")),
    venue_id = .magnus_spec_col("integer", .magnus_g("lieu_de_pratique", "id")),
    venue = .magnus_spec_col("character", .magnus_g("lieu_de_pratique", "nom")),
    game_label = .magnus_spec_col("character", .magnus_g("rencontre_libelle"))
  )
)

# `get_rencontres` games, or one `get_rencontrestours` round (its games inherit
# the round's competition, phase and round; its times are UTC ISO).
.magnus_parse_schedule <- function(raw) {
  payload <- .magnus_payload(raw)
  if (.magnus_is_obj(payload)) {
    scope <- list(
      competition = payload[["competition"]], phase = payload[["phase"]],
      phase_id = payload[["phase_id"]], tour = payload[["tour"]]
    )
    games <- lapply(.magnus_dicts(payload[["rencontres"]]), function(g) c(g, scope[setdiff(names(scope), names(g))]))
  } else {
    games <- .magnus_dicts(payload)
  }
  .magnus_frame(games, .MAGNUS_SCHEDULE)
}

.MAGNUS_ROUNDS <- list(
  round_id = .magnus_spec_col("integer", .magnus_g("id")),
  round = .magnus_spec_col("integer", .magnus_g("tour")),
  round_label = .magnus_spec_col("character", .magnus_g("libelle")),
  phase_id = .magnus_spec_col("integer", .magnus_g("phase_id")),
  phase = .magnus_spec_col("character", .magnus_g("phase", "libelle")),
  competition_id = .magnus_spec_col("integer", .magnus_g("competition", "id")),
  season = .magnus_spec_col("integer", .magnus_g("competition", "saison")),
  start_date = .magnus_spec_col("date", .magnus_g("date_debut")),
  end_date = .magnus_spec_col("date", .magnus_g("date_fin"))
)

.magnus_parse_rounds <- function(raw) {
  .magnus_frame(.magnus_dicts(.magnus_payload(raw)), .MAGNUS_ROUNDS)
}

# The last results as W / L / T letters, in the order the body lists them.
.magnus_form <- function(position) {
  me <- .magnus_int(position[["participant_id"]])
  letters <- character()
  for (game in .magnus_dicts(position[["dernier_resultats"]])) {
    winner <- .magnus_int(game[["participant_gagnant_id"]])
    if (isTRUE(game[["match_nul"]])) {
      letters <- c(letters, "T")
    } else if (!is.na(me) && !is.na(winner)) {
      letters <- c(letters, if (winner == me) "W" else "L")
    }
  }
  if (length(letters)) paste(letters, collapse = "") else NULL
}

.MAGNUS_STANDINGS <- list(
  rank = .magnus_spec_col("integer", .magnus_g("position")),
  team_id = .magnus_spec_col("integer", .magnus_g("equipe", "id")),
  team = .magnus_spec_col("character", .magnus_g("equipe", "nom_court")),
  team_abbreviation = .magnus_spec_col("character", .magnus_g("equipe", "abreviation")),
  team_participant_id = .magnus_spec_col("integer", .magnus_g("participant_id")),
  games_played = .magnus_spec_col("integer", .magnus_g("nombre_rencontres_joues")),
  wins = .magnus_spec_col("integer", .magnus_g("nombre_victoire")),
  ot_wins = .magnus_spec_col("integer", .magnus_g("nombre_vprl")),
  ot_losses = .magnus_spec_col("integer", .magnus_g("nombre_dprl")),
  losses = .magnus_spec_col("integer", .magnus_g("nombre_defaite")),
  ties = .magnus_spec_col("integer", .magnus_g("nombre_nul")),
  points = .magnus_spec_col("integer", .magnus_g("nombre_point")),
  goals_for = .magnus_spec_col("integer", .magnus_g("nombre_but_marque")),
  goals_against = .magnus_spec_col("integer", .magnus_g("nombre_but_concede")),
  penalty_minutes = .magnus_spec_col("integer", .magnus_g("nombre_penalites")),
  last_results = .magnus_spec_col("character", .magnus_form),
  standings_id = .magnus_spec_col("integer", .magnus_g("standings_id"))
)

.magnus_parse_standings <- function(raw) {
  table <- .magnus_payload(raw)
  standings_id <- .magnus_at(table, "classement_id")
  rows <- lapply(.magnus_dicts(.magnus_at(table, "positions")), function(p) {
    p[["standings_id"]] <- standings_id
    p
  })
  .magnus_frame(rows, .MAGNUS_STANDINGS)
}

# ---------------------------------------------------------------------------
# Parsers: game sheet
# ---------------------------------------------------------------------------

.magnus_sheet <- function(raw) if (.magnus_is_obj(raw)) raw else list()

# The game id: the sheet has no id of its own, its scores[] rows carry it.
.magnus_game_id <- function(sheet) {
  ids <- vapply(.magnus_dicts(sheet[["scores"]]), function(s) .magnus_int(s[["sportif_rencontre_id"]]), numeric(1))
  ids <- unique(ids[!is.na(ids)])
  if (length(ids) == 1L) (if (abs(ids) < 2^31) as.integer(ids) else ids) else NULL
}

# The person id of a player object (`id`, else `personne.id`).
.magnus_person_id <- function(obj) {
  pid <- .magnus_int(.magnus_at(obj, "id"))
  if (!is.na(pid)) pid else .magnus_int(.magnus_at(obj, "personne", "id"))
}

.MAGNUS_GAME <- c(
  list(
    game_id = .magnus_spec_col("integer", .magnus_game_id),
    competition = .magnus_spec_col("character", .magnus_g("competition")),
    phase = .magnus_spec_col("character", .magnus_g("phase")),
    round = .magnus_spec_col("integer", .magnus_g("journee")),
    game_datetime = .magnus_spec_col("datetime", .magnus_g("date_rencontre_non_formate")),
    status = .magnus_spec_col("character", function(s) {
      if (isTRUE(s[["est_terminee"]]) || identical(s[["etat"]], "T")) {
        "final"
      } else if (isTRUE(s[["est_debute"]])) {
        "in_progress"
      } else {
        "scheduled"
      }
    })
  ),
  .magnus_team_spec("home_", "receveur"),
  .magnus_team_spec("away_", "visiteur"),
  list(
    home_score = .magnus_spec_col("integer", function(s) .magnus_score(s, "receveur", "score")),
    away_score = .magnus_spec_col("integer", function(s) .magnus_score(s, "visiteur", "score")),
    home_team_participant_id = .magnus_spec_col("integer", function(s) .magnus_score(s, "receveur", "participant_id")),
    away_team_participant_id = .magnus_spec_col("integer", function(s) .magnus_score(s, "visiteur", "participant_id")),
    has_shootout = .magnus_spec_col("logical", function(s) length(.magnus_dicts(s[["tirs_aux_but"]])) > 0L),
    n_periods = .magnus_spec_col("integer", .magnus_g("periodes", "nombre")),
    period_minutes = .magnus_spec_col("integer", .magnus_g("periodes", "duree")),
    attendance = .magnus_spec_col("integer", .magnus_g("nombre_spectateurs")),
    venue = .magnus_spec_col("character", .magnus_g("patinoire"))
  )
)

.magnus_parse_game <- function(raw) {
  sheet <- .magnus_sheet(raw)
  is_sheet <- any(c("evenements", "scores") %in% names(sheet)) # an envelope or an error object is not a game
  .magnus_frame(if (is_sheet) list(sheet) else list(), .MAGNUS_GAME)
}

# The period of an event from its elapsed game seconds (regulation periods are
# 1200 s). A faceoff at exactly 1200 / 2400 / 3600 opens the next period; any
# other event at a boundary belongs to the period ending there (game 77779 has
# shots at exactly 1200 that interleave with period-1 shots). Overtime is
# period 4 and up.
.magnus_period <- function(raw_type, seconds) {
  if (is.na(seconds)) return(NA_integer_)
  if (identical(raw_type, "ENGA")) return(as.integer(seconds %/% .MAGNUS_PERIOD_SECONDS + 1L))
  max(1L, as.integer(ceiling(seconds / .MAGNUS_PERIOD_SECONDS)))
}

.MAGNUS_ACTORS <- c(BUT = "buteur", TIR = "tireur", SANCTION = "joueur", ENGA = "gagnant")

# A numeric id as a lookup key (never scientific notation).
.magnus_key <- function(x) sprintf("%.0f", x)

# Player participant id (per game) -> person id, from `joueurs`.
.magnus_roster <- function(sheet) {
  out <- integer()
  for (player in .magnus_dicts(sheet[["joueurs"]])) {
    pid <- .magnus_int(player[["participant_id"]])
    if (!is.na(pid)) out[[.magnus_key(pid)]] <- .magnus_person_id(player)
  }
  out
}

.magnus_event_row <- function(event, game_id, roster) {
  raw_type <- event[["type"]]
  rt <- if (is.character(raw_type) && length(raw_type) == 1L && !is.na(raw_type)) raw_type else NULL
  seconds <- .magnus_int(event[["temps"]])
  period <- .magnus_period(rt, seconds)
  actor <- if (!is.null(rt) && rt %in% names(.MAGNUS_ACTORS)) .MAGNUS_ACTORS[[rt]] else NULL
  assists <- .magnus_dicts(event[["assistants"]])
  on_ice <- function(key) {
    v <- event[[key]]
    if (!.magnus_is_arr(v)) return(NULL)
    ids <- vapply(v, .magnus_int, numeric(1))
    ids <- ids[!is.na(ids)]
    as.list(unname(roster[.magnus_key(ids)]))
  }
  type_str <- .magnus_str(raw_type)
  list(
    game_id = game_id,
    event_id = event[["id"]],
    event_type = if (type_str %in% names(.MAGNUS_EVENT_TYPES)) {
      .MAGNUS_EVENT_TYPES[[type_str]]
    } else if (.magnus_truthy(raw_type)) {
      tolower(type_str)
    } else {
      NULL
    },
    event_type_raw = raw_type,
    period = period,
    period_seconds = if (!is.na(seconds) && !is.na(period)) as.integer(seconds - (period - 1L) * .MAGNUS_PERIOD_SECONDS) else NA_integer_,
    game_seconds = seconds,
    team_id = .magnus_at(event, "equipe", "id"),
    team_abbreviation = .magnus_at(event, "equipe", "abreviation"),
    player_id = if (!is.null(actor)) .magnus_person_id(event[[actor]]) else NA_real_,
    player_name = if (!is.null(actor)) .magnus_at(event, actor, "nom_complet") else NULL,
    assist1_player_id = if (length(assists) >= 1L) .magnus_person_id(assists[[1]]) else NA_integer_,
    assist1_player_name = if (length(assists) >= 1L) assists[[1]][["nom_complet"]] else NULL,
    assist2_player_id = if (length(assists) >= 2L) .magnus_person_id(assists[[2]]) else NA_real_,
    assist2_player_name = if (length(assists) >= 2L) assists[[2]][["nom_complet"]] else NULL,
    goalie_player_id = .magnus_person_id(event[["gardien"]]),
    goalie_player_name = .magnus_at(event, "gardien", "nom_complet"),
    faceoff_home_player_id = if (identical(rt, "ENGA")) .magnus_person_id(event[["receveur"]]) else NA_real_,
    faceoff_away_player_id = if (identical(rt, "ENGA")) .magnus_person_id(event[["visiteur"]]) else NA_real_,
    strength_code = .magnus_at(event, "surnombre", "code"),
    penalty_code = .magnus_at(event, "sanction", "code"),
    penalty = .magnus_at(event, "sanction", "libelle"),
    penalty_minutes = event[["temps_penalite"]],
    penalty_start_seconds = event[["temps_debut_sanction"]],
    penalty_end_seconds = event[["temps_fin_sanction"]],
    goalie_in_player_id = .magnus_person_id(event[["entrant"]]),
    goalie_out_player_id = .magnus_person_id(event[["sortant"]]),
    on_ice_plus_player_ids = on_ice("joueurs_plus_ids"),
    on_ice_minus_player_ids = on_ice("joueurs_moins_ids")
  )
}

.MAGNUS_PBP_TYPES <- c(
  game_id = "integer", event_id = "character", event_type = "character", event_type_raw = "character",
  period = "integer", period_seconds = "integer", game_seconds = "integer", team_id = "integer",
  team_abbreviation = "character", player_id = "integer", player_name = "character",
  assist1_player_id = "integer", assist1_player_name = "character", assist2_player_id = "integer",
  assist2_player_name = "character", goalie_player_id = "integer", goalie_player_name = "character",
  faceoff_home_player_id = "integer", faceoff_away_player_id = "integer", strength_code = "character",
  penalty_code = "character", penalty = "character", penalty_minutes = "integer",
  penalty_start_seconds = "integer", penalty_end_seconds = "integer", goalie_in_player_id = "integer",
  goalie_out_player_id = "integer", on_ice_plus_player_ids = "intlist", on_ice_minus_player_ids = "intlist"
)
.MAGNUS_PBP <- Map(function(nm, type) .magnus_spec_col(type, .magnus_g(nm)), names(.MAGNUS_PBP_TYPES), .MAGNUS_PBP_TYPES)

# Sort key: period, elapsed second, the same-second tie-break, the event id's number.
.magnus_event_key <- function(rows) {
  num <- function(f, default) vapply(rows, function(r) {
    v <- f(r)
    if (length(v) != 1L || is.na(v)) default else as.numeric(v)
  }, numeric(1))
  tie <- vapply(rows, function(r) {
    t <- r$event_type_raw
    if (is.character(t) && length(t) == 1L && t %in% names(.MAGNUS_TYPE_ORDER)) {
      as.numeric(.MAGNUS_TYPE_ORDER[[t]])
    } else {
      length(.MAGNUS_TYPE_ORDER)
    }
  }, numeric(1))
  digits <- vapply(rows, function(r) {
    id <- r$event_id
    s <- if (is.null(id) || length(id) != 1L || is.na(id)) "" else as.character(id)
    m <- regmatches(s, gregexpr("[0-9]+", s))[[1]]
    if (length(m)) as.numeric(m[[length(m)]]) else 0
  }, numeric(1))
  order(num(function(r) r$period, Inf), num(function(r) r$game_seconds, Inf), tie, digits, method = "radix")
}

# Play-by-play from a game sheet's `evenements`, in game order. Every player
# column is a PERSON id; the on-ice lists, which the sheet gives in per-game
# participant ids, are translated through the roster (an id the roster lacks
# becomes NA inside the list).
.magnus_parse_pbp <- function(raw) {
  sheet <- .magnus_sheet(raw)
  game_id <- .magnus_game_id(sheet)
  roster <- .magnus_roster(sheet)
  rows <- lapply(.magnus_dicts(sheet[["evenements"]]), .magnus_event_row, game_id = game_id, roster = roster)
  if (length(rows)) rows <- rows[.magnus_event_key(rows)]
  .magnus_frame(rows, .MAGNUS_PBP)
}

.magnus_home_away <- function(sheet, team_id) {
  team <- .magnus_int(team_id)
  if (!is.na(team) && isTRUE(team == .magnus_int(.magnus_at(sheet, "receveur", "id")))) return("home")
  if (!is.na(team) && isTRUE(team == .magnus_int(.magnus_at(sheet, "visiteur", "id")))) return("away")
  NULL
}

.MAGNUS_LINEUPS <- list(
  game_id = .magnus_spec_col("integer", .magnus_g(".game_id")),
  team_id = .magnus_spec_col("integer", .magnus_g("equipe_id")),
  home_away = .magnus_spec_col("character", .magnus_g(".home_away")),
  player_id = .magnus_spec_col("integer", .magnus_person_id),
  player_participant_id = .magnus_spec_col("integer", .magnus_g("participant_id")),
  player_name = .magnus_spec_col("character", .magnus_g("nom_complet")),
  jersey_number = .magnus_spec_col("character", .magnus_g("numero")),
  registered_jersey_number = .magnus_spec_col("character", .magnus_g("personne", "numero")),
  licence_number = .magnus_spec_col("character", .magnus_g("code_adherent")),
  position_code = .magnus_spec_col("character", .magnus_g("position", "code")),
  position = .magnus_spec_col("character", function(p) {
    code <- .magnus_str(.magnus_at(p, "position", "code"))
    if (code %in% names(.MAGNUS_POSITIONS)) .MAGNUS_POSITIONS[[code]] else NULL
  }),
  captaincy = .magnus_spec_col("character", function(p) {
    codes <- vapply(.magnus_dicts(p[["attributs"]]), function(a) {
      if (.magnus_truthy(a[["code"]])) .magnus_str(a[["code"]]) else ""
    }, character(1))
    codes <- codes[nzchar(codes)]
    if (length(codes)) paste(codes, collapse = "") else NULL
  }),
  is_mvp = .magnus_spec_col("logical", .magnus_g(".is_mvp")),
  photo_url = .magnus_spec_col("character", .magnus_g("personne", "photo_url"))
)

# The game roster: one row per dressed player from a game sheet's `joueurs`.
.magnus_parse_lineups <- function(raw) {
  sheet <- .magnus_sheet(raw)
  game_id <- .magnus_game_id(sheet)
  mvps <- vapply(.magnus_dicts(sheet[["mvps"]]), function(m) .magnus_int(m[["participant_joueur_id"]]), numeric(1))
  mvps <- mvps[!is.na(mvps)]
  rows <- lapply(.magnus_dicts(sheet[["joueurs"]]), function(p) {
    p[[".game_id"]] <- game_id
    p[[".home_away"]] <- .magnus_home_away(sheet, p[["equipe_id"]])
    pid <- .magnus_int(p[["participant_id"]])
    p[[".is_mvp"]] <- !is.na(pid) && pid %in% mvps
    p
  })
  .magnus_frame(rows, .MAGNUS_LINEUPS)
}

.MAGNUS_SHOOTOUT <- list(
  game_id = .magnus_spec_col("integer", .magnus_g(".game_id")),
  attempt_id = .magnus_spec_col("integer", .magnus_g("id")),
  shootout_round = .magnus_spec_col("integer", .magnus_g("numero")),
  team_id = .magnus_spec_col("integer", .magnus_g("equipe", "id")),
  team_abbreviation = .magnus_spec_col("character", .magnus_g("equipe", "abreviation")),
  shooter_player_id = .magnus_spec_col("integer", function(a) .magnus_person_id(a[["tireur"]])),
  shooter_name = .magnus_spec_col("character", .magnus_g("tireur", "nom_complet")),
  goalie_player_id = .magnus_spec_col("integer", function(a) .magnus_person_id(a[["gardien"]])),
  goalie_name = .magnus_spec_col("character", .magnus_g("gardien", "nom_complet")),
  scored = .magnus_spec_col("logical", .magnus_g("marque"))
)

# Shootout attempts from a game sheet's `tirs_aux_but`, in round order.
.magnus_parse_shootout <- function(raw) {
  sheet <- .magnus_sheet(raw)
  game_id <- .magnus_game_id(sheet)
  rows <- lapply(.magnus_dicts(sheet[["tirs_aux_but"]]), function(a) {
    a[[".game_id"]] <- game_id
    a
  })
  out <- .magnus_frame(rows, .MAGNUS_SHOOTOUT)
  out[order(out$shootout_round, out$attempt_id, na.last = TRUE, method = "radix"), ]
}

# ---------------------------------------------------------------------------
# Exported wrappers
# ---------------------------------------------------------------------------

# Parse (or pass through) a fetched body and tag it.
.magnus_out <- function(raw, parser, return_parsed, label) {
  if (!return_parsed) return(raw)
  make_fastRhockey_data(parser(raw), paste("Ligue Magnus", label, "Information from liguemagnus.com"), Sys.time())
}

#' @title **Ligue Magnus competitions (one per season)**
#' @description Competitions of a competition type from liguemagnus.com
#'   (`get_competition_by_type`): one row per season, with the `competition_id`
#'   the other routes key a season by. Coverage starts in 2020-21.
#' @param competition_type Competition type: `1` = Ligue Magnus (default), `2` =
#'   Division 1.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name             |type     |description|
#'   |:---------------------|:--------|:----------|
#'   |competition_id       |integer  |Ligue Magnus competition id (integer); one per season, the key get_phases and get_rencontres take (1 = 2020-21 ... 240 = 2026-27).|
#'   |season               |integer  |Season by its END year (2027 = 2026-27), the federation's saison field.|
#'   |competition          |character|Competition name as published (sponsor name included, e.g., "Synerglace Ligue Magnus").|
#'   |competition_type_id  |integer  |Competition type id: 1 = Ligue Magnus, 2 = Division 1.|
#'   |competition_type_code|character|Competition type code (NA for the Ligue Magnus).|
#'   |competition_type     |character|Competition type label (e.g., "Ligue Magnus").|
#'   |external_url         |character|External site linked from the competition (streaming partner).|
#'   |published            |logical  |Whether the competition is published on the league site.|
#' @section Errors:
#' `fastRhockey_no_data`: `success = false` with "Aucune donnee disponible", an
#' empty `data` stub, zero rows, a season outside 2020-21 onwards, an unknown or
#' unplayed game. `fastRhockey_asset_fetch`: a failed fetch, `success = false`
#' with any other message, rows outside the requested scope, a partial schedule
#' page, or a game page without its embedded game sheet. `fastRhockey_bad_request`:
#' HTTP 400/422, a validation error naming a rejected parameter, or an invalid
#' argument.
#' @keywords Magnus Competitions
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_competitions())
#' }
magnus_competitions <- function(competition_type = 1, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_competition_by_type",
    id_type = .magnus_id_arg(competition_type, "competition_type", optional = TRUE)
  ))
  .magnus_out(raw, .magnus_parse_competitions, return_parsed, "Competitions")
}

#' @title **Ligue Magnus game header**
#' @description Game header from the game sheet (`/rencontre/{game_id}/`): teams,
#'   score, start time, status, attendance and rink (one row). Staff birth dates
#'   are dropped from the sheet before it is returned.
#' @param game_id Game id (`game_id` of [magnus_schedule()]), e.g. 77779 (2026-27,
#'   shootout and MVPs); also sampled: 68838, 865.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the game sheet extracted from the page's embedded JSON, as a list.
#' @return A tibble (`return_parsed = TRUE`; one row), else the game sheet list:
#'
#'   |col_name                |type     |description|
#'   |:------------------------|:--------|:----------|
#'   |game_id                 |integer  |Game id (integer join key).|
#'   |competition             |character|Competition name as published.|
#'   |phase                   |character|Phase name in French.|
#'   |round                   |integer  |Round (journée) number.|
#'   |game_datetime           |POSIXct  |Start time, Europe/Paris.|
#'   |status                  |character|Game state: final, in_progress or scheduled.|
#'   |home_team_id            |integer  |Team id of the home team (integer).|
#'   |home_team               |character|Display name of the home team.|
#'   |home_team_abbreviation  |character|Abbreviation of the home team.|
#'   |away_team_id            |integer  |Team id of the away team (integer).|
#'   |away_team               |character|Display name of the away team.|
#'   |away_team_abbreviation  |character|Abbreviation of the away team.|
#'   |home_score              |integer  |Goals of the home team (the final score, shootout included).|
#'   |away_score              |integer  |Goals of the away team (the final score, shootout included).|
#'   |home_team_participant_id|integer  |Team participant id of the home team in this phase (integer; joins the standings).|
#'   |away_team_participant_id|integer  |Team participant id of the away team in this phase (integer; joins the standings).|
#'   |has_shootout            |logical  |Whether the game went to a shootout.|
#'   |n_periods               |integer  |Number of regulation periods.|
#'   |period_minutes          |integer  |Length of a regulation period in minutes.|
#'   |attendance              |integer  |Number of spectators (attendance).|
#'   |venue                   |character|Name of the rink (arena).|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Game
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_game(game_id = 77779))
#' }
magnus_game <- function(game_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(sprintf("%s/rencontre/%d/", .MAGNUS_HOST, .magnus_game_id_arg(game_id)))
  .magnus_out(raw, .magnus_parse_game, return_parsed, "Game")
}

#' @title **Ligue Magnus game roster**
#' @description Game roster from the game sheet: one row per dressed player, with
#'   the person id, the per-game participant id, jersey, licence number,
#'   position, captaincy and MVP flag. `player_id` (the person) is stable across
#'   games; `player_participant_id` is this game's roster entry, the key the
#'   sheet's on-ice lists and MVPs use.
#' @inheritParams magnus_game
#' @return A tibble (`return_parsed = TRUE`; zero rows for an empty sheet), else
#'   the game sheet list:
#'
#'   |col_name                |type     |description|
#'   |:------------------------|:--------|:----------|
#'   |game_id                 |integer  |Game id (integer join key).|
#'   |team_id                 |integer  |Team id (integer).|
#'   |home_away               |character|The team's side: home or away.|
#'   |player_id               |integer  |Person id (integer; joueurs[].id = personne.id), stable across games; the player key of every Magnus frame.|
#'   |player_participant_id   |integer  |The player's roster entry in THIS game (integer; changes every game), the key the sheet's on-ice lists and MVPs use; never join across games on it.|
#'   |player_name             |character|Player name (SURNAME First).|
#'   |jersey_number           |character|Jersey number worn in this game (string as published).|
#'   |registered_jersey_number|character|Registered jersey number of the person; can differ from the one worn.|
#'   |licence_number          |character|FFHG licence number (string; keeps leading zeros).|
#'   |position_code           |character|Position code: A (attaquant), D (défenseur), GB (gardien de but).|
#'   |position                |character|Position in English: forward, defence or goalie.|
#'   |captaincy               |character|C (captain) or A (alternate captain); null otherwise.|
#'   |is_mvp                  |logical  |Whether the player was named his team's MVP of the game.|
#'   |photo_url               |character|Photo thumbnail URL of the player.|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Lineups
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_lineups(game_id = 77779))
#' }
magnus_lineups <- function(game_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(sprintf("%s/rencontre/%d/", .MAGNUS_HOST, .magnus_game_id_arg(game_id)))
  .magnus_out(raw, .magnus_parse_lineups, return_parsed, "Lineups")
}

#' @title **Ligue Magnus play-by-play**
#' @description Play-by-play from the game sheet: goals, penalties, shots,
#'   faceoffs, goalie changes and timeouts in game order, with English event
#'   types and derived periods. The sheet groups events by type and has no
#'   sequence number, so events in the same second are ordered by a heuristic
#'   (shot, goal, penalty, goalie change, timeout, faceoff). There are no shot
#'   coordinates, and 2020-21 sheets carry no faceoff events. Every player column
#'   is a person id; the on-ice lists are translated from per-game participant
#'   ids through the roster.
#' @inheritParams magnus_game
#' @return A tibble (`return_parsed = TRUE`; zero rows for an empty sheet), else
#'   the game sheet list:
#'
#'   |col_name               |type     |description|
#'   |:-----------------------|:--------|:----------|
#'   |game_id                |integer  |Game id (integer join key).|
#'   |event_id               |character|Event id as published, the raw type plus a number (e.g., "BUT99238"); unique within a game.|
#'   |event_type             |character|Event type in English: goal, penalty, shot, faceoff, goalie_change or timeout.|
#'   |event_type_raw         |character|Event type as published: BUT, SANCTION, TIR, ENGA, REMPLACANT or TEMPS_MORT.|
#'   |period                 |integer  |Period derived from the elapsed seconds (4 = overtime). A faceoff at 1200 / 2400 / 3600 opens the next period; any other event at a boundary belongs to the period ending there.|
#'   |period_seconds         |integer  |Elapsed seconds within the period.|
#'   |game_seconds           |integer  |Elapsed game seconds (the sheet's temps).|
#'   |team_id                |integer  |Team id of the event's team (integer; for a faceoff, the winning team).|
#'   |team_abbreviation      |character|Abbreviation of the event's team.|
#'   |player_id              |integer  |Person id (integer, stable across games) of the acting player; the scorer, the shooter, the penalised player or the faceoff winner. Null on goalie changes, timeouts and bench penalties.|
#'   |player_name            |character|Name of the acting player (SURNAME First).|
#'   |assist1_player_id      |integer  |Person id of the first assist on a goal (integer).|
#'   |assist1_player_name    |character|Name of the first assist on a goal.|
#'   |assist2_player_id      |integer  |Person id of the second assist on a goal (integer).|
#'   |assist2_player_name    |character|Name of the second assist on a goal.|
#'   |goalie_player_id       |integer  |Person id of the goalie the goal or shot was against (integer).|
#'   |goalie_player_name     |character|Name of the goalie the goal or shot was against.|
#'   |faceoff_home_player_id |integer  |Person id of the home faceoff taker (integer).|
#'   |faceoff_away_player_id |integer  |Person id of the away faceoff taker (integer).|
#'   |strength_code          |character|Strength when a goal was scored: EG (even strength), SUP1 (power play, one man up); other codes unobserved.|
#'   |penalty_code           |character|Infraction code (e.g., CIN = slashing).|
#'   |penalty                |character|Infraction label in French (e.g., "Cinglage (coup de crosse)").|
#'   |penalty_minutes        |integer  |Penalty length in minutes.|
#'   |penalty_start_seconds  |integer  |Elapsed game seconds when the penalty starts.|
#'   |penalty_end_seconds    |integer  |Elapsed game seconds when the penalty ends.|
#'   |goalie_in_player_id    |integer  |Person id of the goalie entering on a goalie change (integer; never observed populated).|
#'   |goalie_out_player_id   |integer  |Person id of the goalie leaving on a goalie change (integer).|
#'   |on_ice_plus_player_ids |list     |Person ids of the scoring team's players on ice at a goal, goalie included (list of integer).|
#'   |on_ice_minus_player_ids|list     |Person ids of the conceding team's players on ice at a goal (list of integer).|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Play-by-play
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_pbp(game_id = 77779))
#' }
magnus_pbp <- function(game_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(sprintf("%s/rencontre/%d/", .MAGNUS_HOST, .magnus_game_id_arg(game_id)))
  .magnus_out(raw, .magnus_parse_pbp, return_parsed, "Play-by-play")
}

#' @title **Ligue Magnus phases of a season**
#' @description Phases of a competition (regular season, playoffs, relegation),
#'   with the `phase_id` the phase-scoped routes take.
#' @param season Season by its END year (2027 = 2026-27), sent as the season's
#'   `competition_id`. Coverage starts in 2020-21 (2021); an earlier season, or
#'   one the federation does not list, is `fastRhockey_no_data`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name         |type     |description|
#'   |:-----------------|:--------|:----------|
#'   |phase_id         |integer  |Phase id (integer), the value the phase_id argument of magnus_teams, magnus_rounds, magnus_round_games, magnus_standings and magnus_schedule takes. Ids change every season.|
#'   |phase            |character|Phase name in French (e.g., "Saison régulière" = regular season).|
#'   |phase_type_code  |character|Phase type code: C (championnat, round robin) or T (tableau, playoff bracket).|
#'   |visible          |logical  |Whether the phase is shown on the public site.|
#'   |competition      |character|Name of the competition (season) the phase belongs to.|
#'   |n_teams          |integer  |Number of teams in the phase.|
#'   |overtime         |logical  |Whether tied games go to overtime in the phase.|
#'   |games_per_matchup|integer  |Games each pair of teams plays in the phase.|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Phases
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_phases(season = 2027))
#' }
magnus_phases <- function(season, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_phases",
    competition_id = .magnus_competition_id(season)
  ))
  .magnus_out(raw, .magnus_parse_phases, return_parsed, "Phases")
}

#' @title **Ligue Magnus games of one round**
#' @description One round (journee) of a phase with its games
#'   (`get_rencontrestours`). A round body publishes UTC ISO times and no win
#'   type: `game_datetime` is converted to Europe/Paris and `decision` is `NA`.
#' @param phase_id Phase id (`phase_id` of [magnus_phases()]), e.g. 714 (the
#'   2026-27 regular season).
#' @param round Round number (tour), 1-based. Shadows the base function.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name                |type     |description|
#'   |:------------------------|:--------|:----------|
#'   |game_id                 |integer  |Game id (integer join key), the game_id argument of magnus_game, magnus_pbp, magnus_lineups and magnus_shootout.|
#'   |season                  |integer  |Season by its END year (2027 = 2026-27).|
#'   |competition_id          |integer  |Competition (season) id (integer).|
#'   |phase_id                |integer  |Phase id (integer; regular season, playoffs, relegation).|
#'   |phase                   |character|Phase name in French (e.g., "Saison régulière").|
#'   |round                   |integer  |Round (journée) number within the phase.|
#'   |game_datetime           |POSIXct  |Scheduled start, Europe/Paris time (get_rencontres publishes local time, a round body UTC; both are converted).|
#'   |status                  |character|Game state: final, in_progress or scheduled.|
#'   |decision                |character|How a final game was decided: regulation, overtime or shootout; null until the game is final (round bodies carry no decision, so null there too).|
#'   |home_team_id            |integer  |Team id of the home team (integer).|
#'   |home_team               |character|Display name of the home team.|
#'   |home_team_abbreviation  |character|Abbreviation of the home team.|
#'   |away_team_id            |integer  |Team id of the away team (integer).|
#'   |away_team               |character|Display name of the away team.|
#'   |away_team_abbreviation  |character|Abbreviation of the away team.|
#'   |home_score              |integer  |Goals of the home team (shootout winner included in the final score); null until the game starts (unplayed games publish a 0-0 placeholder).|
#'   |away_score              |integer  |Goals of the away team; null until the game starts.|
#'   |home_team_participant_id|integer  |Team participant id of the home team in this phase (integer; its own key space, joins team_participant_id of the standings; not a team id).|
#'   |away_team_participant_id|integer  |Team participant id of the away team in this phase (integer; joins team_participant_id of the standings).|
#'   |venue_id                |integer  |Rink id (integer).|
#'   |venue                   |character|Name of the rink (arena).|
#'   |game_label              |character|Game label as published ("HOME / AWAY").|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Schedule
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_round_games(phase_id = 714, round = 1))
#' }
magnus_round_games <- function(phase_id, round, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_rencontrestours",
    phase_id = .magnus_id_arg(phase_id, "phase_id"),
    journee = .magnus_id_arg(round, "round")
  ))
  .magnus_out(raw, .magnus_parse_schedule, return_parsed, "Round Games")
}

#' @title **Ligue Magnus rounds of a phase**
#' @description Rounds (journees) of a phase with their dates (games via
#'   [magnus_round_games()]). `round` is the value [magnus_round_games()] and
#'   [magnus_schedule()] take.
#' @param phase_id Phase id (`phase_id` of [magnus_phases()]), e.g. 714.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name      |type     |description|
#'   |:--------------|:--------|:----------|
#'   |round_id      |integer  |Round id (integer).|
#'   |round         |integer  |Round (journée) number within the phase, the round argument of magnus_round_games and magnus_schedule.|
#'   |round_label   |character|Round label; null in every observed body.|
#'   |phase_id      |integer  |Phase id (integer).|
#'   |phase         |character|Phase name in French.|
#'   |competition_id|integer  |Competition (season) id (integer).|
#'   |season        |integer  |Season by its END year (2027 = 2026-27).|
#'   |start_date    |Date     |First date of the round.|
#'   |end_date      |Date     |Last date of the round.|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Rounds
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_rounds(phase_id = 714))
#' }
magnus_rounds <- function(phase_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_liste_tours",
    phase_id = .magnus_id_arg(phase_id, "phase_id")
  ))
  .magnus_out(raw, .magnus_parse_rounds, return_parsed, "Rounds")
}

#' @title **Ligue Magnus schedule and results**
#' @description Games (schedule and results) of a season from `get_rencontres`,
#'   requested as one page (`par_page = 700`) that must be the whole list: a
#'   partial page is `fastRhockey_asset_fetch`, never returned as a whole
#'   schedule. `get_rencontres` publishes local times; a round body publishes
#'   UTC: both land in `game_datetime` as Europe/Paris. Scores are matched to the
#'   home and away teams by team id (`score[]` is not home-first and some
#'   finished games list extra 0-goal rows for teams that did not play); goals
#'   are `NA` until the game starts (unplayed games publish a 0-0 placeholder).
#' @param season Season by its END year (2027 = 2026-27), sent as the season's
#'   `competition_id`. Coverage starts in 2020-21 (2021); an earlier season, or
#'   one the federation does not list, is `fastRhockey_no_data`.
#' @param phase_id Phase id (`phase_id` of [magnus_phases()]). Omitted: every
#'   phase of the competition.
#' @param team_id Team id (`team_id` of [magnus_teams()]): that team's games only.
#' @param start_date Earliest game date, `YYYY-MM-DD` (a string or a `Date`).
#' @param end_date Latest game date, `YYYY-MM-DD` (a string or a `Date`).
#' @param round Round number (tour): that round's games only. Shadows the base
#'   function.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name                |type     |description|
#'   |:------------------------|:--------|:----------|
#'   |game_id                 |integer  |Game id (integer join key), the game_id argument of magnus_game, magnus_pbp, magnus_lineups and magnus_shootout.|
#'   |season                  |integer  |Season by its END year (2027 = 2026-27).|
#'   |competition_id          |integer  |Competition (season) id (integer).|
#'   |phase_id                |integer  |Phase id (integer; regular season, playoffs, relegation).|
#'   |phase                   |character|Phase name in French (e.g., "Saison régulière").|
#'   |round                   |integer  |Round (journée) number within the phase.|
#'   |game_datetime           |POSIXct  |Scheduled start, Europe/Paris time (get_rencontres publishes local time, a round body UTC; both are converted).|
#'   |status                  |character|Game state: final, in_progress or scheduled.|
#'   |decision                |character|How a final game was decided: regulation, overtime or shootout; null until the game is final (round bodies carry no decision, so null there too).|
#'   |home_team_id            |integer  |Team id of the home team (integer).|
#'   |home_team               |character|Display name of the home team.|
#'   |home_team_abbreviation  |character|Abbreviation of the home team.|
#'   |away_team_id            |integer  |Team id of the away team (integer).|
#'   |away_team               |character|Display name of the away team.|
#'   |away_team_abbreviation  |character|Abbreviation of the away team.|
#'   |home_score              |integer  |Goals of the home team (shootout winner included in the final score); null until the game starts (unplayed games publish a 0-0 placeholder).|
#'   |away_score              |integer  |Goals of the away team; null until the game starts.|
#'   |home_team_participant_id|integer  |Team participant id of the home team in this phase (integer; its own key space, joins team_participant_id of the standings; not a team id).|
#'   |away_team_participant_id|integer  |Team participant id of the away team in this phase (integer; joins team_participant_id of the standings).|
#'   |venue_id                |integer  |Rink id (integer).|
#'   |venue                   |character|Name of the rink (arena).|
#'   |game_label              |character|Game label as published ("HOME / AWAY").|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Schedule
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_schedule(season = 2027, phase_id = 714, round = 2))
#' }
magnus_schedule <- function(season, phase_id = NULL, team_id = NULL, start_date = NULL,
                            end_date = NULL, round = NULL, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_rencontres",
    par_page = 700L,
    competition_id = .magnus_competition_id(season),
    phase_id = .magnus_id_arg(phase_id, "phase_id", optional = TRUE),
    equipe_id = .magnus_id_arg(team_id, "team_id", optional = TRUE),
    date_min = .magnus_date_arg(start_date, "start_date"),
    date_max = .magnus_date_arg(end_date, "end_date"),
    journee = .magnus_id_arg(round, "round", optional = TRUE)
  ))
  .magnus_out(raw, .magnus_parse_schedule, return_parsed, "Schedule")
}

#' @title **Ligue Magnus shootout attempts**
#' @description Shootout attempts from the game sheet, in round order:
#'   `shootout_round` is the round (one attempt per team per round), not a jersey
#'   number. Trailing rounds that were not needed are listed with no shooter and
#'   `scored` `NA`. Zero rows when the game had no shootout.
#' @inheritParams magnus_game
#' @return A tibble (`return_parsed = TRUE`; zero rows without a shootout), else
#'   the game sheet list:
#'
#'   |col_name         |type     |description|
#'   |:-----------------|:--------|:----------|
#'   |game_id          |integer  |Game id (integer join key).|
#'   |attempt_id       |integer  |Shootout attempt id (integer).|
#'   |shootout_round   |integer  |Shootout round of the attempt (one attempt per team per round); not a jersey number.|
#'   |team_id          |integer  |Team id of the shooting team (integer).|
#'   |team_abbreviation|character|Abbreviation of the shooting team.|
#'   |shooter_player_id|integer  |Person id of the shooter (integer); null on rounds not needed.|
#'   |shooter_name     |character|Name of the shooter.|
#'   |goalie_player_id |integer  |Person id of the goalie facing the attempt (integer).|
#'   |goalie_name      |character|Name of the goalie facing the attempt.|
#'   |scored           |logical  |Whether the attempt scored; null on rounds not needed.|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Shootout
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_shootout(game_id = 77779))
#' }
magnus_shootout <- function(game_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(sprintf("%s/rencontre/%d/", .MAGNUS_HOST, .magnus_game_id_arg(game_id)))
  .magnus_out(raw, .magnus_parse_shootout, return_parsed, "Shootout")
}

#' @title **Ligue Magnus standings**
#' @description Standings of a phase, one row per team in rank order. `wins` and
#'   `losses` are regulation results; overtime and shootout results are `ot_wins`
#'   and `ot_losses`.
#' @param phase_id Phase id (`phase_id` of [magnus_phases()]), e.g. 714.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name           |type     |description|
#'   |:-------------------|:--------|:----------|
#'   |rank               |integer  |Rank in the standings table.|
#'   |team_id            |integer  |Team id (integer join key).|
#'   |team               |character|Short team name as the table publishes it (e.g., "MARSEILLE").|
#'   |team_abbreviation  |character|Team abbreviation.|
#'   |team_participant_id|integer  |Team participant id in this phase (integer; joins home_team_participant_id / away_team_participant_id of the schedule).|
#'   |games_played       |integer  |Games played in the table's scope.|
#'   |wins               |integer  |Wins in regulation.|
#'   |ot_wins            |integer  |Wins in overtime or a shootout.|
#'   |ot_losses          |integer  |Losses in overtime or a shootout.|
#'   |losses             |integer  |Losses in regulation.|
#'   |ties               |integer  |Number of tied games.|
#'   |points             |integer  |Points in the standings table.|
#'   |goals_for          |integer  |Goals scored (total).|
#'   |goals_against      |integer  |Goals allowed (goals against).|
#'   |penalty_minutes    |integer  |Penalty minutes.|
#'   |last_results       |character|Form guide: the last results as W (win), L (loss) or T (tie) letters, in the order the table lists them.|
#'   |standings_id       |integer  |Standings table id (integer).|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Standings
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_standings(phase_id = 714))
#' }
magnus_standings <- function(phase_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_classementsphase",
    phase_id = .magnus_id_arg(phase_id, "phase_id")
  ))
  .magnus_out(raw, .magnus_parse_standings, return_parsed, "Standings")
}

#' @title **Ligue Magnus teams of a phase**
#' @description Teams of a phase. `team_id` is stable across seasons and joins
#'   games and standings; `competition_ids` lists every competition the team
#'   entered (all seasons and levels).
#' @param phase_id Phase id (`phase_id` of [magnus_phases()]), e.g. 714. Required:
#'   without it the API answers every team of the federation (~680 KB).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass
#'   `FALSE` for the raw JSON envelope as a list.
#' @return A tibble (`return_parsed = TRUE`), else the raw envelope list:
#'
#'   |col_name         |type     |description|
#'   |:-----------------|:--------|:----------|
#'   |team_id          |integer  |Team id (integer join key), stable across seasons; joins home_team_id / away_team_id of the schedule and team_id of the standings.|
#'   |team             |character|Team display name (e.g., "Rapaces de Gap").|
#'   |team_abbreviation|character|Team abbreviation (e.g., "GAP").|
#'   |team_short_name  |character|Short team label (e.g., "GAP", "ANGLET").|
#'   |team_label       |character|Back-office label, "<club code> - <NAME> <level>"; use team for display.|
#'   |gender           |character|Gender of the team: M (men; the only value observed).|
#'   |logo_url         |character|URL of the team logo image.|
#'   |club_id          |integer  |Id of the club (legal entity) behind the team (integer).|
#'   |club             |character|Name of the club (legal entity).|
#'   |club_code        |character|Federation club code (e.g., "93006S").|
#'   |competition_ids  |list     |Competition ids the team has entered, all seasons and levels (list of integer).|
#' @section Errors:
#' As [magnus_competitions()].
#' @keywords Magnus Teams
#' @family Ligue Magnus Functions
#' @export
#' @examples
#' \donttest{
#'   try(magnus_teams(phase_id = 714))
#' }
magnus_teams <- function(phase_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .magnus_get(.MAGNUS_AJAX, list(
    action = "get_equipes",
    phase_id = .magnus_id_arg(phase_id, "phase_id")
  ))
  .magnus_out(raw, .magnus_parse_teams, return_parsed, "Teams")
}
