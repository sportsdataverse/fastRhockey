# vhl.R -- VHL (Supreme Hockey League, Russia) wrappers over vhlru.ru pages.
#
# Mirrors sdv-py sportsdataverse/hockey/vhl/{vhl_runtime,vhl_parsers}.py and
# tools/codegen/endpoints/vhl.yaml (13 routes + the public helper vhl_tn()).
#
# The VHL serves no JSON: every route is a server-rendered text/html page on
# www.vhlru.ru (or the broadcast on online.vhlru.ru). The getter fetches the page,
# REFUSES every page that is not the one asked for (the site answers a wrong season,
# game, player or team with HTTP 200 and another page), parses it into a record (a
# named list, the shape of the sdv-internal-refs captures) and stamps the request
# under `_request`. The shapers turn the record into tibbles.
#
#   * IP pin. www.vhlru.ru has two A records and 185.210.57.37 stalls after about
#     16 KB (the calendar and stats pages are 4-6 MB). Requests to that host only
#     are pinned to 188.128.49.29 with curl's CURLOPT_RESOLVE, via
#     httr2::req_options(resolve = ...). online.vhlru.ru resolves normally.
#   * Season -> tn. The site keys a season STAGE by tournament id (tn). The public
#     key is the END year + `stage` ("regular" / "playoff"), mapped through the
#     calendar's <select name="tournament"> (fetched once per R session). The KHL
#     rule "playoffs = regular + 1" does NOT hold here (2019/20 = 872 / 875).
#   * Silent wrong answers, all refused: an unknown tn serves the CURRENT season
#     with HTTP 200; /standings/regular/{tn}/ ignores the tn (the wrapper reads
#     /standings/league/{tn}/, regular season only); an unknown game id answers an
#     empty report shell; an unknown player redirects to /404.php and an unknown
#     broadcast to the online index; an unknown team id answers a shell page.
#   * Errors: fastRhockey_no_data (HTTP 404, a season without that stage, a game
#     not in the calendar, a shell page, a redirect to the 404 page / online
#     index), fastRhockey_bad_request (HTTP 400/422, a bad `stage` / `season` / id,
#     a missing argument), fastRhockey_asset_fetch (any other non-200, a dead
#     connection, a non-HTML or empty body, bytes that do not decode in the declared
#     charset, any other redirect, and a page for another scope or with a missing
#     table, header drift, zero rows or a row of the wrong width).
#   * Politeness: every attempt waits .polite_wait() (>= 1 s per host). 429/5xx are
#     retried (3 attempts, 2 s / 4 s apart, or longer when a 429 / 503 sends
#     Retry-After in seconds, capped at 120 s).
#
# Id key spaces (none joins another directly; every id is integer):
#   home_club_id / away_club_id           club id, stable across seasons (calendar)
#   team_season_id                        one per team per tournament
#   team_page_id / home_ / away_          vhlru.ru /en/teams/{id}/ page id
#   tn                                    one season STAGE
#   game_id                               calendar / report / broadcast game id
#   player_id, scorer_id, assist1_id ...  /en/players/{id}/, stable across seasons
# The report and broadcast pages do not print the game id: it comes from the
# request stamp, so a frame parsed from bare HTML has an NA game_id.

.VHL_HOST <- "https://www.vhlru.ru"
.VHL_ONLINE <- "https://online.vhlru.ru"
# curl RESOLVE entry for www.vhlru.ru only (the other A record stalls).
.VHL_PIN <- "www.vhlru.ru:443:188.128.49.29"
# The calendar page whose tournament select maps seasons to tns (serves the current season).
.VHL_CALENDAR_ROOT <- paste0(.VHL_HOST, "/en/calendar/")
.VHL_STAGES <- c("regular", "playoff")
.VHL_STAGE_LABEL <- c(regular = "Regular Season", playoff = "Playoffs")
.VHL_RETRY_STATUSES <- c(429L, 500L, 502L, 503L, 504L)
.VHL_ATTEMPTS <- 3L
.VHL_TIMEOUT <- 90
.VHL_MAX_RETRY_AFTER <- 120

# Sleep seam (tests mock it so a backoff does not cost seconds).
.vhl_sleep <- function(seconds) Sys.sleep(seconds)

# Process-wide caches: the season table and, per tn, the calendar's game -> number index.
.vhl_cache <- new.env(parent = emptyenv())
.vhl_cache_clear <- function() {
  rm(list = ls(.vhl_cache, all.names = TRUE), envir = .vhl_cache)
  invisible(NULL)
}

# ---- small helpers -------------------------------------------------------------

.vhl_na <- function(x) is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))
.vhl_or <- function(a, b) if (.vhl_na(a)) b else a

# Python's str(): "" -> NA (leading / trailing whitespace stripped).
.vhl_str <- function(s) {
  if (.vhl_na(s)) return(NA_character_)
  s <- as.character(s)[[1]]
  if (grepl("^\\s|\\s$", s)) s <- trimws(s)
  if (nzchar(s)) s else NA_character_
}

# Python's _int(): an integer from an integer or integer text, else NA.
.vhl_int <- function(v) {
  if (.vhl_na(v) || is.logical(v)) return(NA_integer_)
  if (is.numeric(v)) return(if (v[[1]] == round(v[[1]])) as.integer(v[[1]]) else NA_integer_)
  v <- as.character(v)[[1]]
  if (grepl("^\\s|\\s$", v)) v <- trimws(v)
  if (grepl("^[-+]?[0-9]+$", v)) suppressWarnings(as.integer(v)) else NA_integer_
}

# Python's _num(): "12" -> 12L, "94.6" -> 94.6, "-" / "" / anything else -> NA.
.vhl_num <- function(s) {
  if (is.na(s)) return(NA_integer_)
  if (grepl("^\\s|\\s$", s)) s <- trimws(s)
  if (grepl("^[-+]?[0-9]+$", s)) {
    v <- suppressWarnings(as.integer(s))
    return(if (is.na(v)) as.numeric(s) else v)
  }
  if (grepl("^[-+]?[0-9]+\\.[0-9]+$", s)) return(as.numeric(s))
  NA_integer_
}

# First match of a PCRE pattern: c(whole, group1, ...) ("" for a group that did not
# take part), or NULL. (*UCP) so \w / \d follow Unicode like Python's str patterns.
.vhl_re <- function(pattern, x) {
  if (.vhl_na(x)) return(NULL)
  m <- regmatches(x, regexec(paste0("(*UCP)", pattern), x, perl = TRUE))[[1]]
  if (length(m)) m else NULL
}

# The numeric id of a /players/<id>/ or /teams/<id>/ link (any host), else NA.
.vhl_id <- function(href, kind) {
  m <- .vhl_re(sprintf("/%s/([0-9]+)/", kind), href)
  if (is.null(m)) NA_integer_ else .vhl_int(m[[2]])
}

# ---- DOM (rvest) -----------------------------------------------------------------

.vhl_html <- function(page) {
  if (!is.character(page) || length(page) != 1L || is.na(page) || !nzchar(page)) page <- "<html></html>"
  rvest::read_html(page, encoding = "UTF-8")
}

.vhl_clean <- function(x) trimws(gsub("[\\s\\p{Z}\\x{85}]+", " ", x, perl = TRUE))

# Whitespace-collapsed text of a node or nodeset (NA for a missing node); block elements
# are padded with spaces (html_text2() does that; plain html_text() is ~50x faster and is
# used for every node with no block / script / style element inside). All calls are
# vectorised: a per-node xpath call costs O(document) in xml2 (it rebuilds the namespace
# table), which is minutes on the 4-6 MB calendar and stats pages.
.vhl_text <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  if (inherits(x, "xml_node")) x <- rvest::html_elements(x, xpath = "self::*")
  blocky <- !is.na(rvest::html_name(rvest::html_element(
    x, xpath = ".//br|.//p|.//div|.//li|.//td|.//th|.//tr|.//script|.//style"
  )))
  out <- rvest::html_text(x)
  if (any(blocky)) out[blocky] <- rvest::html_text2(x[blocky])
  .vhl_clean(out)
}
.vhl_ntext <- function(node) if (is.null(node)) "" else .vhl_text(node)

.vhl_xp_cls <- function(cls) {
  sprintf("contains(concat(' ', normalize-space(@class), ' '), ' %s ')", cls)
}
.vhl_all <- function(node, cls = NULL, tag = "*") {
  xp <- paste0(".//", tag, if (!is.null(cls)) paste0("[", .vhl_xp_cls(cls), "]") else "")
  rvest::html_elements(node, xpath = xp)
}
.vhl_first <- function(node, cls = NULL, tag = "*") {
  ns <- .vhl_all(node, cls, tag)
  if (length(ns)) ns[[1]] else NULL
}
.vhl_attr <- function(nodes, name) {
  if (is.null(nodes) || !length(nodes)) return(character())
  x <- rvest::html_attr(nodes, name)
  x[is.na(x)] <- ""
  x
}
.vhl_links <- function(node) {
  if (is.null(node)) return(character())
  .vhl_attr(rvest::html_elements(node, xpath = ".//a"), "href")
}
.vhl_classes <- function(node) strsplit(.vhl_attr(node, "class"), "\\s+")[[1]]

# ---- refusals ---------------------------------------------------------------------

# The page is not the data asked for. `empty` marks the provider's "nothing here"
# shells (an unknown game's report, an unknown team id's page): fastRhockey_no_data;
# every other refusal is fastRhockey_asset_fetch.
.vhl_need <- function(cond, msg, empty = FALSE) {
  if (!isTRUE(cond)) rlang::abort(msg, class = "vhl_not_parsed", empty = empty)
  invisible(TRUE)
}
.vhl_same <- function(a, b) identical(a, b)

# ---- converters, specs, tables ---------------------------------------------------------

# A converter turns one table cell (NULL = no cell) into named fields. Missing = NA.
.vhl_person <- function(cell) {
  txt <- if (is.null(cell)) "" else cell$text
  m <- .vhl_re("^(?:([0-9]+)\\s*\\.\\s*)?(.*?)\\s*(?:\\(([0-9]+|C|A)\\))?$", txt)
  list(
    id = if (is.null(cell)) NA_integer_ else .vhl_id(cell$phref, "players"),
    name = if (is.null(m)) NA_character_ else .vhl_str(m[[3]]),
    jersey = if (is.null(m) || !nzchar(m[[2]])) NA_integer_ else .vhl_int(m[[2]]),
    paren = if (is.null(m) || !nzchar(m[[4]])) NA_character_ else m[[4]]
  )
}

.vhl_conv <- function(conv, cell, key) {
  txt <- if (is.null(cell)) "" else cell$text
  out <- list()
  switch(conv,
    N = { out[[key]] <- if (is.null(cell)) NA else .vhl_num(txt) },
    N0 = { out[[key]] <- if (is.null(cell)) NA else if (!nzchar(trimws(txt))) 0L else .vhl_num(txt) },
    S = { out[[key]] <- if (is.null(cell)) NA_character_ else .vhl_str(txt) },
    T = { v <- if (is.null(cell)) NA_character_ else .vhl_str(txt); out[[key]] <- if (identical(v, "-")) NA_character_ else v },
    P = { p <- .vhl_person(cell); out[[paste0(key, "_id")]] <- p$id; out[[key]] <- p$name },
    PR = {
      p <- .vhl_person(cell)
      out[[paste0(key, "_id")]] <- p$id; out[[key]] <- p$name
      out[["captaincy"]] <- if (!is.na(p$paren) && p$paren %in% c("C", "A")) p$paren else NA_character_
    },
    PJ = {
      p <- .vhl_person(cell)
      out[[paste0(key, "_id")]] <- p$id; out[[key]] <- p$name
      out[[paste0(key, "_jersey")]] <- p$jersey; out[[paste0(key, "_season_total")]] <- .vhl_int(p$paren)
    },
    PN = {
      p <- .vhl_person(cell)
      out[[paste0(key, "_id")]] <- p$id; out[[key]] <- p$name; out[[paste0(key, "_jersey")]] <- p$jersey
    },
    JL = { out[[key]] <- as.integer(regmatches(txt, gregexpr("[0-9]+", txt))[[1]]) },
    GFA = {
      m <- .vhl_re("^([0-9]+)\\s*-\\s*([0-9]+)$", txt)
      out[["goals_for"]] <- if (is.null(m)) NA_integer_ else .vhl_int(m[[2]])
      out[["goals_against"]] <- if (is.null(m)) NA_integer_ else .vhl_int(m[[3]])
    },
    TOIS = {
      m <- .vhl_re("^([0-9]+:[0-9][0-9])\\s*(?:\\(([0-9]+)\\))?$", txt)
      out[["toi"]] <- if (is.null(m)) NA_character_ else m[[2]]
      out[["shifts"]] <- if (is.null(m) || !nzchar(m[[3]])) NA_integer_ else .vhl_int(m[[3]])
    },
    RUP = {
      m <- .vhl_re("^(.*?)\\s*\\((\\w)\\)$", txt)
      out[[key]] <- if (is.null(m)) .vhl_str(txt) else m[[2]]
      out[["position_code"]] <- if (is.null(m)) NA_character_ else m[[3]]
    },
    STD_TEAM = {
      href <- if (is.null(cell)) "" else cell$href
      m <- .vhl_re("/stats/players/([0-9]+)/all/([0-9]+)/", href)
      out[[key]] <- if (is.null(cell)) NA_character_ else .vhl_str(txt)
      out[["team_season_id"]] <- if (is.null(m)) NA_integer_ else .vhl_int(m[[3]])
      out[["_tn"]] <- if (is.null(m)) NA_integer_ else .vhl_int(m[[2]])
    },
    stop("unknown converter ", conv)
  )
  out
}
`%|na|%` <- function(a, b) if (.vhl_na(a)) b else a

# A spec is a list of columns: list(h = header text (NA = any), key = field (NA = none), conv).
.vs <- function(h, key, conv) list(h = h, key = key, conv = conv)
.vhl_spec_cols <- function(spec) {
  keys <- unlist(lapply(spec, function(s) {
    if (is.na(s$key)) character() else names(.vhl_conv(s$conv, NULL, s$key))
  }))
  unique(keys[!startsWith(keys, "_")])
}
.vhl_spec_ints <- function(...) {
  unique(unlist(lapply(list(...), function(spec) {
    unlist(lapply(spec, function(s) {
      if (!is.na(s$key) && s$conv %in% c("N", "N0")) names(.vhl_conv(s$conv, NULL, s$key)) else character()
    }))
  })))
}

# English header -> key, shared by every stats table (the pages' own legends).
.VHL_STAT <- c(
  GP = "games_played", G = "goals", A = "assists", PTS = "points", `+/-` = "plus_minus",
  `+` = "plus", `-` = "minus", PIM = "penalty_minutes", ESG = "even_strength_goals",
  PPG = "power_play_goals", SHG = "shorthanded_goals", OTG = "overtime_goals",
  GWG = "game_winning_goals", SDS = "shootout_deciding_goals", SOG = "shots_on_goal",
  `%SOG` = "shot_pct", `S/G` = "shots_per_game", FO = "faceoffs", FOW = "faceoffs_won",
  `%FO` = "faceoff_pct", `TOI/G` = "toi_per_game", `Sft/G` = "shifts_per_game", Hits = "hits",
  BLS = "blocked_shots", W = "wins", L = "losses", SOP = "shootouts_played",
  GA = "goals_against", Sv = "saves", `%Sv` = "save_pct", GAA = "gaa", SO = "shutouts",
  TOI = "toi"
)
.VHL_STR_STAT <- c("TOI/G", "TOI")  # mm:ss (minutes may exceed 59: '2314:48')
.VHL_SKATER_COLS <- c("GP", "G", "A", "PTS", "+/-", "PIM", "ESG", "PPG", "SHG", "OTG", "GWG", "SDS", "SOG", "%SOG")
.VHL_SKATER_TAIL <- c("FO", "FOW", "%FO", "TOI/G", "Sft/G", "Hits", "BLS")
.VHL_GOALIE_COLS <- c("GP", "W", "L", "SOP", "SOG", "GA", "Sv", "%Sv", "GAA")

.vhl_stat <- function(headers, goalie = FALSE) {
  lapply(headers, function(h) {
    key <- if (goalie && h == "SOG") "shots_against" else .VHL_STAT[[h]]
    .vs(h, key, if (h %in% .VHL_STR_STAT) "T" else "N")
  })
}

.VHL_STRENGTHS <- c("5 x 5", "5 x 4", "5 x 3", "4 x 4", "4 x 3", "3 x 3", "3 x 4", "3 x 5", "4 x 5")
.VHL_REPORT_GOALS <- list(
  .vs("G", "goal_number", "N"), .vs("Per", "period", "S"), .vs("Time", "time", "S"),
  .vs("Score", "score", "S"), .vs("PMG", "strength", "S"), .vs("Goal Scorer", "scorer", "PJ"),
  .vs("Assist", "assist1", "PJ"), .vs("Assist", "assist2", "PJ"),
  .vs(NA_character_, "home_on_ice", "JL"),
  .vs(NA_character_, "away_on_ice", "JL")  # header = the two short team names (home first)
)
.VHL_REPORT_PENALTIES <- list(
  .vs("Time", "time", "S"), .vs("Player", "player", "PN"), .vs("", "minutes", "N"),
  .vs("Penalty", "infraction", "S")
)
.VHL_REPORT_GOALIES <- c(
  list(.vs("", "jersey", "N"), .vs("Player", "player", "PR")),
  .vhl_stat(c(.VHL_GOALIE_COLS, "A", "SO", "PIM", "TOI"), goalie = TRUE)
)
.VHL_REPORT_SKATERS <- c(
  list(.vs("", "jersey", "N"), .vs("Player", "player", "PR")),
  .vhl_stat(c(.VHL_SKATER_COLS, .VHL_SKATER_TAIL))
)
.VHL_REPORT_POWERPLAY <- list(
  .vs("Team", "team", "S"), .vs("GP", "games_played", "N"), .vs("PP", "power_plays", "N"),
  .vs("GF", "power_play_goals", "N"), .vs("%PP", "power_play_pct", "N"),
  .vs("SHGA", "shorthanded_goals_against", "N"), .vs("TS", "times_shorthanded", "N"),
  .vs("PPGA", "power_play_goals_against", "N"), .vs("%PK", "penalty_kill_pct", "N"),
  .vs("SHG", "shorthanded_goals", "N")
)
.VHL_REPORT_BY_STRENGTH <- c(
  list(.vs("Team", "team", "S"), .vs("GP", "games_played", "N")),
  lapply(.VHL_STRENGTHS, function(s) .vs(s, paste0("g_", sub(" x ", "v", s, fixed = TRUE)), "N")),
  list(.vs("EN", "empty_net", "N"), .vs("PS", "penalty_shot", "N"), .vs("", "total", "N"))
)
.VHL_REPORT_SHOTS <- list(
  .vs("Period", "period", "S"), .vs("G", "home_goals", "N"), .vs("SOG", "home_shots_on_goal", "N"),
  .vs("%SOG", "home_shot_pct", "N"), .vs("", NA_character_, "S"), .vs("G", "away_goals", "N"),
  .vs("SOG", "away_shots_on_goal", "N"), .vs("%SOG", "away_shot_pct", "N")
)
.VHL_STATS_GOALIES <- c(
  list(.vs("Player", "player", "P"), .vs("#", "jersey", "N"), .vs("Team", "team", "S")),
  .vhl_stat(c(.VHL_GOALIE_COLS, "G", "A", "SO", "PIM", "TOI"), goalie = TRUE)
)
.VHL_STATS_SKATERS <- c(
  list(.vs("Player", "player", "P"), .vs("#", "jersey", "N"), .vs("Team", "team", "S")),
  .vhl_stat(c("GP", "G", "A", "PTS", "+/-", "+", "-", "PIM", "ESG", "PPG", "SHG", "OTG", "GWG",
              "SDS", "SOG", "%SOG", "S/G", .VHL_SKATER_TAIL))
)
.VHL_STANDINGS <- list(
  .vs("", "rank", "N"), .vs("Team", "team", "STD_TEAM"), .vs("GP", "games_played", "N"),
  .vs("W", "wins", "N"), .vs("OTW", "ot_wins", "N"), .vs("SOW", "so_wins", "N"),
  .vs("SOL", "so_losses", "N"), .vs("OTL", "ot_losses", "N"), .vs("L", "losses", "N"),
  .vs("G", "goals", "GFA"), .vs("PTS", "points", "N")
)
.VHL_CAREER_SKATER <- .vhl_stat(c("GP", "G", "A", "PTS", "+/-", "PIM", "ESG", "PPG", "SHG", "OTG",
                                  "GWG", "SDS", "SOG", "%SOG", "S/G", .VHL_SKATER_TAIL))
.VHL_CAREER_GOALIE <- .vhl_stat(c(.VHL_GOALIE_COLS, "G", "A", "SO", "PIM", "TOI"), goalie = TRUE)
.VHL_ROSTER <- list(
  .vs("#", "jersey", "N"), .vs("Position", "position", "S"), .vs("Name", "player", "P"),
  .vs("Born", "born", "S"), .vs("Country", "country", "S")
)
# online.vhlru.ru has no /en/ mirror: Russian headers.
.VHL_ONLINE_PLAYERS <- list(
  .vs("#", "jersey", "N"), .vs("\u0418\u0433\u0440\u043E\u043A", "player", "RUP"),
  .vs("\u0428", "goals", "N0"), .vs("\u0410", "assists", "N0"), .vs("\u041E", "points", "N0"),
  .vs("\u0411\u0412", "shots_on_goal", "N0"), .vs("\u0428\u0442\u0440", "penalty_minutes", "N0"),
  .vs("\u0412\u0431\u0440", "faceoffs", "N0"), .vs("\u0412\u0412\u0431\u0440", "faceoffs_won", "N0"),
  .vs("%\u0412\u0412\u0431\u0440", "faceoff_pct", "N"), .vs("\u0411\u043B\u0411", "blocked_shots", "N0"),
  .vs("\u0421\u041F\u0440", "hits", "N0"), .vs("\u0412\u041F(\u0421\u043C)", "toi", "TOIS")
)
.VHL_ONLINE_TEAMS <- list(
  .vs("", NA_character_, "S"), .vs("\u041A\u043E\u043C\u0430\u043D\u0434\u0430", "team", "S"),
  .vs("\u0428", "goals", "N0"), .vs("\u0411\u0412", "shots_on_goal", "N0"),
  .vs("\u0428\u0442\u0440", "penalty_minutes", "N0"), .vs("\u0412\u0412\u0431\u0440", "faceoffs_won", "N0"),
  .vs("%\u0412\u0412\u0431\u0440", "faceoff_pct", "N"), .vs("\u0411\u043B\u0411", "blocked_shots", "N0"),
  .vs("\u0421\u041F\u0440", "hits", "N0")
)
.VHL_EVENT_KINDS <- c(ga = "game_state", co = "comment", go = "goal", pn = "penalty",
                      gc = "goalie_change", vr = "video_review", bu = "shootout_attempt")
.VHL_STRENGTH <- c(
  "\u0420\u0410\u0412" = "even", "\u0411\u041E\u041B" = "power_play",
  "\u041C\u0415\u041D" = "shorthanded", "\u0411\u0423\u041B" = "penalty_shot_or_shootout"
)
.VHL_RU_MONTHS <- c(
  "\u044F\u043D\u0432" = 1L, "\u0444\u0435\u0432" = 2L, "\u043C\u0430\u0440" = 3L,
  "\u0430\u043F\u0440" = 4L, "\u043C\u0430\u0439" = 5L, "\u043C\u0430\u044F" = 5L,
  "\u0438\u044E\u043D" = 6L, "\u0438\u044E\u043B" = 7L, "\u0430\u0432\u0433" = 8L,
  "\u0441\u0435\u043D" = 9L, "\u043E\u043A\u0442" = 10L, "\u043D\u043E\u044F" = 11L,
  "\u0434\u0435\u043A" = 12L
)
.VHL_EN_MONTHS <- stats::setNames(1:12, c("january", "february", "march", "april", "may", "june", "july",
                                          "august", "september", "october", "november", "december"))

.vhl_width <- function(cells) sum(vapply(cells, function(c) c$span, 1L))

# The j-th <td>/<th> of every row, as parallel vectors (NA / "" where a row has no j-th cell).
# `rows` is a nodeset of <tr>; every call is vectorised over the rows.
.vhl_cell_column <- function(rows, j) {
  cj <- rvest::html_element(rows, xpath = sprintf("(td|th)[%d]", j))
  present <- !is.na(rvest::html_name(cj))
  n <- length(rows)
  blank <- function(x, fill) {
    out <- rep(fill, n)
    out[present] <- x
    out
  }
  cp <- cj[present]
  first_a <- rvest::html_element(cp, xpath = ".//a")
  linked <- !is.na(rvest::html_name(first_a))
  phref <- rep("", length(cp))
  if (any(linked)) {
    phref[linked] <- .vhl_attr(rvest::html_element(cp[linked], xpath = ".//a[contains(@href, '/players/')]"), "href")
  }
  style <- gsub(" ", "", tolower(.vhl_attr(cp, "style")), fixed = TRUE)
  colspan <- .vhl_attr(cp, "colspan")
  span <- suppressWarnings(as.integer(colspan))
  span <- pmax(ifelse(is.na(span), 1L, span), 1L)
  list(
    present = present,
    hidden = blank(grepl("display:none", style, fixed = TRUE), FALSE),
    tag = blank(rvest::html_name(cp), NA_character_),
    colspan = blank(colspan, ""),
    rowspan = blank(.vhl_attr(cp, "rowspan"), ""),
    span = blank(span, 1L),
    text = blank(.vhl_text(cp), NA_character_),
    href = blank(.vhl_attr(first_a, "href"), ""),
    phref = blank(phref, ""),
    has_link = blank(linked, FALSE)
  )
}

# (header cells, body rows) of a table: the header is the LAST <thead> row (else a leading
# all-<th> body row), the body the <tbody> rows. A cell is a record
# list(tag, colspan, rowspan, span, text, href (first link), phref (first /players/ link),
# has_link); display:none cells are dropped.
.vhl_table_parts <- function(table) {
  head <- list()
  body <- list()
  trs <- rvest::html_elements(table, xpath = ".//tr")
  n <- length(trs)
  if (!n) return(list(head = head, body = body))
  has_body <- length(rvest::html_elements(table, xpath = ".//tbody")) > 0L
  sect <- rvest::html_name(rvest::html_element(trs, xpath = "parent::*"))
  columns <- list()
  repeat {
    col <- .vhl_cell_column(trs, length(columns) + 1L)
    if (!any(col$present) || length(columns) >= 400L) break
    columns[[length(columns) + 1L]] <- col
  }
  for (i in seq_len(n)) {
    cells <- list()
    for (col in columns) {
      if (!col$present[[i]]) break
      if (col$hidden[[i]]) next
      cells[[length(cells) + 1L]] <- list(
        tag = col$tag[[i]], colspan = col$colspan[[i]], rowspan = col$rowspan[[i]],
        span = col$span[[i]], text = col$text[[i]],
        href = col$href[[i]], phref = col$phref[[i]], has_link = col$has_link[[i]]
      )
    }
    if (identical(sect[[i]], "thead")) {
      head <- cells
    } else if (identical(sect[[i]], "tbody") || (!has_body && !identical(sect[[i]], "tfoot"))) {
      if (!length(head) && length(cells) && all(vapply(cells, function(c) c$tag == "th", NA))) {
        head <- cells
      } else {
        body[length(body) + 1L] <- list(cells)
      }
    }
  }
  list(head = head, body = body)
}

# Rows of a table as named lists per `spec`; refuses a missing table, header drift, a
# row of the wrong width and zero rows. Returns list(rows, cells) (cells of the kept rows).
.vhl_read_table <- function(table, spec, what, skip = NULL) {
  .vhl_need(!is.null(table), paste0(what, ": table missing"))
  parts <- .vhl_table_parts(table)
  head <- parts$head
  got <- vapply(head, function(c) c$text, "")
  want <- vapply(spec, function(s) s$h, "")
  .vhl_need(
    length(got) == length(want) && all(is.na(want) | got == want),
    sprintf("%s: headers [%s] != [%s]", what, paste(got, collapse = ", "), paste(want, collapse = ", "))
  )
  spans <- vapply(head, function(c) c$span, 1L)
  offsets <- cumsum(c(0L, spans))[seq_along(spans)]
  w <- sum(spans)
  rows <- list()
  kept <- list()
  for (n in seq_along(parts$body)) {
    cells <- parts$body[[n]]
    if (!length(cells) || (!is.null(skip) && skip(cells))) next
    .vhl_need(.vhl_width(cells) == w,
              sprintf("%s: row %d spans %d columns, header %d", what, n - 1L, .vhl_width(cells), w))
    grid <- unlist(lapply(cells, function(c) rep(list(c), c$span)), recursive = FALSE)
    row <- list()
    for (i in seq_along(spec)) {
      s <- spec[[i]]
      if (!is.na(s$key)) {
        res <- .vhl_conv(s$conv, grid[[offsets[[i]] + 1L]], s$key)
        row[names(res)] <- res
      }
    }
    rows[length(rows) + 1L] <- list(row)
    kept[length(kept) + 1L] <- list(cells)
  }
  .vhl_need(length(rows) > 0L, paste0(what, ": zero rows"))
  list(rows = rows, cells = kept)
}
.vhl_rows <- function(table, spec, what, skip = NULL) .vhl_read_table(table, spec, what, skip)$rows


# ---- record builders (one per page kind) ------------------------------------------------

# (tn, label) of the selected <option> of <select name=name>; the tn is the first
# /number/ in its value.
.vhl_options <- function(root, name) {
  sel <- rvest::html_elements(root, xpath = sprintf(".//select[@name='%s']", name))
  if (length(sel)) rvest::html_elements(sel[[1]], xpath = ".//option") else sel
}
.vhl_selected <- function(root, name) {
  sel <- rvest::html_elements(root, xpath = sprintf(".//select[@name='%s']", name))
  opt <- if (length(sel)) rvest::html_elements(sel[[1]], xpath = ".//option[@selected]") else sel
  if (!length(opt)) return(list(tn = NA_integer_, label = NA_character_))
  opt <- opt[[1]]
  m <- .vhl_re("/([0-9]+)/", .vhl_attr(opt, "value"))
  list(tn = if (is.null(m)) NA_integer_ else .vhl_int(m[[2]]), label = .vhl_str(.vhl_text(opt)))
}
# "25/26 | Regular Season" -> c("25/26", "Regular Season") (NA when absent).
.vhl_partition <- function(label, sep = " | ") {
  if (.vhl_na(label)) return(c(NA_character_, NA_character_))
  i <- regexpr(sep, label, fixed = TRUE)
  if (i < 0L) return(c(.vhl_str(label), NA_character_))
  c(.vhl_str(substr(label, 1L, i - 1L)), .vhl_str(substr(label, i + nchar(sep), nchar(label))))
}

# The page's own tournament (the selected option); when `tn` is given it must be that
# one (a bad or ignored tn silently serves another season).
.vhl_tournament <- function(root, name, tn = NULL) {
  s <- .vhl_selected(root, name)
  .vhl_need(is.integer(s$tn) && !is.na(s$tn), "no selected tournament")
  if (!is.null(tn)) {
    .vhl_need(.vhl_same(s$tn, tn), sprintf("page is tournament %s, asked %s", s$tn, tn))
  }
  p <- .vhl_partition(s$label)
  list(tn = s$tn, label = s$label, season = p[[1]], stage = p[[2]])
}

# "25/26" -> 2026L, "2025/2026" -> 2026L; anything else NA.
.vhl_end_year <- function(label) {
  m <- if (is.character(label) && length(label) == 1L) {
    .vhl_re("^\\s*([0-9]{2}|[0-9]{4})\\s*/\\s*([0-9]{2}|[0-9]{4})\\s*$", label)
  }
  if (is.null(m)) return(NA_integer_)
  end <- as.integer(m[[3]])
  if (end >= 1000L) end else 2000L + end
}
# "Regular Season" -> "regular", "Playoffs" -> "playoff"; else NA.
.vhl_stage_of <- function(label) {
  low <- if (is.character(label) && length(label) == 1L && !is.na(label)) tolower(trimws(label)) else ""
  if (startsWith(low, "regular")) "regular" else if (startsWith(low, "playoff")) "playoff" else NA_character_
}

# Every tournament the calendar's <select name="tournament"> offers, newest first:
# list of list(tn, label, season (END year), stage).
.vhl_tournament_table <- function(page) {
  root <- if (is.character(page)) .vhl_html(page) else page
  opts <- .vhl_options(root, "tournament")
  values <- .vhl_attr(opts, "value")
  labels <- .vhl_str_vec(.vhl_text(opts))
  out <- list()
  for (i in seq_along(opts)) {
    m <- .vhl_re("/calendar/([0-9]+)/", values[[i]])
    p <- .vhl_partition(labels[[i]])
    end <- .vhl_end_year(p[[1]])
    st <- .vhl_stage_of(p[[2]])
    if (!is.null(m) && !is.na(end) && !is.na(st)) {
      out[[length(out) + 1L]] <- list(tn = .vhl_int(m[[2]]), label = labels[[i]], season = end, stage = st)
    }
  }
  out
}
.vhl_str_vec <- function(x) vapply(x, .vhl_str, "", USE.NAMES = FALSE)

.vhl_calendar <- function(root, want) {
  rec <- list(tournament = .vhl_tournament(root, "tournament", want[["tn"]]))
  tn <- rec$tournament$tn
  opts <- .vhl_options(root, "tournament")
  vals <- .vhl_attr(opts, "value")
  labs <- .vhl_str_vec(.vhl_text(opts))
  rec$tournaments <- list()
  for (i in seq_along(opts)) {
    m <- .vhl_re("/calendar/([0-9]+)/", vals[[i]])
    if (!is.null(m)) rec$tournaments[[length(rec$tournaments) + 1L]] <- list(tn = .vhl_int(m[[2]]), label = labs[[i]])
  }
  clubs <- .vhl_options(root, "club")
  vals <- .vhl_attr(clubs, "value")
  labs <- .vhl_str_vec(.vhl_text(clubs))
  rec$team_seasons <- list()
  for (i in seq_along(clubs)) {
    m <- .vhl_re("/calendar/[0-9]+/0/([0-9]+)/", vals[[i]])
    if (!is.null(m)) rec$team_seasons[[length(rec$team_seasons) + 1L]] <- list(team_season_id = .vhl_int(m[[2]]), name = labs[[i]])
  }
  # Every call below is vectorised over all the cards (see .vhl_text on why).
  cls <- function(x) sprintf("[%s]", .vhl_xp_cls(x))
  # (not `.//*[day]//*[match]`: that is quadratic on a 1,000-game calendar)
  cards <- rvest::html_elements(root, xpath = sprintf(".//*%s[ancestor::*%s]", cls("calendar-page__match"), cls("calendar-page__day")))
  .vhl_need(length(cards) > 0L, "zero games")
  day <- rvest::html_element(cards, xpath = sprintf("ancestor::*%s[1]", cls("calendar-page__day")))
  field <- function(nodes, c) rvest::html_element(nodes, xpath = sprintf(".//*%s", cls(c)))
  txt <- function(nodes) .vhl_str_vec(.vhl_text(nodes))
  dates <- txt(field(day, "calendar-page__day-date"))
  weekdays <- txt(field(day, "calendar-page__day-week"))
  href <- .vhl_attr(field(cards, "calendar-page__match-detail-link"), "href")
  hm <- lapply(href, function(h) .vhl_re("/report/([0-9]+)/\\?idgame=([0-9]+)", h))
  bad <- which(vapply(hm, function(m) is.null(m) || !.vhl_same(.vhl_int(m[[2]]), tn), NA))
  .vhl_need(length(bad) == 0L, sprintf("game link %s outside tn %s", if (length(bad)) href[[bad[[1]]]] else "", tn))
  team_at <- function(k) rvest::html_element(cards, xpath = sprintf("(.//*%s)[%d]", cls("calendar-page__match-team"), k))
  teams <- list(team_at(1L), team_at(2L))
  extra <- team_at(3L)
  .vhl_need(all(!is.na(rvest::html_name(teams[[1]]))) && all(!is.na(rvest::html_name(teams[[2]]))) &&
              all(is.na(rvest::html_name(extra))), "game without two teams")
  score <- txt(field(cards, "calendar-page__match-score_total"))
  sm <- lapply(ifelse(is.na(score), "", score), function(s) .vhl_re("^([0-9]+):([0-9]+)$", s))
  decision <- txt(field(cards, "calendar-page__match-score_type"))
  status <- txt(field(cards, "calendar-page__match-status"))
  num <- txt(field(cards, "calendar-page__match-num"))
  city <- txt(field(cards, "calendar-page__match-city"))
  online <- .vhl_attr(rvest::html_element(cards, xpath = ".//a[contains(@href, 'online.vhlru.ru/online/')]"), "href")
  classes <- .vhl_attr(cards, "class")
  side_fields <- lapply(teams, function(t) {
    list(
      club = lapply(.vhl_attr(rvest::html_element(t, xpath = ".//img"), "src"),
                    function(src) .vhl_re("/teams/\\w+/[0-9]+/([0-9]+)/", src)),
      name = txt(field(t, "calendar-page__match-team-name"))
    )
  })
  games <- vector("list", length(cards))
  for (i in seq_along(cards)) {
    row <- list(
      game_id = .vhl_int(hm[[i]][[3]]),
      game_number = .vhl_int(sub("^\u2116+", "", .vhl_or(num[[i]], ""))),
      date = dates[[i]], weekday = weekdays[[i]]
    )
    for (k in 1:2) {
      side <- c("home", "away")[[k]]
      club <- side_fields[[k]]$club[[i]]
      row[[paste0(side, "_team")]] <- side_fields[[k]]$name[[i]]
      row[[paste0(side, "_club_id")]] <- if (is.null(club)) NA_integer_ else .vhl_int(club[[2]])
    }
    row[["city"]] <- city[[i]]
    row[["score"]] <- score[[i]]
    row[["home_score"]] <- if (is.null(sm[[i]])) NA_integer_ else .vhl_int(sm[[i]][[2]])
    row[["away_score"]] <- if (is.null(sm[[i]])) NA_integer_ else .vhl_int(sm[[i]][[3]])
    row[["decision"]] <- decision[[i]]
    toks <- strsplit(classes[[i]], "\\s+")[[1]]
    row[["winner"]] <- if ("calendar-page__match--win-home" %in% toks) "home" else
      if ("calendar-page__match--win-visitor" %in% toks) "away" else NA_character_
    row[["status"]] <- status[[i]]
    row[["online_url"]] <- if (!nzchar(online[[i]])) NA_character_ else
      if (startsWith(online[[i]], "//")) paste0("https:", online[[i]]) else online[[i]]
    games[[i]] <- row
  }
  rec$games <- games
  rec
}

# '18 Mart 2026, Sreda, 17:00' (Russian) -> c("2026-03-18", "17:00").
.vhl_ru_date <- function(txt) {
  m <- .vhl_re("^([0-9]{1,2})\\s+(\\w+)\\s+([0-9]{4})(?:.*?([0-9]{1,2}:[0-9][0-9]))?", txt)
  mon <- if (is.null(m)) NA_integer_ else unname(.VHL_RU_MONTHS[stringr::str_sub(stringr::str_to_lower(m[[3]]), 1L, 3L)])
  if (is.null(m) || is.na(mon)) return(c(NA_character_, NA_character_))
  c(sprintf("%s-%02d-%02d", m[[4]], mon, as.integer(m[[2]])), if (nzchar(m[[5]])) m[[5]] else NA_character_)
}

.vhl_officials <- function(p) {
  out <- list()
  for (part in strsplit(.vhl_ntext(p), "\\s+and\\s+", perl = TRUE)[[1]]) {
    m <- .vhl_re("^([0-9]+)\\.\\s*(.+)$", trimws(part))
    if (!is.null(m)) out[[length(out) + 1L]] <- list(number = .vhl_int(m[[2]]), name = m[[3]])
  }
  out
}

# Pre-order walk of the protocol: track the current title / team / description headings
# and emit list(title, team, desc, table) for every table, in document order. Tables nested
# in tables are not descended (the shootout table carries team <h4>s in its thead).
.vhl_walk <- function(prot) {
  xp <- sprintf(
    ".//*[(self::table or %s or %s or %s) and not(ancestor::table[ancestor::*[%s]])]",
    .vhl_xp_cls("match-centr-protocol__title"), .vhl_xp_cls("table-wrap__team-name"),
    .vhl_xp_cls("table-wrap__table-desc"), .vhl_xp_cls("match-centr-protocol")
  )
  ctx <- list(title = NA_character_, team = NA_character_, desc = NA_character_)
  out <- list()
  for (node in rvest::html_elements(prot, xpath = xp)) {
    cl <- .vhl_classes(node)
    if (rvest::html_name(node) == "table") {
      out[[length(out) + 1L]] <- c(ctx, list(table = node))
    } else if ("match-centr-protocol__title" %in% cl) {
      ctx <- list(title = .vhl_text(node), team = NA_character_, desc = NA_character_)
    } else if ("table-wrap__team-name" %in% cl) {
      ctx$team <- .vhl_text(node)
      ctx$desc <- NA_character_
    } else if ("table-wrap__table-desc" %in% cl) {
      ctx$desc <- .vhl_text(node)
    }
  }
  out
}

# Rows come in pairs per round: A = [score, home shooter, ..., away goalie];
# B = [home goalie, away shooter]. The score cell (rowspan 2) is the score AFTER the round.
.vhl_shootout <- function(table) {
  parts <- .vhl_table_parts(table)
  body <- parts$body
  .vhl_need(length(body) >= 2L && length(body) %% 2L == 0L,
            sprintf("shootout: %d rows, expected pairs", length(body)))
  w <- .vhl_width(parts$head)
  linked <- function(cells) Filter(function(c) isTRUE(c$has_link), cells)
  rounds <- list()
  for (i in seq(1L, length(body), by = 2L)) {
    a <- body[[i]]
    b <- body[[i + 1L]]
    .vhl_need(
      .vhl_width(a) == w && length(a) > 0L && identical(a[[1]]$rowspan, "2") && .vhl_width(b) == w - 1L,
      sprintf("shootout: round %d spans %d/%d columns, header %d", (i + 1L) %/% 2L, .vhl_width(a), .vhl_width(b), w)
    )
    la <- linked(a)
    lb <- linked(b)
    pn <- function(cell) .vhl_conv("PN", cell, "x")
    na3 <- list(x_id = NA_integer_, x = NA_character_, x_jersey = NA_integer_)
    hs <- if (length(la) == 2L) pn(la[[1]]) else na3
    ag <- if (length(la) == 2L) pn(la[[2]]) else na3
    hg <- if (length(lb) == 2L) pn(lb[[1]]) else na3
    as_ <- if (length(lb) == 2L) pn(lb[[2]]) else na3
    rounds[[length(rounds) + 1L]] <- list(
      round = (i + 1L) %/% 2L, score_after = .vhl_str(a[[1]]$text),
      home_shooter_id = hs$x_id, home_shooter = hs$x, home_shooter_jersey = hs$x_jersey,
      away_goalie_id = ag$x_id, away_goalie = ag$x,
      away_shooter_id = as_$x_id, away_shooter = as_$x, away_shooter_jersey = as_$x_jersey,
      home_goalie_id = hg$x_id, home_goalie = hg$x
    )
  }
  rounds
}

.vhl_report <- function(root, want) {
  card <- .vhl_first(root, "match-card")
  .vhl_need(!is.null(card), "no match-card (unknown game, or a tn that does not hold this game)", empty = TRUE)
  tt <- function(cls, node = card) {
    n <- .vhl_first(node, cls)
    if (is.null(n)) NA_character_ else .vhl_str(.vhl_text(n))
  }
  season <- sub("^Season\\s+", "", .vhl_or(tt("match-card__season"), ""), perl = TRUE)
  number <- .vhl_int(sub("^\u2116+", "", .vhl_or(tt("match-card__number"), "")))
  if (!is.null(want)) {
    .vhl_need(.vhl_same(season, want[["season"]]), sprintf("season '%s' != '%s'", season, want[["season"]]))
    .vhl_need(.vhl_same(tt("match-card__league-name"), want[["stage"]]),
              sprintf("stage '%s' != '%s'", tt("match-card__league-name"), want[["stage"]]))
    .vhl_need(.vhl_same(number, want[["game_number"]]),
              sprintf("game number %s != %s", number, want[["game_number"]]))
  }
  date_text <- tt("match-card__date")
  ru <- .vhl_ru_date(date_text)
  sb <- .vhl_first(card, "match-card__scoreboard")
  score <- tt("match-card__score")
  sm <- .vhl_re("^([0-9]+):([0-9]+)\\s*(OT|SO)?", score)
  .vhl_need(!is.null(sm), sprintf("no final score ('%s')", score))
  game <- list(
    stage = tt("match-card__league-name"), season = season, game_number = number,
    date_text = date_text, date = ru[[1]], local_time = ru[[2]],
    attendance = .vhl_int(.vhl_str(gsub("\\D", "", .vhl_or(tt("match-card__spectators-count"), ""), perl = TRUE))),
    score = paste0(sm[[2]], ":", sm[[3]]), home_score = .vhl_int(sm[[2]]), away_score = .vhl_int(sm[[3]]),
    decision = if (nzchar(sm[[4]])) sm[[4]] else NA_character_,
    period_scores = if (is.null(sb)) character() else .vhl_text(.vhl_all(sb, "match-card__period-score"))
  )
  teams <- .vhl_all(card, "match-card__team")
  .vhl_need(length(teams) == 2L, "match-card without two teams")
  for (i in 1:2) {
    sd <- c("home", "away")[[i]]
    tm <- teams[[i]]
    a <- .vhl_first(tm, tag = "a")
    coach <- .vhl_first(tm, "match-card__coach")
    game[[paste0(sd, "_team_page_id")]] <- .vhl_id(if (is.null(a)) "" else .vhl_attr(a, "href"), "teams")
    game[[paste0(sd, "_team")]] <- tt("match-card__team-name", tm)
    game[[paste0(sd, "_city")]] <- tt("match-card__team-city", tm)
    game[[paste0(sd, "_coach")]] <- if (is.null(coach)) NA_character_ else
      .vhl_str(sub("^Coach:\\s*", "", .vhl_text(coach), perl = TRUE))
  }
  refs <- .vhl_all(card, "match-card__referee")
  game$referees <- .vhl_officials(if (length(refs)) refs[[1]] else NULL)
  game$linesmen <- .vhl_officials(if (length(refs) > 1L) refs[[2]] else NULL)

  prot <- .vhl_first(root, "match-centr-protocol")
  .vhl_need(!is.null(prot), "no match-centr-protocol")
  tables <- .vhl_walk(prot)
  rec <- list(game = game, goals = list(), shootout = list(), penalties = list(), goalies = list(), skaters = list())
  goals_t <- NULL
  for (tb in tables) {
    if (identical(tolower(.vhl_or(tb$title, "")), "scoring summary")) {
      goals_t <- tb$table
      break
    }
  }
  rec$goals <- .vhl_rows(goals_t, .VHL_REPORT_GOALS, "goals")
  hdr <- .vhl_table_parts(goals_t)$head
  short <- vapply(utils::tail(hdr, 2L), function(c) c$text, "")  # short team names, home first
  rec$game$home_short <- short[[1]]
  rec$game$away_short <- short[[2]]
  game <- rec$game

  # section headings use the full name, the goals header the short one
  side <- function(team) {
    home <- c(game$home_team, short[[1]])
    away <- c(game$away_team, short[[2]])
    .vhl_need(length(intersect(home, away)) == 0L && !.vhl_na(team) && team %in% c(home, away),
              sprintf("team heading '%s' not in [%s] / [%s]", .vhl_or(team, "NULL"),
                      paste(home, collapse = ", "), paste(away, collapse = ", ")))
    if (team %in% home) "home" else "away"
  }
  with_side <- function(rows, ..., s) lapply(rows, function(r) c(list(team_side = s), list(...), r))
  seen_players <- list()
  for (tb in tables) {
    ti <- tolower(.vhl_or(tb$title, ""))
    desc <- tb$desc
    if (ti == "shootouts summary") {
      rec$shootout <- .vhl_shootout(tb$table)
    } else if (ti == "penalty summary") {
      rows <- .vhl_rows(tb$table, .VHL_REPORT_PENALTIES, "penalties",
                        skip = function(c) identical(c[[1]]$colspan, "2"))
      rec$penalties <- c(rec$penalties, with_side(rows, s = side(tb$team)))
    } else if (ti == "players summary") {
      s <- side(tb$team)
      seen_players[[length(seen_players) + 1L]] <- list(s, desc)
      if (identical(desc, "Goaltenders")) {
        rec$goalies <- c(rec$goalies, with_side(.vhl_rows(tb$table, .VHL_REPORT_GOALIES, "goalies"), s = s))
      } else if (desc %in% c("Defensmen", "Forwards")) {  # sic: the site spells it Defensmen
        pos <- if (desc == "Defensmen") "defense" else "forward"
        rows <- .vhl_rows(tb$table, .VHL_REPORT_SKATERS, desc)
        rec$skaters <- c(rec$skaters, with_side(rows, position_group = pos, s = s))
      } else {
        .vhl_not_parsed(sprintf("players summary table '%s'", .vhl_or(desc, "NULL")))
      }
    } else if (ti == "teams summary") {
      key <- c(Powerplays = "team_powerplay", `GOALS FOR` = "team_goals_for",
               `GOALS AGAINST` = "team_goals_against")[.vhl_or(desc, "")]
      .vhl_need(!is.na(key), sprintf("teams summary table '%s'", .vhl_or(desc, "NULL")))
      rec[[unname(key)]] <- .vhl_rows(
        tb$table, if (key == "team_powerplay") .VHL_REPORT_POWERPLAY else .VHL_REPORT_BY_STRENGTH, desc
      )
    } else if (ti == "shots") {
      rec$shots_by_period <- .vhl_rows(tb$table, .VHL_REPORT_SHOTS, "shots")
    }
  }
  for (s in c("home", "away")) {
    .vhl_need(any(vapply(seen_players, function(p) p[[1]] == s && identical(p[[2]], "Goaltenders"), NA)),
              sprintf("no %s goalie table", s))
    .vhl_need(any(vapply(seen_players, function(p) p[[1]] == s && !identical(p[[2]], "Goaltenders"), NA)),
              sprintf("no %s skater table", s))
  }
  for (k in c("team_powerplay", "team_goals_for", "team_goals_against", "shots_by_period")) {
    .vhl_need(k %in% names(rec), sprintf("no %s table", k))
  }
  .vhl_need(identical(length(rec$shootout) > 0L, isTRUE(game$decision == "SO")),
            "shootout table vs decision mismatch")
  rec
}

.vhl_online <- function(root, want) {
  title_node <- .vhl_first(root, tag = "title")
  title <- .vhl_ntext(title_node)
  m <- .vhl_re(
    "\u0412\u0425\u041B\\.\\s*(.*?)\\.\\s*\u0418\u0433\u0440\u0430 \u043D\u043E\u043C\u0435\u0440 ([0-9]+)\\s+(.*?):\\s*(.*?)\\s*\\(\u043E\u043D\u043B\u0430\u0439\u043D",
    title
  )
  .vhl_need(!is.null(m), sprintf("not a VHL broadcast page ('%s')", substr(title, 1L, 60L)))
  if (!is.null(want)) {
    .vhl_need(.vhl_same(.vhl_int(m[[3]]), want[["game_number"]]),
              sprintf("game number %s != %s", m[[3]], want[["game_number"]]))
  }
  names_ <- utils::head(.vhl_text(.vhl_all(root, "game__team-name")), 2L)
  cities <- utils::head(.vhl_text(.vhl_all(root, "game__team-city")), 2L)
  .vhl_need(length(names_) == 2L, "no two team names")
  game <- list(
    title = title, stage_ru = m[[2]], game_number = .vhl_int(m[[3]]), date_text = m[[4]],
    home_team = names_[[1]], away_team = names_[[2]],
    home_city = if (length(cities) == 2L) cities[[1]] else NA_character_,
    away_city = if (length(cities) == 2L) cities[[2]] else NA_character_
  )
  evs <- .vhl_all(root, "game-event-item")
  fld <- function(c) rvest::html_element(evs, xpath = sprintf(".//*[%s]", .vhl_xp_cls(c)))
  tm <- fld("game-event-item__time")
  sc <- fld("game-event-item__score-re__num")
  st <- fld("game-event-item__score-status")
  right <- fld("game-event-item__right")
  tt <- .vhl_str_vec(.vhl_text(tm))
  sc_txt <- .vhl_text(sc)
  st_txt <- .vhl_text(st)
  right_txt <- .vhl_str_vec(.vhl_text(right))
  classes <- .vhl_attr(evs, "class")
  # player ids linked in the event text, in link order (k-th player link of every event at once)
  pids <- rep(list(integer()), length(evs))
  for (k in seq_len(50L)) {
    a_k <- rvest::html_element(right, xpath = sprintf("(.//a[contains(@href, '/players/')])[%d]", k))
    if (all(is.na(rvest::html_name(a_k)))) break
    ids <- vapply(.vhl_attr(a_k, "href"), .vhl_id, 1L, kind = "players", USE.NAMES = FALSE)
    for (i in which(!is.na(ids))) pids[[i]] <- c(pids[[i]], ids[[i]])
  }
  events <- vector("list", length(evs))
  for (i in seq_along(evs)) {
    cl <- strsplit(classes[[i]], "\\s+")[[1]]
    pref <- cl[startsWith(cl, "game-event-item--")]
    code <- if (length(pref)) sub("^.*?--", "", pref[[1]], perl = TRUE) else NA_character_
    events[[i]] <- list(
      kind = if (.vhl_na(code)) NA_character_ else unname(.VHL_EVENT_KINDS[code]),
      kind_code = code,
      time = tt[[i]],
      clock = if (is.na(tt[[i]])) NA_character_ else if (identical(code, "ga")) "wall" else
        if (endsWith(tt[[i]], "\u043c\u0438\u043d")) "minute" else "game",
      score = if (is.na(sc_txt[[i]])) NA_character_ else .vhl_str(gsub(" ", "", sc_txt[[i]], fixed = TRUE)),
      strength_code = .vhl_str(st_txt[[i]]),
      strength = if (is.na(st_txt[[i]])) NA_character_ else unname(.VHL_STRENGTH[st_txt[[i]]]),
      text = right_txt[[i]],
      player_ids = pids[[i]]
    )
  }
  .vhl_need(length(events) > 0L, "zero events")
  rec <- list(game = game, events = events, team_stats = list(), player_stats = list())
  boxes <- .vhl_all(root, "game__content_tabs__item")
  box_tabs <- .vhl_attr(boxes, "data-tab")
  for (i in 1:3) {
    tab <- c("stat-teams", "stat-teamA", "stat-teamB")[[i]]
    sidek <- list(NULL, "home", "away")[[i]]
    box <- if (any(box_tabs == tab)) boxes[[which(box_tabs == tab)[[1]]]] else NULL
    .vhl_need(!is.null(box), sprintf("no %s tab", tab))
    periods <- list()
    for (per in rvest::html_elements(box, xpath = ".//*[@data-tab]")) {
      pt <- .vhl_attr(per, "data-tab")
      pm <- .vhl_re("^stats-teams-(match|period-([0-9]+))$", pt)
      if (is.null(pm)) next
      tb <- .vhl_first(per, tag = "table")
      if (is.null(tb)) next
      period <- if (pm[[2]] == "match") "match" else .vhl_int(pm[[3]])
      periods[[length(periods) + 1L]] <- period
      if (is.null(sidek)) {
        rows <- .vhl_rows(tb, .VHL_ONLINE_TEAMS, paste(tab, pt))
        rec$team_stats <- c(rec$team_stats, lapply(rows, function(r) c(list(period = period), r)))
      } else {
        rows <- .vhl_rows(tb, .VHL_ONLINE_PLAYERS, paste(tab, pt))
        rec$player_stats <- c(rec$player_stats,
                              lapply(rows, function(r) c(list(team_side = sidek, period = period), r)))
      }
    }
    # every tab must hold its whole-game table: a tab with only period tables is not a box score
    .vhl_need("match" %in% unlist(periods), sprintf("%s: no stats-teams-match table", tab))
  }
  rec
}

.vhl_standings <- function(root, want) {
  rec <- list(tournament = .vhl_tournament(root, "team", want[["tn"]]))
  tn <- rec$tournament$tn
  rows <- .vhl_rows(.vhl_first(root, tag = "table"), .VHL_STANDINGS, "standings")
  for (i in seq_along(rows)) {  # every team link must point into the same tournament
    .vhl_need(.vhl_same(rows[[i]][["_tn"]], tn),
              sprintf("standings row '%s' links outside tn %s", .vhl_or(rows[[i]]$team, "NULL"), tn))
    rows[[i]][["_tn"]] <- NULL
  }
  rec$rows <- rows
  rec
}

.vhl_stats_players <- function(root, want) {
  rec <- list(tournament = .vhl_tournament(root, "tournament", want[["tn"]]))
  tables <- list()
  for (wrap in .vhl_all(root, "table__wrapper")) {
    h2 <- .vhl_first(wrap, tag = "h2")
    tb <- .vhl_first(wrap, tag = "table")
    if (!is.null(h2) && !is.null(tb)) tables[[.vhl_text(h2)]] <- tb
  }
  rec$goalies <- .vhl_rows(tables[["Goaltenders"]], .VHL_STATS_GOALIES, "Goaltenders")
  rec$defensemen <- .vhl_rows(tables[["Defensemen"]], .VHL_STATS_SKATERS, "Defensemen")
  rec$forwards <- .vhl_rows(tables[["Forwards"]], .VHL_STATS_SKATERS, "Forwards")
  for (k in c("goalies", "defensemen", "forwards")) {
    .vhl_need(all(vapply(rec[[k]], function(r) is.integer(r$player_id) && !is.na(r$player_id), NA)),
              sprintf("%s: a row without a player link", k))
  }
  rec
}

# Season-by-season rows: a label row ('25/26 | Regular Season', no stats) precedes that
# season's team rows.
.vhl_career <- function(table, what) {
  head <- if (is.null(table)) character() else vapply(.vhl_table_parts(table)$head, function(c) c$text, "")
  spec <- c(list(.vs("Season / Team", "team", "S"), .vs("#", "jersey", "N")),
            if ("SOP" %in% head) .VHL_CAREER_GOALIE else .VHL_CAREER_SKATER)
  parsed <- .vhl_read_table(table, spec, what)
  out <- list()
  label <- NA_character_
  for (i in seq_along(parsed$rows)) {
    r <- parsed$rows[[i]]
    if (all(vapply(r[names(r) != "team"], .vhl_na, NA))) {
      label <- r$team
      next
    }
    href <- parsed$cells[[i]][[1]]$href
    m <- .vhl_re("/stats/players/([0-9]+)/all/([0-9]+)/", href)
    p <- .vhl_partition(label)
    out[[length(out) + 1L]] <- c(
      list(season = p[[1]], stage = p[[2]],
           tn = if (is.null(m)) NA_integer_ else .vhl_int(m[[2]]),
           team_season_id = if (is.null(m)) NA_integer_ else .vhl_int(m[[3]])),
      r
    )
  }
  .vhl_need(length(out) > 0L, paste0(what, ": zero season rows"))
  out
}

.vhl_player <- function(root, want) {
  links <- .vhl_links(root)
  games <- links[grepl("/players/[0-9]+/games/", links)][1]
  pid <- .vhl_id(if (is.na(games)) "" else sub("/games/", "/", games, fixed = TRUE), "players")
  .vhl_need(is.integer(pid) && !is.na(pid), "no /players/<id>/games/ link: not a player page")
  if (!is.null(want)) {
    .vhl_need(.vhl_same(pid, want[["player_id"]]), sprintf("page is player %s, asked %s", pid, want[["player_id"]]))
  }
  h1 <- .vhl_first(root, tag = "h1")
  num <- if (is.null(h1)) NULL else .vhl_first(h1, "player-card__number")
  ntxt <- .vhl_ntext(num)
  name <- if (!is.null(h1) && !is.null(num) && nzchar(ntxt)) trimws(gsub(ntxt, "", .vhl_text(h1), fixed = TRUE)) else
    .vhl_ntext(h1)
  .vhl_need(nzchar(name), "no player name")
  props <- list()
  for (item in .vhl_all(root, "player-card__item")) {
    k <- .vhl_first(item, "player-card__property")
    if (!is.null(k)) {
      ktxt <- .vhl_text(k)
      props[[sub(":+$", "", ktxt)]] <- .vhl_str(substring(.vhl_text(item), nchar(ktxt) + 1L))
    }
  }
  img <- .vhl_first(root, "player-card__img", "img")
  pos <- .vhl_first(root, "player-card__subtitle")
  tables <- rvest::html_elements(root, xpath = ".//table")
  rec <- list(
    player_id = pid, name = name,
    jersey = .vhl_int(sub("^#", "", if (is.null(num)) "" else .vhl_text(num))),
    position = if (is.null(pos)) NA_character_ else .vhl_str(.vhl_text(pos)),
    height_cm = .vhl_int(props[["Height"]]), weight_kg = .vhl_int(props[["Weight"]]),
    born = .vhl_or(props[["Born"]], NA_character_),
    age = .vhl_int(.vhl_str(gsub("\\D", "", .vhl_or(props[["Age"]], ""), perl = TRUE))),
    country = .vhl_or(props[["Country"]], NA_character_),
    photo_url = if (is.null(img)) NA_character_ else .vhl_attr(img, "src"),
    seasons = .vhl_career(if (length(tables)) tables[[1]] else NULL, "career by season")
  )
  totals <- if (length(tables) > 1L) tables[[2]] else NULL
  head <- if (is.null(totals)) character() else vapply(.vhl_table_parts(totals)$head, function(c) c$text, "")
  spec <- c(list(.vs("Season / Team", "stage", "S"), .vs("", NA_character_, "S")),
            if ("SOP" %in% head) .VHL_CAREER_GOALIE else .VHL_CAREER_SKATER)
  rec$career_totals <- .vhl_rows(totals, spec, "career totals")
  rec
}

.vhl_team <- function(root, want) {
  h1 <- .vhl_first(root, "title", "h1")
  name <- if (is.null(h1)) NA_character_ else .vhl_str(.vhl_text(h1))
  content <- .vhl_first(root, "team__content")
  # an unknown team id answers a 158 KB shell without the team block
  .vhl_need(!is.na(name) && !is.null(content), "no h1.title / team__content: not a team page", empty = TRUE)
  if (!is.null(want) && !is.null(want[["team_name"]])) {
    .vhl_need(.vhl_same(name, want[["team_name"]]), sprintf("page is team '%s', asked '%s'", name, want[["team_name"]]))
  }
  h2 <- .vhl_first(content, tag = "h2")
  hm <- .vhl_re("^(.*?)\\s*,\\s*(.*?)\\s*\\|\\s*Year of foundation:\\s*([0-9]+)", .vhl_ntext(h2))
  officials <- list()  # every '<p>Role:<b>Name</b></p>' line, whatever the role
  for (p in rvest::html_elements(content, xpath = ".//p")) {
    b <- .vhl_first(p, tag = "b")
    if (!is.null(b)) {
      ptxt <- .vhl_text(p)
      btxt <- .vhl_text(b)
      head <- if (nchar(btxt) == 0L) "" else substr(ptxt, 1L, nchar(ptxt) - nchar(btxt))
      role <- trimws(sub(":+$", "", trimws(head)))
      officials[[length(officials) + 1L]] <- list(role = .vhl_str(role), name = .vhl_str(btxt))
    }
  }
  contacts <- list()
  for (li in .vhl_all(content, "team__contacts-item")) {
    t <- .vhl_text(li)
    i <- regexpr(":", t, fixed = TRUE)
    k <- if (i < 0L) t else substr(t, 1L, i - 1L)
    v <- if (i < 0L) "" else substr(t, i + 1L, nchar(t))
    contacts[[trimws(k)]] <- .vhl_str(v)
  }
  links <- .vhl_links(root)
  cal <- links[grepl("/calendar/[0-9]+/0/[0-9]+/", links)][1]
  cm <- .vhl_re("/calendar/([0-9]+)/0/([0-9]+)/", if (is.na(cal)) "" else cal)
  asof <- .vhl_first(root, "right_team")
  ptexts <- .vhl_text(rvest::html_elements(root, xpath = ".//p"))
  cap <- ptexts[startsWith(ptexts, "Capacity:")][1]
  addr <- .vhl_first(content, tag = "address")
  rec <- list(
    name = name,
    full_name = if (is.null(hm)) NA_character_ else hm[[2]],
    city = if (is.null(hm)) NA_character_ else hm[[3]],
    founded = if (is.null(hm)) NA_integer_ else .vhl_int(hm[[4]]),
    phone = .vhl_or(contacts[["Phone"]], NA_character_),
    site = .vhl_or(contacts[["Site"]], NA_character_),
    officials = officials,
    address = if (is.null(addr)) NA_character_ else .vhl_str(.vhl_text(addr)),
    calendar_tn = if (is.null(cm)) NA_integer_ else .vhl_int(cm[[2]]),
    calendar_team_season_id = if (is.null(cm)) NA_integer_ else .vhl_int(cm[[3]]),
    roster_as_of = if (is.null(asof)) NA_character_ else .vhl_str(sub("^As of\\s*", "", .vhl_text(asof), perl = TRUE)),
    arena_capacity = .vhl_int(.vhl_str(gsub("\\D", "", .vhl_or(cap, ""), perl = TRUE))),
    roster = .vhl_rows(.vhl_first(root, tag = "table"), .VHL_ROSTER, "roster")
  )
  h3s <- rvest::html_elements(root, xpath = ".//h3[not(*) and normalize-space(@class)='']")
  txts <- .vhl_text(h3s)
  rec$honours <- as.list(txts[nzchar(txts) & txts != "Players"])
  rec
}

.vhl_not_parsed <- function(msg, empty = FALSE) .vhl_need(FALSE, msg, empty)

# The parsed record of one page, or a `vhl_not_parsed` condition. `want` is what the
# page must say about itself (the getter's identity check): tn (calendar, standings,
# stats), season / stage / game_number (report), game_number (broadcast), player_id
# (player); NULL skips the identity check but keeps every structural one.
.vhl_page_record <- function(kind, page, want = NULL) {
  builder <- switch(kind,
    calendar = .vhl_calendar, report = .vhl_report, online = .vhl_online, standings = .vhl_standings,
    stats_players = .vhl_stats_players, player = .vhl_player, team = .vhl_team,
    stop("unknown page kind ", kind)
  )
  tryCatch(
    builder(.vhl_html(page), want),
    error = function(e) {  # a page that breaks the parser is not data
      if (inherits(e, "vhl_not_parsed")) stop(e)
      .vhl_not_parsed(sprintf("unexpected page shape: %s", conditionMessage(e)))
    }
  )
}


# ---- transport ----------------------------------------------------------------------------

# Retry-After in seconds (an HTTP-date falls back to the exponential delay), capped.
.vhl_retry_after <- function(resp) {
  v <- suppressWarnings(as.numeric(httr2::resp_header(resp, "retry-after", default = "0")))
  if (length(v) != 1L || is.na(v)) 0 else min(v, .VHL_MAX_RETRY_AFTER)
}

# Body bytes -> text, STRICTLY in the declared charset (UTF-8 when none is declared).
.vhl_decode <- function(url, raw, ctype) {
  where <- .euro_where(url)
  m <- regmatches(ctype, regexec("charset=[\"']?([A-Za-z0-9_.:-]+)", ctype, ignore.case = TRUE))[[1]]
  charset <- if (length(m)) m[[2]] else "utf-8"
  text <- tryCatch(iconv(rawToChar(raw), from = charset, to = "UTF-8"), error = function(e) NA_character_)
  if (is.na(text)) {
    .abort_asset_fetch("{where}: body is not valid {.val {charset}}.")
  }
  Encoding(text) <- "UTF-8"
  if (grepl("\uFFFD", text, fixed = TRUE)) {
    .abort_asset_fetch("{where}: replacement characters in the body (charset mismatch).")
  }
  text
}

# The HTML text of `url` (+ `params`), or the error the answer calls for. Every attempt
# waits its host's 1 s slot; www.vhlru.ru is pinned to the working IP (.VHL_PIN).
.vhl_fetch <- function(url, params = NULL) {
  params <- Filter(Negate(is.null), as.list(params))
  req <- .build_request(url, params, timeout = .VHL_TIMEOUT)
  if (identical(httr2::url_parse(url)$hostname, "www.vhlru.ru")) {
    req <- httr2::req_options(req, resolve = .VHL_PIN)
  }
  req <- httr2::req_error(httr2::req_retry(req, max_tries = 1), is_error = function(resp) FALSE)
  full <- req$url
  where <- .euro_where(url)
  resp <- NULL
  for (attempt in seq_len(.VHL_ATTEMPTS)) {
    .polite_wait(url)
    delay <- 2 * 2^(attempt - 1L)
    last <- attempt == .VHL_ATTEMPTS
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      if (last) {
        attempts <- .VHL_ATTEMPTS
        .abort_asset_fetch("{where}: fetch failed after {attempts} attempts: {conditionMessage(resp)}",
                           parent = resp)
      }
    } else {
      status <- httr2::resp_status(resp)
      if (!status %in% .VHL_RETRY_STATUSES || last) break
      if (status %in% c(429L, 503L)) delay <- max(delay, .vhl_retry_after(resp))  # both may send Retry-After
    }
    .vhl_sleep(delay)
  }
  status <- httr2::resp_status(resp)
  if (status != 200L) {
    .euro_check_status(resp, where)  # raises unless 2xx
    .abort_asset_fetch("{where} answered HTTP {status}, not 200.")
  }
  ctype <- httr2::resp_header(resp, "content-type", default = "")
  if (!grepl("html", tolower(ctype), fixed = TRUE)) {
    shown <- if (nzchar(ctype)) ctype else "no content type"
    .abort_asset_fetch("{where} answered a non-HTML body ({shown}).")
  }
  text <- .vhl_decode(url, httr2::resp_body_raw(resp), ctype)
  if (!nzchar(trimws(text))) .abort_asset_fetch("{where} answered HTTP 200 with an empty body.")
  final <- resp$url %||% ""
  if (!identical(sub("/+$", "", final), sub("/+$", "", full))) {
    target <- if (nzchar(final)) httr2::url_parse(final) else NULL
    path <- target$path %||% ""
    if (identical(path, "/404.php") ||
        (identical(target$hostname, "online.vhlru.ru") && path %in% c("", "/", "/online/"))) {
      seen <- .euro_where(final)
      .abort_no_data("{where}: no such page (redirected to {seen}).")
    }
    seen <- if (nzchar(final)) .euro_where(final) else "an unknown URL"
    .abort_asset_fetch("{where}: redirected to {seen}.")
  }
  text
}

# Fetch and parse one page; a refused parse becomes the error vocabulary.
.vhl_page <- function(kind, url, params = NULL, want = NULL) {
  text <- .vhl_fetch(url, params)
  where <- .euro_where(url)
  tryCatch(
    .vhl_page_record(kind, text, want),
    vhl_not_parsed = function(e) {
      msg <- conditionMessage(e)
      if (isTRUE(e$empty)) {
        .abort_no_data("{where}: {msg}")
      } else {
        .abort_asset_fetch("{where}: not the page asked for: {msg}")
      }
    }
  )
}

# ---- season -> tn -----------------------------------------------------------------------------

.vhl_digits <- function(v) {
  length(v) == 1L && !is.na(v) && !is.logical(v) &&
    ((is.numeric(v) && v == round(v) && v >= 0) || (is.character(v) && grepl("^\\s*[0-9]+\\s*$", v)))
}
.vhl_season <- function(value) {
  if (!.vhl_digits(value)) {
    .abort_bad_request("{.arg season} must be an END year such as 2026 (= 2025/26); got {.val {value}}.")
  }
  as.integer(trimws(as.character(value)))
}
.vhl_stage <- function(value) {
  if (!(is.character(value) && length(value) == 1L && !is.na(value) && value %in% .VHL_STAGES)) {
    stages <- .VHL_STAGES
    .abort_bad_request("{.arg stage} must be one of {.val {stages}}; got {.val {value}}.")
  }
  value
}

# data.frame(season, stage, tn) from the calendar's tournament select (one fetch, cached).
.vhl_tournaments <- function() {
  tab <- .vhl_cache[["tournaments"]]
  if (is.null(tab)) {
    rows <- .vhl_tournament_table(.vhl_fetch(.VHL_CALENDAR_ROOT))
    if (!length(rows)) {
      where <- .euro_where(.VHL_CALENDAR_ROOT)
      .abort_asset_fetch("{where}: no tournament select on the calendar page.")
    }
    tab <- data.frame(
      season = vapply(rows, function(r) r$season, 1L),
      stage = vapply(rows, function(r) r$stage, ""),
      tn = vapply(rows, function(r) r$tn, 1L),
      stringsAsFactors = FALSE
    )
    .vhl_cache[["tournaments"]] <- tab
  }
  tab
}

#' The game number the calendar of `tn` gives `game_id` (the report's identity check).
#' @noRd
.vhl_game_number <- function(tn, game_id) {
  key <- paste0("games_", tn)
  id <- as.character(game_id)
  known <- .vhl_cache[[key]]
  if (!id %in% names(known)) {  # one fresh calendar per miss: a game scheduled since the last read
    rec <- .vhl_page("calendar", sprintf("%s/en/calendar/%s/0/", .VHL_HOST, tn), NULL, list(tn = tn))
    .vhl_remember_games(tn, rec)
    known <- .vhl_cache[[key]]
  }
  if (!id %in% names(known)) {
    .abort_no_data("VHL game {game_id} is not in the calendar of tournament {tn}.")
  }
  known[[id]]
}
.vhl_remember_games <- function(tn, rec) {
  games <- .vhl_list(rec[["games"]])
  idx <- lapply(games, function(g) .vhl_or(g[["game_number"]], NA_integer_))
  names(idx) <- vapply(games, function(g) as.character(g[["game_id"]]), "")
  .vhl_cache[[paste0("games_", tn)]] <- idx
}

# ---- the getter ----------------------------------------------------------------------------------

# (host, path pattern) -> page kind. The number is the season (END year) on tn routes.
.VHL_ROUTES <- list(
  list("www.vhlru.ru", "^/en/calendar/([0-9]+)/0/?$", "calendar"),
  list("www.vhlru.ru", "^/en/report/([0-9]+)/?$", "report"),
  list("www.vhlru.ru", "^/en/standings/league/([0-9]+)/?$", "standings"),
  list("www.vhlru.ru", "^/en/stats/players/([0-9]+)/?$", "stats_players"),
  list("www.vhlru.ru", "^/en/players/([0-9]+)/?$", "player"),
  list("www.vhlru.ru", "^/en/teams/([0-9]+)/?$", "team"),
  list("online.vhlru.ru", "^/online/([0-9]+)\\.html$", "online")
)
.VHL_TN_KINDS <- c("calendar", "report", "standings", "stats_players")

.vhl_route <- function(url) {
  parts <- httr2::url_parse(url)
  for (r in .VHL_ROUTES) {
    m <- regmatches(parts$path %||% "", regexec(r[[2]], parts$path %||% ""))[[1]]
    if (identical(parts$hostname, r[[1]]) && length(m)) return(list(kind = r[[3]], number = as.integer(m[[2]])))
  }
  where <- .euro_where(url)
  .abort_bad_request("Not a VHL route: {where}.")
}

# GET a VHL page, refuse every wrong answer, return its parsed record with the request
# stamped under `_request` (season, stage, tn and the ids that apply). On tn routes
# (calendar, report, standings, stats) the tn path segment of `url` holds the season END
# year, which is mapped to the tn. `params`: `stage` (default "regular") and, for the
# broadcast, `season` are consumed here; `idgame` is sent to the report.
.vhl_get <- function(url, params = list()) {
  q <- Filter(Negate(is.null), as.list(params))
  stage <- .vhl_stage(q[["stage"]] %||% "regular")
  q[["stage"]] <- NULL
  r <- .vhl_route(url)
  kind <- r$kind
  number <- r$number
  req <- list()
  want <- NULL
  if (kind %in% .VHL_TN_KINDS) {
    if (kind == "standings" && stage != "regular") {
      .abort_bad_request("VHL standings exist for the regular season only ({.code stage = \"regular\"}).")
    }
    if (kind == "report") {  # checked before any request
      game_id <- q[["idgame"]]
      if (is.null(game_id) || !.vhl_digits(game_id)) {
        shown <- if (is.null(game_id)) "NULL" else format(game_id)
        .abort_bad_request("The VHL report needs an integer game id ({.arg idgame}); got {shown}.")
      }
      q[["idgame"]] <- as.integer(game_id)
    }
    season <- number
    tn <- vhl_tn(season, stage)
    url <- sub(sprintf("/%d/", number), sprintf("/%d/", tn), url, fixed = TRUE)
    req <- list(season = season, stage = stage, tn = tn)
    want <- list(tn = tn)
    if (kind == "report") {
      req$game_id <- q[["idgame"]]
      want <- list(
        season = sprintf("%d/%d", season - 1L, season),
        stage = unname(.VHL_STAGE_LABEL[[stage]]),
        game_number = .vhl_game_number(tn, q[["idgame"]])
      )
    }
  } else if (kind == "online") {
    if (is.null(q[["season"]])) {
      .abort_bad_request("The VHL broadcast needs the game's {.arg season} (END year) to check the page.")
    }
    season <- .vhl_season(q[["season"]])
    q[["season"]] <- NULL
    tn <- vhl_tn(season, stage)
    req <- list(season = season, stage = stage, tn = tn, game_id = number)
    want <- list(game_number = .vhl_game_number(tn, number))
  } else if (kind == "player") {
    req <- list(player_id = number)
    want <- list(player_id = number)
  } else {  # team: the page names no id of its own; only the shell is refused
    req <- list(team_page_id = number)
  }
  rec <- .vhl_page(kind, url, q, want)
  if (kind == "calendar") .vhl_remember_games(req$tn, rec)
  rec[["_request"]] <- req
  rec
}

# ---- frames -------------------------------------------------------------------------------------

.VHL_INT_COL <- c(
  .vhl_spec_ints(.VHL_REPORT_GOALS, .VHL_REPORT_PENALTIES, .VHL_REPORT_GOALIES, .VHL_REPORT_SKATERS,
                 .VHL_STATS_GOALIES, .VHL_STATS_SKATERS, .VHL_STANDINGS, .VHL_CAREER_GOALIE, .VHL_ROSTER),
  "tn", "season", "game_number", "home_score", "away_score", "attendance", "event_index", "goals_for",
  "goals_against", "height_cm", "weight_kg", "age", "founded", "arena_capacity", "calendar_tn",
  "referee1_number", "referee2_number", "linesman1_number", "linesman2_number"
)

# The dtype a column has by name alone (never sampled).
.vhl_dtype <- function(col) {
  if (col %in% c("home_on_ice", "away_on_ice", "player_ids")) return("list_integer")
  if (col == "period_scores") return("list_character")
  if (col %in% c("toi", "toi_per_game")) return("character")
  if (grepl("(^|_)id$", col)) return("integer")
  if (grepl("(_pct|_per_game)$|^gaa$", col)) return("double")
  if (col %in% .VHL_INT_COL || grepl("_(jersey|season_total)$", col)) return("integer")
  "character"
}

.vhl_col <- function(vals, dtype) {
  one <- function(v) !is.null(v) && length(v) == 1L && !is.na(v) && !is.logical(v)
  switch(dtype,
    integer = vapply(vals, function(v) if (one(v) && is.integer(v)) v else NA_integer_, 1L),
    double = vapply(vals, function(v) if (one(v) && is.numeric(v)) as.numeric(v) else NA_real_, 1),
    character = vapply(vals, function(v) if (one(v)) as.character(v) else NA_character_, ""),
    list_integer = lapply(vals, function(v) {
      if (!is.list(v) && !is.integer(v)) return(NULL)
      v <- unlist(v)
      as.integer(v[vapply(as.list(v), is.integer, NA)])
    }),
    list_character = lapply(vals, function(v) {
      if (!is.list(v) && !is.character(v)) return(NULL)
      v <- unlist(v)
      as.character(v[vapply(as.list(v), is.character, NA)])
    })
  )
}

# A tibble with exactly `cols`, dtypes by name; zero rows keep the documented columns.
.vhl_frame <- function(rows, cols) {
  out <- lapply(cols, function(col) .vhl_col(lapply(rows, function(r) r[[col]]), .vhl_dtype(col)))
  names(out) <- cols
  tibble::as_tibble(out)
}

# ---- shapers (record -> tibble; each takes a record or the page's HTML) ---------------------------

.vhl_obj <- function(x) if (is.list(x)) x else list()
.vhl_list <- function(x) if (is.list(x)) Filter(is.list, unname(x)) else list()
.vhl_req <- function(rec) .vhl_obj(rec[["_request"]])

# The page record: `raw` itself when it is one (it has `key`), else `raw` parsed as HTML.
.vhl_record <- function(raw, kind, key) {
  if (is.list(raw)) return(if (key %in% names(raw)) raw else NULL)
  if (is.character(raw) && length(raw) == 1L && !is.na(raw) && nzchar(trimws(raw))) {
    return(tryCatch(.vhl_page_record(kind, raw), vhl_not_parsed = function(e) NULL))
  }
  NULL
}

# season / stage / tn of a tournament page: the page's own, the request's as fallback.
.vhl_scope <- function(rec) {
  t <- .vhl_obj(rec[["tournament"]])
  req <- .vhl_req(rec)
  list(
    season = .vhl_or(.vhl_end_year(t[["season"]]), .vhl_or(req[["season"]], NA_integer_)),
    stage = .vhl_or(.vhl_stage_of(t[["stage"]]), .vhl_or(req[["stage"]], NA_character_)),
    tn = if (is.integer(t[["tn"]])) t[["tn"]] else .vhl_or(req[["tn"]], NA_integer_)
  )
}

# '18 March' -> '2026-03-18' (August to December fall in the START year).
.vhl_iso_day <- function(text, season) {
  m <- if (is.character(text) && length(text) == 1L) .vhl_re("^\\s*([0-9]{1,2})\\s+([A-Za-z]+)\\s*$", text)
  mon <- if (is.null(m)) NA_integer_ else unname(.VHL_EN_MONTHS[tolower(m[[3]])])
  if (is.null(m) || is.na(mon) || .vhl_na(season)) return(NA_character_)
  sprintf("%d-%02d-%02d", if (mon >= 8L) season - 1L else season, mon, as.integer(m[[2]]))
}

.VHL_SCHEDULE_COLS <- c("season", "stage", "tn", "game_id", "game_number", "game_date", "date", "weekday",
                        "home_team", "home_club_id", "away_team", "away_club_id", "city", "score",
                        "home_score", "away_score", "decision", "winner", "status", "online_url")
.vhl_parse_schedule <- function(raw) {
  rec <- .vhl_record(raw, "calendar", "games")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_SCHEDULE_COLS))
  scope <- .vhl_scope(rec)
  rows <- lapply(.vhl_list(rec[["games"]]), function(g) {
    g[names(scope)] <- scope
    g[["game_date"]] <- .vhl_iso_day(g[["date"]], scope$season)
    g
  })
  .vhl_frame(rows, .VHL_SCHEDULE_COLS)
}

.VHL_GAME_KEY <- c("game_id", "season", "stage", "game_number")
.VHL_GAME_INFO_COLS <- c(
  .VHL_GAME_KEY, "tn", "date", "local_time", "date_text", "attendance", "score", "home_score", "away_score",
  "decision", "period_scores", "home_team_page_id", "home_team", "home_short", "home_city", "home_coach",
  "away_team_page_id", "away_team", "away_short", "away_city", "away_coach", "referee1_number", "referee1",
  "referee2_number", "referee2", "linesman1_number", "linesman1", "linesman2_number", "linesman2"
)
# The game key of a report record: page fields first, the request stamp for what the page lacks.
.vhl_report_key <- function(rec) {
  g <- .vhl_obj(rec[["game"]])
  req <- .vhl_req(rec)
  list(
    game_id = .vhl_or(req[["game_id"]], NA_integer_),
    season = .vhl_or(.vhl_end_year(g[["season"]]), .vhl_or(req[["season"]], NA_integer_)),
    stage = .vhl_or(.vhl_stage_of(g[["stage"]]), .vhl_or(req[["stage"]], NA_character_)),
    game_number = .vhl_or(g[["game_number"]], NA_integer_),
    tn = .vhl_or(req[["tn"]], NA_integer_)
  )
}
.vhl_parse_game_info <- function(raw) {
  rec <- .vhl_record(raw, "report", "game")
  g <- if (is.null(rec)) NULL else rec[["game"]]
  if (!is.list(g)) return(.vhl_frame(list(), .VHL_GAME_INFO_COLS))
  row <- g[!names(g) %in% c("season", "stage")]
  key <- .vhl_report_key(rec)
  row[names(key)] <- key
  for (ki in list(c("referee", "referees"), c("linesman", "linesmen"))) {
    for (i in seq_len(min(2L, length(.vhl_list(g[[ki[[2]]]]))))) {
      o <- .vhl_list(g[[ki[[2]]]])[[i]]
      row[[sprintf("%s%d_number", ki[[1]], i)]] <- o[["number"]] %||% NA_integer_
      row[[sprintf("%s%d", ki[[1]], i)]] <- o[["name"]] %||% NA_character_
    }
  }
  .vhl_frame(list(row), .VHL_GAME_INFO_COLS)
}

.vhl_score <- function(s) {
  m <- if (is.character(s) && length(s) == 1L) .vhl_re("^\\s*([0-9]+)\\s*:\\s*([0-9]+)\\s*$", s)
  if (is.null(m)) NULL else c(as.integer(m[[2]]), as.integer(m[[3]]))
}
# team_page_id / team (short name) of a report row's team_side.
.vhl_side_page <- function(rec, side) {
  g <- .vhl_obj(rec[["game"]])
  if (!(is.character(side) && length(side) == 1L && side %in% c("home", "away"))) {
    return(list(team_page_id = NA_integer_, team = NA_character_))
  }
  list(team_page_id = .vhl_or(g[[paste0(side, "_team_page_id")]], NA_integer_),
       team = .vhl_or(g[[paste0(side, "_short")]], NA_character_))
}

.VHL_GOALS_COLS <- c(.VHL_GAME_KEY, "team_side", "team_page_id", .vhl_spec_cols(.VHL_REPORT_GOALS))
.vhl_parse_goals <- function(raw) {
  rec <- .vhl_record(raw, "report", "goals")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_GOALS_COLS))
  key <- .vhl_report_key(rec)
  prev <- c(0L, 0L)
  rows <- list()
  for (gl in .vhl_list(rec[["goals"]])) {
    now <- .vhl_score(gl[["score"]])
    side <- NA_character_
    if (!is.null(now)) {
      up <- c(now[[1]] > prev[[1]], now[[2]] > prev[[2]])
      side <- if (identical(up, c(TRUE, FALSE))) "home" else if (identical(up, c(FALSE, TRUE))) "away" else NA_character_
      prev <- now
    }
    row <- gl
    row[names(key)] <- key
    row[["team_side"]] <- side
    row[["team_page_id"]] <- .vhl_side_page(rec, side)$team_page_id
    rows[[length(rows) + 1L]] <- row
  }
  .vhl_frame(rows, .VHL_GOALS_COLS)
}

.VHL_TEAM_COLS <- c("team_side", "team_page_id", "team")
.vhl_side_rows <- function(rec, key) {
  k <- .vhl_report_key(rec)
  lapply(.vhl_list(rec[[key]]), function(r) {
    r[names(k)] <- k
    sp <- .vhl_side_page(rec, r[["team_side"]])
    r[names(sp)] <- sp
    r
  })
}
.VHL_PENALTY_COLS <- c(.VHL_GAME_KEY, .VHL_TEAM_COLS, .vhl_spec_cols(.VHL_REPORT_PENALTIES))
.vhl_parse_penalties <- function(raw) {
  rec <- .vhl_record(raw, "report", "penalties")
  .vhl_frame(if (is.null(rec)) list() else .vhl_side_rows(rec, "penalties"), .VHL_PENALTY_COLS)
}
.VHL_PLAYER_BOX_COLS <- c(.VHL_GAME_KEY, .VHL_TEAM_COLS, "position_group", .vhl_spec_cols(.VHL_REPORT_SKATERS))
.vhl_parse_player_box <- function(raw) {
  rec <- .vhl_record(raw, "report", "skaters")
  .vhl_frame(if (is.null(rec)) list() else .vhl_side_rows(rec, "skaters"), .VHL_PLAYER_BOX_COLS)
}
.VHL_GOALIE_BOX_COLS <- c(.VHL_GAME_KEY, .VHL_TEAM_COLS, .vhl_spec_cols(.VHL_REPORT_GOALIES))
.vhl_parse_goalie_box <- function(raw) {
  rec <- .vhl_record(raw, "report", "goalies")
  .vhl_frame(if (is.null(rec)) list() else .vhl_side_rows(rec, "goalies"), .VHL_GOALIE_BOX_COLS)
}

.VHL_PBP_COLS <- c("game_id", "season", "stage", "game_number", "event_index", "kind", "kind_code", "time",
                   "clock", "score", "strength_code", "strength", "text", "player_ids", "home_team", "away_team")
.vhl_parse_pbp <- function(raw) {
  rec <- .vhl_record(raw, "online", "events")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_PBP_COLS))
  g <- .vhl_obj(rec[["game"]])
  req <- .vhl_req(rec)
  key <- list(game_id = req[["game_id"]], season = req[["season"]], stage = req[["stage"]],
              game_number = g[["game_number"]], home_team = g[["home_team"]], away_team = g[["away_team"]])
  events <- .vhl_list(rec[["events"]])
  rows <- lapply(seq_along(events), function(i) {
    e <- events[[i]]
    for (k in names(key)) e[k] <- list(key[[k]])
    e[["event_index"]] <- i - 1L
    e
  })
  .vhl_frame(rows, .VHL_PBP_COLS)
}

.VHL_SCOPE_COLS <- c("season", "stage", "tn")
.VHL_STANDINGS_COLS <- c(.VHL_SCOPE_COLS, .vhl_spec_cols(.VHL_STANDINGS))
.vhl_parse_standings <- function(raw) {
  rec <- .vhl_record(raw, "standings", "rows")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_STANDINGS_COLS))
  scope <- .vhl_scope(rec)
  rows <- lapply(.vhl_list(rec[["rows"]]), function(r) { r[names(scope)] <- scope; r })
  .vhl_frame(rows, .VHL_STANDINGS_COLS)
}

.VHL_SKATER_STATS_COLS <- c(.VHL_SCOPE_COLS, "position_group", .vhl_spec_cols(.VHL_STATS_SKATERS))
.VHL_GOALIE_STATS_COLS <- c(.VHL_SCOPE_COLS, .vhl_spec_cols(.VHL_STATS_GOALIES))
.vhl_parse_skater_stats <- function(raw) {
  rec <- .vhl_record(raw, "stats_players", "forwards")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_SKATER_STATS_COLS))
  scope <- .vhl_scope(rec)
  rows <- list()
  for (kg in list(c("defensemen", "defense"), c("forwards", "forward"))) {
    for (r in .vhl_list(rec[[kg[[1]]]])) {
      r[names(scope)] <- scope
      r[["position_group"]] <- kg[[2]]
      rows[[length(rows) + 1L]] <- r
    }
  }
  .vhl_frame(rows, .VHL_SKATER_STATS_COLS)
}
.vhl_parse_goalie_stats <- function(raw) {
  rec <- .vhl_record(raw, "stats_players", "goalies")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_GOALIE_STATS_COLS))
  scope <- .vhl_scope(rec)
  .vhl_frame(lapply(.vhl_list(rec[["goalies"]]), function(r) { r[names(scope)] <- scope; r }),
             .VHL_GOALIE_STATS_COLS)
}

.VHL_PLAYER_COLS <- c("player_id", "name", "jersey", "position", "height_cm", "weight_kg", "born", "age",
                      "country", "photo_url")
.vhl_parse_player <- function(raw) {
  rec <- .vhl_record(raw, "player", "player_id")
  .vhl_frame(if (is.null(rec)) list() else list(rec), .VHL_PLAYER_COLS)
}

.VHL_CAREER_STATS <- unique(c(.vhl_spec_cols(.VHL_CAREER_SKATER), .vhl_spec_cols(.VHL_CAREER_GOALIE)))
.VHL_CAREER_COLS <- c("player_id", "season", "stage", "tn", "team_season_id", "team", "jersey", .VHL_CAREER_STATS)
.vhl_parse_player_career <- function(raw) {
  rec <- .vhl_record(raw, "player", "seasons")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_CAREER_COLS))
  rows <- lapply(.vhl_list(rec[["seasons"]]), function(r) {
    r[["player_id"]] <- rec[["player_id"]]
    r[["season"]] <- .vhl_end_year(r[["season"]])
    r[["stage"]] <- .vhl_stage_of(r[["stage"]])
    r
  })
  .vhl_frame(rows, .VHL_CAREER_COLS)
}

.VHL_ROSTER_COLS <- c("team_page_id", "team", "calendar_tn", "calendar_team_season_id", "roster_as_of",
                      .vhl_spec_cols(.VHL_ROSTER))
.vhl_parse_roster <- function(raw) {
  rec <- .vhl_record(raw, "team", "roster")
  if (is.null(rec)) return(.vhl_frame(list(), .VHL_ROSTER_COLS))
  team <- list(team_page_id = .vhl_req(rec)[["team_page_id"]], team = rec[["name"]],
               calendar_tn = rec[["calendar_tn"]], calendar_team_season_id = rec[["calendar_team_season_id"]],
               roster_as_of = rec[["roster_as_of"]])
  rows <- lapply(.vhl_list(rec[["roster"]]), function(r) {
    for (k in names(team)) r[k] <- list(team[[k]])
    r
  })
  .vhl_frame(rows, .VHL_ROSTER_COLS)
}

# The fastRhockey_data wrapper every exported function returns.
.vhl_out <- function(df, type) make_fastRhockey_data(df, type, Sys.time())

# The shared body of the exported wrappers.
.vhl_run <- function(url, params, parse, type, return_parsed) {
  rec <- .vhl_get(url, params)
  if (!isTRUE(return_parsed)) return(rec)
  .vhl_out(parse(rec), type)
}

# A positive integer id for a URL path segment.
.vhl_digits_id <- function(value, arg) {
  if (!.vhl_digits(value)) {
    .abort_bad_request("{.arg {arg}} must be an integer id; got {.val {value}}.")
  }
  as.integer(trimws(as.character(value)))
}


# ---- exported wrappers ------------------------------------------------------------------------

#' VHL Tournament Id of a Season
#'
#' The VHL tournament id (`tn`) of a season (END year) and stage. Read from the
#' calendar's tournament select, fetched once per R session. The tn is what the site keys
#' every season page by; the wrappers map it for you, so you need it only to read `tn`
#' columns or build a URL by hand.
#'
#' Source: `GET https://www.vhlru.ru/en/calendar/` (server-rendered HTML, keyless; parsed with rvest).
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the calendar lists no such season and stage.
#' * `fastRhockey_bad_request`: `season` is not a year or `stage` is not `"regular"` / `"playoff"`.
#' * `fastRhockey_asset_fetch`: the calendar page could not be fetched or has no tournament select.
#'
#' @param season Season END year (2026 = 2025/26). The calendar covers 2010/11 on.
#' @param stage `"regular"` (default) or `"playoff"`.
#' @return An integer: the tn (`1374` for the 2025/26 regular season, `1375` for its playoffs).
#' @examples
#' \donttest{
#'   try(vhl_tn(2026))
#'   try(vhl_tn(2026, "playoff"))
#' }
#' @family VHL Functions
#' @export
vhl_tn <- function(season, stage = "regular") {
  season <- .vhl_season(season)
  stage <- .vhl_stage(stage)
  tab <- .vhl_tournaments()
  tn <- tab$tn[tab$season == season & tab$stage == stage]
  if (!length(tn)) {
    years <- range(tab$season)
    .abort_no_data("VHL lists no {stage} tournament for season {season} (END year; calendar covers {years[[1]]}-{years[[2]]}).")
  }
  tn[[length(tn)]]
}

#' VHL Schedule
#'
#' Schedule and results of one season stage: every game with its club ids, score and decision.
#'
#' Source: `GET https://www.vhlru.ru/en/calendar/{season}/0/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per game:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `game_number` | integer | Official game number within the tournament (from '№986'). |
#' | `game_date` | character | Game date, ISO 'YYYY-MM-DD': the calendar day heading plus the season (August-December = START year). |
#' | `date` | character | Day header in English, day and month only ('18 March'); the page prints no year and no start time. |
#' | `weekday` | character | Weekday of the day header (English). |
#' | `home_team` | character | Home team name (Latin, FULL form: 'Kristall S'; standings, stats and the team page use the short 'Kristall'). |
#' | `home_club_id` | integer | Home club id: stable across seasons (the img.khl.ru logo path id). Neither a team-season id nor a team page id. |
#' | `away_team` | character | Away team name (Latin, FULL form, like home_team). |
#' | `away_club_id` | integer | Away club id: stable across seasons (the img.khl.ru logo path id). |
#' | `city` | character | City of the home team's arena (English). |
#' | `score` | character | Final score 'home:away'; '-:-' before the game. |
#' | `home_score` | integer | Home goals including the shootout winner goal; null before the game. |
#' | `away_score` | integer | Away goals including the shootout winner goal; null before the game. |
#' | `decision` | character | How the game ended beyond regulation: OT (overtime) or SO (shootout); null for regulation or unplayed games. |
#' | `winner` | character | Winning side from the win-home / win-visitor class: home or away; null before the game. |
#' | `status` | character | Unobserved in samples (null in every capture); type and meaning unverified. |
#' | `online_url` | character | Text-broadcast URL (`online.vhlru.ru/online/{game_id}.html`). |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_schedule(season = 2026))
#'   try(vhl_schedule(season = 2026, stage = "playoff"))
#' }
#' @family VHL Functions
#' @export
vhl_schedule <- function(season, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/calendar/", .vhl_season(season), "/0/"),
    list(stage = stage),
    .vhl_parse_schedule, "VHL Schedule Information from vhlru.ru", return_parsed
  )
}

#' VHL Game Info
#'
#' Game header of a report: date, attendance, score by period, teams, coaches and officials.
#'
#' Source: `GET https://www.vhlru.ru/en/report/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param game_id Required. Game id (game_id in vhl_schedule; the report's and the broadcast's idgame), e.g. 899223.
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per game (one row):
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `game_number` | integer | Official game number (from '№984'); checked against the request. |
#' | `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
#' | `date` | character | Game date, ISO 'YYYY-MM-DD', parsed from date_text. |
#' | `local_time` | character | Start time 'HH:MM' from date_text; local time zone unverified (likely Moscow). |
#' | `date_text` | character | Date line verbatim; RUSSIAN month and weekday even on /en/ ('18 Март 2026, Среда, 17:00'). The `time datetime` attribute is a constant placeholder (2020-01-23 18:00) and is ignored. |
#' | `attendance` | integer | Attendance (from '910 sp'). |
#' | `score` | character | Final score 'home:away' (the shootout counts as one goal). |
#' | `home_score` | integer | Home final goals. |
#' | `away_score` | integer | Away final goals. |
#' | `decision` | character | OT or SO when the game went past regulation; null otherwise. |
#' | `period_scores` | list of character | Score of each period 'home:away', overtime and shootout included (SO game: '0:2','0:0','2:0','0:0','0:1'). |
#' | `home_team_page_id` | integer | Home team page id. Team page id (vhlru.ru only): the `{team_id}` of `/en/teams/{team_id}/` (Toros = 390). Neither a club id nor a team-season id; it joins nothing else (bridge: the calendar row of the same game_id). |
#' | `home_team` | character | Home team name (Latin, FULL form: 'Kristall S'; the report's tables use the short 'Kristall'). |
#' | `home_short` | character | Home team short name (the report tables' form). |
#' | `home_city` | character | Home team city. |
#' | `home_coach` | character | Home head coach, 'Surname Given'. |
#' | `away_team_page_id` | integer | Away team page id. Team page id (vhlru.ru only): the `{team_id}` of `/en/teams/{team_id}/` (Toros = 390). Neither a club id nor a team-season id; it joins nothing else (bridge: the calendar row of the same game_id). |
#' | `away_team` | character | Away team name (Latin, FULL form). |
#' | `away_short` | character | Away team short name (the report tables' form). |
#' | `away_city` | character | Away team city. |
#' | `away_coach` | character | Away head coach, 'Surname Given'. |
#' | `referee1_number` | integer | First referee's sweater number. |
#' | `referee1` | character | First referee, 'Surname Given'. |
#' | `referee2_number` | integer | Second referee's sweater number. |
#' | `referee2` | character | Second referee, 'Surname Given'. |
#' | `linesman1_number` | integer | First linesman's sweater number. |
#' | `linesman1` | character | First linesman, 'Surname Given'. |
#' | `linesman2_number` | integer | Second linesman's sweater number. |
#' | `linesman2` | character | Second linesman, 'Surname Given'. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_game_info(season = 2026, game_id = 899223))
#' }
#' @family VHL Functions
#' @export
vhl_game_info <- function(season, game_id, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/report/", .vhl_season(season), "/"),
    list(idgame = game_id, stage = stage),
    .vhl_parse_game_info, "VHL Game Info Information from vhlru.ru", return_parsed
  )
}

#' VHL Goals
#'
#' Goals of a game with scorer, assists, strength and the jerseys on ice for both teams.
#'
#' One report page feeds `vhl_game_info()`, `vhl_goals()`, `vhl_penalties()`, `vhl_player_box()` and `vhl_goalie_box()`. Each call fetches it (and, for the first game of a season, that season's calendar, cached for the R session). Use `return_parsed = FALSE` to keep the page's record.
#'
#' Source: `GET https://www.vhlru.ru/en/report/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param game_id Required. Game id (game_id in vhl_schedule; the report's and the broadcast's idgame), e.g. 899223.
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per goal:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `game_number` | integer | Official game number (from '№984'); checked against the request. |
#' | `team_side` | character | Side of the game the row belongs to: home or away. |
#' | `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
#' | `goal_number` | integer | Goal number in the game (G). |
#' | `period` | character | Period (Per) as printed: '1', '2', '3', overtime unverified, 'РБ' (Cyrillic even on /en/: shootout, решающий бросок). |
#' | `time` | character | Elapsed game time 'mm:ss' from the opening faceoff (Time); the shootout row says '65:00'. |
#' | `score` | character | Score after the goal 'home:away' (Score). |
#' | `strength` | character | Goal strength code (PMG): eq (even), ppg (power play), shg (shorthanded); null on the shootout row. |
#' | `scorer_id` | integer | Player id of the scorer: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
#' | `scorer` | character | Goal scorer's name, 'Surname Given' as the report prints it. |
#' | `scorer_jersey` | integer | Scorer jersey number. |
#' | `scorer_season_total` | integer | The scorer's season goal count after this goal (the '(13)' suffix); null on the shootout row. |
#' | `assist1_id` | integer | Player id of the first assist; null when unassisted. |
#' | `assist1` | character | First assist name; null when unassisted. |
#' | `assist1_jersey` | integer | First assist jersey number. |
#' | `assist1_season_total` | integer | The first assist's season assist count after this goal (the '(14)' suffix). |
#' | `assist2_id` | integer | Player id of the second assist; null when none. |
#' | `assist2` | character | Second assist name; null when none. |
#' | `assist2_jersey` | integer | Second assist jersey number. |
#' | `assist2_season_total` | integer | The second assist's season assist count after this goal. |
#' | `home_on_ice` | list of integer | Jersey numbers of the home players on ice at the goal (goalie included); the column header is the home team's short name. Shootout row: the goalie only. |
#' | `away_on_ice` | list of integer | Jersey numbers of the away players on ice at the goal (goalie included); the column header is the away team's short name. Empty on the shootout row. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_goals(season = 2026, game_id = 899223))
#' }
#' @family VHL Functions
#' @export
vhl_goals <- function(season, game_id, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/report/", .vhl_season(season), "/"),
    list(idgame = game_id, stage = stage),
    .vhl_parse_goals, "VHL Goals Information from vhlru.ru", return_parsed
  )
}

#' VHL Penalties
#'
#' Penalties of a game: team, time, player, minutes and infraction.
#'
#' Source: `GET https://www.vhlru.ru/en/report/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param game_id Required. Game id (game_id in vhl_schedule; the report's and the broadcast's idgame), e.g. 899223.
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per penalty:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `game_number` | integer | Official game number (from '№984'); checked against the request. |
#' | `team_side` | character | Side of the game the row belongs to: home or away. |
#' | `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `time` | character | Elapsed game time of the penalty 'mm:ss'. |
#' | `player_id` | integer | Player id of the penalised player; null for a bench (team) penalty. |
#' | `player` | character | Penalised player name; null for a bench (team) penalty. |
#' | `player_jersey` | integer | Penalised player's jersey number. |
#' | `minutes` | integer | Penalty length in minutes (the unlabelled third column). |
#' | `infraction` | character | Infraction in English ('hooking'); null when the site leaves it blank. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_penalties(season = 2026, game_id = 899223))
#' }
#' @family VHL Functions
#' @export
vhl_penalties <- function(season, game_id, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/report/", .vhl_season(season), "/"),
    list(idgame = game_id, stage = stage),
    .vhl_parse_penalties, "VHL Penalties Information from vhlru.ru", return_parsed
  )
}

#' VHL Player Box Score
#'
#' Skater box score of a game: scoring, shots, faceoffs, time on ice, hits and blocks.
#'
#' Source: `GET https://www.vhlru.ru/en/report/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param game_id Required. Game id (game_id in vhl_schedule; the report's and the broadcast's idgame), e.g. 899223.
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per skater:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `game_number` | integer | Official game number (from '№984'); checked against the request. |
#' | `team_side` | character | Side of the game the row belongs to: home or away. |
#' | `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `position_group` | character | Position group: defense or forward. |
#' | `jersey` | integer | Jersey number (integer). |
#' | `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
#' | `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
#' | `captaincy` | character | Captaincy marker from the name suffix: C (captain), A (alternate); null otherwise. |
#' | `games_played` | integer | Games played (GP). |
#' | `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `points` | integer | Points, goals + assists (PTS). |
#' | `plus_minus` | integer | Plus/minus (+/-). |
#' | `penalty_minutes` | integer | Penalty minutes (PIM). |
#' | `even_strength_goals` | integer | Even-strength goals (ESG). |
#' | `power_play_goals` | integer | Power-play goals (PPG). |
#' | `shorthanded_goals` | integer | Shorthanded goals (SHG). |
#' | `overtime_goals` | integer | Overtime goals (OTG). |
#' | `game_winning_goals` | integer | Game-winning goals (GWG). |
#' | `shootout_deciding_goals` | integer | Shootout-deciding goals (SDS; the legend says 'Shootouts deciding shots'). |
#' | `shots_on_goal` | integer | Shots on goal (SOG). |
#' | `shot_pct` | numeric | Shooting percentage, goals / shots on goal x 100 (%SOG); 0-100 scale; null when the page shows '-'. |
#' | `faceoffs` | integer | Faceoffs taken (FO). |
#' | `faceoffs_won` | integer | Faceoffs won (FOW). |
#' | `faceoff_pct` | numeric | Faceoff win percentage (%FO); 0-100 scale; null when the page shows '-' (no faceoffs taken). |
#' | `toi_per_game` | character | Time on ice in this game (TOI/G with GP 1), 'mm:ss'. |
#' | `shifts_per_game` | numeric | Shifts in this game (Sft/G with GP 1). |
#' | `hits` | integer | Body checks delivered (Hits). |
#' | `blocked_shots` | integer | Blocked shots (BLS). |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_player_box(season = 2026, game_id = 899223))
#' }
#' @family VHL Functions
#' @export
vhl_player_box <- function(season, game_id, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/report/", .vhl_season(season), "/"),
    list(idgame = game_id, stage = stage),
    .vhl_parse_player_box, "VHL Player Box Information from vhlru.ru", return_parsed
  )
}

#' VHL Goalie Box Score
#'
#' Goalie box score of a game: shots against, saves, save percentage and time on ice.
#'
#' Source: `GET https://www.vhlru.ru/en/report/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param game_id Required. Game id (game_id in vhl_schedule; the report's and the broadcast's idgame), e.g. 899223.
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per goalie:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `game_number` | integer | Official game number (from '№984'); checked against the request. |
#' | `team_side` | character | Side of the game the row belongs to: home or away. |
#' | `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `jersey` | integer | Jersey number (integer). |
#' | `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
#' | `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
#' | `captaincy` | character | Unobserved in samples (null in every capture); type and meaning unverified. |
#' | `games_played` | integer | Games played (GP). |
#' | `wins` | integer | Goalie wins (W). |
#' | `losses` | integer | Goalie losses (L). |
#' | `shootouts_played` | integer | Shootouts played (SOP). |
#' | `shots_against` | integer | Shots on goal faced (the SOG column of goalie tables). |
#' | `goals_against` | integer | Goals allowed (GA). |
#' | `saves` | integer | Saves (Sv): shots on goal the goalie stopped in the row's scope (the game, or the season stage in stats and career rows). |
#' | `save_pct` | numeric | Save percentage (%Sv); 0-100 scale (93.8 = 93.8 %); null when the page shows '-'. |
#' | `gaa` | numeric | Goals-against average (GAA) = 60 x GA / TOI minutes; null when the page shows '-'. |
#' | `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `shutouts` | integer | Shutouts (SO): games in the row's scope in which the goalie allowed no goal. |
#' | `penalty_minutes` | integer | Penalty minutes (PIM). |
#' | `toi` | character | Time on ice in this game, 'mm:ss'; null for a dressed goalie who did not play ('-'). |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_goalie_box(season = 2026, game_id = 899223))
#' }
#' @family VHL Functions
#' @export
vhl_goalie_box <- function(season, game_id, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/report/", .vhl_season(season), "/"),
    list(idgame = game_id, stage = stage),
    .vhl_parse_goalie_box, "VHL Goalie Box Information from vhlru.ru", return_parsed
  )
}

#' VHL Play-by-Play (text broadcast)
#'
#' Text broadcast of a game (online.vhlru.ru, Russian text): goals, penalties, shootout attempts, goalie changes and comments. No shot coordinates.
#'
#' The broadcast has no `/en/` mirror: event text, team names and strength codes are Russian. `season` (and `stage`) locate the game in the calendar, whose game number the page must carry (the page names no game id).
#'
#' Source: `GET https://online.vhlru.ru/online/{game_id}.html` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param game_id Game id (game_id in vhl_schedule; the report's and the broadcast's idgame), e.g. 899223.
#' @param season Required. Season END year of the game (2026 = 2025/26); with `stage` it locates the game in the calendar, whose game number the broadcast page must carry (the page names no game id).
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per event:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `game_number` | integer | Official game number ('Игра номер 984'); checked against the request. |
#' | `event_index` | integer | Order of the event on the page, 0 = the first listed (the broadcast lists the latest event first). |
#' | `kind` | character | Event kind from the CSS modifier: game_state (ga), comment (co), goal (go), penalty (pn), goalie_change (gc), video_review (vr), shootout_attempt (bu). |
#' | `kind_code` | character | Raw CSS modifier of .game-event-item--`code`. |
#' | `time` | character | Time label verbatim: 'mm:ss' elapsed game time, 'NN мин' (minute of a comment), or 'HH:MM' wall clock on game_state events; null when absent. See clock. |
#' | `clock` | character | What time means: game (elapsed mm:ss), minute ('58 мин'), wall (local wall clock, game_state rows); null when there is no time. |
#' | `score` | character | Score 'home-away' (dash, not colon) after a goal; on a shootout attempt, the shootout score printed beside it (before or after that attempt: unverified). |
#' | `strength_code` | character | Goal strength code in Cyrillic: РАВ (even), БОЛ (power play), МЕН (shorthanded, unobserved), БУЛ (penalty shot / shootout); empty string seen as null. |
#' | `strength` | character | strength_code in English: even, power_play, shorthanded, penalty_shot_or_shootout. |
#' | `text` | character | Event text verbatim in RUSSIAN (team, players, infraction, period stats); not translated. |
#' | `player_ids` | list of integer | Player ids linked in the event text (`//vhlru.ru/players/{id}/`), scorer first; empty for most kinds. |
#' | `home_team` | character | Home team name in Cyrillic. |
#' | `away_team` | character | Away team name in Cyrillic. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_pbp(game_id = 899223, season = 2026))
#' }
#' @family VHL Functions
#' @export
vhl_pbp <- function(game_id, season, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_ONLINE, "/online/", .vhl_digits_id(game_id, "game_id"), ".html"),
    list(season = season, stage = stage),
    .vhl_parse_pbp, "VHL Play-by-Play Information from vhlru.ru", return_parsed
  )
}

#' VHL Standings
#'
#' League standings of one regular season (/standings/league/; /standings/regular/ ignores the season).
#'
#' Regular season only: the site's playoff standings are not read.
#'
#' Source: `GET https://www.vhlru.ru/en/standings/league/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per team:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
#' | `rank` | integer | Rank (unlabelled first column). |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `team_season_id` | integer | Team-season id (khl/ terminology): one id per team per tournament, from the KHL's sequence (khl/: Spartak = 14934 in 1369; here Metallurg = 15017 in 1374). The `{id}` of `/calendar/{tn}/0/{id}/` and `/stats/players/{tn}/all/{id}/`. Not the stable club id (*_club_id) and not the team page id. |
#' | `games_played` | integer | Games played (GP). |
#' | `wins` | integer | Regulation wins (W). |
#' | `ot_wins` | integer | Overtime wins (OTW). |
#' | `so_wins` | integer | Shootout wins (SOW). |
#' | `so_losses` | integer | Shootout losses (SOL). |
#' | `ot_losses` | integer | Overtime losses (OTL). |
#' | `losses` | integer | Regulation losses (L). |
#' | `goals_for` | integer | Goals for (left side of the G column '226-131'). |
#' | `goals_against` | integer | Goals against (right side of the G column). |
#' | `points` | integer | Points (PTS): 2 per win of any kind, 1 per OT or SO loss. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_standings(season = 2026))
#' }
#' @family VHL Functions
#' @export
vhl_standings <- function(season, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .vhl_run(
    paste0(.VHL_HOST, "/en/standings/league/", .vhl_season(season), "/"),
    list(),
    .vhl_parse_standings, "VHL Standings Information from vhlru.ru", return_parsed
  )
}

#' VHL Skater Stats
#'
#' Season stats of every defenseman and forward of one season stage.
#'
#' Source: `GET https://www.vhlru.ru/en/stats/players/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per skater and team:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
#' | `position_group` | character | Position group: defense or forward. |
#' | `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
#' | `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
#' | `jersey` | integer | Jersey number (#; the page legend mislabels this column 'Position'). |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `games_played` | integer | Games played (GP). |
#' | `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `points` | integer | Points, goals + assists (PTS). |
#' | `plus_minus` | integer | Plus/minus (+/-). |
#' | `plus` | integer | Plus: on-ice goals for that count toward plus/minus (+). |
#' | `minus` | integer | Minus: on-ice goals against that count toward plus/minus (-). |
#' | `penalty_minutes` | integer | Penalty minutes (PIM). |
#' | `even_strength_goals` | integer | Even-strength goals (ESG). |
#' | `power_play_goals` | integer | Power-play goals (PPG). |
#' | `shorthanded_goals` | integer | Shorthanded goals (SHG). |
#' | `overtime_goals` | integer | Overtime goals (OTG). |
#' | `game_winning_goals` | integer | Game-winning goals (GWG). |
#' | `shootout_deciding_goals` | integer | Shootout-deciding goals (SDS; the legend says 'Shootouts deciding shots'). |
#' | `shots_on_goal` | integer | Shots on goal (SOG). |
#' | `shot_pct` | numeric | Shooting percentage, goals / shots on goal x 100 (%SOG); 0-100 scale; null when the page shows '-'. |
#' | `shots_per_game` | numeric | Shots on goal per game (S/G). |
#' | `faceoffs` | integer | Faceoffs taken (FO). |
#' | `faceoffs_won` | integer | Faceoffs won (FOW). |
#' | `faceoff_pct` | numeric | Faceoff win percentage (%FO); 0-100 scale; null when the page shows '-' (no faceoffs taken). |
#' | `toi_per_game` | character | Average time on ice per game (TOI/G), 'm:ss' / 'mm:ss' string; null when the page shows '-'. |
#' | `shifts_per_game` | numeric | Average shifts per game (Sft/G); integer in a one-game report, decimal in season tables. |
#' | `hits` | integer | Body checks delivered (Hits). |
#' | `blocked_shots` | integer | Blocked shots (BLS). |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_skater_stats(season = 2026))
#' }
#' @family VHL Functions
#' @export
vhl_skater_stats <- function(season, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/stats/players/", .vhl_season(season), "/"),
    list(stage = stage),
    .vhl_parse_skater_stats, "VHL Skater Stats Information from vhlru.ru", return_parsed
  )
}

#' VHL Goalie Stats
#'
#' Season stats of every goalie of one season stage.
#'
#' Source: `GET https://www.vhlru.ru/en/stats/players/{season}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param season Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With `stage` it is mapped to the site's tournament id, see [vhl_tn()].
#' @param stage Season stage: `"regular"` (default) or `"playoff"`.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per goalie and team:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
#' | `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
#' | `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
#' | `jersey` | integer | Jersey number (#; the page legend mislabels this column 'Position'). |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `games_played` | integer | Games played (GP). |
#' | `wins` | integer | Goalie wins (W). |
#' | `losses` | integer | Goalie losses (L). |
#' | `shootouts_played` | integer | Shootouts played (SOP). |
#' | `shots_against` | integer | Shots on goal faced (the SOG column of goalie tables). |
#' | `goals_against` | integer | Goals allowed (GA). |
#' | `saves` | integer | Saves (Sv): shots on goal the goalie stopped in the row's scope (the game, or the season stage in stats and career rows). |
#' | `save_pct` | numeric | Save percentage (%Sv); 0-100 scale (93.8 = 93.8 %); null when the page shows '-'. |
#' | `gaa` | numeric | Goals-against average (GAA) = 60 x GA / TOI minutes; null when the page shows '-'. |
#' | `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `shutouts` | integer | Shutouts (SO): games in the row's scope in which the goalie allowed no goal. |
#' | `penalty_minutes` | integer | Penalty minutes (PIM). |
#' | `toi` | character | Time on ice (TOI), 'mm:ss' string; minutes run past 59 over a season ('2314:48'); null when the page shows '-'. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_goalie_stats(season = 2026, stage = "playoff"))
#' }
#' @family VHL Functions
#' @export
vhl_goalie_stats <- function(season, stage = "regular", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  stage <- .vhl_stage(stage)
  .vhl_run(
    paste0(.VHL_HOST, "/en/stats/players/", .vhl_season(season), "/"),
    list(stage = stage),
    .vhl_parse_goalie_stats, "VHL Goalie Stats Information from vhlru.ru", return_parsed
  )
}

#' VHL Player Card
#'
#' Player card: position, height, weight, birth date, age, country and photo.
#'
#' Source: `GET https://www.vhlru.ru/en/players/{player_id}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param player_id Player id (player_id in any VHL frame), e.g. 34526.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per player (one row):
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `player_id` | integer | Player id, read from the page's own `/players/{id}/games/` tab link; equals the requested id or the capture refuses. |
#' | `name` | character | Player name, 'Surname Given' (Latin), from h1 without the number. |
#' | `jersey` | integer | Current jersey number (the h1 '#79' badge); null when the page shows none. |
#' | `position` | character | Position in English (h2.player-card__subtitle): Forward and Goalkeeper seen; the defence label is unverified. |
#' | `height_cm` | integer | Height in centimetres (Height). |
#' | `weight_kg` | integer | Weight in kilograms (Weight). |
#' | `born` | character | Birth date 'DD.MM.YYYY' as the public page prints it. |
#' | `age` | integer | Age in years on the capture date (from '24 years old'). |
#' | `country` | character | Country (English); null when the card omits it. |
#' | `photo_url` | character | Photo URL on img.khl.ru, `/teamplayers/{team_season_id}/{player_id}/320.jpg`. The team-season id is the one the photo was filed under, not the current team: 34526's photo uses 15424 = Magnitka in tn 1375 (25/26 playoffs, verified on /en/stats/players/1375/) while his newest row is 15516 (Dynamo-Altai, 1430). |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_player(player_id = 34526))
#' }
#' @family VHL Functions
#' @export
vhl_player <- function(player_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .vhl_run(
    paste0(.VHL_HOST, "/en/players/", .vhl_digits_id(player_id, "player_id"), "/"),
    list(),
    .vhl_parse_player, "VHL Player Information from vhlru.ru", return_parsed
  )
}

#' VHL Player Career
#'
#' Player career by season stage and team (skater and goalie columns in one frame).
#'
#' Source: `GET https://www.vhlru.ru/en/players/{player_id}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param player_id Player id (player_id in any VHL frame), e.g. 34526.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per season stage and team:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `player_id` | integer | Player id, read from the page's own `/players/{id}/games/` tab link; equals the requested id or the capture refuses. |
#' | `season` | integer | Season END year (2026 = 2025/26). |
#' | `stage` | character | Season stage: regular or playoff. |
#' | `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
#' | `team_season_id` | integer | Team-season id (khl/ terminology): one id per team per tournament, from the KHL's sequence (khl/: Spartak = 14934 in 1369; here Metallurg = 15017 in 1374). The `{id}` of `/calendar/{tn}/0/{id}/` and `/stats/players/{tn}/all/{id}/`. Not the stable club id (*_club_id) and not the team page id. With tn, the bridge from a player to every tournament team he played for. |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `jersey` | integer | Jersey number worn for that team that season (#). |
#' | `games_played` | integer | Games played (GP). |
#' | `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
#' | `points` | integer | Points, goals + assists (PTS). |
#' | `plus_minus` | integer | Plus/minus (+/-). |
#' | `penalty_minutes` | integer | Penalty minutes (PIM). |
#' | `even_strength_goals` | integer | Even-strength goals (ESG). |
#' | `power_play_goals` | integer | Power-play goals (PPG). |
#' | `shorthanded_goals` | integer | Shorthanded goals (SHG). |
#' | `overtime_goals` | integer | Overtime goals (OTG). |
#' | `game_winning_goals` | integer | Game-winning goals (GWG). |
#' | `shootout_deciding_goals` | integer | Shootout-deciding goals (SDS; the legend says 'Shootouts deciding shots'). |
#' | `shots_on_goal` | integer | Shots on goal (SOG). |
#' | `shot_pct` | numeric | Shooting percentage, goals / shots on goal x 100 (%SOG); 0-100 scale; null when the page shows '-'. |
#' | `shots_per_game` | numeric | Shots on goal per game (S/G). |
#' | `faceoffs` | integer | Faceoffs taken (FO). |
#' | `faceoffs_won` | integer | Faceoffs won (FOW). |
#' | `faceoff_pct` | numeric | Faceoff win percentage (%FO); 0-100 scale; null when the page shows '-' (no faceoffs taken). |
#' | `toi_per_game` | character | Average time on ice per game (TOI/G), 'm:ss' / 'mm:ss' string; null when the page shows '-'. |
#' | `shifts_per_game` | numeric | Average shifts per game (Sft/G); integer in a one-game report, decimal in season tables. |
#' | `hits` | integer | Body checks delivered (Hits). |
#' | `blocked_shots` | integer | Blocked shots (BLS). |
#' | `wins` | integer | Goalie wins (W). |
#' | `losses` | integer | Goalie losses (L). |
#' | `shootouts_played` | integer | Shootouts played (SOP). |
#' | `shots_against` | integer | Shots on goal faced (the SOG column of goalie tables). |
#' | `goals_against` | integer | Goals allowed (GA). |
#' | `saves` | integer | Saves (Sv): shots on goal the goalie stopped in the row's scope (the game, or the season stage in stats and career rows). |
#' | `save_pct` | numeric | Save percentage (%Sv); 0-100 scale (93.8 = 93.8 %); null when the page shows '-'. |
#' | `gaa` | numeric | Goals-against average (GAA) = 60 x GA / TOI minutes; null when the page shows '-'. |
#' | `shutouts` | integer | Shutouts (SO): games in the row's scope in which the goalie allowed no goal. |
#' | `toi` | character | Time on ice (TOI), 'mm:ss' string; minutes run past 59 over a season ('2314:48'); null when the page shows '-'. |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_player_career(player_id = 34526))
#' }
#' @family VHL Functions
#' @export
vhl_player_career <- function(player_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .vhl_run(
    paste0(.VHL_HOST, "/en/players/", .vhl_digits_id(player_id, "player_id"), "/"),
    list(),
    .vhl_parse_player_career, "VHL Player Career Information from vhlru.ru", return_parsed
  )
}

#' VHL Team Roster
#'
#' CURRENT roster of a team page (player names in Cyrillic even on /en/).
#'
#' The roster is the team page's CURRENT one (names are Cyrillic even on `/en/`), not a season's.
#'
#' Source: `GET https://www.vhlru.ru/en/teams/{team_page_id}/` (server-rendered HTML, keyless; parsed with rvest). Unofficial use of a public source; payloads are not redistributed.
#'
#' @section Owner flag (sanctions):
#' The VHL is run by Russian league operators, and several clubs are owned or sponsored by sanctioned state companies. Fetching on your own machine is fine; a published data release or a hosted mirror of this data needs the owner's sanctions / legal sign-off first.
#'
#' @section Errors:
#' * `fastRhockey_no_data`: the VHL has nothing for the request (a season without that stage, a game not in the season's calendar, an unknown game, player or team page).
#' * `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing argument, or HTTP 400 / 422.
#' * `fastRhockey_asset_fetch`: the page could not be fetched, or it is not the page asked for (another season, game or player; a missing table or header drift).
#'
#' @param team_page_id Team page id (home_team_page_id / away_team_page_id in vhl_game_info), e.g. 390. Neither a club id nor a team-season id.
#' @param return_parsed `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's parsed record (a named list, the sdv-internal-refs capture shape, plus `_request` with the season, stage, tn and the ids asked for).
#'
#' @return A `fastRhockey_data` tibble with one row per player:
#'
#' | col_name | type | description |
#' |---|---|---|
#' | `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
#' | `team` | character | Team short name (Latin; the report tables' form). |
#' | `calendar_tn` | integer | Tournament id of the team's CURRENT season stage (the page's 'To the calendar' link). |
#' | `calendar_team_season_id` | integer | The team's team-season id in calendar_tn. |
#' | `roster_as_of` | character | Date the roster is valid for, as printed ('12.10.2026'). |
#' | `jersey` | integer | Jersey number (integer). |
#' | `position` | character | Position in English, lower case: goaltender, defense, forward. |
#' | `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
#' | `player` | character | Player name, 'Surname Given' — in CYRILLIC even on the /en/ page. |
#' | `born` | character | Birth date 'DD.MM.YYYY' as the public page prints it. |
#' | `country` | character | Citizenship (English). |
#'
#' With `return_parsed = FALSE`, the page's parsed record (a named list) instead.
#'
#' @examples
#' \donttest{
#'   try(vhl_roster(team_page_id = 390))
#' }
#' @family VHL Functions
#' @export
vhl_roster <- function(team_page_id, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .vhl_run(
    paste0(.VHL_HOST, "/en/teams/", .vhl_digits_id(team_page_id, "team_page_id"), "/"),
    list(),
    .vhl_parse_roster, "VHL Roster Information from vhlru.ru", return_parsed
  )
}
