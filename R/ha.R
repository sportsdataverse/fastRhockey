# ha.R -- HockeyAllsvenskan (Sweden) wrappers.
#
# Port of sdv-py sportsdataverse/hockey/ha/ (ha_parsers.py, ha_runtime.py and the
# generated ha.py). One keyless host, hockeyallsvenskan.se, throttled to one
# request a second through the shared European hockey transport
# (euro_hockey_runtime.R). The site serves its data through its own Next.js
# routes:
#
# * GET  /pages/matcher  text/html: the season in progress's game list, decoded
#                        from the React Server Components flight data.
# * GET  /api/game?slug= plain JSON.
# * POST every other route, JSON body. The wrapper's arguments are the body
#                        fields, laid over the route's default body (.HA_BODIES,
#                        the bodies the sdv-internal-refs capture sends).
#
# Seasons: the public `season` is the END year (2026 = 2025/26). The site keys a
# season by its START year ("2025" = 2025/26), so the body carries `season - 1`.
# `NULL` is the season in progress (the July-June season containing today).
# Stats and standings start in 2005/06 (season 2006); an earlier season raises
# fastRhockey_no_data without a request.
#
# Refusals (sdv-py ha_runtime._check): a non-2xx or an empty / non-JSON /
# error-enveloped / truncated / out-of-scope 2xx raises fastRhockey_asset_fetch;
# an unknown game, a season or phase without rows raises fastRhockey_no_data; an
# argument outside its set (phase, category, metric, the directory's team), HTTP
# 400/422 raises fastRhockey_bad_request. Wrappers raise; they never warn and
# return an empty frame.
#
# Retries: a POST retries 429/500/502/503/504 and a dead connection up to three
# attempts, each attempt behind the per-host clock, waiting max(2, 4 s) or the
# server's Retry-After (capped at 120 s) between attempts. A 2xx body that opens
# like JSON but does not parse was cut off in transit: it is fetched again, twice.

.HA_HOST <- "https://hockeyallsvenskan.se"
# Game types (`phase`): regular season, playoffs, playout (relegation series).
.HA_PHASES <- c("HA", "HAPlayoff", "HAkval")
# `categories` (column sets) per stats route; anything else answers zero rows.
.HA_CATEGORIES <- list(
  `/api/all-time-skaters-stats` = c(
    "summary", "points", "goals", "time_on_ice", "power_play", "penalty_kill",
    "equal_strength", "penalties", "plus_minus", "faceoffs", "offensive",
    "defensive", "shootouts", "corsi_fenwick_pdo"
  ),
  `/api/all-time-goalkeepers-stats` = c("summary", "results", "game_strength", "defensive", "shootouts"),
  `/api/all-time-teams-stats` = c(
    "results", "goals", "power_play", "penalty_kill", "penalties", "offensive",
    "defensive", "faceoffs", "shootouts", "corsi_fenwick_pdo", "stoppage_reasons",
    "attendance"
  )
)
# First season (END year) with stats and standings: 2005/06.
.HA_FIRST_SEASON <- 2006L
# Key the request is stamped under (the stats bodies do not say what they answer).
.HA_REQUEST_KEY <- "_request"
# Statuses a POST is retried on.
.HA_RETRY_STATUSES <- c(429L, 500L, 502L, 503L, 504L)
# Cap on a server's Retry-After, in seconds (sdv-py dl_utils._MAX_RETRY_AFTER).
.HA_MAX_RETRY_AFTER <- 120
.HA_HEADERS <- list(`User-Agent` = "fastRhockey", Accept = "application/json")

.HA_FILTERS <- list(
  league = "HA", phase = "HA", team_side = "all", team = "all",
  against_team = "all", all_results = "all", game_status = "all", decided_in = "all"
)
# Default JSON body per POST route (the refs capture ROUTES bodies, minus `season`).
.HA_BODIES <- list(
  `/api/play-by-play` = list(),
  `/api/all-time-games-stats` = c(.HA_FILTERS, list(categories = "all", page = 1L, page_size = 1000L)),
  `/api/league-standings-all-time` = list(league = "HA", phase = "HA", location = "all"),
  `/api/all-time-skaters-stats` = c(
    .HA_FILTERS,
    list(categories = "summary", position = "all", ages = "all", page = 1L, page_size = 1000L)
  ),
  `/api/all-time-goalkeepers-stats` = c(.HA_FILTERS, list(categories = "summary", page = 1L, page_size = 1000L)),
  `/api/all-time-teams-stats` = c(.HA_FILTERS, list(categories = "results")),
  `/api/player-leaderboard` = list(
    league = "HA", team = NULL, phase = "HA", metric = "TP", isGoalkeeper = FALSE,
    page = 1L, pageSize = 100L
  ),
  `/api/all-players` = list(
    scope = "league", orderBy = "familyName", teamShortName = NULL,
    leagueShortName = "HA", searchQuery = "", page = 1L, pageSize = 100L
  )
)
# Routes whose body takes a `season` (START year on the wire).
.HA_SEASONED <- c(
  "/api/all-time-games-stats", "/api/league-standings-all-time", "/api/all-time-skaters-stats",
  "/api/all-time-goalkeepers-stats", "/api/all-time-teams-stats"
)
.HA_PAGED <- c("/api/all-time-games-stats", "/api/all-time-skaters-stats", "/api/all-time-goalkeepers-stats")

# Documented column -> type of every frame (sdv-py's returns-schemas,
# tools/codegen/schemas/native/ha/*.yaml). Zero-row results carry these columns;
# populated frames are cast to the same types.
.HA_SCHEMAS <- list(
  schedule = c(
    season = "integer",
    game_cms_id = "integer",
    game_document_id = "character",
    cms_season = "character",
    statnet_game_number = "integer",
    slug = "character",
    round = "integer",
    scheduled_date_time = "character",
    venue = "character",
    attendance = "integer",
    current_period = "character",
    current_period_start = "character",
    home_score = "integer",
    away_score = "integer",
    ended_date_time = "character",
    decided_in = "character",
    home_score_p1 = "integer",
    away_score_p1 = "integer",
    home_score_p2 = "integer",
    away_score_p2 = "integer",
    home_score_p3 = "integer",
    away_score_p3 = "integer",
    home_ot_score = "integer",
    away_ot_score = "integer",
    home_so_score = "integer",
    away_so_score = "integer",
    game_type = "character",
    play_off_game = "character",
    play_off_game_level = "character",
    played_date_time = "character",
    is_completed = "logical",
    home_statnet_id = "character",
    away_statnet_id = "character",
    created_at = "character",
    updated_at = "character",
    published_at = "character",
    home_team_cms_id = "integer",
    home_team_document_id = "character",
    home_team_name = "character",
    home_team_code = "character",
    home_team_main_color_hex = "character",
    home_team_alternate_color_hex = "character",
    home_team_team_arena = "character",
    home_team_arena_address = "character",
    home_team_arena_latitude = "numeric",
    home_team_arena_longitude = "numeric",
    home_team_arena_telephone = "character",
    home_team_tickets_provider_url = "character",
    home_team_club_mate_lottery_url = "character",
    home_team_created_at = "character",
    home_team_updated_at = "character",
    home_team_published_at = "character",
    home_team_logo_url = "character",
    away_team_cms_id = "integer",
    away_team_document_id = "character",
    away_team_name = "character",
    away_team_code = "character",
    away_team_main_color_hex = "character",
    away_team_alternate_color_hex = "character",
    away_team_team_arena = "character",
    away_team_arena_address = "character",
    away_team_arena_latitude = "numeric",
    away_team_arena_longitude = "numeric",
    away_team_arena_telephone = "character",
    away_team_tickets_provider_url = "character",
    away_team_club_mate_lottery_url = "character",
    away_team_created_at = "character",
    away_team_updated_at = "character",
    away_team_published_at = "character",
    away_team_logo_url = "character"
  ),
  game = c(
    season = "integer",
    game_cms_id = "integer",
    game_document_id = "character",
    cms_season = "character",
    statnet_game_number = "integer",
    slug = "character",
    round = "integer",
    scheduled_date_time = "character",
    venue = "character",
    attendance = "integer",
    current_period = "character",
    current_period_start = "character",
    home_score = "integer",
    away_score = "integer",
    ended_date_time = "character",
    decided_in = "character",
    home_score_p1 = "integer",
    away_score_p1 = "integer",
    home_score_p2 = "integer",
    away_score_p2 = "integer",
    home_score_p3 = "integer",
    away_score_p3 = "integer",
    home_ot_score = "integer",
    away_ot_score = "integer",
    home_so_score = "integer",
    away_so_score = "integer",
    game_type = "character",
    play_off_game = "character",
    play_off_game_level = "character",
    played_date_time = "character",
    is_completed = "logical",
    home_statnet_id = "character",
    away_statnet_id = "character",
    created_at = "character",
    updated_at = "character",
    published_at = "character",
    home_team_cms_id = "integer",
    home_team_document_id = "character",
    home_team_name = "character",
    home_team_code = "character",
    home_team_main_color_hex = "character",
    home_team_alternate_color_hex = "character",
    home_team_team_arena = "character",
    home_team_arena_address = "character",
    home_team_arena_latitude = "numeric",
    home_team_arena_longitude = "numeric",
    home_team_arena_telephone = "character",
    home_team_tickets_provider_url = "character",
    home_team_club_mate_lottery_url = "character",
    home_team_created_at = "character",
    home_team_updated_at = "character",
    home_team_published_at = "character",
    home_team_logo_url = "character",
    away_team_cms_id = "integer",
    away_team_document_id = "character",
    away_team_name = "character",
    away_team_code = "character",
    away_team_main_color_hex = "character",
    away_team_alternate_color_hex = "character",
    away_team_team_arena = "character",
    away_team_arena_address = "character",
    away_team_arena_latitude = "numeric",
    away_team_arena_longitude = "numeric",
    away_team_arena_telephone = "character",
    away_team_tickets_provider_url = "character",
    away_team_club_mate_lottery_url = "character",
    away_team_created_at = "character",
    away_team_updated_at = "character",
    away_team_published_at = "character",
    away_team_logo_url = "character"
  ),
  pbp = c(
    statnet_game_number = "integer",
    home_statnet_id = "character",
    away_statnet_id = "character",
    period = "integer",
    period_label = "character",
    event_type = "character",
    time = "integer",
    is_entering = "logical",
    team_name = "character",
    team_code = "character",
    team_statnet_id = "character",
    player_statnet_id = "integer",
    player_first_name = "character",
    player_family_name = "character",
    player_jersey_number = "integer",
    event_description = "character",
    running_score = "character",
    scorer_season_goals = "integer",
    is_penalty_shot = "logical",
    assist1_name = "character",
    assist1_jersey_today = "integer",
    assist1_season_assists = "integer",
    assist2_name = "character",
    assist2_jersey_today = "integer",
    assist2_season_assists = "integer"
  ),
  games = c(
    season = "integer",
    date = "character",
    home_team_code = "character",
    away_team_code = "character",
    home_score = "integer",
    away_score = "integer",
    decided_in = "character",
    is_played = "logical",
    arena = "character",
    attendance = "integer",
    home_shots_total = "integer",
    home_shots_goal = "integer",
    home_shots_efficiency = "numeric",
    home_powerplay_goals = "integer",
    home_powerplay_num_pp = "integer",
    home_powerplay_efficiency = "numeric",
    home_expulsions_in_minutes = "integer",
    away_shots_total = "integer",
    away_shots_goal = "integer",
    away_shots_efficiency = "numeric",
    away_powerplay_goals = "integer",
    away_powerplay_num_pp = "integer",
    away_powerplay_efficiency = "numeric",
    away_expulsions_in_minutes = "integer",
    period_scores = "character",
    most_points = "character"
  ),
  standings = c(
    season = "integer",
    rank = "integer",
    team_code_mixed = "character",
    team_code = "character",
    games_played = "integer",
    regulation_wins = "integer",
    regulation_losses = "integer",
    ties = "integer",
    overtime_and_shootout_wins = "integer",
    overtime_and_shootout_losses = "integer",
    shootouts_wins = "integer",
    shootouts_losses = "integer",
    goals = "integer",
    goals_against = "integer",
    goal_difference = "integer",
    total_points = "integer",
    power_play_perc = "numeric",
    penalty_kill_perc = "numeric"
  ),
  skater_stats = c(
    season = "integer",
    rank = "integer",
    player_name = "character",
    team_statnet_id = "character",
    gp = "integer",
    tp = "integer",
    g = "integer",
    a = "integer",
    pim = "integer",
    gwg = "integer",
    ppg = "integer",
    sog = "integer",
    hits = "integer",
    bks = "integer",
    plus = "integer",
    minus = "integer",
    plus_minus = "integer",
    toi_gp = "character",
    tppg = "numeric"
  ),
  goalie_stats = c(
    season = "integer",
    rank = "integer",
    player_name = "character",
    team_statnet_id = "character",
    svs = "integer",
    svs_perc = "numeric",
    gp = "integer",
    gpi = "integer",
    ga = "integer",
    gaa = "numeric",
    w = "integer",
    t = "integer",
    l = "integer",
    so = "integer",
    mip = "character",
    valid_ranking = "integer"
  ),
  team_stats = c(
    season = "integer",
    rank = "integer",
    team_code = "character",
    gp = "integer",
    wins_total = "integer",
    losses_total = "integer",
    leading = "integer",
    trailing = "integer",
    g = "integer",
    ga = "integer",
    gfg = "integer",
    otw = "integer",
    otl = "integer",
    sow = "integer",
    sol = "integer",
    p_gp = "numeric",
    gpg = "numeric",
    gapg = "numeric",
    points = "integer"
  ),
  teams = c(
    season = "integer",
    team_statnet_id = "character",
    team_code = "character",
    name = "character",
    full_name = "character",
    logos_icon = "character",
    logos_large = "character",
    logos_small = "character",
    logos_medium = "character",
    logos_xlarge = "character"
  ),
  leaders = c(
    rank = "integer",
    player_name = "character",
    team_code = "character",
    team_statnet_id = "character",
    jersey_number = "integer",
    position = "character",
    metric = "character",
    metric_value = "character",
    player_statnet_id = "integer",
    first_name = "character",
    family_name = "character",
    slug = "character",
    headshot = "character",
    headshot_small = "character",
    headshot_medium = "character",
    team_logo = "character",
    team_name = "character",
    team_main_color = "character"
  ),
  players = c(
    family_name = "character",
    first_name = "character",
    player_cms_id = "integer",
    player_document_id = "character",
    headshots_large = "character",
    headshots_small = "character",
    headshots_thumb = "character",
    headshots_medium = "character",
    position_code = "character",
    jersey_number = "integer",
    birth_date = "character",
    country = "character",
    height = "integer",
    weight = "integer",
    shoots = "character",
    youth_team = "character",
    is_rookie = "logical",
    team_player = "character",
    slug = "character",
    position_name = "character",
    team_cms_id = "integer",
    team_document_id = "character",
    team_code = "character",
    team_main_color_hex = "character",
    team_name = "character",
    team_logo_url = "character"
  )
)

# Column -> type across all frames (one type per column name, as in sdv-py's
# _ha_schema.DTYPES).
.HA_DTYPES <- (function() {
  all <- unlist(unname(.HA_SCHEMAS))
  all[!duplicated(names(all))]
})()

# ---- column typing (sdv-py ha_parsers) ---------------------------------------

# Team StatNet ids are text (AIK, MODO); every other `*_id` is a number.
.HA_STR_ID <- "(^|_)statnet_id$"
.HA_INT_ID <- "(^|_)(id|cms_id|player_statnet_id|statnet_game_number)$"
.HA_FLOAT_COL <- "(perc|(^|_)(efficiency|gaa|gpg|gapg|p_gp|tppg|latitude|longitude))$"
# Counts the site sends as strings (standings, CMS games, play-by-play, the
# directory) plus the sampled stats-table counts, pinned so an empty frame
# matches a full one.
.HA_INT_COLS <- c(
  "rank", "games_played", "regulation_wins", "regulation_losses", "ties",
  "overtime_and_shootout_wins", "overtime_and_shootout_losses", "shootouts_wins",
  "shootouts_losses", "goals", "goals_against", "goal_difference", "total_points",
  "round", "attendance", "home_score", "away_score", "home_score_p1", "home_score_p2",
  "home_score_p3", "away_score_p1", "away_score_p2", "away_score_p3", "home_ot_score",
  "away_ot_score", "home_so_score", "away_so_score", "season", "period", "time",
  "scorer_season_goals", "player_jersey_number", "assist1_jersey_today",
  "assist1_season_assists", "assist2_jersey_today", "assist2_season_assists",
  "jersey_number", "height", "weight",
  "gp", "tp", "g", "a", "pim", "gwg", "ppg", "sog", "hits", "bks", "plus", "minus",
  "plus_minus", "svs", "gpi", "ga", "w", "t", "l", "so", "valid_ranking", "wins_total",
  "losses_total", "leading", "trailing", "gfg", "otw", "otl", "sow", "sol", "points",
  paste0(
    rep(c("home", "away"), each = 5),
    "_", c("shots_total", "shots_goal", "powerplay_goals", "powerplay_num_pp", "expulsions_in_minutes")
  )
)
# Object-valued keys that are sometimes null (a player without headshots): no stray null column.
.HA_OBJECT_KEYS <- c("headshots", "logo", "logos", "team", "player", "Assist1", "Assist2")
.HA_BOOL_COLS <- c("is_penalty_shot", "is_played", "is_entering", "is_completed", "is_rookie")
.HA_ROW_KEYS <- c("results", "data", "standings", "players", "games")
# Separators a head count may carry (1 386, 1,386, 1.386).
.HA_COUNT_NOISE <- paste0("[\\s,.'", intToUtf8(c(0xA0L, 0x202FL)), "]")

#' The dtype of a column: its returns-schema type, else by name alone.
#' @param col Column name.
#' @return One of "integer", "numeric", "logical", "character".
#' @noRd
.ha_dtype_for <- function(col) {
  if (col %in% names(.HA_DTYPES)) return(unname(.HA_DTYPES[[col]]))
  if (grepl(.HA_STR_ID, col, perl = TRUE) && !endsWith(col, "player_statnet_id")) return("character")
  if (grepl(.HA_INT_ID, col, perl = TRUE) || col %in% .HA_INT_COLS) return("integer")
  if (grepl(.HA_FLOAT_COL, col, perl = TRUE)) return("numeric")
  if (col %in% .HA_BOOL_COLS) return("logical")
  "character"
}

#' sdv-py `dl_utils.underscore` + the StatNet spelling (`stat_net` -> `statnet`).
#' @noRd
.ha_col <- function(name) {
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", name, perl = TRUE)
  x <- gsub("([a-z\\d])([A-Z])", "\\1_\\2", x, perl = TRUE)
  x <- tolower(gsub("-", "_", x, fixed = TRUE))
  gsub("stat_net", "statnet", x, fixed = TRUE)
}

# ---- JSON shapes -------------------------------------------------------------
# Bodies are parsed with simplifyVector = FALSE: an object is a named list, an
# array an unnamed list, null is NULL.

.ha_is_obj <- function(x) is.list(x) && !is.null(names(x))
.ha_is_arr <- function(x) is.list(x) && is.null(names(x))
.ha_obj <- function(x) if (.ha_is_obj(x)) x else list()
.ha_objs <- function(x) if (.ha_is_arr(x)) Filter(.ha_is_obj, x) else list()
.ha_nonempty_str <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.ha_one_of <- function(x, set) is.character(x) && length(x) == 1L && !is.na(x) && x %in% set
# jsonlite reads 1 as integer and 1.0 / 1e2 as double; a whole double beyond
# 32 bits is a JSON integer too.
.ha_is_pyint <- function(x) {
  is.integer(x) || (is.numeric(x) && is.finite(x) && x == round(x) && abs(x) >= 2^31)
}
# Type-strict integer test: TRUE and 1.0 are not 1 (sdv-py _is_int).
.ha_is_int <- function(x, value = NULL) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && .ha_is_pyint(x) && (is.null(value) || x == value)
}
.ha_truthy <- function(x) {
  if (is.null(x) || length(x) == 0L) return(FALSE)
  if (is.list(x)) return(TRUE)
  !(is.na(x[1]) || identical(x[[1]], FALSE) || identical(x[[1]], 0L) || identical(x[[1]], 0) || identical(x[[1]], ""))
}
.ha_type_name <- function(x) {
  if (is.null(x)) "null"
  else if (.ha_is_arr(x)) "an array"
  else if (is.character(x)) "a string"
  else if (is.numeric(x)) "a number"
  else if (is.logical(x)) "a boolean"
  else class(x)[1]
}

#' Python `json.dumps(x, ensure_ascii=False)` of a parsed value (lists become
#' JSON strings in the frames).
#' @noRd
.ha_py_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (is.null(names(x))) return(paste0("[", paste(vapply(x, .ha_py_json, ""), collapse = ", "), "]"))
    keys <- vapply(names(x), .ha_py_str, "")
    return(paste0("{", paste(paste0(keys, ": ", vapply(x, .ha_py_json, "")), collapse = ", "), "}"))
  }
  if (is.character(x)) return(.ha_py_str(x))
  if (is.logical(x)) return(if (is.na(x)) "null" else if (x) "true" else "false")
  if (is.integer(x) || .ha_is_pyint(x)) return(format(x, scientific = FALSE, trim = TRUE))
  .ha_py_float(x)
}
.ha_py_str <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  s <- gsub("\b", "\\b", s, fixed = TRUE)
  s <- gsub("\f", "\\f", s, fixed = TRUE)
  paste0("\"", s, "\"")
}
# Python float repr: shortest round-trip digits, ".0" on a whole number.
.ha_py_float <- function(x) {
  if (is.na(x)) return("NaN")
  s <- ""
  for (d in 15:17) {
    s <- sprintf("%.*g", d, x)
    if (as.numeric(s) == x) break
  }
  if (!grepl("[.eE]", s)) s <- paste0(s, ".0")
  s
}

# ---- flattening and frames ---------------------------------------------------

#' One flat row: nested objects as `<parent>_<child>`, a `logo` as its url, lists
#' as JSON strings. Values stay as parsed (NULL = null).
#' @noRd
.ha_flatten <- function(obj, prefix = "") {
  out <- list()
  for (key in names(obj)) {
    if (identical(key, .HA_REQUEST_KEY)) next
    value <- obj[[key]]
    name <- paste0(prefix, .ha_col(key))
    if (is.null(value) && key %in% .HA_OBJECT_KEYS) next
    if (identical(key, "logo") && .ha_is_obj(value)) {
      out[paste0(name, "_url")] <- list(value[["url"]])
    } else if (.ha_is_arr(value)) {
      out[name] <- list(if (length(value)) .ha_py_json(value) else NULL)
    } else if (.ha_is_obj(value)) {
      sub <- .ha_flatten(value, paste0(name, "_"))
      for (n in names(sub)) out[n] <- sub[n]
    } else {
      out[name] <- list(value)
    }
  }
  out
}

# The stamped request of a body (see .ha_stamped()).
.ha_request <- function(obj) {
  req <- if (.ha_is_obj(obj)) obj[[.HA_REQUEST_KEY]] else NULL
  if (.ha_is_obj(req)) req else list()
}

#' Rows of a body: a list is rows; an object's rows are its first list under
#' `results`, `data`, `standings`, `players` or `games`; any other non-empty
#' object is one row.
#' @noRd
.ha_rows <- function(raw) {
  if (.ha_is_arr(raw)) return(.ha_objs(raw))
  if (!.ha_is_obj(raw) || !length(raw)) return(list())
  for (key in .HA_ROW_KEYS) {
    if (key %in% names(raw)) return(.ha_objs(raw[[key]]))  # a row key that is not a list is malformed, not one row
  }
  body <- raw[setdiff(names(raw), .HA_REQUEST_KEY)]
  if (length(body)) list(body) else list()
}

# Rename keys (a later duplicate overwrites the earlier value in place).
.ha_rename <- function(row, map) {
  out <- list()
  for (k in names(row)) {
    nk <- if (k %in% names(map)) map[[k]] else k
    out[nk] <- row[k]
  }
  out
}

# Request keys first, never overriding the body.
.ha_stamp <- function(row, req) c(req[setdiff(names(req), names(row))], row)

.ha_na <- function(type, n) {
  switch(type,
    integer = rep(NA_integer_, n), numeric = rep(NA_real_, n),
    logical = rep(NA, n), rep(NA_character_, n)
  )
}

# The polars kind a column of parsed values would infer: String if any string,
# Float64 for ints and floats, else Int64 / Boolean; Null when all null.
.ha_kind <- function(vals) {
  nn <- Filter(Negate(is.null), vals)
  if (!length(nn)) return("Null")
  k <- unique(vapply(nn, function(v) {
    if (is.character(v)) "s" else if (is.logical(v)) "b" else if (.ha_is_pyint(v)) "i" else "f"
  }, ""))
  if ("s" %in% k) return("String")
  if (length(k) == 1L) return(c(b = "Boolean", i = "Int64", f = "Float64")[[k]])
  if (setequal(k, c("i", "f"))) "Float64" else "String"
}

# A value as polars' cast to String writes it (a float keeps its ".0").
.ha_str <- function(v, kind) {
  if (is.null(v)) return(NA_character_)
  if (is.character(v)) return(v)
  if (is.logical(v)) return(if (is.na(v)) NA_character_ else if (v) "true" else "false")
  if (.ha_is_pyint(v) && kind != "Float64") return(format(v, scientific = FALSE, trim = TRUE))
  .ha_py_float(as.numeric(v))
}

# The column as the native vector polars would have inferred.
.ha_native <- function(vals, kind) {
  switch(kind,
    String = vapply(vals, .ha_str, "", kind = kind, USE.NAMES = FALSE),
    Boolean = vapply(vals, function(v) if (is.null(v)) NA else v, NA, USE.NAMES = FALSE),
    Int64 = .ha_to_int(vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0, USE.NAMES = FALSE)),
    Float64 = vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0, USE.NAMES = FALSE)
  )
}

# Doubles holding integers -> R integer (32-bit) when they fit, else kept double.
.ha_to_int <- function(x) {
  if (all(is.na(x) | abs(x) <= .Machine$integer.max)) as.integer(x) else x
}

.ha_trim <- function(x) trimws(x, whitespace = paste0("[[:space:]", intToUtf8(c(0xA0L, 0x202FL)), "]"))

# Strict numeric parse of strings; anything else is NA (polars cast strict=False).
.ha_parse_num <- function(s, type) {
  s <- .ha_trim(s)
  if (type == "integer") {
    ok <- !is.na(s) & grepl("^[+-]?[0-9]+$", s)
    out <- rep(NA_real_, length(s))
    out[ok] <- as.numeric(s[ok])
    .ha_to_int(out)
  } else {
    ok <- !is.na(s) & grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", s)
    out <- rep(NA_real_, length(s))
    out[ok] <- as.numeric(s[ok])
    out
  }
}

# An id column to an integer: only when every non-null value survives an exact
# round trip (sdv-py _cast_ids_int64); otherwise NULL (left as it is).
.ha_id_int <- function(native, kind) {
  if (kind == "String") {
    nn <- native[!is.na(native)]
    if (!all(grepl("^-?[0-9]+$", nn))) return(NULL)
    num <- as.numeric(nn)
    if (!identical(format(num, scientific = FALSE, trim = TRUE), nn)) return(NULL)
    out <- rep(NA_real_, length(native))
    out[!is.na(native)] <- num
    return(.ha_to_int(out))
  }
  if (kind == "Float64") {
    nn <- native[!is.na(native)]
    if (!all(nn == round(nn))) return(NULL)
    return(.ha_to_int(native))
  }
  NULL
}

#' One column of parsed values cast to its pinned type (sdv-py `_typed`).
#' @noRd
.ha_cast_col <- function(vals, col) {
  want <- .ha_dtype_for(col)
  kind <- .ha_kind(vals)
  if (kind == "Null") return(.ha_na(want, length(vals)))
  native <- .ha_native(vals, kind)
  same <- c(Int64 = "integer", Float64 = "numeric", String = "character", Boolean = "logical")[[kind]]
  if (want == "integer" && grepl(.HA_INT_ID, col, perl = TRUE) && kind != "Int64") {
    out <- .ha_id_int(native, kind)
    if (!is.null(out)) return(out)
  }
  if (same == want) return(native)
  as_str <- function() if (kind == "String") native else vapply(vals, .ha_str, "", kind = kind, USE.NAMES = FALSE)
  if (col == "attendance") {
    z <- gsub(.HA_COUNT_NOISE, "", as_str(), perl = TRUE)
    z[!grepl("^[+-]?[0-9]+$", z)] <- NA
    return(.ha_to_int(as.numeric(z)))
  }
  if (want == "logical" && kind == "String") {
    lo <- tolower(native)
    return(ifelse(lo == "true", TRUE, ifelse(lo == "false", FALSE, NA)))
  }
  if (want %in% c("integer", "numeric") && !grepl(.HA_INT_ID, col, perl = TRUE)) {
    return(.ha_parse_num(as_str(), want))
  }
  if (want == "character") return(as_str())
  native
}

#' Rows (flattened named lists) -> named list of typed columns; `list()` when no row has a value.
#' @noRd
.ha_frame <- function(rows) {
  rows <- Filter(length, rows)
  if (!length(rows)) return(list())
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  out <- lapply(cols, function(cn) .ha_cast_col(lapply(rows, function(r) r[[cn]]), cn))
  names(out) <- cols
  out
}

#' Apply the documented columns: an empty frame keeps them, a missing one is
#' added as nulls, and they lead in documented order (undocumented extras after).
#' @noRd
.ha_finish <- function(cols, columns) {
  if (!is.null(columns)) {
    if (!length(cols)) {
      return(.empty_tibble(stats::setNames(vapply(columns, .ha_dtype_for, ""), columns)))
    }
    n <- length(cols[[1]])
    for (cn in setdiff(columns, names(cols))) cols[[cn]] <- .ha_na(.ha_dtype_for(cn), n)
    cols <- cols[c(columns, setdiff(names(cols), columns))]
  }
  tibble::as_tibble(cols)
}

.ha_table <- function(raw, map, columns) {
  req <- .ha_request(raw)
  rows <- lapply(.ha_rows(raw), function(r) .ha_stamp(.ha_rename(.ha_flatten(r), map), req))
  .ha_finish(.ha_frame(rows), columns)
}

# ---- parsers (sdv-py ha_parsers.parse_ha*) ------------------------------------

# Skater and goalkeeper tables: `player` is a name (no id), `team` the StatNet team id.
.ha_parse_generic <- function(raw, columns = NULL) {
  .ha_table(raw, c(player = "player_name", team = "team_statnet_id"), columns)
}

# W / L are TOTALS including overtime and shootout results.
.ha_parse_team_stats <- function(raw, columns = NULL) {
  .ha_table(raw, c(w = "wins_total", l = "losses_total"), columns)
}

# A stats body's `teams` list: the StatNet id <-> display code crosswalk.
.ha_parse_teams <- function(raw, columns = NULL) {
  req <- .ha_request(raw)
  teams <- if (.ha_is_obj(raw)) .ha_objs(raw[["teams"]]) else list()
  rows <- lapply(teams, function(t) {
    .ha_stamp(.ha_rename(.ha_flatten(t), c(statnet_id = "team_statnet_id", team_code = "team_code")), req)
  })
  .ha_finish(.ha_frame(rows), columns)
}

# Upstream `teamCode` mixes the StatNet id and the display code: kept only as
# `team_code_mixed`; `team_code` is `statNetClubLabel`. Wins / losses are REGULATION only.
.ha_parse_standings <- function(raw, columns = NULL) {
  .ha_table(raw, c(
    team_code = "team_code_mixed", statnet_club_label = "team_code",
    wins = "regulation_wins", losses = "regulation_losses",
    overtime_wins = "overtime_and_shootout_wins", overtime_losses = "overtime_and_shootout_losses"
  ), columns)
}

.ha_games_row <- function(game) {
  result <- .ha_obj(game[["GameResult"]])
  stats <- .ha_obj(game[["GameStatistics"]])
  decided <- game[["decided_in"]]
  row <- list(
    date = game[["Date"]],
    home_team_code = game[["HomeTeam"]],
    away_team_code = game[["AwayTeam"]],
    home_score = result[["Home"]],
    away_score = result[["Away"]],
    decided_in = decided,
    is_played = if (is.character(decided)) decided != "" else NULL,
    arena = game[["Arena"]],
    attendance = game[["Attendance"]]
  )
  for (side in c("home", "away")) {
    sub <- .ha_flatten(.ha_obj(stats[[paste0(side, "_team")]]), paste0(side, "_"))
    for (n in names(sub)) row[n] <- sub[n]
  }
  for (pair in list(c("PeriodScores", "period_scores"), c("MostPoints", "most_points"))) {
    value <- game[[pair[1]]]
    row[pair[2]] <- list(if (.ha_is_arr(value) && length(value)) .ha_py_json(value) else NULL)
  }
  row
}

# The games table lists every scheduled game of the season in progress too:
# `is_played` is `decided_in != ""`.
.ha_parse_games <- function(raw, columns = NULL) {
  req <- .ha_request(raw)
  rows <- lapply(.ha_rows(raw), function(g) .ha_stamp(.ha_games_row(g), req))
  .ha_finish(.ha_frame(rows), columns)
}

.ha_cms_game_row <- function(game) {
  map <- c(id = "game_cms_id", document_id = "game_document_id", season = "cms_season")
  for (side in c("home", "away")) {
    map[paste0(side, "_team_id")] <- paste0(side, "_team_cms_id")
    map[paste0(side, "_team_short_name")] <- paste0(side, "_team_code")
  }
  row <- .ha_rename(.ha_flatten(game), map)
  cms <- row[["cms_season"]]
  m <- if (is.character(cms)) regmatches(cms, regexec("^season_([0-9]{4})$", cms))[[1]] else character()
  season <- if (length(m)) as.integer(m[2]) + 1L else NULL
  c(list(season = season), row)
}

# CMS game records (the schedule page's `games`, /api/game's `data`).
.ha_parse_schedule <- function(raw, columns = NULL) {
  .ha_finish(.ha_frame(lapply(.ha_rows(raw), .ha_cms_game_row)), columns)
}

# One row per event (Shot, Goal, Penalty, GoalkeeperEvent, Timeout); no
# coordinates, no on-ice players.
.ha_parse_pbp <- function(raw, columns = NULL) {
  req <- .ha_request(raw)
  rows <- list()
  periods <- if (.ha_is_obj(raw)) .ha_objs(raw[["game_events"]]) else list()
  for (period in periods) {
    head <- list(period = period[["Period"]], period_label = period[["period_label"]])
    for (event in .ha_objs(period[["Events"]])) {
      row <- .ha_rename(.ha_flatten(event), c(type = "event_type"))
      merged <- head
      for (n in names(row)) merged[n] <- row[n]  # the event's own key wins, in the head's position
      rows[[length(rows) + 1L]] <- .ha_stamp(merged, req)
    }
  }
  .ha_finish(.ha_frame(rows), columns)
}

.ha_parse_leaders <- function(raw, columns = NULL) {
  .ha_table(raw, c(player = "player_name", team_id = "team_statnet_id"), columns)
}

# The CMS directory: ids are CMS ids, not StatNet ids.
.ha_parse_players <- function(raw, columns = NULL) {
  .ha_table(raw, c(
    id = "player_cms_id", document_id = "player_document_id",
    team_id = "team_cms_id", team_short_name = "team_code"
  ), columns)
}

# ---- seasons -----------------------------------------------------------------

#' END year of the July-June season containing `today` (2026-10-12 -> 2027).
#' @noRd
.ha_current_season <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  if (m >= 7L) y + 1L else y
}

#' `list(end, wire)` for a public `season`: the END year and the START-year
#' string the site wants (`2026` -> `"2025"`); `NULL` = the season in progress.
#' @noRd
.ha_body_season <- function(season, today = Sys.Date()) {
  end <- if (is.null(season)) .ha_current_season(today) else season
  ok <- length(end) == 1L && !is.logical(end) && (
    (is.numeric(end) && !is.na(end) && end == round(end)) ||
      (is.character(end) && grepl("^[0-9]+$", end))
  )
  if (!ok) .abort_bad_request("{.arg season} must be an END year such as 2026; got {.val {season}}.")
  end <- as.integer(end)
  if (end < .HA_FIRST_SEASON) {
    first <- .HA_FIRST_SEASON
    .abort_no_data("HockeyAllsvenskan stats start in 2005/06 (season {first}); got season {end}")
  }
  list(end = end, wire = as.character(end - 1L))
}

# ---- request body ------------------------------------------------------------

#' `list(body, stamp)` for a POST route: the route's default body overlaid with
#' the non-NULL params, the END-year season mapped to the START-year wire key,
#' and the enumerated fields checked.
#' @noRd
.ha_build_body <- function(path, params = list(), today = Sys.Date()) {
  if (!path %in% names(.HA_BODIES)) {
    .abort_bad_request("Not a HockeyAllsvenskan POST route: {.val {path}}.")
  }
  body <- .HA_BODIES[[path]]
  for (k in names(params)) {
    v <- params[[k]]
    if (!is.null(v)) body[k] <- list(v)
  }
  stamp <- list()
  if (path %in% .HA_SEASONED) {
    s <- .ha_body_season(body[["season"]], today)
    stamp[["season"]] <- s$end
    body[["season"]] <- s$wire
  }
  if ("phase" %in% names(body) && !.ha_one_of(body[["phase"]], .HA_PHASES)) {
    phases <- .HA_PHASES
    got <- body[["phase"]]
    .abort_bad_request("{.arg phase} must be one of {.val {phases}}; got {.val {got}}.")
  }
  allowed <- .HA_CATEGORIES[[path]]
  if (!is.null(allowed) && !.ha_one_of(body[["categories"]], allowed)) {
    got <- body[["categories"]]
    .abort_bad_request("{.arg categories} must be one of {.val {allowed}}; got {.val {got}} (anything else: 0 rows).")
  }
  if (path == "/api/player-leaderboard" && !.ha_nonempty_str(body[["metric"]])) {
    got <- body[["metric"]]
    .abort_bad_request("{.arg metric} must be a non-empty string such as 'TP'; got {.val {got}}.")
  }
  if (path == "/api/all-players" && !.ha_nonempty_str(body[["teamShortName"]])) {
    .abort_bad_request(paste0(
      "{.arg team} is required: /api/all-players pages unreliably league-wide, so ask one team at a time ",
      "(its CMS short name, e.g. 'AIK', 'MORA')."
    ))
  }
  if (path == "/api/play-by-play") {
    n <- body[["statNetGameNumber"]]
    if (!is.null(n)) body[["statNetGameNumber"]] <- if (is.character(n)) n else format(n, scientific = FALSE, trim = TRUE)
    stamp <- list(
      statnet_game_number = body[["statNetGameNumber"]],
      home_statnet_id = body[["homeStatNetId"]],
      away_statnet_id = body[["awayStatNetId"]]
    )
  }
  list(body = body, stamp = stamp)
}

# ---- transport ---------------------------------------------------------------

# Seams the tests replace: the network call and the sleep.
.ha_perform <- function(req) httr2::req_perform(req)
.ha_sleep <- function(seconds) Sys.sleep(seconds)

#' POST a JSON body with retries on `.HA_RETRY_STATUSES` and dead connections.
#'
#' Each attempt waits on the per-host clock. A `Retry-After` header on a retried
#' status (429 and 503 alike) lengthens the wait to what the server asks, capped
#' at 120 s; otherwise 2 s, then 4 s. Any other status is returned at once (the
#' caller decodes it under the error vocabulary); a dead connection on the last
#' attempt raises `fastRhockey_asset_fetch`.
#' @return An httr2 response.
#' @noRd
.ha_post <- function(url, json, timeout = 30, attempts = 3L) {
  req <- .build_request(url, headers = .HA_HEADERS, timeout = timeout) |>
    httr2::req_body_json(json, auto_unbox = TRUE, null = "null") |>
    httr2::req_retry(max_tries = 1) |>
    httr2::req_error(is_error = function(resp) FALSE)
  for (i in seq_len(attempts)) {
    .polite_wait(url)
    retry_after <- 0
    resp <- tryCatch(.ha_perform(req), httr2_failure = function(e) e)
    if (inherits(resp, "httr2_failure")) {
      if (i == attempts) {
        where <- .euro_where(url)
        msg <- conditionMessage(resp)
        .abort_asset_fetch("{where}: fetch failed after {attempts} attempt{?s}: {msg}", parent = resp)
      }
    } else {
      if (!httr2::resp_status(resp) %in% .HA_RETRY_STATUSES || i == attempts) return(resp)
      ra <- suppressWarnings(httr2::resp_retry_after(resp))
      retry_after <- if (is.null(ra) || is.na(ra)) 0 else max(0, ra)
    }
    .ha_sleep(max(2 * 2^(i - 1L), min(retry_after, .HA_MAX_RETRY_AFTER)))
  }
}

# A 2xx body that opens like JSON (`{` / `[`) but did not decode was cut off.
.ha_truncated <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status < 200 || status >= 300 || !length(resp$body)) return(FALSE)
  substr(trimws(.resp_text(resp), "left"), 1, 1) %in% c("{", "[")
}

#' POST and decode as JSON under the error vocabulary; a truncated 2xx body is
#' fetched again, twice (1 s, then 2 s apart).
#' @noRd
.ha_post_json <- function(url, json) {
  for (attempt in 0:2) {
    resp <- .ha_post(url, json)
    out <- tryCatch(
      .euro_resp_json(resp, simplifyVector = FALSE),
      fastRhockey_asset_fetch = function(e) {
        if (attempt < 2L && .ha_truncated(resp)) structure(list(), class = "ha_truncated") else stop(e)
      }
    )
    if (!inherits(out, "ha_truncated")) return(out)
    .ha_sleep(attempt + 1)
  }
}

# ---- the schedule page (HTML) ------------------------------------------------

# Body text; "" for an empty body (httr2 refuses to read one).
.ha_resp_text <- function(resp) if (length(resp$body)) .resp_text(resp) else ""

#' The RSC flight data of a Next.js page: every
#' `self.__next_f.push([1,"..."])` string literal, decoded and concatenated in
#' document order (an element can span chunks).
#' @noRd
.ha_flight_text <- function(page, where) {
  m <- gregexpr(
    "self\\.__next_f\\.push\\(\\[1,\"[^\"\\\\]*+(?:\\\\.[^\"\\\\]*+)*+\"\\]\\)",
    page, perl = TRUE, useBytes = TRUE
  )
  hits <- regmatches(page, m)[[1]]
  Encoding(hits) <- "UTF-8"
  if (!length(hits)) {
    .abort_asset_fetch("{where}: no self.__next_f.push flight data in the page")
  }
  lits <- substr(hits, nchar("self.__next_f.push([1,") + 1L, nchar(hits) - 2L)
  chunks <- vapply(lits, function(lit) {
    out <- tryCatch(jsonlite::parse_json(lit), error = function(e) NULL)
    if (!is.character(out) || length(out) != 1L) {
      .abort_asset_fetch("{where}: undecodable __next_f chunk")
    }
    out
  }, "", USE.NAMES = FALSE)
  paste(chunks, collapse = "")
}

# Byte length of the first JSON object / array at the start of the raw vector
# `b`, or NA. Tokenises string literals and brackets (in bytes: a character-mode
# gregexpr on a UTF-8 string is quadratic); the prefix is grown (x4) until the
# value closes, and tokens past a string cut off by the prefix are ignored.
.ha_json_len <- function(b) {
  if (!length(b) || !b[1] %in% charToRaw("{[")) return(NA_integer_)
  total <- length(b)
  len <- 65536L
  repeat {
    sub <- rawToChar(b[seq_len(min(len, total))])
    tok <- gregexpr("\"[^\"\\\\]*+(?:\\\\.[^\"\\\\]*+)*+\"|[\\[\\]{}]", sub, perl = TRUE, useBytes = TRUE)[[1]]
    if (tok[1] != -1L) {
      ends <- tok + attr(tok, "match.length") - 1L
      txt <- regmatches(sub, list(tok))[[1]]
      if (len < total) {
        is_str <- startsWith(txt, "\"")
        last_str_end <- if (any(is_str)) max(ends[is_str]) else 0L
        q <- gregexpr("\"", sub, fixed = TRUE, useBytes = TRUE)[[1]]
        dangling <- q[q > last_str_end]
        if (length(dangling)) {
          keep <- tok < dangling[1]
          tok <- tok[keep]; ends <- ends[keep]; txt <- txt[keep]
        }
      }
      depth <- cumsum(txt %in% c("{", "[")) - cumsum(txt %in% c("}", "]"))
      hit <- which(depth == 0L)[1]
      if (!is.na(hit)) return(as.integer(ends[hit]))
    }
    if (len >= total) return(NA_integer_)
    len <- as.integer(min(total, as.numeric(len) * 4))
  }
}

#' Props of the `games.game-calendar-*` RSC element: `list(season, team, games, ...)`.
#'
#' In the flight text the element is `["$","$L4c","games.game-calendar-265-0",{props}]`
#' (the `$L4c` / `-265-0` parts vary between deploys); the props are decoded
#' right after the key.
#' @noRd
.ha_extract_game_list <- function(page, where) {
  text <- .ha_flight_text(page, where)
  m <- regexpr("\"(games\\.game-calendar-[^\"]*)\",", text, perl = TRUE)
  if (m == -1L) .abort_asset_fetch("{where}: no games.game-calendar element in the page")
  rest <- charToRaw(substring(text, m + attr(m, "match.length")))
  n <- .ha_json_len(rest)
  props <- NULL
  if (!is.na(n)) {
    json <- rawToChar(rest[seq_len(n)])
    Encoding(json) <- "UTF-8"
    props <- tryCatch(jsonlite::parse_json(json), error = function(e) NULL)
  }
  if (is.null(props)) .abort_asset_fetch("{where}: game-calendar props are not JSON")
  if (!.ha_is_obj(props) || !.ha_is_arr(props[["games"]])) {
    .abort_asset_fetch("{where}: game-calendar props carry no games list")
  }
  props
}

.ha_get_page <- function(url) {
  where <- .euro_where(url)
  resp <- .build_request(url, headers = list(`User-Agent` = "fastRhockey", Accept = "text/html")) |>
    .polite_perform()
  .euro_check_status(resp)
  status <- httr2::resp_status(resp)
  text <- .ha_resp_text(resp)
  if (!nzchar(trimws(text)) && !status %in% c(204L, 205L)) {
    .abort_asset_fetch("{where} answered HTTP {status} with an empty body.")
  }
  ctype <- httr2::resp_content_type(resp)
  if (!isTRUE(grepl("html", ctype, ignore.case = TRUE))) {
    shown <- if (is.na(ctype)) "no content type" else ctype
    .abort_asset_fetch("{where} answered {shown}, not text/html")
  }
  .ha_extract_game_list(text, where)
}

# ---- checks ------------------------------------------------------------------

# `body[[key]]` as a list of objects; empty when missing or empty; refuses any other shape.
.ha_checked_rows <- function(body, key, fail) {
  rows <- body[[key]]
  if (is.null(rows) || (is.list(rows) && length(rows) == 0L)) return(list())
  if (!.ha_is_arr(rows) || !all(vapply(rows, .ha_is_obj, NA))) {
    fail(paste0(key, " is not a list of objects (", .ha_type_name(rows), ")"))
  }
  rows
}

#' Raise unless `body` is a complete, in-scope answer to the request (sdv-py `_check`).
#' @noRd
.ha_check <- function(path, body, request, stamp, where) {
  fail <- function(msg) .abort_asset_fetch("{where}: {msg}")
  nothing <- function(msg) .abort_no_data("{where}: {msg}")
  if (!.ha_is_obj(body)) {
    kind <- .ha_type_name(body)
    fail(paste0("answered ", kind, ", not an object"))
  }
  if (.ha_truthy(body[["error"]])) {
    err <- body[["error"]]
    fail(paste0("answered an error envelope: ", substr(if (is.character(err)) err else .ha_py_json(err), 1, 120)))
  }
  if (path == "/pages/matcher") {
    games <- .ha_checked_rows(body, "games", fail)
    if (!length(games)) nothing("the schedule page lists no games")
    seasons <- unique(lapply(games, function(g) g[["season"]]))
    if (length(seasons) != 1L || !all(vapply(seasons, .ha_nonempty_str, NA))) {
      shown <- paste(vapply(seasons, .ha_py_json, ""), collapse = ", ")
      fail(paste0("the schedule mixes seasons [", shown, "]"))
    }
    if (!all(vapply(games, function(g) .ha_nonempty_str(g[["slug"]]) && .ha_nonempty_str(g[["statNetGameNumber"]]), NA))) {
      fail("a schedule game has no slug or statNetGameNumber")
    }
    return(invisible(TRUE))
  }
  if (path == "/api/game") {
    rows <- .ha_checked_rows(body, "data", fail)
    slug <- request[["slug"]]
    if (!length(rows)) nothing(paste0("no game record for slug '", slug, "' (records start in 2025/26)"))
    if (!all(vapply(rows, function(r) identical(r[["slug"]], slug), NA))) {
      fail(paste0("rows outside slug='", slug, "'"))
    }
    return(invisible(TRUE))
  }
  if (path == "/api/play-by-play") {
    periods <- .ha_checked_rows(body, "game_events", fail)
    events <- list()
    for (p in periods) events <- c(events, unname(.ha_checked_rows(p, "Events", fail)))
    if (!length(events)) {
      nothing(paste0("no play-by-play events for game '", request[["statNetGameNumber"]], "'"))
    }
    teams <- c(request[["homeStatNetId"]], request[["awayStatNetId"]])
    in_scope <- vapply(events, function(e) {
      id <- .ha_obj(e[["team"]])[["statNetId"]]
      is.character(id) && length(id) == 1L && id %in% teams
    }, NA)
    if (!all(in_scope)) {
      fail(paste0(sum(!in_scope), " events of a team outside [", paste(teams, collapse = ", "), "]"))
    }
    return(invisible(TRUE))
  }
  key <- switch(path,
    `/api/league-standings-all-time` = "standings",
    `/api/all-time-teams-stats` = "data",
    `/api/player-leaderboard` = "players",
    `/api/all-players` = "players",
    "results"
  )
  rows <- .ha_checked_rows(body, key, fail)
  if (!length(rows)) {
    nothing("answered no rows (a season or phase without data, or an unknown filter value)")
  }
  if (path %in% .HA_PAGED && !(.ha_is_int(body[["total_pages"]], 1) && .ha_is_int(body[["count"]], length(rows)))) {
    fail(paste0(
      "truncated page: ", length(rows), " of ", .ha_py_json(body[["count"]]), " rows, ",
      .ha_py_json(body[["total_pages"]]), " pages"
    ))
  }
  if (path == "/api/all-time-games-stats") {
    lo <- as.Date(sprintf("%d-07-01", stamp[["season"]] - 1L))
    hi <- as.Date(sprintf("%d-06-30", stamp[["season"]]))
    for (r in rows) {
      d <- r[["Date"]]
      d <- if (is.character(d) && length(d) == 1L && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)) {
        tryCatch(as.Date(d, format = "%Y-%m-%d"), error = function(e) NA)
      } else {
        NA
      }
      if (is.na(d) || d < lo || d > hi) {
        fail(paste0("a game dated ", .ha_py_json(r[["Date"]]), " is outside season ", stamp[["season"]]))
      }
      if (!is.character(r[["decided_in"]])) {
        fail(paste0("a game without a string decided_in (", .ha_py_json(r[["decided_in"]]), ")"))
      }
    }
  } else if (path == "/api/player-leaderboard") {
    if (!(.ha_is_int(body[["totalPages"]], 1) && .ha_is_int(body[["totalCount"]], length(rows)))) {
      fail(paste0(
        "truncated board: ", length(rows), " of ", .ha_py_json(body[["totalCount"]]), " rows, ",
        .ha_py_json(body[["totalPages"]]), " pages"
      ))
    }
    metric <- request[["metric"]]
    if (!all(vapply(rows, function(r) is.character(r[["metric"]]) && identical(r[["metric"]], metric), NA))) {
      fail(paste0("rows outside metric='", metric, "'"))
    }
  } else if (path == "/api/all-players") {
    page <- .ha_obj(body[["pagination"]])
    if (!(.ha_is_int(page[["pageCount"]], 1) && .ha_is_int(page[["total"]], length(rows)))) {
      fail(paste0(
        "multi-page directory: ", length(rows), " of ", .ha_py_json(page[["total"]]), " players, ",
        .ha_py_json(page[["pageCount"]]),
        " pages; league-wide paging overlaps and skips, so only one complete page is accepted"
      ))
    }
    team <- request[["teamShortName"]]
    if (!all(vapply(rows, function(r) identical(.ha_obj(r[["team"]])[["shortName"]], team), NA))) {
      fail(paste0("rows outside teamShortName='", team, "'"))
    }
  }
  invisible(TRUE)
}

.ha_stamped <- function(body, stamp) {
  if (length(stamp)) body[[.HA_REQUEST_KEY]] <- stamp
  body
}

# `/path` of a URL without query string or trailing slash.
.ha_url_path <- function(url) {
  p <- sub("[?#].*$", "", sub("^[a-z]+://[^/?#]+", "", url))
  p <- sub("/+$", "", p)
  if (nzchar(p)) p else "/"
}

#' Fetch one HockeyAllsvenskan route (GET page / GET JSON / POST JSON by path),
#' checked and stamped.
#'
#' @param url Fully built request URL.
#' @param params The wrapper's parameters: the query string of `/api/game`, the
#'   JSON body fields of a POST route (`NULL` keeps the route default), unused by
#'   `/pages/matcher`.
#' @return The decoded body (a list), with the request stamped under `_request`
#'   where the body does not say what it answers.
#' @noRd
.ha_get <- function(url, params = list()) {
  path <- .ha_url_path(url)
  where <- .euro_where(url)
  if (path == "/pages/matcher") {
    body <- .ha_get_page(url)
    .ha_check(path, body, list(), list(), where)
    return(body)
  }
  if (path == "/api/game") {
    clean <- Filter(Negate(is.null), params)
    body <- .euro_get(url, clean, headers = .HA_HEADERS, simplifyVector = FALSE)
    .ha_check(path, body, clean, list(), where)
    return(body)
  }
  built <- .ha_build_body(path, params)
  body <- .ha_post_json(url, built$body)
  .ha_check(path, body, built$body, built$stamp, where)
  .ha_stamped(body, built$stamp)
}

# An argument outside its enumerated set is a bad request.
.ha_check_choice <- function(value, name, choices) {
  if (!.ha_one_of(value, choices)) {
    .abort_bad_request("{.arg {name}} must be one of {.val {choices}}; got {.val {value}}.")
  }
  invisible(value)
}

# ---- exported wrappers (generated from sdv-py's ha.yaml) -----------------------

#' @title **HockeyAllsvenskan Schedule (season in progress)**
#' @description The current season's games (CMS records, decoded from the schedule page): slug, StatNet game number
#'   and team ids, start time, score.
#'
#' The `slug`, `statnet_game_number`, `home_statnet_id`, `away_statnet_id` and `scheduled_date_time`
#' columns are the [ha_pbp()] request. The page lists the season in progress only; `season` is the END
#' year (`cms_season` `season_2026` = 2026/27 is `season` 2027). Ids: `home_statnet_id` /
#' `away_statnet_id` are StatNet team ids, `home_team_code` / `away_team_code` display codes,
#' `game_cms_id` and `home_team_cms_id` / `away_team_cms_id` CMS ids (three team key spaces, kept in
#' separate columns). Scores and attendance are sent as strings and parsed to integers (an attendance
#' of 0 for a played game means unknown).
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name                       |type     |description|
#'    |:------------------------------|:--------|:----------|
#'    |season                         |integer  |Season END year (2026 = 2025/26).|
#'    |game_cms_id                    |integer  |CMS (Strapi) record id of the game.|
#'    |game_document_id               |character|CMS (Strapi) document id of the game.|
#'    |cms_season                     |character|CMS season key as sent (season_2026 = 2026/27: a START year).|
#'    |statnet_game_number            |integer  |StatNet game number (23401 = 2026/27 game 1); the play-by-play request key.|
#'    |slug                           |character|Game slug yyyymmdd-home-away (lower-case CMS short names); the /api/game?slug= key.|
#'    |round                          |integer  |Round number of the game in the season schedule.|
#'    |scheduled_date_time            |character|Scheduled face-off time (ISO 8601 UTC).|
#'    |venue                          |character|Arena name on the CMS game record.|
#'    |attendance                     |integer  |Attendance; null for an unplayed game (0 for a played one means unknown).|
#'    |current_period                 |character|Live period while the game runs; empty string '' before and after.|
#'    |current_period_start           |character|Start time of the live period; null outside a live game.|
#'    |home_score                     |integer  |Home goals, final.|
#'    |away_score                     |integer  |Away goals, final.|
#'    |ended_date_time                |character|End time (ISO 8601 UTC).|
#'    |decided_in                     |character|How the game was decided (FT, OT, SO); empty for a game not yet played.|
#'    |home_score_p1                  |integer  |Home goals in period 1.|
#'    |away_score_p1                  |integer  |Away goals in period 1.|
#'    |home_score_p2                  |integer  |Home goals in period 2.|
#'    |away_score_p2                  |integer  |Away goals in period 2.|
#'    |home_score_p3                  |integer  |Home goals in period 3.|
#'    |away_score_p3                  |integer  |Away goals in period 3.|
#'    |home_ot_score                  |integer  |Home overtime goals; null when the game had no overtime.|
#'    |away_ot_score                  |integer  |Away overtime goals; null when the game had no overtime.|
#'    |home_so_score                  |integer  |Home shootout goals; null when the game had no shootout.|
#'    |away_so_score                  |integer  |Away shootout goals; null when the game had no shootout.|
#'    |game_type                      |character|Game type (phase): HA regular season, HAPlayoff playoffs, HAkval playout.|
#'    |play_off_game                  |character|Game of the playoff series (1, 2, ...); null outside the playoffs.|
#'    |play_off_game_level            |character|Playoff round label.|
#'    |played_date_time               |character|Actual start time (ISO 8601 UTC).|
#'    |is_completed                   |logical  |true once the game is final.|
#'    |home_statnet_id                |character|StatNet team id of the home team (AIK, MODO, MIK).|
#'    |away_statnet_id                |character|StatNet team id of the away team.|
#'    |created_at                     |character|CMS creation time of the record (ISO 8601 UTC).|
#'    |updated_at                     |character|CMS update time of the record (ISO 8601 UTC).|
#'    |published_at                   |character|CMS publish time of the record (ISO 8601 UTC).|
#'    |home_team_cms_id               |integer  |CMS (Strapi) record id of the home team (81 = AIK).|
#'    |home_team_document_id          |character|Home team: CMS (Strapi) document id (string; stable across drafts and locales).|
#'    |home_team_name                 |character|Home team: short club name (the teams list's name).|
#'    |home_team_code                 |character|Display code of the home team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |home_team_main_color_hex       |character|Home team: primary colour (hex).|
#'    |home_team_alternate_color_hex  |character|Home team: secondary colour (hex).|
#'    |home_team_team_arena           |character|Home team: home arena.|
#'    |home_team_arena_address        |character|Home team: postal address of the arena (multi-line, newline-separated).|
#'    |home_team_arena_latitude       |numeric  |Home team: arena latitude, decimal degrees.|
#'    |home_team_arena_longitude      |numeric  |Home team: arena longitude, decimal degrees.|
#'    |home_team_arena_telephone      |character|Home team: arena telephone contact.|
#'    |home_team_tickets_provider_url |character|Home team: ticket shop URL.|
#'    |home_team_club_mate_lottery_url|character|Home team: club lottery URL.|
#'    |home_team_created_at           |character|Home team: CMS creation time of the record (ISO 8601 UTC).|
#'    |home_team_updated_at           |character|Home team: CMS update time of the record (ISO 8601 UTC).|
#'    |home_team_published_at         |character|Home team: CMS publish time of the record (ISO 8601 UTC).|
#'    |home_team_logo_url             |character|Logo URL of the home team.|
#'    |away_team_cms_id               |integer  |CMS (Strapi) record id of the away team.|
#'    |away_team_document_id          |character|Away team: CMS (Strapi) document id (string; stable across drafts and locales).|
#'    |away_team_name                 |character|Away team: short club name (the teams list's name).|
#'    |away_team_code                 |character|Display code of the away team, not the StatNet id.|
#'    |away_team_main_color_hex       |character|Away team: primary colour (hex).|
#'    |away_team_alternate_color_hex  |character|Away team: secondary colour (hex).|
#'    |away_team_team_arena           |character|Away team: home arena.|
#'    |away_team_arena_address        |character|Away team: postal address of the arena (multi-line, newline-separated).|
#'    |away_team_arena_latitude       |numeric  |Away team: arena latitude, decimal degrees.|
#'    |away_team_arena_longitude      |numeric  |Away team: arena longitude, decimal degrees.|
#'    |away_team_arena_telephone      |character|Away team: arena telephone contact.|
#'    |away_team_tickets_provider_url |character|Away team: ticket shop URL.|
#'    |away_team_club_mate_lottery_url|character|Away team: club lottery URL.|
#'    |away_team_created_at           |character|Away team: CMS creation time of the record (ISO 8601 UTC).|
#'    |away_team_updated_at           |character|Away team: CMS update time of the record (ISO 8601 UTC).|
#'    |away_team_published_at         |character|Away team: CMS publish time of the record (ISO 8601 UTC).|
#'    |away_team_logo_url             |character|Logo URL of the away team.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Schedule
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_schedule())
#' }
ha_schedule <- function(return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .ha_get(paste0(.HA_HOST, "/pages/matcher"), list())
  if (return_parsed) make_fastRhockey_data(.ha_parse_schedule(raw, names(.HA_SCHEMAS$schedule)), "HA Schedule Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Game record**
#' @description One game's CMS record by slug (2025/26 onward): StatNet game number and team ids, scores by period,
#'   venue, attendance.
#'
#' Same columns as [ha_schedule()]. Game records start in 2025/26; a slug the site does not know
#' answers `fastRhockey_no_data`.
#' @param slug Game slug yyyymmdd-home-away (slug in ha_schedule), e.g. 20260918-aik-modo.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name                       |type     |description|
#'    |:------------------------------|:--------|:----------|
#'    |season                         |integer  |Season END year (2026 = 2025/26).|
#'    |game_cms_id                    |integer  |CMS (Strapi) record id of the game.|
#'    |game_document_id               |character|CMS (Strapi) document id of the game.|
#'    |cms_season                     |character|CMS season key as sent (season_2026 = 2026/27: a START year).|
#'    |statnet_game_number            |integer  |StatNet game number (23401 = 2026/27 game 1); the play-by-play request key.|
#'    |slug                           |character|Game slug yyyymmdd-home-away (lower-case CMS short names); the /api/game?slug= key.|
#'    |round                          |integer  |Round number of the game in the season schedule.|
#'    |scheduled_date_time            |character|Scheduled face-off time (ISO 8601 UTC).|
#'    |venue                          |character|Arena name on the CMS game record.|
#'    |attendance                     |integer  |Attendance; null for an unplayed game (0 for a played one means unknown).|
#'    |current_period                 |character|Live period while the game runs; empty string '' before and after.|
#'    |current_period_start           |character|Start time of the live period; null outside a live game.|
#'    |home_score                     |integer  |Home goals, final.|
#'    |away_score                     |integer  |Away goals, final.|
#'    |ended_date_time                |character|End time (ISO 8601 UTC).|
#'    |decided_in                     |character|How the game was decided (FT, OT, SO); empty for a game not yet played.|
#'    |home_score_p1                  |integer  |Home goals in period 1.|
#'    |away_score_p1                  |integer  |Away goals in period 1.|
#'    |home_score_p2                  |integer  |Home goals in period 2.|
#'    |away_score_p2                  |integer  |Away goals in period 2.|
#'    |home_score_p3                  |integer  |Home goals in period 3.|
#'    |away_score_p3                  |integer  |Away goals in period 3.|
#'    |home_ot_score                  |integer  |Home overtime goals; null when the game had no overtime.|
#'    |away_ot_score                  |integer  |Away overtime goals; null when the game had no overtime.|
#'    |home_so_score                  |integer  |Home shootout goals; null when the game had no shootout.|
#'    |away_so_score                  |integer  |Away shootout goals; null when the game had no shootout.|
#'    |game_type                      |character|Game type (phase): HA regular season, HAPlayoff playoffs, HAkval playout.|
#'    |play_off_game                  |character|Game of the playoff series (1, 2, ...); null outside the playoffs.|
#'    |play_off_game_level            |character|Playoff round label.|
#'    |played_date_time               |character|Actual start time (ISO 8601 UTC).|
#'    |is_completed                   |logical  |true once the game is final.|
#'    |home_statnet_id                |character|StatNet team id of the home team (AIK, MODO, MIK).|
#'    |away_statnet_id                |character|StatNet team id of the away team.|
#'    |created_at                     |character|CMS creation time of the record (ISO 8601 UTC).|
#'    |updated_at                     |character|CMS update time of the record (ISO 8601 UTC).|
#'    |published_at                   |character|CMS publish time of the record (ISO 8601 UTC).|
#'    |home_team_cms_id               |integer  |CMS (Strapi) record id of the home team (81 = AIK).|
#'    |home_team_document_id          |character|Home team: CMS (Strapi) document id (string; stable across drafts and locales).|
#'    |home_team_name                 |character|Home team: short club name (the teams list's name).|
#'    |home_team_code                 |character|Display code of the home team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |home_team_main_color_hex       |character|Home team: primary colour (hex).|
#'    |home_team_alternate_color_hex  |character|Home team: secondary colour (hex).|
#'    |home_team_team_arena           |character|Home team: home arena.|
#'    |home_team_arena_address        |character|Home team: postal address of the arena (multi-line, newline-separated).|
#'    |home_team_arena_latitude       |numeric  |Home team: arena latitude, decimal degrees.|
#'    |home_team_arena_longitude      |numeric  |Home team: arena longitude, decimal degrees.|
#'    |home_team_arena_telephone      |character|Home team: arena telephone contact.|
#'    |home_team_tickets_provider_url |character|Home team: ticket shop URL.|
#'    |home_team_club_mate_lottery_url|character|Home team: club lottery URL.|
#'    |home_team_created_at           |character|Home team: CMS creation time of the record (ISO 8601 UTC).|
#'    |home_team_updated_at           |character|Home team: CMS update time of the record (ISO 8601 UTC).|
#'    |home_team_published_at         |character|Home team: CMS publish time of the record (ISO 8601 UTC).|
#'    |home_team_logo_url             |character|Logo URL of the home team.|
#'    |away_team_cms_id               |integer  |CMS (Strapi) record id of the away team.|
#'    |away_team_document_id          |character|Away team: CMS (Strapi) document id (string; stable across drafts and locales).|
#'    |away_team_name                 |character|Away team: short club name (the teams list's name).|
#'    |away_team_code                 |character|Display code of the away team, not the StatNet id.|
#'    |away_team_main_color_hex       |character|Away team: primary colour (hex).|
#'    |away_team_alternate_color_hex  |character|Away team: secondary colour (hex).|
#'    |away_team_team_arena           |character|Away team: home arena.|
#'    |away_team_arena_address        |character|Away team: postal address of the arena (multi-line, newline-separated).|
#'    |away_team_arena_latitude       |numeric  |Away team: arena latitude, decimal degrees.|
#'    |away_team_arena_longitude      |numeric  |Away team: arena longitude, decimal degrees.|
#'    |away_team_arena_telephone      |character|Away team: arena telephone contact.|
#'    |away_team_tickets_provider_url |character|Away team: ticket shop URL.|
#'    |away_team_club_mate_lottery_url|character|Away team: club lottery URL.|
#'    |away_team_created_at           |character|Away team: CMS creation time of the record (ISO 8601 UTC).|
#'    |away_team_updated_at           |character|Away team: CMS update time of the record (ISO 8601 UTC).|
#'    |away_team_published_at         |character|Away team: CMS publish time of the record (ISO 8601 UTC).|
#'    |away_team_logo_url             |character|Logo URL of the away team.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Game
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_game(slug = "20260918-aik-modo"))
#' }
ha_game <- function(slug, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .ha_get(paste0(.HA_HOST, "/api/game"), list(
    slug = slug
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_schedule(raw, names(.HA_SCHEMAS$game)), "HA Game Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Play-by-play**
#' @description One game's events (2025/26 onward): shots with outcome, goals with assists and strength, penalties,
#'   goalkeeper changes, timeouts. No coordinates, no on-ice players.
#'
#' One row per event (Shot, Goal, Penalty, GoalkeeperEvent, Timeout); `time` is seconds into the
#' period. The body names no game, so `statnet_game_number`, `home_statnet_id` and `away_statnet_id`
#' come from the request. `team_statnet_id` is the StatNet team id and `team_code` the display code;
#' `player_statnet_id` is the StatNet player id (the same id space as SHL's isa id). Goals carry
#' `running_score`, the strength in `event_description` and the assists by NAME (`assist1_name`). No
#' shot coordinates, no on-ice players. Get the four required ids from a row of [ha_schedule()] or
#' [ha_game()].
#' @param statnet_game_number StatNet game number (statnet_game_number in ha_schedule / ha_game), e.g. 23401.
#' @param home_statnet_id StatNet team id of the home team, e.g. AIK.
#' @param away_statnet_id StatNet team id of the away team, e.g. MODO.
#' @param scheduled_date_time Scheduled start (scheduled_date_time in ha_schedule), ISO 8601 UTC. Required by the site.
#' @param played_date_time Actual start (played_date_time), ISO 8601 UTC. Optional. `NULL` = not sent.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name              |type     |description|
#'    |:---------------------|:--------|:----------|
#'    |statnet_game_number   |integer  |StatNet game number (23401 = 2026/27 game 1); the play-by-play request key.|
#'    |home_statnet_id       |character|StatNet team id of the home team (AIK, MODO, MIK).|
#'    |away_statnet_id       |character|StatNet team id of the away team.|
#'    |period                |integer  |Period number the event belongs to.|
#'    |period_label          |character|Period label ('Period 1').|
#'    |event_type            |character|Event type: Shot, Goal, Penalty, GoalkeeperEvent or Timeout.|
#'    |time                  |integer  |Seconds into the period.|
#'    |is_entering           |logical  |Goalkeeper event: true when the goalkeeper enters, false when he leaves.|
#'    |team_name             |character|Team label as shown on the card (the display team code).|
#'    |team_code             |character|Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |team_statnet_id       |character|StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code.|
#'    |player_statnet_id     |integer  |StatNet player id (the same id space as SHL's isa id, verified on one player).|
#'    |player_first_name     |character|Player: first name of the player.|
#'    |player_family_name    |character|Player: family name of the player.|
#'    |player_jersey_number  |integer  |Player: jersey number of the player.|
#'    |event_description     |character|Shot outcome, goal strength (EQ), penalty text or timeout note.|
#'    |running_score         |character|Score after the goal, home-away.|
#'    |scorer_season_goals   |integer  |Scorer's season goals after this goal.|
#'    |is_penalty_shot       |logical  |Goal scored on a penalty shot.|
#'    |assist1_name          |character|First assist, by name (no id).|
#'    |assist1_jersey_today  |integer  |First assist's jersey number in this game.|
#'    |assist1_season_assists|integer  |First assist's season assists after this goal.|
#'    |assist2_name          |character|Second assist, by name (no id).|
#'    |assist2_jersey_today  |integer  |Second assist's jersey number in this game.|
#'    |assist2_season_assists|integer  |Second assist's season assists after this goal.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Play-by-play
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_pbp(
#'     statnet_game_number = 23401, home_statnet_id = "AIK", away_statnet_id = "MODO",
#'     scheduled_date_time = "2026-09-18T17:00:00.000Z"
#'   ))
#' }
ha_pbp <- function(
  statnet_game_number,
  home_statnet_id,
  away_statnet_id,
  scheduled_date_time,
  played_date_time = NULL,
  return_parsed = TRUE
) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  raw <- .ha_get(paste0(.HA_HOST, "/api/play-by-play"), list(
    statNetGameNumber = statnet_game_number,
    homeStatNetId = home_statnet_id,
    awayStatNetId = away_statnet_id,
    scheduledDateTime = scheduled_date_time,
    playedDateTime = played_date_time
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_pbp(raw, names(.HA_SCHEMAS$pbp)), "HA Pbp Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Games of a season**
#' @description Every game of a season, played or scheduled (is_played): date, display team codes, score, periods,
#'   arena, attendance, shots and power play. No game id.
#'
#' The season in progress lists every scheduled game. An unplayed one has `decided_in == ""`, a 0-0
#' score and a null attendance, so `is_played` is `decided_in != ""`: filter on it rather than on the
#' score. `home_team_code` / `away_team_code` are display codes, while the `most_points` JSON names
#' StatNet team ids. The table carries no game id: join CMS games ([ha_schedule()]) on `date` + team
#' codes. `*_efficiency` is a percentage. The call is refused as truncated unless the site answers one
#' complete page.
#' @param season Season END year (2026 = 2025/26); the site's START-year key is sent as `season - 1`. `NULL` = the
#'   season in progress. Stats and standings start in 2005/06 (2006).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param team_side Home / away split. Allowed: `all`, `home`, `away`.
#' @param team Team filter: all, or one team of the site's filter list (values not probed).
#' @param against_team Opponent filter: all, or one team (values not probed).
#' @param all_results Result filter: W win, T tie, L loss. Allowed: `all`, `W`, `T`, `L`.
#' @param game_status Game status filter: played drops the season's unplayed games. Allowed: `all`, `played`.
#' @param decided_in How the game was decided: ft full time, ot overtime, so shootout, wo walkover. Allowed: `all`, `ft`,
#'   `ot`, `so`, `wo`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name                  |type     |description|
#'    |:-------------------------|:--------|:----------|
#'    |season                    |integer  |Season END year (2026 = 2025/26).|
#'    |date                      |character|Game date (YYYY-MM-DD).|
#'    |home_team_code            |character|Display code of the home team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |away_team_code            |character|Display code of the away team, not the StatNet id.|
#'    |home_score                |integer  |Home goals, final.|
#'    |away_score                |integer  |Away goals, final.|
#'    |decided_in                |character|How the game was decided (FT, OT, SO); empty for a game not yet played.|
#'    |is_played                 |logical  |True when the game has been played (decided_in is not empty).|
#'    |arena                     |character|Arena the game is played in, as the games table names it.|
#'    |attendance                |integer  |Attendance; null for an unplayed game (0 for a played one means unknown).|
#'    |home_shots_total          |integer  |Home team: shots on goal.|
#'    |home_shots_goal           |integer  |Home team: goals.|
#'    |home_shots_efficiency     |numeric  |Home team: shooting percentage (goals per 100 shots); sent as text, 0 or null.|
#'    |home_powerplay_goals      |integer  |Home team: power-play goals.|
#'    |home_powerplay_num_pp     |integer  |Home team: power plays.|
#'    |home_powerplay_efficiency |numeric  |Home team: power-play percentage; sent as text, 0 or null.|
#'    |home_expulsions_in_minutes|integer  |Home team: penalty minutes.|
#'    |away_shots_total          |integer  |Away team: shots on goal.|
#'    |away_shots_goal           |integer  |Away team: goals.|
#'    |away_shots_efficiency     |numeric  |Away team: shooting percentage (goals per 100 shots); sent as text, 0 or null.|
#'    |away_powerplay_goals      |integer  |Away team: power-play goals.|
#'    |away_powerplay_num_pp     |integer  |Away team: power plays.|
#'    |away_powerplay_efficiency |numeric  |Away team: power-play percentage; sent as text, 0 or null.|
#'    |away_expulsions_in_minutes|integer  |Away team: penalty minutes.|
#'    |period_scores             |character|Goals per period, JSON list of (Home, Away); null for an unplayed game.|
#'    |most_points               |character|Top point scorers of the game, JSON list of (player, team (StatNet id), points).|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Games
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_games(season = 2026))
#' }
ha_games <- function(
  season = NULL,
  phase = "HA",
  team_side = "all",
  team = "all",
  against_team = "all",
  all_results = "all",
  game_status = "all",
  decided_in = "all",
  return_parsed = TRUE
) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  .ha_check_choice(team_side, "team_side", c("all", "home", "away"))
  .ha_check_choice(all_results, "all_results", c("all", "W", "T", "L"))
  .ha_check_choice(game_status, "game_status", c("all", "played"))
  .ha_check_choice(decided_in, "decided_in", c("all", "ft", "ot", "so", "wo"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/all-time-games-stats"), list(
    season = season,
    phase = phase,
    team_side = team_side,
    team = team,
    against_team = against_team,
    all_results = all_results,
    game_status = game_status,
    decided_in = decided_in
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_games(raw, names(.HA_SCHEMAS$games)), "HA Games Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Standings**
#' @description Standings of a season. Wins and losses are REGULATION only; overtime counts include shootouts.
#'
#' Wins and losses are REGULATION only (`regulation_wins` / `regulation_losses`) and the overtime
#' counts include shootouts (`overtime_and_shootout_wins` / `_losses`); the team table's `wins_total` /
#' `losses_total` of [ha_team_stats()] are totals including overtime and shootout results, so do not
#' mix them. `team_code` is the display code; the upstream `teamCode` mixes the StatNet id (MODO) and
#' the display code (MORA) and is kept only as `team_code_mixed`. `power_play_perc` /
#' `penalty_kill_perc` arrive as `""` (NA).
#' @param season Season END year (2026 = 2025/26); the site's START-year key is sent as `season - 1`. `NULL` = the
#'   season in progress. Stats and standings start in 2005/06 (2006).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param location Home / away standings split. Allowed: `all`, `home`, `away`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name                    |type     |description|
#'    |:---------------------------|:--------|:----------|
#'    |season                      |integer  |Season END year (2026 = 2025/26).|
#'    |rank                        |integer  |Rank in the table.|
#'    |team_code_mixed             |character|Upstream standings teamCode: the StatNet id for some clubs (MODO), the display code for others (MORA, NVIF, ÖIK). Join on team_code instead.|
#'    |team_code                   |character|Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |games_played                |integer  |Games played in the season and phase.|
#'    |regulation_wins             |integer  |Regulation wins only (the team table's wins_total also counts OT and shootout wins).|
#'    |regulation_losses           |integer  |Regulation losses only.|
#'    |ties                        |integer  |Ties (0 in the overtime era).|
#'    |overtime_and_shootout_wins  |integer  |Overtime wins, shootout wins included.|
#'    |overtime_and_shootout_losses|integer  |Overtime losses, shootout losses included.|
#'    |shootouts_wins              |integer  |Shootout wins as sent (0 in every sampled row: they are counted in overtime_and_shootout_wins).|
#'    |shootouts_losses            |integer  |Shootout losses as sent (0 in every sampled row).|
#'    |goals                       |integer  |Goals scored by the team (goals for).|
#'    |goals_against               |integer  |Goals conceded by the team.|
#'    |goal_difference             |integer  |Goal difference.|
#'    |total_points                |integer  |Standings points of the team.|
#'    |power_play_perc             |numeric  |Power-play percentage; empty-string placeholder '' in every sample.|
#'    |penalty_kill_perc           |numeric  |Penalty-kill percentage; empty-string placeholder '' in every sample.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Standings
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_standings(season = 2026))
#' }
ha_standings <- function(season = NULL, phase = "HA", location = "all", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  .ha_check_choice(location, "location", c("all", "home", "away"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/league-standings-all-time"), list(
    season = season,
    phase = phase,
    location = location
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_standings(raw, names(.HA_SCHEMAS$standings)), "HA Standings Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Skater stats**
#' @description Skater stats of a season, one category (column set) per call. Names and StatNet team ids, no player
#'   ids.
#'
#' Names and StatNet team ids only: the table carries no player id (use [ha_leaders()] for the current
#' season's StatNet player ids). The `season` column is the request's END year (the table does not
#' say). Only the sampled `summary` column set is pinned in the documented columns below; other
#' `category` values bring their own count columns (sent as numbers, parsed from the payload).
#' @param season Season END year (2026 = 2025/26); the site's START-year key is sent as `season - 1`. `NULL` = the
#'   season in progress. Stats and standings start in 2005/06 (2006).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param category Stats category (column set), one of summary, points, goals, time_on_ice, power_play, penalty_kill,
#'   equal_strength, penalties, plus_minus, faceoffs, offensive, defensive, shootouts, corsi_fenwick_pdo.
#'   Anything else answers zero rows. Allowed: `summary`, `points`, `goals`, `time_on_ice`, `power_play`,
#'   `penalty_kill`, `equal_strength`, `penalties`, `plus_minus`, `faceoffs`, `offensive`, `defensive`,
#'   `shootouts`, `corsi_fenwick_pdo`.
#' @param position Position filter (back = defence). Allowed: `all`, `forward`, `back`, `goalkeeper`.
#' @param ages Age-group filter. Allowed: `all`, `junior`, `rookie`.
#' @param team_side Home / away split. Allowed: `all`, `home`, `away`.
#' @param team Team filter: all, or one team of the site's filter list (values not probed).
#' @param against_team Opponent filter: all, or one team (values not probed).
#' @param all_results Result filter: W win, T tie, L loss. Allowed: `all`, `W`, `T`, `L`.
#' @param game_status Game status filter: played drops the season's unplayed games. Allowed: `all`, `played`.
#' @param decided_in How the game was decided: ft full time, ot overtime, so shootout, wo walkover. Allowed: `all`, `ft`,
#'   `ot`, `so`, `wo`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name       |type     |description|
#'    |:--------------|:--------|:----------|
#'    |season         |integer  |Season END year (2026 = 2025/26).|
#'    |rank           |integer  |Rank in the table.|
#'    |player_name    |character|Player name (the table carries no player id).|
#'    |team_statnet_id|character|StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code.|
#'    |gp             |integer  |Games played in the season and phase.|
#'    |tp             |integer  |Points (goals + assists).|
#'    |g              |integer  |Goals scored by the skater.|
#'    |a              |integer  |Assists credited to the skater.|
#'    |pim            |integer  |Penalty minutes.|
#'    |gwg            |integer  |Game-winning goals.|
#'    |ppg            |integer  |Power-play goals.|
#'    |sog            |integer  |Shots on goal by the skater.|
#'    |hits           |integer  |Hits (0 in seasons before tracking).|
#'    |bks            |integer  |Blocked shots (0 in seasons before tracking).|
#'    |plus           |integer  |Even-strength / short-handed goals for while on ice (plus count).|
#'    |minus          |integer  |Goals against while on ice (minus count).|
#'    |plus_minus     |integer  |Plus/minus: the Plus count less the Minus count.|
#'    |toi_gp         |character|Average time on ice per game, mm:ss string ('0:00' = not tracked, e.g. 2010).|
#'    |tppg           |numeric  |Points per game.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Skater Stats
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_skater_stats(season = 2026, category = "summary"))
#' }
ha_skater_stats <- function(
  season = NULL,
  phase = "HA",
  category = "summary",
  position = "all",
  ages = "all",
  team_side = "all",
  team = "all",
  against_team = "all",
  all_results = "all",
  game_status = "all",
  decided_in = "all",
  return_parsed = TRUE
) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  .ha_check_choice(category, "category", c("summary", "points", "goals", "time_on_ice", "power_play", "penalty_kill", "equal_strength", "penalties", "plus_minus", "faceoffs", "offensive", "defensive", "shootouts", "corsi_fenwick_pdo"))
  .ha_check_choice(position, "position", c("all", "forward", "back", "goalkeeper"))
  .ha_check_choice(ages, "ages", c("all", "junior", "rookie"))
  .ha_check_choice(team_side, "team_side", c("all", "home", "away"))
  .ha_check_choice(all_results, "all_results", c("all", "W", "T", "L"))
  .ha_check_choice(game_status, "game_status", c("all", "played"))
  .ha_check_choice(decided_in, "decided_in", c("all", "ft", "ot", "so", "wo"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/all-time-skaters-stats"), list(
    season = season,
    phase = phase,
    categories = category,
    position = position,
    ages = ages,
    team_side = team_side,
    team = team,
    against_team = against_team,
    all_results = all_results,
    game_status = game_status,
    decided_in = decided_in
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_generic(raw, names(.HA_SCHEMAS$skater_stats)), "HA Skater Stats Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Goalkeeper stats**
#' @description Goalkeeper stats of a season, one category per call. Names and StatNet team ids, no player ids.
#'
#' Names and StatNet team ids only: the table carries no player id. The `season` column is the
#' request's END year. Only the sampled `summary` column set is pinned in the documented columns below;
#' other `category` values bring their own columns.
#' @param season Season END year (2026 = 2025/26); the site's START-year key is sent as `season - 1`. `NULL` = the
#'   season in progress. Stats and standings start in 2005/06 (2006).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param category Stats category (column set), one of summary, results, game_strength, defensive, shootouts. Anything
#'   else answers zero rows. Allowed: `summary`, `results`, `game_strength`, `defensive`, `shootouts`.
#' @param team_side Home / away split. Allowed: `all`, `home`, `away`.
#' @param team Team filter: all, or one team of the site's filter list (values not probed).
#' @param against_team Opponent filter: all, or one team (values not probed).
#' @param all_results Result filter: W win, T tie, L loss. Allowed: `all`, `W`, `T`, `L`.
#' @param game_status Game status filter: played drops the season's unplayed games. Allowed: `all`, `played`.
#' @param decided_in How the game was decided: ft full time, ot overtime, so shootout, wo walkover. Allowed: `all`, `ft`,
#'   `ot`, `so`, `wo`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name       |type     |description|
#'    |:--------------|:--------|:----------|
#'    |season         |integer  |Season END year (2026 = 2025/26).|
#'    |rank           |integer  |Rank in the table.|
#'    |player_name    |character|Player name (the table carries no player id).|
#'    |team_statnet_id|character|StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code.|
#'    |svs            |integer  |Saves made by the goalkeeper.|
#'    |svs_perc       |numeric  |Save percentage ('92.61').|
#'    |gp             |integer  |Games played in the season and phase.|
#'    |gpi            |integer  |Games played in (appeared).|
#'    |ga             |integer  |Goals against: goals allowed in the season and phase.|
#'    |gaa            |numeric  |Goals-against average.|
#'    |w              |integer  |Wins credited to the goalkeeper.|
#'    |t              |integer  |Ties credited to the goalkeeper (0 in the overtime era).|
#'    |l              |integer  |Losses credited to the goalkeeper.|
#'    |so             |integer  |Shutouts recorded by the goalkeeper.|
#'    |mip            |character|Minutes played, total mm:ss string ('1726:51').|
#'    |valid_ranking  |integer  |1 when the goalkeeper qualifies for the ranked list (minimum games), else 0.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Goalie Stats
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_goalie_stats(season = 2026, category = "summary"))
#' }
ha_goalie_stats <- function(
  season = NULL,
  phase = "HA",
  category = "summary",
  team_side = "all",
  team = "all",
  against_team = "all",
  all_results = "all",
  game_status = "all",
  decided_in = "all",
  return_parsed = TRUE
) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  .ha_check_choice(category, "category", c("summary", "results", "game_strength", "defensive", "shootouts"))
  .ha_check_choice(team_side, "team_side", c("all", "home", "away"))
  .ha_check_choice(all_results, "all_results", c("all", "W", "T", "L"))
  .ha_check_choice(game_status, "game_status", c("all", "played"))
  .ha_check_choice(decided_in, "decided_in", c("all", "ft", "ot", "so", "wo"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/all-time-goalkeepers-stats"), list(
    season = season,
    phase = phase,
    categories = category,
    team_side = team_side,
    team = team,
    against_team = against_team,
    all_results = all_results,
    game_status = game_status,
    decided_in = decided_in
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_generic(raw, names(.HA_SCHEMAS$goalie_stats)), "HA Goalie Stats Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Team stats**
#' @description Team stats of a season, one category per call. W / L are totals including overtime and shootout
#'   (wins_total / losses_total).
#'
#' `wins_total` / `losses_total` (the site's W / L) are TOTALS including overtime and shootout results
#' (`otw`, `sow`, `otl`, `sol` are subsets); standings' `regulation_wins` count regulation wins only.
#' `team_code` is the display code. Only the sampled `results` column set is pinned in the documented
#' columns below.
#' @param season Season END year (2026 = 2025/26); the site's START-year key is sent as `season - 1`. `NULL` = the
#'   season in progress. Stats and standings start in 2005/06 (2006).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param category Stats category (column set), one of results, goals, power_play, penalty_kill, penalties, offensive,
#'   defensive, faceoffs, shootouts, corsi_fenwick_pdo, stoppage_reasons, attendance. Anything else
#'   answers zero rows. Allowed: `results`, `goals`, `power_play`, `penalty_kill`, `penalties`,
#'   `offensive`, `defensive`, `faceoffs`, `shootouts`, `corsi_fenwick_pdo`, `stoppage_reasons`,
#'   `attendance`.
#' @param team_side Home / away split. Allowed: `all`, `home`, `away`.
#' @param team Team filter: all, or one team of the site's filter list (values not probed).
#' @param against_team Opponent filter: all, or one team (values not probed).
#' @param all_results Result filter: W win, T tie, L loss. Allowed: `all`, `W`, `T`, `L`.
#' @param game_status Game status filter: played drops the season's unplayed games. Allowed: `all`, `played`.
#' @param decided_in How the game was decided: ft full time, ot overtime, so shootout, wo walkover. Allowed: `all`, `ft`,
#'   `ot`, `so`, `wo`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name    |type     |description|
#'    |:-----------|:--------|:----------|
#'    |season      |integer  |Season END year (2026 = 2025/26).|
#'    |rank        |integer  |Rank in the table.|
#'    |team_code   |character|Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |gp          |integer  |Games played in the season and phase.|
#'    |wins_total  |integer  |Wins in total, overtime and shootout wins included (otw and sow are subsets).|
#'    |losses_total|integer  |Losses in total, overtime and shootout losses included (otl and sol are subsets).|
#'    |leading     |integer  |Games or situations leading (StatNet 'Leading'; meaning unverified).|
#'    |trailing    |integer  |Games or situations trailing (StatNet 'Trailing'; meaning unverified).|
#'    |g           |integer  |Goals scored by the team.|
#'    |ga          |integer  |Goals against: goals allowed in the season and phase.|
#'    |gfg         |integer  |Games in which the team scored the first goal (inferred from the label; unverified).|
#'    |otw         |integer  |Overtime wins (a subset of wins_total).|
#'    |otl         |integer  |Overtime losses.|
#'    |sow         |integer  |Shootout wins (a subset of wins_total).|
#'    |sol         |integer  |Shootout losses.|
#'    |p_gp        |numeric  |Points per game.|
#'    |gpg         |numeric  |Goals for per game (number).|
#'    |gapg        |numeric  |Goals against per game (number).|
#'    |points      |integer  |Standings points: 3 per regulation win, 2 per overtime/shootout win, 1 per overtime/shootout loss (2025 IFB: 102 + 14 + 3 = 119).|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Team Stats
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_team_stats(season = 2026, category = "results"))
#' }
ha_team_stats <- function(
  season = NULL,
  phase = "HA",
  category = "results",
  team_side = "all",
  team = "all",
  against_team = "all",
  all_results = "all",
  game_status = "all",
  decided_in = "all",
  return_parsed = TRUE
) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  .ha_check_choice(category, "category", c("results", "goals", "power_play", "penalty_kill", "penalties", "offensive", "defensive", "faceoffs", "shootouts", "corsi_fenwick_pdo", "stoppage_reasons", "attendance"))
  .ha_check_choice(team_side, "team_side", c("all", "home", "away"))
  .ha_check_choice(all_results, "all_results", c("all", "W", "T", "L"))
  .ha_check_choice(game_status, "game_status", c("all", "played"))
  .ha_check_choice(decided_in, "decided_in", c("all", "ft", "ot", "so", "wo"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/all-time-teams-stats"), list(
    season = season,
    phase = phase,
    categories = category,
    team_side = team_side,
    team = team,
    against_team = against_team,
    all_results = all_results,
    game_status = game_status,
    decided_in = decided_in
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_team_stats(raw, names(.HA_SCHEMAS$team_stats)), "HA Team Stats Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Team key crosswalk**
#' @description Teams of a season with both team keys: StatNet team id <-> display code (from the stats tables'
#'   teams list).
#'
#' The StatNet team id <-> display code crosswalk of a season, read from the goalkeeper table's `teams`
#' list (e.g. MoDo = MODO, MORA = MIK, NVIF = NYB, OIK = OSIK). Join standings and team stats on
#' `team_code`, play-by-play on `team_statnet_id`.
#' @param season Season END year (2026 = 2025/26); the site's START-year key is sent as `season - 1`. `NULL` = the
#'   season in progress. Stats and standings start in 2005/06 (2006).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name       |type     |description|
#'    |:--------------|:--------|:----------|
#'    |season         |integer  |Season END year (2026 = 2025/26).|
#'    |team_statnet_id|character|StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code.|
#'    |team_code      |character|Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |name           |character|Short club name (the teams list's name).|
#'    |full_name      |character|Full club name (the teams list's fullName).|
#'    |logos_icon     |character|Logo: icon-size logo URL.|
#'    |logos_large    |character|Logo: large logo URL.|
#'    |logos_small    |character|Logo: small logo rendition (500 px).|
#'    |logos_medium   |character|Logo: medium logo URL.|
#'    |logos_xlarge   |character|Logo: extra-large logo URL.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Teams
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_teams(season = 2026))
#' }
ha_teams <- function(season = NULL, phase = "HA", return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/all-time-goalkeepers-stats"), list(
    season = season,
    phase = phase
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_teams(raw, names(.HA_SCHEMAS$teams)), "HA Teams Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Player leaderboard**
#' @description Current-season top 25 of one metric, with the StatNet player id (the only stats route that has one).
#'
#' The current season's top 25 of one metric. The only stats route with a player id:
#' `player_statnet_id` (StatNet player id). `team_statnet_id` is the StatNet team id and `team_code`
#' the display code; `metric_value` stays character (a count for `TP`, a clock for `TOI/GP`). The call
#' is refused as truncated unless the board is one complete page.
#' @param metric Leaderboard metric. Seen on the site: TP, G, A, PlusMinus, Hits, FOPerc, TOI/GP (skaters).
#' @param phase Game type: HA regular season, HAPlayoff playoffs, HAkval playout (relegation series). Allowed: `HA`,
#'   `HAPlayoff`, `HAkval`.
#' @param team Team filter; omitted = the whole league (values not probed). `NULL` = not sent.
#' @param is_goalkeeper True for a goalkeeper metric. `NULL` = not sent.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name         |type     |description|
#'    |:----------------|:--------|:----------|
#'    |rank             |integer  |Rank in the table.|
#'    |player_name      |character|Player name (the table carries no player id).|
#'    |team_code        |character|Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |team_statnet_id  |character|StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code.|
#'    |jersey_number    |integer  |Jersey number of the player.|
#'    |position         |character|Playing position: F forward, D defence (G goalkeeper on goalie metrics).|
#'    |metric           |character|Metric code (echo of the request metric).|
#'    |metric_value     |character|Value of the metric (a count, a percentage or a clock, as text).|
#'    |player_statnet_id|integer  |StatNet player id (the same id space as SHL's isa id, verified on one player).|
#'    |first_name       |character|First name of the player.|
#'    |family_name      |character|Family name of the player.|
#'    |slug             |character|Player slug; joins ha_players.|
#'    |headshot         |character|Headshot URL (medium rendition).|
#'    |headshot_small   |character|Small headshot URL.|
#'    |headshot_medium  |character|Medium headshot URL.|
#'    |team_logo        |character|Logo URL of the player's team.|
#'    |team_name        |character|Team label as shown on the card (the display team code).|
#'    |team_main_color  |character|Team primary colour (hex).|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Leaders
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_leaders(metric = "TP"))
#' }
ha_leaders <- function(
  metric,
  phase = "HA",
  team = NULL,
  is_goalkeeper = NULL,
  return_parsed = TRUE
) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(phase, "phase", c("HA", "HAPlayoff", "HAkval"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/player-leaderboard"), list(
    metric = metric,
    phase = phase,
    team = team,
    isGoalkeeper = is_goalkeeper
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_leaders(raw, names(.HA_SCHEMAS$leaders)), "HA Leaders Information from hockeyallsvenskan.se", Sys.time()) else raw
}

#' @title **HockeyAllsvenskan Player directory**
#' @description One team's players from the CMS directory: CMS ids, position, jersey, birth date, size, headshots.
#'   One team per call (league-wide paging is unreliable).
#'
#' Ids are CMS ids (`player_cms_id`, `player_document_id`, `team_cms_id`), not StatNet ids; `team_code`
#' is the team's display code and `slug` joins [ha_leaders()]. One team per call: the directory pages
#' unreliably league-wide, so only one complete page of that team's players is accepted.
#' @param team Team display code (CMS short name: AIK, MORA, ÖIK, ...). Required: the directory pages unreliably
#'   league-wide.
#' @param order_by Sort: familyName or firstName. Allowed: `familyName`, `firstName`.
#' @param search_query Free-text name search; omitted = everyone. `NULL` = not sent.
#' @param return_parsed Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the raw
#'   JSON as a list (stamped with the request under `_request` where the body does not say what it answers).
#' @return A tibble (`return_parsed = TRUE`), else the raw JSON list:
#'
#'    |col_name           |type     |description|
#'    |:------------------|:--------|:----------|
#'    |family_name        |character|Family name of the player.|
#'    |first_name         |character|First name of the player.|
#'    |player_cms_id      |integer  |CMS (Strapi) record id of the player (not the StatNet id).|
#'    |player_document_id |character|CMS (Strapi) document id of the player.|
#'    |headshots_large    |character|Headshot URL (large rendition).|
#'    |headshots_small    |character|Headshot URL (small rendition).|
#'    |headshots_thumb    |character|Headshot URL (thumb rendition).|
#'    |headshots_medium   |character|Headshot URL (medium rendition).|
#'    |position_code      |character|Playing position code (CE centre, LW left wing, ...); positionName spells it out in English.|
#'    |jersey_number      |integer  |Jersey number of the player.|
#'    |birth_date         |character|Birth date, YYYY-MM-DD.|
#'    |country            |character|Nationality, IOC/ISO alpha-3 code (SWE, NOR).|
#'    |height             |integer  |Player height in centimetres.|
#'    |weight             |integer  |Player weight in kilograms.|
#'    |shoots             |character|Shoots / catches: L or R.|
#'    |youth_team         |character|Youth (development) club.|
#'    |is_rookie          |logical  |True when the CMS directory flags the player as a rookie.|
#'    |team_player        |character|Card label 'TEAM // POS JERSEY // Family name'.|
#'    |slug               |character|Player slug; joins ha_leaders.|
#'    |position_name      |character|Playing position, English label (Center, Left Wing, ...).|
#'    |team_cms_id        |integer  |CMS (Strapi) record id of the team (81 = AIK).|
#'    |team_document_id   |character|CMS (Strapi) document id of the team.|
#'    |team_code          |character|Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id.|
#'    |team_main_color_hex|character|Primary colour of the team (hex).|
#'    |team_name          |character|Team label as shown on the card (the display team code).|
#'    |team_logo_url      |character|Logo URL of the team.|
#' @section Errors:
#' `fastRhockey_no_data`: the site has nothing for the request (a season before 2005/06, an unknown or
#' pre-2025/26 game, a phase not played, a body without rows). `fastRhockey_bad_request`: a value
#' outside its enumerated set, a field the site requires is missing, or HTTP 400/422.
#' `fastRhockey_asset_fetch`: any other non-2xx, a connection failure after retries, or an empty,
#' non-JSON, error-shaped, truncated or out-of-scope 200 (the answer is unknown, not empty).
#' @keywords HA Players
#' @family HA Functions
#' @export
#' @examples
#' \donttest{
#'   try(ha_players(team = "AIK"))
#' }
ha_players <- function(team, order_by = "familyName", search_query = NULL, return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  .ha_check_choice(order_by, "order_by", c("familyName", "firstName"))
  raw <- .ha_get(paste0(.HA_HOST, "/api/all-players"), list(
    teamShortName = team,
    orderBy = order_by,
    searchQuery = search_query
  ))
  if (return_parsed) make_fastRhockey_data(.ha_parse_players(raw, names(.HA_SCHEMAS$players)), "HA Players Information from hockeyallsvenskan.se", Sys.time()) else raw
}

