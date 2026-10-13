# nationalleague.R -- National League (Switzerland) app API wrappers.
#
# Mirrors sdv-py sportsdataverse/hockey/nl/ (nationalleague_runtime.py,
# nationalleague_parsers.py and the generated nl_* wrappers), route for route.
# The API (www.nationalleague.ch/api) is keyless JSON in four body shapes: row
# lists (/games*, /player*, /teams), one object (/player/{id}, /teams/{id}/info),
# bracket trees (/games/playoffs, /games/playouts) and the game page
# (/games/{gameId}, which feeds six wrappers).
#
# robots.txt on www.nationalleague.ch disallows /api. The owner approved building
# on it on 2026-10-12 for polite, incremental use only: at least 1 s between
# requests (the shared per-host throttle in euro_hockey_runtime.R, on every
# attempt, retries included), one worker, and no bulk backfill or published
# mirror without asking again. The ruling is repeated in every exported
# function's @details.
#
# Seasons are END years (2026 = 2025/26) and go on the wire unchanged. Ids are
# whole-number doubles (sdv-py Int64): a 14-digit game_id exceeds R's 32-bit
# integer. Every other whole-number column is an integer.

.NL_HOST <- "https://www.nationalleague.ch/api"

# ---- small helpers -----------------------------------------------------------

.nl_is_obj <- function(x) is.list(x) && !is.null(names(x))
.nl_is_arr <- function(x) is.list(x) && is.null(names(x))
.nl_obj <- function(x) if (.nl_is_obj(x)) x else list()
.nl_arr <- function(x) if (.nl_is_arr(x)) x else list()
.nl_is_digits <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && grepl("^[0-9]+$", x)

# Python str() of a JSON scalar (used for dictionary keys, so 9 and "9" meet).
.nl_str <- function(x) {
  if (is.null(x)) return("None")
  if (is.logical(x)) return(if (isTRUE(x)) "True" else "False")
  if (is.numeric(x) && length(x) == 1 && is.finite(x) && x == floor(x) && abs(x) < 1e15) {
    return(sprintf("%.0f", x))
  }
  paste(as.character(x), collapse = "")
}

.nl_title <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))

# What describes the body in a refusal message.
.nl_kind <- function(x) {
  if (is.null(x)) "null" else if (.nl_is_obj(x)) "object" else if (.nl_is_arr(x)) "array" else class(x)[1]
}

# A dictionary with duplicate names: first position, last value (Python dict semantics).
.nl_dedupe <- function(x) {
  u <- unique(names(x))
  x[vapply(u, function(k) max(which(names(x) == k)), 1L)]
}

# Set b's entries into a (existing names keep their position).
.nl_set <- function(a, b) {
  for (i in seq_along(b)) a[names(b)[i]] <- b[i]
  a
}

# sdv-py dl_utils.underscore(): camelCase -> snake_case (teamID -> team_id, sogOt1 -> sog_ot1).
.nl_underscore <- function(word) {
  word <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", word, perl = TRUE)
  word <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", word, perl = TRUE)
  tolower(gsub("-", "_", word, fixed = TRUE))
}

# json.dumps(x, ensure_ascii=False) for the list cells (tv_partner, referees, streak).
.nl_json_str <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  paste0("\"", s, "\"")
}
.nl_json_dumps <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (.nl_is_obj(x)) {
      if (!length(x)) return("{}")
      return(paste0("{", paste0(vapply(names(x), .nl_json_str, ""), ": ",
                                vapply(x, .nl_json_dumps, ""), collapse = ", "), "}"))
    }
    return(paste0("[", paste(vapply(x, .nl_json_dumps, ""), collapse = ", "), "]"))
  }
  if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
  if (is.character(x)) return(.nl_json_str(x))
  .nl_str(x)
}

# ---- the api key never leaves the getter or a parser -------------------------

# `x` without any api-key entry (any case or separator) and with apiKey= URL values redacted.
.nl_scrub <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[!grepl("^api[_-]?key$", names(x), ignore.case = TRUE)]
    return(lapply(x, .nl_scrub))
  }
  if (is.character(x)) {
    return(gsub("(?i)(api[_-]?key=)[^&#\\s\"']+", "\\1<redacted>", x, perl = TRUE))
  }
  x
}

# One level of {k: v} per leaf: nested non-empty objects become parent_child snake_case keys.
.nl_flatten <- function(obj) {
  keys <- character()
  vals <- list()
  walk <- function(o, prefix) {
    nms <- names(o)
    for (i in seq_along(o)) {
      key <- .nl_underscore(nms[i])
      name <- if (nzchar(prefix)) paste0(prefix, "_", key) else key
      v <- o[[i]]
      if (.nl_is_obj(v) && length(v) > 0) {
        walk(v, name)
      } else {
        keys[length(keys) + 1L] <<- name
        vals[length(vals) + 1L] <<- list(v)
      }
    }
  }
  walk(obj, "")
  names(vals) <- keys
  if (anyDuplicated(keys)) vals <- .nl_dedupe(vals)
  vals
}

.nl_rename <- function(row, map) {
  if (!length(map) || !length(row)) return(row)
  nm <- names(row)
  hit <- match(nm, names(map))
  nm[!is.na(hit)] <- unname(map[hit[!is.na(hit)]])
  names(row) <- nm
  if (anyDuplicated(nm)) row <- .nl_dedupe(row)
  row
}

# The row rule: an array -> its non-empty object items; a non-empty object -> one row; else none.
.nl_rows <- function(raw) {
  if (.nl_is_arr(raw)) return(Filter(function(r) .nl_is_obj(r) && length(r) > 0, raw))
  if (.nl_is_obj(raw) && length(raw) > 0) return(list(raw))
  list()
}

# Flattened, renamed rows of `raw`; lists_only refuses an object (a list route's malformed body).
.nl_flat_rows <- function(raw, renames = NULL, lists_only = FALSE) {
  if (lists_only && !.nl_is_arr(raw)) return(list())
  lapply(.nl_rows(.nl_scrub(raw)), function(r) .nl_rename(.nl_flatten(r), renames))
}

# ---- documented schemas (sdv-py _SCHEMAS; codes s = character, i = whole number,
# f = double, b = logical) -------------------------------------------------------

.nl_schema <- function(spec) {
  tok <- strsplit(trimws(spec), "[[:space:]]+")[[1]]
  out <- sub("^.*:", "", tok)
  names(out) <- sub(":[sifb]$", "", tok)
  out
}

.NL_GAME_ROW <- "
game_id:i home_team_short_name:s home_team_name:s away_team_short_name:s away_team_name:s home_team_id:i
away_team_id:i vimeo_video_id:i date:s status:s base_status:s home_team_result:i away_team_result:i
is_overtime:b is_shootout:b highlight_vimeo_video_id:i has_highlight:b is_current:b is_current_for_team:b
tv_partner:s spectators:i has_my_sports:b referees:s is_exhibition:b home_team_coach:s away_team_coach:s
game_time:i show_game_time:b arena:s
"
.NL_SERIES <- "
series_group:i series_home_team_short_name:s series_away_team_short_name:s series_home_team_id:i
series_away_team_id:i series_home_team_result:i series_away_team_result:i series_best_of:i series_title:s
round:s
"
.NL_PLAYER_ROW <- "
player_id:i first_name:s last_name:s team_id:i team_short_name:s team_name:s number:s position:s
topscorer_points:i topscorer_point_average:f topscorer_market_value:i topscorer_market_value_trend:i
topscorer_amount:i is_topscorer:b nationality:s birth:s height:i weight:i hand:s lcrid:i noc:i gp:i g:i a1:i
a2:i penalties:i pim:i geq:i gpp:i gpk:i gwg:i ps:i pg:i pl:i mi:i toi:i toi_eq:i toipp:i toi_pk:i sog:i shm:i
shp:i shb:i bks:i fow:i fol:i shi:i gwgk:i gw:i gl:i fk:i ga:i sa:i svs:i so:i mip:i assists:i points:i
goal_efficiency:f save_percentage:f face_offs_percentage:f face_offs:i is_captain:b first_goalkeeper:b
ga_per_game:f points_per_game:f svs_per_game:f bks_per_game:f sog_per_game:f shm_per_game:f shp_per_game:f
fow_per_game:f fol_per_game:f toi_eq_per_game:f toi_pp_per_game:f toi_pk_per_game:f
face_offs_per_game_percentage:f toi_average:f plus_minus:i tgs:i sep:i toi_percentage:f gwit:i glit:i sag:i
sago:i sam:i sap:i sab:i saslot:i sagoslot:i aeq:i app:i apk:i ha:i fo2:i fo5:i fo10:i gm:i mp:i pm:i pwg:i
sgo:i fowoz:i fownz:i fowdz:i foloz:i folnz:i foldz:i psg:i pssvs:i got:i ot_rank:i ot_g:i ot_a1:i ot_a2:i
ot_toi:i ot_a:i ot_p:i psgwg:i sogslot:i pswg:i toishi:f shigp:f saslotgp:f samgp:f sapgp:f svs_pslot:f
sa_per_game:f otw:i otl:i fopoz:f fopnz:f fopdz:f fot:i fogp:f psm:i ps_percentage:f gw_shutout:i
sogslot_per_game:f psag:i pssvs_percentage:f show_rank:b chf:i team_stats:s
"
.NL_TEAM_ROW <- "
rank:i team_id:i name:s short_name:s streak:s gp:i gw:i gwot:i gwpe:i gl:i glot:i glpe:i po:i g:i geq:i gpp:i
gpk:i ga:i gaeq:i gapp:i gapk:i go:i ppo:i pks:i sog:i penalties:i pim:i fow:i fol:i shm:i shp:i shb:i bks:i
sa:i sam:i sap:i sab:i sort_order:i website:s position_change:s is_live:b current_result:i
current_opponent_result:i pos:i hg:i gsow:i gfd:i gff:i gfi:i gasow:i pp:i bp:i fo2:i fo5:i fo10:i gm:i mp:i
ps:i psg:i psm:i psa:i psga:i pssvs:i spec:i cap:i fowoz:i fownz:i fowdz:i foloz:i folnz:i foldz:i sgo:i sslot:i
sag:i sago:i saslot:i qualification_status:s is_qualified_chl:b goal_efficiency:f ppg_efficiency:f ppggp:f
ppopgp:f ppgp:f gpg:f gagp:f gappgp:f geqgp:f gaeqgp:f bks_per_game:f pim_per_game:f ps_percentage:f
attendance:i capacity:i utilization:f soggp:f sslotgp:f shmgp:f shpgp:f gpkgp:f pk_efficiency:f pksgp:f gapkgp:f
bpgp:f fot:i face_offs_percentage:f fogp:f fowgp:f folgp:f fopoz:f fopnz:f fopdz:f sagp:f saslotgp:f gffd:i
gffp:f gfdp:f gfip:f pssvs_percentage:f pgp:f gwit:i gwo:i glit:i glo:i
"
.NL_TEAM_INFO <- "
name:s cms_node_id:i url_segment:s type:s content_type:s url:s fields_grid:s fields_logo_name:s fields_logo_id:i
fields_logo_url_segment:s fields_logo_type:s fields_logo_content_type:s fields_logo_url:s
fields_logo_fields_umbraco_file_src:s fields_logo_fields_umbraco_file_focal_point:s
fields_logo_fields_umbraco_file_crops:s fields_logo_fields_umbraco_width:i fields_logo_fields_umbraco_height:i
fields_logo_fields_umbraco_bytes:i fields_logo_fields_umbraco_extension:s team_id:i fields_website:s
fields_ticket_url:s fields_game_ticket_url:s fields_overriden_name:s fields_players:s fields_api_name:s
"
.NL_OVERVIEW <- "
game_id:i home_team_short_name:s home_team_name:s away_team_short_name:s away_team_name:s home_team_id:i
away_team_id:i vimeo_video_id:i date:s status:s base_status:s home_team_result:i away_team_result:i
is_overtime:b is_shootout:b highlight_vimeo_video_id:i has_highlight:b is_current:b is_current_for_team:b
tv_partner:s spectators:i has_my_sports:b referees:s is_exhibition:b home_team_coach:s away_team_coach:s
game_time:i show_game_time:b arena:s home_team_first_result:i away_team_first_result:i home_team_second_result:i
away_team_second_result:i home_team_third_result:i away_team_third_result:i home_team_overtime_result:i
away_team_overtime_result:i home_team_overtime1_result:i away_team_overtime1_result:i
home_team_overtime2_result:i away_team_overtime2_result:i home_team_overtime3_result:i
away_team_overtime3_result:i home_team_shootout_result:i away_team_shootout_result:i home_aggregated_score:s
away_aggregated_score:s
"
.NL_PBP <- "
game_id:i event_index:i team_side:s game_seconds:i team_id:i home_team:b time:s video_time:i video_vimeo_id:i
action:s foul_type:i foul_minutes:i player_first_name:s player_last_name:s player_number:s player_total:i
period:i overtime:i goal_situation:s home_team_result:i away_team_result:i assist1_first_name:s
assist1_last_name:s assist1_number:s assist1_total:i assist2_first_name:s assist2_last_name:s assist2_number:s
assist2_total:i situation:s revoked_goal_type:i revoked_goal_sub_type:i review_successful:s review_goal_counts:b
"
.NL_LINEUPS <- "
game_id:i team_side:s team_id:i unit:s player_id:i position:s line:i first_name:s last_name:s number:s
is_topscorer:b is_captain:b first_goalkeeper:b
"
.NL_PLAYER_BOX <- "
game_id:i team_side:s team_id:i player_id:i number:s first_name:s last_name:s short_name:s name:s position:s
fow:i fol:i sog:i shm:i shp:i shb:i bks:i toi:i toi_eq:i toi_pp:i toi_pk:i pim:i g:i a1:i a2:i pl:i mi:i mip:i
ga:i svs:i sa:i gaa:f save_percentage:f faceoffs:i faceoffs_percentage:f a:i pts:i pl_mi:i
"
.NL_TEAM_STATS <- "
game_id:i team_side:s team_id:i g:i ga:i sa:i sog1:i sog2:i sog3:i sog_ot:i sog_ot1:i sog_ot2:i sog_ot3:i shm:i
shp:i shb:i bks:i fow_d:i fow_n:i fow_o:i fol_n:i fol_o:i fol_d:i ppt:i ppo:i ppg:i ppga:i pkt:i pks:i pkg:i
pkga:i faceoffs:i pim:i sog:i sog_percentage:f pp_percentage:f pk_percentage:f fow:i fol:i faceoffs_percentage:f
"
.NL_SHOTS <- "
game_id:i team_side:s team_id:i player_id:i player_number:s player_first_name:s player_last_name:s period:i
time:i strength:s shot_type:s pos_x:f pos_y:f pos_x_percentage:f pos_y_percentage:f video_time:i xg:f xg_sum:f
unix:i unix_start:i
"

.NL_SCHEMAS <- list(
  games = .nl_schema(.NL_GAME_ROW),
  bracket = .nl_schema(paste(.NL_SERIES, .NL_GAME_ROW)),
  player_stats = .nl_schema(.NL_PLAYER_ROW),
  standings = .nl_schema(.NL_TEAM_ROW),
  team_info = .nl_schema(.NL_TEAM_INFO),
  game_overview = .nl_schema(.NL_OVERVIEW),
  pbp = .nl_schema(.NL_PBP),
  lineups = .nl_schema(.NL_LINEUPS),
  player_box = .nl_schema(.NL_PLAYER_BOX),
  team_stats = .nl_schema(.NL_TEAM_STATS),
  shots = .nl_schema(.NL_SHOTS)
)

# `fo` means two things upstream: penalties taken in season rows, faceoffs taken in game rows.
.NL_SEASON_RENAMES <- c(fo = "penalties")
.NL_GAME_BOX_RENAMES <- c(fo = "faceoffs", fo_percentage = "faceoffs_percentage")

# ---- typing: documented schema first, a value-based guess for a column the API adds ----

# Whole-number columns that exceed R's 32-bit integer are doubles: every id, plus these.
.NL_BIG_COLUMNS <- c("unix", "unix_start", "fields_logo_fields_umbraco_bytes")
.nl_is_id <- function(name) name == "id" || endsWith(name, "_id")
.nl_whole_is_double <- function(name) .nl_is_id(name) || name %in% .NL_BIG_COLUMNS

.nl_rtype <- function(name, code) {
  switch(code, s = "character", b = "logical", f = "double",
         i = if (.nl_whole_is_double(name)) "double" else "integer")
}

# Dtype for a column the schema does not list (a field the API added later).
.nl_infer <- function(name, values) {
  present <- Filter(Negate(is.null), values)
  if (.nl_is_id(name)) return("i")
  if (length(present) && all(vapply(present, is.logical, NA))) return("b")
  if (length(present) && all(vapply(present, function(v) is.numeric(v) && length(v) == 1, NA))) {
    non_whole <- any(vapply(present, function(v) v != floor(v), NA))
    return(if (grepl("(percentage|per_game|average|efficiency|utilization|^xg)", name) || non_whole) "f" else "i")
  }
  "s"
}

# One value as a scalar of `code`'s type; anything that does not fit becomes NA.
.nl_coerce1 <- function(v, code) {
  na <- switch(code, s = NA_character_, b = NA, NA_real_)
  if (is.null(v)) return(na)
  if (code == "s") {
    if (is.list(v)) return(.nl_json_dumps(v))
    if (is.logical(v)) return(if (isTRUE(v)) "true" else "false")
    return(if (is.character(v)) v else .nl_str(v))
  }
  if (code == "b") return(if (is.logical(v) && length(v) == 1) v else NA)
  if (is.logical(v) || is.list(v) || length(v) != 1) return(na)
  if (code == "i") {
    if (is.numeric(v)) return(if (is.finite(v) && v == floor(v)) as.double(v) else na)
    if (is.character(v)) {
      text <- trimws(v)
      return(if (grepl("^-?[0-9]+$", text)) as.double(text) else na)
    }
    return(na)
  }
  if (is.numeric(v)) return(as.double(v))
  if (is.character(v)) return(suppressWarnings(as.double(v)))
  na
}

.nl_cells <- function(values, code) {
  proto <- switch(code, s = character(1), b = logical(1), numeric(1))
  vapply(values, .nl_coerce1, proto, code = code, USE.NAMES = FALSE)
}

# One typed column: the documented code, widened (never dropped) when a value does not fit it.
.nl_column <- function(name, values, code) {
  cells <- .nl_cells(values, code)
  misfit <- is.na(cells) & !vapply(values, is.null, NA) & !vapply(values, identical, NA, "")
  if (!.nl_is_id(name) && any(misfit)) {
    guess <- .nl_infer(name, values)
    code <- if (code != guess) guess else "s"
    cells <- .nl_cells(values, code)
  }
  if (code == "i" && !.nl_whole_is_double(name)) {
    ok <- cells[!is.na(cells)]
    if (!length(ok) || max(abs(ok)) <= .Machine$integer.max) cells <- as.integer(cells)
  }
  cells
}

# Rows (flat, snake_cased named lists) -> a tibble with the schema's columns first, extras after;
# a zero-row tibble with the documented types when there are no rows.
.nl_frame <- function(rows, key) {
  schema <- .NL_SCHEMAS[[key]]
  rows <- Filter(.nl_is_obj, rows)
  if (!length(rows)) {
    types <- unname(mapply(.nl_rtype, names(schema), schema))
    names(types) <- names(schema)
    return(.empty_tibble(types))
  }
  extra <- setdiff(unique(unlist(lapply(rows, names), use.names = FALSE)), names(schema))
  cols <- lapply(c(names(schema), extra), function(name) {
    values <- lapply(rows, function(r) r[[name]])
    code <- if (name %in% names(schema)) schema[[name]] else .nl_infer(name, values)
    .nl_column(name, values, code)
  })
  names(cols) <- c(names(schema), extra)
  tibble::as_tibble(cols, .name_repair = "minimal")
}

# ---- parsers (sdv-py parse_nationalleague_*) -------------------------------------

.nl_parse_games <- function(raw) .nl_frame(.nl_flat_rows(raw, lists_only = TRUE), "games")

# rounds -> series -> games: one row per game with its round and the series_* columns.
.nl_parse_bracket <- function(raw) {
  rows <- list()
  for (rnd in if (.nl_is_arr(raw)) .nl_rows(.nl_scrub(raw)) else list()) {
    for (series in .nl_arr(rnd[["games"]])) {
      if (!.nl_is_obj(series)) next
      head <- .nl_flatten(series[names(series) != "games"])
      if (length(head)) names(head) <- paste0("series_", names(head))
      head <- .nl_set(head, list(round = rnd[["round"]]))
      for (game in .nl_arr(series[["games"]])) {
        if (.nl_is_obj(game)) rows[[length(rows) + 1L]] <- .nl_set(head, .nl_flatten(game))
      }
    }
  }
  .nl_frame(rows, "bracket")
}

.nl_parse_player_stats <- function(raw) .nl_frame(.nl_flat_rows(raw, .NL_SEASON_RENAMES), "player_stats")
.nl_parse_standings <- function(raw) {
  .nl_frame(.nl_flat_rows(raw, .NL_SEASON_RENAMES, lists_only = TRUE), "standings")
}
.nl_parse_team_info <- function(raw) {
  .nl_frame(.nl_flat_rows(raw, c(id = "cms_node_id", fields_team_id = "team_id")), "team_info")
}

# The game page (/games/{gameId}).
.nl_game <- function(raw) if (.nl_is_obj(raw)) .nl_scrub(raw) else list()

# The game id and each side's team id, from `overview`.
.nl_sides <- function(game) {
  ov <- .nl_obj(game[["overview"]])
  list(game_id = ov[["gameId"]], home = ov[["homeTeamId"]], away = ov[["awayTeamId"]])
}

# {(side, sweater number)} and {(side, player id)} -> lineup player, from the lineups.
.nl_roster <- function(game) {
  out <- new.env(parent = emptyenv())
  for (side in c("home", "away")) {
    for (unit in .nl_arr(game[[paste0("lineup", .nl_title(side))]])) {
      for (p in .nl_arr(.nl_obj(unit)[["players"]])) {
        if (!.nl_is_obj(p)) next
        out[[paste(side, "number", .nl_str(p[["number"]]), sep = "|")]] <- p
        out[[paste(side, "player", .nl_str(p[["playerId"]]), sep = "|")]] <- p
      }
    }
  }
  out
}
.nl_roster_get <- function(roster, side, kind, value) {
  key <- paste(side, kind, .nl_str(value), sep = "|")
  if (exists(key, envir = roster, inherits = FALSE)) get(key, envir = roster, inherits = FALSE) else list()
}

.nl_head <- function(sides, side) {
  list(game_id = sides$game_id, team_side = side, team_id = sides[[side]])
}
.nl_side_list <- function(game, prefix, side) .nl_arr(game[[paste0(prefix, .nl_title(side))]])

.nl_game_frame <- function(raw, key, build) {
  game <- .nl_game(raw)
  .nl_frame(if (length(game)) build(game) else list(), key)
}

.nl_parse_game_overview <- function(raw) {
  .nl_game_frame(raw, "game_overview", function(game) {
    ov <- .nl_obj(game[["overview"]])
    if (!length(ov)) return(list())
    row <- .nl_flatten(ov)
    result <- .nl_flatten(.nl_obj(game[["result"]]))
    list(c(row, result[setdiff(names(result), names(row))]))
  })
}

# "822" (8:22 elapsed, MMSS without a separator) -> 502
.nl_clock_seconds <- function(x) {
  empty <- is.null(x) || identical(x, "") || (is.numeric(x) && length(x) == 1 && x == 0) || identical(x, FALSE)
  text <- if (empty) "" else trimws(.nl_str(x))
  if (!grepl("^[0-9]+$", text)) return(NA_real_)
  n <- nchar(text)
  head <- if (n > 2) as.numeric(substr(text, 1, n - 2)) else 0
  head * 60 + as.numeric(substr(text, max(1, n - 1), n))
}

.nl_parse_pbp <- function(raw) {
  .nl_game_frame(raw, "pbp", function(game) {
    sides <- .nl_sides(game)
    rows <- list()
    for (period in .nl_arr(game[["actions"]])) {
      for (event in .nl_arr(.nl_obj(period)[["actions"]])) {
        if (!.nl_is_obj(event)) next
        row <- .nl_rename(.nl_flatten(event), c(third = "period"))
        home <- event[["homeTeam"]]
        head <- list(
          game_id = sides$game_id,
          event_index = length(rows) + 1L,
          team_side = if (is.null(home)) NULL else if (isTRUE(as.logical(home))) "home" else "away",
          game_seconds = .nl_clock_seconds(event[["time"]])
        )
        rows[[length(rows) + 1L]] <- .nl_set(head, row)
      }
    }
    rows
  })
}

.nl_parse_lineups <- function(raw) {
  .nl_game_frame(raw, "lineups", function(game) {
    sides <- .nl_sides(game)
    rows <- list()
    for (side in c("home", "away")) {
      for (unit in .nl_side_list(game, "lineup", side)) {
        for (p in .nl_arr(.nl_obj(unit)[["players"]])) {
          if (!.nl_is_obj(p)) next
          head <- c(.nl_head(sides, side), list(unit = .nl_obj(unit)[["line"]]))
          rows[[length(rows) + 1L]] <- .nl_set(head, .nl_flatten(p))
        }
      }
    }
    rows
  })
}

.nl_parse_player_box <- function(raw) {
  .nl_game_frame(raw, "player_box", function(game) {
    sides <- .nl_sides(game)
    roster <- .nl_roster(game)
    rows <- list()
    for (side in c("home", "away")) {
      for (p in .nl_side_list(game, "playerStats", side)) {
        if (!.nl_is_obj(p)) next
        player <- .nl_roster_get(roster, side, "number", p[["number"]])
        head <- c(.nl_head(sides, side), list(player_id = player[["playerId"]]))
        rows[[length(rows) + 1L]] <- .nl_set(head, .nl_rename(.nl_flatten(p), .NL_GAME_BOX_RENAMES))
      }
    }
    rows
  })
}

.nl_parse_team_stats <- function(raw) {
  .nl_game_frame(raw, "team_stats", function(game) {
    sides <- .nl_sides(game)
    rows <- list()
    for (side in c("home", "away")) {
      stats <- .nl_obj(game[[paste0("teamStats", .nl_title(side))]])
      if (length(stats)) {
        rows[[length(rows) + 1L]] <- .nl_set(.nl_head(sides, side),
                                             .nl_rename(.nl_flatten(stats), .NL_GAME_BOX_RENAMES))
      }
    }
    rows
  })
}

.nl_parse_shots <- function(raw) {
  renames <- c(pid = "player_id", sec = "period", sit = "strength", type = "shot_type")
  .nl_game_frame(raw, "shots", function(game) {
    sides <- .nl_sides(game)
    roster <- .nl_roster(game)
    rows <- list()
    for (side in c("home", "away")) {
      for (shot in .nl_side_list(game, "shots", side)) {
        if (!.nl_is_obj(shot)) next
        who <- .nl_roster_get(roster, side, "player", shot[["pid"]])
        row <- .nl_rename(.nl_flatten(shot), renames)
        pid <- row[["player_id"]]
        row <- row[names(row) != "player_id"]
        head <- c(.nl_head(sides, side), list(
          player_id = pid, player_number = who[["number"]],
          player_first_name = who[["firstName"]], player_last_name = who[["lastName"]]
        ))
        rows[[length(rows) + 1L]] <- .nl_set(head, row)
      }
    }
    key <- vapply(rows, function(r) if (is.numeric(r[["time"]])) as.numeric(r[["time"]]) else -1, 1)
    rows[order(key)]
  })
}

# ---- getter: throttled, checked, api keys dropped (sdv-py nationalleague_runtime) ----

.NL_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status")

# (regex under /api, scope); the one capture group is the id the body must echo. Most specific first.
.NL_ROUTE_RX <- list(
  list("/games/(?:playoffs|playouts)$", "bracket"),
  list("/games/current$", "games"),
  list("/games/team/([^/]+)$", "team_games"),
  list("/games/([^/]+)$", "game"),
  list("/games$", "games"),
  list("/player/topscorer$", "player_rows"),
  list("/player/team/([^/]+)$", "team_players"),
  list("/player/([^/]+)$", "player"),
  list("/player$", "player_rows"),
  list("/teams/([^/]+)/info$", "team_info"),
  list("/teams$", "team_rows")
)

.nl_route <- function(url) {
  path <- sub("^[a-z]+://[^/]+", "", sub("[?#].*$", "", url))
  at <- regexpr("/api", path, fixed = TRUE)[[1]]
  if (at > 0) path <- substring(path, at + 4L)
  for (rule in .NL_ROUTE_RX) {
    m <- regmatches(path, regexec(rule[[1]], path, perl = TRUE))[[1]]
    if (length(m)) return(list(scope = rule[[2]], want = if (length(m) > 1) m[[2]] else ""))
  }
  list(scope = "", want = "")
}

# Every game in a games list or a rounds -> series -> games tree.
.nl_games_of <- function(url, body) {
  out <- list()
  for (item in body) {
    if (!.nl_is_obj(item)) {
      kind <- .nl_kind(item)
      where <- .euro_where(url)
      .abort_asset_fetch("{where}: a {kind} where a game, round or series belongs")
    }
    if (.nl_is_arr(item[["games"]])) {
      out <- c(out, .nl_games_of(url, item[["games"]]))
    } else {
      out[[length(out) + 1L]] <- item
    }
  }
  out
}

# Raise unless `body` is a real answer to `url` + `params` (sdv-py nationalleague_runtime._check).
.nl_check <- function(url, params, body) {
  where <- .euro_where(url)
  route <- .nl_route(url)
  scope <- route$scope
  want <- route$want
  if (.nl_is_obj(body) && length(body) > 0 && all(names(body) %in% .NL_ERROR_KEYS)) {
    keys <- sort(names(body))
    .abort_asset_fetch("{where} answered an error object: {.val {keys}}")
  }
  list_scopes <- c("games", "team_games", "bracket", "player_rows", "team_players", "team_rows")
  if (scope %in% list_scopes) {
    if (!.nl_is_arr(body)) {
      kind <- .nl_kind(body)
      .abort_asset_fetch("{where} answered {kind}, not an array")
    }
    if (!length(body)) .abort_no_data("{where} answered an empty array")
  } else if (nzchar(scope) && !(.nl_is_obj(body) && length(body) > 0)) {
    if (.nl_is_obj(body)) .abort_no_data("{where} answered an empty object")
    kind <- .nl_kind(body)
    .abort_asset_fetch("{where} answered {kind}, not an object")
  }

  if (scope %in% c("player_rows", "team_players", "team_rows")) {
    key <- if (scope == "team_rows") "teamId" else "playerId"
    for (row in body) {
      if (!(.nl_is_obj(row) && length(row) > 0 && .nl_is_digits(row[[key]]))) {
        excerpt <- substr(.nl_json_dumps(row), 1, 60)
        .abort_asset_fetch("{where}: a row without a digit-string {key}: {excerpt}")
      }
    }
    if (scope == "team_players" && any(vapply(body, function(r) !identical(r[["teamId"]], want), NA))) {
      .abort_asset_fetch("{where}: asked team {want}, served a player of another team")
    }
  } else if (scope %in% c("games", "team_games", "bracket")) {
    games <- .nl_games_of(url, body)
    if (!length(games)) .abort_no_data("{where}: no games yet")
    ids <- lapply(games, function(g) g[["gameId"]])
    if (!all(vapply(ids, .nl_is_digits, NA))) .abort_asset_fetch("{where}: a game without a digit-string gameId")
    season <- as.character(params[["season"]] %||% "")
    if (nzchar(season)) {
      bad <- unlist(ids)[!startsWith(unlist(ids), season)]
      if (length(bad)) .abort_asset_fetch("{where}: asked season {season}, served game {bad[[1]]}")
    }
    if (scope == "team_games") {
      has_team <- vapply(games, function(g) identical(g[["homeTeamId"]], want) || identical(g[["awayTeamId"]], want), NA)
      if (!all(has_team)) .abort_asset_fetch("{where}: asked team {want}, served a game without it")
    }
  } else if (scope %in% c("game", "player", "team_info")) {
    got <- switch(scope,
      game = .nl_obj(body[["overview"]])[["gameId"]],
      player = body[["playerId"]],
      team_info = .nl_obj(body[["fields"]])[["teamID"]]
    )
    if (!.nl_is_digits(got)) {
      shown <- .nl_json_dumps(got)
      .abort_asset_fetch("{where}: the page carries no digit-string id to check ({shown})")
    }
    if (!identical(got, want)) .abort_asset_fetch("{where}: asked {want}, served {got}")
  }
  invisible(body)
}

# GET a National League API route as JSON: throttled (every attempt), checked, api keys dropped.
.nl_get <- function(url, params = list()) {
  body <- .euro_get(url, params, simplifyVector = FALSE)
  .nl_check(url, params, body)
  .nl_scrub(body)
}

# ---- the shared core of the 17 exported wrappers -------------------------------------

# route -> path under /api ({id} is the path id), the parser, the fastRhockey_data label
.NL_ROUTES <- list(
  schedule = list("/games", "games", "Schedule"),
  current_games = list("/games/current", "games", "Current Games"),
  team_schedule = list("/games/team/{id}", "games", "Team Schedule"),
  playoffs = list("/games/playoffs", "bracket", "Playoffs"),
  playouts = list("/games/playouts", "bracket", "Playouts"),
  game_overview = list("/games/{id}", "game_overview", "Game Overview"),
  pbp = list("/games/{id}", "pbp", "Play-by-Play"),
  lineups = list("/games/{id}", "lineups", "Lineups"),
  player_box = list("/games/{id}", "player_box", "Player Box Score"),
  team_stats = list("/games/{id}", "team_stats", "Team Stats"),
  shots = list("/games/{id}", "shots", "Shots"),
  player_stats = list("/player", "player_stats", "Player Stats"),
  top_scorers = list("/player/topscorer", "player_stats", "Top Scorers"),
  team_player_stats = list("/player/team/{id}", "player_stats", "Team Player Stats"),
  player_season = list("/player/{id}", "player_stats", "Player Season"),
  standings = list("/teams", "standings", "Standings"),
  team_info = list("/teams/{id}/info", "team_info", "Team Info")
)

# A path id or query value as a digit string: 101144, "101144" or 2.0261105e13 -> "..." ; else a bad request.
.nl_digits_arg <- function(x, what) {
  ok <- length(x) == 1 && !is.na(x) &&
    ((is.numeric(x) && is.finite(x) && x >= 0 && x == floor(x)) || .nl_is_digits(as.character(x)))
  if (!ok) {
    .abort_bad_request("{.arg {what}} must be a single whole number or digit string, not {.val {x}}.")
  }
  if (is.numeric(x)) sprintf("%.0f", x) else x
}

.nl_run <- function(route, id = NULL, query = list(), return_parsed = TRUE) {
  old <- options(scipen = 999, stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  spec <- .NL_ROUTES[[route]]
  path <- spec[[1]]
  if (!is.null(id)) {
    path <- sub("{id}", .nl_digits_arg(id[[1]], names(id)), path, fixed = TRUE)
  }
  query <- Filter(Negate(is.null), query)
  query <- if (length(query)) Map(.nl_digits_arg, query, names(query)) else list()
  raw <- .nl_get(paste0(.NL_HOST, path), query)
  if (!isTRUE(return_parsed)) return(raw)
  df <- get(paste0(".nl_parse_", spec[[2]]), mode = "function")(raw)
  make_fastRhockey_data(df, paste0("National League ", spec[[3]], " Information from nationalleague.ch"), Sys.time())
}

# ---- exported wrappers (sdv-py nl_*, one per route) ---------------------------------

#' **National League schedule and results**
#'
#' Schedule and results for one season: NL plus exhibitions.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games`.
#'
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (29 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | home_team_short_name | character | Home team abbreviation (e.g. HCD) |
#' | home_team_name | character | Home team display name |
#' | away_team_short_name | character | Away team abbreviation |
#' | away_team_name | character | Away team display name |
#' | home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | date | character | Scheduled start, ISO 8601 with UTC offset |
#' | status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
#' | base_status | character | Coarse game state; equal to status in every sampled game |
#' | home_team_result | integer | Home team goals (final or current score; 0 before the game) |
#' | away_team_result | integer | Away team goals (final or current score; 0 before the game) |
#' | is_overtime | logical | Game went to overtime |
#' | is_shootout | logical | Game went to a shootout |
#' | highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | has_highlight | logical | A highlight video exists |
#' | is_current | logical | Game belongs to the current game day |
#' | is_current_for_team | logical | Game is the team's current game (team-scoped views) |
#' | tv_partner | character | Broadcaster logo image URLs; unobserved: null, absent or empty in every sampled row |
#' | spectators | integer | Attendance (whole number; the API sends a digit string). |
#' | has_my_sports | logical | Broadcast on MySports |
#' | referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games) |
#' | is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
#' | home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
#' | show_game_time | logical | App flag: show gameTime |
#' | arena | character | Name of the arena the game is played in |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_schedule(season = 2026))
#' }
nl_schedule <- function(season = NULL, return_parsed = TRUE) {
  .nl_run("schedule", query = list(season = season), return_parsed = return_parsed)
}

#' **National League current or next game day**
#'
#' The current or next game day.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/current`.
#'
#' @param season The running season's END year (2027 on 2026-10-12). Season END year (`2026` is 2025/26), sent to the API
#'   unchanged. `NULL` (the default) leaves the choice to the API.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (29 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | home_team_short_name | character | Home team abbreviation (e.g. HCD) |
#' | home_team_name | character | Home team display name |
#' | away_team_short_name | character | Away team abbreviation |
#' | away_team_name | character | Away team display name |
#' | home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | date | character | Scheduled start, ISO 8601 with UTC offset |
#' | status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
#' | base_status | character | Coarse game state; equal to status in every sampled game |
#' | home_team_result | integer | Home team goals (final or current score; 0 before the game) |
#' | away_team_result | integer | Away team goals (final or current score; 0 before the game) |
#' | is_overtime | logical | Game went to overtime |
#' | is_shootout | logical | Game went to a shootout |
#' | highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | has_highlight | logical | A highlight video exists |
#' | is_current | logical | Game belongs to the current game day |
#' | is_current_for_team | logical | Game is the team's current game (team-scoped views) |
#' | tv_partner | character | Broadcaster logo image URLs |
#' | spectators | integer | Attendance (whole number; the API sends a digit string). |
#' | has_my_sports | logical | Broadcast on MySports |
#' | referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games); unobserved: null, absent or empty in every sampled row |
#' | is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
#' | home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
#' | show_game_time | logical | App flag: show gameTime |
#' | arena | character | Name of the arena the game is played in |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_current_games(season = 2027))
#' }
nl_current_games <- function(season = NULL, return_parsed = TRUE) {
  .nl_run("current_games", query = list(season = season), return_parsed = return_parsed)
}

#' **National League team schedule**
#'
#' One team's games for one season.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/team/{teamId}`.
#'
#' @param team_id SIHF team id (`team_id` in [nl_standings()]; the same id as the SIHF family's `team_id`), e.g. `101144` (EV
#'   Zug). A whole number or a digit string.
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (29 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | home_team_short_name | character | Home team abbreviation (e.g. HCD) |
#' | home_team_name | character | Home team display name |
#' | away_team_short_name | character | Away team abbreviation |
#' | away_team_name | character | Away team display name |
#' | home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | date | character | Scheduled start, ISO 8601 with UTC offset |
#' | status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
#' | base_status | character | Coarse game state; equal to status in every sampled game |
#' | home_team_result | integer | Home team goals (final or current score; 0 before the game) |
#' | away_team_result | integer | Away team goals (final or current score; 0 before the game) |
#' | is_overtime | logical | Game went to overtime |
#' | is_shootout | logical | Game went to a shootout |
#' | highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | has_highlight | logical | A highlight video exists |
#' | is_current | logical | Game belongs to the current game day |
#' | is_current_for_team | logical | Game is the team's current game (team-scoped views) |
#' | tv_partner | character | Broadcaster logo image URLs; unobserved: null, absent or empty in every sampled row |
#' | spectators | integer | Attendance (whole number; the API sends a digit string). |
#' | has_my_sports | logical | Broadcast on MySports |
#' | referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games) |
#' | is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
#' | home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
#' | show_game_time | logical | App flag: show gameTime |
#' | arena | character | Name of the arena the game is played in |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_team_schedule(season = 2026, team_id = 101144))
#' }
nl_team_schedule <- function(team_id, season = NULL, return_parsed = TRUE) {
  .nl_run("team_schedule", id = list(team_id = team_id), query = list(season = season), return_parsed = return_parsed)
}

#' **National League playoff series**
#'
#' Playoff and play-in series by round: one row per game, with its series score.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/playoffs`.
#'
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (39 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | series_group | integer | Series number within the round (1 = first pairing; 0 seen in playouts) |
#' | series_home_team_short_name | character | Abbreviation of the team with home ice |
#' | series_away_team_short_name | character | Abbreviation of the other team |
#' | series_home_team_id | numeric | Team with home ice in the series. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | series_away_team_id | numeric | The other team in the series. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | series_home_team_result | integer | Series wins of the team with home ice |
#' | series_away_team_result | integer | Series wins of the other team |
#' | series_best_of | integer | Series length in games (7; 2 in the play-in) |
#' | series_title | character | Series length label, e.g. 'Best of 7' |
#' | round | character | Round key: final, semifinal, quarterfinal, playIn (/playoffs); leagueQualifications, playoutFinal (/playouts) |
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | home_team_short_name | character | Home team abbreviation (e.g. HCD) |
#' | home_team_name | character | Home team display name |
#' | away_team_short_name | character | Away team abbreviation |
#' | away_team_name | character | Away team display name |
#' | home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | date | character | Scheduled start, ISO 8601 with UTC offset |
#' | status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
#' | base_status | character | Coarse game state; equal to status in every sampled game |
#' | home_team_result | integer | Home team goals (final or current score; 0 before the game) |
#' | away_team_result | integer | Away team goals (final or current score; 0 before the game) |
#' | is_overtime | logical | Game went to overtime |
#' | is_shootout | logical | Game went to a shootout |
#' | highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | has_highlight | logical | A highlight video exists |
#' | is_current | logical | Game belongs to the current game day |
#' | is_current_for_team | logical | Game is the team's current game (team-scoped views) |
#' | tv_partner | character | Broadcaster logo image URLs |
#' | spectators | integer | Attendance (whole number; the API sends a digit string). |
#' | has_my_sports | logical | Broadcast on MySports |
#' | referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games) |
#' | is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
#' | home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
#' | show_game_time | logical | App flag: show gameTime |
#' | arena | character | Name of the arena the game is played in |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_playoffs(season = 2026))
#' }
nl_playoffs <- function(season = NULL, return_parsed = TRUE) {
  .nl_run("playoffs", query = list(season = season), return_parsed = return_parsed)
}

#' **National League playout and qualification series**
#'
#' Playout and league-qualification series by round: one row per game, with its series score.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/playouts`.
#'
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (39 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | series_group | integer | Series number within the round (1 = first pairing; 0 seen in playouts) |
#' | series_home_team_short_name | character | Abbreviation of the team with home ice |
#' | series_away_team_short_name | character | Abbreviation of the other team |
#' | series_home_team_id | numeric | Team with home ice in the series. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | series_away_team_id | numeric | The other team in the series. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | series_home_team_result | integer | Series wins of the team with home ice |
#' | series_away_team_result | integer | Series wins of the other team |
#' | series_best_of | integer | Series length in games (7; 2 in the play-in) |
#' | series_title | character | Series length label, e.g. 'Best of 7' |
#' | round | character | Round key: final, semifinal, quarterfinal, playIn (/playoffs); leagueQualifications, playoutFinal (/playouts) |
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | home_team_short_name | character | Home team abbreviation (e.g. HCD) |
#' | home_team_name | character | Home team display name |
#' | away_team_short_name | character | Away team abbreviation |
#' | away_team_name | character | Away team display name |
#' | home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | date | character | Scheduled start, ISO 8601 with UTC offset |
#' | status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
#' | base_status | character | Coarse game state; equal to status in every sampled game |
#' | home_team_result | integer | Home team goals (final or current score; 0 before the game) |
#' | away_team_result | integer | Away team goals (final or current score; 0 before the game) |
#' | is_overtime | logical | Game went to overtime |
#' | is_shootout | logical | Game went to a shootout |
#' | highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | has_highlight | logical | A highlight video exists |
#' | is_current | logical | Game belongs to the current game day |
#' | is_current_for_team | logical | Game is the team's current game (team-scoped views) |
#' | tv_partner | character | Broadcaster logo image URLs |
#' | spectators | integer | Attendance (whole number; the API sends a digit string). |
#' | has_my_sports | logical | Broadcast on MySports |
#' | referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games) |
#' | is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
#' | home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
#' | game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
#' | show_game_time | logical | App flag: show gameTime |
#' | arena | character | Name of the arena the game is played in |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_playouts(season = 2026))
#' }
nl_playouts <- function(season = NULL, return_parsed = TRUE) {
  .nl_run("playouts", query = list(season = season), return_parsed = return_parsed)
}

#' **National League game header**
#'
#' One game's header: teams, date, status, final and per-period score, coaches, referees, arena, attendance.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.
#'
#' The six game-page wrappers ([nl_game_overview()], [nl_pbp()], [nl_lineups()], [nl_player_box()],
#' [nl_team_stats()], [nl_shots()]) each read the same `/games/{gameId}` body, one request each.
#'
#' @param game_id SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>` (`game_id` in [nl_schedule()]; the same
#'   id as the SIHF family's `game_id`), e.g. `20261105000325`. A whole number or a digit string; 14 digits are
#'   exact as a double, so do not format them through `integer`.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (47 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | home_team_short_name | character | Home team abbreviation (e.g. HCD) |
#' | home_team_name | character | Home team display name |
#' | away_team_short_name | character | Away team abbreviation |
#' | away_team_name | character | Away team display name |
#' | home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | date | character | Scheduled start, ISO 8601 with UTC offset |
#' | status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
#' | base_status | character | Coarse game state; equal to status in every sampled game |
#' | home_team_result | integer | Home team goals (final or current score; 0 before the game) |
#' | away_team_result | integer | Away team goals (final or current score; 0 before the game) |
#' | is_overtime | logical | Game went to overtime |
#' | is_shootout | logical | Game went to a shootout |
#' | highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | has_highlight | logical | A highlight video exists |
#' | is_current | logical | Game belongs to the current game day |
#' | is_current_for_team | logical | Game is the team's current game (team-scoped views) |
#' | tv_partner | character | Broadcaster logo image URLs |
#' | spectators | integer | Attendance (whole number; the API sends a digit string). |
#' | has_my_sports | logical | Broadcast on MySports |
#' | referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games) |
#' | is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
#' | home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists |
#' | away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists |
#' | game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
#' | show_game_time | logical | App flag: show gameTime |
#' | arena | character | Name of the arena the game is played in |
#' | home_team_first_result | integer | Home goals in the 1st period |
#' | away_team_first_result | integer | Away goals in the 1st period |
#' | home_team_second_result | integer | Home goals in the 2nd period |
#' | away_team_second_result | integer | Away goals in the 2nd period |
#' | home_team_third_result | integer | Home goals in the 3rd period |
#' | away_team_third_result | integer | Away goals in the 3rd period |
#' | home_team_overtime_result | integer | Home overtime goals, all overtime periods |
#' | away_team_overtime_result | integer | Away overtime goals, all overtime periods |
#' | home_team_overtime1_result | integer | Home goals in overtime period 1 (playoff games can have several) |
#' | away_team_overtime1_result | integer | Away goals in overtime period 1 |
#' | home_team_overtime2_result | integer | Home goals in overtime period 2 |
#' | away_team_overtime2_result | integer | Away goals in overtime period 2 |
#' | home_team_overtime3_result | integer | Home goals in overtime period 3 |
#' | away_team_overtime3_result | integer | Away goals in overtime period 3 |
#' | home_team_shootout_result | integer | Home shootout goals |
#' | away_team_shootout_result | integer | Away shootout goals |
#' | home_aggregated_score | character | Home aggregate over a two-legged tie; unobserved: null or empty in every sampled row |
#' | away_aggregated_score | character | Away aggregate over a two-legged tie; unobserved: null or empty in every sampled row |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_game_overview(game_id = 20261105000325))
#' }
nl_game_overview <- function(game_id, return_parsed = TRUE) {
  .nl_run("game_overview", id = list(game_id = game_id), return_parsed = return_parsed)
}

#' **National League play-by-play**
#'
#' One game's event timeline: goals with assists, penalties, goaltender changes, revoked goals.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.
#'
#' The six game-page wrappers ([nl_game_overview()], [nl_pbp()], [nl_lineups()], [nl_player_box()],
#' [nl_team_stats()], [nl_shots()]) each read the same `/games/{gameId}` body, one request each.
#'
#' @param game_id SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>` (`game_id` in [nl_schedule()]; the same
#'   id as the SIHF family's `game_id`), e.g. `20261105000325`. A whole number or a digit string; 14 digits are
#'   exact as a double, so do not format them through `integer`.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (34 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | event_index | integer | 1-based position of the event in the game's timeline. |
#' | team_side | character | home or away: the side of the team this row belongs to. |
#' | game_seconds | integer | Elapsed game time of the event in seconds, from the MMSS `time` (822 = 8:22 -> 502). |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | home_team | logical | Event belongs to the home team |
#' | time | character | Game clock elapsed as an MMSS string without separator ('822' = 8:22, '5954' = 59:54) |
#' | video_time | integer | Seconds into the game video; 0 when there is no clip |
#' | video_vimeo_id | numeric | Clip of this event. Vimeo video id (whole number): vimeo.com/`<id>`. |
#' | action | character | Event type: goal, foul (penalty), in / out (goaltender enters / leaves), revokedGoal |
#' | foul_type | integer | SIHF penalty code (116-125 in the sample); 0 on non-penalty events |
#' | foul_minutes | integer | Penalty minutes; 0 on non-penalty events |
#' | player_first_name | character | Shooter's first name, from the game's lineup. |
#' | player_last_name | character | Shooter's last name, from the game's lineup. |
#' | player_number | character | Shooter's sweater number, from the game's lineup (a string). |
#' | player_total | integer | Scorer's goals this season including this one; 0 on non-goal events |
#' | period | integer | Period number, an integer (the parent `third` is a string) |
#' | overtime | integer | Overtime period number; 0 in regulation |
#' | goal_situation | character | Goal kind; 'normal' on every sampled event |
#' | home_team_result | integer | Home score after the event |
#' | away_team_result | integer | Away score after the event |
#' | assist1_first_name | character | First assist: first name |
#' | assist1_last_name | character | First assist: last name |
#' | assist1_number | character | First assist: sweater number, a string |
#' | assist1_total | integer | First assister's assists this season including this one |
#' | assist2_first_name | character | Second assist: first name |
#' | assist2_last_name | character | Second assist: last name |
#' | assist2_number | character | Second assist: sweater number, a string |
#' | assist2_total | integer | Second assister's assists this season including this one |
#' | situation | character | Goal tags, comma-separated: EQ (even strength), PP1 (power play), GWG (game-winning goal); null on non-goals |
#' | revoked_goal_type | integer | Revoked-goal category code (1 on every sampled event) |
#' | revoked_goal_sub_type | integer | Revoked-goal reason code (1 on the revoked goal, 0 elsewhere) |
#' | review_successful | character | Video-review outcome; unobserved: null or empty in every sampled row |
#' | review_goal_counts | logical | Goal stood after review |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_pbp(game_id = 20261105000325))
#' }
nl_pbp <- function(game_id, return_parsed = TRUE) {
  .nl_run("pbp", id = list(game_id = game_id), return_parsed = return_parsed)
}

#' **National League game lineups**
#'
#' One game's lineups: every dressed player by unit, with licence number, position and line.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.
#'
#' The six game-page wrappers ([nl_game_overview()], [nl_pbp()], [nl_lineups()], [nl_player_box()],
#' [nl_team_stats()], [nl_shots()]) each read the same `/games/{gameId}` body, one request each.
#'
#' @param game_id SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>` (`game_id` in [nl_schedule()]; the same
#'   id as the SIHF family's `game_id`), e.g. `20261105000325`. A whole number or a digit string; 14 digits are
#'   exact as a double, so do not format them through `integer`.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (13 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | team_side | character | home or away: the side of the team this row belongs to. |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | unit | character | Lineup group: goalkeeper, or the line number 1-4 as a string. |
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | position | character | goalkeeper, defender or forwarder (sic) |
#' | line | integer | Line number, an integer (goalkeepers 1-2 = first / backup) |
#' | first_name | character | First (given) name of the player |
#' | last_name | character | Last (family) name of the player |
#' | number | character | Sweater number, a string |
#' | is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey |
#' | is_captain | logical | Whether the player is the team captain |
#' | first_goalkeeper | logical | Starting goaltender |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_lineups(game_id = 20261105000325))
#' }
nl_lineups <- function(game_id, return_parsed = TRUE) {
  .nl_run("lineups", id = list(game_id = game_id), return_parsed = return_parsed)
}

#' **National League player box score**
#'
#' One game's player box score: goals, assists, shots, faceoffs, ice time, goaltending.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.
#'
#' The six game-page wrappers ([nl_game_overview()], [nl_pbp()], [nl_lineups()], [nl_player_box()],
#' [nl_team_stats()], [nl_shots()]) each read the same `/games/{gameId}` body, one request each.
#'
#' @param game_id SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>` (`game_id` in [nl_schedule()]; the same
#'   id as the SIHF family's `game_id`), e.g. `20261105000325`. A whole number or a digit string; 14 digits are
#'   exact as a double, so do not format them through `integer`.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (38 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | team_side | character | home or away: the side of the team this row belongs to. |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | number | character | Sweater number, a string |
#' | first_name | character | First (given) name of the player |
#' | last_name | character | Last (family) name of the player |
#' | short_name | character | Initial and last name, e.g. 'S. Aeschlimann' |
#' | name | character | First and last name |
#' | position | character | goalkeeper, defender or forwarder (sic) |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | sog | integer | Number of shots on goal |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | toi | integer | Time on ice, seconds |
#' | toi_eq | integer | Even-strength time on ice, seconds |
#' | toi_pp | integer | Power-play time on ice, seconds |
#' | toi_pk | integer | Shorthanded time on ice, seconds |
#' | pim | integer | Penalty minutes |
#' | g | integer | Number of goals scored |
#' | a1 | integer | First (primary) assists credited |
#' | a2 | integer | Second (secondary) assists credited |
#' | pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
#' | mi | integer | Minus count: on ice for a goal against |
#' | mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
#' | ga | integer | Goals against: number of goals conceded |
#' | svs | integer | Number of saves made by the goaltender |
#' | sa | integer | Shots on goal against |
#' | gaa | numeric | Goals-against average; 0 in every sampled row, even for goaltenders who conceded |
#' | save_percentage | numeric | Save percentage, 0-100 |
#' | faceoffs | integer | Faceoffs taken (upstream `fo`: fow + fol). Not penalties. |
#' | faceoffs_percentage | numeric | Faceoff win percentage (upstream `foPercentage`); -1 without faceoffs. |
#' | a | integer | Assists (a1 + a2) |
#' | pts | integer | Points (goals + assists) |
#' | pl_mi | integer | Plus/minus (pl - mi) |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_player_box(game_id = 20261105000325))
#' }
nl_player_box <- function(game_id, return_parsed = TRUE) {
  .nl_run("player_box", id = list(game_id = game_id), return_parsed = return_parsed)
}

#' **National League team game totals**
#'
#' One game's team totals: shots by period, faceoffs by zone, power play and penalty kill.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.
#'
#' The six game-page wrappers ([nl_game_overview()], [nl_pbp()], [nl_lineups()], [nl_player_box()],
#' [nl_team_stats()], [nl_shots()]) each read the same `/games/{gameId}` body, one request each.
#'
#' @param game_id SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>` (`game_id` in [nl_schedule()]; the same
#'   id as the SIHF family's `game_id`), e.g. `20261105000325`. A whole number or a digit string; 14 digits are
#'   exact as a double, so do not format them through `integer`.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (40 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | team_side | character | home or away: the side of the team this row belongs to. |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | g | integer | Number of goals scored |
#' | ga | integer | Goals against: number of goals conceded |
#' | sa | integer | Shots on goal against |
#' | sog1 | integer | Shots on goal in the 1st period |
#' | sog2 | integer | Shots on goal in the 2nd period |
#' | sog3 | integer | Shots on goal in the 3rd period |
#' | sog_ot | integer | Shots on goal in overtime, all periods |
#' | sog_ot1 | integer | Shots on goal in overtime period 1 |
#' | sog_ot2 | integer | Shots on goal in overtime period 2 |
#' | sog_ot3 | integer | Shots on goal in overtime period 3 |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | fow_d | integer | Defensive-zone faceoffs won (0 in every sampled game) |
#' | fow_n | integer | Neutral-zone faceoffs won (0 in every sampled game) |
#' | fow_o | integer | Offensive-zone faceoffs won (0 in every sampled game) |
#' | fol_n | integer | Neutral-zone faceoffs lost (0 in every sampled game) |
#' | fol_o | integer | Offensive-zone faceoffs lost (0 in every sampled game) |
#' | fol_d | integer | Defensive-zone faceoffs lost (0 in every sampled game) |
#' | ppt | integer | Power-play time, seconds |
#' | ppo | integer | Power-play opportunities |
#' | ppg | integer | Power-play goals |
#' | ppga | integer | Shorthanded goals allowed while on the power play |
#' | pkt | integer | Shorthanded time, seconds |
#' | pks | integer | Times shorthanded |
#' | pkg | integer | Shorthanded goals scored |
#' | pkga | integer | Power-play goals allowed while shorthanded |
#' | faceoffs | integer | Faceoffs taken (upstream `fo`: fow + fol). Not penalties. |
#' | pim | integer | Penalty minutes |
#' | sog | integer | Number of shots on goal |
#' | sog_percentage | numeric | Shooting percentage, 100 * g / sog |
#' | pp_percentage | numeric | Power-play percentage, 100 * ppg / ppo |
#' | pk_percentage | numeric | Penalty-kill percentage, 0-100 |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | faceoffs_percentage | numeric | Faceoff win percentage (upstream `foPercentage`); -1 without faceoffs. |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_team_stats(game_id = 20261105000325))
#' }
nl_team_stats <- function(game_id, return_parsed = TRUE) {
  .nl_run("team_stats", id = list(game_id = game_id), return_parsed = return_parsed)
}

#' **National League shots with location and xG**
#'
#' One game's shot attempts with rink location (metres) and expected goals (xG); NL games only.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.
#'
#' The six game-page wrappers ([nl_game_overview()], [nl_pbp()], [nl_lineups()], [nl_player_box()],
#' [nl_team_stats()], [nl_shots()]) each read the same `/games/{gameId}` body, one request each.
#'
#' @param game_id SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>` (`game_id` in [nl_schedule()]; the same
#'   id as the SIHF family's `game_id`), e.g. `20261105000325`. A whole number or a digit string; 14 digits are
#'   exact as a double, so do not format them through `integer`.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (20 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
#' | team_side | character | home or away: the side of the team this row belongs to. |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | player_number | character | Shooter's sweater number, from the game's lineup (a string). |
#' | player_first_name | character | Shooter's first name, from the game's lineup. |
#' | player_last_name | character | Shooter's last name, from the game's lineup. |
#' | period | integer | Period number (1-3; overtime follows) |
#' | time | integer | Game clock elapsed in seconds (period 2 starts at 1200) |
#' | strength | character | Strength: EQ (even), PP (power play), PK (shorthanded) |
#' | shot_type | character | Outcome: SOG (saved shot on goal), MISS, POST, BLOCK, GOAL |
#' | pos_x | numeric | Across-ice position in metres, 0-30 (rink width). Both axes run in metres on a 30 x 60 m rink. |
#' | pos_y | numeric | Distance in metres from the end boards of the attacked end, 0-60 along the rink length. Shots are mirrored to one attacking end (inferred); above 30 is a shot from beyond centre; the goal line is 4 m out. |
#' | pos_x_percentage | numeric | posX / 30 (verified on 475 shots) |
#' | pos_y_percentage | numeric | pos_y / 24, the depth of the app's zone drawing; above 1 for long shots. |
#' | video_time | integer | Seconds into the game video (= unix - unixStart; verified on 475 shots) |
#' | xg | numeric | Expected goals of this shot, 0-1. Non-zero from the 2023 season (one sampled game per season; 0 in the 2021 and 2022 games); always 0 on a BLOCK. |
#' | xg_sum | numeric | Running sum of this team's xg up to and including this shot |
#' | unix | numeric | Wall-clock time of the shot, Unix epoch seconds |
#' | unix_start | numeric | Wall-clock start of the game video, Unix epoch seconds |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_shots(game_id = 20261105000325))
#' }
nl_shots <- function(game_id, return_parsed = TRUE) {
  .nl_run("shots", id = list(game_id = game_id), return_parsed = return_parsed)
}

#' **National League player season stats**
#'
#' Season stats for every player, with playerId.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/player`.
#'
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param phase SIHF phase id (`4940` is the 2025/26 Regular Season, `5297` the playoffs); phase ids change every season.
#'   `NULL` (the default) leaves the choice to the API: on `/player` that served the regular season (verified,
#'   byte-identical to `phase = 4940`); on the other routes that default is assumed, not verified.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (145 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | first_name | character | First (given) name of the player |
#' | last_name | character | Last (family) name of the player |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | team_short_name | character | Team abbreviation |
#' | team_name | character | Team name as the league app displays it |
#' | number | character | Sweater number, a string |
#' | position | character | goalkeeper, defender or forwarder (sic); one of 502 rows in the full 2026 body has the integer 0 |
#' | topscorer_points | integer | PostFinance Top Scorer game score (an app fantasy rating, not on-ice points) |
#' | topscorer_point_average | numeric | PostFinance Top Scorer game score per game |
#' | topscorer_market_value | integer | PostFinance Top Scorer game market value |
#' | topscorer_market_value_trend | integer | Market-value trend: 1 up, 0 flat, -1 down |
#' | topscorer_amount | integer | CHF amount credited to the team's Top Scorer jersey holder (non-zero for 14 players, one per team) |
#' | is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey (one per team) |
#' | nationality | character | Nationality; unobserved: null or empty in every sampled row |
#' | birth | character | Date of birth, YYYY-MM-DD |
#' | height | integer | Height; unobserved: null or empty in every sampled row |
#' | weight | integer | Weight; unobserved: null or empty in every sampled row |
#' | hand | character | Shooting or catching hand; unobserved: null or empty in every sampled row |
#' | lcrid | integer | Not determined (small integer, 0-4 in 2026) |
#' | noc | integer | Not determined (small integer, 0-2 in 2026) |
#' | gp | integer | Number of games played |
#' | g | integer | Number of goals scored |
#' | a1 | integer | First (primary) assists credited |
#' | a2 | integer | Second (secondary) assists credited |
#' | penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
#' | pim | integer | Penalty minutes |
#' | geq | integer | Even-strength goals |
#' | gpp | integer | Power-play goals |
#' | gpk | integer | Shorthanded goals |
#' | gwg | integer | Game-winning goals |
#' | ps | integer | Shootout shots taken |
#' | pg | integer | Not determined; 0 on every 2026 row |
#' | pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
#' | mi | integer | Minus count: on ice for a goal against |
#' | toi | integer | Time on ice, seconds |
#' | toi_eq | integer | Even-strength time on ice, seconds |
#' | toipp | integer | Power-play time on ice, seconds (lower-case pp, unlike the box score's toiPp) |
#' | toi_pk | integer | Shorthanded time on ice, seconds |
#' | sog | integer | Number of shots on goal |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | shi | integer | Number of shifts played |
#' | gwgk | integer | Not determined; 0 on every 2026 row |
#' | gw | integer | Goaltender wins (incl. overtime and shootout) |
#' | gl | integer | Goaltender losses (incl. overtime and shootout) |
#' | fk | integer | Games started in goal (inferred: fk <= gp; 42 of 44 for the 2026 games-played leader) |
#' | ga | integer | Goals against: number of goals conceded |
#' | sa | integer | Shots on goal against |
#' | svs | integer | Number of saves made by the goaltender |
#' | so | integer | Number of shutouts (games without a goal conceded) |
#' | mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
#' | assists | integer | Assists (a1 + a2; verified on every 2026 row) |
#' | points | integer | Points (g + assists; verified) |
#' | goal_efficiency | numeric | Shooting percentage, 100 * g / sog (verified); -1 with no shots |
#' | save_percentage | numeric | Save percentage, 0-100 |
#' | face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
#' | face_offs | integer | Faceoffs taken (fow + fol; verified) |
#' | is_captain | logical | Whether the player is the team captain |
#' | first_goalkeeper | logical | Team's first-choice goaltender |
#' | ga_per_game | numeric | Goals against per game (ga / gp; verified) |
#' | points_per_game | numeric | Points per game |
#' | svs_per_game | numeric | Goaltender saves per game played |
#' | bks_per_game | numeric | Blocked shots per game |
#' | sog_per_game | numeric | Shots on goal per game |
#' | shm_per_game | numeric | Missed shots per game |
#' | shp_per_game | numeric | Post hits per game |
#' | fow_per_game | numeric | Faceoffs won per game |
#' | fol_per_game | numeric | Faceoffs lost per game |
#' | toi_eq_per_game | numeric | Even-strength time on ice per game, seconds |
#' | toi_pp_per_game | numeric | Power-play time on ice per game, seconds |
#' | toi_pk_per_game | numeric | Shorthanded time on ice per game, seconds |
#' | face_offs_per_game_percentage | numeric | Not determined; 0 on the sampled rows |
#' | toi_average | numeric | Time on ice per game, seconds |
#' | plus_minus | integer | Plus/minus (pl - mi; verified) |
#' | tgs | integer | Team's total game time, seconds (about 3600 per game incl. overtime) |
#' | sep | integer | Goaltender time played, seconds |
#' | toi_percentage | numeric | Share of the team's game time in goal, 100 * sep / tgs (verified) |
#' | gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
#' | glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
#' | sag | integer | Shots on goal against that were saved (= svs on every 2026 goaltender row) |
#' | sago | integer | Shots on goal against that went in (= ga on every 2026 goaltender row) |
#' | sam | integer | Shots against that missed the net |
#' | sap | integer | Shots against that hit the post |
#' | sab | integer | Shots against that were blocked |
#' | saslot | integer | Slot shots against |
#' | sagoslot | integer | Slot shots against that went in |
#' | aeq | integer | Even-strength assists |
#' | app | integer | Power-play assists |
#' | apk | integer | Shorthanded assists |
#' | ha | integer | Hat tricks (inferred; at most 2 in 2026) |
#' | fo2 | integer | Minor (2-minute) penalties |
#' | fo5 | integer | Major (5-minute) penalties |
#' | fo10 | integer | 10-minute misconducts |
#' | gm | integer | Game misconducts |
#' | mp | integer | Match penalties |
#' | pm | integer | Not determined; 0 on every 2026 row |
#' | pwg | integer | Not determined; 0 on every 2026 row |
#' | sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
#' | fowoz | integer | Offensive-zone faceoffs won |
#' | fownz | integer | Neutral-zone faceoffs won |
#' | fowdz | integer | Defensive-zone faceoffs won |
#' | foloz | integer | Offensive-zone faceoffs lost |
#' | folnz | integer | Neutral-zone faceoffs lost |
#' | foldz | integer | Defensive-zone faceoffs lost |
#' | psg | integer | Goals scored in shootouts |
#' | pssvs | integer | Saves made in shootouts |
#' | got | integer | Overtime goals (a subset of g) |
#' | ot_rank | integer | Overtime-scoring rank; 0 on the 2026 rows (filled in 2027) |
#' | ot_g | integer | Overtime goals in the overtime table; 0 on every 2026 row |
#' | ot_a1 | integer | Overtime first assists; 0 on every 2026 row |
#' | ot_a2 | integer | Overtime second assists; 0 on every 2026 row |
#' | ot_toi | integer | Overtime time on ice, seconds; 0 on every 2026 row |
#' | ot_a | integer | Overtime assists; 0 on every 2026 row |
#' | ot_p | integer | Overtime points; 0 on every 2026 row |
#' | psgwg | integer | Game-deciding shootout goals; 0 on every 2026 row |
#' | sogslot | integer | Shots on goal from the slot |
#' | pswg | integer | Shootout winners (inferred) |
#' | toishi | numeric | Time on ice per shift, seconds |
#' | shigp | numeric | Shifts per game |
#' | saslotgp | numeric | Slot shots against per game |
#' | samgp | numeric | Missed shots against per game |
#' | sapgp | numeric | Post hits against per game |
#' | svs_pslot | numeric | Slot save percentage, 100 * (saslot - sagoslot) / saslot |
#' | sa_per_game | numeric | Shots on goal against per game |
#' | otw | integer | Goaltender overtime or shootout wins |
#' | otl | integer | Goaltender overtime or shootout losses |
#' | fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
#' | fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
#' | fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
#' | fot | integer | Faceoffs taken (fow + fol) |
#' | fogp | numeric | Faceoffs taken per game |
#' | psm | integer | Shootout misses (inferred; ps = psg + psm fails for 67 of 447 skaters) |
#' | ps_percentage | numeric | Shootout success percentage, 0-100 |
#' | gw_shutout | integer | Not determined; equals gw on goaltender rows |
#' | sogslot_per_game | numeric | Slot shots on goal per game |
#' | psag | integer | Shootout goals against (equals ps on goaltender rows) |
#' | pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
#' | show_rank | logical | App flag: show the row's rank |
#' | chf | integer | CHF credited for the player's points: 300 * points on every 2026 row (PostFinance youth-fund scheme; inferred) |
#' | team_stats | character | Per-team split for a player who changed teams; unobserved: null or empty in every sampled row |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_player_stats(phase = 4940, season = 2026))
#' }
nl_player_stats <- function(season = NULL, phase = NULL, return_parsed = TRUE) {
  .nl_run("player_stats", query = list(season = season, phase = phase), return_parsed = return_parsed)
}

#' **National League top scorers**
#'
#' Top scorers (same row shape as /player).
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/player/topscorer`.
#'
#' @param take Number of rows to return (`take = 3` answers 3). `NULL` (the default) returns the full list.
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param phase SIHF phase id (`4940` is the 2025/26 Regular Season, `5297` the playoffs); phase ids change every season.
#'   `NULL` (the default) leaves the choice to the API: on `/player` that served the regular season (verified,
#'   byte-identical to `phase = 4940`); on the other routes that default is assumed, not verified.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (145 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | first_name | character | First (given) name of the player |
#' | last_name | character | Last (family) name of the player |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | team_short_name | character | Team abbreviation |
#' | team_name | character | Team name as the league app displays it |
#' | number | character | Sweater number, a string |
#' | position | character | goalkeeper, defender or forwarder (sic); one of 502 rows in the full 2026 body has the integer 0 |
#' | topscorer_points | integer | PostFinance Top Scorer game score (an app fantasy rating, not on-ice points) |
#' | topscorer_point_average | numeric | PostFinance Top Scorer game score per game |
#' | topscorer_market_value | integer | PostFinance Top Scorer game market value |
#' | topscorer_market_value_trend | integer | Market-value trend: 1 up, 0 flat, -1 down |
#' | topscorer_amount | integer | CHF amount credited to the team's Top Scorer jersey holder (non-zero for 14 players, one per team) |
#' | is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey (one per team) |
#' | nationality | character | Nationality; unobserved: null or empty in every sampled row |
#' | birth | character | Date of birth, YYYY-MM-DD |
#' | height | integer | Height; unobserved: null or empty in every sampled row |
#' | weight | integer | Weight; unobserved: null or empty in every sampled row |
#' | hand | character | Shooting or catching hand; unobserved: null or empty in every sampled row |
#' | lcrid | integer | Not determined (small integer, 0-4 in 2026) |
#' | noc | integer | Not determined (small integer, 0-2 in 2026) |
#' | gp | integer | Number of games played |
#' | g | integer | Number of goals scored |
#' | a1 | integer | First (primary) assists credited |
#' | a2 | integer | Second (secondary) assists credited |
#' | penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
#' | pim | integer | Penalty minutes |
#' | geq | integer | Even-strength goals |
#' | gpp | integer | Power-play goals |
#' | gpk | integer | Shorthanded goals |
#' | gwg | integer | Game-winning goals |
#' | ps | integer | Shootout shots taken |
#' | pg | integer | Not determined; 0 on every 2026 row |
#' | pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
#' | mi | integer | Minus count: on ice for a goal against |
#' | toi | integer | Time on ice, seconds |
#' | toi_eq | integer | Even-strength time on ice, seconds |
#' | toipp | integer | Power-play time on ice, seconds (lower-case pp, unlike the box score's toiPp) |
#' | toi_pk | integer | Shorthanded time on ice, seconds |
#' | sog | integer | Number of shots on goal |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | shi | integer | Number of shifts played |
#' | gwgk | integer | Not determined; 0 on every 2026 row |
#' | gw | integer | Goaltender wins (incl. overtime and shootout) |
#' | gl | integer | Goaltender losses (incl. overtime and shootout) |
#' | fk | integer | Games started in goal (inferred: fk <= gp; 42 of 44 for the 2026 games-played leader) |
#' | ga | integer | Goals against: number of goals conceded |
#' | sa | integer | Shots on goal against |
#' | svs | integer | Number of saves made by the goaltender |
#' | so | integer | Number of shutouts (games without a goal conceded) |
#' | mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
#' | assists | integer | Assists (a1 + a2; verified on every 2026 row) |
#' | points | integer | Points (g + assists; verified) |
#' | goal_efficiency | numeric | Shooting percentage, 100 * g / sog (verified); -1 with no shots |
#' | save_percentage | numeric | Save percentage, 0-100 |
#' | face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
#' | face_offs | integer | Faceoffs taken (fow + fol; verified) |
#' | is_captain | logical | Whether the player is the team captain |
#' | first_goalkeeper | logical | Team's first-choice goaltender |
#' | ga_per_game | numeric | Goals against per game (ga / gp; verified) |
#' | points_per_game | numeric | Points per game |
#' | svs_per_game | numeric | Goaltender saves per game played |
#' | bks_per_game | numeric | Blocked shots per game |
#' | sog_per_game | numeric | Shots on goal per game |
#' | shm_per_game | numeric | Missed shots per game |
#' | shp_per_game | numeric | Post hits per game |
#' | fow_per_game | numeric | Faceoffs won per game |
#' | fol_per_game | numeric | Faceoffs lost per game |
#' | toi_eq_per_game | numeric | Even-strength time on ice per game, seconds |
#' | toi_pp_per_game | numeric | Power-play time on ice per game, seconds |
#' | toi_pk_per_game | numeric | Shorthanded time on ice per game, seconds |
#' | face_offs_per_game_percentage | numeric | Not determined; 0 on the sampled rows |
#' | toi_average | numeric | Time on ice per game, seconds |
#' | plus_minus | integer | Plus/minus (pl - mi; verified) |
#' | tgs | integer | Team's total game time, seconds (about 3600 per game incl. overtime) |
#' | sep | integer | Goaltender time played, seconds |
#' | toi_percentage | numeric | Share of the team's game time in goal, 100 * sep / tgs (verified) |
#' | gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
#' | glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
#' | sag | integer | Shots on goal against that were saved (= svs on every 2026 goaltender row) |
#' | sago | integer | Shots on goal against that went in (= ga on every 2026 goaltender row) |
#' | sam | integer | Shots against that missed the net |
#' | sap | integer | Shots against that hit the post |
#' | sab | integer | Shots against that were blocked |
#' | saslot | integer | Slot shots against |
#' | sagoslot | integer | Slot shots against that went in |
#' | aeq | integer | Even-strength assists |
#' | app | integer | Power-play assists |
#' | apk | integer | Shorthanded assists |
#' | ha | integer | Hat tricks (inferred; at most 2 in 2026) |
#' | fo2 | integer | Minor (2-minute) penalties |
#' | fo5 | integer | Major (5-minute) penalties |
#' | fo10 | integer | 10-minute misconducts |
#' | gm | integer | Game misconducts |
#' | mp | integer | Match penalties |
#' | pm | integer | Not determined; 0 on every 2026 row |
#' | pwg | integer | Not determined; 0 on every 2026 row |
#' | sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
#' | fowoz | integer | Offensive-zone faceoffs won |
#' | fownz | integer | Neutral-zone faceoffs won |
#' | fowdz | integer | Defensive-zone faceoffs won |
#' | foloz | integer | Offensive-zone faceoffs lost |
#' | folnz | integer | Neutral-zone faceoffs lost |
#' | foldz | integer | Defensive-zone faceoffs lost |
#' | psg | integer | Goals scored in shootouts |
#' | pssvs | integer | Saves made in shootouts |
#' | got | integer | Overtime goals (a subset of g) |
#' | ot_rank | integer | Overtime-scoring rank; 0 on the 2026 rows (filled in 2027) |
#' | ot_g | integer | Overtime goals in the overtime table; 0 on every 2026 row |
#' | ot_a1 | integer | Overtime first assists; 0 on every 2026 row |
#' | ot_a2 | integer | Overtime second assists; 0 on every 2026 row |
#' | ot_toi | integer | Overtime time on ice, seconds; 0 on every 2026 row |
#' | ot_a | integer | Overtime assists; 0 on every 2026 row |
#' | ot_p | integer | Overtime points; 0 on every 2026 row |
#' | psgwg | integer | Game-deciding shootout goals; 0 on every 2026 row |
#' | sogslot | integer | Shots on goal from the slot |
#' | pswg | integer | Shootout winners (inferred) |
#' | toishi | numeric | Time on ice per shift, seconds |
#' | shigp | numeric | Shifts per game |
#' | saslotgp | numeric | Slot shots against per game |
#' | samgp | numeric | Missed shots against per game |
#' | sapgp | numeric | Post hits against per game |
#' | svs_pslot | numeric | Slot save percentage, 100 * (saslot - sagoslot) / saslot |
#' | sa_per_game | numeric | Shots on goal against per game |
#' | otw | integer | Goaltender overtime or shootout wins |
#' | otl | integer | Goaltender overtime or shootout losses |
#' | fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
#' | fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
#' | fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
#' | fot | integer | Faceoffs taken (fow + fol) |
#' | fogp | numeric | Faceoffs taken per game |
#' | psm | integer | Shootout misses (inferred; ps = psg + psm fails for 67 of 447 skaters) |
#' | ps_percentage | numeric | Shootout success percentage, 0-100 |
#' | gw_shutout | integer | Not determined; equals gw on goaltender rows |
#' | sogslot_per_game | numeric | Slot shots on goal per game |
#' | psag | integer | Shootout goals against (equals ps on goaltender rows) |
#' | pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
#' | show_rank | logical | App flag: show the row's rank |
#' | chf | integer | CHF credited for the player's points: 300 * points on every 2026 row (PostFinance youth-fund scheme; inferred) |
#' | team_stats | character | Per-team split for a player who changed teams; unobserved: null or empty in every sampled row |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_top_scorers(phase = 4940, season = 2026, take = 3))
#' }
nl_top_scorers <- function(take = NULL, season = NULL, phase = NULL, return_parsed = TRUE) {
  .nl_run("top_scorers", query = list(take = take, season = season, phase = phase), return_parsed = return_parsed)
}

#' **National League team player season stats**
#'
#' Season stats for one team's players.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/player/team/{teamId}`.
#'
#' @param team_id SIHF team id (`team_id` in [nl_standings()]; the same id as the SIHF family's `team_id`), e.g. `101144` (EV
#'   Zug). A whole number or a digit string.
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param phase SIHF phase id (`4940` is the 2025/26 Regular Season, `5297` the playoffs); phase ids change every season.
#'   `NULL` (the default) leaves the choice to the API: on `/player` that served the regular season (verified,
#'   byte-identical to `phase = 4940`); on the other routes that default is assumed, not verified.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (145 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | first_name | character | First (given) name of the player |
#' | last_name | character | Last (family) name of the player |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | team_short_name | character | Team abbreviation |
#' | team_name | character | Team name as the league app displays it |
#' | number | character | Sweater number, a string |
#' | position | character | goalkeeper, defender or forwarder (sic); one of 502 rows in the full 2026 body has the integer 0 |
#' | topscorer_points | integer | PostFinance Top Scorer game score (an app fantasy rating, not on-ice points) |
#' | topscorer_point_average | numeric | PostFinance Top Scorer game score per game |
#' | topscorer_market_value | integer | PostFinance Top Scorer game market value |
#' | topscorer_market_value_trend | integer | Market-value trend: 1 up, 0 flat, -1 down |
#' | topscorer_amount | integer | CHF amount credited to the team's Top Scorer jersey holder (non-zero for 14 players, one per team) |
#' | is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey (one per team) |
#' | nationality | character | Nationality; unobserved: null or empty in every sampled row |
#' | birth | character | Date of birth, YYYY-MM-DD |
#' | height | integer | Height; unobserved: null or empty in every sampled row |
#' | weight | integer | Weight; unobserved: null or empty in every sampled row |
#' | hand | character | Shooting or catching hand; unobserved: null or empty in every sampled row |
#' | lcrid | integer | Not determined (small integer, 0-4 in 2026) |
#' | noc | integer | Not determined (small integer, 0-2 in 2026) |
#' | gp | integer | Number of games played |
#' | g | integer | Number of goals scored |
#' | a1 | integer | First (primary) assists credited |
#' | a2 | integer | Second (secondary) assists credited |
#' | penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
#' | pim | integer | Penalty minutes |
#' | geq | integer | Even-strength goals |
#' | gpp | integer | Power-play goals |
#' | gpk | integer | Shorthanded goals |
#' | gwg | integer | Game-winning goals |
#' | ps | integer | Shootout shots taken |
#' | pg | integer | Not determined; 0 on every 2026 row |
#' | pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
#' | mi | integer | Minus count: on ice for a goal against |
#' | toi | integer | Time on ice, seconds |
#' | toi_eq | integer | Even-strength time on ice, seconds |
#' | toipp | integer | Power-play time on ice, seconds (lower-case pp, unlike the box score's toiPp) |
#' | toi_pk | integer | Shorthanded time on ice, seconds |
#' | sog | integer | Number of shots on goal |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | shi | integer | Number of shifts played |
#' | gwgk | integer | Not determined; 0 on every 2026 row |
#' | gw | integer | Goaltender wins (incl. overtime and shootout) |
#' | gl | integer | Goaltender losses (incl. overtime and shootout) |
#' | fk | integer | Games started in goal (inferred: fk <= gp; 42 of 44 for the 2026 games-played leader) |
#' | ga | integer | Goals against: number of goals conceded |
#' | sa | integer | Shots on goal against |
#' | svs | integer | Number of saves made by the goaltender |
#' | so | integer | Number of shutouts (games without a goal conceded) |
#' | mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
#' | assists | integer | Assists (a1 + a2; verified on every 2026 row) |
#' | points | integer | Points (g + assists; verified) |
#' | goal_efficiency | numeric | Shooting percentage, 100 * g / sog (verified); -1 with no shots |
#' | save_percentage | numeric | Save percentage, 0-100 |
#' | face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
#' | face_offs | integer | Faceoffs taken (fow + fol; verified) |
#' | is_captain | logical | Whether the player is the team captain |
#' | first_goalkeeper | logical | Team's first-choice goaltender |
#' | ga_per_game | numeric | Goals against per game (ga / gp; verified) |
#' | points_per_game | numeric | Points per game |
#' | svs_per_game | numeric | Goaltender saves per game played |
#' | bks_per_game | numeric | Blocked shots per game |
#' | sog_per_game | numeric | Shots on goal per game |
#' | shm_per_game | numeric | Missed shots per game |
#' | shp_per_game | numeric | Post hits per game |
#' | fow_per_game | numeric | Faceoffs won per game |
#' | fol_per_game | numeric | Faceoffs lost per game |
#' | toi_eq_per_game | numeric | Even-strength time on ice per game, seconds |
#' | toi_pp_per_game | numeric | Power-play time on ice per game, seconds |
#' | toi_pk_per_game | numeric | Shorthanded time on ice per game, seconds |
#' | face_offs_per_game_percentage | numeric | Not determined; 0 on the sampled rows |
#' | toi_average | numeric | Time on ice per game, seconds |
#' | plus_minus | integer | Plus/minus (pl - mi; verified) |
#' | tgs | integer | Team's total game time, seconds (about 3600 per game incl. overtime) |
#' | sep | integer | Goaltender time played, seconds |
#' | toi_percentage | numeric | Share of the team's game time in goal, 100 * sep / tgs (verified) |
#' | gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
#' | glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
#' | sag | integer | Shots on goal against that were saved (= svs on every 2026 goaltender row) |
#' | sago | integer | Shots on goal against that went in (= ga on every 2026 goaltender row) |
#' | sam | integer | Shots against that missed the net |
#' | sap | integer | Shots against that hit the post |
#' | sab | integer | Shots against that were blocked |
#' | saslot | integer | Slot shots against |
#' | sagoslot | integer | Slot shots against that went in |
#' | aeq | integer | Even-strength assists |
#' | app | integer | Power-play assists |
#' | apk | integer | Shorthanded assists |
#' | ha | integer | Hat tricks (inferred; at most 2 in 2026) |
#' | fo2 | integer | Minor (2-minute) penalties |
#' | fo5 | integer | Major (5-minute) penalties |
#' | fo10 | integer | 10-minute misconducts |
#' | gm | integer | Game misconducts |
#' | mp | integer | Match penalties |
#' | pm | integer | Not determined; 0 on every 2026 row |
#' | pwg | integer | Not determined; 0 on every 2026 row |
#' | sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
#' | fowoz | integer | Offensive-zone faceoffs won |
#' | fownz | integer | Neutral-zone faceoffs won |
#' | fowdz | integer | Defensive-zone faceoffs won |
#' | foloz | integer | Offensive-zone faceoffs lost |
#' | folnz | integer | Neutral-zone faceoffs lost |
#' | foldz | integer | Defensive-zone faceoffs lost |
#' | psg | integer | Goals scored in shootouts |
#' | pssvs | integer | Saves made in shootouts |
#' | got | integer | Overtime goals (a subset of g) |
#' | ot_rank | integer | Overtime-scoring rank; 0 on the 2026 rows (filled in 2027) |
#' | ot_g | integer | Overtime goals in the overtime table; 0 on every 2026 row |
#' | ot_a1 | integer | Overtime first assists; 0 on every 2026 row |
#' | ot_a2 | integer | Overtime second assists; 0 on every 2026 row |
#' | ot_toi | integer | Overtime time on ice, seconds; 0 on every 2026 row |
#' | ot_a | integer | Overtime assists; 0 on every 2026 row |
#' | ot_p | integer | Overtime points; 0 on every 2026 row |
#' | psgwg | integer | Game-deciding shootout goals; 0 on every 2026 row |
#' | sogslot | integer | Shots on goal from the slot |
#' | pswg | integer | Shootout winners (inferred) |
#' | toishi | numeric | Time on ice per shift, seconds |
#' | shigp | numeric | Shifts per game |
#' | saslotgp | numeric | Slot shots against per game |
#' | samgp | numeric | Missed shots against per game |
#' | sapgp | numeric | Post hits against per game |
#' | svs_pslot | numeric | Slot save percentage, 100 * (saslot - sagoslot) / saslot |
#' | sa_per_game | numeric | Shots on goal against per game |
#' | otw | integer | Goaltender overtime or shootout wins |
#' | otl | integer | Goaltender overtime or shootout losses |
#' | fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
#' | fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
#' | fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
#' | fot | integer | Faceoffs taken (fow + fol) |
#' | fogp | numeric | Faceoffs taken per game |
#' | psm | integer | Shootout misses (inferred; ps = psg + psm fails for 67 of 447 skaters) |
#' | ps_percentage | numeric | Shootout success percentage, 0-100 |
#' | gw_shutout | integer | Not determined; equals gw on goaltender rows |
#' | sogslot_per_game | numeric | Slot shots on goal per game |
#' | psag | integer | Shootout goals against (equals ps on goaltender rows) |
#' | pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
#' | show_rank | logical | App flag: show the row's rank |
#' | chf | integer | CHF credited for the player's points: 300 * points on every 2026 row (PostFinance youth-fund scheme; inferred) |
#' | team_stats | character | Per-team split for a player who changed teams; unobserved: null or empty in every sampled row |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_team_player_stats(phase = 4940, season = 2026, team_id = 101144))
#' }
nl_team_player_stats <- function(team_id, season = NULL, phase = NULL, return_parsed = TRUE) {
  .nl_run("team_player_stats", id = list(team_id = team_id), query = list(season = season, phase = phase), return_parsed = return_parsed)
}

#' **National League single player season stats**
#'
#' One player's season stats.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/player/{playerId}`.
#'
#' @param player_id SIHF licence number (`player_id` in [nl_player_stats()]; joins the SIHF family's `player_id`), e.g.
#'   `305519`. A whole number or a digit string.
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. `NULL` (the default) answers the *current*
#'   season (`gp` 7 in 2027 against 51 for `season = 2026` for one player), so pass it for history.
#' @param phase SIHF phase id (`4940` is the 2025/26 Regular Season, `5297` the playoffs); phase ids change every season.
#'   `NULL` (the default) leaves the choice to the API: on `/player` that served the regular season (verified,
#'   byte-identical to `phase = 4940`); on the other routes that default is assumed, not verified.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (145 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF *LicenceNr. |
#' | first_name | character | First (given) name of the player |
#' | last_name | character | Last (family) name of the player |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | team_short_name | character | Team abbreviation |
#' | team_name | character | Team name as the league app displays it |
#' | number | character | Sweater number, a string |
#' | position | character | goalkeeper, defender or forwarder (sic); one of 502 rows in the full 2026 body has the integer 0 |
#' | topscorer_points | integer | PostFinance Top Scorer game score (an app fantasy rating, not on-ice points) |
#' | topscorer_point_average | numeric | PostFinance Top Scorer game score per game |
#' | topscorer_market_value | integer | PostFinance Top Scorer game market value |
#' | topscorer_market_value_trend | integer | Market-value trend: 1 up, 0 flat, -1 down |
#' | topscorer_amount | integer | CHF amount credited to the team's Top Scorer jersey holder (non-zero for 14 players, one per team) |
#' | is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey (one per team) |
#' | nationality | character | Nationality; unobserved: null or empty in every sampled row |
#' | birth | character | Date of birth, YYYY-MM-DD |
#' | height | integer | Height; unobserved: null or empty in every sampled row |
#' | weight | integer | Weight; unobserved: null or empty in every sampled row |
#' | hand | character | Shooting or catching hand; unobserved: null or empty in every sampled row |
#' | lcrid | integer | Not determined (small integer, 0-4 in 2026) |
#' | noc | integer | Not determined (small integer, 0-2 in 2026) |
#' | gp | integer | Number of games played |
#' | g | integer | Number of goals scored |
#' | a1 | integer | First (primary) assists credited |
#' | a2 | integer | Second (secondary) assists credited |
#' | penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
#' | pim | integer | Penalty minutes |
#' | geq | integer | Even-strength goals |
#' | gpp | integer | Power-play goals |
#' | gpk | integer | Shorthanded goals |
#' | gwg | integer | Game-winning goals |
#' | ps | integer | Shootout shots taken |
#' | pg | integer | Not determined; 0 on every 2026 row |
#' | pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
#' | mi | integer | Minus count: on ice for a goal against |
#' | toi | integer | Time on ice, seconds |
#' | toi_eq | integer | Even-strength time on ice, seconds |
#' | toipp | integer | Power-play time on ice, seconds (lower-case pp, unlike the box score's toiPp) |
#' | toi_pk | integer | Shorthanded time on ice, seconds |
#' | sog | integer | Number of shots on goal |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | shi | integer | Number of shifts played |
#' | gwgk | integer | Not determined; 0 on every 2026 row |
#' | gw | integer | Goaltender wins (incl. overtime and shootout) |
#' | gl | integer | Goaltender losses (incl. overtime and shootout) |
#' | fk | integer | Games started in goal (inferred: fk <= gp; 42 of 44 for the 2026 games-played leader) |
#' | ga | integer | Goals against: number of goals conceded |
#' | sa | integer | Shots on goal against |
#' | svs | integer | Number of saves made by the goaltender |
#' | so | integer | Number of shutouts (games without a goal conceded) |
#' | mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
#' | assists | integer | Assists (a1 + a2; verified on every 2026 row) |
#' | points | integer | Points (g + assists; verified) |
#' | goal_efficiency | numeric | Shooting percentage, 100 * g / sog (verified); -1 with no shots |
#' | save_percentage | numeric | Save percentage, 0-100 |
#' | face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
#' | face_offs | integer | Faceoffs taken (fow + fol; verified) |
#' | is_captain | logical | Whether the player is the team captain |
#' | first_goalkeeper | logical | Team's first-choice goaltender |
#' | ga_per_game | numeric | Goals against per game (ga / gp; verified) |
#' | points_per_game | numeric | Points per game |
#' | svs_per_game | numeric | Goaltender saves per game played |
#' | bks_per_game | numeric | Blocked shots per game |
#' | sog_per_game | numeric | Shots on goal per game |
#' | shm_per_game | numeric | Missed shots per game |
#' | shp_per_game | numeric | Post hits per game |
#' | fow_per_game | numeric | Faceoffs won per game |
#' | fol_per_game | numeric | Faceoffs lost per game |
#' | toi_eq_per_game | numeric | Even-strength time on ice per game, seconds |
#' | toi_pp_per_game | numeric | Power-play time on ice per game, seconds |
#' | toi_pk_per_game | numeric | Shorthanded time on ice per game, seconds |
#' | face_offs_per_game_percentage | numeric | Not determined; 0 on the sampled rows |
#' | toi_average | numeric | Time on ice per game, seconds |
#' | plus_minus | integer | Plus/minus (pl - mi; verified) |
#' | tgs | integer | Team's total game time, seconds (about 3600 per game incl. overtime) |
#' | sep | integer | Goaltender time played, seconds |
#' | toi_percentage | numeric | Share of the team's game time in goal, 100 * sep / tgs (verified) |
#' | gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
#' | glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
#' | sag | integer | Shots on goal against that were saved (= svs on every 2026 goaltender row) |
#' | sago | integer | Shots on goal against that went in (= ga on every 2026 goaltender row) |
#' | sam | integer | Shots against that missed the net |
#' | sap | integer | Shots against that hit the post |
#' | sab | integer | Shots against that were blocked |
#' | saslot | integer | Slot shots against |
#' | sagoslot | integer | Slot shots against that went in |
#' | aeq | integer | Even-strength assists |
#' | app | integer | Power-play assists |
#' | apk | integer | Shorthanded assists |
#' | ha | integer | Hat tricks (inferred; at most 2 in 2026) |
#' | fo2 | integer | Minor (2-minute) penalties |
#' | fo5 | integer | Major (5-minute) penalties |
#' | fo10 | integer | 10-minute misconducts |
#' | gm | integer | Game misconducts |
#' | mp | integer | Match penalties |
#' | pm | integer | Not determined; 0 on every 2026 row |
#' | pwg | integer | Not determined; 0 on every 2026 row |
#' | sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
#' | fowoz | integer | Offensive-zone faceoffs won |
#' | fownz | integer | Neutral-zone faceoffs won |
#' | fowdz | integer | Defensive-zone faceoffs won |
#' | foloz | integer | Offensive-zone faceoffs lost |
#' | folnz | integer | Neutral-zone faceoffs lost |
#' | foldz | integer | Defensive-zone faceoffs lost |
#' | psg | integer | Goals scored in shootouts |
#' | pssvs | integer | Saves made in shootouts |
#' | got | integer | Overtime goals (a subset of g) |
#' | ot_rank | integer | Overtime-scoring rank; 0 on the 2026 rows (filled in 2027) |
#' | ot_g | integer | Overtime goals in the overtime table; 0 on every 2026 row |
#' | ot_a1 | integer | Overtime first assists; 0 on every 2026 row |
#' | ot_a2 | integer | Overtime second assists; 0 on every 2026 row |
#' | ot_toi | integer | Overtime time on ice, seconds; 0 on every 2026 row |
#' | ot_a | integer | Overtime assists; 0 on every 2026 row |
#' | ot_p | integer | Overtime points; 0 on every 2026 row |
#' | psgwg | integer | Game-deciding shootout goals; 0 on every 2026 row |
#' | sogslot | integer | Shots on goal from the slot |
#' | pswg | integer | Shootout winners (inferred) |
#' | toishi | numeric | Time on ice per shift, seconds |
#' | shigp | numeric | Shifts per game |
#' | saslotgp | numeric | Slot shots against per game |
#' | samgp | numeric | Missed shots against per game |
#' | sapgp | numeric | Post hits against per game |
#' | svs_pslot | numeric | Slot save percentage, 100 * (saslot - sagoslot) / saslot |
#' | sa_per_game | numeric | Shots on goal against per game |
#' | otw | integer | Goaltender overtime or shootout wins |
#' | otl | integer | Goaltender overtime or shootout losses |
#' | fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
#' | fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
#' | fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
#' | fot | integer | Faceoffs taken (fow + fol) |
#' | fogp | numeric | Faceoffs taken per game |
#' | psm | integer | Shootout misses (inferred; ps = psg + psm fails for 67 of 447 skaters) |
#' | ps_percentage | numeric | Shootout success percentage, 0-100 |
#' | gw_shutout | integer | Not determined; equals gw on goaltender rows |
#' | sogslot_per_game | numeric | Slot shots on goal per game |
#' | psag | integer | Shootout goals against (equals ps on goaltender rows) |
#' | pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
#' | show_rank | logical | App flag: show the row's rank |
#' | chf | integer | CHF credited for the player's points: 300 * points on every 2026 row (PostFinance youth-fund scheme; inferred) |
#' | team_stats | character | Per-team split for a player who changed teams; unobserved: null or empty in every sampled row |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_player_season(phase = 4940, player_id = 305519, season = 2026))
#' }
nl_player_season <- function(player_id, season = NULL, phase = NULL, return_parsed = TRUE) {
  .nl_run("player_season", id = list(player_id = player_id), query = list(season = season, phase = phase), return_parsed = return_parsed)
}

#' **National League standings**
#'
#' Standings with team season stats.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/teams`.
#'
#' @param season Season END year (`2026` is 2025/26), sent to the API unchanged. Changing it changes every list, so it
#'   scopes the answer. Lists are served for 2019-2027 (the app's season picker) and patchily before: 2012 and
#'   2017 answered, 2010/2014/2015/2016/2018 answered an empty list (probed 2026-10-12). `NULL` (the default)
#'   leaves the choice to the API.
#' @param phase SIHF phase id (`4940` is the 2025/26 Regular Season, `5297` the playoffs); phase ids change every season.
#'   `NULL` (the default) leaves the choice to the API: on `/player` that served the regular season (verified,
#'   byte-identical to `phase = 4940`); on the other routes that default is assumed, not verified.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (123 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | rank | integer | Position of the team in the standings table |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | name | character | Team name as the league app displays it |
#' | short_name | character | Team abbreviation |
#' | streak | character | Last results, most recent first: W or L |
#' | gp | integer | Number of games played |
#' | gw | integer | Wins, all kinds |
#' | gwot | integer | Overtime + shootout wins (gwo + gwpe) |
#' | gwpe | integer | Number of wins decided in a shootout |
#' | gl | integer | Losses, all kinds |
#' | glot | integer | Overtime + shootout losses (glo + glpe) |
#' | glpe | integer | Shootout losses |
#' | po | integer | Standings points: 3 per regulation win, 2 per overtime/shootout win, 1 per overtime/shootout loss (verified) |
#' | g | integer | Number of goals scored |
#' | geq | integer | Even-strength goals (geq + gpp + gpk can fall short of g) |
#' | gpp | integer | Power-play goals |
#' | gpk | integer | Shorthanded goals |
#' | ga | integer | Goals against: number of goals conceded |
#' | gaeq | integer | Even-strength goals against |
#' | gapp | integer | Shorthanded goals allowed on the power play |
#' | gapk | integer | Power-play goals allowed while shorthanded |
#' | go | integer | Goal difference, g - ga (verified) |
#' | ppo | integer | Power-play opportunities |
#' | pks | integer | Times shorthanded |
#' | sog | integer | Number of shots on goal |
#' | penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
#' | pim | integer | Penalty minutes |
#' | fow | integer | Number of faceoffs won |
#' | fol | integer | Number of faceoffs lost |
#' | shm | integer | Shots that missed the net |
#' | shp | integer | Shots that hit the post |
#' | shb | integer | Own shots blocked by an opponent |
#' | bks | integer | Opponent shots blocked |
#' | sa | integer | Shots on goal against |
#' | sam | integer | Shots against that missed the net; 0 on every 2026 team row |
#' | sap | integer | Shots against that hit the post; 0 on every 2026 team row |
#' | sab | integer | Shots against that were blocked; 0 on every 2026 team row |
#' | sort_order | integer | App sort key; not the rank |
#' | website | character | Club website URL |
#' | position_change | character | Rank movement since the last round ('none' on every 2026 row) |
#' | is_live | logical | Team is playing now |
#' | current_result | integer | Team's goals in a live game; 0 otherwise |
#' | current_opponent_result | integer | Opponent's goals in a live game; 0 otherwise |
#' | pos | integer | Standings position (= rank on every 2026 row) |
#' | hg | integer | Home games played (gp / 2 on every 2026 row) |
#' | gsow | integer | Shootout wins; duplicate of gwpe (equal on every 2026 row) |
#' | gfd | integer | Goals by defenders |
#' | gff | integer | Goals by forwards |
#' | gfi | integer | Goals by import (non-Swiss-licence) players (inferred; cf. the sihf/ teamGoalPosNation table) |
#' | gasow | integer | Shootout losses; duplicate of glpe (equal on every 2026 row) |
#' | pp | integer | Power-play time, seconds |
#' | bp | integer | Shorthanded (box-play) time, seconds |
#' | fo2 | integer | Minor (2-minute) penalties |
#' | fo5 | integer | Major (5-minute) penalties |
#' | fo10 | integer | 10-minute misconducts |
#' | gm | integer | Game misconducts |
#' | mp | integer | Match penalties |
#' | ps | integer | Shootout attempts taken |
#' | psg | integer | Goals scored in shootouts |
#' | psm | integer | Shootout misses (ps = psg + psm on every 2026 team row) |
#' | psa | integer | Shootout shots faced (psga + pssvs) |
#' | psga | integer | Shootout goals against |
#' | pssvs | integer | Saves made in shootouts |
#' | spec | integer | Total home attendance |
#' | cap | integer | Total home arena capacity over the home games |
#' | fowoz | integer | Offensive-zone faceoffs won |
#' | fownz | integer | Neutral-zone faceoffs won |
#' | fowdz | integer | Defensive-zone faceoffs won |
#' | foloz | integer | Offensive-zone faceoffs lost |
#' | folnz | integer | Neutral-zone faceoffs lost |
#' | foldz | integer | Defensive-zone faceoffs lost |
#' | sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
#' | sslot | integer | Shots from the slot |
#' | sag | integer | Shots on goal against that were saved; 0 on every 2026 team row (use sa) |
#' | sago | integer | Shots on goal against that went in; 0 on every 2026 team row (use ga) |
#' | saslot | integer | Slot shots against |
#' | qualification_status | character | Clinch marker ('none' on every 2026 row) |
#' | is_qualified_chl | logical | Qualified for the Champions Hockey League |
#' | goal_efficiency | numeric | Shooting percentage, 100 * g / sog (verified); -1 with no shots |
#' | ppg_efficiency | numeric | Power-play percentage, 100 * gpp / ppo (verified) |
#' | ppggp | numeric | Power-play goals per game |
#' | ppopgp | numeric | Power-play opportunities per game |
#' | ppgp | numeric | Power-play time per game, seconds |
#' | gpg | numeric | Goals scored per game played |
#' | gagp | numeric | Goals against per game |
#' | gappgp | numeric | Shorthanded goals against per game |
#' | geqgp | numeric | Even-strength goals per game |
#' | gaeqgp | numeric | Even-strength goals against per game |
#' | bks_per_game | numeric | Blocked shots per game |
#' | pim_per_game | numeric | Penalty minutes per game |
#' | ps_percentage | numeric | Shootout success percentage, 0-100 |
#' | attendance | integer | Average home attendance (spec / hg) |
#' | capacity | integer | Arena capacity (cap / hg) |
#' | utilization | numeric | Capacity utilisation, 100 * spec / cap |
#' | soggp | numeric | Shots on goal per game |
#' | sslotgp | numeric | Slot shots per game |
#' | shmgp | numeric | Missed shots per game |
#' | shpgp | numeric | Post hits per game |
#' | gpkgp | numeric | Shorthanded goals per game |
#' | pk_efficiency | numeric | Penalty-kill percentage, 100 * (1 - gapk / pks) (verified) |
#' | pksgp | numeric | Times shorthanded per game |
#' | gapkgp | numeric | Power-play goals against per game |
#' | bpgp | numeric | Shorthanded time per game, seconds |
#' | fot | integer | Faceoffs taken (fow + fol) |
#' | face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
#' | fogp | numeric | Faceoffs taken per game |
#' | fowgp | numeric | Faceoffs won per game |
#' | folgp | numeric | Faceoffs lost per game |
#' | fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
#' | fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
#' | fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
#' | sagp | numeric | Shots on goal against per game |
#' | saslotgp | numeric | Slot shots against per game |
#' | gffd | integer | Goals by forwards and defenders (gff + gfd; verified) |
#' | gffp | numeric | Share of goals by forwards, 100 * gff / g |
#' | gfdp | numeric | Share of goals by defenders, 100 * gfd / g |
#' | gfip | numeric | Share of goals by import players, 100 * gfi / g |
#' | pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
#' | pgp | numeric | Standings points per game |
#' | gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
#' | gwo | integer | Number of wins in overtime |
#' | glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
#' | glo | integer | Overtime losses |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_standings(phase = 4940, season = 2026))
#' }
nl_standings <- function(season = NULL, phase = NULL, return_parsed = TRUE) {
  .nl_run("standings", query = list(season = season, phase = phase), return_parsed = return_parsed)
}

#' **National League club record**
#'
#' Club CMS record: names, logo, website, ticket links.
#'
#' @details
#' **robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows `/api`. The owner approved building on it
#' on 2026-10-12, for polite, incremental use only: requests are spaced at least one second apart (on every
#' attempt, retries included), and a bulk backfill or a published mirror needs the owner's sign-off first. Crawl
#' incrementally: fetch a finished game once (keyed by `game_id`), find new games with one [nl_schedule()] call
#' per run, and fetch season tables at most once per game day.
#'
#' Seasons are END years and go on the wire unchanged. Ids (`*_id`) are whole-number `numeric` (a 14-digit
#' `game_id` exceeds R's 32-bit integer); every other whole-number column is `integer`. The club backend key the
#' API ships in `/teams/{id}/info` is dropped before parsing and never returned.
#'
#' Endpoint: `GET https://www.nationalleague.ch/api/teams/{teamId}/info`.
#'
#' @param team_id SIHF team id (`team_id` in [nl_standings()]; the same id as the SIHF family's `team_id`), e.g. `101144` (EV
#'   Zug). A whole number or a digit string.
#' @param return_parsed Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble; `FALSE` returns the raw JSON body
#'   as a nested list (api keys already dropped).
#' @return A `fastRhockey_data` tibble (27 columns) with the columns below, or the raw JSON body when `return_parsed = FALSE`.
#'   A valid but empty payload returns a zero-row tibble with these columns.
#'
#' | col_name | type | description |
#' |:---|:---|:---|
#' | name | character | Club name from the club's CMS record |
#' | cms_node_id | numeric | Umbraco CMS node id of the club page (whole number; not a team id). |
#' | url_segment | character | URL slug of the club page |
#' | type | character | CMS node kind ('Content') |
#' | content_type | character | CMS document type ('club') |
#' | url | character | Site path of the club page |
#' | fields_grid | character | Page layout blocks; unobserved: null or empty in every sampled row |
#' | fields_logo_name | character | Title of the club logo media item in the club CMS |
#' | fields_logo_id | numeric | Umbraco CMS media id; not a team id |
#' | fields_logo_url_segment | character | URL slug; unobserved: null or empty in every sampled row |
#' | fields_logo_type | character | CMS node kind ('Media') |
#' | fields_logo_content_type | character | Media kind ('Image') |
#' | fields_logo_url | character | Site path of the image |
#' | fields_logo_fields_umbraco_file_src | character | Site path of the file |
#' | fields_logo_fields_umbraco_file_focal_point | character | Crop focal point; unobserved: null or empty in every sampled row |
#' | fields_logo_fields_umbraco_file_crops | character | Crop presets; unobserved: null or empty in every sampled row |
#' | fields_logo_fields_umbraco_width | integer | Image width, pixels |
#' | fields_logo_fields_umbraco_height | integer | Image height, pixels |
#' | fields_logo_fields_umbraco_bytes | numeric | File size, bytes |
#' | fields_logo_fields_umbraco_extension | character | File extension of the club logo media file |
#' | team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
#' | fields_website | character | Club website URL |
#' | fields_ticket_url | character | Club ticket-shop URL |
#' | fields_game_ticket_url | character | Per-game ticket URL template; empty string in the sample |
#' | fields_overriden_name | character | Display-name override (sic); empty string in the sample |
#' | fields_players | character | Roster override; empty string in the sample |
#' | fields_api_name | character | Club key in the app's backend (e.g. 'evz') |
#'
#' @section Errors:
#' Raises a classed condition (all inherit `fastRhockey_error`): `fastRhockey_no_data` for a 404 or an empty answer (no
#' games or rows for that season, team or phase), `fastRhockey_asset_fetch` for a failed fetch, an error object in a 200, a body
#' without the ids it must carry, or an answer for another season, team, game or player, and `fastRhockey_bad_request`
#' for a 400/422 or an argument that is not a whole number.
#' @family National League Functions
#' @export
#' @examples
#' \donttest{
#'   try(nl_team_info(team_id = 101144))
#' }
nl_team_info <- function(team_id, return_parsed = TRUE) {
  .nl_run("team_info", id = list(team_id = team_id), return_parsed = return_parsed)
}
