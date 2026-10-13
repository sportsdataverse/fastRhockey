# khl.R -- Kontinental Hockey League (KHL) wrappers.
#
# Two backends, 23 exported wrappers, names/args/routes identical to sdv-py
# (sportsdataverse.hockey.khl, sdv-py #826):
#
#   khl_*            14 wrappers over POST https://www.khl.ru/rest/<route>/
#                    (session priming + Bitrix sessid + POST form)
#   khl_webcaster_*   9 wrappers over GET https://khl.api.webcaster.pro/api/khl_mobile/*.json
#                    (keyless JSON; honours getOption("fastRhockey.proxy"))
#
# Seasons are END years (2026 = 2025/26). Neither backend takes a year: the
# season (+ stage) is mapped to the khl.ru tournament id (tnId) / webcaster
# stage id through the season table of webcaster data.json -> stages_v2, the
# same table sdv-py reads (no arithmetic rule exists: 2009/10 is 167/168,
# 2008/09 is 160/165). The All-Star games ride along in calendar/list under
# their own tnId and are dropped by keeping only the requested tournament.
#
# Transport: every request goes through .build_request() / .polite_perform()
# (>= 1 s per host on every attempt). Refusals use the classed conditions of
# utils.R (fastRhockey_no_data / _asset_fetch / _bad_request) and mirror the
# sdv-py runtimes: an answer for something other than what was asked is a
# failed fetch, an empty one is "no data".
#
# Sections: constants/state -> value coercion (Python-faithful) -> generic frame
# builders -> khl.ru parsers -> webcaster parsers -> khl.ru runtime ->
# webcaster runtime -> exported wrappers (generated roxygen from sdv-py's
# manual_column_descriptions.yaml).

# Returns schemas of the 23 wrappers: column -> type code (i integer, f double, s character,
# b logical), in the documented order. Generated from sdv-py's khl_schemas.py; the column
# order and types here ARE the contract the parity tests pin.
.KHL_SCHEMAS <- list(
  khl_game_header = c(
    game_id = "i", tournament_id = "i", home_club_id = "i", home_team_season_id = "i",
    away_club_id = "i", away_team_season_id = "i", arena_id = "i", home_score = "i",
    away_score = "i", round = "i", roundname = "s", roundname_en = "s", date = "s",
    dayofweek = "i", time = "s", changes = "i", number = "i", home_name_l3 = "s",
    home_name_en = "s", home_name_l3_en = "s", home_city_en = "s", visitor_name_l3 = "s",
    visitor_name_en = "s", visitor_name_l3_en = "s", visitor_city_en = "s", arena = "s",
    arena_en = "s", arena_city = "s", arena_city_en = "s", msktime_sft = "i", gmttime_sft = "i",
    temp = "s", score = "s", ots = "s", attendance = "i", approved = "i", format_date_day = "s",
    time_format = "s", full_date = "i", date_type = "s", type = "s", winner = "s",
    visible_date = "s", visible_date_full = "s", visible_date_full2 = "s", visible_date_day = "s",
    win = "i"
  ),
  khl_game_info = c(
    game_id = "i", tournament_id = "i", home_club_id = "i", home_team_season_id = "i",
    away_club_id = "i", away_team_season_id = "i", arena_id = "i", home_score = "i",
    away_score = "i", round = "i", roundname = "s", roundname_en = "s", date = "s",
    dayofweek = "i", time = "s", changes = "i", number = "i", home_name_l3 = "s",
    home_name_en = "s", home_name_l3_en = "s", home_city_en = "s", visitor_name_l3 = "s",
    visitor_name_en = "s", visitor_name_l3_en = "s", visitor_city_en = "s", arena = "s",
    arena_en = "s", arena_city = "s", arena_city_en = "s", msktime_sft = "i", gmttime_sft = "i",
    temp = "s", score = "s", ots = "s", sc_p1 = "s", sc_p2 = "s", sc_p3 = "s", sc_p4 = "s",
    attendance = "i", approved = "i", format_date_day = "s", time_format = "s", full_date = "i",
    date_type = "s", type = "s", winner = "s", visible_date = "s", visible_date_full = "s",
    visible_date_full2 = "s", visible_date_day = "s", statusvalue = "s", pernum = "i",
    numlines = "i", gk_a_jn = "i", gk_b_jn = "i", is_events_have_multi_ovt = "b", idmainref1 = "i",
    idmainref2 = "i", idlineref1 = "i", idlineref2 = "i", jnmainref1 = "s", jnmainref2 = "s",
    jnlineref1 = "s", jnlineref2 = "s", refm1 = "s", refm2 = "s", refl1 = "s", refl2 = "s",
    status = "i", gk_a_player_id = "i", gk_b_player_id = "i"
  ),
  khl_game_protocol = c(
    game_id = "i", tournament_id = "i", period = "i", goal_number = "i", time = "s",
    team_side = "s", score_a = "i", score_b = "i", strength = "s", strength_text = "s",
    scorer_player_id = "i", scorer_name = "s", scorer_jn = "i", assist_1_player_id = "i",
    assist_1_name = "s", assist_1_jn = "i", assist_2_player_id = "i", assist_2_name = "s",
    assist_2_jn = "i", on_ice_a = "s", on_ice_b = "s"
  ),
  khl_pbp = c(
    game_id = "i", tournament_id = "i", period = "i", event_index = "i", event_type = "s",
    time = "s", time_seconds = "i", team_side = "s", jn = "i", player_id = "i", result = "s",
    result_text = "s", penalty_minutes = "i", score_a = "i", score_b = "i", won_side = "s",
    won_jn = "i", won_player_id = "i", lost_side = "s", lost_jn = "i", lost_player_id = "i",
    won_zone = "s", shooter_side = "s", shooter_jn = "i", shooter_player_id = "i",
    period_action = "s"
  ),
  khl_player_box = c(
    game_id = "i", tournament_id = "i", period = "i", team_side = "s", jn = "i", player_id = "i",
    player_name = "s", position = "s", g = "i", a = "i", p = "s", shots = "i", pim = "i", fo = "i",
    fow = "i", fow_pct = "f", bls = "i", hits = "i", fa = "i", p_intc = "i", sft = "s", sfte = "s",
    sftpp = "s", sftsh = "s", sften = "s", tiams = "s", ts = "f", dt = "f", as = "f", pcd = "f"
  ),
  khl_player_career = c(
    player_id = "i", season_label = "s", tournament_id = "i", team_season_id = "i", club_id = "i",
    team_name = "s", jn = "i", gp = "i", g = "i", a = "i", pts = "i", pm = "i", plus = "i",
    minus = "i", pim = "i", eqg = "i", ppg = "i", shg = "i", otg = "i", gwg = "i", sds = "i",
    sog = "i", sog_pct = "f", sog_avg = "f", fo = "i", fow = "i", fo_pct = "f", toi_avg = "s",
    sft_avg = "f", tie_avg = "s", sfte_avg = "f", tipp_avg = "s", sftpp_avg = "f", tish_avg = "s",
    sftsh_avg = "f", hits = "i", bls = "i", foa = "i", tka = "i", p_intc = "i"
  ),
  khl_player_profile = c(
    player_id = "i", firstname = "s", lastname = "s", photo = "s", position = "s",
    position_code = "s", biography = "s", is_hero = "b", is_rip = "b", links_stats = "s",
    pagelink = "s", summary_stats_gp = "i", summary_stats_a = "i", summary_stats_pm = "i",
    summary_stats_g = "i", summary_stats_pts = "i", jn = "i", club_name = "s", club_logo = "s",
    club_code = "s", height = "s", weight = "s", birthdate = "s", age = "s", contract_name = "s",
    contract_date = "s", country_name = "s", country_flag = "s", sport_country_name = "s",
    sport_country_flag = "s", stick = "s"
  ),
  khl_player_stats = c(
    group = "s", rank = "i", player_id = "i", player_name = "s", image = "s", jn = "i", gp = "i",
    g = "i", a = "i", pts = "i", pm = "i", plus = "i", minus = "i", pim = "i", eqg = "i",
    ppg = "i", shg = "i", otg = "i", gwg = "i", sds = "i", sog = "i", sog_pct = "f", sog_avg = "f",
    fo = "i", fow = "i", fo_pct = "f", toi_avg = "s", sft_avg = "f", tie_avg = "s", sfte_avg = "f",
    tipp_avg = "s", sftpp_avg = "f", tish_avg = "s", sftsh_avg = "f", hits = "i", bls = "i"
  ),
  khl_playoff_bracket = c(
    round = "i", wins_to_advance = "i", pair = "i", conference = "s", seed = "i", club_id = "i",
    name = "s", series_wins = "i", game_ids = "s"
  ),
  khl_roster = c(
    group = "s", player_id = "i", photo = "s", country = "s", lastname = "s", firstname = "s",
    jn = "i", age = "i", favorite = "b", link = "s"
  ),
  khl_schedule = c(
    game_id = "i", tournament_id = "i", date = "s", time = "s", home_club_id = "i",
    home_name = "s", away_club_id = "i", away_name = "s", home_score = "i", away_score = "i",
    decision = "s", period_scores = "s", winner_club_id = "i", approved = "i", arena_id = "i",
    arena_name = "s"
  ),
  khl_shots = c(
    game_id = "i", tournament_id = "i", period = "i", time = "s", time_seconds = "i",
    team_side = "s", jn = "i", player_id = "i", shot_type = "i", x = "i", y = "i",
    indent_left = "f", indent_top = "f", indent_right = "f", indent_bottom = "f"
  ),
  khl_standings = c(
    table = "s", rank = "i", club_id = "i", name = "s", gp = "i", w = "i", otw = "i", sow = "i",
    sol = "i", otl = "i", otl_en = "i", l = "i", pts = "i", goals_for = "i", goals_against = "i",
    is_out_playoff = "b", link = "s"
  ),
  khl_stat_filters = c(
    filter = "s", filter_name = "s", value = "s", name = "s", def = "i", active = "i"
  ),
  khl_webcaster_event = c(
    game_state_key = "s", period = "i", hd = "b", id = "i", season = "s", stage_id = "i",
    type_id = "i", commentator = "b", likes_enabled = "b", is_vertical_video = "b", khl_id = "i",
    arena_id = "i", arena_capacity = "i", arena_phone = "s", arena_website = "s", arena_name = "s",
    arena_address = "s", arena_city = "s", arena_image = "s", arena_geo_lat = "f",
    arena_geo_long = "f", team_a_id = "i", team_a_khl_id = "i", team_a_image = "s",
    team_a_name = "s", team_a_location = "s", team_a_shots = "i", team_a_gf = "i",
    team_a_ppg = "i", team_a_shg = "i", team_a_ppc = "i", team_a_vbr = "i", team_a_pim = "i",
    team_a_total_puck_control_time = "f", team_a_total_distance_travelled = "f",
    team_a_offensive_blue_line_crossings_count = "i", team_a_top_players = "s",
    team_a_start_fives = "s", team_a_players = "s", team_a_active = "b", team_b_id = "i",
    team_b_khl_id = "i", team_b_image = "s", team_b_name = "s", team_b_location = "s",
    team_b_shots = "i", team_b_gf = "i", team_b_ppg = "i", team_b_shg = "i", team_b_ppc = "i",
    team_b_vbr = "i", team_b_pim = "i", team_b_total_puck_control_time = "f",
    team_b_total_distance_travelled = "f", team_b_offensive_blue_line_crossings_count = "i",
    team_b_top_players = "s", team_b_start_fives = "s", team_b_players = "s", team_b_active = "b",
    quotes = "s", goals = "s", violations = "s", other_events_with_both_teams = "s",
    social_tags = "s", free = "b", yandex_rights = "b", yandex_id = "s", match_id = "i",
    balancer_type = "s", event_or_quote_type_name = "s", outer_stage_id = "i", stage_name = "s",
    parent_id = "i", name = "s", mref1 = "s", mref2 = "s", lref1 = "s", lref2 = "s",
    announce = "s", views = "s", stage_type = "s", audio_tracks = "s", image_big = "s",
    location = "s", start_at_day = "i", start_at = "i", event_start_at = "i", end_at = "i",
    tickets = "s", text_events = "s", not_regular = "b", infographics = "s",
    infographics_enabled = "b", score = "s", condensed_game_id = "i", condensed_game_name = "s",
    condensed_game_start_ts = "i", condensed_game_m3u8_url = "s", condensed_game_feed_url = "s",
    condensed_game_iframe_url = "s", condensed_game_iframe_code = "s",
    condensed_game_finish_ts = "i", condensed_game_image_url = "s",
    condensed_game_description = "s", condensed_game_audio_tracks = "s",
    condensed_game_is_vertical_video = "b", condensed_game_stage_id = "i",
    condensed_game_free = "b", condensed_game_yandex_rights = "b", condensed_game_yandex_id = "s",
    condensed_game_match_id = "i", condensed_game_balancer_type = "s",
    condensed_game_event_or_quote_type_name = "s", condensed_game_outer_stage_id = "i",
    condensed_game_stage_name = "s", highlight_id = "i", highlight_name = "s",
    highlight_start_ts = "i", highlight_m3u8_url = "s", highlight_feed_url = "s",
    highlight_iframe_url = "s", highlight_iframe_code = "s", highlight_finish_ts = "i",
    highlight_image_url = "s", highlight_description = "s", highlight_audio_tracks = "s",
    highlight_is_vertical_video = "b", highlight_stage_id = "i", highlight_free = "b",
    highlight_yandex_rights = "b", highlight_yandex_id = "s", highlight_match_id = "i",
    highlight_balancer_type = "s", highlight_event_or_quote_type_name = "s",
    highlight_outer_stage_id = "i", highlight_stage_name = "s", outer_url = "s", m3u8_url = "s",
    feed_url = "s", iframe_url = "s", iframe_code = "s", scores_first_period = "s",
    scores_second_period = "s", scores_third_period = "s", scores_overtime = "s",
    scores_bullitt = "s", this_pair_stat_events_count = "i",
    this_pair_stat_team_a_wins_count = "i", this_pair_stat_team_a_goals_count = "i",
    this_pair_stat_team_a_points_count = "i", this_pair_stat_team_b_wins_count = "i",
    this_pair_stat_team_b_goals_count = "i", this_pair_stat_team_b_points_count = "i",
    transaction_types = "s", sscore = "s", commentators_names = "s", transactions = "s",
    has_video = "b", geo_error = "s"
  ),
  khl_webcaster_events = c(
    game_state_key = "s", period = "i", hd = "b", id = "i", stage_id = "i", type_id = "i",
    commentator = "b", likes_enabled = "b", is_vertical_video = "b", khl_id = "i", team_a_id = "i",
    team_a_khl_id = "i", team_a_image = "s", team_a_name = "s", team_a_location = "s",
    team_b_id = "i", team_b_khl_id = "i", team_b_image = "s", team_b_name = "s",
    team_b_location = "s", name = "s", tickets = "s", location = "s", start_at_day = "i",
    start_at = "i", event_start_at = "i", end_at = "i", not_regular = "b", score = "s",
    scores_first_period = "s", scores_second_period = "s", scores_third_period = "s",
    scores_overtime = "s", scores_bullitt = "s", sscore = "s", image = "s",
    condensed_game_id = "i", highlight_id = "i", infographics = "s", infographics_enabled = "b",
    has_video = "b", m3u8_url = "s", feed_url = "s", iframe_url = "s", iframe_code = "s",
    free = "b", yandex_rights = "b", yandex_id = "s", match_id = "i", balancer_type = "s",
    event_or_quote_type_name = "s", outer_stage_id = "i", stage_name = "s", previews = "s"
  ),
  khl_webcaster_leaders = c(
    category = "s", rank = "i", id = "i", khl_id = "i", name = "s", image = "s",
    shirt_number = "i", role_key = "s", team_id = "i", team_khl_id = "i", team_name = "s",
    team_location = "s", pos = "s", stats = "s"
  ),
  khl_webcaster_players = c(
    id = "i", height = "i", weight = "i", age = "i", shirt_number = "i", khl_id = "i", role = "s",
    role_key = "s", country = "s", image = "s", flag_image_url = "s", name = "s", birthday = "i",
    stick = "s", team_id = "i", team_khl_id = "i", team_name = "s", team_location = "s",
    team_image = "s", team_division = "s", team_division_key = "s", team_conference = "s",
    team_conference_key = "s", stats = "s", teams = "s", quotes = "s", seasons_count_khl = "i",
    seasons_count_team = "i", positions = "s"
  ),
  khl_webcaster_players_light = c(
    id = "i", shirt_number = "i", khl_id = "i", image = "s", name = "s", team_id = "i",
    team_khl_id = "i", team_name = "s", team_location = "s", team = "s"
  ),
  khl_webcaster_stages = c(
    season = "i", type = "s", tournament_id = "i", stage_id = "i", season_label = "s", title = "s",
    display_rule = "s", percent_points_scored = "b"
  ),
  khl_webcaster_tables = c(
    season = "i", season_label = "s", rank = "i", gp = "i", w = "i", otw = "i", sow = "i",
    sol = "i", otl = "i", otl_en = "i", l = "i", pts = "i", pts_pct = "f", soa = "i", so = "i",
    gf = "i", ga = "i", pim = "i", pima = "i", id = "i", khl_id = "i", image = "s", tv_image = "s",
    name = "s", location = "s", division = "s", division_key = "s", conference = "s",
    conference_key = "s"
  ),
  khl_webcaster_team = c(
    id = "i", stage = "s", khl_id = "i", name = "s", location = "s", feed_items = "s",
    arena_id = "i", arena_capacity = "i", arena_phone = "s", arena_website = "s", arena_name = "s",
    arena_address = "s", arena_city = "s", arena_image = "s", arena_geo_lat = "f",
    arena_geo_long = "f", arenas = "s", calendar_events = "s", recent_events = "s", website = "s",
    mail = "s", foundation_year = "s", photo = "s", phone = "s", head_coach_photo = "s",
    head_coach_name = "s", social_networks_tw = "s", social_networks_vk = "s",
    social_networks_ok = "s", social_networks_fb = "s", social_networks_instagram = "s",
    social_networks_youtube = "s", social_networks_telegram = "s", apps_ios = "s",
    apps_android = "s", apps_windows = "s", address = "s", image = "s", division = "s",
    division_key = "s", conference = "s", conference_key = "s", stats = "s", players = "s"
  ),
  khl_webcaster_teams = c(
    id = "i", khl_id = "i", name = "s", location = "s", image = "s", division = "s",
    division_key = "s", conference = "s", conference_key = "s"
  )
)

# ---- constants and state ----------------------------------------------------

.KHL_SITE <- "https://www.khl.ru"
.KHL_WC_BASE <- "https://khl.api.webcaster.pro/api/khl_mobile"
.KHL_STAGES <- c("regular", "playoff")
# Keys removed from every body at any depth: betting odds / affiliate links and
# the caller's IP echoed by data.json.
.KHL_REDACTED <- c("bets", "fonbet", "remote_ip")
.KHL_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status")
.KHL_TOURNAMENT <- "values[tournament]"
# Routes that answer an error envelope without a tournament.
.KHL_NEEDS_TOURNAMENT <- c("/game/header/", "/game/text/", "/game/protocol/", "/standings/playoff/")
# Where each route keeps its rows (an empty container there is "no data"); an
# integer step is a 1-based list position.
.KHL_ROWS <- list(
  "/calendar/list/" = list("GAMES"),
  "/game/header/" = list("game"),
  "/game/text/" = list("game", "players"),
  "/game/protocol/" = list("teams"),
  "/standings/regular/" = list("json", "league"),
  "/standings/playoff/" = list("LIST"),
  "/stat/players/" = list("list", 1L, "list"),
  "/players/profile/" = list("DATA"),
  "/players/stats/" = list("tournament_summary"),
  "/clubs/team/" = list("players"),
  "/filters/statplayers/" = list("LIST")
)
.KHL_UA <- paste("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)",
                 "Chrome/126.0.0.0 Safari/537.36")

# sessions (site -> cookie jar + sessid) and the cached season table
.khl_state <- new.env(parent = emptyenv())

# Sleeping is a function so tests can replace it.
.khl_sleep <- function(seconds) Sys.sleep(seconds)

# ---- value coercion (the sdv-py parser helpers, Python semantics) -------------

.khl_is_obj <- function(x) is.list(x) && !is.null(names(x))
.khl_is_arr <- function(x) is.list(x) && is.null(names(x))
.khl_obj <- function(x) if (.khl_is_obj(x)) x else list()
.khl_arr <- function(x) if (.khl_is_arr(x)) x else list()
# Python truthiness of a JSON scalar / container
.khl_truthy <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) return(length(x) > 0)
  if (length(x) == 0 || is.na(x[[1]])) return(FALSE)
  if (is.character(x)) return(nzchar(x[[1]]))
  if (is.numeric(x) || is.logical(x)) return(x[[1]] != 0)
  TRUE
}
# Python `a or b`
.khl_or <- function(a, b) if (.khl_truthy(a)) a else b

# Python repr of a float ("3.0", "2.5", "1e-05"); whole numbers at or beyond
# 2^31 are ints in the JSON we read (jsonlite parses every big int as double).
.khl_pyfloat <- function(x) {
  if (is.nan(x)) return("nan")
  if (is.infinite(x)) return(if (x > 0) "inf" else "-inf")
  if (x == trunc(x) && abs(x) < 1e16) return(sprintf("%.1f", x))
  for (d in 1:17) {
    s <- sprintf(paste0("%.", d, "g"), x)
    if (as.numeric(s) == x) return(s)
  }
  s
}
.khl_is_bigint <- function(x) is.double(x) && is.finite(x) && x == trunc(x) && abs(x) >= 2147483648
# Python str() of a JSON scalar
.khl_pystr <- function(v) {
  if (is.logical(v)) return(if (isTRUE(v)) "True" else "False")
  if (is.integer(v) || .khl_is_bigint(v)) return(sprintf("%.0f", v))
  if (is.double(v)) return(.khl_pyfloat(v))
  as.character(v)
}

# A caller-supplied id as text: 18 and 18L both "18" (never Python's "18.0").
.khl_idstr <- function(x) {
  if (is.numeric(x) && length(x) == 1 && is.finite(x) && x == trunc(x)) sprintf("%.0f", x) else as.character(x)[[1]]
}

.khl_int <- function(v) {
  if (is.null(v) || is.list(v) || is.logical(v) || length(v) != 1) return(NA_real_)
  if (is.numeric(v)) return(if (is.finite(v) && v == trunc(v)) as.numeric(v) else NA_real_)
  text <- trimws(as.character(v))
  if (grepl("^-?[0-9]+$", text)) as.numeric(text) else NA_real_
}
.khl_float <- function(v) {
  if (is.null(v) || is.list(v) || is.logical(v) || length(v) != 1) return(NA_real_)
  if (is.numeric(v)) return(as.numeric(v))
  suppressWarnings(as.numeric(trimws(as.character(v))))
}
.khl_str <- function(v) {
  if (is.null(v) || is.list(v) || length(v) != 1) return(NA_character_)
  text <- .khl_pystr(v)
  if (is.na(text) || !nzchar(trimws(text))) NA_character_ else text
}
.khl_bool <- function(v) {
  if (is.null(v) || (length(v) == 1 && !is.list(v) && identical(v, ""))) return(NA)
  .khl_truthy(v)
}
# a numeric vector (whole numbers) as integer when it fits, else double
.khl_as_int <- function(x) {
  if (all(is.na(x) | abs(x) <= 2147483647)) as.integer(x) else x
}

.khl_underscore <- function(word) {
  word <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", word, perl = TRUE)
  word <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", word, perl = TRUE)
  tolower(gsub("-", "_", word, fixed = TRUE))
}

# One column from a list of JSON scalars, typed by schema code i / f / s / b.
.khl_col <- function(vals, code) {
  switch(code,
    i = .khl_as_int(vapply(vals, .khl_int, numeric(1))),
    f = vapply(vals, .khl_float, numeric(1)),
    s = vapply(vals, .khl_str, character(1)),
    b = vapply(vals, .khl_bool, logical(1)),
    stop("unknown column code: ", code)
  )
}
.khl_na <- function(code, n) {
  switch(code, i = rep(NA_integer_, n), f = rep(NA_real_, n),
         s = rep(NA_character_, n), b = rep(NA, n))
}

# Rows (list of named lists) -> a tibble with exactly `codes` (zero rows keep the types).
.khl_typed <- function(rows, codes) {
  cols <- lapply(names(codes), function(cn) {
    .khl_col(lapply(rows, function(r) r[[cn]]), codes[[cn]])
  })
  names(cols) <- names(codes)
  tibble::as_tibble(cols)
}

# ---- generic frame builders ---------------------------------------------------

.khl_empty <- function(key) {
  types <- c(i = "integer", f = "double", s = "character", b = "logical")
  schema <- .KHL_SCHEMAS[[key]]
  .empty_tibble(structure(unname(types[schema]), names = names(schema)))
}

# Nested objects -> one level of dotted keys; arrays stay cells; {} is NULL.
.khl_flatten <- function(obj, prefix = "") {
  out <- list()
  for (key in names(obj)) {
    value <- obj[[key]]
    name <- paste0(prefix, key)
    if (.khl_is_obj(value) && length(value) > 0) {
      nested <- .khl_flatten(value, paste0(name, "."))
      for (n in names(nested)) out[n] <- list(nested[[n]])
    } else if (.khl_is_obj(value)) {
      out[name] <- list(NULL)
    } else {
      out[name] <- list(value)
    }
  }
  out
}

# json.dumps(value, ensure_ascii=False), byte for byte.
.khl_json_str <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  s <- gsub("\b", "\\b", s, fixed = TRUE)
  s <- gsub("\f", "\\f", s, fixed = TRUE)
  if (grepl("[\001-\037]", s)) {
    for (i in setdiff(1:31, c(8L, 9L, 10L, 12L, 13L))) {
      s <- gsub(intToUtf8(i), sprintf("\\u%04x", i), s, fixed = TRUE)
    }
  }
  paste0("\"", s, "\"")
}
.khl_pyjson <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (.khl_is_obj(x)) {
      if (!length(x)) return("{}")
      keys <- vapply(names(x), .khl_json_str, character(1))
      vals <- vapply(x, .khl_pyjson, character(1))
      return(paste0("{", paste(paste0(keys, ": ", vals), collapse = ", "), "}"))
    }
    return(paste0("[", paste(vapply(x, .khl_pyjson, character(1)), collapse = ", "), "]"))
  }
  if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
  if (is.character(x)) return(.khl_json_str(x))
  .khl_pystr(x)
}
.khl_cell <- function(v) if (is.list(v)) .khl_pyjson(v) else v

.khl_kind <- function(v) {
  if (is.logical(v)) "bool"
  else if (is.integer(v) || .khl_is_bigint(v)) "int"
  else if (is.double(v)) "float"
  else "str"
}

# A column of JSON scalars (NULL = missing) as one R vector, the way polars infers it:
# a single kind keeps its type, int + float is float, any other mix is text.
.khl_vec <- function(values) {
  present <- Filter(Negate(is.null), values)
  kinds <- unique(vapply(present, .khl_kind, ""))
  if (length(kinds) > 1 && !setequal(kinds, c("int", "float"))) {
    values <- lapply(values, function(v) if (is.null(v)) NULL else .khl_pystr(v))
    kinds <- "str"
  }
  if (!length(kinds)) return(rep(NA_character_, length(values)))
  num <- function() vapply(values, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))
  switch(kinds[[1]],
    str = vapply(values, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1)),
    bool = vapply(values, function(v) if (is.null(v)) NA else v, logical(1)),
    int = if (length(kinds) == 1) .khl_as_int(num()) else num(),
    num()
  )
}

.KHL_ID <- "(^|_)(id|khl_id|tn_id|clubid|clubidt|cid|gid|tid|teid|teama|teamb|arenaid|key)$"

# Cast id columns to integer only when every value survives an exact round trip.
.khl_cast_ids <- function(df, cols) {
  if (nrow(df) == 0) return(df)
  for (col in cols) {
    x <- df[[col]]
    if (is.null(x) || is.integer(x) || is.logical(x)) next
    if (is.character(x)) {
      ok <- is.na(x) | grepl("^-?[0-9]+$", x)
      if (!all(ok) || any(!is.na(x) & x != sprintf("%.0f", as.numeric(x)))) next
      df[[col]] <- .khl_as_int(suppressWarnings(as.numeric(x)))
    } else if (is.double(x)) {
      if (!all(is.na(x) | (is.finite(x) & x == trunc(x)))) next
      df[[col]] <- .khl_as_int(x)
    }
  }
  df
}

# Row objects -> flattened (dotted keys) -> snake_case -> arrays as JSON text -> typed columns.
.khl_generic <- function(rows, key) {
  flat <- lapply(Filter(.khl_is_obj, rows), .khl_flatten)
  if (!length(flat)) return(.khl_empty(key))
  keys <- unique(unlist(lapply(flat, names)))
  data <- list()
  for (k in keys) {
    name <- gsub(".", "_", .khl_underscore(k), fixed = TRUE)
    while (name %in% names(data)) name <- paste0(name, "_")
    data[[name]] <- .khl_vec(lapply(flat, function(r) .khl_cell(r[[k]])))
  }
  df <- tibble::as_tibble(data)
  .khl_cast_ids(df, grep(.KHL_ID, names(df), value = TRUE, perl = TRUE))
}

# Documented columns first (typed as documented), then any extra the body carried. A
# documented column the body lacks is backfilled as typed NA and an all-NA one is cast;
# a column holding values keeps them.
.khl_conform <- function(df, key) {
  schema <- .KHL_SCHEMAS[[key]]
  if (nrow(df) == 0) return(.khl_empty(key))
  for (cn in names(schema)) {
    if (!cn %in% names(df) || all(is.na(df[[cn]]))) df[[cn]] <- .khl_na(schema[[cn]], nrow(df))
  }
  df[c(names(schema), setdiff(names(df), names(schema)))]
}

# Stat columns sent as numbers or numeric strings: mm:ss text stays text, rates are double,
# all-integer columns integer, all-numeric ones double; "-" and "" are NA.
.khl_stat_cast <- function(df, cols) {
  float_stats <- c("ts", "dt", "as", "pcd", "dt_a", "dt_b")
  for (cn in cols) {
    if (!cn %in% names(df)) next
    x <- df[[cn]]
    text <- trimws(vapply(seq_along(x), function(i) {
      if (is.na(x[[i]])) NA_character_ else .khl_pystr(x[[i]])
    }, character(1)))
    vals <- text[!is.na(text) & !text %in% c("", "-")]
    if (any(grepl(":", vals, fixed = TRUE))) next
    num <- suppressWarnings(as.numeric(text))
    if (grepl("(_pct|_avg)$", cn) || cn %in% float_stats) {
      df[[cn]] <- num
    } else if (all(grepl("^-?[0-9]+$", vals))) {
      df[[cn]] <- .khl_as_int(ifelse(grepl("^-?[0-9]+$", text), num, NA_real_))
    } else if (all(!is.na(suppressWarnings(as.numeric(vals))))) {
      df[[cn]] <- num
    }
  }
  df
}

.khl_player_id <- function(link) {
  m <- regmatches(as.character(link %||% ""), regexec("/players/([0-9]+)/", as.character(link %||% "")))[[1]]
  if (length(m) == 2) as.numeric(m[[2]]) else NA_real_
}

# game-clock seconds: an int, a numeric string (2013-era bodies) or mm:ss
.khl_seconds <- function(v) {
  secs <- .khl_int(v)
  if (!is.na(secs)) return(secs)
  m <- regmatches(trimws(as.character(v %||% "")), regexec("^([0-9]+):([0-9]{2})$", trimws(as.character(v %||% ""))))[[1]]
  if (length(m) == 3) as.numeric(m[[2]]) * 60 + as.numeric(m[[3]]) else NA_real_
}
.khl_mmss <- function(secs) sprintf("%02d:%02d", as.integer(secs %/% 60), as.integer(secs %% 60))

# ---- khl.ru parsers -----------------------------------------------------------

# The `data` object of a success envelope (empty for an error envelope or anything else).
.khl_data <- function(raw) {
  if (!.khl_is_obj(raw)) return(list())
  if ("status" %in% names(raw) &&
      (tolower(as.character(raw[["status"]] %||% "")) != "success" || .khl_truthy(raw[["errors"]]))) {
    return(list())
  }
  data <- if ("data" %in% names(raw)) raw[["data"]] else raw
  if (.khl_is_obj(data)) data else list()
}
.khl_game <- function(raw) .khl_obj(.khl_data(raw)[["game"]])
.khl_ids <- function(game) {
  d <- .khl_obj(.khl_obj(game[["info"]])[["data"]])
  list(game_id = d[["id"]], tournament_id = d[["tnId"]])
}

# players.{A,B}.<jn> -> one row per (team_side, jn), first wins
.khl_roster_of <- function(game) {
  rows <- list()
  for (side in c("A", "B")) {
    pl <- .khl_obj(.khl_obj(game[["players"]])[[side]])
    for (jn in names(pl)) {
      p <- .khl_obj(pl[[jn]])
      rows[[length(rows) + 1L]] <- list(
        team_side = side, jn = if ("jn" %in% names(p)) p[["jn"]] else jn,
        player_id = p[["id"]], player_name = p[["name"]], position = p[["pos"]]
      )
    }
  }
  r <- .khl_typed(rows, c(team_side = "s", jn = "i", player_id = "i", player_name = "s", position = "s"))
  r[!duplicated(paste(r$team_side, r$jn)), ]
}
# Left-join roster columns onto (side, jn) of df.
.khl_join_roster <- function(df, roster, side, jn, out) {
  key <- paste(df[[side]], df[[jn]], sep = "\r")
  idx <- match(key, paste(roster$team_side, roster$jn, sep = "\r"))
  idx[is.na(df[[side]]) | is.na(df[[jn]])] <- NA_integer_
  for (nm in names(out)) df[[out[[nm]]]] <- roster[[nm]][idx]
  df
}

.KHL_GAME_RENAMES <- c(
  id = "game_id", tnId = "tournament_id", teama = "home_club_id", homeId = "home_team_season_id",
  teamb = "away_club_id", visitorId = "away_team_season_id", arenaid = "arena_id",
  homeScore = "home_score", visitorScore = "away_score", gkA = "gk_a_jn", gkB = "gk_b_jn"
)
.KHL_GAME_INTS <- c(
  "game_id", "tournament_id", "home_club_id", "home_team_season_id", "away_club_id",
  "away_team_season_id", "arena_id", "home_score", "away_score", "round", "number",
  "dayofweek", "changes", "attendance", "gk_a_jn", "gk_b_jn", "gk_a_player_id", "gk_b_player_id"
)
# scalars of the game objects, first object wins, the id traps renamed
.khl_game_row <- function(...) {
  row <- list()
  for (obj in list(...)) {
    obj <- .khl_obj(obj)
    for (key in names(obj)) {
      value <- obj[[key]]
      if (is.list(value)) next
      nm <- if (key %in% names(.KHL_GAME_RENAMES)) .KHL_GAME_RENAMES[[key]] else .khl_underscore(key)
      if (!nm %in% names(row)) row[nm] <- list(value)
    }
  }
  row
}
.khl_game_frame <- function(row, key) {
  if (!.khl_truthy(row[["game_id"]])) return(.khl_empty(key))
  lead <- .KHL_GAME_INTS[1:9][.KHL_GAME_INTS[1:9] %in% names(row)]
  ordered <- c(lead, setdiff(names(row), lead))
  cols <- lapply(ordered, function(nm) {
    v <- row[[nm]]
    if (nm %in% .KHL_GAME_INTS) {
      s <- if (is.null(v)) NA_character_ else .khl_pystr(v)
      return(if (!is.na(s) && grepl("^-?[0-9]+$", s)) as.numeric(s) else NA_real_)
    }
    if (is.null(v)) return(NA_character_)
    if (is.numeric(v) || is.logical(v)) v else .khl_str(v)
  })
  names(cols) <- ordered
  for (nm in intersect(ordered, .KHL_GAME_INTS)) cols[[nm]] <- .khl_as_int(cols[[nm]])
  tibble::as_tibble(cols)
}

.khl_parse_game_header <- function(raw) {
  .khl_conform(.khl_game_frame(.khl_game_row(.khl_game(raw)), "khl_game_header"), "khl_game_header")
}
.khl_parse_game_info <- function(raw) {
  game <- .khl_game(raw)
  row <- .khl_game_row(.khl_obj(game[["info"]])[["data"]], game[["gameItem"]])
  roster <- .khl_roster_of(game)
  for (pair in list(c("A", "gk_a_jn"), c("B", "gk_b_jn"))) {
    if (pair[[2]] %in% names(row)) {
      jn <- .khl_int(row[[pair[[2]]]])
      hit <- which(roster$team_side == pair[[1]] & roster$jn == jn)
      id <- if (length(hit)) roster$player_id[[hit[[1]]]] else NA
      row[sub("_jn$", "_player_id", pair[[2]])] <- list(if (is.na(id)) NULL else id)
    }
  }
  .khl_conform(.khl_game_frame(row, "khl_game_info"), "khl_game_info")
}

.KHL_BOX_KEYS <- c(game_id = "i", tournament_id = "i", period = "i", team_side = "s", jn = "i")
.KHL_BOX_STATS <- c(
  g = "i", a = "i", p = "s", shots = "i", pim = "i", fo = "i", fow = "i", fow_pct = "f", bls = "i",
  hits = "i", fa = "i", p_intc = "i", sft = "s", sfte = "s", sftpp = "s", sftsh = "s", sften = "s",
  tiams = "s", ts = "f", dt = "f", as = "f", pcd = "f"
)
.khl_parse_player_box <- function(raw) {
  game <- .khl_game(raw)
  ids <- .khl_ids(game)
  rows <- list()
  for (side in c("A", "B")) {
    for (block in .khl_arr(.khl_obj(.khl_obj(game[["tab_stats"]])[["playerstats"]])[[side]])) {
      block <- .khl_obj(block)
      for (item in .khl_arr(block[["data"]])) {
        item <- .khl_obj(item)
        stats <- .khl_obj(item[["stats"]])
        names(stats) <- .khl_underscore(names(stats))
        rows[[length(rows) + 1L]] <- c(
          list(game_id = ids$game_id, tournament_id = ids$tournament_id, period = block[["period"]],
               team_side = side, jn = item[["jn"]]), stats
        )
      }
    }
  }
  box <- .khl_typed(rows, c(.KHL_BOX_KEYS, .KHL_BOX_STATS))
  box <- .khl_join_roster(box, .khl_roster_of(game), "team_side", "jn",
                          c(player_id = "player_id", player_name = "player_name", position = "position"))
  box[names(.KHL_SCHEMAS[["khl_player_box"]])]
}

.khl_parse_pbp <- function(raw) {
  game <- .khl_game(raw)
  ids <- .khl_ids(game)
  legend <- .khl_obj(.khl_obj(game[["tab_playbyplay"]])[["gameLegend"]])
  pnum <- vapply(names(legend), .khl_int, numeric(1))
  rows <- list()
  for (period in names(legend)[order(is.na(pnum), ifelse(is.na(pnum), 0, pnum))]) {
    for (ev in rev(.khl_arr(legend[[period]]))) {
      ev <- .khl_obj(ev)
      marker <- identical(ev[["type"]], "period")
      secs <- if (marker) NA_real_ else .khl_seconds(ev[["time"]])
      clock <- .khl_or(ev[["timems"]], ev[["timeMS"]])
      if (marker || (is.null(clock) && is.na(secs))) {
        clock <- ev[["time"]]
      } else if (is.null(clock) && !is.na(secs)) {
        clock <- .khl_mmss(secs)
      }
      rows[[length(rows) + 1L]] <- list(
        game_id = ids$game_id, tournament_id = ids$tournament_id, period = period,
        event_index = length(rows), event_type = ev[["type"]], time = clock,
        time_seconds = if (is.na(secs)) NULL else secs, team_side = ev[["teamAB"]], jn = ev[["jn"]],
        result = ev[["result"]], result_text = ev[["resultText"]], penalty_minutes = ev[["mins"]],
        score_a = ev[["scA"]], score_b = ev[["scB"]], won_side = ev[["wonAB"]], won_jn = ev[["wonJn"]],
        lost_side = ev[["lostAB"]], lost_jn = ev[["lostJn"]], won_zone = ev[["wonZone"]],
        shooter_side = ev[["shooterAB"]], shooter_jn = ev[["shooterJn"]],
        period_action = if (marker) ev[["action"]] else NULL
      )
    }
  }
  schema <- .KHL_SCHEMAS[["khl_pbp"]]
  df <- .khl_typed(rows, schema[!grepl("player_id$", names(schema))])
  roster <- .khl_roster_of(game)
  for (p in list(c("team_side", "jn", "player_id"), c("won_side", "won_jn", "won_player_id"),
                 c("lost_side", "lost_jn", "lost_player_id"),
                 c("shooter_side", "shooter_jn", "shooter_player_id"))) {
    df <- .khl_join_roster(df, roster, p[[1]], p[[2]], c(player_id = p[[3]]))
  }
  df[names(schema)]
}

.khl_parse_shots <- function(raw) {
  game <- .khl_game(raw)
  ids <- .khl_ids(game)
  rows <- lapply(.khl_arr(.khl_obj(game[["tab_shotsmap"]])[["shots"]]), function(shot) {
    shot <- .khl_obj(shot)
    secs <- .khl_seconds(shot[["time"]])
    list(
      game_id = ids$game_id, tournament_id = ids$tournament_id, period = shot[["period"]],
      time = .khl_or(shot[["timeMS"]], if (is.na(secs)) NULL else .khl_mmss(secs)),
      time_seconds = if (is.na(secs)) NULL else secs, team_side = shot[["teamAB"]], jn = shot[["jn"]],
      shot_type = shot[["shotType"]], x = shot[["x"]], y = shot[["y"]],
      indent_left = shot[["indentLeft"]], indent_top = shot[["indentTop"]],
      indent_right = shot[["indentRight"]], indent_bottom = shot[["indentBottom"]]
    )
  })
  schema <- .KHL_SCHEMAS[["khl_shots"]]
  df <- .khl_typed(rows, schema[names(schema) != "player_id"])
  df <- .khl_join_roster(df, .khl_roster_of(game), "team_side", "jn", c(player_id = "player_id"))
  df[names(schema)]
}

.khl_parse_schedule <- function(raw) {
  data <- .khl_data(raw)
  games <- list()
  for (week in .khl_arr(data[["GAMES"]])) {
    for (day in .khl_obj(week)) {
      for (g in .khl_arr(day)) if (.khl_is_obj(g)) games[[length(games) + 1L]] <- g
    }
  }
  wanted <- .khl_int(.khl_obj(data[["values"]])[["tournament"]])
  if (is.na(wanted) && length(games)) {
    tn <- vapply(games, function(g) .khl_int(g[["tnId"]]), numeric(1))
    wanted <- unique(tn)[[which.max(tabulate(match(tn, unique(tn))))]]
  }
  teams <- .khl_obj(data[["TEAMS"]])
  arenas <- .khl_obj(data[["ARENAS"]])
  lookup <- function(x, k) if (is.null(k)) NULL else x[[as.character(.khl_pystr(k))]]
  rows <- list()
  for (g in games) {
    if (!identical(.khl_int(g[["tnId"]]), wanted)) next
    sc <- paste(vapply(.khl_arr(g[["scP"]]), .khl_pystr, character(1)), collapse = ", ")
    rows[[length(rows) + 1L]] <- list(
      game_id = g[["id"]], tournament_id = g[["tnId"]], date = g[["date"]], time = g[["time_format"]],
      home_club_id = g[["teama"]], home_name = .khl_obj(lookup(teams, g[["teama"]]))[["NAME"]],
      away_club_id = g[["teamb"]], away_name = .khl_obj(lookup(teams, g[["teamb"]]))[["NAME"]],
      home_score = g[["homeScore"]], away_score = g[["visitorScore"]], decision = g[["ots"]],
      period_scores = if (nzchar(sc)) sc else NULL,
      winner_club_id = if (.khl_truthy(g[["win"]])) g[["win"]] else NULL,
      approved = g[["approved"]], arena_id = g[["arenaid"]], arena_name = lookup(arenas, g[["arenaid"]])
    )
  }
  df <- .khl_typed(rows, .KHL_SCHEMAS[["khl_schedule"]])
  df[order(df$date, df$time, df$game_id, na.last = TRUE, method = "radix"), ]
}

.khl_parse_game_protocol <- function(raw) {
  data <- .khl_data(raw)
  ids <- list(tournament_id = NULL, game_id = NULL)
  for (pdf in .khl_obj(data[["pdf"]])) {
    url <- as.character(.khl_obj(pdf)[["URL"]] %||% "")
    m <- regmatches(url, regexec("/pdf/([0-9]+)/([0-9]+)/", url))[[1]]
    if (length(m) == 3) {
      ids <- list(tournament_id = as.numeric(m[[2]]), game_id = as.numeric(m[[3]]))
      break
    }
  }
  rows <- list()
  goals_by_period <- .khl_obj(data[["goals"]])
  for (period in names(goals_by_period)) {
    for (goal in .khl_arr(goals_by_period[[period]])) {
      goal <- .khl_obj(goal)
      row <- list(
        game_id = ids$game_id, tournament_id = ids$tournament_id,
        period = if ("per" %in% names(goal)) goal[["per"]] else period,
        goal_number = goal[["num"]], time = goal[["pmg_time"]], team_side = goal[["teamAB"]],
        score_a = goal[["scoreA"]], score_b = goal[["scoreB"]], strength = goal[["pmg"]],
        strength_text = goal[["pmg_trans"]], on_ice_a = goal[["oniceA"]], on_ice_b = goal[["oniceB"]]
      )
      for (who in c("scorer", "assist_1", "assist_2")) {
        person <- .khl_obj(goal[[who]])
        row[paste0(who, "_player_id")] <- list(if (is.na(.khl_player_id(person[["link"]]))) NULL
                                                else .khl_player_id(person[["link"]]))
        row[paste0(who, "_name")] <- list(person[["name"]])
        row[paste0(who, "_jn")] <- list(person[["num"]])
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  df <- .khl_typed(rows, .KHL_SCHEMAS[["khl_game_protocol"]])
  df[order(df$period, df$goal_number, na.last = FALSE, method = "radix"), ]
}

.khl_parse_standings <- function(raw) {
  tables <- .khl_obj(.khl_data(raw)[["json"]])
  rows <- list()
  for (kind in c("league", "conferences", "divisions")) {
    for (table in .khl_arr(tables[[kind]])) {
      table <- .khl_obj(table)
      for (team in .khl_arr(table[["rows"]])) {
        team <- .khl_obj(team)
        stats <- .khl_obj(team[["s"]])
        g <- as.character(.khl_or(stats[["g"]], ""))
        gf <- sub("-.*$", "", g)
        ga <- if (grepl("-", g, fixed = TRUE)) sub("^[^-]*-", "", g) else ""
        row <- c(
          list(table = .khl_or(table[["table_tab"]], .khl_or(table[["item"]], kind)),
               rank = team[["r"]], club_id = team[["clubid"]], name = team[["name"]]),
          stats,
          list(goals_for = gf, goals_against = ga, is_out_playoff = team[["is_out_playoff"]],
               link = team[["link"]])
        )
        rows[[length(rows) + 1L]] <- row[!duplicated(names(row), fromLast = TRUE)]
      }
    }
  }
  .khl_typed(rows, .KHL_SCHEMAS[["khl_standings"]])
}

.khl_parse_playoff_bracket <- function(raw) {
  data <- .khl_data(raw)
  clubs <- .khl_obj(data[["CLUBS"]])
  rows <- list()
  for (rnd in .khl_arr(data[["LIST"]])) {
    rnd <- .khl_obj(rnd)
    couples <- .khl_obj(rnd[["couple"]])
    for (pair in names(couples)) {
      for (team in .khl_obj(couples[[pair]])) {
        team <- .khl_obj(team)
        games <- Filter(function(g) .khl_is_obj(g) && .khl_truthy(g[["id"]]), .khl_arr(team[["games"]]))
        ids <- vapply(games, function(g) .khl_pystr(g[["id"]]), character(1))
        club <- if (is.null(team[["id"]])) NULL else clubs[[.khl_pystr(team[["id"]])]]
        rows[[length(rows) + 1L]] <- list(
          round = rnd[["number"]], wins_to_advance = rnd[["wins"]],
          pair = if ("pair" %in% names(team)) team[["pair"]] else pair,
          conference = team[["conf"]], seed = team[["num"]], club_id = team[["id"]],
          name = .khl_obj(club)[["name"]], series_wins = team[["score"]],
          game_ids = if (length(ids)) paste(ids, collapse = ",") else NULL
        )
      }
    }
  }
  .khl_typed(rows, .KHL_SCHEMAS[["khl_playoff_bracket"]])
}

.khl_parse_player_stats <- function(raw) {
  rows <- list()
  for (group in .khl_arr(.khl_data(raw)[["list"]])) {
    group <- .khl_obj(group)
    for (item in .khl_arr(group[["list"]])) {
      item <- .khl_obj(item)
      cells <- lapply(.khl_arr(item[["first"]]), .khl_obj)
      rank <- NULL
      for (cell in cells) if (identical(cell[["type"]], "t")) { rank <- cell[["val"]]; break }
      who <- list()
      for (cell in cells) if (.khl_truthy(cell[["link"]])) { who <- cell; break }
      pid <- .khl_player_id(who[["link"]])
      rows[[length(rows) + 1L]] <- c(
        list(group = group[["code"]], rank = rank, player_id = if (is.na(pid)) NULL else pid,
             player_name = who[["val"]], image = who[["image"]]),
        .khl_obj(item[["cols"]])
      )
    }
  }
  df <- .khl_generic(rows, "khl_player_stats")
  df <- .khl_stat_cast(df, setdiff(names(df), c("group", "player_name", "image")))
  .khl_conform(df, "khl_player_stats")
}

.khl_parse_player_profile <- function(raw) {
  person <- .khl_obj(.khl_data(raw)[["DATA"]])
  if (!length(person)) return(.khl_empty("khl_player_profile"))
  pid <- .khl_player_id(person[["pagelink"]])
  df <- .khl_generic(list(c(list(player_id = if (is.na(pid)) NULL else pid), person)), "khl_player_profile")
  df <- .khl_stat_cast(df, grep("^summary_stats_", names(df), value = TRUE))
  .khl_conform(df, "khl_player_profile")
}

.khl_parse_player_career <- function(raw) {
  data <- .khl_data(raw)
  rows <- list()
  for (season in .khl_arr(data[["tournament_summary"]])) {
    season <- .khl_obj(season)
    for (tn in .khl_arr(season[["tournamnets"]])) {
      tn <- .khl_obj(tn)
      team <- .khl_obj(tn[["team"]])
      rows[[length(rows) + 1L]] <- c(
        list(player_id = data[["id"]], season_label = season[["name"]], tournament_id = tn[["key"]],
             team_season_id = tn[["tid"]], club_id = team[["gid"]], team_name = team[["name"]]),
        .khl_obj(tn[["stats"]])
      )
    }
  }
  df <- .khl_generic(rows, "khl_player_career")
  keep <- c("player_id", "season_label", "tournament_id", "team_season_id", "club_id", "team_name")
  df <- .khl_cast_ids(df, intersect(c("player_id", "tournament_id", "team_season_id", "club_id"), names(df)))
  .khl_conform(.khl_stat_cast(df, setdiff(names(df), keep)), "khl_player_career")
}

.khl_parse_roster <- function(raw) {
  players <- .khl_obj(.khl_data(raw)[["players"]])
  rows <- list()
  for (group in names(players)) {
    for (p in .khl_arr(players[[group]])) {
      p <- .khl_obj(p)
      rows[[length(rows) + 1L]] <- c(list(group = group, player_id = p[["id"]]),
                                     p[names(p) != "id"])
    }
  }
  .khl_conform(.khl_generic(rows, "khl_roster"), "khl_roster")
}

.khl_parse_stat_filters <- function(raw) {
  filters <- .khl_obj(.khl_data(raw)[["LIST"]])
  rows <- list()
  for (code in names(filters)) {
    f <- .khl_obj(filters[[code]])
    for (o in .khl_arr(f[["VALUE"]])) {
      o <- .khl_obj(o)
      rows[[length(rows) + 1L]] <- c(list(filter = code, filter_name = f[["NAME"]], value = o[["i"]]),
                                     o[names(o) != "i"])
    }
  }
  df <- .khl_generic(rows, "khl_stat_filters")
  if ("value" %in% names(df)) {
    df$value <- vapply(seq_len(nrow(df)), function(i) {
      if (is.na(df$value[[i]])) NA_character_ else .khl_pystr(df$value[[i]])
    }, character(1))
  }
  df <- .khl_cast_ids(df, intersect("def", names(df)))
  .khl_conform(df, "khl_stat_filters")
}

# ---- webcaster parsers --------------------------------------------------------

# {"event": {...}} -> {...}: a one-key object wrapping one object
.khl_unwrap <- function(item) {
  if (.khl_is_obj(item) && length(item) == 1 && .khl_is_obj(item[[1]])) item[[1]] else item
}
.khl_parse_webcaster <- function(raw, key) {
  rows <- if (.khl_is_arr(raw)) lapply(raw, .khl_unwrap)
          else if (.khl_is_obj(raw) && length(raw)) list(.khl_unwrap(raw))
          else list()
  .khl_conform(.khl_generic(rows, key), key)
}
.khl_end_year <- function(label) {
  .khl_int(sub("^.*/", "", as.character(label %||% "")))
}
.khl_parse_stages <- function(raw) {
  rows <- lapply(.khl_arr(.khl_obj(raw)[["stages_v2"]]), function(s) {
    s <- .khl_obj(s)
    list(season = .khl_end_year(s[["season"]]), type = s[["type"]], tournament_id = s[["khl_id"]],
         stage_id = s[["id"]], season_label = s[["season"]], title = s[["title"]],
         display_rule = s[["display_rule"]], percent_points_scored = s[["percent_points_scored"]])
  })
  .khl_typed(rows, .KHL_SCHEMAS[["khl_webcaster_stages"]])
}
.khl_parse_leaders <- function(raw) {
  rows <- list()
  for (cat in .khl_arr(raw)) {
    cat <- .khl_obj(cat)
    players <- .khl_arr(cat[["players"]])
    for (i in seq_along(players)) {
      rows[[length(rows) + 1L]] <- c(list(category = cat[["name"]], rank = i), .khl_obj(players[[i]]))
    }
  }
  .khl_conform(.khl_generic(rows, "khl_webcaster_leaders"), "khl_webcaster_leaders")
}
.KHL_TABLE_TEXT <- c("image", "tv_image", "name", "location", "division", "division_key",
                     "conference", "conference_key")
.khl_parse_tables <- function(raw) {
  rows <- list()
  for (block in .khl_arr(raw)) {
    block <- .khl_obj(block)
    teams <- .khl_arr(.khl_obj(block[["tables"]])[["championship"]])
    for (i in seq_along(teams)) {
      rows[[length(rows) + 1L]] <- c(
        list(season = .khl_end_year(block[["season"]]), season_label = block[["season"]], rank = i),
        .khl_obj(teams[[i]])
      )
    }
  }
  df <- .khl_generic(rows, "khl_webcaster_tables")
  skip <- c("season", "season_label", "rank", "id", "khl_id", .KHL_TABLE_TEXT)
  .khl_conform(.khl_stat_cast(df, setdiff(names(df), skip)), "khl_webcaster_tables")
}

# ---- webcaster runtime ----------------------------------------------------------

# One throttled GET of a webcaster route (JSON, simplifyVector = FALSE). A repeated query
# key (team_ids, player_ids) is sent once per value. Through fastRhockey.proxy the failure
# is reported without its cause, which can carry the proxy URL.
.khl_wc_fetch <- function(url, query = list()) {
  query <- Filter(Negate(is.null), query)
  run <- function() {
    # HTTP/1.1 (CURL_HTTP_VERSION_1_1): over HTTP/2 this host's responses stalled for ~60 s.
    req <- .build_request(url) |> httr2::req_options(http_version = 2L)
    if (length(query)) req <- httr2::req_url_query(req, !!!query, .multi = "explode")
    req |> .polite_perform() |> .euro_resp_json(simplifyVector = FALSE)
  }
  if (is.null(.resolve_proxy(NULL))) return(run())
  where <- .euro_where(url)
  tryCatch(
    run(),
    fastRhockey_asset_fetch = function(e) {
      .abort_asset_fetch("{where}: the fetch through the {.code fastRhockey.proxy} option failed.", parent = NA)
    }
  )
}

# Every regular-season and playoff stage of data.json -> stages_v2 (cached in-process;
# `.khl_state$season_table <- NULL` refetches).
.khl_season_table <- function() {
  if (!is.null(.khl_state$season_table)) return(.khl_state$season_table)
  raw <- .khl_wc_fetch(paste0(.KHL_WC_BASE, "/data.json"), list(locale = "en"))
  stages <- .khl_parse_stages(raw)
  keep <- stages[stages$type %in% .KHL_STAGES & !is.na(stages$season) & !is.na(stages$type), ]
  if (nrow(keep) == 0) {
    where <- .euro_where(paste0(.KHL_WC_BASE, "/data.json"))
    .abort_no_data("{where}: stages_v2 lists no regular or playoff stage")
  }
  .khl_state$season_table <- keep
  keep
}

# The stage of a season END year: its webcaster stage_id and khl.ru tournament_id.
.khl_stage_for <- function(season, stage = "regular") {
  kind <- if (.khl_truthy(stage)) stage else "regular"
  if (!is.character(kind) || length(kind) != 1 || !kind %in% .KHL_STAGES) {
    valid <- .KHL_STAGES
    .abort_bad_request("{.arg stage} must be one of {.val {valid}}, not {.val {stage}}.")
  }
  year <- if (length(season) == 1) suppressWarnings(as.integer(season)) else NA_integer_
  if (is.na(year)) {
    .abort_bad_request("{.arg season} must be the season's END year as an integer (2026 = 2025/26), not {.val {season}}.")
  }
  table <- .khl_season_table()
  hit <- which(table$season == year & table$type == kind)
  if (length(hit)) return(as.list(table[hit[[1]], ]))
  years <- range(table$season)
  .abort_no_data(
    "The KHL has no {kind} stage for season {year} (END year); the season table spans {years[[1]]}-{years[[2]]}."
  )
}

.khl_strip <- function(x) {
  if (!is.list(x)) return(x)
  if (!is.null(names(x))) x <- x[!names(x) %in% .KHL_REDACTED]
  lapply(x, .khl_strip)
}

.khl_wc_check <- function(url, raw, query) {
  where <- .euro_where(url)
  if (.khl_is_obj(raw) && length(raw) && all(names(raw) %in% .KHL_ERROR_KEYS) && "error" %in% names(raw)) {
    err <- raw[["error"]]
    code <- if (.khl_is_obj(err)) err[["code"]] else NULL
    message <- substr(gsub("\\s+", " ", trimws(paste(as.character(
      if (.khl_is_obj(err)) err[["message"]] %||% "" else err), collapse = " "))), 1, 200)
    if (identical(as.character(code), "404")) .abort_no_data("{where} answered an error envelope: 404 {message}")
    code <- if (is.null(code)) "NULL" else as.character(code)
    .abort_asset_fetch("{where} answered an error envelope: {code} {message}")
  }
  if (.khl_is_obj(raw) && !length(raw)) .abort_no_data("{where} answered an empty object")
  route <- sub("^.*/", "", url)
  wanted <- query[["id"]]
  if (route %in% c("event_v2.json", "team_v2.json") && !is.null(wanted)) {
    got <- if (.khl_is_obj(raw)) .khl_obj(raw[[sub("_.*$", "", route)]])[["id"]] else NULL
    got_s <- if (is.null(got)) "None" else .khl_pystr(got)
    if (!identical(got_s, .khl_idstr(wanted))) {
      wanted_s <- .khl_idstr(wanted)
      .abort_asset_fetch("{where}: asked for id {wanted_s}, answered {got_s}")
    }
  }
  if (route == "events_v2.json" && !is.null(query[["stage_id"]]) && .khl_is_arr(raw)) {
    asked <- .khl_idstr(query[["stage_id"]])
    got <- unique(vapply(Filter(.khl_is_obj, raw), function(r) {
      id <- .khl_obj(r[["event"]])[["stage_id"]]
      if (is.null(id)) "None" else .khl_pystr(id)
    }, character(1)))
    other <- sort(setdiff(got, asked))
    if (length(other)) {
      other_s <- paste(other, collapse = ", ")
      .abort_asset_fetch("{where}: asked for stage {asked}, answered stage(s) {other_s}")
    }
  }
  ids <- query[["q[id_in][]"]]
  if (route == "players_v2.json" && !is.null(ids) && .khl_is_arr(raw)) {
    asked <- vapply(as.list(ids), .khl_idstr, character(1))
    got <- unique(vapply(Filter(.khl_is_obj, raw), function(r) {
      id <- .khl_obj(r[["player"]])[["id"]]
      if (is.null(id)) "None" else .khl_pystr(id)
    }, character(1)))
    other <- setdiff(got, asked)
    if (length(other)) {
      asked_s <- paste(sort(asked), collapse = ", ")
      other_s <- paste(sort(other), collapse = ", ")
      .abort_asset_fetch("{where}: asked for players {asked_s}, answered {other_s}")
    }
  }
  invisible(raw)
}

# GET a webcaster route (season / stage resolved to stage_id), verify it, strip the redactions.
.khl_wc_get <- function(route, params = list()) {
  url <- paste0(.KHL_WC_BASE, "/", route)
  query <- Filter(Negate(is.null), params)
  season <- query[["season"]]
  query[c("season", "stage")] <- NULL
  if (!is.null(season) && is.null(query[["stage_id"]])) {
    query[["stage_id"]] <- .khl_stage_for(season, params[["stage"]])[["stage_id"]]
  }
  raw <- .khl_wc_fetch(url, query)
  .khl_wc_check(url, raw, query)
  .khl_strip(raw)
}

# ---- khl.ru runtime -------------------------------------------------------------

.khl_headers <- function(site, extra = NULL) {
  utils::modifyList(
    list(`User-Agent` = .KHL_UA, `X-Requested-With` = "XMLHttpRequest", Origin = site,
         Referer = paste0(site, "/"), Accept = "application/json, */*"),
    as.list(extra)
  )
}

# Throttled perform; a 500 / 502 / 504 is retried up to `attempts` in all (2, 4 s apart);
# connection failures and 429 / 503 are retried by .polite_perform().
.khl_send <- function(req, attempts = 3L) {
  for (i in seq_len(attempts)) {
    resp <- .polite_perform(req)
    if (!httr2::resp_status(resp) %in% c(500L, 502L, 504L) || i == attempts) return(resp)
    .khl_sleep(2 * 2^(i - 1))
  }
}

# GET site/ (following the cookie-setting 307) and scrape the Bitrix sessid.
.khl_prime <- function(session, timeout = 60) {
  home <- paste0(session$site, "/")
  resp <- .build_request(home, headers = list(`User-Agent` = .KHL_UA, Accept = "text/html,*/*"),
                         timeout = timeout) |>
    httr2::req_cookie_preserve(session$cookies) |>
    .khl_send()
  .euro_check_status(resp)
  text <- if (httr2::resp_has_body(resp)) .resp_text(resp) else ""
  m <- regmatches(text, regexec("sessid\":\"([a-f0-9]{32})", text))[[1]]
  if (length(m) != 2) {
    where <- .euro_where(home)
    status <- httr2::resp_status(resp)
    .abort_asset_fetch("{where} answered HTTP {status} without a sessid; the session cannot be primed")
  }
  session$sessid <- m[[2]]
  invisible(session)
}
.khl_new_session <- function(site, timeout = 60) {
  session <- new.env(parent = emptyenv())
  session$site <- site
  session$cookies <- tempfile("khl_cookies_")
  .khl_prime(session, timeout)
  .khl_state$sessions[[site]] <- session
  session
}

.khl_str_form <- function(v) {
  if (is.numeric(v)) format(v, scientific = FALSE, trim = TRUE, digits = 15) else as.character(v)
}
# The POST form: NULL dropped, season / stage resolved to values[tournament].
.khl_form <- function(route, params) {
  form <- Filter(Negate(is.null), params)
  season <- form[["season"]]
  stage <- form[["stage"]]
  form[c("season", "stage")] <- NULL
  if (!is.null(season) && is.null(form[[.KHL_TOURNAMENT]])) {
    form[[.KHL_TOURNAMENT]] <- .khl_stage_for(season, stage)[["tournament_id"]]
  }
  if (route %in% .KHL_NEEDS_TOURNAMENT && is.null(form[[.KHL_TOURNAMENT]])) {
    .abort_bad_request("{route} needs a season (END year, with stage) or a tournament_id")
  }
  lapply(form, .khl_str_form)
}

.khl_dig <- function(obj, path) {
  for (step in path) {
    obj <- if (is.numeric(step)) {
      if (.khl_is_arr(obj) && length(obj) >= step) obj[[step]] else NULL
    } else if (.khl_is_obj(obj)) obj[[step]] else NULL
  }
  obj
}

# Raise on an error envelope, an empty answer, or one scoped to something not requested.
.khl_rest_check <- function(url, route, raw, form) {
  where <- .euro_where(url)
  if (!.khl_is_obj(raw) || !"status" %in% names(raw)) {
    .abort_asset_fetch("{where}: the body is not a khl.ru envelope")
  }
  errors <- raw[["errors"]]
  status <- as.character(raw[["status"]] %||% "")
  if (tolower(status) != "success" || .khl_truthy(errors)) {
    first <- if (.khl_is_arr(errors) && length(errors) && .khl_is_obj(errors[[1]])) errors[[1]] else list()
    detail <- paste(vapply(Filter(function(k) .khl_truthy(first[[k]]), c("code", "message")),
                           function(k) .khl_pystr(first[[k]]), character(1)), collapse = " ")
    detail <- substr(gsub("\\s+", " ", trimws(detail)), 1, 200)
    .abort_asset_fetch("{where}: khl.ru answered status='{status}': {detail}")
  }
  data <- raw[["data"]]
  if (!is.list(data) || !length(data)) .abort_no_data("{where}: khl.ru answered success with no data")
  rows <- if (route %in% names(.KHL_ROWS)) .khl_dig(data, .KHL_ROWS[[route]]) else data
  if (!is.list(rows) || !length(rows)) .abort_no_data("{where}: khl.ru answered success with no rows")
  page <- if (.khl_is_obj(data)) data else list()
  game <- form[["values[gameid]"]] %||% "None"
  tn <- form[[.KHL_TOURNAMENT]]
  if (route %in% c("/game/header/", "/game/text/")) {
    echo <- .khl_obj(.khl_dig(page, if (route == "/game/header/") list("game") else list("game", "info", "data")))
    got <- c(if (is.null(echo[["id"]])) "None" else .khl_pystr(echo[["id"]]),
             if (is.null(echo[["tnId"]])) "None" else .khl_pystr(echo[["tnId"]]))
    if (!identical(got, c(game, tn %||% "None"))) {
      want_tn <- tn %||% "None"
      .abort_asset_fetch("{where}: asked for game {game} of tournament {want_tn}, answered game {got[[1]]} of {got[[2]]}")
    }
  }
  if (route == "/players/stats/" && !identical(.khl_pystr(page[["id"]] %||% "None"), form[["values[id]"]])) {
    asked <- form[["values[id]"]] %||% "None"
    answered <- if (is.null(page[["id"]])) "None" else .khl_pystr(page[["id"]])
    .abort_asset_fetch("{where}: asked for player {asked}, answered player {answered}")
  }
  if (route == "/players/profile/") {
    link <- as.character(.khl_dig(page, list("DATA", "pagelink")) %||% "")
    if (!identical(paste0("/players/", form[["values[id]"]], "/"), link)) {
      asked <- form[["values[id]"]] %||% "None"
      answered <- if (nzchar(link)) link else "no player link"
      .abort_asset_fetch("{where}: asked for player {asked}, answered {answered}")
    }
  }
  if (route == "/calendar/list/" && !is.null(tn)) {
    ids <- character()
    for (week in .khl_arr(page[["GAMES"]])) {
      if (!.khl_is_obj(week)) next
      for (day in week) {
        if (!.khl_is_arr(day)) next
        for (g in day) if (.khl_is_obj(g)) ids <- c(ids, if (is.null(g[["tnId"]])) "None" else .khl_pystr(g[["tnId"]]))
      }
    }
    if (!length(ids)) .abort_no_data("{where}: khl.ru answered no games for tournament {tn}")
    if (mean(ids == tn) < 0.9) {
      .abort_asset_fetch("{where}: asked for tournament {tn}, but most of its games are of another tournament")
    }
  }
  invisible(raw)
}

# POST a khl.ru REST route (the form is `params`) within the primed session, then verify it.
.khl_get <- function(url, params = list(), headers = NULL, timeout = 60) {
  route <- sub("^/rest", "", httr2::url_parse(url)$path)
  form <- .khl_form(route, params)
  site <- sub("^([a-z]+://[^/]+).*$", "\\1", url)
  session <- .khl_state$sessions[[site]] %||% .khl_new_session(site, timeout)
  raw <- NULL
  for (attempt in 0:1) {  # one recovery: a re-prime after a 307 or after an invalid_csrf
    resp <- .build_request(url, headers = .khl_headers(site, headers), timeout = timeout) |>
      httr2::req_cookie_preserve(session$cookies) |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_body_form(!!!c(list(sessid = session$sessid), form)) |>
      .khl_send()
    if (httr2::resp_status(resp) == 307L) {
      if (attempt) {
        where <- .euro_where(url)
        .abort_asset_fetch("{where} answered 307 again after re-priming the session")
      }
      .khl_prime(session, timeout)
      next
    }
    raw <- .euro_resp_json(resp, simplifyVector = FALSE)
    stale <- .khl_is_obj(raw) && grepl("invalid_csrf", paste(unlist(raw[["errors"]]), collapse = " "), fixed = TRUE)
    if (!stale) break
    .khl_state$sessions[[site]] <- NULL  # the sessid expired: drop it, prime a fresh jar, retry once
    if (attempt) break
    session <- .khl_new_session(site, timeout)
  }
  .khl_rest_check(url, route, raw, form)
  .khl_strip(raw)
}

# ---- argument checks and the exported-wrapper tail --------------------------------

.khl_check_stage <- function(stage) {
  if (!is.character(stage) || length(stage) != 1 || !stage %in% .KHL_STAGES) {
    valid <- .KHL_STAGES
    .abort_bad_request("{.arg stage} must be one of {.val {valid}}, not {.val {stage}}.", call = NULL)
  }
}
.khl_check_locale <- function(locale) {
  if (!is.character(locale) || length(locale) != 1 || !locale %in% c("en", "ru")) {
    .abort_bad_request("{.arg locale} must be one of {.val {c('en', 'ru')}}, not {.val {locale}}.", call = NULL)
  }
}
.khl_need <- function(x, name) {
  if (is.null(x) || length(x) != 1 || is.na(x)) {
    .abort_bad_request("{.arg {name}} is required: a single id.", call = NULL)
  }
  invisible(x)
}
.khl_wrap <- function(df, what) make_fastRhockey_data(df, what, Sys.time())

# ---- exported wrappers: khl.ru REST (14) ----------------------------------------

#' @title **KHL Schedule**
#' @description Season schedule and results of a tournament, one row per game (All-Star games
#'   dropped).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/calendar/list/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). NULL = the site's
#'   current season.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (16 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name       |types     |description |
#'    |:--------------|:---------|:----------|
#'    |game_id        |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id  |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |date           |character |Game date YYYY-MM-DD (Moscow time) |
#'    |time           |character |Start time hh:mm (Moscow time); not a game clock |
#'    |home_club_id   |integer   |Home CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |home_name      |character |Home team name (Russian on www.khl.ru) |
#'    |away_club_id   |integer   |Visitor CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |away_name      |character |Visitor team name (Russian on www.khl.ru) |
#'    |home_score     |integer   |Home goals (final, shootout winner counted) |
#'    |away_score     |integer   |Visitor goals (final, shootout winner counted) |
#'    |decision       |character |OT or SO when decided in overtime / a shootout; null in regulation |
#'    |period_scores  |character |Per-period home:away scores, comma-joined |
#'    |winner_club_id |integer   |Club id of the winner |
#'    |approved       |integer   |1 = result confirmed by the league |
#'    |arena_id       |integer   |khl.ru arena id |
#'    |arena_name     |character |Arena (city) name, from data.ARENAS |
#' @examples
#' \donttest{
#'   try(khl_schedule(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_schedule <- function(season = NULL, stage = "regular", headers = NULL, tournament_id = NULL,
                         return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/calendar/list/"),
                  params = list(`season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_schedule(raw),
            "KHL Schedule from khl.ru")
}

#' @title **KHL Roster**
#' @description Club roster (current season only): one row per player.
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/clubs/team/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param club_id Required. khl.ru CLUB id (7 = Spartak; schedule home_club_id), not a team-season
#'   id.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (10 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name  |types     |description |
#'    |:---------|:---------|:----------|
#'    |group     |character |List the row comes from (position group / roster section) |
#'    |player_id |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |photo     |character |Player photo URL on khl.ru (protocol-relative) |
#'    |country   |character |Citizenship (Russian) |
#'    |lastname  |character |Last name in Russian |
#'    |firstname |character |First name in Russian |
#'    |jn        |integer   |Jersey number (not an id; 0 on a team event) |
#'    |age       |integer   |Player age in years |
#'    |favorite  |logical   |Front-end favourite flag of the signed-in user (always False here) |
#'    |link      |character |Relative khl.ru page path |
#' @examples
#' \donttest{
#'   try(khl_roster(club_id = 7))
#' }
#' @family KHL Functions
#' @export
khl_roster <- function(club_id, headers = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(club_id, "club_id")
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/clubs/team/"),
                  params = list(`page` = "team",
                                `values[club_id]` = club_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_roster(raw),
            "KHL Roster from khl.ru")
}

#' @title **KHL Stat Filters**
#' @description Stats filter options: seasons, tournaments, position groups and clubs (one row per
#'   option).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/filters/statplayers/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). NULL = the site's
#'   current season.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (6 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name    |types     |description |
#'    |:-----------|:---------|:----------|
#'    |filter      |character |Filter code: season, tournament, amplua_full or clubs_full |
#'    |filter_name |character |Filter label as the site prints it |
#'    |value       |character |Option code to send for the filter (a tournament id, a season code such as s19, a club slug) |
#'    |name        |character |Option label (Russian) |
#'    |def         |integer   |Default tournament id (tnId) of a season option |
#'    |active      |integer   |1 when the option is selectable |
#' @examples
#' \donttest{
#'   try(khl_stat_filters(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_stat_filters <- function(season = NULL, stage = "regular", headers = NULL, tournament_id = NULL,
                             return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/filters/statplayers/"),
                  params = list(`season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_stat_filters(raw),
            "KHL Stat Filters from khl.ru")
}

#' @title **KHL Game Header**
#' @description Game header: status, teams, score, arena (one row).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/game/header/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param game_id Required. khl.ru game id (the schedule's game_id; equals webcaster event khl_id).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). The game's season;
#'   required unless tournament_id is given.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (47 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name            |types     |description |
#'    |:-------------------|:---------|:----------|
#'    |game_id             |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id       |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |home_club_id        |integer   |Home CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |home_team_season_id |integer   |Home team-season id (one per tournament; joins nothing in the webcaster API) |
#'    |away_club_id        |integer   |Visitor CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |away_team_season_id |integer   |Visitor team-season id (one per tournament; joins nothing in the webcaster API) |
#'    |arena_id            |integer   |khl.ru arena id |
#'    |home_score          |integer   |Home goals (final, shootout winner counted) |
#'    |away_score          |integer   |Visitor goals (final, shootout winner counted) |
#'    |round               |integer   |Playoff round number |
#'    |roundname           |character |Round label in Russian ('Неделя 29' = Week 29) |
#'    |roundname_en        |character |Round label in English |
#'    |date                |character |Game date YYYY-MM-DD (Moscow time) |
#'    |dayofweek           |integer   |Day of week, 1 = Sunday (6 = Friday 2026-03-20) |
#'    |time                |character |Start time hh:mm:ss (Moscow time); not a game clock |
#'    |changes             |integer   |Count of changes to the game record (0 in the captured bodies; unverified) |
#'    |number              |integer   |Game number within the tournament |
#'    |home_name_l3        |character |Home club 3-letter code in Russian |
#'    |home_name_en        |character |Home club name in English |
#'    |home_name_l3_en     |character |Home club 3-letter code in English |
#'    |home_city_en        |character |Home club city in English |
#'    |visitor_name_l3     |character |Visiting club 3-letter code in Russian |
#'    |visitor_name_en     |character |Visiting club name in English |
#'    |visitor_name_l3_en  |character |Visiting club 3-letter code in English |
#'    |visitor_city_en     |character |Visiting club city in English |
#'    |arena               |character |Arena name in Russian |
#'    |arena_en            |character |Arena name in English |
#'    |arena_city          |character |Arena city in Russian |
#'    |arena_city_en       |character |Arena city in English |
#'    |msktime_sft         |integer   |Offset of local time from Moscow time, hours (unverified) |
#'    |gmttime_sft         |integer   |Offset of Moscow time from GMT, hours (unverified) |
#'    |temp                |character |Undocumented provider field `temp`; carried as sent. |
#'    |score               |character |Final score 'home:visitor' |
#'    |ots                 |character |How the game ended: empty = regulation, OT = overtime, SO = shootout |
#'    |attendance          |integer   |Attendance (numeric string) |
#'    |approved            |integer   |1 = result confirmed by the league |
#'    |format_date_day     |character |Game date YYYY-MM-DD (Moscow time), as the site formats it |
#'    |time_format         |character |Start time hh:mm (Moscow time) |
#'    |full_date           |integer   |Start time, unix seconds (UTC) |
#'    |date_type           |character |Front-end date bucket of the game relative to today ('last' = already played; other values unverified) |
#'    |type                |character |Game state/type code (sogame = finished in a shootout) |
#'    |winner              |character |Winning side: A = home, B = visitor |
#'    |visible_date        |character |Short display date in Russian ('20 мар' = 20 March) |
#'    |visible_date_full   |character |Display date in Russian without the year ('20 марта') |
#'    |visible_date_full2  |character |Display date in Russian with the year ('20 марта 2026') |
#'    |visible_date_day    |character |Abbreviated weekday in Russian ('пт' = Friday) |
#'    |win                 |integer   |True when this club won |
#' @examples
#' \donttest{
#'   try(khl_game_header(game_id = 898236, season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_game_header <- function(game_id, season = NULL, stage = "regular", headers = NULL,
                            tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(game_id, "game_id")
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/game/header/"),
                  params = list(`page` = "resume",
                                `values[gameid]` = game_id,
                                `season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_game_header(raw),
            "KHL Game Header from khl.ru")
}

#' @title **KHL Game Protocol**
#' @description Game protocol: one row per goal with scorer, assistants, strength and the jerseys on
#'   the ice.
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/game/protocol/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param game_id Required. khl.ru game id (the schedule's game_id; equals webcaster event khl_id).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). The game's season;
#'   required unless tournament_id is given.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (21 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name           |types     |description |
#'    |:------------------|:---------|:----------|
#'    |game_id            |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id      |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |period             |integer   |Period number (box score: 0 = the whole game; 4+ = overtime) |
#'    |goal_number        |integer   |Goal number in the game |
#'    |time               |character |Game clock mm:ss (game-elapsed); period marker rows keep their wall-clock HH:MM |
#'    |team_side          |character |A = home, B = visitor |
#'    |score_a            |integer   |Home goals after the goal |
#'    |score_b            |integer   |Visitor goals after the goal |
#'    |strength           |character |Strength code as the site prints it (Russian: РАВ even, БОЛ power play, МЕН short-handed) |
#'    |strength_text      |character |Strength label as the site prints it |
#'    |scorer_player_id   |integer   |Scorer's khl.ru player id |
#'    |scorer_name        |character |Scorer's name with the season count in brackets |
#'    |scorer_jn          |integer   |Scorer's jersey |
#'    |assist_1_player_id |integer   |First assistant's khl.ru player id |
#'    |assist_1_name      |character |First assistant's name with the season count in brackets |
#'    |assist_1_jn        |integer   |First assistant's jersey |
#'    |assist_2_player_id |integer   |Second assistant's khl.ru player id |
#'    |assist_2_name      |character |Second assistant's name with the season count in brackets |
#'    |assist_2_jn        |integer   |Second assistant's jersey |
#'    |on_ice_a           |character |Home jerseys on the ice, comma-separated |
#'    |on_ice_b           |character |Visitor jerseys on the ice, comma-separated |
#' @examples
#' \donttest{
#'   try(khl_game_protocol(game_id = 898236, season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_game_protocol <- function(game_id, season = NULL, stage = "regular", headers = NULL,
                              tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(game_id, "game_id")
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/game/protocol/"),
                  params = list(`values[gameid]` = game_id,
                                `season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_game_protocol(raw),
            "KHL Game Protocol from khl.ru")
}

#' @title **KHL Game Info**
#' @description Game info: teams, score, arena, officials, starting goalies (one row).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/game/text/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param game_id Required. khl.ru game id (the schedule's game_id; equals webcaster event khl_id).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). The game's season;
#'   required unless tournament_id is given.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (71 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name                 |types     |description |
#'    |:------------------------|:---------|:----------|
#'    |game_id                  |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id            |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |home_club_id             |integer   |Home CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |home_team_season_id      |integer   |Home team-season id (one per tournament; joins nothing in the webcaster API) |
#'    |away_club_id             |integer   |Visitor CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |away_team_season_id      |integer   |Visitor team-season id (one per tournament; joins nothing in the webcaster API) |
#'    |arena_id                 |integer   |khl.ru arena id |
#'    |home_score               |integer   |Home goals (final, shootout winner counted) |
#'    |away_score               |integer   |Visitor goals (final, shootout winner counted) |
#'    |round                    |integer   |Playoff round number |
#'    |roundname                |character |Round label in Russian ('Неделя 29' = Week 29) |
#'    |roundname_en             |character |Round label in English |
#'    |date                     |character |Game date YYYY-MM-DD (Moscow time) |
#'    |dayofweek                |integer   |Day of week, 1 = Sunday (6 = Friday 2026-03-20) |
#'    |time                     |character |Start time hh:mm:ss (Moscow time); not a game clock |
#'    |changes                  |integer   |Count of changes to the game record (0 in the captured bodies; unverified) |
#'    |number                   |integer   |Game number within the tournament |
#'    |home_name_l3             |character |Home club 3-letter code in Russian |
#'    |home_name_en             |character |Home club name in English |
#'    |home_name_l3_en          |character |Home club 3-letter code in English |
#'    |home_city_en             |character |Home club city in English |
#'    |visitor_name_l3          |character |Visiting club 3-letter code in Russian |
#'    |visitor_name_en          |character |Visiting club name in English |
#'    |visitor_name_l3_en       |character |Visiting club 3-letter code in English |
#'    |visitor_city_en          |character |Visiting club city in English |
#'    |arena                    |character |Arena name in Russian |
#'    |arena_en                 |character |Arena name in English |
#'    |arena_city               |character |Arena city in Russian |
#'    |arena_city_en            |character |Arena city in English |
#'    |msktime_sft              |integer   |Offset of local time from Moscow time, hours (unverified) |
#'    |gmttime_sft              |integer   |Offset of Moscow time from GMT, hours (unverified) |
#'    |temp                     |character |Undocumented provider field `temp`; carried as sent. |
#'    |score                    |character |Final score 'home:visitor' |
#'    |ots                      |character |How the game ended: empty = regulation, OT = overtime, SO = shootout |
#'    |sc_p1                    |character |1st-period score 'home:visitor' |
#'    |sc_p2                    |character |2nd-period score 'home:visitor' |
#'    |sc_p3                    |character |3rd-period score 'home:visitor' |
#'    |sc_p4                    |character |Overtime or shootout score 'home:visitor' |
#'    |attendance               |integer   |Attendance (numeric string) |
#'    |approved                 |integer   |1 = result confirmed by the league |
#'    |format_date_day          |character |Game date YYYY-MM-DD (Moscow time), as the site formats it |
#'    |time_format              |character |Start time hh:mm (Moscow time) |
#'    |full_date                |integer   |Start time, unix seconds (UTC) |
#'    |date_type                |character |Front-end date bucket of the game relative to today ('last' = already played; other values unverified) |
#'    |type                     |character |Game state/type code (sogame = finished in a shootout) |
#'    |winner                   |character |Winning side: A = home, B = visitor |
#'    |visible_date             |character |Short display date in Russian ('20 мар' = 20 March) |
#'    |visible_date_full        |character |Display date in Russian without the year ('20 марта') |
#'    |visible_date_full2       |character |Display date in Russian with the year ('20 марта 2026') |
#'    |visible_date_day         |character |Abbreviated weekday in Russian ('пт' = Friday) |
#'    |statusvalue              |character |Game status in Russian (матч завершен = match finished) |
#'    |pernum                   |integer   |Number of regulation periods played |
#'    |numlines                 |integer   |Number of lines on the line-up card |
#'    |gk_a_jn                  |integer   |Home starting goalie's JERSEY number (gameItem.gkA), not an id |
#'    |gk_b_jn                  |integer   |Visitor starting goalie's JERSEY number (gameItem.gkB), not an id |
#'    |is_events_have_multi_ovt |logical   |True when the game has more than one overtime period (playoff) |
#'    |idmainref1               |integer   |Referee 1: khl.ru referee id |
#'    |idmainref2               |integer   |Referee 2: khl.ru referee id |
#'    |idlineref1               |integer   |Linesman 1: khl.ru referee id |
#'    |idlineref2               |integer   |Linesman 2: khl.ru referee id |
#'    |jnmainref1               |character |Referee 1 sweater number |
#'    |jnmainref2               |character |Referee 2 sweater number |
#'    |jnlineref1               |character |Linesman 1 sweater number |
#'    |jnlineref2               |character |Linesman 2 sweater number |
#'    |refm1                    |character |Referee 1, name in Russian |
#'    |refm2                    |character |Referee 2, name in Russian |
#'    |refl1                    |character |Linesman 1, name in Russian |
#'    |refl2                    |character |Linesman 2, name in Russian |
#'    |status                   |integer   |Game status code (90 = finished; other codes UNVERIFIED) |
#'    |gk_a_player_id           |integer   |khl.ru player id of the home starting goalie (gk_a_jn resolved through players) |
#'    |gk_b_player_id           |integer   |khl.ru player id of the visitor starting goalie (gk_b_jn resolved through players) |
#' @examples
#' \donttest{
#'   try(khl_game_info(game_id = 898236, season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_game_info <- function(game_id, season = NULL, stage = "regular", headers = NULL,
                          tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(game_id, "game_id")
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/game/text/"),
                  params = list(`values[gameid]` = game_id,
                                `season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_game_info(raw),
            "KHL Game Info from khl.ru")
}

#' @title **KHL Player Box Score**
#' @description Skater box score by period: one row per period, side and jersey, with the player id.
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/game/text/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param game_id Required. khl.ru game id (the schedule's game_id; equals webcaster event khl_id).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). The game's season;
#'   required unless tournament_id is given.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (30 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name      |types     |description |
#'    |:-------------|:---------|:----------|
#'    |game_id       |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |period        |integer   |Period number (box score: 0 = the whole game; 4+ = overtime) |
#'    |team_side     |character |A = home, B = visitor |
#'    |jn            |integer   |Jersey number (not an id; 0 on a team event) |
#'    |player_id     |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |player_name   |character |Player name as the site prints it (Russian on www.khl.ru) |
#'    |position      |character |Position code (Russian letter on www.khl.ru) |
#'    |g             |integer   |Goals scored in the period block |
#'    |a             |integer   |Home roster keyed by jersey number |
#'    |p             |character |Points (empty string when none) |
#'    |shots         |integer   |Shots on goal in the period block |
#'    |pim           |integer   |Penalty minutes |
#'    |fo            |integer   |Faceoffs taken in the period block |
#'    |fow           |integer   |Faceoffs won in the period block |
#'    |fow_pct       |numeric   |Stat code the bar is computed from |
#'    |bls           |integer   |Shots blocked in the period block |
#'    |hits          |integer   |Hits (body checks) |
#'    |fa            |integer   |Takeaways (the site labels this column 'Отборы', takeaways) |
#'    |p_intc        |integer   |Pass interceptions |
#'    |sft           |character |Time on ice 'mm:ss(shifts)' |
#'    |sfte          |character |Even-strength time on ice 'mm:ss(shifts)' |
#'    |sftpp         |character |Power-play time on ice 'mm:ss(shifts)' |
#'    |sftsh         |character |Short-handed time on ice 'mm:ss(shifts)' |
#'    |sften         |character |Empty-net time on ice 'mm:ss(shifts)' |
#'    |tiams         |character |Time in the attacking zone (mm:ss) |
#'    |ts            |numeric   |Top speed, km/h (tracking; present by 2019/20) |
#'    |dt            |numeric   |Distance skated, km (tracking) |
#'    |as            |numeric   |Average speed, km/h (tracking) |
#'    |pcd           |numeric   |Puck-control duration (tracking; unit unverified) |
#' @examples
#' \donttest{
#'   try(khl_player_box(game_id = 898236, season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_player_box <- function(game_id, season = NULL, stage = "regular", headers = NULL,
                           tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(game_id, "game_id")
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/game/text/"),
                  params = list(`values[gameid]` = game_id,
                                `season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_player_box(raw),
            "KHL Player Box Score from khl.ru")
}

#' @title **KHL Play-by-Play**
#' @description Play-by-play: shots, faceoffs, hits, blocks, giveaways, takeaways, penalties and
#'   goals (2008-09 on).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/game/text/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param game_id Required. khl.ru game id (the schedule's game_id; equals webcaster event khl_id).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). The game's season;
#'   required unless tournament_id is given.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (26 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name          |types     |description |
#'    |:-----------------|:---------|:----------|
#'    |game_id           |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id     |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |period            |integer   |Period number (box score: 0 = the whole game; 4+ = overtime) |
#'    |event_index       |integer   |0-based order of the event in the game, oldest first |
#'    |event_type        |character |period (start / end marker), faceoff, shot, block, action, penalty or goal |
#'    |time              |character |Game clock mm:ss (game-elapsed); period marker rows keep their wall-clock HH:MM |
#'    |time_seconds      |integer   |Game-elapsed seconds (null on period marker rows) |
#'    |team_side         |character |A = home, B = visitor |
#'    |jn                |integer   |Jersey number (not an id; 0 on a team event) |
#'    |player_id         |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |result            |character |Event result code (shots: goal / saved / miss; actions: hit, gva = giveaway, tka = takeaway, intc = interception) |
#'    |result_text       |character |Event result label |
#'    |penalty_minutes   |integer   |Penalty minutes (penalty events) |
#'    |score_a           |integer   |Home goals after the goal |
#'    |score_b           |integer   |Visitor goals after the goal |
#'    |won_side          |character |Faceoff winner's side (A / B) |
#'    |won_jn            |integer   |Faceoff winner's jersey |
#'    |won_player_id     |integer   |Faceoff winner's khl.ru player id |
#'    |lost_side         |character |Faceoff loser's side (A / B) |
#'    |lost_jn           |integer   |Faceoff loser's jersey |
#'    |lost_player_id    |integer   |Faceoff loser's khl.ru player id |
#'    |won_zone          |character |Zone of the faceoff from the winner's view: offensive, neutral or defensive |
#'    |shooter_side      |character |Blocked shooter's side (A / B) |
#'    |shooter_jn        |integer   |Blocked shooter's jersey |
#'    |shooter_player_id |integer   |Blocked shooter's khl.ru player id |
#'    |period_action     |character |Period marker: 0 = start, 1 = end (inferred) |
#' @examples
#' \donttest{
#'   try(khl_pbp(game_id = 898236, season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_pbp <- function(game_id, season = NULL, stage = "regular", headers = NULL,
                    tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(game_id, "game_id")
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/game/text/"),
                  params = list(`values[gameid]` = game_id,
                                `season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_pbp(raw),
            "KHL Play-by-Play from khl.ru")
}

#' @title **KHL Shot Map**
#' @description Shot map: one row per shot with its x / y on the 1000 x 500 rink grid (2024-25 on).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/game/text/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param game_id Required. khl.ru game id (the schedule's game_id; equals webcaster event khl_id).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). The game's season;
#'   required unless tournament_id is given.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (15 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name      |types     |description |
#'    |:-------------|:---------|:----------|
#'    |game_id       |integer   |khl.ru game id; equals webcaster event khl_id / match_id |
#'    |tournament_id |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |period        |integer   |Period number (box score: 0 = the whole game; 4+ = overtime) |
#'    |time          |character |Game clock mm:ss (game-elapsed); period marker rows keep their wall-clock HH:MM |
#'    |time_seconds  |integer   |Game-elapsed seconds (null on period marker rows) |
#'    |team_side     |character |A = home, B = visitor |
#'    |jn            |integer   |Jersey number (not an id; 0 on a team event) |
#'    |player_id     |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |shot_type     |integer   |1 = goal, 2 = saved (inferred upstream; unverified) |
#'    |x             |integer   |Shot x on the site's 1000-wide rink grid (indent_left = x/10 %); whether shots are normalised to one attacking direction is UNVERIFIED |
#'    |y             |integer   |Shot y on the site's 500-high rink grid (indent_top = y/5 %); orientation UNVERIFIED |
#'    |indent_left   |numeric   |Shot position as percent from the left edge |
#'    |indent_top    |numeric   |Shot position as percent from the top edge |
#'    |indent_right  |numeric   |Shot position as percent from the right edge |
#'    |indent_bottom |numeric   |Shot position as percent from the bottom edge |
#' @examples
#' \donttest{
#'   try(khl_shots(game_id = 898236, season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_shots <- function(game_id, season = NULL, stage = "regular", headers = NULL,
                      tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(game_id, "game_id")
  .khl_check_stage(stage)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/game/text/"),
                  params = list(`values[gameid]` = game_id,
                                `season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_shots(raw),
            "KHL Shot Map from khl.ru")
}

#' @title **KHL Player Profile**
#' @description Player profile: bio, contract, club (one row).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/players/profile/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param player_id Required. khl.ru player id (the box score / roster player_id).
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (31 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name           |types     |description |
#'    |:------------------|:---------|:----------|
#'    |player_id          |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |firstname          |character |First name in Russian |
#'    |lastname           |character |Last name in Russian |
#'    |photo              |character |Player photo URL on khl.ru (protocol-relative) |
#'    |position           |character |Position code (Russian letter on www.khl.ru) |
#'    |position_code      |character |Position key in English (goaltender, defender, forward) |
#'    |biography          |character |Biography text in Russian (HTML) |
#'    |is_hero            |logical   |Site 'hero' flag (unverified meaning) |
#'    |is_rip             |logical   |True for a deceased player |
#'    |links_stats        |character |Relative khl.ru page path |
#'    |pagelink           |character |Relative khl.ru page path |
#'    |summary_stats_gp   |integer   |Career KHL games played (profile summary) |
#'    |summary_stats_a    |integer   |Career KHL assists (profile summary) |
#'    |summary_stats_pm   |integer   |Career KHL plus-minus (profile summary) |
#'    |summary_stats_g    |integer   |Career KHL goals (profile summary) |
#'    |summary_stats_pts  |integer   |Career KHL points (profile summary) |
#'    |jn                 |integer   |Jersey number (not an id; 0 on a team event) |
#'    |club_name          |character |Name in Russian (Cyrillic); the *_en / *en sibling holds the English transliteration |
#'    |club_logo          |character |Current club logo URL on khl.ru (protocol-relative) |
#'    |club_code          |character |Club slug (/clubs/<slug>/) |
#'    |height             |character |Height with its Russian unit ('183 см' = 183 cm) |
#'    |weight             |character |Weight with its Russian unit ('85 кг' = 85 kg) |
#'    |birthdate          |character |Birth date (Russian display format) |
#'    |age                |character |Age with its Russian unit ('27 лет' = 27 years) |
#'    |contract_name      |character |Contract label in Russian |
#'    |contract_date      |character |Contract end date |
#'    |country_name       |character |Name in Russian (Cyrillic); the *_en / *en sibling holds the English transliteration |
#'    |country_flag       |character |Flag image path of the player's country (citizenship) |
#'    |sport_country_name |character |Name in Russian (Cyrillic); the *_en / *en sibling holds the English transliteration |
#'    |sport_country_flag |character |Flag image path of the country the player represents in sport |
#'    |stick              |character |Shooting hand: l = left, r = right |
#' @examples
#' \donttest{
#'   try(khl_player_profile(player_id = 41928))
#' }
#' @family KHL Functions
#' @export
khl_player_profile <- function(player_id, headers = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(player_id, "player_id")
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/players/profile/"),
                  params = list(`values[id]` = player_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_player_profile(raw),
            "KHL Player Profile from khl.ru")
}

#' @title **KHL Player Career**
#' @description Player career: one row per season tournament.
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/players/stats/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param player_id Required. khl.ru player id (the box score / roster player_id).
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (40 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name       |types     |description |
#'    |:--------------|:---------|:----------|
#'    |player_id      |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |season_label   |character |Season label as sent (26/27, 2025/2026) |
#'    |tournament_id  |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |team_season_id |integer   |Team-season id (tid; one per tournament; joins nothing in the webcaster API) |
#'    |club_id        |integer   |khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |team_name      |character |Team name as the site prints it |
#'    |jn             |integer   |Jersey number (not an id; 0 on a team event) |
#'    |gp             |integer   |Games played by the player |
#'    |g              |integer   |Goals scored by the player |
#'    |a              |integer   |Assists credited to the player |
#'    |pts            |integer   |Points (goals plus assists) |
#'    |pm             |integer   |Plus-minus rating of the player |
#'    |plus           |integer   |Plus (on ice for a goal for) |
#'    |minus          |integer   |Minus (on ice for a goal against) |
#'    |pim            |integer   |Penalty minutes |
#'    |eqg            |integer   |Even-strength goals |
#'    |ppg            |integer   |Power-play goals |
#'    |shg            |integer   |Short-handed goals |
#'    |otg            |integer   |Overtime goals scored by the player |
#'    |gwg            |integer   |Game-winning goals |
#'    |sds            |integer   |Shootout-deciding goals |
#'    |sog            |integer   |Shots on goal by the player |
#'    |sog_pct        |numeric   |Shooting percentage |
#'    |sog_avg        |numeric   |Shots on goal per game |
#'    |fo             |integer   |Faceoffs taken by the player |
#'    |fow            |integer   |Faceoffs won by the player |
#'    |fo_pct         |numeric   |Faceoff win percentage |
#'    |toi_avg        |character |Average time on ice per game (mm:ss) |
#'    |sft_avg        |numeric   |Average shifts per game |
#'    |tie_avg        |character |Average even-strength time on ice per game |
#'    |sfte_avg       |numeric   |Average even-strength shifts per game |
#'    |tipp_avg       |character |Average power-play time on ice per game |
#'    |sftpp_avg      |numeric   |Average power-play shifts per game |
#'    |tish_avg       |character |Average short-handed time on ice per game |
#'    |sftsh_avg      |numeric   |Average short-handed shifts per game |
#'    |hits           |integer   |Hits (body checks) |
#'    |bls            |integer   |Shots blocked by the player |
#'    |foa            |integer   |Faceoffs lost by the player |
#'    |tka            |integer   |Takeaways by the player |
#'    |p_intc         |integer   |Pass interceptions |
#' @examples
#' \donttest{
#'   try(khl_player_career(player_id = 41928))
#' }
#' @family KHL Functions
#' @export
khl_player_career <- function(player_id, headers = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(player_id, "player_id")
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/players/stats/"),
                  params = list(`values[id]` = player_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_player_career(raw),
            "KHL Player Career from khl.ru")
}

#' @title **KHL Playoff Bracket**
#' @description Playoff bracket: one row per team per series.
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/standings/playoff/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). Required unless
#'   tournament_id is given.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (9 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name        |types     |description |
#'    |:---------------|:---------|:----------|
#'    |round           |integer   |Playoff round number |
#'    |wins_to_advance |integer   |Wins needed to win a series of the round |
#'    |pair            |integer   |Series number within the round |
#'    |conference      |character |Conference (west / east) |
#'    |seed            |integer   |Conference seed |
#'    |club_id         |integer   |khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |name            |character |Name in Russian (Cyrillic); the *_en / *en sibling holds the English transliteration |
#'    |series_wins     |integer   |Wins in the series so far |
#'    |game_ids        |character |khl.ru game ids of the series, comma-joined |
#' @examples
#' \donttest{
#'   try(khl_playoff_bracket(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_playoff_bracket <- function(season = NULL, headers = NULL, tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/standings/playoff/"),
                  params = list(`stage` = "playoff",
                                `values[type]` = "playoff",
                                `season` = season,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_playoff_bracket(raw),
            "KHL Playoff Bracket from khl.ru")
}

#' @title **KHL Standings**
#' @description Regular-season standings: one row per team per table (league, conferences,
#'   divisions).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/standings/regular/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). NULL = the site's
#'   current season.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (17 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name       |types     |description |
#'    |:--------------|:---------|:----------|
#'    |table          |character |Standings table: league, a conference (west / east) or a division key |
#'    |rank           |integer   |1-based position in its table / list |
#'    |club_id        |integer   |khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
#'    |name           |character |Name in Russian (Cyrillic); the *_en / *en sibling holds the English transliteration |
#'    |gp             |integer   |Games played (gp = w + otw + sow + sol + otl + l) |
#'    |w              |integer   |Regulation wins only: overtime (otw) and shootout (sow) wins are separate columns |
#'    |otw            |integer   |Overtime wins (not counted in w) |
#'    |sow            |integer   |Shootout wins (not counted in w) |
#'    |sol            |integer   |Shootout losses |
#'    |otl            |integer   |Overtime losses |
#'    |otl_en         |integer   |Overtime losses with the net empty |
#'    |l              |integer   |Regulation losses only: overtime (otl) and shootout (sol) losses are separate columns |
#'    |pts            |integer   |Points: 2 per win of any kind, 1 per overtime or shootout loss (3-2-1 in early seasons such as 2013-14) |
#'    |goals_for      |integer   |Goals scored by the team |
#'    |goals_against  |integer   |Goals allowed by the team |
#'    |is_out_playoff |logical   |Eliminated from playoff contention |
#'    |link           |character |Relative khl.ru page path |
#' @examples
#' \donttest{
#'   try(khl_standings(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_standings <- function(season = NULL, headers = NULL, tournament_id = NULL, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/standings/regular/"),
                  params = list(`values[type]` = "regular",
                                `season` = season,
                                `values[tournament]` = tournament_id),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_standings(raw),
            "KHL Standings from khl.ru")
}

#' @title **KHL Player Stats**
#' @description League player stats of a tournament, one page (one row per player).
#' @details
#' Endpoint: `POST https://www.khl.ru/rest/stat/players/`.
#'
#' khl.ru REST needs a primed session (a GET of the site root sets the cookie jar and the Bitrix
#' `sessid`, then each call POSTs a form); this is done for you, one session per host per R session,
#' re-primed when the site rotates its cookie or the `sessid` expires. No route takes a year:
#' `season` (the END year, 2026 = 2025/26) and `stage` are mapped to the tournament id (tnId)
#' through the webcaster season table, or pass `tournament_id` (the All-Star games have their own
#' tnId).
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL tournament id (tnId)
#'   through the webcaster season table (khl_webcaster_stages). NULL = the site's
#'   current season.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param position Position group: goalies, defenses, forwards, skaters or all. NULL = the site's
#'   default. One of `goalies`, `defenses`, `forwards`, `skaters`, `all`.
#' @param club Club slug (khl_stat_filters, filter clubs_full) or all. NULL = every club.
#' @param page 1-based page. Loop until a page comes back shorter than per_page.
#' @param headers Named list of extra request headers, merged over the defaults (`X-Requested-With`,
#'   `Origin`, `Referer`). The cookie jar and `sessid` are kept in-process, so nothing
#'   needs reusing across calls.
#' @param tournament_id khl.ru tournament id (tnId), sent as is instead of season / stage (1369 =
#'   2025-26 regular season, 1370 its playoff; the schedule's tournament_id
#'   column). The All-Star games have their own.
#' @param per_page Rows per page: 12, 20, 24 or 36. Keep 20: the site computes page counts and ranks
#'   for 20.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw khl.ru envelope as a list (`status`, `data`,
#'   `errors`; betting fields removed).
#' @return A `fastRhockey_data` tibble (36 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name    |types     |description |
#'    |:-----------|:---------|:----------|
#'    |group       |character |List the row comes from (position group / roster section) |
#'    |rank        |integer   |1-based position in its table / list |
#'    |player_id   |integer   |khl.ru player id; equals webcaster player khl_id |
#'    |player_name |character |Player name as the site prints it (Russian on www.khl.ru) |
#'    |image       |character |Player photo URL on khl.ru (team-season specific) |
#'    |jn          |integer   |Jersey number (not an id; 0 on a team event) |
#'    |gp          |integer   |Games played by the player |
#'    |g           |integer   |Goals scored by the player |
#'    |a           |integer   |Assists credited to the player |
#'    |pts         |integer   |Points (goals plus assists) |
#'    |pm          |integer   |Plus-minus rating of the player |
#'    |plus        |integer   |Plus (on ice for a goal for) |
#'    |minus       |integer   |Minus (on ice for a goal against) |
#'    |pim         |integer   |Penalty minutes |
#'    |eqg         |integer   |Even-strength goals |
#'    |ppg         |integer   |Power-play goals |
#'    |shg         |integer   |Short-handed goals |
#'    |otg         |integer   |Overtime goals scored by the player |
#'    |gwg         |integer   |Game-winning goals |
#'    |sds         |integer   |Shootout-deciding goals |
#'    |sog         |integer   |Shots on goal by the player |
#'    |sog_pct     |numeric   |Shooting percentage |
#'    |sog_avg     |numeric   |Shots on goal per game |
#'    |fo          |integer   |Faceoffs taken by the player |
#'    |fow         |integer   |Faceoffs won by the player |
#'    |fo_pct      |numeric   |Faceoff win percentage |
#'    |toi_avg     |character |Average time on ice per game (mm:ss) |
#'    |sft_avg     |numeric   |Average shifts per game |
#'    |tie_avg     |character |Average even-strength time on ice per game |
#'    |sfte_avg    |numeric   |Average even-strength shifts per game |
#'    |tipp_avg    |character |Average power-play time on ice per game |
#'    |sftpp_avg   |numeric   |Average power-play shifts per game |
#'    |tish_avg    |character |Average short-handed time on ice per game |
#'    |sftsh_avg   |numeric   |Average short-handed shifts per game |
#'    |hits        |integer   |Hits (body checks) |
#'    |bls         |integer   |Shots blocked by the player |
#' @examples
#' \donttest{
#'   try(khl_player_stats(position = "forwards", season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_player_stats <- function(season = NULL, stage = "regular", position = NULL, club = NULL, page = 1,
                             headers = NULL, tournament_id = NULL, per_page = 20, return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  if (!is.null(position) && !position %in% c("goalies", "defenses", "forwards", "skaters", "all")) {
    .abort_bad_request("{.arg position} must be one of {.val {c('goalies', 'defenses', 'forwards', 'skaters', 'all')}}, not {.val {position}}.")
  }
  raw <- .khl_get(paste0(.KHL_SITE, "/rest/stat/players/"),
                  params = list(`season` = season,
                                `stage` = stage,
                                `values[tournament]` = tournament_id,
                                `values[amplua_full]` = position,
                                `values[clubs_full]` = club,
                                `nav[page]` = page,
                                `nav[count]` = per_page),
                  headers = headers)
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_player_stats(raw),
            "KHL Player Stats from khl.ru")
}

# ---- exported wrappers: webcaster (9) -------------------------------------------

#' @title **KHL Webcaster Stages**
#' @description Season table: every stage (season END year, regular / playoff) with its khl.ru
#'   tournament id and webcaster stage id.
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/data.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (8 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name              |types     |description |
#'    |:---------------------|:---------|:----------|
#'    |season                |integer   |Season END year (2026 = 2025-26) |
#'    |type                  |character |regular or playoff |
#'    |tournament_id         |integer   |khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
#'    |stage_id              |integer   |Webcaster stage id (its own id space; not a khl.ru tournament id) |
#'    |season_label          |character |Season label as sent (26/27, 2025/2026) |
#'    |title                 |character |Stage title (Regular / Playoff) |
#'    |display_rule          |character |Front-end display rule key |
#'    |percent_points_scored |logical   |Front-end flag: rank the stage by points percentage (False in every stage captured; unverified) |
#' @examples
#' \donttest{
#'   try(khl_webcaster_stages())
#' }
#' @family KHL Functions
#' @export
khl_webcaster_stages <- function(locale = "en", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("data.json",
                     list(`locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_stages(raw),
            "KHL Webcaster Stages from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Event**
#' @description One game: officials, team totals, per-player match stats, goals, penalties (one row;
#'   lists as JSON text).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/event_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param event_id Required. Webcaster event id (khl_webcaster_events id), not the khl.ru game id.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (159 columns; zero rows with the same columns when the
#'   payload is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name                                   |types     |description |
#'    |:------------------------------------------|:---------|:----------|
#'    |game_state_key                             |character |Game state key (finished, ...) |
#'    |period                                     |integer   |Period number (box score: 0 = the whole game; 4+ = overtime) |
#'    |hd                                         |logical   |True when the broadcast is in HD |
#'    |id                                         |integer   |Webcaster id of the `arena` object (webcaster key space); no khl.ru counterpart in the body |
#'    |season                                     |character |Season END year (2026 = 2025-26) |
#'    |stage_id                                   |integer   |Webcaster stage id (its own id space; not a khl.ru tournament id) |
#'    |type_id                                    |integer   |Webcaster event type code |
#'    |commentator                                |logical   |True when the broadcast has a commentator |
#'    |likes_enabled                              |logical   |True when likes are enabled on the broadcast page |
#'    |is_vertical_video                          |logical   |True when the broadcast video is vertical (portrait) |
#'    |khl_id                                     |integer   |khl.ru game id of the `event` object (site key space): the join key to khl-rest |
#'    |arena_id                                   |integer   |khl.ru arena id |
#'    |arena_capacity                             |integer   |Arena seating capacity |
#'    |arena_phone                                |character |Arena phone number as published |
#'    |arena_website                              |character |Arena website URL |
#'    |arena_name                                 |character |Arena (city) name, from data.ARENAS |
#'    |arena_address                              |character |Arena street address (English with locale=en) |
#'    |arena_city                                 |character |Arena city (English with locale=en) |
#'    |arena_image                                |character |Arena photo URL on khl.ru |
#'    |arena_geo_lat                              |numeric   |Arena latitude, decimal degrees |
#'    |arena_geo_long                             |numeric   |Arena longitude, decimal degrees |
#'    |team_a_id                                  |integer   |Webcaster id of the `team_a` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_a_khl_id                              |integer   |khl.ru club id of the `team_a` object (site key space): the join key to khl-rest |
#'    |team_a_image                               |character |Home team logo URL (webcaster thumbnail) |
#'    |team_a_name                                |character |Home team name (English with locale=en) |
#'    |team_a_location                            |character |Home team city / region (English with locale=en) |
#'    |team_a_shots                               |integer   |Home shots on goal |
#'    |team_a_gf                                  |integer   |Home team goals (final) |
#'    |team_a_ppg                                 |integer   |Power-play goals |
#'    |team_a_shg                                 |integer   |Short-handed goals |
#'    |team_a_ppc                                 |integer   |Count; equals the team's own penalties in the sample (5 for 10 PIM), so probably penalties taken. UNVERIFIED |
#'    |team_a_vbr                                 |integer   |Faceoffs won ('vbr' from Russian вбрасывания; = game/text fow) |
#'    |team_a_pim                                 |integer   |Penalty minutes |
#'    |team_a_total_puck_control_time             |numeric   |Team puck-control time, decimal MINUTES (21.81 = 21:48) |
#'    |team_a_total_distance_travelled            |numeric   |Distance skated by the team, km (= game/text dtA) |
#'    |team_a_offensive_blue_line_crossings_count |integer   |Offensive blue-line crossings |
#'    |team_a_top_players                         |character |Home game leaders (hardest shot, ...), JSON list with the webcaster and khl.ru player ids |
#'    |team_a_start_fives                         |character |Home starting lineup, JSON list of players (webcaster id, khl_id, jersey, role) |
#'    |team_a_players                             |character |Home game roster, JSON list of players (webcaster id, khl_id, jersey, role) |
#'    |team_a_active                              |logical   |True when the home team is active in the webcaster app |
#'    |team_b_id                                  |integer   |Webcaster id of the `team_b` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_b_khl_id                              |integer   |khl.ru club id of the `team_b` object (site key space): the join key to khl-rest |
#'    |team_b_image                               |character |Visitor team logo URL (webcaster thumbnail) |
#'    |team_b_name                                |character |Visitor team name (English with locale=en) |
#'    |team_b_location                            |character |Visitor team city / region (English with locale=en) |
#'    |team_b_shots                               |integer   |Visitor shots on goal |
#'    |team_b_gf                                  |integer   |Visitor team goals (final) |
#'    |team_b_ppg                                 |integer   |Power-play goals |
#'    |team_b_shg                                 |integer   |Short-handed goals |
#'    |team_b_ppc                                 |integer   |Count; equals the team's own penalties in the sample (3 for 6 PIM), so probably penalties taken. UNVERIFIED |
#'    |team_b_vbr                                 |integer   |Faceoffs won ('vbr' from Russian вбрасывания; = game/text fow) |
#'    |team_b_pim                                 |integer   |Penalty minutes |
#'    |team_b_total_puck_control_time             |numeric   |Team puck-control time, decimal MINUTES (21.81 = 21:48) |
#'    |team_b_total_distance_travelled            |numeric   |Distance skated by the team, km (= game/text dtA) |
#'    |team_b_offensive_blue_line_crossings_count |integer   |Offensive blue-line crossings |
#'    |team_b_top_players                         |character |Visitor game leaders (hardest shot, ...), JSON list with the webcaster and khl.ru player ids |
#'    |team_b_start_fives                         |character |Visitor starting lineup, JSON list of players (webcaster id, khl_id, jersey, role) |
#'    |team_b_players                             |character |Visitor game roster, JSON list of players (webcaster id, khl_id, jersey, role) |
#'    |team_b_active                              |logical   |True when the visitor team is active in the webcaster app |
#'    |quotes                                     |character |Video clips (webcaster quotes) |
#'    |goals                                      |character |Goals of the game, JSON list (time in game seconds, score, status, author, assistants) |
#'    |violations                                 |character |Penalties in order |
#'    |other_events_with_both_teams               |character |Other games between the two teams, JSON list of webcaster events |
#'    |social_tags                                |character |Social-media hashtags of the game, JSON list |
#'    |free                                       |logical   |True when the broadcast is free to watch (no subscription) |
#'    |yandex_rights                              |logical   |True when Yandex holds rights to the broadcast (unverified) |
#'    |yandex_id                                  |character |Yandex video id of the broadcast (m-prefixed) |
#'    |match_id                                   |integer   |khl.ru game id as a string (= khl_id) |
#'    |balancer_type                              |character |Video delivery type (record = archived broadcast) |
#'    |event_or_quote_type_name                   |character |Video category label in Russian (Архив = archive) |
#'    |outer_stage_id                             |integer   |khl.ru tournament id of the event's stage (= stage.khl_id) |
#'    |stage_name                                 |character |Stage label (Regular 2025/2026) |
#'    |parent_id                                  |integer   |Undocumented provider field `parent_id`; carried as sent. |
#'    |name                                       |character |Name (English with locale=en) |
#'    |mref1                                      |character |Referee 1, name in Russian |
#'    |mref2                                      |character |Referee 2, name in Russian |
#'    |lref1                                      |character |Linesman 1, name in Russian |
#'    |lref2                                      |character |Linesman 2, name in Russian |
#'    |announce                                   |character |Undocumented provider field `announce`; carried as sent. |
#'    |views                                      |character |Broadcast view count as display text ('7,579') |
#'    |stage_type                                 |character |Stage type: regular or playoff |
#'    |audio_tracks                               |character |Broadcast audio tracks, JSON list of id / title (main, intersound) |
#'    |image_big                                  |character |Large broadcast thumbnail image URL |
#'    |location                                   |character |Home team city / region (English with locale=en) |
#'    |start_at_day                               |integer   |Game day start (midnight Moscow time), unix seconds |
#'    |start_at                                   |integer   |Start time, unix milliseconds |
#'    |event_start_at                             |integer   |Puck-drop time, unix milliseconds |
#'    |end_at                                     |integer   |End time, unix milliseconds |
#'    |tickets                                    |character |Undocumented provider field `tickets`; carried as sent. |
#'    |text_events                                |character |Summary text events only; no shot-level play-by-play |
#'    |not_regular                                |logical   |True for a game outside the regular schedule (False in the captured bodies; unverified) |
#'    |infographics                               |character |Scoreboard overlay label of the broadcast (With scoreboard) |
#'    |infographics_enabled                       |logical   |True when the broadcast carries the scoreboard overlay |
#'    |score                                      |character |Score after the goal 'a:b' |
#'    |condensed_game_id                          |integer   |Webcaster id of the `condensed_game` object (webcaster key space); no khl.ru counterpart in the body |
#'    |condensed_game_name                        |character |Condensed-game video title |
#'    |condensed_game_start_ts                    |integer   |Condensed-game video start time, unix milliseconds |
#'    |condensed_game_m3u8_url                    |character |Condensed-game video HLS (m3u8) stream URL; signed and expiring |
#'    |condensed_game_feed_url                    |character |Condensed-game video webcaster player feed URL; signed and expiring |
#'    |condensed_game_iframe_url                  |character |Condensed-game video embeddable player (iframe) URL |
#'    |condensed_game_iframe_code                 |character |Condensed-game video embed HTML (an iframe tag) |
#'    |condensed_game_finish_ts                   |integer   |Condensed-game video end time, unix milliseconds |
#'    |condensed_game_image_url                   |character |Condensed-game video thumbnail image URL |
#'    |condensed_game_description                 |character |Condensed-game video description text (HTML-escaped; Russian goal list on highlights) |
#'    |condensed_game_audio_tracks                |character |Condensed-game video audio tracks, JSON list of id / title (main, intersound) |
#'    |condensed_game_is_vertical_video           |logical   |Condensed-game video: True when the video is vertical (portrait) |
#'    |condensed_game_stage_id                    |integer   |Condensed-game video: webcaster stage id (its own id space; not a khl.ru tournament id) |
#'    |condensed_game_free                        |logical   |Condensed-game video: True when free to watch (no subscription) |
#'    |condensed_game_yandex_rights               |logical   |Condensed-game video: True when Yandex holds rights to the video (unverified) |
#'    |condensed_game_yandex_id                   |character |Condensed-game video: Yandex video id of the broadcast (m-prefixed) |
#'    |condensed_game_match_id                    |integer   |khl.ru game id as a string (= khl_id) |
#'    |condensed_game_balancer_type               |character |Condensed-game video: video delivery type (record = archived broadcast, file = clip) |
#'    |condensed_game_event_or_quote_type_name    |character |Condensed-game video: video category label (Архив = archive, Хайлайты = highlights) |
#'    |condensed_game_outer_stage_id              |integer   |khl.ru tournament id of the event's stage (= stage.khl_id) |
#'    |condensed_game_stage_name                  |character |Condensed-game video: stage label (Regular 2025/2026) |
#'    |highlight_id                               |integer   |Webcaster id of the `highlight` object (webcaster key space); no khl.ru counterpart in the body |
#'    |highlight_name                             |character |Highlights video title |
#'    |highlight_start_ts                         |integer   |Highlights video start time, unix milliseconds |
#'    |highlight_m3u8_url                         |character |Highlights video HLS (m3u8) stream URL; signed and expiring |
#'    |highlight_feed_url                         |character |Highlights video webcaster player feed URL; signed and expiring |
#'    |highlight_iframe_url                       |character |Highlights video embeddable player (iframe) URL |
#'    |highlight_iframe_code                      |character |Highlights video embed HTML (an iframe tag) |
#'    |highlight_finish_ts                        |integer   |Highlights video end time, unix milliseconds |
#'    |highlight_image_url                        |character |Highlights video thumbnail image URL |
#'    |highlight_description                      |character |Highlights video description text (HTML-escaped; Russian goal list on highlights) |
#'    |highlight_audio_tracks                     |character |Highlights video audio tracks, JSON list of id / title (main, intersound) |
#'    |highlight_is_vertical_video                |logical   |Highlights video: True when the video is vertical (portrait) |
#'    |highlight_stage_id                         |integer   |Highlights video: webcaster stage id (its own id space; not a khl.ru tournament id) |
#'    |highlight_free                             |logical   |Highlights video: True when free to watch (no subscription) |
#'    |highlight_yandex_rights                    |logical   |Highlights video: True when Yandex holds rights to the video (unverified) |
#'    |highlight_yandex_id                        |character |Highlights video: Yandex video id of the broadcast (m-prefixed) |
#'    |highlight_match_id                         |integer   |khl.ru game id as a string (= khl_id) |
#'    |highlight_balancer_type                    |character |Highlights video: video delivery type (record = archived broadcast, file = clip) |
#'    |highlight_event_or_quote_type_name         |character |Highlights video: video category label (Архив = archive, Хайлайты = highlights) |
#'    |highlight_outer_stage_id                   |integer   |khl.ru tournament id of the event's stage (= stage.khl_id) |
#'    |highlight_stage_name                       |character |Highlights video: stage label (Regular 2025/2026) |
#'    |outer_url                                  |character |khl.ru page of the game (the game_id and tournament_id are in its path) |
#'    |m3u8_url                                   |character |Broadcast HLS (m3u8) stream URL; empty without rights |
#'    |feed_url                                   |character |Broadcast webcaster player feed URL; empty without rights |
#'    |iframe_url                                 |character |Broadcast embeddable player (iframe) URL |
#'    |iframe_code                                |character |Broadcast embed HTML (an iframe tag) |
#'    |scores_first_period                        |character |1st-period score 'a:b' |
#'    |scores_second_period                       |character |2nd-period score 'a:b' |
#'    |scores_third_period                        |character |3rd-period score 'a:b' |
#'    |scores_overtime                            |character |Overtime score 'a:b' (null if none) |
#'    |scores_bullitt                             |character |Shootout score 'a:b' (null if none); 'bullitt' is Russian for shootout |
#'    |this_pair_stat_events_count                |integer   |Head-to-head: games between the two teams on record (scope unverified) |
#'    |this_pair_stat_team_a_wins_count           |integer   |Head-to-head: home team wins in those games |
#'    |this_pair_stat_team_a_goals_count          |integer   |Head-to-head: home team goals in those games |
#'    |this_pair_stat_team_a_points_count         |integer   |Head-to-head: home team standings points in those games |
#'    |this_pair_stat_team_b_wins_count           |integer   |Head-to-head: visitor team wins in those games |
#'    |this_pair_stat_team_b_goals_count          |integer   |Head-to-head: visitor team goals in those games |
#'    |this_pair_stat_team_b_points_count         |integer   |Head-to-head: visitor team standings points in those games |
#'    |transaction_types                          |character |Paid video products (app store) |
#'    |sscore                                     |character |Undocumented provider field `sscore`; carried as sent. |
#'    |commentators_names                         |character |Broadcast commentators, comma-separated |
#'    |transactions                               |character |Undocumented provider field `transactions`; carried as sent. |
#'    |has_video                                  |logical   |True when the broadcast has a video |
#'    |geo_error                                  |character |Undocumented provider field `geo_error`; carried as sent. |
#' @examples
#' \donttest{
#'   try(khl_webcaster_event(event_id = 2786714))
#' }
#' @family KHL Functions
#' @export
khl_webcaster_event <- function(event_id, locale = "en", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(event_id, "event_id")
  .khl_check_locale(locale)
  raw <- .khl_wc_get("event_v2.json",
                     list(`id` = event_id,
                          `locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_webcaster(raw, "khl_webcaster_event"),
            "KHL Webcaster Event from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Events**
#' @description Games of a stage, 16 per page (one row per game; khl_id is the khl.ru game id).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/events_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL webcaster stage id
#'   through the webcaster season table (khl_webcaster_stages). NULL = the current
#'   stage.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param page 1-based page of 16 games; an empty page ends the stage.
#' @param stage_id Webcaster stage id, sent as is instead of season / stage (370 = 2025-26 regular
#'   season; khl_webcaster_stages lists them). Not a khl.ru tournament id.
#' @param team_ids Webcaster team id(s) (khl_webcaster_teams id, not a khl.ru club id): games of
#'   these teams.
#' @param start_after Games starting after this unix time (seconds). Needs a stage.
#' @param start_before Games starting before this unix time (seconds). Needs a stage.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (54 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name                 |types     |description |
#'    |:------------------------|:---------|:----------|
#'    |game_state_key           |character |Game state key (only finished observed) |
#'    |period                   |integer   |Period number (box score: 0 = the whole game; 4+ = overtime) |
#'    |hd                       |logical   |HD video available |
#'    |id                       |integer   |Webcaster id of the `event` object (webcaster key space); join to khl.ru through its khl_id |
#'    |stage_id                 |integer   |Webcaster stage id (its own id space; not a khl.ru tournament id) |
#'    |type_id                  |integer   |Webcaster event type code |
#'    |commentator              |logical   |Commentary track available |
#'    |likes_enabled            |logical   |True when likes are enabled on the broadcast page |
#'    |is_vertical_video        |logical   |True when the broadcast video is vertical (portrait) |
#'    |khl_id                   |integer   |khl.ru game id of the `event` object (site key space): the join key to khl-rest |
#'    |team_a_id                |integer   |Webcaster id of the `team_a` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_a_khl_id            |integer   |khl.ru club id of the `team_a` object (site key space): the join key to khl-rest |
#'    |team_a_image             |character |Home team logo URL (webcaster thumbnail) |
#'    |team_a_name              |character |Home team name (English with locale=en) |
#'    |team_a_location          |character |Home team city / region (English with locale=en) |
#'    |team_b_id                |integer   |Webcaster id of the `team_b` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_b_khl_id            |integer   |khl.ru club id of the `team_b` object (site key space): the join key to khl-rest |
#'    |team_b_image             |character |Visitor team logo URL (webcaster thumbnail) |
#'    |team_b_name              |character |Visitor team name (English with locale=en) |
#'    |team_b_location          |character |Visitor team city / region (English with locale=en) |
#'    |name                     |character |Matchup label in English |
#'    |tickets                  |character |Undocumented provider field `tickets`; carried as sent. |
#'    |location                 |character |Home team city / region (English with locale=en) |
#'    |start_at_day             |integer   |Game day start (midnight Moscow time), unix seconds |
#'    |start_at                 |integer   |Start time, unix milliseconds |
#'    |event_start_at           |integer   |Puck-drop time, unix milliseconds |
#'    |end_at                   |integer   |End time, unix milliseconds |
#'    |not_regular              |logical   |True for a game outside the regular schedule (False in the captured bodies; unverified) |
#'    |score                    |character |Final score 'team_a:team_b' |
#'    |scores_first_period      |character |1st-period score 'a:b' |
#'    |scores_second_period     |character |2nd-period score 'a:b' |
#'    |scores_third_period      |character |3rd-period score 'a:b' |
#'    |scores_overtime          |character |Overtime score 'a:b' (null if none) |
#'    |scores_bullitt           |character |Shootout score 'a:b' (null if none); 'bullitt' is Russian for shootout |
#'    |sscore                   |character |Undocumented provider field `sscore`; carried as sent. |
#'    |image                    |character |Broadcast thumbnail image URL |
#'    |condensed_game_id        |integer   |Webcaster event id of the condensed-game video |
#'    |highlight_id             |integer   |Webcaster event id of the highlights video |
#'    |infographics             |character |Scoreboard overlay label of the broadcast (With scoreboard) |
#'    |infographics_enabled     |logical   |True when the broadcast carries the scoreboard overlay |
#'    |has_video                |logical   |Video available |
#'    |m3u8_url                 |character |Broadcast HLS (m3u8) stream URL; empty without rights |
#'    |feed_url                 |character |Broadcast webcaster player feed URL; empty without rights |
#'    |iframe_url               |character |Broadcast embeddable player (iframe) URL |
#'    |iframe_code              |character |Broadcast embed HTML (an iframe tag) |
#'    |free                     |logical   |Video free to watch |
#'    |yandex_rights            |logical   |Yandex broadcast rights flag |
#'    |yandex_id                |character |Yandex video id of the broadcast (m-prefixed) |
#'    |match_id                 |integer   |khl.ru game id as a string (= khl_id) |
#'    |balancer_type            |character |Video delivery type (record = archived broadcast) |
#'    |event_or_quote_type_name |character |Video category label in Russian (Архив = archive) |
#'    |outer_stage_id           |integer   |khl.ru tournament id of the event's stage (= stage.khl_id) |
#'    |stage_name               |character |Stage label (Regular 2025/2026) |
#'    |previews                 |character |Undocumented provider field `previews`; carried as sent. |
#' @examples
#' \donttest{
#'   try(khl_webcaster_events(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_webcaster_events <- function(season = NULL, stage = "regular", page = 1, stage_id = NULL,
                                 team_ids = NULL, start_after = NULL, start_before = NULL, locale = "en",
                                 return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("events_v2.json",
                     list(`season` = season,
                          `stage` = stage,
                          `stage_id` = stage_id,
                          `page` = page,
                          `q[team_a_or_team_b_in][]` = team_ids,
                          `q[start_at_gt_time_from_unixtime]` = start_after,
                          `q[start_at_lt_time_from_unixtime]` = start_before,
                          `locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_webcaster(raw, "khl_webcaster_events"),
            "KHL Webcaster Events from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Leaders**
#' @description Stat leaders of a stage: one row per player per category.
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/leaders_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL webcaster stage id
#'   through the webcaster season table (khl_webcaster_stages). NULL = the current
#'   stage.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param stage_id Webcaster stage id, sent as is instead of season / stage (370 = 2025-26 regular
#'   season; khl_webcaster_stages lists them). Not a khl.ru tournament id.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (14 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name      |types     |description |
#'    |:-------------|:---------|:----------|
#'    |category      |character |Leader category |
#'    |rank          |integer   |1-based position in its table / list |
#'    |id            |integer   |Webcaster id of the `players` object (webcaster key space); join to khl.ru through its khl_id |
#'    |khl_id        |integer   |khl.ru player id of the `players` object (site key space): the join key to khl-rest |
#'    |name          |character |Name (English with locale=en) |
#'    |image         |character |Player photo URL on khl.ru |
#'    |shirt_number  |integer   |Jersey number (not an id) |
#'    |role_key      |character |Position key: goaltender, defensemen, forward |
#'    |team_id       |integer   |Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_khl_id   |integer   |khl.ru club id of the `team` object (site key space): the join key to khl-rest |
#'    |team_name     |character |Team name as the site prints it |
#'    |team_location |character |Team city / region (English with locale=en) |
#'    |pos           |character |Leader position in the category, as text ('1' = leader) |
#'    |stats         |character |Category value(s) |
#' @examples
#' \donttest{
#'   try(khl_webcaster_leaders(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_webcaster_leaders <- function(season = NULL, stage = "regular", stage_id = NULL, locale = "en",
                                  return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("leaders_v2.json",
                     list(`season` = season,
                          `stage` = stage,
                          `stage_id` = stage_id,
                          `locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_leaders(raw),
            "KHL Webcaster Leaders from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Players**
#' @description Players with stage-scoped stats (one row per player; khl_id is the khl.ru player
#'   id).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/players_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param player_ids Webcaster player id(s), not khl.ru ids. NULL = every player of the stage (a
#'   large body).
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL webcaster stage id
#'   through the webcaster season table (khl_webcaster_stages). NULL = the current
#'   stage.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param stage_id Webcaster stage id, sent as is instead of season / stage (370 = 2025-26 regular
#'   season; khl_webcaster_stages lists them). Not a khl.ru tournament id.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (29 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name            |types     |description |
#'    |:-------------------|:---------|:----------|
#'    |id                  |integer   |Webcaster id of the `player` object (webcaster key space); join to khl.ru through its khl_id |
#'    |height              |integer   |Player height in cm |
#'    |weight              |integer   |Player weight in kg |
#'    |age                 |integer   |Player age in years |
#'    |shirt_number        |integer   |Jersey number (not an id) |
#'    |khl_id              |integer   |khl.ru player id of the `player` object (site key space): the join key to khl-rest |
#'    |role                |character |Position: goaltender, defensemen or forward |
#'    |role_key            |character |Position key: goaltender, defensemen, forward |
#'    |country             |character |Country (English with locale=en) |
#'    |image               |character |Player photo URL on khl.ru |
#'    |flag_image_url      |character |Country flag image URL |
#'    |name                |character |Player name (English with locale=en) |
#'    |birthday            |integer   |Birth date, unix seconds |
#'    |stick               |character |Shooting hand: l or r |
#'    |team_id             |integer   |Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_khl_id         |integer   |khl.ru club id of the `team` object (site key space): the join key to khl-rest |
#'    |team_name           |character |Team name as the site prints it |
#'    |team_location       |character |Team city / region (English with locale=en) |
#'    |team_image          |character |Team logo URL (webcaster thumbnail) |
#'    |team_division       |character |Team division name (English with locale=en) |
#'    |team_division_key   |character |Team division key (bobrov, tarasov, kharlamov, chernyshev) |
#'    |team_conference     |character |Team conference name (West / East) |
#'    |team_conference_key |character |Team conference key (west / east) |
#'    |stats               |character |Stage-scoped stat lines |
#'    |teams               |character |Teams played for |
#'    |quotes              |character |Video clips (webcaster quotes) |
#'    |seasons_count_khl   |integer   |KHL seasons played |
#'    |seasons_count_team  |integer   |Seasons played for the current team |
#'    |positions           |character |Positions played |
#' @examples
#' \donttest{
#'   try(khl_webcaster_players(player_ids = "[33405, 1144]", season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_webcaster_players <- function(player_ids = NULL, season = NULL, stage = "regular", stage_id = NULL,
                                  locale = "en", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("players_v2.json",
                     list(`q[id_in][]` = player_ids,
                          `season` = season,
                          `stage` = stage,
                          `stage_id` = stage_id,
                          `locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_webcaster(raw, "khl_webcaster_players"),
            "KHL Webcaster Players from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Players (Light)**
#' @description Every current player: webcaster id, khl.ru id, jersey and team (one row per player).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/players_v2_light.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (10 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name      |types     |description |
#'    |:-------------|:---------|:----------|
#'    |id            |integer   |Webcaster id of the `row` object (webcaster key space); join to khl.ru through its khl_id |
#'    |shirt_number  |integer   |Jersey number (not an id) |
#'    |khl_id        |integer   |khl.ru id of the `row` object (site key space): the join key to khl-rest |
#'    |image         |character |Player photo URL on khl.ru |
#'    |name          |character |Name (English with locale=en) |
#'    |team_id       |integer   |Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
#'    |team_khl_id   |integer   |khl.ru club id of the `team` object (site key space): the join key to khl-rest |
#'    |team_name     |character |Team name as the site prints it |
#'    |team_location |character |Team city / region (English with locale=en) |
#'    |team          |character |Team (webcaster) |
#' @examples
#' \donttest{
#'   try(khl_webcaster_players_light())
#' }
#' @family KHL Functions
#' @export
khl_webcaster_players_light <- function(locale = "en", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("players_v2_light.json",
                     list(`locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_webcaster(raw, "khl_webcaster_players_light"),
            "KHL Webcaster Players (Light) from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Tables**
#' @description League table of every season (one row per season per team, from the championship
#'   table).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/tables_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (29 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name       |types     |description |
#'    |:--------------|:---------|:----------|
#'    |season         |integer   |Season END year (2026 = 2025-26) |
#'    |season_label   |character |Season label as sent (26/27, 2025/2026) |
#'    |rank           |integer   |1-based position in its table / list |
#'    |gp             |integer   |Games played (gp = w + otw + sow + sol + otl + l) |
#'    |w              |integer   |Regulation wins only: overtime (otw) and shootout (sow) wins are separate columns |
#'    |otw            |integer   |Overtime wins (not counted in w) |
#'    |sow            |integer   |Shootout wins (not counted in w) |
#'    |sol            |integer   |Shootout losses |
#'    |otl            |integer   |Overtime losses |
#'    |otl_en         |integer   |Overtime losses with the net empty |
#'    |l              |integer   |Regulation losses only: overtime (otl) and shootout (sol) losses are separate columns |
#'    |pts            |integer   |Points: 2 per win of any kind, 1 per overtime or shootout loss (3-2-1 in early seasons such as 2013-14) |
#'    |pts_pct        |numeric   |Points percentage |
#'    |soa            |integer   |Shutouts against, 1-7 a season (the refs say shots on goal against; the values do not fit that; unverified) |
#'    |so             |integer   |Shutouts (refs: khl-returns.md tables_v2) |
#'    |gf             |integer   |Goals scored by the team |
#'    |ga             |integer   |Goals allowed by the team |
#'    |pim            |integer   |Penalty minutes |
#'    |pima           |integer   |Opponents' penalty minutes |
#'    |id             |integer   |Webcaster id of the `stages` object (webcaster key space); no khl.ru counterpart in the body |
#'    |khl_id         |integer   |khl.ru club id of the `championship` object (site key space): the join key to khl-rest |
#'    |image          |character |Team logo URL (webcaster thumbnail) |
#'    |tv_image       |character |Team logo path on the KHL TV front end |
#'    |name           |character |Name (English with locale=en) |
#'    |location       |character |Team city / region (English with locale=en) |
#'    |division       |character |Division name (English with locale=en) |
#'    |division_key   |character |Division key (bobrov, tarasov, kharlamov, chernyshev) |
#'    |conference     |character |Conference (west / east) |
#'    |conference_key |character |Conference key (east / west) |
#' @examples
#' \donttest{
#'   try(khl_webcaster_tables())
#' }
#' @family KHL Functions
#' @export
khl_webcaster_tables <- function(locale = "en", return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("tables_v2.json",
                     list(`locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_tables(raw),
            "KHL Webcaster Tables from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Team**
#' @description One team with roster, stats and news feed (one row; lists as JSON text).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/team_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param team_id Required. Webcaster team id (khl_webcaster_teams id), not the khl.ru club id.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL webcaster stage id
#'   through the webcaster season table (khl_webcaster_stages). NULL = the current
#'   stage.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param stage_id Webcaster stage id, sent as is instead of season / stage (370 = 2025-26 regular
#'   season; khl_webcaster_stages lists them). Not a khl.ru tournament id.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (44 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name                  |types     |description |
#'    |:-------------------------|:---------|:----------|
#'    |id                        |integer   |Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
#'    |stage                     |character |Stage type the team page answers for: regular or playoff |
#'    |khl_id                    |integer   |khl.ru club id of the `team` object (site key space): the join key to khl-rest |
#'    |name                      |character |Name (English with locale=en) |
#'    |location                  |character |Team city / region (English with locale=en) |
#'    |feed_items                |character |Team news feed, JSON list (type, date, id, title, image) |
#'    |arena_id                  |integer   |Webcaster id of the team's home arena |
#'    |arena_capacity            |integer   |Arena seating capacity |
#'    |arena_phone               |character |Arena phone number as published |
#'    |arena_website             |character |Arena website URL |
#'    |arena_name                |character |Home arena name (English with locale=en) |
#'    |arena_address             |character |Arena street address (English with locale=en) |
#'    |arena_city                |character |Arena city (English with locale=en) |
#'    |arena_image               |character |Arena photo URL on khl.ru |
#'    |arena_geo_lat             |numeric   |Arena latitude, decimal degrees |
#'    |arena_geo_long            |numeric   |Arena longitude, decimal degrees |
#'    |arenas                    |character |Home arenas, JSON list (id, capacity, phone, website, name, address) |
#'    |calendar_events           |character |Team schedule, JSON list of webcaster events |
#'    |recent_events             |character |Recent team games, JSON list of webcaster events |
#'    |website                   |character |Club website URL |
#'    |mail                      |character |Club e-mail address as published |
#'    |foundation_year           |character |Year the club was founded, as text |
#'    |photo                     |character |Team photo URL on khl.ru |
#'    |phone                     |character |Club phone number as published |
#'    |head_coach_photo          |character |Head coach photo URL on khl.ru |
#'    |head_coach_name           |character |Head coach name (English with locale=en) |
#'    |social_networks_tw        |character |Club Twitter / X page URL (empty when none) |
#'    |social_networks_vk        |character |Club VKontakte page URL |
#'    |social_networks_ok        |character |Club Odnoklassniki page URL |
#'    |social_networks_fb        |character |Club Facebook page URL (empty when none) |
#'    |social_networks_instagram |character |Club Instagram page URL |
#'    |social_networks_youtube   |character |Club YouTube channel URL (empty when none) |
#'    |social_networks_telegram  |character |Club Telegram channel URL |
#'    |apps_ios                  |character |Undocumented provider field `apps_ios`; carried as sent. |
#'    |apps_android              |character |Undocumented provider field `apps_android`; carried as sent. |
#'    |apps_windows              |character |Undocumented provider field `apps_windows`; carried as sent. |
#'    |address                   |character |Club office address (English with locale=en) |
#'    |image                     |character |Team logo URL (webcaster thumbnail) |
#'    |division                  |character |Division name (English with locale=en) |
#'    |division_key              |character |Division key (bobrov, tarasov, kharlamov, chernyshev) |
#'    |conference                |character |Conference (west / east) |
#'    |conference_key            |character |Conference key (east / west) |
#'    |stats                     |character |Team stat lines |
#'    |players                   |character |Roster (webcaster players) |
#' @examples
#' \donttest{
#'   try(khl_webcaster_team(season = 2026, team_id = 18))
#' }
#' @family KHL Functions
#' @export
khl_webcaster_team <- function(team_id, season = NULL, stage = "regular", stage_id = NULL, locale = "en",
                               return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_need(team_id, "team_id")
  .khl_check_stage(stage)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("team_v2.json",
                     list(`id` = team_id,
                          `season` = season,
                          `stage` = stage,
                          `stage_id` = stage_id,
                          `locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_webcaster(raw, "khl_webcaster_team"),
            "KHL Webcaster Team from khl.api.webcaster.pro")
}

#' @title **KHL Webcaster Teams**
#' @description Teams of a stage (one row per team; khl_id is the khl.ru club id).
#' @details
#' Endpoint: `GET https://khl.api.webcaster.pro/api/khl_mobile/teams_v2.json`.
#'
#' The webcaster API is keyless JSON. Neither backend takes a year: `season` (the END year, 2026 =
#' 2025/26) and `stage` are mapped to the webcaster stage id through the season table of
#' `khl_webcaster_stages()` (cached for the R session), or pass `stage_id`. Honours
#' `getOption("fastRhockey.proxy")`; betting fields and the caller's IP are removed from every body.
#'
#' Ids and counts are integers (sdv-py Int64); a column holding values past 2^31 (epoch
#' milliseconds) is returned as a whole-number double.
#'
#' At least one second is left between two requests to a host.
#'
#' **Owner flag (sanctions).** The KHL and its webcaster platform are run by Russian operators, some
#' clubs by sanctioned state companies. Fetching on your own machine is fine; any published data
#' release, hosted mirror or redistribution needs owner (sanctions / legal) sign-off first.
#' @param season Season by its END year (2026 = 2025-26), mapped to the KHL webcaster stage id
#'   through the webcaster season table (khl_webcaster_stages). NULL = the current
#'   stage.
#' @param stage Which stage of the season: regular or playoff. One of `regular`, `playoff`.
#' @param stage_id Webcaster stage id, sent as is instead of season / stage (370 = 2025-26 regular
#'   season; khl_webcaster_stages lists them). Not a khl.ru tournament id.
#' @param locale Response language: en or ru. One of `en`, `ru`.
#' @param return_parsed Logical. `TRUE` (default) returns the parsed `fastRhockey_data` tibble;
#'   `FALSE` returns the raw JSON body as a list (betting fields and the caller's
#'   IP removed).
#' @return A `fastRhockey_data` tibble (9 columns; zero rows with the same columns when the payload
#'   is empty), or the raw body as a list when `return_parsed = FALSE`. Raises
#'   `fastRhockey_no_data` (nothing there, or an unknown season / stage),
#'   `fastRhockey_asset_fetch` (an error envelope, an answer for another id than the one
#'   asked, or a failed fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP
#'   400 / 422).
#'
#'    |col_name       |types     |description |
#'    |:--------------|:---------|:----------|
#'    |id             |integer   |Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
#'    |khl_id         |integer   |khl.ru club id of the `team` object (site key space): the join key to khl-rest |
#'    |name           |character |Team name (English with locale=en) |
#'    |location       |character |Team city / region (English with locale=en) |
#'    |image          |character |Team logo URL (webcaster thumbnail) |
#'    |division       |character |Division name (English with locale=en) |
#'    |division_key   |character |Division key (bobrov, tarasov, kharlamov, chernyshev) |
#'    |conference     |character |Conference (west / east) |
#'    |conference_key |character |Conference key (east/west) |
#' @examples
#' \donttest{
#'   try(khl_webcaster_teams(season = 2026))
#' }
#' @family KHL Functions
#' @export
khl_webcaster_teams <- function(season = NULL, stage = "regular", stage_id = NULL, locale = "en",
                                return_parsed = TRUE) {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old), add = TRUE)
  .khl_check_stage(stage)
  .khl_check_locale(locale)
  raw <- .khl_wc_get("teams_v2.json",
                     list(`season` = season,
                          `stage` = stage,
                          `stage_id` = stage_id,
                          `locale` = locale))
  if (!isTRUE(return_parsed)) return(raw)
  .khl_wrap(.khl_parse_webcaster(raw, "khl_webcaster_teams"),
            "KHL Webcaster Teams from khl.api.webcaster.pro")
}
