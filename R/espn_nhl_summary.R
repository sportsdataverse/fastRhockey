# espn_nhl_summary.R
# ESPN NHL: summary-endpoint-derived functions
# Internal league-generic helpers + public NHL shims.
#
# Five deliverables, all driven by a single fetch of
#   site_v2 /summary?event={game_id}
#
#   1. .espn_hockey_pbp()        -> espn_nhl_pbp()        (plays[])
#   2. .espn_hockey_team_box()   -> espn_nhl_team_box()   (boxscore$teams[])
#   3. .espn_hockey_player_box() -> espn_nhl_player_box() (boxscore$players[])
#   4. .espn_hockey_game_all()   -> espn_nhl_game_all()   (named list, ONE fetch)
#   5. .espn_hockey_summary()    -> espn_nhl_summary()    (raw nested list)

# ---------------------------------------------------------------------------
# Shared internal: fetch the summary once and return the raw parsed list.
# ---------------------------------------------------------------------------

#' @noRd
.espn_hockey_fetch_summary <- function(league = "nhl", game_id, ...) {
  .espn_hockey_request(
    "site_v2", "summary",
    query          = list(event = as.character(game_id)),
    league         = league,
    simplifyVector = FALSE,
    ...
  )
}

# ---------------------------------------------------------------------------
# Shared internal: extract home/away team meta from header$competitions[1]
# ---------------------------------------------------------------------------

#' @noRd
.espn_summary_team_meta <- function(raw) {
  hdr   <- raw[["header"]] %||% list()
  comps <- hdr[["competitions"]] %||% list()
  comp  <- if (length(comps) > 0) comps[[1]] else list()
  season     <- hdr[["season"]] %||% list()
  season_year <- as.integer(season[["year"]] %||% NA_integer_)
  season_type <- as.integer(season[["type"]] %||% NA_integer_)
  game_date   <- comp[["date"]] %||% NA_character_

  competitors <- comp[["competitors"]] %||% list()
  home <- away <- list()
  for (c in competitors) {
    if (identical(c[["homeAway"]], "home")) home <- c
    if (identical(c[["homeAway"]], "away")) away <- c
  }

  ht <- home[["team"]] %||% list()
  at <- away[["team"]] %||% list()

  list(
    season_year      = season_year,
    season_type      = season_type,
    game_date        = game_date,
    home_team_id     = ht[["id"]]           %||% NA_character_,
    home_team_name   = ht[["name"]]         %||% NA_character_,
    home_team_location = ht[["location"]]   %||% NA_character_,
    home_team_abbrev = ht[["abbreviation"]] %||% NA_character_,
    home_team_display_name = ht[["displayName"]] %||% NA_character_,
    home_team_short_display_name = ht[["shortDisplayName"]] %||% NA_character_,
    home_team_color  = ht[["color"]]        %||% NA_character_,
    home_team_alternate_color = ht[["alternateColor"]] %||% NA_character_,
    home_team_logo   = ht[["logo"]]         %||% NA_character_,
    home_score       = as.character(home[["score"]] %||% NA_character_),
    home_winner      = as.logical(home[["winner"]] %||% NA),
    away_team_id     = at[["id"]]           %||% NA_character_,
    away_team_name   = at[["name"]]         %||% NA_character_,
    away_team_location = at[["location"]]   %||% NA_character_,
    away_team_abbrev = at[["abbreviation"]] %||% NA_character_,
    away_team_display_name = at[["displayName"]] %||% NA_character_,
    away_team_short_display_name = at[["shortDisplayName"]] %||% NA_character_,
    away_team_color  = at[["color"]]        %||% NA_character_,
    away_team_alternate_color = at[["alternateColor"]] %||% NA_character_,
    away_team_logo   = at[["logo"]]         %||% NA_character_,
    away_score       = as.character(away[["score"]] %||% NA_character_),
    away_winner      = as.logical(away[["winner"]] %||% NA)
  )
}

# ---------------------------------------------------------------------------
# 1. PBP internal helper
# ---------------------------------------------------------------------------

#' Internal: parse plays[] from a summary list into a wide tibble.
#' @noRd
.parse_espn_hockey_pbp <- function(raw, game_id, league = "nhl") {
  plays <- raw[["plays"]] %||% list()
  if (length(plays) == 0) return(data.frame())

  meta <- .espn_summary_team_meta(raw)

  rows <- lapply(plays, function(p) {
    period   <- p[["period"]] %||% list()
    clock    <- p[["clock"]]  %||% list()
    ptype    <- p[["type"]]   %||% list()
    team     <- p[["team"]]   %||% list()
    coord    <- p[["coordinate"]] %||% list()
    strength <- p[["strength"]] %||% list()
    shot_info <- p[["shotInfo"]] %||% list()

    # participants -> up to 3 athlete ids
    parts <- p[["participants"]] %||% list()
    ath_id <- function(i) {
      if (length(parts) >= i) {
        a <- parts[[i]][["athlete"]] %||% list()
        a[["id"]] %||% NA_character_
      } else NA_character_
    }

    list(
      id                   = p[["id"]] %||% NA_character_,
      sequence_number      = p[["sequenceNumber"]] %||% NA_character_,
      type_id              = ptype[["id"]] %||% NA_character_,
      type_text            = ptype[["text"]] %||% NA_character_,
      text                 = p[["text"]] %||% NA_character_,
      away_score           = as.integer(p[["awayScore"]] %||% NA_integer_),
      home_score           = as.integer(p[["homeScore"]] %||% NA_integer_),
      scoring_play         = as.logical(p[["scoringPlay"]] %||% NA),
      score_value          = as.integer(p[["scoreValue"]] %||% NA_integer_),
      shooting_play        = as.logical(p[["shootingPlay"]] %||% NA),
      wallclock            = p[["wallclock"]] %||% NA_character_,
      period_number        = as.integer(period[["number"]] %||% NA_integer_),
      period_display_value = period[["displayValue"]] %||% NA_character_,
      clock_display_value  = clock[["displayValue"]] %||% NA_character_,
      team_id              = if (length(team) > 0) team[["id"]] %||% NA_character_ else NA_character_,
      coordinate_x         = if (length(coord) > 0) as.numeric(coord[["x"]] %||% NA_real_) else NA_real_,
      coordinate_y         = if (length(coord) > 0) as.numeric(coord[["y"]] %||% NA_real_) else NA_real_,
      strength_id          = strength[["id"]] %||% NA_character_,
      strength_text        = strength[["text"]] %||% NA_character_,
      shot_info_id         = shot_info[["id"]] %||% NA_character_,
      shot_info_text       = shot_info[["text"]] %||% NA_character_,
      athlete_id_1         = ath_id(1),
      athlete_id_2         = ath_id(2),
      athlete_id_3         = ath_id(3),
      home_team_id         = meta$home_team_id,
      home_team_name       = meta$home_team_name,
      home_team_location   = meta$home_team_location,
      home_team_abbrev     = meta$home_team_abbrev,
      home_team_display_name = meta$home_team_display_name,
      home_team_color      = meta$home_team_color,
      home_team_alternate_color = meta$home_team_alternate_color,
      home_team_logo       = meta$home_team_logo,
      home_team_score      = meta$home_score,
      home_team_winner     = meta$home_winner,
      away_team_id         = meta$away_team_id,
      away_team_name       = meta$away_team_name,
      away_team_location   = meta$away_team_location,
      away_team_abbrev     = meta$away_team_abbrev,
      away_team_display_name = meta$away_team_display_name,
      away_team_color      = meta$away_team_color,
      away_team_alternate_color = meta$away_team_alternate_color,
      away_team_logo       = meta$away_team_logo,
      away_team_score      = meta$away_score,
      away_team_winner     = meta$away_winner,
      game_id              = as.character(game_id),
      season               = meta$season_year,
      season_type          = meta$season_type,
      game_date            = meta$game_date
    )
  })

  dplyr::bind_rows(lapply(rows, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  })) %>%
    janitor::clean_names() %>%
    make_fastRhockey_data(
      paste0(toupper(league), " Play-by-Play data from ESPN.com"),
      Sys.time()
    )
}

# ---------------------------------------------------------------------------
# 2. Team box internal helper
# ---------------------------------------------------------------------------

#' Internal: parse boxscore$teams[] from a summary list into a wide tibble.
#' @noRd
.parse_espn_hockey_team_box <- function(raw, game_id, league = "nhl") {
  bx_teams <- (raw[["boxscore"]] %||% list())[["teams"]] %||% list()
  if (length(bx_teams) == 0) return(data.frame())

  meta <- .espn_summary_team_meta(raw)

  rows <- lapply(bx_teams, function(t) {
    team    <- t[["team"]] %||% list()
    home_away <- t[["homeAway"]] %||% NA_character_
    stats   <- t[["statistics"]] %||% list()

    row <- list(
      team_id              = team[["id"]]           %||% NA_character_,
      team_uid             = team[["uid"]]          %||% NA_character_,
      team_slug            = team[["slug"]]         %||% NA_character_,
      team_location        = team[["location"]]     %||% NA_character_,
      team_name            = team[["name"]]         %||% NA_character_,
      team_abbreviation    = team[["abbreviation"]] %||% NA_character_,
      team_display_name    = team[["displayName"]]  %||% NA_character_,
      team_short_display_name = team[["shortDisplayName"]] %||% NA_character_,
      team_color           = team[["color"]]        %||% NA_character_,
      team_alternate_color = team[["alternateColor"]] %||% NA_character_,
      team_logo            = team[["logo"]]         %||% NA_character_,
      home_away            = home_away,
      game_id              = as.character(game_id),
      season               = meta$season_year,
      season_type          = meta$season_type,
      game_date            = meta$game_date
    )

    # Pivot each stat by name -> column
    for (s in stats) {
      col_name       <- s[["name"]] %||% s[["abbreviation"]] %||% "stat"
      row[[col_name]] <- s[["displayValue"]] %||% NA_character_
    }

    row
  })

  dplyr::bind_rows(lapply(rows, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  })) %>%
    janitor::clean_names() %>%
    make_fastRhockey_data(
      paste0(toupper(league), " Team Box Score data from ESPN.com"),
      Sys.time()
    )
}

# ---------------------------------------------------------------------------
# 3. Player box internal helper
# ---------------------------------------------------------------------------

#' Internal: parse boxscore$players[] from a summary list into a wide tibble.
#' Skaters (forwards + defenses) and goalies are included; columns are union-
#' ed via dplyr::bind_rows so skater-only cols are NA for goalies and vice versa.
#' @noRd
.parse_espn_hockey_player_box <- function(raw, game_id, league = "nhl") {
  bx_players <- (raw[["boxscore"]] %||% list())[["players"]] %||% list()
  if (length(bx_players) == 0) return(data.frame())

  meta <- .espn_summary_team_meta(raw)

  all_rows <- list()

  for (pg in bx_players) {
    team      <- pg[["team"]] %||% list()
    team_id   <- team[["id"]]           %||% NA_character_
    team_abbr <- team[["abbreviation"]] %||% NA_character_
    # Determine home_away from meta
    home_away <- dplyr::case_when(
      identical(team_id, meta$home_team_id) ~ "home",
      identical(team_id, meta$away_team_id) ~ "away",
      TRUE ~ NA_character_
    )
    team_display_name <- dplyr::case_when(
      identical(team_id, meta$home_team_id) ~ meta$home_team_display_name,
      identical(team_id, meta$away_team_id) ~ meta$away_team_display_name,
      TRUE ~ NA_character_
    )
    opp_team_id <- dplyr::case_when(
      identical(team_id, meta$home_team_id) ~ meta$away_team_id,
      identical(team_id, meta$away_team_id) ~ meta$home_team_id,
      TRUE ~ NA_character_
    )
    opp_team_abbrev <- dplyr::case_when(
      identical(team_id, meta$home_team_id) ~ meta$away_team_abbrev,
      identical(team_id, meta$away_team_id) ~ meta$home_team_abbrev,
      TRUE ~ NA_character_
    )
    team_score <- dplyr::case_when(
      identical(team_id, meta$home_team_id) ~ meta$home_score,
      identical(team_id, meta$away_team_id) ~ meta$away_score,
      TRUE ~ NA_character_
    )
    team_winner <- dplyr::case_when(
      identical(team_id, meta$home_team_id) ~ meta$home_winner,
      identical(team_id, meta$away_team_id) ~ meta$away_winner,
      TRUE ~ NA
    )

    for (grp in pg[["statistics"]] %||% list()) {
      grp_name <- grp[["name"]] %||% NA_character_
      # Skip "skaters" aggregate group (it duplicates forwards + defenses rows
      # with empty athletes list in practice, but guard anyway)
      if (identical(grp_name, "skaters") &&
          length(grp[["athletes"]] %||% list()) == 0) next

      labels <- grp[["labels"]]       %||% character(0)
      descriptions <- grp[["descriptions"]] %||% character(0)
      clean_labels <- janitor::make_clean_names(labels)

      for (ath_entry in grp[["athletes"]] %||% list()) {
        ath   <- ath_entry[["athlete"]] %||% list()
        stats <- ath_entry[["stats"]]   %||% character(0)

        pos   <- ath[["position"]] %||% list()
        hs    <- ath[["headshot"]] %||% list()

        row <- list(
          athlete_id           = ath[["id"]]          %||% NA_character_,
          athlete_display_name = ath[["displayName"]] %||% NA_character_,
          athlete_short_name   = ath[["shortName"]]   %||% NA_character_,
          athlete_last_name    = ath[["lastName"]]     %||% NA_character_,
          athlete_jersey       = ath[["jersey"]]       %||% NA_character_,
          athlete_headshot_href = hs[["href"]]          %||% NA_character_,
          athlete_position_name = pos[["name"]]         %||% NA_character_,
          athlete_position_abbreviation = pos[["abbreviation"]] %||% NA_character_,
          athlete_active       = as.logical(ath[["active"]] %||% NA),
          athlete_scratched    = as.logical(ath[["scratched"]] %||% NA),
          stat_group           = grp_name,
          team_id              = team_id,
          team_abbreviation    = team_abbr,
          team_display_name    = team_display_name,
          home_away            = home_away,
          team_score           = team_score,
          team_winner          = team_winner,
          opponent_team_id     = opp_team_id,
          opponent_team_abbreviation = opp_team_abbrev,
          game_id              = as.character(game_id),
          season               = meta$season_year,
          season_type          = meta$season_type,
          game_date            = meta$game_date
        )

        # Assign each stat by its clean label name
        for (si in seq_along(clean_labels)) {
          col <- clean_labels[si]
          val <- if (si <= length(stats)) stats[[si]] else NA_character_
          row[[col]] <- val
        }

        all_rows <- c(all_rows, list(row))
      }
    }
  }

  if (length(all_rows) == 0) return(data.frame())

  dplyr::bind_rows(lapply(all_rows, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  })) %>%
    janitor::clean_names() %>%
    make_fastRhockey_data(
      paste0(toupper(league), " Player Box Score data from ESPN.com"),
      Sys.time()
    )
}

# ===========================================================================
# 1. espn_nhl_pbp — internal + public
# ===========================================================================

#' Internal: ESPN hockey play-by-play from summary (league-generic)
#'
#' @param league ESPN league slug.
#' @param game_id ESPN event identifier.
#' @param ... Passed through to `.retry_request()`.
#' @return A `fastRhockey_data` tibble, one row per play.
#' @noRd
.espn_hockey_pbp <- function(league = "nhl", game_id, ...) {
  .args <- .capture_args()
  pbp   <- data.frame()

  tryCatch(
    expr = {
      raw <- .espn_hockey_fetch_summary(league, game_id, ...)
      pbp <- .parse_espn_hockey_pbp(raw, game_id, league)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} play-by-play data for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} play-by-play for game_id = {game_id}",
        args = .args)
    },
    finally = {}
  )
  return(pbp)
}

#' @name espn_nhl_pbp
NULL
#' @title **Get ESPN NHL Play-by-Play**
#' @rdname espn_nhl_pbp
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per play:
#'
#'    |col_name                  |types     |description                                     |
#'    |:-------------------------|:---------|:-----------------------------------------------|
#'    |id                        |character |ESPN play identifier.                           |
#'    |sequence_number           |character |Play sequence number.                           |
#'    |type_id                   |character |Play type identifier.                           |
#'    |type_text                 |character |Play type display text.                         |
#'    |text                      |character |Play description text.                          |
#'    |away_score                |integer   |Away team score at time of play.                |
#'    |home_score                |integer   |Home team score at time of play.                |
#'    |scoring_play              |logical   |Whether the play resulted in a goal.            |
#'    |score_value               |integer   |Goal value (1 for regulation/OT goals).         |
#'    |shooting_play             |logical   |Whether the play was a shot attempt.            |
#'    |wallclock                 |character |Wall-clock timestamp (ISO 8601).                |
#'    |period_number             |integer   |Period number (1-3 regulation, 4+ OT/SO).       |
#'    |period_display_value      |character |Period display label (e.g. "1st Period").       |
#'    |clock_display_value       |character |Game clock display string (e.g. "14:32").       |
#'    |team_id                   |character |ESPN team identifier for the acting team.       |
#'    |coordinate_x              |numeric   |X coordinate of play location.                  |
#'    |coordinate_y              |numeric   |Y coordinate of play location.                  |
#'    |strength_id               |character |Strength situation identifier.                  |
#'    |strength_text             |character |Strength situation text (e.g. "Even Strength"). |
#'    |shot_info_id              |character |Shot type identifier.                           |
#'    |shot_info_text            |character |Shot type text (e.g. "Wrist Shot").             |
#'    |athlete_id_1              |character |Primary participant athlete identifier.         |
#'    |athlete_id_2              |character |Secondary participant athlete identifier.       |
#'    |athlete_id_3              |character |Tertiary participant athlete identifier.        |
#'    |home_team_id              |character |Home team ESPN identifier.                      |
#'    |home_team_name            |character |Home team name.                                 |
#'    |home_team_location        |character |Home team city/location.                        |
#'    |home_team_abbrev          |character |Home team abbreviation.                         |
#'    |home_team_display_name    |character |Home team display name.                         |
#'    |home_team_color           |character |Home team primary color hex.                    |
#'    |home_team_alternate_color |character |Home team alternate color hex.                  |
#'    |home_team_logo            |character |Home team logo URL.                             |
#'    |home_team_score           |character |Home team final score.                          |
#'    |home_team_winner          |logical   |Whether the home team won.                      |
#'    |away_team_id              |character |Away team ESPN identifier.                      |
#'    |away_team_name            |character |Away team name.                                 |
#'    |away_team_location        |character |Away team city/location.                        |
#'    |away_team_abbrev          |character |Away team abbreviation.                         |
#'    |away_team_display_name    |character |Away team display name.                         |
#'    |away_team_color           |character |Away team primary color hex.                    |
#'    |away_team_alternate_color |character |Away team alternate color hex.                  |
#'    |away_team_logo            |character |Away team logo URL.                             |
#'    |away_team_score           |character |Away team final score.                          |
#'    |away_team_winner          |logical   |Whether the away team won.                      |
#'    |game_id                   |character |ESPN event identifier (echoed from arg).        |
#'    |season                    |integer   |Season end-year.                                |
#'    |season_type               |integer   |Season type code.                               |
#'    |game_date                 |character |Game date-time (ISO 8601).                      |
#'
#' @importFrom dplyr bind_rows case_when
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_pbp(game_id = "401688263"))
#' }
espn_nhl_pbp <- function(game_id, ...) {
  .espn_hockey_pbp(league = "nhl", game_id = game_id, ...)
}


# ===========================================================================
# 2. espn_nhl_team_box — internal + public
# ===========================================================================

#' Internal: ESPN hockey team box score from summary (league-generic)
#'
#' @param league ESPN league slug.
#' @param game_id ESPN event identifier.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per team (2 rows).
#' @noRd
.espn_hockey_team_box <- function(league = "nhl", game_id, ...) {
  .args <- .capture_args()
  team_box <- data.frame()

  tryCatch(
    expr = {
      raw      <- .espn_hockey_fetch_summary(league, game_id, ...)
      team_box <- .parse_espn_hockey_team_box(raw, game_id, league)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} team box score for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} team box score for game_id = {game_id}",
        args = .args)
    },
    finally = {}
  )
  return(team_box)
}

#' @name espn_nhl_team_box
NULL
#' @title **Get ESPN NHL Team Box Score**
#' @rdname espn_nhl_team_box
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per team (2 rows):
#'
#'    |col_name               |types     |description                                       |
#'    |:----------------------|:---------|:-------------------------------------------------|
#'    |team_id                |character |ESPN team identifier.                             |
#'    |team_uid               |character |ESPN team uid.                                    |
#'    |team_slug              |character |Team URL slug.                                    |
#'    |team_location          |character |Team city/location.                               |
#'    |team_name              |character |Team name (mascot).                               |
#'    |team_abbreviation      |character |Team abbreviation.                                |
#'    |team_display_name      |character |Team display name.                                |
#'    |team_short_display_name|character |Team short display name.                          |
#'    |team_color             |character |Team primary color hex.                           |
#'    |team_alternate_color   |character |Team alternate color hex.                         |
#'    |team_logo              |character |Team logo URL.                                    |
#'    |home_away              |character |"home" or "away".                                 |
#'    |game_id                |character |ESPN event identifier (echoed from arg).          |
#'    |season                 |integer   |Season end-year.                                  |
#'    |season_type            |integer   |Season type code.                                 |
#'    |game_date              |character |Game date-time (ISO 8601).                        |
#'    |blocked_shots          |character |Total blocked shots.                              |
#'    |hits                   |character |Total hits.                                       |
#'    |takeaways              |character |Total takeaways.                                  |
#'    |shots_total            |character |Total shots on goal.                              |
#'    |power_play_goals       |character |Power play goals.                                 |
#'    |power_play_opportunities|character|Power play opportunities.                         |
#'    |power_play_pct         |character |Power play percentage.                            |
#'    |short_handed_goals     |character |Short-handed goals.                               |
#'    |shootout_goals         |character |Shootout goals.                                   |
#'    |faceoffs_won           |character |Faceoffs won.                                     |
#'    |faceoff_percent        |character |Faceoff win percentage.                           |
#'    |giveaways              |character |Giveaways.                                        |
#'    |penalties              |character |Penalty count.                                    |
#'    |penalty_minutes        |character |Penalty minutes.                                  |
#'
#' @importFrom dplyr bind_rows case_when
#' @importFrom janitor clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_team_box(game_id = "401688263"))
#' }
espn_nhl_team_box <- function(game_id, ...) {
  .espn_hockey_team_box(league = "nhl", game_id = game_id, ...)
}


# ===========================================================================
# 3. espn_nhl_player_box — internal + public
# ===========================================================================

#' Internal: ESPN hockey player box score from summary (league-generic)
#'
#' @param league ESPN league slug.
#' @param game_id ESPN event identifier.
#' @param ... Passed through.
#' @return A `fastRhockey_data` tibble, one row per player.
#' @noRd
.espn_hockey_player_box <- function(league = "nhl", game_id, ...) {
  .args <- .capture_args()
  player_box <- data.frame()

  tryCatch(
    expr = {
      raw        <- .espn_hockey_fetch_summary(league, game_id, ...)
      player_box <- .parse_espn_hockey_player_box(raw, game_id, league)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} player box score for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} player box score for game_id = {game_id}",
        args = .args)
    },
    finally = {}
  )
  return(player_box)
}

#' @name espn_nhl_player_box
NULL
#' @title **Get ESPN NHL Player Box Score**
#' @rdname espn_nhl_player_box
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A `fastRhockey_data` tibble with one row per player appearance.
#'   Skater and goalie rows are combined via `dplyr::bind_rows`; columns
#'   not applicable to a position group are `NA`. Key columns:
#'
#'    |col_name                        |types     |description                                            |
#'    |:-------------------------------|:---------|:------------------------------------------------------|
#'    |athlete_id                      |character |ESPN athlete identifier.                               |
#'    |athlete_display_name            |character |Athlete full display name.                             |
#'    |athlete_short_name              |character |Athlete short name.                                    |
#'    |athlete_last_name               |character |Athlete last name.                                     |
#'    |athlete_jersey                  |character |Jersey number.                                         |
#'    |athlete_headshot_href           |character |Headshot image URL.                                    |
#'    |athlete_position_name           |character |Position name.                                         |
#'    |athlete_position_abbreviation   |character |Position abbreviation.                                 |
#'    |athlete_active                  |logical   |Whether the athlete was active.                        |
#'    |athlete_scratched               |logical   |Whether the athlete was scratched.                     |
#'    |stat_group                      |character |Stat group name ("forwards","defenses","goalies").     |
#'    |team_id                         |character |ESPN team identifier.                                  |
#'    |team_abbreviation               |character |Team abbreviation.                                     |
#'    |team_display_name               |character |Team display name.                                     |
#'    |home_away                       |character |"home" or "away".                                      |
#'    |team_score                      |character |Team final score.                                      |
#'    |team_winner                     |logical   |Whether the team won.                                  |
#'    |opponent_team_id                |character |Opponent team identifier.                              |
#'    |opponent_team_abbreviation      |character |Opponent team abbreviation.                            |
#'    |game_id                         |character |ESPN event identifier (echoed from arg).               |
#'    |season                          |integer   |Season end-year.                                       |
#'    |season_type                     |integer   |Season type code.                                      |
#'    |game_date                       |character |Game date-time (ISO 8601).                             |
#'    |bs                              |character |Blocked shots (skaters).                               |
#'    |ht                              |character |Hits (skaters).                                        |
#'    |tk                              |character |Takeaways (skaters).                                   |
#'    |x                               |character |Plus/minus rating (skaters; col named "x" after clean_names of "+/-"). |
#'    |toi                             |character |Time on ice (skaters, MM:SS).                          |
#'    |pptoi                           |character |Power play time on ice (skaters, MM:SS).               |
#'    |shtoi                           |character |Short-handed time on ice (skaters, MM:SS).             |
#'    |estoi                           |character |Even-strength time on ice (skaters, MM:SS).            |
#'    |shft                            |character |Total shifts (skaters).                                |
#'    |g                               |character |Goals (skaters).                                       |
#'    |ytdg                            |character |Year-to-date goals (skaters/goalies).                  |
#'    |a                               |character |Assists (skaters).                                     |
#'    |s                               |character |Shots (skaters).                                       |
#'    |sm                              |character |Missed shots (skaters).                                |
#'    |sog                             |character |Shots on goal (skaters).                               |
#'    |fw                              |character |Faceoffs won (skaters).                                |
#'    |fl                              |character |Faceoffs lost (skaters).                               |
#'    |fo_percent                      |character |Faceoff percentage (skaters).                          |
#'    |gv                              |character |Giveaways (skaters).                                   |
#'    |pn                              |character |Penalties taken (skaters).                             |
#'    |pim                             |character |Penalty minutes (skaters/goalies).                     |
#'    |ga                              |character |Goals against (goalies).                               |
#'    |sa                              |character |Shots against (goalies).                               |
#'    |sos                             |character |Shootout saves (goalies).                              |
#'    |sosa                            |character |Shootout shots against (goalies).                      |
#'    |sv                              |character |Saves (goalies).                                       |
#'    |sv_percent                      |character |Save percentage (goalies).                             |
#'    |essv                            |character |Even-strength saves (goalies).                         |
#'    |ppsv                            |character |Power play saves (goalies).                            |
#'    |shsv                            |character |Short-handed saves (goalies).                          |
#'
#' @importFrom dplyr bind_rows case_when
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   try(espn_nhl_player_box(game_id = "401688263"))
#' }
espn_nhl_player_box <- function(game_id, ...) {
  .espn_hockey_player_box(league = "nhl", game_id = game_id, ...)
}


# ===========================================================================
# 4. espn_nhl_game_all — internal + public
#    ONE fetch shared across the three parse helpers.
# ===========================================================================

#' Internal: ESPN hockey game data (pbp + team box + player box) — ONE fetch.
#'
#' @param league ESPN league slug.
#' @param game_id ESPN event identifier.
#' @param ... Passed through.
#' @return A named list with elements `plays`, `team_box`, `player_box`.
#' @noRd
.espn_hockey_game_all <- function(league = "nhl", game_id, ...) {
  .args <- .capture_args()

  result <- list(plays = data.frame(), team_box = data.frame(), player_box = data.frame())
  raw    <- NULL

  tryCatch(
    expr = {
      raw <- .espn_hockey_fetch_summary(league, game_id, ...)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Could not fetch ESPN {league} summary for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} summary for game_id = {game_id}",
        args = .args)
    },
    finally = {}
  )

  if (is.null(raw)) return(result)

  tryCatch(
    expr = {
      result[["plays"]] <- .parse_espn_hockey_pbp(raw, game_id, league)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Could not parse ESPN {league} plays for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {},
    finally = {}
  )

  tryCatch(
    expr = {
      result[["team_box"]] <- .parse_espn_hockey_team_box(raw, game_id, league)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Could not parse ESPN {league} team box for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {},
    finally = {}
  )

  tryCatch(
    expr = {
      result[["player_box"]] <- .parse_espn_hockey_player_box(raw, game_id, league)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Could not parse ESPN {league} player box for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {},
    finally = {}
  )

  return(result)
}

#' @name espn_nhl_game_all
NULL
#' @title **Get ESPN NHL Game Data (PBP + Team Box + Player Box)**
#' @rdname espn_nhl_game_all
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A named list with three elements, all parsed from a **single** HTTP
#'   request to the ESPN summary endpoint:
#'
#'   - **`plays`** — `fastRhockey_data` tibble, one row per play.
#'     See [espn_nhl_pbp()] for the full column reference.
#'
#'   - **`team_box`** — `fastRhockey_data` tibble, one row per team (2 rows).
#'     See [espn_nhl_team_box()] for the full column reference.
#'
#'   - **`player_box`** — `fastRhockey_data` tibble, one row per player.
#'     See [espn_nhl_player_box()] for the full column reference.
#'
#' @importFrom dplyr bind_rows case_when
#' @importFrom janitor clean_names make_clean_names
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   g <- try(espn_nhl_game_all(game_id = "401688263"))
#'   if (!inherits(g, "try-error")) {
#'     names(g)           # "plays" "team_box" "player_box"
#'     nrow(g$plays)
#'     nrow(g$team_box)
#'     nrow(g$player_box)
#'   }
#' }
espn_nhl_game_all <- function(game_id, ...) {
  .espn_hockey_game_all(league = "nhl", game_id = game_id, ...)
}


# ===========================================================================
# 5. espn_nhl_summary — internal + public (raw nested list)
# ===========================================================================

#' Internal: ESPN hockey raw summary (league-generic)
#'
#' @param league ESPN league slug.
#' @param game_id ESPN event identifier.
#' @param ... Passed through.
#' @return The raw parsed JSON as a nested R list.
#' @noRd
.espn_hockey_summary <- function(league = "nhl", game_id, ...) {
  .args <- .capture_args()
  result <- list()

  tryCatch(
    expr = {
      result <- .espn_hockey_fetch_summary(league, game_id, ...)
    },
    error = function(e) {
      .report_api_error(e,
        hint = "Invalid arguments or no ESPN {league} summary for game_id = {game_id}",
        args = .args)
    },
    warning = function(w) {
      .report_api_warning(w,
        hint = "Warning fetching ESPN {league} summary for game_id = {game_id}",
        args = .args)
    },
    finally = {}
  )
  return(result)
}

#' @name espn_nhl_summary
NULL
#' @title **Get ESPN NHL Raw Game Summary**
#' @rdname espn_nhl_summary
#' @author Saiem Gilani
#' @param game_id ESPN event identifier (character or numeric).
#' @param ... Reserved for forward compatibility.
#' @return A nested R list representing the full parsed ESPN summary JSON.
#'   **This function does NOT return a tibble.** It is intended for power
#'   users who need fields not exposed by [espn_nhl_pbp()],
#'   [espn_nhl_team_box()], or [espn_nhl_player_box()].
#'
#'   Top-level keys typically include:
#'   `boxscore`, `format`, `gameInfo`, `leaders`, `seasonseries`,
#'   `broadcasts`, `pickcenter`, `odds`, `plays`, `header`, `standings`,
#'   and others.
#'
#'   - `$boxscore$teams` — list of 2 team stat objects.
#'   - `$boxscore$players` — list of 2 player stat group objects.
#'   - `$plays` — list of play objects.
#'   - `$header$season` — season year and type.
#'   - `$header$competitions[[1]]$competitors` — home/away team + score.
#'
#' @importFrom jsonlite fromJSON
#' @export
#' @family ESPN NHL Functions
#' @examples
#' \donttest{
#'   s <- try(espn_nhl_summary(game_id = "401688263"))
#'   if (!inherits(s, "try-error")) {
#'     names(s)
#'     length(s$plays)
#'   }
#' }
espn_nhl_summary <- function(game_id, ...) {
  .espn_hockey_summary(league = "nhl", game_id = game_id, ...)
}
