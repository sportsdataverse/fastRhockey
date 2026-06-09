#' @keywords internal
#' Convert a "MM:SS" countdown-clock string to total integer seconds.
#'
#' Returns NA_integer_ for NA or empty-string inputs. Countdown-clock values
#' (e.g., "03:16") are converted exactly as supplied -- the caller is
#' responsible for any inversion logic.
#'
#' @param x A character string of the form "MM:SS" (e.g., "03:16").
#' @return An integer, or NA_integer_ when x is NA or "".
#' @noRd
.mmss_to_seconds <- function(x) {
  if (is.na(x) || identical(x, "")) return(NA_integer_)
  parts <- strsplit(as.character(x), ":", fixed = TRUE)[[1]]
  if (length(parts) != 2L) return(NA_integer_)
  m <- suppressWarnings(as.integer(parts[1]))
  s <- suppressWarnings(as.integer(parts[2]))
  if (is.na(m) || is.na(s)) return(NA_integer_)
  m * 60L + s
}


#' @keywords internal
#' Derive the end-year (integer) from a season name string.
#'
#' Handles both "YYYY-YY" (e.g., "2024-25 Regular Season" -> 2025) and
#' standalone "YYYY" (e.g., "2024 Regular Season" -> 2024).
#'
#' @param name A season name character string.
#' @return An integer year, or NA_integer_ if no year pattern is matched.
#' @noRd
.derive_season_year <- function(name) {
  if (is.na(name) || !nzchar(name)) return(NA_integer_)
  # Try "YYYY-YY" format first (e.g., "2024-25 Regular Season")
  m <- regexpr("(\\d{4})-(\\d{2})", name, perl = TRUE)
  if (m > 0L) {
    full   <- regmatches(name, m)
    pieces <- strsplit(full, "-")[[1]]
    start  <- as.integer(pieces[1])
    end2   <- as.integer(pieces[2])
    end    <- (start %/% 100L) * 100L + end2
    if (end < start) end <- end + 100L
    return(end)
  }
  # Try standalone "YYYY" format
  m2 <- regexpr("(\\d{4})", name, perl = TRUE)
  if (m2 > 0L) {
    return(as.integer(regmatches(name, m2)))
  }
  NA_integer_
}


#' @keywords internal
#' Derive a game-type label from a HockeyTech season name string.
#'
#' Named with the .ht_ prefix to avoid collision with the NHL .game_type_label
#' internal helper in nhl_game_feed.R (which converts numeric game-type codes).
#'
#' @param name A season name character string.
#' @return One of "preseason", "playoffs", or "regular".
#' @noRd
.ht_game_type_label <- function(name) {
  n <- tolower(name %||% "")
  if (grepl("pre[- ]?season", n, perl = TRUE)) return("preseason")
  if (grepl("playoff|post",   n, perl = TRUE)) return("playoffs")
  "regular"
}


#' @keywords internal
#' Parse a HockeyTech modulekit/seasons JSON payload into a data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_seasons().
#' Reads payload$SiteKit$Seasons (a list of season dicts) and returns a
#' data.frame with columns: season_id, season_name, season_short, career,
#' playoff, start_date, end_date, season_yr, game_type_label.
#' An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame, one row per season.
#' @noRd
.parse_hockeytech_seasons <- function(payload) {
  raw <- ((payload %||% list())$SiteKit %||% list())$Seasons %||% list()
  if (length(raw) == 0L) return(data.frame())

  rows <- vector("list", length(raw))
  for (i in seq_along(raw)) {
    s    <- raw[[i]]
    name <- s$season_name %||% NA_character_
    rows[[i]] <- data.frame(
      season_id       = if (!is.null(s$season_id)) as.numeric(s$season_id) else NA_real_,
      season_name     = as.character(name),
      season_short    = as.character(s$shortname %||% NA_character_),
      career          = as.character(s$career  %||% "0"),
      playoff         = as.character(s$playoff %||% "0"),
      start_date      = as.character(s$start_date %||% NA_character_),
      end_date        = as.character(s$end_date   %||% NA_character_),
      season_yr       = .derive_season_year(name),
      game_type_label = .ht_game_type_label(name),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}


#' @keywords internal
#' Parse a HockeyTech modulekit/gameshifts JSON payload into a data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_shifts().
#' Reads payload$SiteKit$Gameshifts$home and $visitor (lists of player dicts,
#' each with a $shifts list of shift dicts). Emits one row per player-shift
#' stint with start_s/end_s as countdown-clock seconds (start_s >= end_s).
#' An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @param game_id Optional game identifier echoed onto every row.
#' @return A data.frame, one row per shift stint.
#' @noRd
#' @keywords internal
#' Extract a flat player sub-dict from a raw player sub-object (or NULL).
#'
#' Mirrors Python _parsers.py::_player(). Returns a list with id/first/last/pos,
#' all NA when the input is NULL or missing fields.
#'
#' @param d A list (player sub-object from parsed JSON) or NULL.
#' @return A named list: id, first, last, pos.
#' @noRd
.ht_player <- function(d) {
  d <- d %||% list()
  list(
    id    = d$id    %||% NA,
    first = as.character(d$firstName %||% NA_character_),
    last  = as.character(d$lastName  %||% NA_character_),
    pos   = as.character(d$position  %||% NA_character_)
  )
}


#' @keywords internal
#' Coerce a value to character, returning NA_character_ for NULL/empty.
#'
#' Mirrors Python _parsers.py::_str_or_none().
#'
#' @param v Any scalar value.
#' @return character(1) or NA_character_.
#' @noRd
.ht_str_or_na <- function(v) {
  if (is.null(v)) return(NA_character_)
  s <- as.character(v)
  if (!nzchar(s)) NA_character_ else s
}


#' @keywords internal
#' Parse a HockeyTech play-by-play payload (list of event dicts) into a
#' data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_pbp() /
#' _parse_pbp_a(). Both dialect-a (PWHL/AHL) and dialect-b (OHL/WHL/QMJHL)
#' use the same nested {"event": ..., "details": {...}} structure, so one
#' parser handles both. The pbp_style argument is kept for signature parity.
#'
#' Column mapping:
#'   event, team_id, period_of_game, time_of_period, x_coord, y_coord
#'   player_id/player_name_first/player_name_last/player_position
#'   goal (logical), goalie_id/goalie_first/goalie_last
#'   event_type, shot_quality, penalty_length, power_play
#'   empty_net, game_winner, penalty_shot, insurance, short_handed
#'   player_two_*/player_three_* (assists / penalty secondary)
#'   plus_player_one..five_* / minus_player_one..five_* (word ordinals)
#'
#' Penalty events: servedBy -> player_id, takenBy -> player_two_*.
#' An empty payload returns an empty data.frame().
#'
#' @param payload A list of event dicts (parsed JSON from gameCenterPlayByPlay).
#' @param pbp_style Character; dialect flag kept for signature parity.
#'   Both "hockeytech_a" and "hockeytech_b" are handled identically.
#' @param game_id Optional game identifier echoed onto every row as game_id.
#' @return A data.frame, one row per event. Column set is union of all
#'   event-type columns; missing columns for a given event type are NA.
#' @noRd
.parse_hockeytech_pbp <- function(payload, pbp_style = "hockeytech_a",
                                   game_id = NULL) {
  events <- if (is.list(payload)) payload else list()
  if (length(events) == 0L) return(data.frame())

  .ORDINALS <- c("one", "two", "three", "four", "five")

  rows <- vector("list", length(events))

  for (i in seq_along(events)) {
    e <- events[[i]]
    if (!is.list(e)) next

    ev <- e[["event"]]
    d  <- e[["details"]] %||% list()

    # -- period_of_game --------------------------------------------------
    period_raw <- d[["period"]]
    if (is.list(period_raw)) {
      period <- period_raw[["id"]]
    } else {
      period <- period_raw
    }

    # -- team_id ---------------------------------------------------------
    raw_team <- d[["team_id"]] %||% d[["shooterTeamId"]] %||% d[["teamId"]]
    team_id  <- .ht_str_or_na(raw_team)

    # -- base row (defaults; overridden per event type below) ------------
    base <- list(
      game_id           = game_id,
      event             = ev,
      team_id           = team_id,
      period_of_game    = as.character(period %||% NA_character_),
      time_of_period    = as.character(d[["time"]] %||% NA_character_),
      x_coord           = d[["xLocation"]] %||% NA_real_,
      y_coord           = d[["yLocation"]] %||% NA_real_,
      player_id         = NA,
      player_name_first = NA_character_,
      player_name_last  = NA_character_,
      player_position   = NA_character_,
      goal              = NA,
      goalie_id         = NA,
      goalie_first      = NA_character_,
      goalie_last       = NA_character_
    )

    # -- per-event-type overrides ----------------------------------------
    if (!is.null(ev) && ev %in% c("shot", "blocked_shot")) {
      sh <- .ht_player(d[["shooter"]])
      gl <- .ht_player(d[["goalie"]])
      base$player_id         <- sh$id
      base$player_name_first <- sh$first
      base$player_name_last  <- sh$last
      base$player_position   <- sh$pos
      base$player_team_id    <- .ht_str_or_na(d[["shooterTeamId"]])
      base$event_type        <- as.character(d[["shotType"]]    %||% NA_character_)
      base$shot_quality      <- as.character(d[["shotQuality"]] %||% NA_character_)
      base$goal              <- if (!is.null(ev) && ev == "shot") {
        isTRUE(as.logical(d[["isGoal"]]))
      } else {
        FALSE
      }
      base$goalie_id   <- gl$id
      base$goalie_first <- gl$first
      base$goalie_last  <- gl$last

    } else if (!is.null(ev) && ev == "goal") {
      sc      <- .ht_player(d[["scoredBy"]])
      assists <- d[["assists"]] %||% list()
      props   <- d[["properties"]] %||% list()
      team_obj  <- d[["team"]] %||% list()
      team_id_g <- .ht_str_or_na(team_obj[["id"]]) %||% team_id

      base$player_id         <- sc$id
      base$player_name_first <- sc$first
      base$player_name_last  <- sc$last
      base$player_position   <- sc$pos
      base$team_id           <- team_id_g
      base$goal              <- TRUE
      base$empty_net         <- props[["isEmptyNet"]]  %||% NA
      base$game_winner       <- props[["isGameWinningGoal"]] %||% NA
      base$penalty_shot      <- props[["isPenaltyShot"]]     %||% NA
      base$insurance         <- props[["isInsuranceGoal"]]   %||% NA
      base$short_handed      <- props[["isShortHanded"]]     %||% NA
      base$power_play        <- props[["isPowerPlay"]]        %||% NA

      # assists: player_two = primary, player_three = secondary
      assist_names <- c("two", "three")
      for (j in seq_along(assists)) {
        if (j > 2L) break
        pa <- .ht_player(assists[[j]])
        ord <- assist_names[[j]]
        base[[paste0("player_", ord, "_id")]]         <- pa$id
        base[[paste0("player_", ord, "_name_first")]] <- pa$first
        base[[paste0("player_", ord, "_name_last")]]  <- pa$last
        base[[paste0("player_", ord, "_position")]]   <- pa$pos
      }

      # plus/minus players with WORD ordinals (up to 5)
      for (sign in c("plus", "minus")) {
        key <- paste0(sign, "_players")
        players_list <- d[[key]] %||% list()
        n_p <- min(length(players_list), 5L)
        for (j in seq_len(n_p)) {
          ord <- .ORDINALS[[j]]
          pp  <- .ht_player(players_list[[j]])
          base[[paste0(sign, "_player_", ord, "_id")]]       <- pp$id
          base[[paste0(sign, "_player_", ord, "_first")]]    <- pp$first
          base[[paste0(sign, "_player_", ord, "_last")]]     <- pp$last
          base[[paste0(sign, "_player_", ord, "_position")]] <- pp$pos
        }
      }

    } else if (!is.null(ev) && ev == "faceoff") {
      hp <- .ht_player(d[["homePlayer"]])
      base$player_id         <- hp$id
      base$player_name_first <- hp$first
      base$player_name_last  <- hp$last
      base$player_position   <- hp$pos
      base$home_win          <- d[["homeWin"]] %||% NA

    } else if (!is.null(ev) && ev == "hit") {
      pl_info <- .ht_player(d[["player"]])
      base$player_id         <- pl_info$id
      base$player_name_first <- pl_info$first
      base$player_name_last  <- pl_info$last
      base$player_position   <- pl_info$pos
      base$team_id           <- .ht_str_or_na(d[["teamId"]]) %||% team_id

    } else if (!is.null(ev) && ev == "penalty") {
      # servedBy -> primary player (player_id), takenBy -> player_two_*
      sb <- .ht_player(d[["servedBy"]])
      tb <- .ht_player(d[["takenBy"]])
      against <- d[["againstTeam"]] %||% list()

      base$player_id             <- sb$id
      base$player_name_first     <- sb$first
      base$player_name_last      <- sb$last
      base$player_position       <- sb$pos
      base$player_two_id         <- tb$id
      base$player_two_name_first <- tb$first
      base$player_two_name_last  <- tb$last
      base$player_two_position   <- tb$pos
      base$team_id               <- .ht_str_or_na(against[["id"]])
      base$penalty_length        <- as.character(d[["minutes"]] %||% NA_character_)
      base$event_type            <- as.character(d[["description"]] %||% NA_character_)
      base$power_play            <- if (isTRUE(d[["isPowerPlay"]]) ||
                                        identical(d[["isPowerPlay"]], "1") ||
                                        identical(d[["isPowerPlay"]], 1L) ||
                                        identical(d[["isPowerPlay"]], 1)) "1" else "0"

    } else if (!is.null(ev) && ev == "goalie_change") {
      gc <- .ht_player(d[["goalieComingIn"]])
      base$goalie_id    <- gc$id
      base$goalie_first <- gc$first
      base$goalie_last  <- gc$last
    }
    # default branch: identity — just emit base row for unrecognised event types

    rows[[i]] <- base
  }

  # Remove NULLs (skipped non-list entries)
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(data.frame())

  dplyr::bind_rows(rows)
}


.parse_hockeytech_shifts <- function(payload, game_id = NULL) {
  gs <- ((payload %||% list())$SiteKit %||% list())$Gameshifts %||% list()

  rows <- list()
  for (side in c("home", "visitor")) {
    players <- gs[[side]] %||% list()
    for (player in players) {
      shifts <- player$shifts %||% list()
      for (sh in shifts) {
        rows[[length(rows) + 1L]] <- list(
          game_id           = game_id,
          player_id         = player$player_id %||% NA,
          first_name        = as.character(player$first_name %||% NA_character_),
          last_name         = as.character(player$last_name  %||% NA_character_),
          jersey_number     = as.character(player$jersey_number %||% NA_character_),
          home              = as.integer(player$home %||% if (side == "home") 1L else 0L),
          period            = if (!is.null(sh$period)) as.integer(sh$period) else NA_integer_,
          start_time        = as.character(sh$start_time %||% NA_character_),
          end_time          = as.character(sh$end_time   %||% NA_character_),
          length            = as.character(sh$length     %||% NA_character_),
          start_s           = .mmss_to_seconds(sh$start_time %||% NA_character_),
          end_s             = .mmss_to_seconds(sh$end_time   %||% NA_character_),
          goal_on_shift     = as.integer(sh$goal_on_shift    %||% 0L),
          penalty_on_shift  = as.integer(sh$penalty_on_shift %||% 0L)
        )
      }
    }
  }

  if (length(rows) == 0L) return(data.frame())
  dplyr::bind_rows(rows)
}


# ===========================================================================
# New parsers (B3.1a): schedule, standings, teams, roster, player_stats,
# leaders, game_summary. Mirror Python _parsers.py behaviour exactly.
# ===========================================================================

#' @keywords internal
#' Parse a HockeyTech modulekit/scorebar JSON payload into a data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_schedule().
#' Reads payload$SiteKit$Scorebar and applies the canonical _SCOREBAR_RENAME
#' mapping plus game_type pass-through.
#' An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame, one row per game.
#' @noRd
.parse_hockeytech_schedule <- function(payload) {
  games <- ((payload %||% list())$SiteKit %||% list())$Scorebar %||% list()
  if (length(games) == 0L) return(data.frame())

  rows <- vector("list", length(games))
  for (i in seq_along(games)) {
    g <- games[[i]]
    rows[[i]] <- list(
      game_id      = g[["ID"]],
      game_date    = as.character(g[["GameDateISO8601"]] %||% NA_character_),
      game_status  = as.character(g[["GameStatusStringLong"]] %||% NA_character_),
      home_team    = as.character(g[["HomeLongName"]]     %||% NA_character_),
      home_team_id = g[["HomeID"]],
      home_score   = g[["HomeGoals"]],
      away_team    = as.character(g[["VisitorLongName"]]  %||% NA_character_),
      away_team_id = g[["VisitorID"]],
      away_score   = g[["VisitorGoals"]],
      venue        = as.character(g[["venue_name"]]       %||% NA_character_),
      season_id    = g[["SeasonID"]],
      game_type    = g[["game_type"]]
    )
  }
  dplyr::bind_rows(rows)
}


#' @keywords internal
#' Parse a HockeyTech statviewfeed/teams (standings) JSON payload into a
#' data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_standings().
#' The payload may be:
#'   - A list of section dicts (each with data[].row sub-structure)
#'   - A single dict with a "sections" key containing that list
#' Renames rank -> team_rank, name -> team. Computes wins = regulation_wins +
#' non_reg_wins (PWHL 3-2-1-0 system). An empty or NULL payload returns an
#' empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame, one row per team.
#' @noRd
.parse_hockeytech_standings <- function(payload) {
  if (is.null(payload) || length(payload) == 0L) return(data.frame())

  # Normalise to a list of section dicts.
  # Python: sections = payload if isinstance(payload, list) else (payload or {}).get("sections", [])
  if (is.list(payload) && !is.null(names(payload))) {
    # Named list -> single dict with optional "sections" key
    sections <- payload[["sections"]] %||% list()
  } else if (is.list(payload) && is.null(names(payload))) {
    # Unnamed list -> treat as list of section dicts directly
    sections <- payload
  } else {
    sections <- list()
  }

  rows <- list()
  for (sec in sections) {
    if (!is.list(sec)) next

    # Handle two levels: outer section may have inner sections (like Python's
    # `for blk in sec.get("sections", [sec])`) or data directly.
    inner_sections <- sec[["sections"]]
    if (!is.null(inner_sections) && is.list(inner_sections) && length(inner_sections) > 0L) {
      blocks <- inner_sections
    } else {
      blocks <- list(sec)
    }

    for (blk in blocks) {
      data_items <- blk[["data"]] %||% list()
      for (item in data_items) {
        r <- if (is.list(item)) item[["row"]] else NULL
        if (is.list(r) && length(r) > 0L) {
          rows[[length(rows) + 1L]] <- r
        }
      }
    }
  }

  if (length(rows) == 0L) return(data.frame())

  df <- dplyr::bind_rows(rows)

  # Rename rank -> team_rank, name -> team
  if ("rank" %in% names(df)) names(df)[names(df) == "rank"] <- "team_rank"
  if ("name" %in% names(df)) names(df)[names(df) == "name"] <- "team"

  # Coerce numeric-like standings columns
  for (col in c("regulation_wins", "non_reg_wins", "non_reg_losses",
                "games_played", "points", "losses")) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  # Compute total wins = regulation_wins + non_reg_wins (PWHL 3-2-1-0)
  if ("regulation_wins" %in% names(df) && "non_reg_wins" %in% names(df)) {
    df$wins <- df$regulation_wins + df$non_reg_wins
  }

  df
}


#' @keywords internal
#' Parse a HockeyTech modulekit/teamsbyseason JSON payload into a data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_teams().
#' Reads payload$SiteKit$Teamsbyseason and maps to canonical column names:
#' team_name, team_id, team_code, team_nickname, team_label (city), division,
#' team_logo. An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame, one row per team.
#' @noRd
.parse_hockeytech_teams <- function(payload) {
  raw <- ((payload %||% list())$SiteKit %||% list())$Teamsbyseason %||% list()
  if (length(raw) == 0L) return(data.frame())

  rows <- vector("list", length(raw))
  for (i in seq_along(raw)) {
    t <- raw[[i]]
    rows[[i]] <- list(
      team_name     = as.character(t[["name"]]     %||% NA_character_),
      team_id       = t[["id"]],
      team_code     = as.character(t[["code"]]     %||% NA_character_),
      team_nickname = as.character(t[["nickname"]] %||% NA_character_),
      team_label    = as.character(t[["city"]]     %||% NA_character_),
      division      = t[["division_id"]] %||% t[["division"]],
      team_logo     = as.character(t[["team_logo_url"]] %||% t[["logo"]] %||% NA_character_)
    )
  }
  dplyr::bind_rows(rows)
}


#' @keywords internal
#' Parse a HockeyTech modulekit/roster JSON payload into a data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_roster().
#' Reads payload$SiteKit$Roster, skipping non-dict (non-list) entries such as
#' the coaching-staff sub-list. List-valued fields (e.g. draftinfo) are dropped
#' before row binding because they cannot be coerced to scalar columns.
#' An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame, one row per player.
#' @noRd
.parse_hockeytech_roster <- function(payload) {
  raw <- ((payload %||% list())$SiteKit %||% list())$Roster %||% list()
  if (length(raw) == 0L) return(data.frame())

  rows <- list()
  for (player in raw) {
    # Skip non-dict entries (coaching-staff sub-lists etc.)
    if (!is.list(player) || is.null(names(player))) next
    # Drop list-valued fields (e.g. draftinfo)
    flat <- player[vapply(player, function(v) !is.list(v), logical(1))]
    if (length(flat) > 0L) rows[[length(rows) + 1L]] <- flat
  }

  if (length(rows) == 0L) return(data.frame())
  df <- dplyr::bind_rows(rows)
  # Apply snake_case via janitor if available, otherwise leave as-is
  if (requireNamespace("janitor", quietly = TRUE)) {
    df <- janitor::clean_names(df)
  }
  df
}


#' @keywords internal
#' Parse a HockeyTech modulekit/player (seasonstats) JSON payload into a
#' data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_player_stats().
#' SiteKit$Player is a dict with regular/exhibition/playoff sub-lists; all are
#' concatenated with a stat_type column. Scalar values are coerced to character
#' to avoid mixed-type column errors (matches Python's str() coercion).
#' An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame with one row per season-stat entry.
#' @noRd
.parse_hockeytech_player_stats <- function(payload) {
  player <- ((payload %||% list())$SiteKit %||% list())$Player %||% list()
  if (length(player) == 0L) return(data.frame())

  rows <- list()
  for (stat_type in c("regular", "exhibition", "playoff")) {
    sub <- player[[stat_type]]
    if (is.null(sub) || !is.list(sub)) next
    for (season in sub) {
      if (!is.list(season) || is.null(names(season))) next
      # Coerce all scalar values to character (matches Python str() coercion)
      row <- lapply(season, function(v) {
        if (is.null(v) || is.list(v)) v
        else as.character(v)
      })
      row[["stat_type"]] <- stat_type
      rows[[length(rows) + 1L]] <- row
    }
  }

  if (length(rows) == 0L) return(data.frame())
  dplyr::bind_rows(rows)
}


#' @keywords internal
#' Parse a HockeyTech leaders payload into a data.frame.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_leaders().
#' Handles two payload shapes:
#'   Shape 1: SiteKit$Statviewtype -- list of flat player dicts.
#'   Shape 2: Top-level skaters/goalies categories with results lists (as in
#'            the captured pwhl_leaders_5.json fixture).
#' For Shape 2, each result is treated as a flat player dict (the fixture shows
#' flat dicts directly in results, not wrapped under a "player" key).
#' An empty or NULL payload returns an empty data.frame().
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @return A data.frame, one row per player entry.
#' @noRd
.parse_hockeytech_leaders <- function(payload) {
  if (is.null(payload) || length(payload) == 0L) return(data.frame())

  # Shape 1: SiteKit.Statviewtype
  stat_view <- ((payload %||% list())$SiteKit %||% list())$Statviewtype
  if (!is.null(stat_view)) {
    rows <- Filter(function(p) is.list(p) && !is.null(names(p)), stat_view)
    if (length(rows) == 0L) return(data.frame())
    return(dplyr::bind_rows(rows))
  }

  # Shape 2: top-level skaters/goalies categories (leadersExtended-style)
  rows <- list()
  top <- if (is.list(payload) && !is.null(names(payload))) payload else list()
  for (pos_key in c("skaters", "goalies")) {
    pos_data <- top[[pos_key]]
    if (!is.list(pos_data) || is.null(names(pos_data))) next
    for (cat_key in names(pos_data)) {
      cat_val <- pos_data[[cat_key]]
      if (!is.list(cat_val)) next
      results <- cat_val[["results"]] %||% list()
      for (entry in results) {
        if (!is.list(entry)) next
        # Entry may be flat OR wrap player under a "player" key
        player <- entry[["player"]] %||% entry
        if (is.list(player) && !is.null(names(player))) {
          rows[[length(rows) + 1L]] <- player
        }
      }
    }
  }

  if (length(rows) == 0L) return(data.frame())
  dplyr::bind_rows(rows)
}


#' @keywords internal
#' Flatten one row dict by expanding named sub-lists (dicts) with "_" separator
#' and dropping unnamed sub-lists (arrays like plus/minus player arrays).
#'
#' This mirrors Python pd.json_normalize(records, sep="_") for one record.
#' Recursion is limited to one level deep (dict fields may themselves contain
#' named sub-lists which are expanded, but arrays within those are dropped).
#'
#' @param row A named list representing one raw row from a parsed payload.
#' @param prefix Character prefix to prepend (used for recursive calls).
#' @return A flat named list suitable for dplyr::bind_rows().
#' @noRd
.ht_flatten_row <- function(row, prefix = "") {
  if (!is.list(row)) return(row)
  out <- list()
  for (key in names(row)) {
    val   <- row[[key]]
    fkey  <- if (nzchar(prefix)) paste0(prefix, "_", key) else key
    if (is.null(val) || length(val) == 0L) {
      out[[fkey]] <- NA
    } else if (is.list(val) && !is.null(names(val))) {
      # Named sub-list (dict): expand recursively with prefix
      sub_flat <- .ht_flatten_row(val, prefix = fkey)
      out <- c(out, sub_flat)
    } else if (is.list(val) && is.null(names(val))) {
      # Unnamed sub-list (array like plus/minus): drop it
      # (matches pd.json_normalize which would error on bare arrays)
    } else {
      # Scalar or atomic vector length-1
      out[[fkey]] <- val
    }
  }
  out
}


#' @keywords internal
#' Normalise shotsByPeriod into a list of flat row dicts.
#'
#' The PWHL gc/gamesummary endpoint returns:
#'   {"visitor": {"1": 11, "2": 11, "3": 9}, "home": {"1": 10, "2": 13, ...}}
#' This converts that to one row per (side, period) pair. If sbp is already a
#' list (old/alternate dialect), it is returned unchanged. NULL yields list().
#'
#' @param sbp The raw shotsByPeriod field from GC.Gamesummary.
#' @return A list of row dicts for dplyr::bind_rows().
#' @noRd
.ht_shots_by_period_to_records <- function(sbp) {
  if (is.null(sbp)) return(list())
  # Already a list of row dicts
  if (is.list(sbp) && (is.null(names(sbp)) || !any(c("visitor", "home") %in% names(sbp)))) {
    return(sbp)
  }
  # Dict format: {visitor: {period: shots}, home: {period: shots}}
  if (is.list(sbp) && !is.null(names(sbp))) {
    rows <- list()
    for (side in c("visitor", "home")) {
      per_period <- sbp[[side]]
      if (!is.list(per_period) || is.null(names(per_period))) next
      for (period in names(per_period)) {
        rows[[length(rows) + 1L]] <- list(
          side   = side,
          period = period,
          shots  = per_period[[period]]
        )
      }
    }
    return(rows)
  }
  list()
}


#' @keywords internal
#' Parse a HockeyTech gc/gamesummary JSON payload into a named list of
#' data.frames.
#'
#' Mirrors Python sportsdataverse/hockeytech/_parsers.py::parse_game_summary().
#' Returns a list with five data.frame elements:
#'   game          -- one-row header (date, status, venue, attendance, scores).
#'   goals         -- scoring summary (one row per goal).
#'   penalties     -- penalty summary (one row per penalty).
#'   shots_by_period -- shots breakdown (one row per side/period pair).
#'   three_stars   -- post-game three-star selections (falls back to mvps).
#'
#' The PWHL gc/gamesummary response nests all data under GC.Gamesummary. The
#' visitor/home keys are flat team dicts; totalGoals carries the final score;
#' shotsByPeriod is a {visitor: {period: shots}, home: {period: shots}} dict.
#' When threeStars is empty, mvps is used as fallback.
#'
#' An empty or NULL payload returns the five-key list with zero-row frames
#' (except game which has one row with only game_id set).
#'
#' @param payload Parsed JSON list from .hockeytech_api().
#' @param game_id Optional game identifier echoed onto the game row.
#' @return A named list of data.frames.
#' @noRd
.parse_hockeytech_game_summary <- function(payload, game_id = NULL) {
  gc_root <- ((payload %||% list())[["GC"]] %||% list())

  # Prefer GC.Gamesummary (live PWHL path); fall back to direct GC keys
  if (!is.null(gc_root[["Gamesummary"]])) {
    summary <- gc_root[["Gamesummary"]] %||% list()
  } else if (!is.null(gc_root[["details"]]) || !is.null(gc_root[["homeTeam"]])) {
    summary <- gc_root
  } else {
    summary <- gc_root[["Gamesummary"]] %||% list()
  }

  # Team info: live path -> flat home/visitor dicts;
  # alternate -> homeTeam/visitingTeam with info/stats sub-keys.
  home_raw <- summary[["home"]] %||% summary[["homeTeam"]] %||% list()
  away_raw <- summary[["visitor"]] %||% summary[["visitingTeam"]] %||% list()
  home_info  <- home_raw[["info"]] %||% home_raw
  away_info  <- away_raw[["info"]] %||% away_raw
  home_stats <- home_raw[["stats"]] %||% list()
  away_stats <- away_raw[["stats"]] %||% list()

  details     <- summary[["details"]] %||% list()
  total_goals <- summary[["totalGoals"]] %||% list()

  game_row <- list(
    game_id    = game_id,
    date       = as.character(
      summary[["game_date"]] %||% details[["date"]] %||%
      summary[["date_played"]] %||% NA_character_
    ),
    status     = as.character(
      summary[["status_value"]] %||% details[["status"]] %||%
      summary[["status"]] %||% NA_character_
    ),
    venue      = as.character(summary[["venue"]] %||% details[["venue"]] %||% NA_character_),
    attendance = summary[["attendance"]] %||% details[["attendance"]],
    home_team    = as.character(home_info[["name"]] %||% NA_character_),
    home_team_id = home_info[["id"]] %||% home_info[["team_id"]],
    home_score   = home_stats[["goals"]] %||% total_goals[["home"]],
    away_team    = as.character(away_info[["name"]] %||% NA_character_),
    away_team_id = away_info[["id"]] %||% away_info[["team_id"]],
    away_score   = away_stats[["goals"]] %||% total_goals[["visitor"]]
  )

  goals_raw    <- summary[["goals"]]    %||% list()
  penalties_raw <- summary[["penalties"]] %||% list()

  # shotsByPeriod: dict {side: {period: shots}} or plain list
  sbp_raw <- .ht_shots_by_period_to_records(
    summary[["shotsByPeriod"]] %||% summary[["shots_by_period"]]
  )

  # threeStars falls back to mvps
  stars_raw <- summary[["threeStars"]] %||% summary[["three_stars"]] %||%
               summary[["mvps"]] %||% list()

  # Ensure lists (not NULL) before bind_rows
  if (!is.list(goals_raw))    goals_raw    <- list()
  if (!is.list(penalties_raw)) penalties_raw <- list()
  if (!is.list(stars_raw))    stars_raw    <- list()

  to_df <- function(lst) {
    if (length(lst) == 0L) return(data.frame())
    # Flatten each row: named sub-lists (dicts) are expanded with "_" separator;
    # unnamed sub-lists (arrays like plus/minus) are dropped. This mirrors
    # Python's pd.json_normalize(records, sep="_") behaviour.
    flat_lst <- lapply(lst, .ht_flatten_row)
    dplyr::bind_rows(flat_lst)
  }

  list(
    game           = dplyr::bind_rows(list(game_row)),
    goals          = to_df(goals_raw),
    penalties      = to_df(penalties_raw),
    shots_by_period = to_df(sbp_raw),
    three_stars    = to_df(stars_raw)
  )
}
