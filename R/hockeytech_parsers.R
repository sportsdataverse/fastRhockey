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
    full <- regmatches(name, m)
    pieces <- strsplit(full, "-")[[1]]
    start_century <- as.integer(substr(pieces[1], 1L, 2L))
    end_short <- as.integer(pieces[2])
    return(start_century * 100L + end_short)
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
