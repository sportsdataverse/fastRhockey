# Pure HockeyTech analytics: data.frame(s) -> data.frame. No network.
#
# Corsi/Fenwick caveat: the HockeyTech feed has no missed-shot event, so shot
# attempts = shot + blocked_shot + goal. Both metrics are proxies; outputs carry
# corsi_includes_missed = FALSE.

.SHOT_EVENTS_HT    <- c("shot", "blocked_shot", "goal")
.FENWICK_EVENTS_HT <- c("shot", "goal")

# ---------------------------------------------------------------------------
# Shot geometry
# ---------------------------------------------------------------------------

#' Add shot_distance and shot_angle columns to a PBP data frame.
#'
#' For rows whose `event` is one of shot/blocked_shot/goal:
#'   shot_distance = sqrt((goal_x - abs(x_coord))^2 + y_coord^2)
#'   shot_angle    = abs(atan2(y_coord, goal_x - abs(x_coord))) * 180 / pi
#' Non-shot rows receive NA for both columns.
#' x_coord/y_coord are coerced to numeric (suppressWarnings); all-NA coords
#' produce NA distance/angle rather than an error.
#'
#' @param pbp     data.frame with at least `event`, `x_coord`, `y_coord`.
#' @param goal_x  X-coordinate of the offensive net in feet. Default 89.
#' @return pbp with `shot_distance` (numeric) and `shot_angle` (numeric) added.
#' @noRd
hockeytech_shot_distance_angle <- function(pbp, goal_x = 89) {
  if (nrow(pbp) == 0) {
    pbp$shot_distance <- numeric(0)
    pbp$shot_angle    <- numeric(0)
    return(pbp)
  }

  x <- suppressWarnings(as.numeric(pbp$x_coord))
  y <- suppressWarnings(as.numeric(pbp$y_coord))

  is_shot <- pbp$event %in% .SHOT_EVENTS_HT

  dx   <- goal_x - abs(x)
  dist <- sqrt(dx^2 + y^2)
  ang  <- abs(atan2(abs(y), dx)) * (180 / pi)

  pbp$shot_distance <- ifelse(is_shot, dist, NA_real_)
  pbp$shot_angle    <- ifelse(is_shot, ang,  NA_real_)
  pbp
}

# ---------------------------------------------------------------------------
# Scoring chances
# ---------------------------------------------------------------------------

#' Flag scoring_chance for shot-type events within threshold_ft of the net.
#'
#' @param pbp          data.frame. If `shot_distance` is absent,
#'                     [hockeytech_shot_distance_angle()] is called first.
#' @param threshold_ft Distance threshold in feet. Default 25.
#' @return pbp with logical `scoring_chance` column added.
#' @noRd
hockeytech_scoring_chances <- function(pbp, threshold_ft = 25) {
  if (!"shot_distance" %in% names(pbp)) {
    pbp <- hockeytech_shot_distance_angle(pbp)
  }
  pbp$scoring_chance <- !is.na(pbp$shot_distance) & pbp$shot_distance <= threshold_ft
  pbp
}

# ---------------------------------------------------------------------------
# Player TOI
# ---------------------------------------------------------------------------

#' Compute per-player time-on-ice from a parsed shifts frame.
#'
#' The HockeyTech shift feed uses a countdown clock: start_s >= end_s.
#' shift_length = start_s - end_s.
#'
#' @param shifts data.frame with at least `player_id`, `first_name`,
#'   `last_name`, `start_s`, `end_s`.
#' @return data.frame with one row per player: `player_id`, `first_name`,
#'   `last_name`, `toi_seconds`, `num_shifts`, `avg_shift_s`, sorted
#'   descending by `toi_seconds`.
#' @noRd
hockeytech_player_toi <- function(shifts) {
  empty_cols <- c("player_id", "first_name", "last_name",
                  "toi_seconds", "num_shifts", "avg_shift_s")
  if (nrow(shifts) == 0) {
    out <- as.data.frame(
      matrix(nrow = 0, ncol = length(empty_cols),
             dimnames = list(NULL, empty_cols))
    )
    out$player_id   <- integer(0)
    out$first_name  <- character(0)
    out$last_name   <- character(0)
    out$toi_seconds <- integer(0)
    out$num_shifts  <- integer(0)
    out$avg_shift_s <- numeric(0)
    return(out)
  }

  shifts$shift_s <- shifts$start_s - shifts$end_s

  agg <- dplyr::summarise(
    dplyr::group_by(shifts, .data$player_id, .data$first_name, .data$last_name),
    toi_seconds = sum(.data$shift_s, na.rm = TRUE),
    num_shifts  = dplyr::n(),
    avg_shift_s = mean(.data$shift_s, na.rm = TRUE),
    .groups = "drop"
  )

  agg[order(-agg$toi_seconds), ]
}

# ---------------------------------------------------------------------------
# On-ice player tracking
# ---------------------------------------------------------------------------

#' Attach on_ice_home / on_ice_away (comma-joined sorted integer player_ids) per event.
#'
#' `pbp` must have integer `period_of_game` and `time_s` (countdown seconds).
#' A player is on ice iff a shift in that period has start_s >= time_s >= end_s.
#' `shifts` must have `player_id`, `home` (1=home, 0=away), `period`,
#' `start_s`, `end_s`.
#'
#' IMPORTANT: the shift `player_id` is renamed to `.shift_pid` before any join
#' so it never collides with the event-level `player_id` that `pbp` may carry.
#' Player ids are formatted as integers (no ".0" suffix).
#'
#' @param pbp    data.frame with `period_of_game` (integer) and `time_s` (integer).
#' @param shifts data.frame with shift records.
#' @return pbp with `on_ice_home` and `on_ice_away` character columns added.
#' @noRd
hockeytech_build_on_ice <- function(pbp, shifts) {
  if (nrow(pbp) == 0 || nrow(shifts) == 0) {
    pbp$on_ice_home <- NA_character_
    pbp$on_ice_away <- NA_character_
    return(pbp)
  }

  # Add a row index to preserve event order and allow left-join back.
  pbp$.eidx <- seq_len(nrow(pbp))

  # Rename shift player_id -> .shift_pid to avoid collision with any
  # player_id column already in pbp.
  shifts_sel <- data.frame(
    .shift_pid = as.integer(shifts$player_id),
    home       = shifts$home,
    period     = shifts$period,
    start_s    = shifts$start_s,
    end_s      = shifts$end_s,
    stringsAsFactors = FALSE
  )

  # Cross-join events x shifts on matching period, then filter by time interval.
  # We do this via merge (inner join on period).
  joined <- merge(
    pbp[, c(".eidx", "period_of_game", "time_s"), drop = FALSE],
    shifts_sel,
    by.x = "period_of_game",
    by.y = "period",
    all   = FALSE
  )

  # Keep only shifts covering the event time (countdown: start_s >= time_s >= end_s)
  on_ice <- joined[joined$start_s >= joined$time_s & joined$time_s >= joined$end_s, ]

  # Helper: aggregate one side into comma-joined sorted integer ids
  .agg_side <- function(side_flag) {
    sub_df <- on_ice[on_ice$home == side_flag, c(".eidx", ".shift_pid"), drop = FALSE]
    if (nrow(sub_df) == 0) {
      return(data.frame(.eidx = integer(0), side_ids = character(0),
                        stringsAsFactors = FALSE))
    }
    # For each event index, collect unique sorted integer ids and paste
    result <- tapply(
      sub_df$.shift_pid,
      sub_df$.eidx,
      function(ids) {
        paste(sort(unique(as.integer(ids))), collapse = ",")
      }
    )
    data.frame(
      .eidx    = as.integer(names(result)),
      side_ids = as.character(result),
      stringsAsFactors = FALSE
    )
  }

  home_agg <- .agg_side(1)
  away_agg <- .agg_side(0)

  # Left-join home ids back to pbp
  pbp <- merge(pbp, home_agg, by = ".eidx", all.x = TRUE, sort = FALSE)
  names(pbp)[names(pbp) == "side_ids"] <- "on_ice_home"

  # Left-join away ids back to pbp
  pbp <- merge(pbp, away_agg, by = ".eidx", all.x = TRUE, sort = FALSE)
  names(pbp)[names(pbp) == "side_ids"] <- "on_ice_away"

  # Restore original row order and drop the index column
  pbp <- pbp[order(pbp$.eidx), ]
  pbp$.eidx <- NULL

  pbp
}

# ---------------------------------------------------------------------------
# Team-level Corsi / Fenwick
# ---------------------------------------------------------------------------

#' Compute team-level Corsi and Fenwick shot-attempt counts.
#'
#' Corsi  = shot + blocked_shot + goal (proxy; no missed-shot in HockeyTech).
#' Fenwick = shot + goal.
#' Every output row carries corsi_includes_missed = FALSE.
#'
#' @param pbp data.frame with at least `event` and `team_id`.
#' @return data.frame with one row per team: `team_id`, `corsi_for`,
#'   `corsi_against`, `corsi_for_pct`, `fenwick_for`, `fenwick_against`,
#'   `fenwick_for_pct`, `corsi_includes_missed`.
#' @noRd
hockeytech_corsi_fenwick <- function(pbp) {
  empty_schema <- data.frame(
    team_id              = integer(0),
    corsi_for            = integer(0),
    corsi_against        = integer(0),
    corsi_for_pct        = numeric(0),
    fenwick_for          = integer(0),
    fenwick_against      = integer(0),
    fenwick_for_pct      = numeric(0),
    corsi_includes_missed = logical(0),
    stringsAsFactors = FALSE
  )

  if (nrow(pbp) == 0) return(empty_schema)

  teams <- unique(pbp$team_id)
  teams <- teams[!is.na(teams)]
  if (length(teams) == 0) return(empty_schema)

  rows <- lapply(teams, function(t) {
    cf <- sum(pbp$event %in% .SHOT_EVENTS_HT & pbp$team_id == t, na.rm = TRUE)
    ca <- sum(pbp$event %in% .SHOT_EVENTS_HT & pbp$team_id != t & !is.na(pbp$team_id), na.rm = TRUE)
    ff <- sum(pbp$event %in% .FENWICK_EVENTS_HT & pbp$team_id == t, na.rm = TRUE)
    fa <- sum(pbp$event %in% .FENWICK_EVENTS_HT & pbp$team_id != t & !is.na(pbp$team_id), na.rm = TRUE)

    data.frame(
      team_id               = t,
      corsi_for             = cf,
      corsi_against         = ca,
      corsi_for_pct         = if ((cf + ca) > 0) cf / (cf + ca) else NA_real_,
      fenwick_for           = ff,
      fenwick_against       = fa,
      fenwick_for_pct       = if ((ff + fa) > 0) ff / (ff + fa) else NA_real_,
      corsi_includes_missed = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Player-level on-ice Corsi / Fenwick
# ---------------------------------------------------------------------------

#' Compute player-level on-ice Corsi and Fenwick from an enriched PBP frame.
#'
#' The enriched PBP must carry: `event`, `team_id`, `home_team_id`,
#' `on_ice_home`, `on_ice_away`.
#'
#' For each shot-attempt event:
#'   - event-team side = home if team_id == home_team_id, else away.
#'   - "for" players (event side): CF +1, FF +1 if not blocked_shot.
#'   - "against" players (other side): CA +1, FA +1 if not blocked_shot.
#'
#' @param pbp data.frame with the columns described above.
#' @return data.frame with one row per player: `player_id`, `corsi_for`,
#'   `corsi_against`, `corsi_for_pct`, `fenwick_for`, `fenwick_against`,
#'   `fenwick_for_pct`, `corsi_includes_missed`.
#' @noRd
hockeytech_corsi_fenwick_on_ice <- function(pbp) {
  empty_schema <- data.frame(
    player_id             = character(0),
    corsi_for             = integer(0),
    corsi_against         = integer(0),
    corsi_for_pct         = numeric(0),
    fenwick_for           = integer(0),
    fenwick_against       = integer(0),
    fenwick_for_pct       = numeric(0),
    corsi_includes_missed = logical(0),
    stringsAsFactors = FALSE
  )

  required <- c("event", "team_id", "home_team_id", "on_ice_home", "on_ice_away")
  if (nrow(pbp) == 0 || !all(required %in% names(pbp))) return(empty_schema)

  # Accumulate CF/CA/FF/FA per player (string key)
  stats <- list()

  ensure_pid <- function(pid) {
    if (is.null(stats[[pid]])) {
      stats[[pid]] <<- c(cf = 0L, ca = 0L, ff = 0L, fa = 0L)
    }
  }

  for (i in seq_len(nrow(pbp))) {
    event        <- pbp$event[i]
    if (!event %in% .SHOT_EVENTS_HT) next

    team_id      <- as.character(pbp$team_id[i])
    home_team_id <- as.character(pbp$home_team_id[i])
    oih          <- pbp$on_ice_home[i]
    oiw          <- pbp$on_ice_away[i]

    if (is.na(team_id) || is.na(home_team_id)) next
    if (is.na(oih) || is.na(oiw)) next

    is_home_event  <- (team_id == home_team_id)
    for_raw        <- if (is_home_event) oih else oiw
    against_raw    <- if (is_home_event) oiw else oih

    for_players     <- trimws(strsplit(for_raw,     ",", fixed = TRUE)[[1]])
    against_players <- trimws(strsplit(against_raw, ",", fixed = TRUE)[[1]])
    for_players     <- for_players[nchar(for_players) > 0]
    against_players <- against_players[nchar(against_players) > 0]

    is_fenwick <- event %in% .FENWICK_EVENTS_HT

    for (pid in for_players) {
      ensure_pid(pid)
      stats[[pid]]["cf"] <- stats[[pid]]["cf"] + 1L
      if (is_fenwick) stats[[pid]]["ff"] <- stats[[pid]]["ff"] + 1L
    }
    for (pid in against_players) {
      ensure_pid(pid)
      stats[[pid]]["ca"] <- stats[[pid]]["ca"] + 1L
      if (is_fenwick) stats[[pid]]["fa"] <- stats[[pid]]["fa"] + 1L
    }
  }

  if (length(stats) == 0) return(empty_schema)

  rows <- lapply(names(stats), function(pid) {
    s  <- stats[[pid]]
    cf <- s["cf"]; ca <- s["ca"]; ff <- s["ff"]; fa <- s["fa"]
    data.frame(
      player_id             = pid,
      corsi_for             = as.integer(cf),
      corsi_against         = as.integer(ca),
      corsi_for_pct         = if ((cf + ca) > 0) cf / (cf + ca) else NA_real_,
      fenwick_for           = as.integer(ff),
      fenwick_against       = as.integer(fa),
      fenwick_for_pct       = if ((ff + fa) > 0) ff / (ff + fa) else NA_real_,
      corsi_includes_missed = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Per-60 rate
# ---------------------------------------------------------------------------

#' Compute a per-60-minute rate.
#'
#' @param value       Numeric count value.
#' @param toi_seconds Numeric time-on-ice in seconds.
#' @return Numeric per-60 rate (value / toi_seconds * 3600).
#' @noRd
hockeytech_per60 <- function(value, toi_seconds) {
  value / toi_seconds * 3600
}
