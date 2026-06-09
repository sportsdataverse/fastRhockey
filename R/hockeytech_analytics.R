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


# ---------------------------------------------------------------------------
# Coordinate transforms
# ---------------------------------------------------------------------------

#' Add normalized coordinate columns from raw x_coord / y_coord.
#'
#' Ported from Python sportsdataverse/hockeytech/_analytics.py::add_coord_transforms(),
#' which itself was ported from fastRhockey R/pwhl_pbp.R lines ~484-496.
#'
#' Raw coordinates (x_coord, y_coord) come from the HockeyTech feed on an
#' approximately 850x400 canvas. This function adds ten derived columns:
#'
#'   x_coord_original / y_coord_original = raw x_coord, y_coord
#'   x_coord_neutral  = ox - 300
#'   y_coord_neutral  = oy - 150
#'   x_t = (ox / 3) - 100                            [intermediate]
#'   y_t = 42.5 - (oy * 85 / 300)                    [intermediate, simplified]
#'   x_coord_fixed    = x_t / 3
#'   y_coord_fixed    = 42.5 - ((y_t * 85 / 300) - 42.5)
#'   x_coord_right    = if (team_id == home_team_id) 100 + (100 - x_t) else x_t
#'   y_coord_right    = if (team_id == home_team_id) 42.5 - (y_t - 42.5) else y_t
#'   x_coord_vertical = 42.5 - (y_coord_right - 42.5)
#'   y_coord_vertical = x_coord_right
#'
#' Rows with null x_coord or y_coord produce NA for all ten columns.
#' Rows with null team_id or home_team_id produce NA for the team-dependent
#' transforms (x/y_coord_right, x/y_coord_vertical).
#'
#' x_coord and y_coord are MUTATED in place to the transformed values
#' (x_t, y_t) to match the R/pwhl_pbp.R behaviour exactly; the raw values
#' are preserved in x_coord_original / y_coord_original.
#'
#' @param pbp data.frame with at least x_coord, y_coord columns. If
#'   home_team_id is present, the team-dependent right/vertical transforms
#'   use it; otherwise those columns are NA.
#' @return pbp with ten coordinate columns appended (or replaced). x_coord
#'   and y_coord are overwritten with the transformed feet-scale values.
#' @noRd
hockeytech_add_coord_transforms <- function(pbp) {
  if (nrow(pbp) == 0) {
    for (col in c("x_coord_original", "y_coord_original",
                  "x_coord_neutral",  "y_coord_neutral",
                  "x_coord_fixed",    "y_coord_fixed",
                  "x_coord_right",    "y_coord_right",
                  "x_coord_vertical", "y_coord_vertical")) {
      pbp[[col]] <- numeric(0)
    }
    return(pbp)
  }

  ox <- suppressWarnings(as.numeric(pbp$x_coord))
  oy <- suppressWarnings(as.numeric(pbp$y_coord))

  # Intermediate transformed coords (R pwhl_pbp.R lines 489-490)
  x_t <- (ox / 3) - 100
  y_t <- 42.5 - ((oy * 85) / 300)      # simplified: 42.5 - ((oy*85/300 - 42.5) - 42.5) = 42.5 - oy*85/300

  pbp$x_coord_original <- ox
  pbp$y_coord_original <- oy
  pbp$x_coord_neutral  <- ox - 300
  pbp$y_coord_neutral  <- oy - 150

  # Overwrite x_coord / y_coord with transformed values (matches pwhl_pbp.R)
  pbp$x_coord <- x_t
  pbp$y_coord <- y_t

  pbp$x_coord_fixed <- x_t / 3
  pbp$y_coord_fixed <- 42.5 - (((y_t * 85) / 300) - 42.5)

  # Team-dependent right/vertical transforms
  if ("home_team_id" %in% names(pbp)) {
    is_home <- !is.na(pbp$team_id) & !is.na(pbp$home_team_id) &
               as.character(pbp$team_id) == as.character(pbp$home_team_id)
    pbp$x_coord_right <- ifelse(is_home, 100 + (100 - x_t), x_t)
    pbp$y_coord_right <- ifelse(is_home, 42.5 - (y_t - 42.5), y_t)
  } else {
    pbp$x_coord_right <- x_t
    pbp$y_coord_right <- y_t
  }

  pbp$x_coord_vertical <- 42.5 - (pbp$y_coord_right - 42.5)
  pbp$y_coord_vertical <- pbp$x_coord_right

  pbp
}


# ---------------------------------------------------------------------------
# Clock columns
# ---------------------------------------------------------------------------

#' Add period-clock columns derived from time_of_period (elapsed MM:SS).
#'
#' Ported from Python sportsdataverse/hockeytech/_analytics.py::add_clock_columns(),
#' which itself was ported from fastRhockey R/pwhl_pbp.R lines ~498-525.
#'
#' time_of_period is ELAPSED seconds counting UP from "0:00" at the start of
#' each period. Adds four columns:
#'
#'   minute_start (integer): elapsed minutes component.
#'   second_start (integer): elapsed seconds component.
#'   clock (character): remaining time formatted as "M:SS". Computed as
#'     (19 - minute_start):(60 - second_start) with R special-cases:
#'     - When minute_start == 0 AND second_start == 0 (start of period):
#'       clock = "20:00".
#'     - When second_start == 0 (but not both zero): second = 0.
#'   sec_from_start (integer): cumulative game-seconds elapsed since start.
#'     Within each period: minute_start * 60 + second_start; plus per-period
#'     offsets: period 1 -> +0, 2 -> +1200, 3 -> +2400, 4 -> +3600, 5 -> +4800.
#'
#' Rows with NA time_of_period receive NA for all four columns.
#'
#' @param pbp data.frame with at least time_of_period (character "M:SS") and
#'   period_of_game (character or castable to integer).
#' @return pbp with minute_start, second_start, clock, sec_from_start added.
#' @noRd
hockeytech_add_clock_columns <- function(pbp) {
  if (nrow(pbp) == 0) {
    pbp$minute_start  <- integer(0)
    pbp$second_start  <- integer(0)
    pbp$clock         <- character(0)
    pbp$sec_from_start <- integer(0)
    return(pbp)
  }

  top <- pbp$time_of_period

  # Parse "M:SS" into minute_start and second_start
  parts <- strsplit(as.character(top), ":", fixed = TRUE)
  minute_start <- suppressWarnings(
    as.integer(vapply(parts, function(p) if (length(p) >= 1) p[[1]] else NA_character_, character(1)))
  )
  second_start <- suppressWarnings(
    as.integer(vapply(parts, function(p) if (length(p) >= 2) p[[2]] else NA_character_, character(1)))
  )

  # Remaining clock time (mirrors R pwhl_pbp.R lines 502-512)
  clock_minute <- ifelse(
    !is.na(minute_start) & !is.na(second_start) &
    (19L - minute_start == 19L) & (60L - second_start == 60L),
    20L,
    19L - minute_start
  )
  clock_second <- ifelse(!is.na(second_start) & (60L - second_start == 60L),
                         0L,
                         60L - second_start)
  # Zero-pad seconds
  clock_second_str <- ifelse(
    !is.na(clock_second) & clock_second < 10L,
    paste0("0", clock_second),
    as.character(clock_second)
  )
  clock <- ifelse(is.na(clock_minute) | is.na(clock_second),
                  NA_character_,
                  paste0(clock_minute, ":", clock_second_str))

  # sec_from_start = elapsed seconds within period + per-period offset
  base_sfs <- minute_start * 60L + second_start
  period_int <- suppressWarnings(as.integer(pbp$period_of_game))
  sec_from_start <- dplyr::case_when(
    period_int == 2L ~ base_sfs + 1200L,
    period_int == 3L ~ base_sfs + 2400L,
    period_int == 4L ~ base_sfs + 3600L,
    period_int == 5L ~ base_sfs + 4800L,
    TRUE             ~ base_sfs
  )

  pbp$minute_start   <- minute_start
  pbp$second_start   <- second_start
  pbp$clock          <- clock
  pbp$sec_from_start <- sec_from_start
  pbp
}


# ---------------------------------------------------------------------------
# Power-play / short-handed back-fill
# ---------------------------------------------------------------------------

#' Back-fill power_play and short_handed for shot/faceoff events.
#'
#' Ported from Python sportsdataverse/hockeytech/_analytics.py::backfill_power_play(),
#' which itself was ported from fastRhockey R/pwhl_pbp.R lines ~522-565.
#'
#' For each penalty event whose power_play flag is "1", a PP window
#' [start_sec, end_sec] is derived:
#'   start_sec = sec_from_start of the penalty event.
#'   end_sec   = start_sec + penalty_length * 60, truncated at the first goal
#'               scored during that window (power-kill-ends-on-goal logic).
#'
#' For every shot or faceoff event inside a PP window:
#'   If the event team_id == advantage_team: power_play="1", short_handed="0".
#'   Otherwise:                              power_play="0", short_handed="1".
#'
#' All other rows are left unchanged. Safe when zero penalty events exist.
#' Requires sec_from_start, event, power_play, penalty_length, team_id,
#' home_team_id, away_team_id columns. Creates short_handed if absent.
#'
#' @param df PBP data.frame after hockeytech_add_clock_columns has run.
#' @return df with power_play / short_handed back-filled for shot/faceoff
#'   events during active PP windows.
#' @noRd
hockeytech_backfill_power_play <- function(df) {
  # Ensure short_handed column exists
  if (!"short_handed" %in% names(df)) df$short_handed <- NA_character_
  if (!"power_play"   %in% names(df)) df$power_play   <- NA_character_

  if (nrow(df) == 0) return(df)

  # Extract penalty rows with power_play == "1" and valid sec_from_start
  pen_mask <- !is.na(df$event) & df$event == "penalty" &
              !is.na(df$power_play) & df$power_play == "1" &
              !is.na(df$sec_from_start)
  pens_df <- df[pen_mask, , drop = FALSE]

  if (nrow(pens_df) == 0) return(df)

  # Derive advantage_team: team NOT penalized
  # penalized = team_id on penalty row; advantage = the other side
  is_home_pen <- !is.na(pens_df$team_id) & !is.na(pens_df$home_team_id) &
                 as.character(pens_df$team_id) == as.character(pens_df$home_team_id)
  pens_df$advantage_team <- ifelse(is_home_pen, pens_df$away_team_id, pens_df$home_team_id)

  # Penalty interval: start_sec, end_sec, advantage_team
  pen_length_s <- suppressWarnings(as.numeric(pens_df$penalty_length)) * 60
  starts <- as.numeric(pens_df$sec_from_start)
  ends   <- starts + pen_length_s
  adv    <- as.character(pens_df$advantage_team)

  # Goal times for PP truncation
  goal_mask  <- !is.na(df$event) & df$event == "goal" & !is.na(df$sec_from_start)
  goal_times <- sort(as.numeric(df$sec_from_start[goal_mask]))

  # Truncate each penalty window at first goal within it
  # (mirrors R pwhl_pbp.R loop)
  n_pen <- length(starts)
  pen_intervals <- vector("list", n_pen)
  for (i in seq_len(n_pen)) {
    s <- starts[[i]]; e <- ends[[i]]; adv_i <- adv[[i]]
    if (is.na(s) || is.na(e)) next
    prev_end <- if (i > 1 && !is.null(pen_intervals[[i - 1L]])) pen_intervals[[i - 1L]][[2]] else NULL
    for (g in goal_times) {
      if (g >= s && g <= e) {
        if (is.null(prev_end) || g > prev_end) {
          e <- g
          break
        }
      }
    }
    pen_intervals[[i]] <- list(s, e, adv_i)
  }
  pen_intervals <- Filter(Negate(is.null), pen_intervals)

  if (length(pen_intervals) == 0) return(df)

  # Back-fill shot/faceoff rows using computed intervals
  event_col <- df$event
  sec_col   <- suppressWarnings(as.numeric(df$sec_from_start))
  team_col  <- as.character(df$team_id)
  pp_col    <- as.character(df$power_play)
  sh_col    <- as.character(df$short_handed)

  new_pp <- pp_col
  new_sh <- sh_col

  for (idx in seq_len(nrow(df))) {
    ev  <- event_col[[idx]]
    if (is.na(ev) || !ev %in% c("shot", "faceoff")) next
    sec <- sec_col[[idx]]
    if (is.na(sec)) next
    team <- team_col[[idx]]
    for (pint in pen_intervals) {
      ps <- pint[[1]]; pe <- pint[[2]]; padv <- pint[[3]]
      if (!is.na(ps) && !is.na(pe) && ps <= sec && sec <= pe) {
        if (!is.na(team) && team == padv) {
          new_pp[[idx]] <- "1"
          new_sh[[idx]] <- "0"
        } else {
          new_pp[[idx]] <- "0"
          new_sh[[idx]] <- "1"
        }
        break  # first matching interval only
      }
    }
  }

  df$power_play   <- new_pp
  df$short_handed <- new_sh
  df
}


# ---------------------------------------------------------------------------
# Enrich PBP orchestrator
# ---------------------------------------------------------------------------

#' Enrich a parsed HockeyTech PBP frame -- league-generic.
#'
#' Ported from Python sportsdataverse/hockeytech/_analytics.py::enrich_pbp().
#'
#' Applies the full enrichment pipeline:
#'   1. Game-meta join (game_date, game_season, game_season_id, home_team,
#'      home_team_id, away_team, away_team_id) from gc/gamesummary.
#'   2. Coordinate transforms via hockeytech_add_coord_transforms().
#'   3. Clock columns via hockeytech_add_clock_columns().
#'   4. PP/SH back-fill via hockeytech_backfill_power_play().
#'   5. Shot geometry (shot_distance, shot_angle, scoring_chance) using the
#'      intermediate feet-scale coords: x_t = x_coord_original/3 - 100,
#'      y_t = 42.5 - (y_coord_original * 85 / 300).
#'   6. On-ice player tracking (on_ice_home, on_ice_away) from gameshifts via
#'      .parse_hockeytech_shifts() + hockeytech_build_on_ice().
#'
#' Pure when meta_payload and shifts_payload are injected (no network calls).
#' When either is NULL the function attempts a live fetch via .hockeytech_api().
#'
#' @param df data.frame produced by .parse_hockeytech_pbp().
#' @param league HockeyTech league code (e.g. "pwhl", "ohl", "whl").
#' @param game_id Numeric game identifier.
#' @param meta_payload Optional pre-fetched gc/gamesummary JSON list.
#' @param shifts_payload Optional pre-fetched modulekit/gameshifts JSON list.
#'   Pass list() to suppress on-ice computation entirely.
#' @return data.frame; the enriched play-by-play.
#' @noRd
hockeytech_enrich_pbp <- function(df, league, game_id,
                                   meta_payload = NULL,
                                   shifts_payload = NULL) {
  # ------------------------------------------------------------------
  # Step 1: fetch meta if not provided
  # ------------------------------------------------------------------
  if (is.null(meta_payload)) {
    meta_payload <- .hockeytech_api(
      .hockeytech_url(league, "gc", "gamesummary", list(game_id = game_id))
    )
  }

  # ------------------------------------------------------------------
  # Step 2: extract game-meta fields from GC.Gamesummary
  # ------------------------------------------------------------------
  gc_root <- (meta_payload %||% list())[["GC"]] %||% list()
  gs <- gc_root[["Gamesummary"]] %||% gc_root
  gs_meta  <- gs[["meta"]] %||% list()
  home_raw <- gs[["home"]] %||% list()
  away_raw <- gs[["visitor"]] %||% list()

  home_team    <- as.character(home_raw[["name"]] %||% home_raw[["city"]] %||% "")
  home_team_id <- as.character(
    gs_meta[["home_team"]] %||% home_raw[["id"]] %||% home_raw[["team_id"]] %||% ""
  )
  away_team    <- as.character(away_raw[["name"]] %||% away_raw[["city"]] %||% "")
  away_team_id <- as.character(
    gs_meta[["visiting_team"]] %||% away_raw[["id"]] %||% away_raw[["team_id"]] %||% ""
  )

  game_date_raw <- gs_meta[["date_played"]] %||%
                   gs[["game_date_iso_8601"]] %||%
                   gs[["game_date"]] %||% ""
  game_date  <- as.character(game_date_raw)
  game_season_raw <- if (nchar(game_date) >= 4) substr(game_date, 1, 4) else NA_character_
  game_season <- suppressWarnings(as.integer(game_season_raw))
  game_season_id <- as.character(gs_meta[["season_id"]] %||% "")

  # ------------------------------------------------------------------
  # Step 3: add game-meta literal columns BEFORE coord transforms
  #   (coord transforms need home_team_id to compute right/vertical)
  # ------------------------------------------------------------------
  df$game_date      <- game_date
  df$game_season    <- game_season
  df$game_season_id <- game_season_id
  df$home_team      <- home_team
  df$home_team_id   <- home_team_id
  df$away_team      <- away_team
  df$away_team_id   <- away_team_id

  # ------------------------------------------------------------------
  # Step 4: coordinate transforms
  # ------------------------------------------------------------------
  df <- hockeytech_add_coord_transforms(df)

  # ------------------------------------------------------------------
  # Step 5: clock columns
  # ------------------------------------------------------------------
  df <- hockeytech_add_clock_columns(df)

  # ------------------------------------------------------------------
  # Step 5b: PP / SH back-fill
  # ------------------------------------------------------------------
  df <- hockeytech_backfill_power_play(df)

  # ------------------------------------------------------------------
  # Step 6: shot geometry on feet-scale intermediate coords
  #   Python uses: x_t = x_coord_original/3 - 100
  #                y_t = 42.5 - (y_coord_original * 85 / 300)
  # ------------------------------------------------------------------
  # Build a temporary frame with feet-scale x/y for shot geometry
  geo_df <- df
  geo_df$x_coord <- suppressWarnings(as.numeric(geo_df$x_coord_original)) / 3 - 100
  geo_df$y_coord <- 42.5 - (suppressWarnings(as.numeric(geo_df$y_coord_original)) * 85 / 300)
  geo_df <- hockeytech_shot_distance_angle(geo_df)
  geo_df <- hockeytech_scoring_chances(geo_df)

  df$shot_distance  <- geo_df$shot_distance
  df$shot_angle     <- geo_df$shot_angle
  df$scoring_chance <- geo_df$scoring_chance

  # ------------------------------------------------------------------
  # Step 7: on-ice player tracking via shifts
  # ------------------------------------------------------------------
  if (is.null(shifts_payload)) {
    shifts_payload <- .hockeytech_api(
      .hockeytech_url(league, "modulekit", "gameshifts", list(game_id = game_id))
    )
  }

  shifts <- NULL
  if (is.list(shifts_payload) && length(shifts_payload) > 0) {
    shifts <- .parse_hockeytech_shifts(shifts_payload, game_id = game_id)
  }

  if (!is.null(shifts) && nrow(df) > 0 && nrow(shifts) > 0) {
    # Compute per-period ceiling from shifts: max(start_s) in each period.
    # Fallback to 1200 when a period has no shifts.
    period_len_map <- tapply(shifts$start_s, shifts$period, function(x) {
      v <- max(x, na.rm = TRUE)
      if (is.finite(v)) as.integer(v) else 1200L
    })

    period_int <- suppressWarnings(as.integer(df$period_of_game))
    plen <- vapply(period_int, function(p) {
      if (is.na(p)) return(1200L)
      pmap_val <- period_len_map[as.character(p)]
      if (length(pmap_val) == 0 || is.na(pmap_val)) 1200L else as.integer(pmap_val)
    }, integer(1))

    elapsed_s  <- df$minute_start * 60L + df$second_start
    df$time_s  <- plen - elapsed_s

    # Save original period_of_game (string) and cast to integer for join
    orig_period <- df$period_of_game
    df$period_of_game <- period_int

    df <- hockeytech_build_on_ice(df, shifts)

    # Restore string period_of_game
    df$period_of_game <- orig_period
    df$time_s <- NULL
  } else {
    df$on_ice_home <- NA_character_
    df$on_ice_away <- NA_character_
  }

  df
}
