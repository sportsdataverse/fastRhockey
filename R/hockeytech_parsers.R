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
