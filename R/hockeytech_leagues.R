#' HockeyTech league registry. Mirrors sdv-py sportsdataverse/hockeytech/_leagues.py.
#' @noRd
#' @keywords internal
.hockeytech_leagues <- function() {
  ls <- "https://lscluster.hockeytech.com/feed/index.php"
  lg <- "https://cluster.leaguestat.com/feed/index.php"
  list(
    pwhl  = list(name = "PWHL",  client_code = "pwhl",  api_key = "446521baf8c38984",
                 league_id = 1, site_id = 0, base_url = ls, pbp_style = "hockeytech_a"),
    ahl   = list(name = "AHL",   client_code = "ahl",   api_key = "ccb91f29d6744675",
                 league_id = 4, site_id = 3, base_url = ls, pbp_style = "hockeytech_a"),
    ohl   = list(name = "OHL",   client_code = "ohl",   api_key = "f1aa699db3d81487",
                 league_id = 1, site_id = 1, base_url = ls, pbp_style = "hockeytech_b"),
    whl   = list(name = "WHL",   client_code = "whl",   api_key = "f1aa699db3d81487",
                 league_id = 7, site_id = 0, base_url = ls, pbp_style = "hockeytech_b"),
    qmjhl = list(name = "QMJHL", client_code = "lhjmq", api_key = "f322673b6bcae299",
                 league_id = 6, site_id = 0, base_url = lg, pbp_style = "hockeytech_b")
  )
}

#' TRUE for season names that are one-off events, not a regular season or
#' playoffs: the feed lists them as seasons too ("2026 All-Star Challenge",
#' "2025 Top Prospects", "CCHL Pre-Draft Combine 2026"), and the seasons parser
#' labels them "regular". Mirrors sdv-py `SPECIAL_EVENT_SEASON_RE`.
#' @noRd
#' @keywords internal
.ht_is_special_event <- function(name) {
  grepl(
    "(?i)all[- ]?star|showcase|prospect|combine|special event|exhibition|play[- ]?in\\b",
    ifelse(is.na(name), "", name),
    perl = TRUE
  )
}

#' Resolve an end-year `season` to integer HockeyTech season_id. Explicit season_id
#' short-circuits. PWHL falls back to the hardcoded table via .pwhl_resolve_season_id.
#'
#' Mirrors sdv-py `resolve_season_id`: of the rows with that season_yr and
#' game_type_label, regular and playoff lookups drop one-off events (the AHL
#' lists "2026 All-Star Challenge" ahead of "2025-26 Regular Season"), then a
#' name that says its game type wins, then a name spanning two years, then feed
#' order.
#' @noRd
#' @keywords internal
.hockeytech_season_id <- function(league, season = NULL, game_type = "regular", season_id = NULL) {
  if (!is.null(season_id)) return(as.integer(season_id))
  if (is.null(season)) stop("Provide season (end-year) or season_id", call. = FALSE)
  # Only PWHL has a fallback table; for any other league a failed seasons fetch raises,
  # rather than reading as "no such season".
  seasons <- tryCatch(
    .parse_hockeytech_seasons(.hockeytech_api(.hockeytech_url(league, "modulekit", "seasons", list()))),
    error = function(e) if (league == "pwhl") data.frame() else stop(e)
  )
  if (is.data.frame(seasons) && nrow(seasons) > 0) {
    hit <- seasons[which(seasons$season_yr == season & seasons$game_type_label == game_type), , drop = FALSE]
    if (game_type %in% c("regular", "playoffs")) {
      hit <- hit[!.ht_is_special_event(hit$season_name), , drop = FALSE]
    }
    if (nrow(hit) > 0) {
      # ponytail: sdv-py also ranks names carrying another league's code last (the OJHL
      # feed lists CCHL seasons); none of the five R leagues needs it.
      nm <- ifelse(is.na(hit$season_name), "", hit$season_name)
      type_re <- switch(game_type, regular = "regular season", playoffs = "playoff",
                        preseason = "pre[- ]?season", game_type)
      named <- grepl(type_re, nm, ignore.case = TRUE)
      spans <- grepl("\\d{2}\\s*[-/]\\s*\\d{2}", nm)
      return(as.integer(hit$season_id[order(!named, !spans)[1]]))
    }
  }
  if (league == "pwhl") return(as.integer(.pwhl_resolve_season_id(season, game_type)))
  stop(sprintf("No %s season for season=%s, game_type=%s", league, season, game_type), call. = FALSE)
}

#' @keywords internal
#' @noRd
.hockeytech_resolve_key <- function(league, view = NULL) {
  env <- Sys.getenv(paste0("SDV_", toupper(league), "_API_KEY"), unset = "")
  if (nzchar(env)) return(env)
  if (!is.null(view) && view == "gameCenterPlayByPlay" && league == "pwhl") {
    return("694cfeed58c932ee")
  }
  .hockeytech_leagues()[[league]]$api_key
}
