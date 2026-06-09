#' @keywords internal
#' HockeyTech league registry. Mirrors sdv-py sportsdataverse/hockeytech/_leagues.py.
#' @noRd
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
