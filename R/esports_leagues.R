# esports_leagues.R -- the eSports (hokejovyzapis) league registry and getter.
#
# Mirrors sdv-py sportsdataverse/hockey/esports_runtime.py and the four
# tools/codegen/endpoints/{del,icehl,eihl,metal}_esports.yaml files. The four
# leagues' scoring system writes one key layout into four public S3 buckets on
# s3.eu-west-1.amazonaws.com; keyless JSON. The buckets share one host, so the
# per-host throttle spaces all four leagues together (as in sdv-py).
#
# Season: every bucket keys a season by its START year (2026 = 2026/27). The
# wrappers take the END year and .esports_get() maps it (season = 2027 requests
# .../2026/...).

#' eSports league registry: bucket, competition ids and the routes each league
#' publishes (route -> path template). Paths differ per league where the
#' buckets differ (EIHL `game-info` vs `game-header`; EIHL/Metal `table`
#' vs `tables`).
#' @noRd
#' @keywords internal
.esports_leagues <- function() {
  s3 <- "https://s3.eu-west-1.amazonaws.com/"
  r <- list(
    schedule                 = "/league-matches/{season}/{league_id}.json",
    team_player_stats        = "/league-team-stats/{season}/{league_id}/{team_id}.json",
    top_scorers              = "/league-top-scorers/{season}/{league_id}.json",
    league_part_player_stats = "/league-part-players-stats/{league_part_id}.json",
    faceoffs                 = "/matches/{match_id}/faceoffs.json",
    game_header              = "/matches/{match_id}/game-header.json",
    game_info                = "/matches/{match_id}/game-info.json",
    pbp                      = "/matches/{match_id}/period-events.json",
    roster                   = "/matches/{match_id}/roster.json",
    shifts                   = "/matches/{match_id}/shiftsSC.json",
    player_box               = "/matches/{match_id}/team-stats/{team_id}.json",
    goalie_box               = "/matches/{match_id}/top-goalies.json",
    tables                   = "/tables/{season}/{league_id}.json",
    table                    = "/table/{season}/{league_id}.json",
    shots                    = "/visualization/shots/{match_id}.json",
    game_center              = "/widget/esports/match/{match_id}.json"
  )
  # route name (the sdv-py `<league>_<short>`) -> template
  pick <- function(...) {
    m <- c(...)
    nm <- names(m) %||% rep("", length(m))
    out <- r[unname(m)]
    names(out) <- ifelse(nzchar(nm), nm, unname(m))
    out
  }
  list(
    del = list(
      name = "DEL", host = paste0(s3, "de.hokejovyzapis.cz"), league_id = 1L,
      league_ids = c(`1` = "DEL regular season", `2` = "Test-Liga pre-season", `3` = "DEL playoffs",
                     `4` = "MagentaSport Cup 2020/21", `5` = "NHL Global Series 2022/23"),
      routes = pick("schedule", "team_player_stats", "top_scorers", "faceoffs",
                    game_info = "game_header", "pbp", "roster", "shifts", "player_box",
                    "goalie_box", standings = "tables", "shots")
    ),
    icehl = list(
      name = "ICE Hockey League", host = paste0(s3, "icehl.hokejovyzapis.cz"), league_id = 1L,
      league_ids = c(`1` = "ICE Hockey League, regular season and playoffs in one file (split by league_part_name)",
                     `2` = "pre-season (2025/26 on)"),
      routes = pick("schedule", "league_part_player_stats", "team_player_stats", "top_scorers",
                    "faceoffs", game_info = "game_header", "pbp", "player_box", "goalie_box",
                    standings = "tables", "shots", "game_center")
    ),
    eihl = list(
      name = "EIHL", host = paste0(s3, "eihl.hokejovyzapis.cz"), league_id = 3L,
      league_ids = c(`3` = "Elite League", `4` = "Challenge Cup", `5` = "playoffs"),
      routes = pick("schedule", "team_player_stats", "top_scorers", "game_info", "pbp",
                    "player_box", "goalie_box", standings = "table", "shots")
    ),
    metal = list(
      name = "Metal Ligaen", host = paste0(s3, "den.hokejovyzapis.cz"), league_id = 1L,
      league_ids = c(`1` = "Metal Ligaen, regular season and playoffs in one file (split by league_part_name)",
                     `2` = "pre-season", `4` = "Final-4 cup"),
      routes = pick("schedule", "top_scorers", game_info = "game_header", "pbp", "player_box",
                    "goalie_box", standings = "table", "game_center")
    )
  )
}

#' GET one eSports bucket key, throttled, and refuse a body without rows.
#'
#' The internal core every exported `del_*` / `icehl_*` / `eihl_*` / `metal_*`
#' wrapper calls. Returns the body parsed with `simplifyVector = FALSE`
#' (objects are named lists, arrays unnamed lists); the family parsers flatten it.
#'
#' @param league One of `names(.esports_leagues())`.
#' @param route A route the league publishes (`names(.esports_leagues()[[league]]$routes)`),
#'   named like the sdv-py function suffix: `schedule`, `pbp`, `standings`, ...
#' @param season Season END year (2027 = 2026/27); mapped to the bucket's START year.
#' @param league_id Competition id; defaults to the league's main competition.
#' @param match_id,team_id,league_part_id Ids the route's path needs (eSports ids,
#'   not the league website's).
#' @return The parsed JSON body.
#' @section Errors:
#' `fastRhockey_no_data`: the key does not exist (S3 404 NoSuchKey) or the body
#' has no rows (`[]`, `{}`, a schedule with no matches, a game with no events, an
#' empty shot shell). `fastRhockey_asset_fetch`: any other non-2xx (403
#' AccessDenied), an error envelope, a body of the wrong shape, or a body for
#' another season or match. `fastRhockey_bad_request`: an unknown league or
#' route, or a missing id.
#' @keywords internal
#' @noRd
.esports_get <- function(league, route, season = NULL, league_id = NULL,
                         match_id = NULL, team_id = NULL, league_part_id = NULL) {
  leagues <- .esports_leagues()
  lg <- leagues[[league]]
  if (is.null(lg)) .abort_bad_request("Unknown eSports league {.val {league}}; one of {.val {names(leagues)}}.")
  path <- lg$routes[[route]]
  if (is.null(path)) {
    .abort_bad_request("{lg$name} publishes no {.val {route}} route; available: {.val {names(lg$routes)}}.")
  }
  vals <- list(
    season = if (!is.null(season)) .season_start_year(season),
    league_id = league_id %||% lg$league_id,
    match_id = match_id, team_id = team_id, league_part_id = league_part_id
  )
  need <- regmatches(path, gregexpr("(?<=\\{)[a-z_]+(?=\\})", path, perl = TRUE))[[1]]
  missing <- need[vapply(vals[need], is.null, logical(1))]
  if (length(missing)) .abort_bad_request("{lg$name} {route} needs {.arg {missing}}.")
  ids <- lapply(vals[need], format, scientific = FALSE, trim = TRUE) # never "1e+05"
  url <- paste0(lg$host, glue::glue_data(ids, path))
  body <- .euro_get(url, simplifyVector = FALSE)
  .esports_check(url, body)
  body
}

#' A 200 whose keys are all of these is an error envelope, not data.
#' @noRd
.ESPORTS_ERROR_KEYS <- c("error", "errors", "message", "code", "detail", "status")

.esports_is_obj <- function(x) is.list(x) && !is.null(names(x))
.esports_nonempty <- function(x) is.list(x) && length(x) > 0

#' Raise unless `body` holds rows for exactly what `url` asked (sdv-py `_check`).
#' @noRd
.esports_check <- function(url, body) {
  where <- .euro_where(url)
  is_obj <- .esports_is_obj(body)
  if (is_obj && length(body) && all(names(body) %in% .ESPORTS_ERROR_KEYS)) {
    keys <- sort(names(body))
    .abort_asset_fetch("{where}: answered an error envelope {.val {keys}}.")
  }
  has_rows <- if (is_obj) any(vapply(body, .esports_nonempty, logical(1))) else .esports_nonempty(body)
  if (!has_rows) .abort_no_data("{where}: answered a body with no rows.")
  object_route <- "(/league-matches/\\d+/\\d+|/visualization/shots/\\d+|/widget/esports/match/\\d+|/period-events)\\.json$"
  if (grepl(object_route, url) && !is_obj) {
    .abort_asset_fetch("{where}: expected a JSON object, the body is an array.")
  }
  id_of <- function(pattern) sub(pattern, "\\1", regmatches(url, regexpr(pattern, url)))
  if (grepl("/league-matches/\\d+/\\d+\\.json$", url)) {
    want <- id_of("^.*/league-matches/(\\d+)/\\d+\\.json$")
    matches <- body[["matches"]]
    if (!.esports_nonempty(matches)) .abort_no_data("{where}: the schedule lists no matches.")
    got <- unique(vapply(matches, function(x) as.character(x[["season"]] %||% "NULL")[1], character(1)))
    if (!identical(got, want)) {
      got <- utils::head(sort(got), 3)
      .abort_asset_fetch("{where}: asked for season {want}, the body holds {.val {got}}.")
    }
  } else if (grepl("/visualization/shots/\\d+\\.json$", url)) {
    want <- id_of("^.*/visualization/shots/(\\d+)\\.json$")
    shots <- body[["match"]][["shots"]]
    if (!.esports_nonempty(shots)) .abort_no_data("{where}: the shot file is an empty shell (no shots recorded).")
    got <- as.character(body[["match"]][["id"]] %||% "NULL")
    if (!identical(got, want)) .abort_asset_fetch("{where}: asked for match {want}, the body is match {got}.")
  } else if (grepl("/widget/esports/match/\\d+\\.json$", url)) {
    want <- id_of("^.*/widget/esports/match/(\\d+)\\.json$")
    got <- as.character(body[["data"]][["gameData"]][["id"]] %||% "NULL")
    if (!identical(got, want)) .abort_asset_fetch("{where}: asked for match {want}, the body is match {got}.")
  } else if (grepl("/period-events\\.json$", url)) {
    if (!any(vapply(body, function(v) is.list(v) && is.null(names(v)) && length(v) > 0, logical(1)))) {
      .abort_no_data("{where}: the game has no events.")
    }
  }
  invisible(body)
}
