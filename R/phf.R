#' @name phf
#' @aliases phf phf_deprecated
#' @title **PHF (Premier Hockey Federation) Endpoint Overview — DEPRECATED**
#' @description
#' Wrappers for the now-defunct Premier Hockey Federation (PHF, formerly
#' NWHL). The PHF ceased operations in mid-2023, so these functions are
#' kept in the package for historical access to cached data but are
#' formally **deprecated** in v1.0.0 and emit
#' `lifecycle::deprecate_stop()`. Tests for these functions check for the
#' `lifecycle_error_deprecated` class.
#'
#' @details
#'
#' ## **Game-level**
#'
#' | Function | Purpose |
#' |---|---|
#' | [phf_game_all()]     | Per-game payload (all sections) |
#' | [phf_game_raw()]     | Raw API response |
#' | [phf_game_details()] | Game details only |
#' | [phf_game_summary()] | Game summary only |
#' | [phf_pbp()]          | Per-game play-by-play + loader entry point |
#' | [phf_player_box()]   | Per-game player boxscore |
#' | [phf_team_box()]     | Per-game team boxscore |
#'
#' ## **Season-level**
#'
#' | Function | Purpose |
#' |---|---|
#' | [phf_schedule()]      | Season schedule |
#' | [phf_standings()]     | Standings |
#' | [phf_team_roster()]   | Team rosters |
#' | [phf_team_stats()]    | Team season stats |
#' | [phf_player_stats()]  | Player season stats |
#' | [phf_leaders()]       | League leaders |
#' | [phf_league_info()]   | League info |
#'
#' @section Deprecation:
#'
#' All PHF wrappers call `lifecycle::deprecate_stop()` and raise errors.
#' Use the still-active [pwhl] family for current women's pro hockey.
#'
#' @keywords PHF Deprecated
#' @family PHF
NULL
