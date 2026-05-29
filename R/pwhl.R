#' @name pwhl
#' @aliases pwhl pwhl_overview pwhl_game pwhl_team pwhl_player pwhl_league
#' @title **PWHL (Professional Women's Hockey League) Endpoint Overview**
#' @description
#' Wrappers around the HockeyTech feeds that back the PWHL stats portal,
#' grouped by what they return. Three feed flavors are used internally
#' (`statviewfeed`, `modulekit`, `gc`) — see `pwhl_helpers.R` for the
#' `.pwhl_api()` / `.pwhl_modulekit_url()` / `.pwhl_gc_url()` helpers
#' that build the JSONP URLs and strip the Angular callbacks.
#'
#' @details
#'
#' ## **Game-level**
#'
#' | Function | Feed | Purpose |
#' |---|---|---|
#' | [pwhl_schedule()]      | statviewfeed | Season schedule |
#' | [pwhl_scorebar()]      | modulekit    | Recent + upcoming scorebar |
#' | [pwhl_game_info()]     | statviewfeed | Per-game metadata |
#' | [pwhl_game_summary()]  | gc           | Per-game summary (rosters, periods, three stars) |
#' | [pwhl_pbp()]           | statviewfeed | Play-by-play |
#' | [pwhl_player_box()]    | statviewfeed | Per-game player boxscore |
#'
#' ## **Team-level**
#'
#' | Function | Feed | Purpose |
#' |---|---|---|
#' | [pwhl_teams()]        | statviewfeed | Team listing |
#' | [pwhl_team_roster()]  | statviewfeed | Active roster |
#' | [pwhl_standings()]    | statviewfeed | Current standings |
#'
#' ## **Player-level**
#'
#' | Function | Feed | Purpose |
#' |---|---|---|
#' | [pwhl_player_info()]      | modulekit    | Player profile |
#' | [pwhl_player_stats()]     | modulekit    | Career / season stats |
#' | [pwhl_player_game_log()]  | modulekit    | Game-by-game player log |
#' | [pwhl_player_search()]    | modulekit    | Search by name |
#' | [pwhl_stats()]            | statviewfeed | League-wide stats |
#' | [pwhl_leaders()]          | modulekit    | League leaders (top scorers / goalies) |
#' | [pwhl_streaks()]          | modulekit    | Player streaks |
#'
#' ## **League-level**
#'
#' | Function | Feed | Purpose |
#' |---|---|---|
#' | [pwhl_season_id()]        | modulekit (with hardcoded fallback) | Map (season, game_type) -> HockeyTech `season_id` |
#' | [pwhl_transactions()]     | modulekit    | Player transactions |
#' | [pwhl_playoff_bracket()]  | modulekit    | Playoff bracket |
#' | [most_recent_pwhl_season()] | (computed) | Most-recent PWHL season (end-year) |
#'
#' @section Season Convention:
#'
#' PWHL functions use the **end year** of the season (e.g. `2026` for the
#' 2025-26 season), matching `most_recent_pwhl_season()`.
#'
#' @keywords PWHL
#' @family PWHL
NULL
