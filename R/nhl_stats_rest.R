#' @name nhl_stats_rest
#' @aliases nhl_stats_rest nhl_stats nhl_stats_skaters nhl_stats_goalies nhl_stats_teams
#' @title **NHL Stats REST API Endpoint Overview**
#' @description
#' Wrappers around the NHL Stats REST backend at
#' `api.nhle.com/stats/rest/{lang}/...`. These cover the documented
#' Cayenne-filterable endpoints for skater / goalie / team stats, draft
#' stats, franchise + player listings, glossary, country, config, leaders,
#' and milestones. Most pages use the consistent
#' `{data: [...], total: N}` shape and accept `cayenneExp`, `sort`,
#' `limit`, `start` query parameters.
#'
#' @details
#'
#' ## **Skater / goalie / team stats**
#'
#' | Function | Endpoint | Purpose |
#' |---|---|---|
#' | [nhl_stats_skaters()]            | `skater/summary`             | Skater season stats |
#' | [nhl_stats_goalies()]            | `goalie/summary`             | Goalie season stats |
#' | [nhl_stats_teams()]              | `team/summary`               | Team season stats |
#' | [nhl_stats_misc()]               | `miscellaneousSkaterStats`   | Misc skater stats + draft + season list |
#' | [nhl_stats_skater_leaders()]     | `leaders/skaters/{attribute}`| Skater leaderboards by attribute |
#' | [nhl_stats_goalie_leaders()]     | `leaders/goalies/{attribute}`| Goalie leaderboards by attribute |
#' | [nhl_stats_skater_milestones()]  | `skater/milestones`          | Skater milestone achievements |
#' | [nhl_stats_goalie_milestones()]  | `goalie/milestones`          | Goalie milestone achievements |
#'
#' ## **Franchise / player listings**
#'
#' | Function | Endpoint | Purpose |
#' |---|---|---|
#' | [nhl_stats_franchise()]      | `franchise`        | Franchise listing |
#' | [nhl_stats_players()]        | `players`          | Players listing (requires `cayenne_exp`) |
#' | [nhl_stats_team_listing()]   | `team`             | Top-level team listing |
#' | [nhl_stats_game_listing()]   | `game`             | Top-level game listing |
#'
#' ## **Metadata helpers**
#'
#' | Function | Endpoint | Purpose |
#' |---|---|---|
#' | [nhl_stats_glossary()]        | `glossary`            | Stat-definition glossary |
#' | [nhl_stats_country()]         | `country`             | Country lookup |
#' | [nhl_stats_config()]          | `config`              | Configuration payload |
#' | [nhl_stats_ping()]            | `ping`                | Health check |
#' | [nhl_stats_content_module()]  | (NHL CMS)             | NHL.com content modules |
#'
#' @section Notes:
#'
#' - The `leaders/{skaters,goalies}/{attribute}` endpoint does **not** accept
#'   `start` / `limit` query parameters (returns 500 if you pass them).
#' - Valid `goalie` leader attributes are restricted to `savePctg`, `gaa`,
#'   and `shutouts`.
#'
#' @keywords NHL Stats REST
#' @family NHL Stats REST
NULL
