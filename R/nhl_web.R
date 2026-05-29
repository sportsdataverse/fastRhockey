#' @name nhl_web
#' @aliases nhl_web nhl_game nhl_schedule_family nhl_team_family nhl_player_family
#' @title **NHL Web API Endpoint Overview**
#' @description
#' Wrappers around the modern NHL Web API at `api-web.nhle.com/v1/...`.
#' These cover game feeds, schedules, standings, rosters, gamecenter,
#' draft, scoreboard, scores, meta, location, partner-game,
#' where-to-watch, smartlinks, postal-lookup, ppt-replay, and WSC PBP.
#'
#' All responses are clean JSON parsed with `jsonlite::fromJSON()` and
#' wrapped in the `fastRhockey_data` S3 class via `make_fastRhockey_data()`.
#'
#' @details
#'
#' ## **Game-level**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_game_feed()]      | `gamecenter/{id}/play-by-play` | Full PBP + rosters + game info |
#' | [nhl_game_boxscore()]  | `gamecenter/{id}/boxscore`     | Player + team boxscore tables |
#' | [nhl_game_shifts()]    | `stats/rest/en/shiftcharts`    | Per-shift records (with HTML fallback for empty endpoints) |
#' | [nhl_game_story()]     | `wsc/game-story/{id}`          | Narrative-format recap |
#' | [nhl_game_content()]   | `gamecenter/{id}/...`          | Media + content |
#' | [nhl_gamecenter_landing()]     | `gamecenter/{id}/landing`      | Game-center landing payload |
#' | [nhl_wsc_pbp()]        | `wsc/play-by-play/{id}`        | WSC narrative-format play-by-play |
#' | [nhl_ppt_replay()]     | `ppt-replay/...`               | Event-level replay metadata |
#' | [nhl_ppt_replay_goal()]| `ppt-replay/goal/...`          | Goal-specific replay metadata |
#'
#' ## **Schedule / scores**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_schedule()]              | `schedule/{date}`              | Daily / season schedule |
#' | [nhl_schedule_calendar()]     | `schedule-calendar/{date}`     | Calendar view |
#' | [nhl_club_schedule()]         | `club-schedule/{team}/season`  | Per-club schedule |
#' | [nhl_scoreboard()]            | `scoreboard/{date}/now`        | Live + daily scoreboard |
#' | [nhl_scores()]                | `score/{date}`                 | Scores |
#' | [nhl_tv_schedule()]           | `network/tv-schedule`          | TV broadcast schedule |
#' | [nhl_where_to_watch()]        | `where-to-watch/{date}`        | Region-aware watch info |
#'
#' ## **Standings / seasons**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_standings()]         | `standings/{date}`     | Current / historical standings |
#' | [nhl_standings_season()]  | `standings-season`     | Season list |
#' | [nhl_seasons()]           | `seasons` + rankings   | Seasons + draft rankings |
#' | [nhl_playoff_carousel()]  | `playoff-series/carousel/...` | Playoff carousel |
#' | [nhl_playoff_schedule()]  | `playoff-series/{year}/{seriesLetter}` | Playoff schedule |
#'
#' ## **Teams**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_teams()]              | `teams`                       | Team info |
#' | [nhl_teams_info()]         | (via `nhl_teams`)             | Team info by abbreviation |
#' | [nhl_teams_roster()]       | `roster/{team}/current`       | Current roster |
#' | [nhl_roster_season()]      | `roster/{team}/{season}`      | Roster by season |
#' | [nhl_team_prospects()]     | `prospects/{team}`            | Team prospects |
#' | [nhl_team_scoreboard()]    | `scoreboard/{team}/now`       | Team-specific scoreboard |
#' | [nhl_team_summary_range()] | (helper)                      | Multi-season team summary |
#' | [nhl_conferences()]        | (derived from standings)      | Conferences |
#' | [nhl_conferences_info()]   | (derived)                     | Conference details |
#' | [nhl_divisions()]          | (derived)                     | Divisions |
#' | [nhl_divisions_info()]     | (derived)                     | Division details |
#'
#' ## **Players**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_player_info()]            | `player/{id}/landing`        | Player biographical info |
#' | [nhl_player_game_log()]        | `player/{id}/game-log`       | Player game logs |
#' | [nhl_player_spotlight()]       | `player-spotlight`           | Featured player spotlight |
#' | [nhl_skater_summary_range()]   | (helper)                     | Multi-season skater summary |
#' | [nhl_goalie_summary_range()]   | (helper)                     | Multi-season goalie summary |
#' | [nhl_all_players_by_season()]  | (helper)                     | All rostered players across teams for a season |
#'
#' ## **Aggregators**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_game_ids_by_season()]   | (helper)   | All game IDs across teams for a season |
#'
#' ## **Meta / location / smart-links**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_meta()]            | `meta` + playoff-series + per-game meta | League / game / series metadata |
#' | [nhl_postal_lookup()]   | `postal-lookup/{zip}`                    | Broadcast region by postal code |
#' | [nhl_smartlinks()]      | NHL.com smart-link router                | Smart-link resolver |
#'
#' @keywords NHL Web API
#' @family NHL Web API
NULL
