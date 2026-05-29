#' @name nhl_records
#' @aliases nhl_records nhl_records_franchise nhl_records_player nhl_records_skater nhl_records_goalie nhl_records_draft
#' @title **NHL Records API Endpoint Overview**
#' @description
#' Wrappers around the NHL Records backend at `records.nhl.com/site/api/...`.
#' These cover all 442 documented record-keeping endpoints (franchises,
#' players, draft, trophies, awards, attendance, venues, officials, combine).
#' All 25 wrappers route through `.nhl_records_api()` in
#' `helpers_nhl_records.R` which standardizes the `cayenneExp` filtering,
#' `sort` + `limit` + `start` pagination, and the `{data: [...], total: N}`
#' tabular response shape.
#'
#' @details
#'
#' ## **Franchise**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_records_franchise()]                     | `franchise`                       | Franchise listing |
#' | [nhl_records_franchise_detail()]              | `franchise-detail`                | Franchise details |
#' | [nhl_records_franchise_totals()]              | `franchise-team-totals`           | All-time team totals |
#' | [nhl_records_franchise_team_totals()]         | `franchise-team-totals`           | Team totals (alt) |
#' | [nhl_records_franchise_season_results()]      | `franchise-season-results`        | Per-season results |
#' | [nhl_records_franchise_playoff_appearances()] | `franchise-playoff-appearances`   | Playoff appearances |
#'
#' ## **Player**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_records_player()]                            | `player`                          | Players listing |
#' | [nhl_records_player_byteam()]                     | `player`                          | Players filtered by team |
#' | [nhl_records_player_stats()]                      | `player-stats`                    | Player stats |
#' | [nhl_records_skater_real_time_stats_career()]     | `skater-real-time-stats/career`   | Career real-time skater stats |
#' | [nhl_records_skater_real_time_stats_season()]     | `skater-real-time-stats/season`   | Season real-time skater stats |
#' | [nhl_records_goalie_career_stats()]               | `goalie-career-stats`             | Career goalie stats |
#' | [nhl_records_goalie_season_stats()]               | `goalie-season-stats`             | Season goalie stats |
#' | [nhl_records_goalie_shutout_streak()]             | `goalie-shutout-streak`           | Goalie shutout-streak records |
#'
#' ## **Draft**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_records_draft()]                | `draft`                  | Draft listing |
#' | [nhl_records_draft_lottery_odds()]   | `draft-lottery-odds`     | Lottery odds |
#' | [nhl_records_draft_lottery_picks()]  | `draft-lottery-picks`    | Lottery pick results |
#' | [nhl_records_draft_prospect()]       | `draft-prospect`         | Draft-eligible prospects |
#' | [nhl_records_combine()]              | `draft-combine`          | Draft combine measurements |
#'
#' ## **Honors and miscellany**
#'
#' | Function | Endpoint family | Purpose |
#' |---|---|---|
#' | [nhl_records_trophy()]         | `trophy`                | Trophy listing |
#' | [nhl_records_award_details()]  | `trophy/{id}`           | Trophy / award winners |
#' | [nhl_records_hof_players()]    | `hof-player`            | Hall of Fame |
#' | [nhl_records_officials()]      | `officials`             | On-ice officials |
#' | [nhl_records_attendance()]     | `attendance`            | Attendance records |
#' | [nhl_records_venue()]          | `venue`                 | Arena / venue listing |
#'
#' @keywords NHL Records
#' @family NHL Records
NULL
