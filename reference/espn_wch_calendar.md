# **Get ESPN WCH Calendar**

**Get ESPN WCH Calendar**

## Usage

``` r
espn_wch_calendar(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game date:

|  |  |  |
|----|----|----|
| col_name | type | description |
| date | character | Game date (ISO 8601 datetime string). |
| calendar_type | character | Calendar type (e.g. `"day"`). |
| calendar_start_date | character | Calendar season start date. |
| calendar_end_date | character | Calendar season end date. |
| calendar_is_whitelist | logical | Whether dates are a whitelist of game days. |

## Details

League slug `womens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_calendar()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar.md);
columns verified against a captured WCH body.

## See also

Other ESPN WCH Functions:
[`espn_wch_award()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_award.md),
[`espn_wch_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_awards.md),
[`espn_wch_coach()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_coach.md),
[`espn_wch_coach_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_coach_record.md),
[`espn_wch_coach_season()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_coach_season.md),
[`espn_wch_conferences()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_conferences.md),
[`espn_wch_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_draft.md),
[`espn_wch_fpi()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_fpi.md),
[`espn_wch_franchise()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_franchise.md),
[`espn_wch_franchises()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_franchises.md),
[`espn_wch_game()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game.md),
[`espn_wch_game_broadcasts()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_broadcasts.md),
[`espn_wch_game_competition()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_competition.md),
[`espn_wch_game_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_leaders.md),
[`espn_wch_game_odds()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_odds.md),
[`espn_wch_game_official_detail()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_official_detail.md),
[`espn_wch_game_officials()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_officials.md),
[`espn_wch_game_play()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_play.md),
[`espn_wch_game_play_personnel()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_play_personnel.md),
[`espn_wch_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_plays.md),
[`espn_wch_game_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_powerindex.md),
[`espn_wch_game_predictor()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_predictor.md),
[`espn_wch_game_probabilities()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_probabilities.md),
[`espn_wch_game_propbets()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_propbets.md),
[`espn_wch_game_scoringplays()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_scoringplays.md),
[`espn_wch_game_situation()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_situation.md),
[`espn_wch_game_status()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_status.md),
[`espn_wch_game_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_team.md),
[`espn_wch_game_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_team_leaders.md),
[`espn_wch_game_team_linescores()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_team_linescores.md),
[`espn_wch_game_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_team_record.md),
[`espn_wch_game_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_team_roster.md),
[`espn_wch_game_team_statistics()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_team_statistics.md),
[`espn_wch_game_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_game_teams.md),
[`espn_wch_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_games.md),
[`espn_wch_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_injuries.md),
[`espn_wch_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_leaders.md),
[`espn_wch_leaders_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_leaders_core.md),
[`espn_wch_league_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_league_notes.md),
[`espn_wch_league_root()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_league_root.md),
[`espn_wch_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_news.md),
[`espn_wch_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_pbp.md),
[`espn_wch_player_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_awards.md),
[`espn_wch_player_bio()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_bio.md),
[`espn_wch_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_box.md),
[`espn_wch_player_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_career_stats.md),
[`espn_wch_player_contracts()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_contracts.md),
[`espn_wch_player_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_core.md),
[`espn_wch_player_eventlog()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_eventlog.md),
[`espn_wch_player_gamelog()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_gamelog.md),
[`espn_wch_player_info()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_info.md),
[`espn_wch_player_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_injuries.md),
[`espn_wch_player_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_news.md),
[`espn_wch_player_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_notes.md),
[`espn_wch_player_overview()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_overview.md),
[`espn_wch_player_records()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_records.md),
[`espn_wch_player_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_seasons.md),
[`espn_wch_player_splits()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_splits.md),
[`espn_wch_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_statisticslog.md),
[`espn_wch_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_stats.md),
[`espn_wch_player_vs_player()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_vs_player.md),
[`espn_wch_players_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_players_index.md),
[`espn_wch_position()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_position.md),
[`espn_wch_positions()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_positions.md),
[`espn_wch_rankings()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_rankings.md),
[`espn_wch_recruiting_players()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_recruiting_players.md),
[`espn_wch_recruiting_rankings()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_recruiting_rankings.md),
[`espn_wch_recruiting_years()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_recruiting_years.md),
[`espn_wch_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_scoreboard.md),
[`espn_wch_season_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_awards.md),
[`espn_wch_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_coaches.md),
[`espn_wch_season_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_draft.md),
[`espn_wch_season_draft_round_picks()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_draft_round_picks.md),
[`espn_wch_season_freeagents()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_freeagents.md),
[`espn_wch_season_futures()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_futures.md),
[`espn_wch_season_group()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_group.md),
[`espn_wch_season_group_children()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_group_children.md),
[`espn_wch_season_group_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_group_teams.md),
[`espn_wch_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_groups.md),
[`espn_wch_season_info()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_info.md),
[`espn_wch_season_players()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_players.md),
[`espn_wch_season_pointer()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_pointer.md),
[`espn_wch_season_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_powerindex.md),
[`espn_wch_season_powerindex_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_powerindex_leaders.md),
[`espn_wch_season_recruits()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_recruits.md),
[`espn_wch_season_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_team.md),
[`espn_wch_season_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_teams.md),
[`espn_wch_season_type()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_type.md),
[`espn_wch_season_type_corrections()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_type_corrections.md),
[`espn_wch_season_type_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_type_leaders.md),
[`espn_wch_season_types()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_types.md),
[`espn_wch_season_week()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_week.md),
[`espn_wch_season_week_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_week_games.md),
[`espn_wch_season_week_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_week_powerindex.md),
[`espn_wch_season_week_rankings()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_week_rankings.md),
[`espn_wch_season_weeks()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_weeks.md),
[`espn_wch_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_seasons.md),
[`espn_wch_standings()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_standings.md),
[`espn_wch_standings_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_standings_core.md),
[`espn_wch_statistics_league()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_statistics_league.md),
[`espn_wch_summary()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_summary.md),
[`espn_wch_talentpicks()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_talentpicks.md),
[`espn_wch_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team.md),
[`espn_wch_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_box.md),
[`espn_wch_team_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_core.md),
[`espn_wch_team_depthcharts()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_depthcharts.md),
[`espn_wch_team_history()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_history.md),
[`espn_wch_team_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_injuries.md),
[`espn_wch_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_leaders.md),
[`espn_wch_team_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_news.md),
[`espn_wch_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_record.md),
[`espn_wch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_roster.md),
[`espn_wch_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_schedule.md),
[`espn_wch_team_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_transactions.md),
[`espn_wch_teams_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_teams_core.md),
[`espn_wch_teams_site()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_teams_site.md),
[`espn_wch_tournaments()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_tournaments.md),
[`espn_wch_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_transactions.md),
[`espn_wch_venue()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_venue.md),
[`espn_wch_venues()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_venues.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_wch_calendar())
#> ── WOMENS-COLLEGE-HOCKEY Calendar data from ESPN.com ───────────────────────────
#> ℹ Data updated: 2026-10-30 00:10:32 UTC
#> # A tibble: 83 × 5
#>    date              calendar_type calendar_start_date calendar_end_date
#>    <chr>             <chr>         <chr>               <chr>            
#>  1 2026-09-18T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  2 2026-09-19T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  3 2026-09-24T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  4 2026-09-25T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  5 2026-09-26T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  6 2026-10-01T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  7 2026-10-02T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  8 2026-10-03T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  9 2026-10-04T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#> 10 2026-10-06T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#> # ℹ 73 more rows
#> # ℹ 1 more variable: calendar_is_whitelist <lgl>
# }
```
