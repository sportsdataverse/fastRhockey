# **Get ESPN NHL Game Team (Competitor Detail, core-v2)**

**Get ESPN NHL Game Team (Competitor Detail, core-v2)**

## Usage

``` r
espn_nhl_game_team(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier. Use
  [`espn_nhl_game_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_teams.md)
  to discover valid team ids for a game.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with competitor detail:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| team_id | character | ESPN team id (echoed from arg). |
| id | character | ESPN competitor identifier. |
| uid | character | Competitor uid string. |
| type | character | Competitor type (e.g. "team"). |
| order | integer | Competitor display order. |
| home_away | character | `"home"` or `"away"`. |
| winner | logical | Whether this competitor won the game. |
| team_ref | character | `$ref` URL for the team-in-season object. |
| score_ref | character | `$ref` URL for the score object. |
| linescores_ref | character | `$ref` URL for the per-period linescores. |
| roster_ref | character | `$ref` URL for the game roster. |
| statistics_ref | character | `$ref` URL for the team game statistics. |
| leaders_ref | character | `$ref` URL for the team game leaders. |
| record_ref | character | `$ref` URL for the team record at the time of the game. |
| competitor_ref | character | `$ref` URL for this competitor object. |

## See also

Other ESPN NHL Functions:
[`espn_nhl_athletes_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_athletes_index.md),
[`espn_nhl_award()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_award.md),
[`espn_nhl_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_awards.md),
[`espn_nhl_calendar()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar.md),
[`espn_nhl_calendar_offseason()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar_offseason.md),
[`espn_nhl_calendar_ondays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar_ondays.md),
[`espn_nhl_calendar_postseason()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar_postseason.md),
[`espn_nhl_calendar_regular_season()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar_regular_season.md),
[`espn_nhl_coach()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach.md),
[`espn_nhl_coach_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach_record.md),
[`espn_nhl_coach_season()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach_season.md),
[`espn_nhl_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coaches.md),
[`espn_nhl_conferences()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_conferences.md),
[`espn_nhl_countries()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_countries.md),
[`espn_nhl_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_draft.md),
[`espn_nhl_franchise()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_franchise.md),
[`espn_nhl_franchises()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_franchises.md),
[`espn_nhl_game()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game.md),
[`espn_nhl_game_all()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_all.md),
[`espn_nhl_game_broadcasts()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_broadcasts.md),
[`espn_nhl_game_competition()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_competition.md),
[`espn_nhl_game_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_leaders.md),
[`espn_nhl_game_odds()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_odds.md),
[`espn_nhl_game_official_detail()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_official_detail.md),
[`espn_nhl_game_officials()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_officials.md),
[`espn_nhl_game_play()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_play.md),
[`espn_nhl_game_play_personnel()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_play_personnel.md),
[`espn_nhl_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_plays.md),
[`espn_nhl_game_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_powerindex.md),
[`espn_nhl_game_predictor()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_predictor.md),
[`espn_nhl_game_probabilities()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_probabilities.md),
[`espn_nhl_game_propbets()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_propbets.md),
[`espn_nhl_game_scoringplays()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_scoringplays.md),
[`espn_nhl_game_situation()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_situation.md),
[`espn_nhl_game_status()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_status.md),
[`espn_nhl_game_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_leaders.md),
[`espn_nhl_game_team_linescores()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_linescores.md),
[`espn_nhl_game_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_record.md),
[`espn_nhl_game_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_roster.md),
[`espn_nhl_game_team_statistics()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_statistics.md),
[`espn_nhl_game_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_teams.md),
[`espn_nhl_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_games.md),
[`espn_nhl_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_injuries.md),
[`espn_nhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_leaders.md),
[`espn_nhl_leaders_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_leaders_core.md),
[`espn_nhl_league_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_league_notes.md),
[`espn_nhl_league_root()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_league_root.md),
[`espn_nhl_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_news.md),
[`espn_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_pbp.md),
[`espn_nhl_player_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_awards.md),
[`espn_nhl_player_bio()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_bio.md),
[`espn_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_box.md),
[`espn_nhl_player_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_career_stats.md),
[`espn_nhl_player_contracts()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_contracts.md),
[`espn_nhl_player_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_core.md),
[`espn_nhl_player_eventlog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_eventlog.md),
[`espn_nhl_player_gamelog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_gamelog.md),
[`espn_nhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_info.md),
[`espn_nhl_player_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_injuries.md),
[`espn_nhl_player_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_news.md),
[`espn_nhl_player_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_notes.md),
[`espn_nhl_player_overview()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_overview.md),
[`espn_nhl_player_records()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_records.md),
[`espn_nhl_player_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_seasons.md),
[`espn_nhl_player_splits()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_splits.md),
[`espn_nhl_player_statistics()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_statistics.md),
[`espn_nhl_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_statisticslog.md),
[`espn_nhl_player_stats_v3()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_stats_v3.md),
[`espn_nhl_player_vs_player()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_vs_player.md),
[`espn_nhl_players_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_players_index.md),
[`espn_nhl_position()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_position.md),
[`espn_nhl_positions()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_positions.md),
[`espn_nhl_providers()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_providers.md),
[`espn_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_schedule.md),
[`espn_nhl_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_scoreboard.md),
[`espn_nhl_season_athletes()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_athletes.md),
[`espn_nhl_season_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_awards.md),
[`espn_nhl_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_coaches.md),
[`espn_nhl_season_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_draft.md),
[`espn_nhl_season_draft_round_picks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_draft_round_picks.md),
[`espn_nhl_season_freeagents()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_freeagents.md),
[`espn_nhl_season_futures()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_futures.md),
[`espn_nhl_season_group()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_group.md),
[`espn_nhl_season_group_children()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_group_children.md),
[`espn_nhl_season_group_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_group_teams.md),
[`espn_nhl_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_groups.md),
[`espn_nhl_season_info()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_info.md),
[`espn_nhl_season_players()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_players.md),
[`espn_nhl_season_pointer()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_pointer.md),
[`espn_nhl_season_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_powerindex.md),
[`espn_nhl_season_powerindex_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_powerindex_leaders.md),
[`espn_nhl_season_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_team.md),
[`espn_nhl_season_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_teams.md),
[`espn_nhl_season_type()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type.md),
[`espn_nhl_season_type_corrections()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type_corrections.md),
[`espn_nhl_season_type_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type_leaders.md),
[`espn_nhl_season_types()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_types.md),
[`espn_nhl_season_week()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_week.md),
[`espn_nhl_season_week_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_week_games.md),
[`espn_nhl_season_weeks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_weeks.md),
[`espn_nhl_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_seasons.md),
[`espn_nhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_standings.md),
[`espn_nhl_standings_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_standings_core.md),
[`espn_nhl_statistics_league()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_statistics_league.md),
[`espn_nhl_summary()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_summary.md),
[`espn_nhl_talentpicks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_talentpicks.md),
[`espn_nhl_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team.md),
[`espn_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_box.md),
[`espn_nhl_team_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_core.md),
[`espn_nhl_team_depthcharts()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_depthcharts.md),
[`espn_nhl_team_history()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_history.md),
[`espn_nhl_team_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_injuries.md),
[`espn_nhl_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_leaders.md),
[`espn_nhl_team_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_news.md),
[`espn_nhl_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_record.md),
[`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md),
[`espn_nhl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_schedule.md),
[`espn_nhl_team_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_transactions.md),
[`espn_nhl_teams_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams_core.md),
[`espn_nhl_teams_site()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams_site.md),
[`espn_nhl_tournaments()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_tournaments.md),
[`espn_nhl_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_transactions.md),
[`espn_nhl_venue()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_venue.md),
[`espn_nhl_venues()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_venues.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_nhl_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_nhl_game_teams(event_id = eid)
    espn_nhl_game_team(event_id = eid, team_id = tms$team_id[1])
  })
#> ── NHL Game Team data from ESPN core-v2 ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:26:03 UTC
#> # A tibble: 1 × 17
#>   event_id  cid       team_id id    uid    type  order home_away winner team_ref
#>   <chr>     <chr>     <chr>   <chr> <chr>  <chr> <int> <chr>     <lgl>  <chr>   
#> 1 401688263 401688263 5       5     s:70~… team      0 home      TRUE   http://…
#> # ℹ 7 more variables: score_ref <chr>, linescores_ref <chr>, roster_ref <chr>,
#> #   statistics_ref <chr>, leaders_ref <chr>, record_ref <chr>,
#> #   competitor_ref <chr>
# }
```
