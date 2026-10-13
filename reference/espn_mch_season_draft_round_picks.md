# **Get ESPN MCH Season Draft Round Picks (core-v2)**

**Get ESPN MCH Season Draft Round Picks (core-v2)**

## Usage

``` r
espn_mch_season_draft_round_picks(
  season = most_recent_nhl_season(),
  round_num = 1,
  limit = 100,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- round_num:

  Draft round number (default `1`). Used to build the path
  `seasons/{season}/draft/rounds/{round_num}/picks`.

- limit:

  Maximum picks to return per page (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per draft pick, or an empty
tibble when the endpoint is unavailable. The confirmed path is
`seasons/{season}/draft/rounds/{round_num}/picks`; for NHL this returns
HTTP 404 — the draft data is not served by the core-v2 sub-tree:

|            |           |                                       |
|------------|-----------|---------------------------------------|
| col_name   | types     | description                           |
| ref        | character | `$ref` URL for the draft pick object. |
| pick_id    | character | Pick id parsed from the `$ref` URL.   |
| season     | integer   | Season year (echoed from arg).        |
| round_num  | integer   | Round number (echoed from arg).       |
| count      | integer   | Total picks in the collection.        |
| page_count | integer   | Total pages in the collection.        |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_draft_round_picks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_draft_round_picks.md)
(columns inherited); a collection may be empty.

## See also

Other ESPN MCH Functions:
[`espn_mch_award()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_award.md),
[`espn_mch_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_awards.md),
[`espn_mch_calendar()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_calendar.md),
[`espn_mch_coach()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_coach.md),
[`espn_mch_coach_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_coach_record.md),
[`espn_mch_coach_season()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_coach_season.md),
[`espn_mch_conferences()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_conferences.md),
[`espn_mch_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_draft.md),
[`espn_mch_fpi()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_fpi.md),
[`espn_mch_franchise()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_franchise.md),
[`espn_mch_franchises()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_franchises.md),
[`espn_mch_game()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game.md),
[`espn_mch_game_broadcasts()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_broadcasts.md),
[`espn_mch_game_competition()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_competition.md),
[`espn_mch_game_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_leaders.md),
[`espn_mch_game_odds()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_odds.md),
[`espn_mch_game_official_detail()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_official_detail.md),
[`espn_mch_game_officials()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_officials.md),
[`espn_mch_game_play()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_play.md),
[`espn_mch_game_play_personnel()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_play_personnel.md),
[`espn_mch_game_plays()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_plays.md),
[`espn_mch_game_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_powerindex.md),
[`espn_mch_game_predictor()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_predictor.md),
[`espn_mch_game_probabilities()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_probabilities.md),
[`espn_mch_game_propbets()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_propbets.md),
[`espn_mch_game_scoringplays()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_scoringplays.md),
[`espn_mch_game_situation()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_situation.md),
[`espn_mch_game_status()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_status.md),
[`espn_mch_game_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_team.md),
[`espn_mch_game_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_team_leaders.md),
[`espn_mch_game_team_linescores()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_team_linescores.md),
[`espn_mch_game_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_team_record.md),
[`espn_mch_game_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_team_roster.md),
[`espn_mch_game_team_statistics()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_team_statistics.md),
[`espn_mch_game_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_game_teams.md),
[`espn_mch_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_games.md),
[`espn_mch_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_injuries.md),
[`espn_mch_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_leaders.md),
[`espn_mch_leaders_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_leaders_core.md),
[`espn_mch_league_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_league_notes.md),
[`espn_mch_league_root()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_league_root.md),
[`espn_mch_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_news.md),
[`espn_mch_player_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_awards.md),
[`espn_mch_player_bio()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_bio.md),
[`espn_mch_player_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_career_stats.md),
[`espn_mch_player_contracts()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_contracts.md),
[`espn_mch_player_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_core.md),
[`espn_mch_player_eventlog()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_eventlog.md),
[`espn_mch_player_gamelog()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_gamelog.md),
[`espn_mch_player_info()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_info.md),
[`espn_mch_player_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_injuries.md),
[`espn_mch_player_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_news.md),
[`espn_mch_player_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_notes.md),
[`espn_mch_player_overview()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_overview.md),
[`espn_mch_player_records()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_records.md),
[`espn_mch_player_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_seasons.md),
[`espn_mch_player_splits()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_splits.md),
[`espn_mch_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_statisticslog.md),
[`espn_mch_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_stats.md),
[`espn_mch_player_vs_player()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_vs_player.md),
[`espn_mch_players_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_players_index.md),
[`espn_mch_position()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_position.md),
[`espn_mch_positions()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_positions.md),
[`espn_mch_rankings()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_rankings.md),
[`espn_mch_recruiting_players()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_recruiting_players.md),
[`espn_mch_recruiting_rankings()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_recruiting_rankings.md),
[`espn_mch_recruiting_years()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_recruiting_years.md),
[`espn_mch_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_scoreboard.md),
[`espn_mch_season_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_awards.md),
[`espn_mch_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_coaches.md),
[`espn_mch_season_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_draft.md),
[`espn_mch_season_freeagents()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_freeagents.md),
[`espn_mch_season_futures()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_futures.md),
[`espn_mch_season_group()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_group.md),
[`espn_mch_season_group_children()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_group_children.md),
[`espn_mch_season_group_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_group_teams.md),
[`espn_mch_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_groups.md),
[`espn_mch_season_info()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_info.md),
[`espn_mch_season_players()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_players.md),
[`espn_mch_season_pointer()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_pointer.md),
[`espn_mch_season_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_powerindex.md),
[`espn_mch_season_powerindex_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_powerindex_leaders.md),
[`espn_mch_season_recruits()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_recruits.md),
[`espn_mch_season_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_team.md),
[`espn_mch_season_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_teams.md),
[`espn_mch_season_type()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_type.md),
[`espn_mch_season_type_corrections()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_type_corrections.md),
[`espn_mch_season_type_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_type_leaders.md),
[`espn_mch_season_types()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_types.md),
[`espn_mch_season_week()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_week.md),
[`espn_mch_season_week_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_week_games.md),
[`espn_mch_season_week_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_week_powerindex.md),
[`espn_mch_season_week_rankings()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_week_rankings.md),
[`espn_mch_season_weeks()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_weeks.md),
[`espn_mch_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_seasons.md),
[`espn_mch_standings()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_standings.md),
[`espn_mch_standings_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_standings_core.md),
[`espn_mch_statistics_league()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_statistics_league.md),
[`espn_mch_summary()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_summary.md),
[`espn_mch_talentpicks()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_talentpicks.md),
[`espn_mch_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team.md),
[`espn_mch_team_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_core.md),
[`espn_mch_team_depthcharts()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_depthcharts.md),
[`espn_mch_team_history()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_history.md),
[`espn_mch_team_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_injuries.md),
[`espn_mch_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_leaders.md),
[`espn_mch_team_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_news.md),
[`espn_mch_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_record.md),
[`espn_mch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_roster.md),
[`espn_mch_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_schedule.md),
[`espn_mch_team_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_transactions.md),
[`espn_mch_teams_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_teams_core.md),
[`espn_mch_teams_site()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_teams_site.md),
[`espn_mch_tournaments()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_tournaments.md),
[`espn_mch_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_transactions.md),
[`espn_mch_venue()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_venue.md),
[`espn_mch_venues()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_venues.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try(espn_mch_season_draft_round_picks(season = 2025, round_num = 1))
#> ! 2026-10-13 01:52:24.932812: ESPN mens-college-hockey season draft round picks for 2025 round 1 is not available (HTTP 404 is normal for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
