# **Get ESPN WCH Season Team (core-v2)**

**Get ESPN WCH Season Team (core-v2)**

## Usage

``` r
espn_wch_season_team(season = most_recent_nhl_season(), team_id, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- team_id:

  ESPN team identifier (character or numeric). Use
  [`espn_wch_season_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_season_teams.md)
  to discover valid team ids for a season.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with team-in-season metadata:

|                    |           |                                            |
|--------------------|-----------|--------------------------------------------|
| col_name           | types     | description                                |
| season             | integer   | Season year (echoed from arg).             |
| team_id            | character | ESPN team id (echoed from arg).            |
| id                 | character | ESPN team identifier.                      |
| guid               | character | Team global unique identifier.             |
| uid                | character | Team uid string.                           |
| slug               | character | Team URL slug.                             |
| location           | character | Team city/location.                        |
| name               | character | Team mascot/name.                          |
| nickname           | character | Team nickname.                             |
| abbreviation       | character | Team abbreviation.                         |
| display_name       | character | Team display name.                         |
| short_display_name | character | Team short display name.                   |
| color              | character | Team primary color hex.                    |
| alternate_color    | character | Team alternate color hex.                  |
| is_active          | logical   | Whether the team is currently active.      |
| is_all_star        | logical   | Whether this is an all-star team.          |
| team_ref           | character | `$ref` URL for this team-in-season object. |
| record_ref         | character | `$ref` URL for the team record.            |
| athletes_ref       | character | `$ref` URL for the team athletes.          |
| venue_ref          | character | `$ref` URL for the team venue.             |
| groups_ref         | character | `$ref` URL for the team groups.            |
| statistics_ref     | character | `$ref` URL for the team statistics.        |
| leaders_ref        | character | `$ref` URL for the team leaders.           |
| injuries_ref       | character | `$ref` URL for the team injuries.          |
| awards_ref         | character | `$ref` URL for the team awards.            |
| franchise_ref      | character | `$ref` URL for the team franchise.         |
| events_ref         | character | `$ref` URL for the team events.            |
| transactions_ref   | character | `$ref` URL for the team transactions.      |
| coaches_ref        | character | `$ref` URL for the team coaches.           |

## Details

League slug `womens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_team.md)
(columns inherited); a collection may be empty.

## See also

Other ESPN WCH Functions:
[`espn_wch_award()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_award.md),
[`espn_wch_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_awards.md),
[`espn_wch_calendar()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_calendar.md),
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
  try({
    tms <- espn_wch_season_teams(season = 2026)
    espn_wch_season_team(season = 2026, team_id = tms$team_id[1])
  })
#> ── WOMENS-COLLEGE-HOCKEY Season Team data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-10-30 00:10:55 UTC
#> # A tibble: 1 × 29
#>   season team_id id    guid     uid   slug  location name  nickname abbreviation
#>    <int> <chr>   <chr> <chr>    <chr> <chr> <chr>    <chr> <chr>    <chr>       
#> 1   2026 41      41    eaa8057… s:70… ucon… UConn    Husk… UConn    CONN        
#> # ℹ 19 more variables: display_name <chr>, short_display_name <chr>,
#> #   color <chr>, alternate_color <chr>, is_active <lgl>, is_all_star <lgl>,
#> #   team_ref <chr>, record_ref <chr>, athletes_ref <chr>, venue_ref <chr>,
#> #   groups_ref <chr>, statistics_ref <chr>, leaders_ref <chr>,
#> #   injuries_ref <chr>, awards_ref <chr>, franchise_ref <chr>,
#> #   events_ref <chr>, transactions_ref <chr>, coaches_ref <chr>
# }
```
