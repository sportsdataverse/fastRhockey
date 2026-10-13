# **Get ESPN WCH Team Roster**

**Get ESPN WCH Team Roster**

## Usage

``` r
espn_wch_team_roster(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"2137"` for
  Pittsburgh Penguins).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per player:

|                              |           |                                     |
|------------------------------|-----------|-------------------------------------|
| col_name                     | types     | description                         |
| position_group               | character | Position group name (e.g. Centers). |
| id                           | character | ESPN player identifier.             |
| uid                          | character | ESPN player uid.                    |
| guid                         | character | Player global unique identifier.    |
| alternate_id                 | character | Alternate player identifier.        |
| alternate_ids_sdr            | character | Alternate ids sdr.                  |
| first_name                   | character | First name.                         |
| last_name                    | character | Last name.                          |
| full_name                    | character | Full name.                          |
| display_name                 | character | Display name.                       |
| short_name                   | character | Short name.                         |
| weight                       | numeric   | Weight (lbs).                       |
| display_weight               | character | Display weight string.              |
| height                       | numeric   | Height (inches).                    |
| display_height               | character | Display height string.              |
| age                          | integer   | Player age.                         |
| date_of_birth                | character | Date of birth.                      |
| slug                         | character | URL slug.                           |
| jersey                       | character | Jersey number.                      |
| headshot_href                | character | Headshot image URL.                 |
| headshot_alt                 | character | Headshot alt text.                  |
| hand_type                    | character | Shooting/catching hand type.        |
| hand_abbreviation            | character | Hand abbreviation.                  |
| hand_display_value           | character | Hand display value.                 |
| position_id                  | character | Position identifier.                |
| position_name                | character | Position name.                      |
| position_display_name        | character | Position display name.              |
| position_abbreviation        | character | Position abbreviation.              |
| position_leaf                | logical   | Whether position is a leaf node.    |
| position_parent_id           | character | Parent position identifier.         |
| position_parent_name         | character | Parent position name.               |
| position_parent_display_name | character | Parent position display name.       |
| position_parent_abbreviation | character | Parent position abbreviation.       |
| position_parent_leaf         | logical   | Whether parent position is leaf.    |
| experience_years             | integer   | Experience years.                   |
| debut_year                   | numeric   | Debut year.                         |
| status_id                    | character | Status identifier.                  |
| status_name                  | character | Status name.                        |
| status_type                  | character | Status type.                        |
| status_abbreviation          | character | Status abbreviation.                |
| birth_place_city             | character | Birth place city.                   |
| birth_place_state            | character | Birth place state.                  |
| birth_place_country          | character | Birth place country.                |
| birth_place_display_text     | character | Birth place display text.           |
| birth_country_abbreviation   | character | Birth country abbreviation.         |
| college_id                   | character | College identifier.                 |
| college_guid                 | character | College guid.                       |
| college_mascot               | character | College mascot.                     |
| college_name                 | character | College name.                       |
| college_short_name           | character | College short name.                 |
| college_abbrev               | character | College abbreviation.               |
| college_logos                | character | College logo URLs (pipe-delimited). |
| team_id                      | character | ESPN team identifier (from arg).    |

## Details

League slug `womens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md)
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
[`espn_wch_player_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_awards.md),
[`espn_wch_player_bio()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_player_bio.md),
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
[`espn_wch_team_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_core.md),
[`espn_wch_team_depthcharts()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_depthcharts.md),
[`espn_wch_team_history()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_history.md),
[`espn_wch_team_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_injuries.md),
[`espn_wch_team_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_leaders.md),
[`espn_wch_team_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_news.md),
[`espn_wch_team_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_wch_team_record.md),
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
  try(espn_wch_team_roster(team_id = "2137"))
#> data frame with 0 columns and 0 rows
# }
```
