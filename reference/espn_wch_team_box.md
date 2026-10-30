# **Get ESPN WCH Team Box Score**

**Get ESPN WCH Team Box Score**

## Usage

``` r
espn_wch_team_box(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble, one row per team (2 rows):

|  |  |  |
|----|----|----|
| col_name | type | description |
| team_id | character | ESPN team identifier. |
| team_uid | character | ESPN team uid. |
| team_slug | character | Team URL slug. |
| team_location | character | Team city/location. |
| team_name | character | Team name (mascot). |
| team_abbreviation | character | Team abbreviation. |
| team_display_name | character | Team display name. |
| team_short_display_name | character | Team short display name. |
| team_color | character | Team primary color hex. |
| team_alternate_color | character | Team alternate color hex. |
| team_logo | character | Team logo URL. |
| home_away | character | "home" or "away". |
| game_id | character | ESPN event identifier (echoed from arg). |
| season | integer | Season end-year. |
| season_type | integer | Season type code. |
| game_date | character | Game date-time (ISO 8601). |
| blocked_shots | character | Total blocked shots. |
| hits | character | Total hits. |
| takeaways | character | Total takeaways. |
| shots_total | character | Total shots on goal. |
| power_play_goals | character | Power play goals. |
| power_play_opportunities | character | Power play opportunities. |
| power_play_pct | character | Power play percentage. |
| short_handed_goals | character | Short-handed goals. |
| shootout_goals | character | Shootout goals. |
| faceoffs_won | character | Faceoffs won. |
| faceoff_percent | character | Faceoff win percentage. |
| giveaways | character | Giveaways. |
| penalties | character | Penalty count. |
| penalty_minutes | character | Penalty minutes. |

## Details

League slug `womens-college-hockey`. Parsed from the site-v2 `summary`
payload by the same helper as
[`espn_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_box.md);
columns verified against a captured wch summary body.

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
  try(espn_wch_team_box(game_id = "401762970"))
#> ── WOMENS-COLLEGE-HOCKEY Team Box Score data from ESPN.com ─────────────────────
#> ℹ Data updated: 2026-10-30 00:11:02 UTC
#> # A tibble: 2 × 30
#>   team_id team_uid         team_slug   team_location team_name team_abbreviation
#>   <chr>   <chr>            <chr>       <chr>         <chr>     <chr>            
#> 1 2137    s:70~l:92~t:2137 clarkson-g… Clarkson      Golden K… CLAR             
#> 2 275     s:70~l:92~t:275  wisconsin-… Wisconsin     Badgers   WIS              
#> # ℹ 24 more variables: team_display_name <chr>, team_short_display_name <chr>,
#> #   team_color <chr>, team_alternate_color <chr>, team_logo <chr>,
#> #   home_away <chr>, game_id <chr>, season <int>, season_type <int>,
#> #   game_date <chr>, blocked_shots <chr>, hits <chr>, takeaways <chr>,
#> #   shots_total <chr>, power_play_goals <chr>, power_play_opportunities <chr>,
#> #   power_play_pct <chr>, short_handed_goals <chr>, shootout_goals <chr>,
#> #   faceoffs_won <chr>, faceoff_percent <chr>, giveaways <chr>, …
# }
```
