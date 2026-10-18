# **Get ESPN MCH Play-by-Play**

**Get ESPN MCH Play-by-Play**

## Usage

``` r
espn_mch_pbp(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble, one row per play:

|  |  |  |
|----|----|----|
| col_name | type | description |
| id | character | ESPN play identifier. |
| sequence_number | character | Play sequence number. |
| type_id | character | Play type identifier. |
| type_text | character | Play type display text. |
| text | character | Play description text. |
| away_score | integer | Away team score at time of play. |
| home_score | integer | Home team score at time of play. |
| scoring_play | logical | Whether the play resulted in a goal. |
| score_value | integer | Goal value (1 for regulation/OT goals). |
| shooting_play | logical | Whether the play was a shot attempt. |
| wallclock | character | Wall-clock timestamp (ISO 8601). |
| period_number | integer | Period number (1-3 regulation, 4+ OT/SO). |
| period_display_value | character | Period display label (e.g. "1st Period"). |
| clock_display_value | character | Game clock display string (e.g. "14:32"). |
| team_id | character | ESPN team identifier for the acting team. |
| coordinate_x | numeric | X coordinate of play location. |
| coordinate_y | numeric | Y coordinate of play location. |
| strength_id | character | Strength situation identifier. |
| strength_text | character | Strength situation text (e.g. "Even Strength"). |
| shot_info_id | character | Shot type identifier. |
| shot_info_text | character | Shot type text (e.g. "Wrist Shot"). |
| athlete_id_1 | character | Primary participant athlete identifier. |
| athlete_id_2 | character | Secondary participant athlete identifier. |
| athlete_id_3 | character | Tertiary participant athlete identifier. |
| home_team_id | character | Home team ESPN identifier. |
| home_team_name | character | Home team name. |
| home_team_location | character | Home team city/location. |
| home_team_abbrev | character | Home team abbreviation. |
| home_team_display_name | character | Home team display name. |
| home_team_color | character | Home team primary color hex. |
| home_team_alternate_color | character | Home team alternate color hex. |
| home_team_logo | character | Home team logo URL. |
| home_team_score | character | Home team final score. |
| home_team_winner | logical | Whether the home team won. |
| away_team_id | character | Away team ESPN identifier. |
| away_team_name | character | Away team name. |
| away_team_location | character | Away team city/location. |
| away_team_abbrev | character | Away team abbreviation. |
| away_team_display_name | character | Away team display name. |
| away_team_color | character | Away team primary color hex. |
| away_team_alternate_color | character | Away team alternate color hex. |
| away_team_logo | character | Away team logo URL. |
| away_team_score | character | Away team final score. |
| away_team_winner | logical | Whether the away team won. |
| game_id | character | ESPN event identifier (echoed from arg). |
| season | integer | Season end-year. |
| season_type | integer | Season type code. |
| game_date | character | Game date-time (ISO 8601). |

## Details

League slug `mens-college-hockey`. Parsed from the site-v2 `summary`
payload by the same helper as
[`espn_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_pbp.md);
columns verified against a captured mch summary body.

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
[`espn_mch_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_box.md),
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
[`espn_mch_season_draft_round_picks()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_draft_round_picks.md),
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
[`espn_mch_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_box.md),
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
  try(espn_mch_pbp(game_id = "401717648"))
#> ── MENS-COLLEGE-HOCKEY Play-by-Play data from ESPN.com ─────────────────────────
#> ℹ Data updated: 2026-10-18 12:02:54 UTC
#> # A tibble: 12 × 48
#>    id             sequence_number type_id type_text text   away_score home_score
#>    <chr>          <chr>           <chr>   <chr>     <chr>       <int>      <int>
#>  1 40171764804386 1               505     Goal      Even …          0          1
#>  2 40171764804387 2               505     Goal      Even …          1          1
#>  3 40171764804388 1               509     Penalty   Penal…          0          0
#>  4 40171764804389 3               505     Goal      Even …          1          2
#>  5 40171764804390 4               505     Goal      Even …          1          3
#>  6 40171764804391 2               509     Penalty   Penal…          0          0
#>  7 40171764804392 5               505     Goal      Power…          2          3
#>  8 40171764804393 3               509     Penalty   Penal…          0          0
#>  9 40171764804394 4               509     Penalty   Penal…          0          0
#> 10 40171764804395 6               505     Goal      Even …          2          4
#> 11 40171764804396 7               505     Goal      Even …          2          5
#> 12 40171764804397 8               505     Goal      Empty…          2          6
#> # ℹ 41 more variables: scoring_play <lgl>, score_value <int>,
#> #   shooting_play <lgl>, wallclock <chr>, period_number <int>,
#> #   period_display_value <chr>, clock_display_value <chr>, team_id <chr>,
#> #   coordinate_x <dbl>, coordinate_y <dbl>, strength_id <chr>,
#> #   strength_text <chr>, shot_info_id <chr>, shot_info_text <chr>,
#> #   athlete_id_1 <chr>, athlete_id_2 <chr>, athlete_id_3 <chr>,
#> #   home_team_id <chr>, home_team_name <chr>, home_team_location <chr>, …
# }
```
