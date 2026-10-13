# **Get ESPN MCH Scoreboard**

**Get ESPN MCH Scoreboard**

## Usage

``` r
espn_mch_scoreboard(dates = NULL, ...)
```

## Arguments

- dates:

  Optional date or date-range string in `"YYYYMMDD"` or
  `"YYYYMMDD-YYYYMMDD"` format. When `NULL` (default) the current day is
  used.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | character | ESPN event identifier. |
| uid | character | ESPN event uid. |
| date | character | Game date-time (ISO 8601). |
| name | character | Full game name. |
| short_name | character | Short game name. |
| season_year | integer | Season end year. |
| season_type | integer | Season type code (1=pre, 2=regular, 3=post). |
| season_slug | character | Season type slug. |
| status_type_id | character | Status type identifier. |
| status_type_name | character | Status type name. |
| status_type_state | character | Status state (pre/in/post). |
| status_type_completed | logical | Whether the game is complete. |
| status_type_description | character | Status description. |
| status_type_detail | character | Status detail text. |
| status_type_short_detail | character | Short status detail. |
| status_clock | numeric | Game clock in seconds. |
| status_display_clock | character | Display clock string. |
| status_period | integer | Current period. |
| neutral_site | logical | Whether the game is at a neutral site. |
| conference_competition | character | Whether it is a conference competition. |
| attendance | integer | Game attendance. |
| venue_id | character | ESPN venue identifier. |
| venue_full_name | character | Venue full name. |
| venue_city | character | Venue city. |
| venue_state | character | Venue state. |
| venue_indoor | logical | Whether the venue is indoors. |
| broadcast | character | Broadcast network(s). |
| note | character | Game note or headline. |
| home_id | character | Home team ESPN identifier. |
| home_name | character | Home team name. |
| home_abbreviation | character | Home team abbreviation. |
| home_display_name | character | Home team display name. |
| home_location | character | Home team city/location. |
| home_color | character | Home team primary color hex. |
| home_alternate_color | character | Home team alternate color hex. |
| home_logo | character | Home team logo URL. |
| home_score | character | Home team score. |
| home_winner | logical | Whether the home team won. |
| home_rank | integer | Home team rank (if ranked). |
| away_id | character | Away team ESPN identifier. |
| away_name | character | Away team name. |
| away_abbreviation | character | Away team abbreviation. |
| away_display_name | character | Away team display name. |
| away_location | character | Away team city/location. |
| away_color | character | Away team primary color hex. |
| away_alternate_color | character | Away team alternate color hex. |
| away_logo | character | Away team logo URL. |
| away_score | character | Away team score. |
| away_winner | logical | Whether the away team won. |
| away_rank | integer | Away team rank (if ranked). |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_scoreboard.md)
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
  try(espn_mch_scoreboard(dates = "20250110"))
#> ── MENS-COLLEGE-HOCKEY Scoreboard data from ESPN.com ───────────────────────────
#> ℹ Data updated: 2026-10-13 01:52:24 UTC
#> # A tibble: 29 × 50
#>    game_id   uid      date  name  short_name season_year season_type season_slug
#>    <chr>     <chr>    <chr> <chr> <chr>            <int>       <int> <chr>      
#>  1 401715296 s:70~l:… 2025… Minn… MINN @ OSU        2025           2 regular-se…
#>  2 401711967 s:70~l:… 2025… Alas… UAA @ NIA         2025           2 regular-se…
#>  3 401711856 s:70~l:… 2025… Merc… MERC @ AR…        2025           2 regular-se…
#>  4 401711903 s:70~l:… 2025… RIT … RIT @ CAN         2025           2 regular-se…
#>  5 401712001 s:70~l:… 2025… Robe… RMU @ HC          2025           2 regular-se…
#>  6 401712011 s:70~l:… 2025… Corn… COR @ SHU         2025           2 regular-se…
#>  7 401712030 s:70~l:… 2025… Verm… UVM @ BU          2025           2 regular-se…
#>  8 401712089 s:70~l:… 2025… UCon… CONN @ UNH        2025           2 regular-se…
#>  9 401713283 s:70~l:… 2025… Unio… UNNY @ CL…        2025           2 regular-se…
#> 10 401713340 s:70~l:… 2025… Quin… QUIN @ PR…        2025           2 regular-se…
#> # ℹ 19 more rows
#> # ℹ 42 more variables: status_type_id <chr>, status_type_name <chr>,
#> #   status_type_state <chr>, status_type_completed <lgl>,
#> #   status_type_description <chr>, status_type_detail <chr>,
#> #   status_type_short_detail <chr>, status_clock <dbl>,
#> #   status_display_clock <chr>, status_period <int>, neutral_site <lgl>,
#> #   conference_competition <chr>, attendance <int>, venue_id <chr>, …
# }
```
