# **Get ESPN WCH Player Box Score**

**Get ESPN WCH Player Box Score**

## Usage

``` r
espn_wch_player_box(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble, one row per player appearance (skaters and
goalies bound; columns not kept for a position group are `NA`):

|  |  |  |
|----|----|----|
| col_name | type | description |
| athlete_id | character | ESPN athlete identifier. |
| athlete_display_name | character | Athlete full display name. |
| athlete_short_name | character | Athlete short name. |
| athlete_last_name | character | Athlete last name. |
| athlete_jersey | character | Jersey number. |
| athlete_headshot_href | character | Headshot image URL. |
| athlete_position_name | character | Position name. |
| athlete_position_abbreviation | character | Position abbreviation. |
| athlete_active | logical | Whether the athlete was active. |
| athlete_scratched | logical | Whether the athlete was scratched. |
| stat_group | character | Stat group name ("forwards","defenses","goalies"). |
| team_id | character | ESPN team identifier. |
| team_abbreviation | character | Team abbreviation. |
| team_display_name | character | Team display name. |
| home_away | character | "home" or "away". |
| team_score | character | Team final score. |
| team_winner | logical | Whether the team won. |
| opponent_team_id | character | Opponent team identifier. |
| opponent_team_abbreviation | character | Opponent team abbreviation. |
| game_id | character | ESPN event identifier (echoed from arg). |
| season | integer | Season end-year. |
| season_type | integer | Season type code. |
| game_date | character | Game date-time (ISO 8601). |
| bs | character | Blocked shots (skaters). |
| ht | character | Hits (skaters). |
| tk | character | Takeaways (skaters). |
| plus_minus | character | Plus/minus rating (skaters). |
| toi | character | Time on ice (skaters, MM:SS). |
| pptoi | character | Power play time on ice (skaters, MM:SS). |
| shtoi | character | Short-handed time on ice (skaters, MM:SS). |
| estoi | character | Even-strength time on ice (skaters, MM:SS). |
| shft | character | Total shifts (skaters). |
| g | character | Goals (skaters). |
| ytdg | character | Year-to-date goals (skaters/goalies). |
| a | character | Assists (skaters). |
| s | character | Shots (skaters). |
| sm | character | Missed shots (skaters). |
| sog | character | Shots on goal (skaters). |
| fw | character | Faceoffs won (skaters). |
| fl | character | Faceoffs lost (skaters). |
| fo_percent | character | Faceoff percentage (skaters). |
| gv | character | Giveaways (skaters). |
| pn | character | Penalties taken (skaters). |
| pim | character | Penalty minutes (skaters/goalies). |
| ga | character | Goals against (goalies). |
| sa | character | Shots against (goalies). |
| sos | character | Shootout saves (goalies). |
| sosa | character | Shootout shots against (goalies). |
| sv | character | Saves (goalies). |
| sv_percent | character | Save percentage (goalies). |
| essv | character | Even-strength saves (goalies). |
| ppsv | character | Power play saves (goalies). |
| shsv | character | Short-handed saves (goalies). |

## Details

League slug `womens-college-hockey`. Parsed from the site-v2 `summary`
payload by the same helper as
[`espn_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_box.md);
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
  try(espn_wch_player_box(game_id = "401762970"))
#> ── WOMENS-COLLEGE-HOCKEY Player Box Score data from ESPN.com ───────────────────
#> ℹ Data updated: 2026-10-18 12:04:59 UTC
#> # A tibble: 40 × 53
#>    athlete_id athlete_display_name athlete_short_name athlete_last_name
#>    <chr>      <chr>                <chr>              <chr>            
#>  1 5263060    Greta Brezenski      G. Brezenski       Brezenski        
#>  2 5263072    Sena Catterall       S. Catterall       Catterall        
#>  3 5263074    Madison Chantler     M. Chantler        Chantler         
#>  4 5263071    Anne Cherkowski      A. Cherkowski      Cherkowski       
#>  5 5263057    Caroline Goffredo    C. Goffredo        Goffredo         
#>  6 5263058    Jenna Goodwin        J. Goodwin         Goodwin          
#>  7 5263055    Rhea Hicks           R. Hicks           Hicks            
#>  8 5263056    Baylee Kirwan        B. Kirwan          Kirwan           
#>  9 5263075    Shelby Laidlaw       S. Laidlaw         Laidlaw          
#> 10 5263062    Kirstyn McQuigge     K. McQuigge        McQuigge         
#> # ℹ 30 more rows
#> # ℹ 49 more variables: athlete_jersey <chr>, athlete_headshot_href <chr>,
#> #   athlete_position_name <chr>, athlete_position_abbreviation <chr>,
#> #   athlete_active <lgl>, athlete_scratched <lgl>, stat_group <chr>,
#> #   team_id <chr>, team_abbreviation <chr>, team_display_name <chr>,
#> #   home_away <chr>, team_score <chr>, team_winner <lgl>,
#> #   opponent_team_id <chr>, opponent_team_abbreviation <chr>, game_id <chr>, …
# }
```
