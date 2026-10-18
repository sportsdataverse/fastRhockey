# **Get ESPN MCH Rankings (Site v2 polls)**

**Get ESPN MCH Rankings (Site v2 polls)**

## Usage

``` r
espn_mch_rankings(...)
```

## Arguments

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per (poll, team), always with
the same 25 columns; zero rows (with those columns) when no poll is
published:

|  |  |  |
|----|----|----|
| col_name | type | description |
| poll_id | integer | ESPN poll identifier. |
| poll_name | character | Poll name. |
| poll_short_name | character | Poll short name. |
| poll_type | character | Poll type code. |
| season | integer | Season end-year. |
| season_type | integer | Season type (2 = regular season, 3 = postseason). |
| week | integer | Poll week within the season type. |
| week_display | character | Poll week display string. |
| poll_date | character | Poll date (ISO 8601). |
| ranked | logical | `TRUE` for a ranked team, `FALSE` for a team receiving votes. |
| team_id | character | ESPN team identifier. |
| rank | integer | Current rank (`NA` when not ranked). |
| previous_rank | integer | Previous poll rank (`0` = unranked). |
| points | numeric | Poll points. |
| first_place_votes | integer | First-place votes (`NA` when not published). |
| trend | character | Rank trend indicator. |
| record_summary | character | Team record summary. |
| team_uid | character | ESPN team UID. |
| team_location | character | Team location. |
| team_name | character | Team name. |
| team_nickname | character | Team nickname. |
| team_abbreviation | character | Team abbreviation. |
| team_color | character | Team colour hex (`NA` when ESPN ships `"NULL"`). |
| team_logo | character | Team logo URL. |
| last_updated | character | Entry last-updated timestamp. |

## Details

ESPN men's college hockey polls (USCHO), league slug
`mens-college-hockey`. `rank` is `NA` on the vote-receiving rows
(`ranked = FALSE`); `previous_rank` keeps ESPN's `0` for a team unranked
in the previous poll. Mirrors sdv-py `espn_mch_rankings()`.

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
[`espn_mch_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_pbp.md),
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
  try(espn_mch_rankings())
#> ── MENS-COLLEGE-HOCKEY Rankings data from ESPN.com ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:10:37 UTC
#> # A tibble: 41 × 25
#>    poll_id poll_name        poll_short_name  poll_type  season season_type  week
#>      <int> <chr>            <chr>            <chr>       <int>       <int> <int>
#>  1   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  2   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  3   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  4   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  5   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  6   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  7   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  8   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#>  9   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#> 10   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     3
#> # ℹ 31 more rows
#> # ℹ 18 more variables: week_display <chr>, poll_date <chr>, ranked <lgl>,
#> #   team_id <chr>, rank <int>, previous_rank <int>, points <dbl>,
#> #   first_place_votes <int>, trend <chr>, record_summary <chr>, team_uid <chr>,
#> #   team_location <chr>, team_name <chr>, team_nickname <chr>,
#> #   team_abbreviation <chr>, team_color <chr>, team_logo <chr>,
#> #   last_updated <chr>
# }
```
