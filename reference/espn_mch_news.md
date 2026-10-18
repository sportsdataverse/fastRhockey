# **Get ESPN MCH News**

**Get ESPN MCH News**

## Usage

``` r
espn_mch_news(limit = 50, ...)
```

## Arguments

- limit:

  Maximum number of articles to return (default `50`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per article:

|               |           |                                         |
|---------------|-----------|-----------------------------------------|
| col_name      | type      | description                             |
| article_id    | character | ESPN article identifier.                |
| now_id        | character | ESPN Now identifier.                    |
| content_key   | character | Content management key.                 |
| type          | character | Article type (e.g. "Story", "Media").   |
| headline      | character | Article headline.                       |
| description   | character | Article description/excerpt.            |
| last_modified | character | ISO 8601 last-modified datetime.        |
| published     | character | ISO 8601 published datetime.            |
| byline        | character | Author byline.                          |
| premium       | logical   | Whether the article is premium content. |
| image_href    | character | URL of the lead image.                  |
| web_href      | character | URL of the article on ESPN.com.         |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_news.md);
columns verified against a captured MCH body.

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
  try(espn_mch_news(limit = 10))
#> ── MENS-COLLEGE-HOCKEY News data from ESPN.com ─────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:02:53 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50206118   1-50206118 50206118-1-29… Media Arizona… Arizona St… 2026-10-18T0…
#>  2 50205749   1-50205749 50205749-1-29… Media Northea… Northeaste… 2026-10-18T0…
#>  3 50199200   1-50199200 50199200-1-29… Media Minneso… Minnesota … 2026-10-17T0…
#>  4 50199090   1-50199090 50199090-1-29… Media Colorad… Colorado C… 2026-10-17T0…
#>  5 50199061   1-50199061 50199061-1-29… Media Boston … Boston U. … 2026-10-17T0…
#>  6 50196596   1-50196596 50196596-1-5-1 Head… NCAA su… The NCAA h… 2026-10-16T2…
#>  7 50193166   1-50193166 50193166-1-29… Media St. Clo… St. Cloud … 2026-10-16T0…
#>  8 50153041   1-50153041 50153041-1-29… Media Brown B… Brown Bear… 2026-10-11T0…
#>  9 50153001   1-50153001 50153001-1-29… Media Quinnip… Quinnipiac… 2026-10-12T1…
#> 10 50152575   1-50152575 50152575-1-29… Media Bowling… Bowling Gr… 2026-10-11T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
