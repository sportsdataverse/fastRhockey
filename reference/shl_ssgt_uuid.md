# SHL ssgt uuid of a season and game type

The SHL keys a season by an "ssgt" uuid (season + series + game type).
This maps a season (END year) and game type to it through
`/statistics-v2/layout-info`, fetched once per R session. Use it for the
routes that take an ssgt uuid directly
([`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md),
[`shl_sites()`](https://fastRhockey.sportsdataverse.org/reference/shl_sites.md),
[`shl_ssgt_provider()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_provider.md),
[`shl_post_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_post_game_team_stats.md),
[`shl_player_h2h()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_h2h.md)).

## Usage

``` r
shl_ssgt_uuid(season, game_type = "regular")
```

## Arguments

- season:

  Season END year (2026 = 2025/26).

- game_type:

  Game type: `"regular"` (Grundserie), `"playoff"` (Slutspel),
  `"qualdown"` (relegation qualification) or `"playin"` (2023/24 on).

## Value

A character string, the ssgt uuid (`"iuzqg7dqk9"` for the 2025/26
regular season). A length-one vector, not a data frame:

|          |           |                |
|----------|-----------|----------------|
| col_name | type      | description    |
| (value)  | character | The ssgt uuid. |

## Errors

`fastRhockey_no_data`: layout-info lists no ssgt for that season and
game type. `fastRhockey_bad_request`: `season` is not a year.
`fastRhockey_asset_fetch`: layout-info could not be fetched or read.

## See also

Other SHL Functions:
[`shl_athlete_career()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_career.md),
[`shl_athlete_details()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_details.md),
[`shl_athlete_game_log()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_game_log.md),
[`shl_athlete_profile()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_profile.md),
[`shl_athlete_seasons()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_seasons.md),
[`shl_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/shl_boxscore.md),
[`shl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_header.md),
[`shl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_overview.md),
[`shl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_summary.md),
[`shl_last_game_day()`](https://fastRhockey.sportsdataverse.org/reference/shl_last_game_day.md),
[`shl_layout_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_layout_info.md),
[`shl_next_game_day()`](https://fastRhockey.sportsdataverse.org/reference/shl_next_game_day.md),
[`shl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/shl_pbp.md),
[`shl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_box.md),
[`shl_player_crosswalk()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_crosswalk.md),
[`shl_player_h2h()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_h2h.md),
[`shl_post_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_post_game_team_stats.md),
[`shl_roster()`](https://fastRhockey.sportsdataverse.org/reference/shl_roster.md),
[`shl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/shl_schedule.md),
[`shl_season_filter()`](https://fastRhockey.sportsdataverse.org/reference/shl_season_filter.md),
[`shl_sites()`](https://fastRhockey.sportsdataverse.org/reference/shl_sites.md),
[`shl_ssgt_provider()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_provider.md),
[`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md),
[`shl_stats_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_stats_info.md),
[`shl_team()`](https://fastRhockey.sportsdataverse.org/reference/shl_team.md),
[`shl_team_played_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_played_games.md),
[`shl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_stats.md),
[`shl_team_upcoming_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_upcoming_games.md)

## Examples

``` r
# \donttest{
  shl_ssgt_uuid(2026)
#> Error in .shl_directory(): www.shl.se/api/statistics-v2/layout-info: no ssgt directory in the body
  shl_ssgt_uuid(2025, "playoff")
#> Error in .shl_directory(): www.shl.se/api/statistics-v2/layout-info: no ssgt directory in the body
# }
```
