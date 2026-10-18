# Series, seasons (1975/76 on) and game types with their uuids, plus the default ssgt

Wraps
`GET https://www.shl.se/api/sports-v2/season-series-game-types-filter`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_season_filter(series_uuid = NULL, season_uuid = NULL, return_parsed = TRUE)
```

## Arguments

- series_uuid:

  Series uuid (SHL qQ9-bb0bzEWUk, CHL qQ9-9397drVEh). No effect on the
  body in the drop-one probe.

- season_uuid:

  Season uuid; picks the default ssgt returned for that season (omit for
  the current season).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| kind | character | Directory entry kind: series, season or game_type. |
| code | character | Game type code: regular, playoff, qualdown (legacy career logs: qualification). |
| name | character | Game type label in Swedish: Grundserie = regular season, Slutspel = playoffs, Kvalspel nedflyttning = relegation qualification, Kvalserien = the old promotion/relegation series. |
| uuid | character | Game type uuid (platform key space; regular qQ9-af37Ti40B); the gameTypeUuid param of game-schedule. |
| deleted | logical | Soft-delete flag of the record (false in every sample). |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

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
[`shl_sites()`](https://fastRhockey.sportsdataverse.org/reference/shl_sites.md),
[`shl_ssgt_provider()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_provider.md),
[`shl_ssgt_uuid()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_uuid.md),
[`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md),
[`shl_stats_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_stats_info.md),
[`shl_team()`](https://fastRhockey.sportsdataverse.org/reference/shl_team.md),
[`shl_team_played_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_played_games.md),
[`shl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_stats.md),
[`shl_team_upcoming_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_upcoming_games.md)

## Examples

``` r
# \donttest{
  shl_season_filter(series_uuid = "qQ9-bb0bzEWUk", season_uuid = "xs4m9qupsi")
#> ── SHL Season Filter Information from www.shl.se ───── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:11:44 UTC
#> # A tibble: 57 × 5
#>    kind   code  name      uuid           deleted
#>    <chr>  <chr> <chr>     <chr>          <lgl>  
#>  1 series CHL   CHL       qQ9-9397drVEh  FALSE  
#>  2 series SHL   SHL       qQ9-bb0bzEWUk  FALSE  
#>  3 season 2026  2026/2027 ndcf81nlb3     FALSE  
#>  4 season 2025  2025/2026 xs4m9qupsi     FALSE  
#>  5 season 2024  2024/2025 qeb-73bZkIm9A  FALSE  
#>  6 season 2023  2023/2024 qcz-3NvSZ2Cmh  FALSE  
#>  7 season 2022  2022/2023 qbN-XMFfjGVt   FALSE  
#>  8 season 2021  2021/2022 qZl-8qa6OaFXf  FALSE  
#>  9 season 2020  2020/2021 qY7-AdVh5z1XJ  FALSE  
#> 10 season 2019  2019/2020 qWX-334j11U5o1 FALSE  
#> # ℹ 47 more rows
# }
```
