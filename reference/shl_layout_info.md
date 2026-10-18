# Directory of every SHL ssgt (season + series + game type) with teams and the stats modules

Wraps `GET https://www.shl.se/api/statistics-v2/layout-info` of the SHL
site API (www.shl.se). Unofficial use of a public source; seasons are
END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_layout_info(return_parsed = TRUE)
```

## Arguments

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season END year (2026 = 2025/26). |
| season_code | character | Provider season code: the START year as a string (2025 = 2025/26). |
| season_name | character | Season label (e.g. 2025/2026). |
| game_type | character | Game type record (uuid, code regular / playoff / qualdown, Swedish name). |
| game_type_name | character | Game type label in Swedish: Grundserie = regular season, Slutspel = playoffs, Kvalspel nedflyttning = relegation qualification, Kvalserien = the old promotion/relegation series. |
| series_code | character | Series code (SHL, CHL; HA and Kvalserien in old career logs). |
| ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams\[\]. |
| state | character | ssgt state: active (the current season) or closed. |
| season_uuid | character | Season uuid (platform key space; code 2025 = 2025/2026); the seasonUuid param of game-schedule. |
| series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
| game_type_uuid | character | Game type uuid (platform key space; regular qQ9-af37Ti40B); the gameTypeUuid param of game-schedule. |

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
  shl_layout_info()
#> ── SHL Layout Info Information from www.shl.se ─────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:11:28 UTC
#> # A tibble: 110 × 11
#>    season season_code season_name game_type game_type_name series_code ssgt_uuid
#>     <int> <chr>       <chr>       <chr>     <chr>          <chr>       <chr>    
#>  1   2025 2024        2024/2025   playoff   Slutspel       SHL         fg2i6uu9…
#>  2   2026 2025        2025/2026   regular   Grundserie     SHL         iuzqg7dq…
#>  3   2026 2025        2025/2026   qualdown  Kvalspel nedf… SHL         jjruyvpq…
#>  4   2025 2024        2024/2025   qualdown  Kvalspel nedf… SHL         kr8hte1o…
#>  5   2027 2026        2026/2027   regular   Grundserie     SHL         qa98unlb…
#>  6   2022 2021        2021/2022   qualdown  Kvalspel nedf… SHL         qbK-6Iso…
#>  7   2022 2021        2021/2022   playoff   Slutspel       SHL         qbK-6ItY…
#>  8   2023 2022        2022/2023   regular   Grundserie     SHL         qbN-6KVf…
#>  9   2023 2022        2022/2023   playoff   Slutspel       SHL         qbT-5I5t…
#> 10   2023 2022        2022/2023   qualdown  Kvalspel nedf… SHL         qbT-5I9a…
#> # ℹ 100 more rows
#> # ℹ 4 more variables: state <chr>, season_uuid <chr>, series_uuid <chr>,
#> #   game_type_uuid <chr>
# }
```
