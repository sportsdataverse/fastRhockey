# **WHL Season IDs**

All seasons for the WHL with end-year and game-type labels, from the
HockeyTech modulekit/seasons feed.

## Usage

``` r
whl_season_id()
```

## Value

A `fastRhockey_data` data frame with columns: season_id, season_name,
season_short, career, playoff, start_date, end_date, season_yr,
game_type_label.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/whl_leaders.md),
[`whl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/whl_pbp.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_toi.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_season_id()) 
#> ── WHL Season IDs from HockeyTech ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 02:23:59 UTC
#> # A tibble: 78 × 9
#>    season_id season_name         season_short career playoff start_date end_date
#>        <dbl> <chr>               <chr>        <chr>  <chr>   <chr>      <chr>   
#>  1       293 2026 WHL Playoffs   '26 Playoffs 1      1       2026-03-27 2026-05…
#>  2       292 WHL Prospects Game… WHL Prospec… 0      0       2026-02-17 2026-02…
#>  3       289 2025 - 26 Regular … 25 - 26 Reg  1      0       2025-09-18 2026-03…
#>  4       290 2025 - 26 Pre-Seas… 25 - 26 Pre  0      0       2025-08-31 2025-09…
#>  5       288 2025 WHL Playoffs   '25 Playoffs 1      1       2025-03-28 2025-05…
#>  6       285 2024 - 25 Regular … 24 - 25 Reg  1      0       2024-09-20 2025-03…
#>  7       286 2024 - 25 Pre-Seas… 24 - 25 Pre  0      0       2024-09-03 2024-09…
#>  8       284 2024 WHL Playoffs   '24 Playoffs 1      1       2024-03-28 2024-05…
#>  9       281 2023 - 24 Regular … 23 - 24 Reg  1      0       2023-09-22 2024-03…
#> 10       282 2023 - 24 Pre-Seas… 23 - 24 Pre  0      0       2023-09-04 2023-09…
#> # ℹ 68 more rows
#> # ℹ 2 more variables: season_yr <int>, game_type_label <chr>
```
