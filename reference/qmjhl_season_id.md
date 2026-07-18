# **QMJHL Season IDs**

All seasons for the QMJHL with end-year and game-type labels, from the
HockeyTech modulekit/seasons feed.

## Usage

``` r
qmjhl_season_id()
```

## Value

A `fastRhockey_data` data frame with columns: season_id, season_name,
season_short, career, playoff, start_date, end_date, season_yr,
game_type_label.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_season_id()) 
#> ── QMJHL Season IDs from HockeyTech ─────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:49:20 UTC
#> # A tibble: 131 × 9
#>    season_id season_name         season_short career playoff start_date end_date
#>        <dbl> <chr>               <chr>        <chr>  <chr>   <chr>      <chr>   
#>  1       214 2026-27 | Regular … 2026-27      1      0       2026-09-15 2027-03…
#>  2       213 Pre-Season 2026     Pre-Sea 2026 0      0       2026-05-22 2026-09…
#>  3       212 2026 | Playoffs     2026 Plfs    1      1       2026-03-24 2026-05…
#>  4       211 2025-26 | Regular … 2025-26      1      0       2025-09-16 2026-03…
#>  5       210 Pre-Season 2025     Pre-Sea 2025 0      0       2025-05-23 2025-09…
#>  6       209 2025 | Playoffs     2025 Plfs    1      1       2025-03-25 2025-05…
#>  7       208 2024-25 | Regular … 2024-25      1      0       2024-09-17 2025-03…
#>  8       207 Pre-Season 2024     Pre-Sea 2024 0      0       2024-05-24 2024-09…
#>  9       206 2024 | Playoffs     2024 Plfs    1      1       2024-03-26 2024-05…
#> 10       205 2023-24 | Regular … 2023-24      1      0       2023-09-19 2024-03…
#> # ℹ 121 more rows
#> # ℹ 2 more variables: season_yr <int>, game_type_label <chr>
```
