# **OHL Season IDs**

All seasons for the OHL with end-year and game-type labels, from the
HockeyTech modulekit/seasons feed.

## Usage

``` r
ohl_season_id()
```

## Value

A `fastRhockey_data` data frame with columns: season_id, season_name,
season_short, career, playoff, start_date, end_date, season_yr,
game_type_label.

## See also

Other OHL Functions:
[`most_recent_ohl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ohl_season.md),
[`ohl`](https://fastRhockey.sportsdataverse.org/reference/ohl.md),
[`ohl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_corsi.md),
[`ohl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_shifts.md),
[`ohl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_summary.md),
[`ohl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ohl_leaders.md),
[`ohl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ohl_pbp.md),
[`ohl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_stats.md),
[`ohl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_toi.md),
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_season_id()) 
#> ── OHL Season IDs from HockeyTech ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-10 00:47:45 UTC
#> # A tibble: 73 × 9
#>    season_id season_name         season_short career playoff start_date end_date
#>        <dbl> <chr>               <chr>        <chr>  <chr>   <chr>      <chr>   
#>  1        88 2026-27 Regular Se… 26-27 Reg. … 1      0       2026-09-16 2027-03…
#>  2        87 2026 Pre-season     2026 Presea… 0      0       2026-08-11 2026-09…
#>  3        85 2026 Playoffs       26 Playoffs  1      1       2026-03-24 2026-05…
#>  4        84 2025-26 OHL Top Pr… OHL TPG      0      0       2026-01-10 2026-01…
#>  5        83 2025-26 Regular Se… 25-26 Reg. … 1      0       2025-09-16 2026-03…
#>  6        82 2025 Pre-Season     2025 Presea… 0      0       2025-08-13 2025-09…
#>  7        81 2025 Playoffs       25 Playoffs  1      1       2025-03-26 2025-05…
#>  8        79 2024-25 Regular Se… 24-25 Reg. … 1      0       2024-09-23 2025-03…
#>  9        78 2024 Pre-season     2024 Presea… 0      0       2024-08-15 2024-09…
#> 10        77 2024 Playoffs       24 Playoffs  1      1       2024-03-26 2024-05…
#> # ℹ 63 more rows
#> # ℹ 2 more variables: season_yr <int>, game_type_label <chr>
```
