# **AHL Season IDs**

All seasons for the AHL with end-year and game-type labels, from the
HockeyTech modulekit/seasons feed.

## Usage

``` r
ahl_season_id()
```

## Value

A `fastRhockey_data` data frame with columns: season_id, season_name,
season_short, career, playoff, start_date, end_date, season_yr,
game_type_label.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_stats.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_season_id()) 
#> ── AHL Season IDs from HockeyTech ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-12 18:38:58 UTC
#> # A tibble: 76 × 9
#>    season_id season_name         season_short career playoff start_date end_date
#>        <dbl> <chr>               <chr>        <chr>  <chr>   <chr>      <chr>   
#>  1        94 2026-27 Regular Se… 2026-27      1      0       2026-10-02 2027-04…
#>  2        92 2026 Calder Cup Pl… 26 Playoffs  1      1       2026-04-20 2026-06…
#>  3        91 2026 All-Star Chal… 26 All-Star  0      0       2026-02-10 2026-02…
#>  4        90 2025-26 Regular Se… 2025-26      1      0       2025-10-07 2026-04…
#>  5        88 2025 Calder Cup Pl… 25 Playoffs  1      1       2025-04-21 2025-06…
#>  6        87 2025 All-Star Chal… 25 All-Star  0      0       2025-02-03 2025-02…
#>  7        86 2024-25 Regular Se… 2024-25      1      0       2024-10-09 2025-04…
#>  8        84 2024 Calder Cup Pl… 24 Playoffs  1      1       2024-04-22 2024-06…
#>  9        83 2024 All-Star Chal… 24 All-Star  0      0       2024-02-05 2024-02…
#> 10        81 2023-24 Regular Se… 2023-24      1      0       2023-10-12 2024-04…
#> # ℹ 66 more rows
#> # ℹ 2 more variables: season_yr <int>, game_type_label <chr>
```
