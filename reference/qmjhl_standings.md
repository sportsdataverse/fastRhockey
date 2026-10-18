# **QMJHL Standings**

QMJHL standings from the HockeyTech feed (one row per team).

## Usage

``` r
qmjhl_standings(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

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
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_standings()) 
#> ── QMJHL Standings from HockeyTech ─────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:51:04 UTC
#> # A tibble: 18 × 20
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses row  
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>           <chr>
#>  1 NFL       8          3 1         2       0             0               8    
#>  2 Rim       7          2 1         0       1             0               6    
#>  3 Que       7          2 0         1       0             0               7    
#>  4 SNB       6          2 1         1       2             1               4    
#>  5 Hal       4          1 3         1       2             1               2    
#>  6 Mon       6          4 0         3       1             0               5    
#>  7 Cha       5          6 2         0       1             0               4    
#>  8 Cap       3          3 1         1       0             3               3    
#>  9 Chi       4          6 0         0       0             0               4    
#> 10 BaC       0          9 0         0       0             1               0    
#> 11 VdO       9          1 1         1       1             0               8    
#> 12 She       7          1 1         1       0             0               7    
#> 13 Gat       6          3 2         1       1             1               5    
#> 14 Rou       6          4 2         2       0             0               6    
#> 15 Vic       6          3 0         1       0             1               6    
#> 16 BLB       4          7 1         2       0             0               4    
#> 17 Dru       3          5 0         0       1             2               2    
#> 18 Sha       3          5 1         0       0             0               3    
#> # ℹ 12 more variables: points <dbl>, penalty_minutes <chr>, streak <chr>,
#> #   goals_for <chr>, goals_against <chr>, goals_diff <chr>, percentage <chr>,
#> #   overall_rank <chr>, games_played <dbl>, team_rank <int>, past_10 <chr>,
#> #   team <chr>
```
