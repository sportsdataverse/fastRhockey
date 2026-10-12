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
#> ── QMJHL Standings from HockeyTech ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-12 12:09:38 UTC
#> # A tibble: 18 × 20
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses row  
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>           <chr>
#>  1 Que       7          1 0         1       0             0               7    
#>  2 Rim       6          2 1         0       0             0               6    
#>  3 NFL       6          2 1         2       0             0               6    
#>  4 SNB       5          1 1         1       1             1               4    
#>  5 Cha       5          3 2         0       1             0               4    
#>  6 Hal       4          1 3         1       2             0               2    
#>  7 Mon       5          3 0         3       1             0               4    
#>  8 Cap       2          3 0         1       0             3               2    
#>  9 Chi       3          5 0         0       0             0               3    
#> 10 BaC       0          7 0         0       0             1               0    
#> 11 VdO       6          1 1         1       0             0               6    
#> 12 Vic       6          2 0         1       0             0               6    
#> 13 Gat       5          2 2         1       1             0               4    
#> 14 She       5          1 1         0       0             0               5    
#> 15 Rou       4          3 2         1       0             0               4    
#> 16 Dru       2          5 0         0       0             1               2    
#> 17 BLB       2          7 0         2       0             0               2    
#> 18 Sha       1          4 1         0       0             0               1    
#> # ℹ 12 more variables: points <dbl>, penalty_minutes <chr>, streak <chr>,
#> #   goals_for <chr>, goals_against <chr>, goals_diff <chr>, percentage <chr>,
#> #   overall_rank <chr>, games_played <dbl>, team_rank <int>, past_10 <chr>,
#> #   team <chr>
```
