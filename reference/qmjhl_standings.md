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
#> ── QMJHL Standings from HockeyTech ──────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:23:33 UTC
#> # A tibble: 18 × 20
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses row  
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>           <chr>
#>  1 Que       12         4 1         2       0             0               12   
#>  2 NFL       11         5 1         2       1             2               10   
#>  3 SNB       11         5 1         1       2             1               9    
#>  4 Rim       9          6 3         1       1             0               8    
#>  5 Hal       8          5 3         2       2             1               6    
#>  6 Cap       7          5 2         1       0             3               7    
#>  7 Mon       9          8 0         3       2             1               7    
#>  8 Chi       9          8 0         0       0             0               9    
#>  9 Cha       7         10 2         1       1             0               6    
#> 10 BaC       3         13 0         1       0             1               3    
#> 11 VdO       15         2 1         2       1             0               14   
#> 12 Vic       13         4 0         1       1             1               12   
#> 13 Rou       10         5 3         2       0             0               10   
#> 14 Gat       9          7 3         2       1             1               8    
#> 15 She       9          5 2         1       0             0               9    
#> 16 Dru       8          8 0         1       1             2               7    
#> 17 BLB       7         11 1         3       0             0               7    
#> 18 Sha       4          9 3         0       0             0               4    
#> # ℹ 12 more variables: points <dbl>, penalty_minutes <chr>, streak <chr>,
#> #   goals_for <chr>, goals_against <chr>, goals_diff <chr>, percentage <chr>,
#> #   overall_rank <chr>, games_played <dbl>, team_rank <int>, past_10 <chr>,
#> #   team <chr>
```
