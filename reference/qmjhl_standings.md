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
#> ℹ Data updated: 2026-06-24 02:07:15 UTC
#> # A tibble: 18 × 20
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses row  
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>           <chr>
#>  1 Rim       0          0 0         0       0             0               0    
#>  2 Mon       0          0 0         0       0             0               0    
#>  3 BaC       0          0 0         0       0             0               0    
#>  4 Chi       0          0 0         0       0             0               0    
#>  5 Que       0          0 0         0       0             0               0    
#>  6 SNB       0          0 0         0       0             0               0    
#>  7 Cha       0          0 0         0       0             0               0    
#>  8 Hal       0          0 0         0       0             0               0    
#>  9 Cap       0          0 0         0       0             0               0    
#> 10 NFL       0          0 0         0       0             0               0    
#> 11 She       0          0 0         0       0             0               0    
#> 12 BLB       0          0 0         0       0             0               0    
#> 13 Vic       0          0 0         0       0             0               0    
#> 14 VdO       0          0 0         0       0             0               0    
#> 15 Dru       0          0 0         0       0             0               0    
#> 16 Sha       0          0 0         0       0             0               0    
#> 17 Gat       0          0 0         0       0             0               0    
#> 18 Rou       0          0 0         0       0             0               0    
#> # ℹ 12 more variables: points <dbl>, penalty_minutes <chr>, streak <chr>,
#> #   goals_for <chr>, goals_against <chr>, goals_diff <chr>, percentage <chr>,
#> #   overall_rank <chr>, games_played <dbl>, team_rank <int>, past_10 <chr>,
#> #   team <chr>
```
