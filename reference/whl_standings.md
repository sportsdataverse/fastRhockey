# **WHL Standings**

WHL standings from the HockeyTech feed (one row per team).

## Usage

``` r
whl_standings(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

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
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_standings()) 
#> ── WHL Standings from HockeyTech ───────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:13:05 UTC
#> # A tibble: 23 × 21
#>    team_code wins  losses ties  ot_losses ot_wins shootout_wins shootout_losses
#>    <chr>     <chr>  <dbl> <chr> <chr>     <chr>   <chr>         <chr>          
#>  1 BDN       7          3 0     0         1       2             0              
#>  2 SC        7          4 0     0         0       2             0              
#>  3 SAS       5          3 0     1         0       0             0              
#>  4 REG       4          4 0     0         0       0             1              
#>  5 MJ        4          4 0     0         1       2             0              
#>  6 PA        2          6 0     0         0       0             2              
#>  7 RD        7          3 0     0         0       0             1              
#>  8 MH        5          3 0     0         0       0             1              
#>  9 CGY       5          4 0     0         1       1             0              
#> 10 LET       5          6 0     0         0       0             0              
#> # ℹ 13 more rows
#> # ℹ 13 more variables: regulation_wins <dbl>, row <chr>, points <dbl>,
#> #   penalty_minutes <chr>, streak <chr>, goals_for <chr>, goals_against <chr>,
#> #   goals_diff <chr>, percentage <chr>, games_played <dbl>, team_rank <int>,
#> #   past_10 <chr>, team <chr>
```
