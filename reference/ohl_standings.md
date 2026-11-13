# **OHL Standings**

OHL standings from the HockeyTech feed (one row per team).

## Usage

``` r
ohl_standings(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

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
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_standings()) 
#> ── OHL Standings from HockeyTech ────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:33:55 UTC
#> # A tibble: 20 × 21
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>          
#>  1 BFD       13         5 0         0       1             1              
#>  2 KGN       13         7 0         2       0             0              
#>  3 OTT       10         6 2         2       1             0              
#>  4 PBO       10        10 0         4       0             0              
#>  5 OSH       6          8 4         0       0             1              
#>  6 SBY       13         5 1         1       1             0              
#>  7 NB        8         11 1         1       0             0              
#>  8 NIAG      7          8 3         3       0             0              
#>  9 BAR       4         13 3         2       0             0              
#> 10 BRAM      4         13 1         0       0             1              
#> 11 WSR       12         5 2         0       0             1              
#> 12 SOO       12         7 2         5       0             0              
#> 13 FLNT      11         5 1         1       1             0              
#> 14 SAG       8          9 2         1       1             1              
#> 15 SAR       8         12 1         0       0             0              
#> 16 GUE       16         3 0         3       0             0              
#> 17 KIT       11         6 2         3       0             1              
#> 18 LDN       11         5 1         3       1             1              
#> 19 ER        10         8 2         0       1             0              
#> 20 OS        6         10 4         1       0             0              
#> # ℹ 14 more variables: regulation_wins <dbl>, row <chr>, points <dbl>,
#> #   penalty_minutes <chr>, streak <chr>, goals_for <chr>, goals_against <chr>,
#> #   goals_diff <chr>, percentage <chr>, overall_rank <chr>, games_played <dbl>,
#> #   team_rank <int>, past_10 <chr>, team <chr>
```
