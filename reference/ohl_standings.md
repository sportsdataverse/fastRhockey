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
#> ℹ Data updated: 2026-11-24 19:38:30 UTC
#> # A tibble: 20 × 21
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>          
#>  1 BFD       16         7 0         0       1             1              
#>  2 KGN       15         9 0         2       0             1              
#>  3 PBO       14        11 0         4       0             0              
#>  4 OTT       10        10 3         2       1             0              
#>  5 OSH       8         11 4         0       1             1              
#>  6 SBY       14         8 1         1       1             0              
#>  7 NB        10        11 2         1       0             1              
#>  8 NIAG      10        10 3         3       1             0              
#>  9 BRAM      6         15 1         0       0             2              
#> 10 BAR       5         17 3         2       1             0              
#> 11 WSR       15         7 2         1       0             1              
#> 12 SOO       14        10 2         6       1             0              
#> 13 FLNT      14         7 1         1       1             0              
#> 14 SAR       10        13 2         0       0             0              
#> 15 SAG       9         11 3         1       1             1              
#> 16 GUE       19         4 0         3       0             0              
#> 17 KIT       16         6 2         4       0             1              
#> 18 ER        13         8 2         0       1             1              
#> 19 LDN       12         8 1         3       1             2              
#> 20 OS        8         12 4         1       1             0              
#> # ℹ 14 more variables: regulation_wins <dbl>, row <chr>, points <dbl>,
#> #   penalty_minutes <chr>, streak <chr>, goals_for <chr>, goals_against <chr>,
#> #   goals_diff <chr>, percentage <chr>, overall_rank <chr>, games_played <dbl>,
#> #   team_rank <int>, past_10 <chr>, team <chr>
```
