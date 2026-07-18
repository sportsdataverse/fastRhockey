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
#> ── OHL Standings from HockeyTech ────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:48:52 UTC
#> # A tibble: 20 × 21
#>    team_code wins  losses ot_losses ot_wins shootout_wins shootout_losses
#>    <chr>     <chr>  <dbl> <chr>     <chr>   <chr>         <chr>          
#>  1 BFD       0          0 0         ""      0             0              
#>  2 PBO       0          0 0         ""      0             0              
#>  3 OTT       0          0 0         ""      0             0              
#>  4 OSH       0          0 0         ""      0             0              
#>  5 KGN       0          0 0         ""      0             0              
#>  6 NIAG      0          0 0         ""      0             0              
#>  7 NB        0          0 0         ""      0             0              
#>  8 BRAM      0          0 0         ""      0             0              
#>  9 SBY       0          0 0         ""      0             0              
#> 10 BAR       0          0 0         ""      0             0              
#> 11 SAR       0          0 0         ""      0             0              
#> 12 SAG       0          0 0         ""      0             0              
#> 13 WSR       0          0 0         ""      0             0              
#> 14 SOO       0          0 0         ""      0             0              
#> 15 FLNT      0          0 0         ""      0             0              
#> 16 LDN       0          0 0         ""      0             0              
#> 17 OS        0          0 0         ""      0             0              
#> 18 KIT       0          0 0         ""      0             0              
#> 19 GUE       0          0 0         ""      0             0              
#> 20 ER        0          0 0         ""      0             0              
#> # ℹ 14 more variables: regulation_wins <dbl>, row <chr>, points <dbl>,
#> #   penalty_minutes <chr>, streak <chr>, goals_for <chr>, goals_against <chr>,
#> #   goals_diff <chr>, percentage <chr>, overall_rank <chr>, games_played <dbl>,
#> #   team_rank <int>, past_10 <chr>, team <chr>
```
