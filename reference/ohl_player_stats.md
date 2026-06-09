# **OHL Player Stats**

OHL player season stats across all seasons.

## Usage

``` r
ohl_player_stats(player_id)
```

## Arguments

- player_id:

  Numeric or character OHL player identifier.

## Value

A `fastRhockey_data` data frame, one row per season-stat entry.

## See also

Other OHL Functions:
[`most_recent_ohl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ohl_season.md),
[`ohl`](https://fastRhockey.sportsdataverse.org/reference/ohl.md),
[`ohl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_corsi.md),
[`ohl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_shifts.md),
[`ohl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_summary.md),
[`ohl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ohl_leaders.md),
[`ohl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ohl_pbp.md),
[`ohl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_toi.md),
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_player_stats(player_id = 1)) 
#> ── OHL Player Stats from HockeyTech ─────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:29:54 UTC
#> # A tibble: 8 × 52
#>   season_id season_name             shortname playoff career sopt_track_faceoffs
#>   <chr>     <chr>                   <chr>     <chr>   <chr>  <chr>              
#> 1 9         1999-2000 Regular Seas… 99-00 Reg 0       1      0                  
#> 2 6         1998-99 Regular Season  98-99 Reg 0       1      0                  
#> 3 4         1997-98 Regular Season  97-98 Re… 0       1      0                  
#> 4 19        Total                   Total     0       3      NA                 
#> 5 10        2000 Playoffs           2000 Pla… 1       1      0                  
#> 6 7         1999 Playoffs           99  Play… 1       1      0                  
#> 7 5         1998 Playoffs           97-98 Pl… 1       1      0                  
#> 8 22        Total                   Total     1       3      NA                 
#> # ℹ 46 more variables: max_start_date <chr>, veteran_status <chr>,
#> #   veteran <chr>, jersey_number <chr>, goals <chr>, games_played <chr>,
#> #   assists <chr>, points <chr>, plus_minus <chr>, penalty_minutes <chr>,
#> #   power_play_goals <chr>, power_play_assists <chr>, shots <chr>,
#> #   shootout_attempts <chr>, shootout_goals <chr>, shootout_percentage <chr>,
#> #   shooting_percentage <chr>, shootout_winning_goals <chr>,
#> #   points_per_game <chr>, short_handed_goals <chr>, …
```
