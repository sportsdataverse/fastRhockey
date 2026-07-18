# **OHL Play-by-Play**

OHL play-by-play for a single game (one row per event, fully enriched
with coordinate transforms, on-ice players, Corsi geometry).

## Usage

``` r
ohl_pbp(game_id)
```

## Arguments

- game_id:

  Numeric OHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per play-by-play event.

## See also

Other OHL Functions:
[`most_recent_ohl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ohl_season.md),
[`ohl`](https://fastRhockey.sportsdataverse.org/reference/ohl.md),
[`ohl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_corsi.md),
[`ohl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_shifts.md),
[`ohl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_summary.md),
[`ohl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ohl_leaders.md),
[`ohl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_stats.md),
[`ohl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_toi.md),
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_pbp(game_id = 27225)) 
#> ── OHL Play-by-Play from HockeyTech ─────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 17:04:50 UTC
#> # A tibble: 149 × 104
#>    game_id event team_id period_of_game time_of_period x_coord y_coord player_id
#>      <dbl> <chr> <chr>   <chr>          <chr>            <dbl>   <dbl>     <int>
#>  1   27225 goal… 11      1              0:00              NA     NA           NA
#>  2   27225 goal… 34      1              0:00              NA     NA           NA
#>  3   27225 face… NA      1              0:00               0      0         8384
#>  4   27225 face… NA      1              0:10             -66.7   28.6       8384
#>  5   27225 shot  34      1              1:05              67.7    5.38      8186
#>  6   27225 face… NA      1              1:05               0      0         8179
#>  7   27225 face… NA      1              1:24             -52.3  -28.6       8179
#>  8   27225 shot  34      1              1:54              73    -12.8       8772
#>  9   27225 shot  11      1              2:51             -55.7    2.27      8600
#> 10   27225 face… NA      1              2:51             -52.3  -28.6       9027
#> # ℹ 139 more rows
#> # ℹ 96 more variables: player_name_first <chr>, player_name_last <chr>,
#> #   player_position <chr>, goal <lgl>, goalie_id <int>, goalie_first <chr>,
#> #   goalie_last <chr>, home_win <chr>, player_team_id <chr>, event_type <chr>,
#> #   shot_quality <chr>, player_two_id <int>, player_two_name_first <chr>,
#> #   player_two_name_last <chr>, player_two_position <chr>,
#> #   penalty_length <chr>, power_play <chr>, empty_net <chr>, …
```
