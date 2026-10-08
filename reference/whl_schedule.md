# **WHL Schedule**

WHL schedule from the HockeyTech feed: one row per game of one season
(modulekit/schedule). A season's playoffs and preseason are separate
HockeyTech season ids; pass `season_id` for them.

## Usage

``` r
whl_schedule(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional. Defaults to the newest regular
  season when neither `season` nor `season_id` is given.

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per game.

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
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_schedule()) 
#> ── WHL Schedule from HockeyTech ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 10:28:04 UTC
#> # A tibble: 782 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 1023081 2026-09-18T1… Final       Brandon … 201          5          Saskatoo…
#>  2 1023084 2026-09-18T1… Final       Prince A… 209          7          Regina P…
#>  3 1023082 2026-09-18T1… Final SO    Calgary … 202          3          Red Deer…
#>  4 1023083 2026-09-18T1… Final       Lethbrid… 205          2          Medicine…
#>  5 1023085 2026-09-18T1… Final       Prince G… 210          4          Penticto…
#>  6 1023089 2026-09-19T1… Final OT    Moose Ja… 207          7          Saskatoo…
#>  7 1023092 2026-09-19T1… Final       Swift Cu… 216          5          Prince A…
#>  8 1023091 2026-09-19T1… Final       Red Deer… 211          1          Edmonton…
#>  9 1023088 2026-09-19T1… Final       Medicine… 206          5          Lethbrid…
#> 10 1023087 2026-09-19T1… Final       Kamloops… 203          2          Victoria…
#> # ℹ 772 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
