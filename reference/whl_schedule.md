# **WHL Schedule**

WHL schedule from the HockeyTech feed (one row per game).

## Usage

``` r
whl_schedule(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional.

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
#> ℹ Data updated: 2026-06-13 02:50:58 UTC
#> # A tibble: 10,000 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 21510   ""            Final       Calgary … 202          1          Saskatoo…
#>  2 23060   ""            Final       Calgary … 202          3          Kootenay…
#>  3 20021   "1996-03-20T… Final       Prince A… 209          8          Medicine…
#>  4 20025   "1996-03-20T… Final       Regina P… 212          5          Lethbrid…
#>  5 20013   "1996-03-21T… Final       Brandon … 201          3          Saskatoo…
#>  6 20019   "1996-03-21T… Final OT    Swift Cu… 216          3          Red Deer…
#>  7 20022   "1996-03-21T… Final       Prince A… 209          6          Medicine…
#>  8 20026   "1996-03-21T… Final 2nd … Regina P… 212          3          Lethbrid…
#>  9 20014   "1996-03-22T… Final       Brandon … 201          4          Saskatoo…
#> 10 20020   "1996-03-22T… Final       Swift Cu… 216          5          Red Deer…
#> # ℹ 9,990 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
