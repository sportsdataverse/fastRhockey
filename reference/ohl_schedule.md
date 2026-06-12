# **OHL Schedule**

OHL schedule from the HockeyTech feed (one row per game).

## Usage

``` r
ohl_schedule(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional.

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per game.

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
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_schedule()) 
#> ── OHL Schedule from HockeyTech ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:23:03 UTC
#> # A tibble: 10,000 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 6106    1997-09-17T0… Final       Kingston… 2            9          Oshawa G…
#>  2 6107    1997-09-18T0… Final       North Ba… 3            5          Erie Ott…
#>  3 6108    1997-09-18T0… Final       Peterbor… 6            4          Bellevil…
#>  4 6109    1997-09-18T0… Final       Windsor … 17           5          Sault St…
#>  5 6110    1997-09-19T0… Final       Guelph S… 9            4          Toronto …
#>  6 6111    1997-09-19T0… Final       Kingston… 2            9          Peterbor…
#>  7 6112    1997-09-19T0… Final       Kitchene… 10           3          Barrie C…
#>  8 6114    1997-09-19T0… Final       Ottawa 6… 5            2          Sarnia S…
#>  9 6115    1997-09-19T0… Final       Sudbury … 12           2          Erie Ott…
#> 10 6116    1997-09-20T0… Final       Barrie C… 7            3          Sudbury …
#> # ℹ 9,990 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
