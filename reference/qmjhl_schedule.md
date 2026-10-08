# **QMJHL Schedule**

QMJHL schedule from the HockeyTech feed: one row per game of one season
(modulekit/schedule). A season's playoffs and preseason are separate
HockeyTech season ids; pass `season_id` for them.

## Usage

``` r
qmjhl_schedule(season = NULL, season_id = NULL)
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
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_schedule()) 
#> ── QMJHL Schedule from HockeyTech ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 11:33:12 UTC
#> # A tibble: 580 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 32940   2026-09-18T1… Final       Newfound… 2            2          Saint Jo…
#>  2 32715   2026-09-18T1… Final SO    Cape Bre… 3            4          Halifax,…
#>  3 32747   2026-09-18T1… Final       Charlott… 7            3          Moncton,…
#>  4 32779   2026-09-18T1… Final       Chicouti… 10           2          Shawinig…
#>  5 32651   2026-09-18T1… Final       Baie-Com… 16           2          Rimouski…
#>  6 33199   2026-09-18T1… Final       Victoria… 17           3          Val-d'Or…
#>  7 32683   2026-09-18T1… Final       Blainvil… 19           2          Rouyn-No…
#>  8 33134   2026-09-18T1… Final       Sherbroo… 60           4          Québec, …
#>  9 32652   2026-09-19T1… Final       Baie-Com… 16           2          Rimouski…
#> 10 32843   2026-09-19T1… Final       Gatineau… 12           4          Rouyn-No…
#> # ℹ 570 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
