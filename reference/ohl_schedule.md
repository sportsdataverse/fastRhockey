# **OHL Schedule**

OHL schedule from the HockeyTech feed: one row per game of one season
(modulekit/schedule). A season's playoffs and preseason are separate
HockeyTech season ids; pass `season_id` for them.

## Usage

``` r
ohl_schedule(season = NULL, season_id = NULL)
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
#> ℹ Data updated: 2026-10-08 10:27:35 UTC
#> # A tibble: 684 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 28992   2026-09-17T1… Final       Peterbor… 6            3          Kingston…
#>  2 28993   2026-09-18T1… Final       Brampton… 18           2          Oshawa G…
#>  3 28998   2026-09-18T1… Final       North Ba… 19           2          Barrie C…
#>  4 28994   2026-09-18T1… Final       Brantfor… 1            3          Niagara …
#>  5 28996   2026-09-18T1… Final       Kitchene… 10           0          Owen Sou…
#>  6 28997   2026-09-18T1… Final OT    London K… 14           3          Windsor …
#>  7 29000   2026-09-18T1… Final       Sudbury … 12           2          Peterbor…
#>  8 28995   2026-09-18T1… Final       Guelph S… 9            1          Erie Ott…
#>  9 28999   2026-09-18T1… Final OT    Soo Grey… 16           5          Saginaw …
#> 10 29002   2026-09-19T1… Final       Brantfor… 1            6          Guelph S…
#> # ℹ 674 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
