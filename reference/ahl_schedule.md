# **AHL Schedule**

AHL schedule from the HockeyTech feed (one row per game).

## Usage

``` r
ahl_schedule(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional.

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per game.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ahl_leaders.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_stats.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_schedule()) 
#> ── AHL Schedule from HockeyTech ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:45:00 UTC
#> # A tibble: 10,000 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 1005950 1995-01-17T2… Final       U.S. AHL… 377          4          Canadian…
#>  2 1005949 1996-01-16T1… Final       U.S. AHL… 377          6          Canadian…
#>  3 1005948 1997-01-16T2… Final SO    Canadian… 367          2          World AH…
#>  4 1005947 1998-02-11T1… Final       PlanetUS… 368          10         Canadian…
#>  5 1005946 1999-01-25T2… Final SO    PlanetUS… 368          5          Canadian…
#>  6 1005942 2000-01-17T1… Final       PlanetUS… 368          3          Canadian…
#>  7 1005940 2001-01-15T1… Final       PlanetUS… 368          10         Canadian…
#>  8 1005939 2002-02-14T2… Final       Canadian… 367          13         PlanetUS…
#>  9 1005938 2003-02-03T1… Final       PlanetUS… 368          7          Canadian…
#> 10 1005937 2004-02-09T1… Final       PlanetUS… 368          5          Canadian…
#> # ℹ 9,990 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
