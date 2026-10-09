# **AHL Schedule**

AHL schedule from the HockeyTech feed: one row per game of one season
(modulekit/schedule). A season's playoffs and preseason are separate
HockeyTech season ids; pass `season_id` for them.

## Usage

``` r
ahl_schedule(season = NULL, season_id = NULL)
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
#> ℹ Data updated: 2026-10-09 05:33:39 UTC
#> # A tibble: 1,152 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 1029073 2026-10-02T1… Final       Clevelan… 373          2          Grand Ra…
#>  2 1029071 2026-10-02T1… Final       Bellevil… 413          2          Hamilton…
#>  3 1029075 2026-10-02T1… Final       Laval Ro… 415          0          Syracuse…
#>  4 1029076 2026-10-02T1… Final       Providen… 309          1          Utica Co…
#>  5 1029077 2026-10-02T1… Final       Rocheste… 323          3          Toronto …
#>  6 1029078 2026-10-02T1… Final SO    Texas St… 380          4          Iowa Wild
#>  7 1029072 2026-10-02T1… Final       Calgary … 444          4          Abbotsfo…
#>  8 1029074 2026-10-02T1… Final OT    Coachell… 445          5          Ontario …
#>  9 1029084 2026-10-03T1… Final       Manitoba… 321          3          Chicago …
#> 10 1029082 2026-10-03T1… Final       Laval Ro… 415          6          Syracuse…
#> # ℹ 1,142 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
