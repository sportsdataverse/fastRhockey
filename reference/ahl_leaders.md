# **AHL Statistical Leaders**

AHL statistical leaders for a season from the HockeyTech feed.

## Usage

``` r
ahl_leaders(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player entry.

## See also

Other AHL Functions:
[`ahl`](https://fastRhockey.sportsdataverse.org/reference/ahl.md),
[`ahl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_corsi.md),
[`ahl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_shifts.md),
[`ahl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ahl_game_summary.md),
[`ahl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ahl_pbp.md),
[`ahl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_stats.md),
[`ahl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ahl_player_toi.md),
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`ahl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ahl_teams.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_leaders()) 
#> ── AHL Leaders from HockeyTech ──────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:17:34 UTC
#> # A tibble: 10 × 15
#>     rank player_id jersey_number name      team_id team_name team_code team_logo
#>    <int> <chr>     <chr>         <chr>     <chr>   <chr>     <chr>     <chr>    
#>  1     1 5896      63            Matthew … 323     Rocheste… ROC       https://…
#>  2     2 10978     3             Luke Mit… 415     Laval Ro… LAV       https://…
#>  3     3 6627      65            Dominik … 328     Grand Ra… GR        https://…
#>  4     4 9119      90            Brendan … 335     Toronto … TOR       https://…
#>  5     5 7079      21            Mikey Ey… 309     Providen… PRO       https://…
#>  6     1 7079      21            Mikey Ey… 309     Providen… PRO       https://…
#>  7     2 6627      65            Dominik … 328     Grand Ra… GR        https://…
#>  8     3 7401      39            Cameron … 412     Tucson R… TUC       https://…
#>  9     4 10231     12            Kasper H… 413     Bellevil… BEL       https://…
#> 10     5 7653      18            Ben Jones 444     Calgary … CGY       https://…
#> # ℹ 7 more variables: team_logo_small <chr>, stat_formatted <chr>,
#> #   type_formatted <chr>, photo <chr>, photo_small <chr>, position <chr>,
#> #   division <chr>
```
