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
#> ── AHL Leaders from HockeyTech ─────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:51:23 UTC
#> # A tibble: 10 × 15
#>     rank player_id jersey_number name      team_id team_name team_code team_logo
#>    <int> <chr>     <chr>         <chr>     <chr>   <chr>     <chr>     <chr>    
#>  1     1 5927      27            Laurent … 415     Laval Ro… LAV       https://…
#>  2     2 4573      11            Logan Sh… 335     Toronto … TOR       https://…
#>  3     3 10174     20            Andrew C… 319     Hershey … HER       https://…
#>  4     4 10190     43            Tyson Ju… 445     Coachell… CV        https://…
#>  5     5 10127     33            Jani Nym… 445     Coachell… CV        https://…
#>  6     1 10127     33            Jani Nym… 445     Coachell… CV        https://…
#>  7     2 9119      90            Brendan … 335     Toronto … TOR       https://…
#>  8     3 11068     92            Jesse Ki… 328     Grand Ra… GR        https://…
#>  9     4 10874     23            Quinn Hu… 402     Bakersfi… BAK       https://…
#> 10     5 7149      18            Noah Gre… 321     Manitoba… MB        https://…
#> # ℹ 7 more variables: team_logo_small <chr>, stat_formatted <chr>,
#> #   type_formatted <chr>, photo <chr>, photo_small <chr>, position <chr>,
#> #   division <chr>
```
