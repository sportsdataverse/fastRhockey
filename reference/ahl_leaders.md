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
#> ℹ Data updated: 2026-10-18 05:42:43 UTC
#> # A tibble: 10 × 15
#>     rank player_id jersey_number name      team_id team_name team_code team_logo
#>    <int> <chr>     <chr>         <chr>     <chr>   <chr>     <chr>     <chr>    
#>  1     1 10978     3             Luke Mit… 415     Laval Ro… LAV       https://…
#>  2     2 4573      11            Logan Sh… 335     Toronto … TOR       https://…
#>  3     3 5927      27            Laurent … 415     Laval Ro… LAV       https://…
#>  4     4 10190     43            Tyson Ju… 445     Coachell… CV        https://…
#>  5     5 10633     20            Alex Bump 313     Lehigh V… LV        https://…
#>  6     1 10633     20            Alex Bump 313     Lehigh V… LV        https://…
#>  7     2 7653      18            Ben Jones 444     Calgary … CGY       https://…
#>  8     3 9119      90            Brendan … 335     Toronto … TOR       https://…
#>  9     4 10127     33            Jani Nym… 445     Coachell… CV        https://…
#> 10     5 6939      18            Steven L… 373     Clevelan… CLE       https://…
#> # ℹ 7 more variables: team_logo_small <chr>, stat_formatted <chr>,
#> #   type_formatted <chr>, photo <chr>, photo_small <chr>, position <chr>,
#> #   division <chr>
```
