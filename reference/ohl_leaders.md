# **OHL Statistical Leaders**

OHL statistical leaders for a season from the HockeyTech feed.

## Usage

``` r
ohl_leaders(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player entry.

## See also

Other OHL Functions:
[`most_recent_ohl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ohl_season.md),
[`ohl`](https://fastRhockey.sportsdataverse.org/reference/ohl.md),
[`ohl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_corsi.md),
[`ohl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_shifts.md),
[`ohl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/ohl_game_summary.md),
[`ohl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ohl_pbp.md),
[`ohl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_stats.md),
[`ohl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/ohl_player_toi.md),
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md),
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_leaders()) 
#> ── OHL Leaders from HockeyTech ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-30 14:45:38 UTC
#> # A tibble: 10 × 15
#>     rank player_id jersey_number name      team_id team_name team_code team_logo
#>    <int> <chr>     <chr>         <chr>     <chr>   <chr>     <chr>     <chr>    
#>  1     1 9142      66            Braidy W… 34      Saginaw … SAG       https://…
#>  2     2 9118      71            Dima Zhi… 34      Saginaw … SAG       https://…
#>  3     3 9773      10            Jason Mu… 1       Brantfor… BFD       https://…
#>  4     4 9769      66            Aiden Ke… 1       Brantfor… BFD       https://…
#>  5     5 9575      89            Ryan Kac… 16      Soo Grey… SOO       https://…
#>  6     1 9575      89            Ryan Kac… 16      Soo Grey… SOO       https://…
#>  7     2 9576      91            Noah Laus 16      Soo Grey… SOO       https://…
#>  8     3 8945      37            Tanner L… 10      Kitchene… KIT       https://…
#>  9     4 9142      66            Braidy W… 34      Saginaw … SAG       https://…
#> 10     5 9118      71            Dima Zhi… 34      Saginaw … SAG       https://…
#> # ℹ 7 more variables: team_logo_small <chr>, stat_formatted <chr>,
#> #   type_formatted <chr>, photo <chr>, photo_small <chr>, position <chr>,
#> #   division <chr>
```
