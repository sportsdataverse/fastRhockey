# **OHL Teams**

OHL teams for a given season from the HockeyTech feed.

## Usage

``` r
ohl_teams(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

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
[`ohl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ohl_schedule.md),
[`ohl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ohl_season_id.md),
[`ohl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ohl_standings.md),
[`ohl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ohl_team_roster.md)

## Examples

``` r
 try(ohl_teams()) 
#> ── OHL Teams from HockeyTech ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 17:05:04 UTC
#> # A tibble: 20 × 7
#>    team_name       team_id team_code team_nickname team_label division team_logo
#>    <chr>           <chr>   <chr>     <chr>         <chr>      <chr>    <chr>    
#>  1 Barrie Colts    7       BAR       Colts         Barrie     2        https://…
#>  2 Brampton Steel… 18      BRAM      Steelheads    Brampton   2        https://…
#>  3 Brantford Bull… 1       BFD       Bulldogs      Brantford  1        https://…
#>  4 Erie Otters     8       ER        Otters        Erie       4        https://…
#>  5 Flint Firebirds 13      FLNT      Firebirds     Flint      3        https://…
#>  6 Guelph Storm    9       GUE       Storm         Guelph     4        https://…
#>  7 Kingston Front… 2       KGN       Frontenacs    Kingston   1        https://…
#>  8 Kitchener Rang… 10      KIT       Rangers       Kitchener  4        https://…
#>  9 London Knights  14      LDN       Knights       London     4        https://…
#> 10 Niagara IceDogs 20      NIAG      IceDogs       Niagara    2        https://…
#> 11 North Bay Batt… 19      NB        Battalion     North Bay  2        https://…
#> 12 Oshawa Generals 4       OSH       Generals      Oshawa     1        https://…
#> 13 Ottawa 67's     5       OTT       67's          Ottawa     1        https://…
#> 14 Owen Sound Att… 11      OS        Attack        Owen Sound 4        https://…
#> 15 Peterborough P… 6       PBO       Petes         Peterboro… 1        https://…
#> 16 Saginaw Spirit  34      SAG       Spirit        Saginaw    3        https://…
#> 17 Sarnia Sting    15      SAR       Sting         Sarnia     3        https://…
#> 18 Soo Greyhounds  16      SOO       Greyhounds    Sault Ste… 3        https://…
#> 19 Sudbury Wolves  12      SBY       Wolves        Sudbury    2        https://…
#> 20 Windsor Spitfi… 17      WSR       Spitfires     Windsor    3        https://…
```
