# **WHL Teams**

WHL teams for a given season from the HockeyTech feed.

## Usage

``` r
whl_teams(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/whl_leaders.md),
[`whl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/whl_pbp.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_toi.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md)

## Examples

``` r
 try(whl_teams()) 
#> ── WHL Teams from HockeyTech ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:48:10 UTC
#> # A tibble: 17 × 7
#>    team_name       team_id team_code team_nickname team_label division team_logo
#>    <chr>           <chr>   <chr>     <chr>         <chr>      <chr>    <chr>    
#>  1 Brandon Wheat … 201     BDN       Wheat Kings   Brandon    1        https://…
#>  2 Calgary Hitmen  202     CGY       Hitmen        Calgary    3        https://…
#>  3 Edmonton Oil K… 228     EDM       Oil Kings     Edmonton   3        https://…
#>  4 Everett Silver… 226     EVT       Silvertips    Everett    6        https://…
#>  5 Kamloops Blaze… 203     KAM       Blazers       Kamloops   2        https://…
#>  6 Kelowna Rockets 204     KEL       Rockets       Kelowna    2        https://…
#>  7 Medicine Hat T… 206     MH        Tigers        Medicine … 3        https://…
#>  8 Penticton Vees  277     PEN       Vees          Penticton  2        https://…
#>  9 Portland Winte… 208     POR       Winterhawks   Portland   6        https://…
#> 10 Prince Albert … 209     PA        Raiders       Prince Al… 1        https://…
#> 11 Prince George … 210     PG        Cougars       Prince Ge… 2        https://…
#> 12 Red Deer Rebels 211     RD        Rebels        Red Deer   1        https://…
#> 13 Regina Pats     212     REG       Pats          Regina     1        https://…
#> 14 Saskatoon Blad… 213     SAS       Blades        Saskatoon  1        https://…
#> 15 Seattle Thunde… 214     SEA       Thunderbirds  Seattle    6        https://…
#> 16 Spokane Chiefs  215     SPO       Chiefs        Spokane    6        https://…
#> 17 TBA             263     TBA       TBA           TBA        1        https://…
```
