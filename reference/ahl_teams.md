# **AHL Teams**

AHL teams for a given season from the HockeyTech feed.

## Usage

``` r
ahl_teams(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per team.

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
[`ahl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ahl_schedule.md),
[`ahl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/ahl_season_id.md),
[`ahl_standings()`](https://fastRhockey.sportsdataverse.org/reference/ahl_standings.md),
[`ahl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/ahl_team_roster.md),
[`most_recent_ahl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_ahl_season.md)

## Examples

``` r
 try(ahl_teams()) 
#> ── AHL Teams from HockeyTech ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-12 18:38:59 UTC
#> # A tibble: 32 × 7
#>    team_name       team_id team_code team_nickname team_label division team_logo
#>    <chr>           <chr>   <chr>     <chr>         <chr>      <chr>    <chr>    
#>  1 Abbotsford Can… 440     ABB       Canucks       Abbotsford 25       https://…
#>  2 Bakersfield Co… 402     BAK       Condors       Bakersfie… 25       https://…
#>  3 Belleville Sen… 413     BEL       Senators      Belleville 16       https://…
#>  4 Calgary Wrangl… 444     CGY       Wranglers     Calgary    25       https://…
#>  5 Charlotte Chec… 384     CLT       Checkers      Charlotte  15       https://…
#>  6 Chicago Wolves  330     CHI       Wolves        Chicago    24       https://…
#>  7 Cleveland Mons… 373     CLE       Monsters      Cleveland  16       https://…
#>  8 Coachella Vall… 445     CV        Firebirds     Coachella… 25       https://…
#>  9 Colorado Eagles 419     COL       Eagles        Colorado   25       https://…
#> 10 Grand Rapids G… 328     GR        Griffins      Grand Rap… 24       https://…
#> # ℹ 22 more rows
```
