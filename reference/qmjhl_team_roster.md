# **QMJHL Team Roster**

QMJHL roster for a given team and season from the HockeyTech feed.

## Usage

``` r
qmjhl_team_roster(team_id, season = NULL, season_id = NULL)
```

## Arguments

- team_id:

  Numeric or character QMJHL team identifier.

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_team_roster(team_id = 1)) 
#> ── QMJHL Team Roster from HockeyTech ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 13:00:30 UTC
#> # A tibble: 24 × 45
#>    id    person_id active first_name last_name phonetic_name display_name shoots
#>    <chr> <chr>     <chr>  <chr>      <chr>     <chr>         <chr>        <chr> 
#>  1 24551 25541     1      Tommy      Coccaro   ""            ""           L     
#>  2 23957 26017     1      Myles      Brosnan   ""            ""           R     
#>  3 20247 25783     1      Ben        Lindsay   ""            ""           L     
#>  4 24550 31136     1      Christoph… Baird-Ga… ""            ""           L     
#>  5 21315 26820     1      Jackson    Batchild… ""            ""           R     
#>  6 20194 25748     1      Liam       Kilfoil   ""            ""           L     
#>  7 23945 27134     1      William    Manchuso  ""            ""           L     
#>  8 24589 31165     1      Jiko       Laitinen  ""            ""           L     
#>  9 23415 27123     1      Noah       Survilas  ""            ""           R     
#> 10 21351 26824     1      Charlie    Benigno   ""            ""           R     
#> # ℹ 14 more rows
#> # ℹ 37 more variables: hometown <chr>, homeprov <chr>, homecntry <chr>,
#> #   homeplace <chr>, birthtown <chr>, birthprov <chr>, birthcntry <chr>,
#> #   birthplace <chr>, height <chr>, weight <chr>, height_hyphenated <chr>,
#> #   hidden <chr>, current_team <chr>, player_id <chr>, status <chr>,
#> #   birthdate <chr>, birthdate_year <chr>, rawbirthdate <chr>,
#> #   latest_team_id <chr>, veteran_status <chr>, veteran_description <chr>, …
```
