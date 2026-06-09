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
#> ℹ Data updated: 2026-06-09 20:31:08 UTC
#> # A tibble: 32 × 44
#>    id    person_id active first_name last_name phonetic_name display_name shoots
#>    <chr> <chr>     <chr>  <chr>      <chr>     <chr>         <chr>        <chr> 
#>  1 23761 29979     1      Max        Vilen     ""            ""           L     
#>  2 21322 27011     1      Dominik    Necak     ""            ""           L     
#>  3 21315 26820     1      Jackson    Batchild… ""            ""           R     
#>  4 19539 24640     1      Gabe       Smith     ""            ""           L     
#>  5 19090 23871     1      Alex       Mercier   ""            ""           R     
#>  6 22466 28440     1      Simon      Binkley   ""            ""           L     
#>  7 21400 27010     1      Loïc       Nasreddi… ""            ""           L     
#>  8 20414 25480     1      Teddy      Mutryn    ""            ""           R     
#>  9 21678 27546     1      Alexander  Hendry    ""            ""           L     
#> 10 23995 30224     1      Matthew    Virgilio  ""            ""           R     
#> # ℹ 22 more rows
#> # ℹ 36 more variables: hometown <chr>, homeprov <chr>, homecntry <chr>,
#> #   homeplace <chr>, birthtown <chr>, birthprov <chr>, birthcntry <chr>,
#> #   birthplace <chr>, height <chr>, weight <chr>, height_hyphenated <chr>,
#> #   hidden <chr>, current_team <chr>, player_id <chr>, status <chr>,
#> #   birthdate <chr>, birthdate_year <chr>, rawbirthdate <chr>,
#> #   latest_team_id <chr>, veteran_status <chr>, veteran_description <chr>, …
```
