# **OHL Team Roster**

OHL roster for a given team and season from the HockeyTech feed.

## Usage

``` r
ohl_team_roster(team_id, season = NULL, season_id = NULL)
```

## Arguments

- team_id:

  Numeric or character OHL team identifier.

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player.

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
[`ohl_teams()`](https://fastRhockey.sportsdataverse.org/reference/ohl_teams.md)

## Examples

``` r
 try(ohl_team_roster(team_id = 1)) 
#> ── OHL Team Roster from HockeyTech ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 11:32:54 UTC
#> # A tibble: 27 × 45
#>    id    person_id active first_name last_name phonetic_name display_name shoots
#>    <chr> <chr>     <chr>  <chr>      <chr>     <chr>         <chr>        <chr> 
#>  1 9509  9270      1      George     Komadoski "COMM-uh-DAH… ""           R     
#>  2 9781  9573      1      Jean-Samu… Daigneau… ""            ""           L     
#>  3 9777  9569      1      Nathan     Hauad     ""            ""           R     
#>  4 9475  9221      1      Jeremy     Freeman   "FREE-man"    ""           R     
#>  5 9773  9565      1      Jason      Musa      ""            ""           L     
#>  6 9766  9558      1      Jack       Torr      "TOR"         ""           R     
#>  7 9778  9570      1      Abe        Barnett   ""            ""           L     
#>  8 9552  9313      1      Kaden      McGregor  "MUH-GREG-ER" ""           R     
#>  9 9765  9557      1      Xavier     Lieb      "LEEB"        ""           R     
#> 10 10038 9858      1      Ethan      Chen      ""            ""           R     
#> # ℹ 17 more rows
#> # ℹ 37 more variables: hometown <chr>, homeprov <chr>, homecntry <chr>,
#> #   homeplace <chr>, birthtown <chr>, birthprov <chr>, birthcntry <chr>,
#> #   birthplace <chr>, height <chr>, weight <chr>, height_hyphenated <chr>,
#> #   hidden <chr>, current_team <chr>, player_id <chr>, status <chr>,
#> #   birthdate <chr>, birthdate_year <chr>, rawbirthdate <chr>,
#> #   latest_team_id <chr>, veteran_status <chr>, veteran_description <chr>, …
```
