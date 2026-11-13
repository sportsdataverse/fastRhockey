# **Load NHL schedules (alias)**

Alias of
[`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_nhl_schedules(seasons = most_recent_nhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2010.

- ...:

  Additional arguments passed to an underlying function.

## Value

See
[`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md).

## See also

[`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md)

Other NHL Loader Functions:
[`load_nhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_boxscores.md),
[`load_nhl_pbp_full()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_full.md),
[`load_nhl_player_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscore.md),
[`load_nhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscores.md),
[`load_nhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_boxscores.md),
[`load_nhl_team_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscore.md),
[`load_nhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_nhl_schedules(2022))
#> ── NHL schedule ─────────────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-12 03:17:25 UTC
#> # A tibble: 1,401 × 35
#>       game_id season_full game_type game_date  game_time          home_team_abbr
#>         <int> <chr>       <chr>     <chr>      <chr>              <chr>         
#>  1 2021030416 20212022    P         2022-06-27 2022-06-27T00:00:… TBL           
#>  2 2021030415 20212022    P         2022-06-25 2022-06-25T00:00:… COL           
#>  3 2021030414 20212022    P         2022-06-23 2022-06-23T00:00:… TBL           
#>  4 2021030413 20212022    P         2022-06-21 2022-06-21T00:00:… TBL           
#>  5 2021030412 20212022    P         2022-06-19 2022-06-19T00:00:… COL           
#>  6 2021030411 20212022    P         2022-06-16 2022-06-16T00:00:… COL           
#>  7 2021030316 20212022    P         2022-06-12 2022-06-12T00:00:… TBL           
#>  8 2021030315 20212022    P         2022-06-10 2022-06-10T00:00:… NYR           
#>  9 2021030314 20212022    P         2022-06-08 2022-06-08T00:00:… TBL           
#> 10 2021030324 20212022    P         2022-06-07 2022-06-07T00:00:… EDM           
#> # ℹ 1,391 more rows
#> # ℹ 29 more variables: away_team_abbr <chr>, home_team_name <chr>,
#> #   away_team_name <chr>, home_score <int>, away_score <int>, game_state <chr>,
#> #   venue <chr>, series_letter <chr>, playoff_round <int>,
#> #   series_game_number <int>, season <int>, game_json <lgl>,
#> #   game_json_url <chr>, PBP <lgl>, team_box <lgl>, player_box <lgl>,
#> #   skater_box <lgl>, goalie_box <lgl>, game_info <lgl>, game_rosters <lgl>, …
# }
```
