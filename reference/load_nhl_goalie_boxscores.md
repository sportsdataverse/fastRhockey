# **Load NHL goalie box scores (alias)**

Alias of
[`load_nhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_box.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_nhl_goalie_boxscores(seasons = most_recent_nhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

## Value

See
[`load_nhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_box.md).

## See also

[`load_nhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_box.md)

Other NHL Loader Functions:
[`load_nhl_pbp_full()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_full.md),
[`load_nhl_player_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscore.md),
[`load_nhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscores.md),
[`load_nhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedules.md),
[`load_nhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_boxscores.md),
[`load_nhl_team_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscore.md),
[`load_nhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_nhl_goalie_boxscores(2022))
#> ── NHL goalie boxscores ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-09 07:37:00 UTC
#> # A tibble: 5,605 × 24
#>    home_away team_id team_abbrev player_id player_name     sweater_number
#>    <chr>       <int> <chr>           <int> <chr>                    <int>
#>  1 away            5 PIT           8479193 C. DeSmith                   1
#>  2 away            5 PIT           8477465 T. Jarry                    35
#>  3 home           14 TBL           8470880 B. Elliott                   1
#>  4 home           14 TBL           8476883 A. Vasilevskiy              88
#>  5 away           55 SEA           8475831 P. Grubauer                 31
#>  6 away           55 SEA           8476904 C. Driedger                 60
#>  7 home           54 VGK           8476316 L. Brossoit                 39
#>  8 home           54 VGK           8475215 R. Lehner                   90
#>  9 away            8 MTL           8474596 J. Allen                    34
#> 10 away            8 MTL           8478470 S. Montembeault             35
#> # ℹ 5,595 more rows
#> # ℹ 18 more variables: even_strength_shots_against <chr>,
#> #   power_play_shots_against <chr>, shorthanded_shots_against <chr>,
#> #   save_shots_against <chr>, save_pctg <dbl>,
#> #   even_strength_goals_against <int>, power_play_goals_against <int>,
#> #   shorthanded_goals_against <int>, pim <int>, goals_against <int>, toi <chr>,
#> #   starter <lgl>, decision <chr>, shots_against <int>, saves <int>, …
# }
```
