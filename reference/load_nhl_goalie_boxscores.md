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
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_goalie_boxscores/goalie_box_2022.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_goalie_boxscores/goalie_box_2022.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
