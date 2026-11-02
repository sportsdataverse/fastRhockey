# **Load fastRhockey NHL play-by-play (full, alias)**

Alias of
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
for naming parity with sportsdataverse-py. In this package
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
already serves the full `nhl_pbp_full` release.

## Usage

``` r
load_nhl_pbp_full(seasons = most_recent_nhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2010.

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

## Value

See
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md).

## See also

[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
[`load_nhl_pbp_lite()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_lite.md)

Other NHL Loader Functions:
[`load_nhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_boxscores.md),
[`load_nhl_player_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscore.md),
[`load_nhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscores.md),
[`load_nhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedules.md),
[`load_nhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_boxscores.md),
[`load_nhl_team_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscore.md),
[`load_nhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscores.md)

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
  try(load_nhl_pbp_full(2024))
# }
}
```
