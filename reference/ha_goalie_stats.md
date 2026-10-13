# **HockeyAllsvenskan Goalkeeper stats**

Goalkeeper stats of a season, one category per call. Names and StatNet
team ids, no player ids.

Names and StatNet team ids only: the table carries no player id. The
`season` column is the request's END year. Only the sampled `summary`
column set is pinned in the documented columns below; other `category`
values bring their own columns.

## Usage

``` r
ha_goalie_stats(
  season = NULL,
  phase = "HA",
  category = "summary",
  team_side = "all",
  team = "all",
  against_team = "all",
  all_results = "all",
  game_status = "all",
  decided_in = "all",
  return_parsed = TRUE
)
```

## Arguments

- season:

  Season END year (2026 = 2025/26); the site's START-year key is sent as
  `season - 1`. `NULL` = the season in progress. Stats and standings
  start in 2005/06 (2006).

- phase:

  Game type: HA regular season, HAPlayoff playoffs, HAkval playout
  (relegation series). Allowed: `HA`, `HAPlayoff`, `HAkval`.

- category:

  Stats category (column set), one of summary, results, game_strength,
  defensive, shootouts. Anything else answers zero rows. Allowed:
  `summary`, `results`, `game_strength`, `defensive`, `shootouts`.

- team_side:

  Home / away split. Allowed: `all`, `home`, `away`.

- team:

  Team filter: all, or one team of the site's filter list (values not
  probed).

- against_team:

  Opponent filter: all, or one team (values not probed).

- all_results:

  Result filter: W win, T tie, L loss. Allowed: `all`, `W`, `T`, `L`.

- game_status:

  Game status filter: played drops the season's unplayed games. Allowed:
  `all`, `played`.

- decided_in:

  How the game was decided: ft full time, ot overtime, so shootout, wo
  walkover. Allowed: `all`, `ft`, `ot`, `so`, `wo`.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (stamped with the request under `_request` where
  the body does not say what it answers).

## Value

A tibble (`return_parsed = TRUE`), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season END year (2026 = 2025/26). |
| rank | integer | Rank in the table. |
| player_name | character | Player name (the table carries no player id). |
| team_statnet_id | character | StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code. |
| svs | integer | Saves made by the goalkeeper. |
| svs_perc | numeric | Save percentage ('92.61'). |
| gp | integer | Games played in the season and phase. |
| gpi | integer | Games played in (appeared). |
| ga | integer | Goals against: goals allowed in the season and phase. |
| gaa | numeric | Goals-against average. |
| w | integer | Wins credited to the goalkeeper. |
| t | integer | Ties credited to the goalkeeper (0 in the overtime era). |
| l | integer | Losses credited to the goalkeeper. |
| so | integer | Shutouts recorded by the goalkeeper. |
| mip | character | Minutes played, total mm:ss string ('1726:51'). |
| valid_ranking | integer | 1 when the goalkeeper qualifies for the ranked list (minimum games), else 0. |

## Errors

`fastRhockey_no_data`: the site has nothing for the request (a season
before 2005/06, an unknown or pre-2025/26 game, a phase not played, a
body without rows). `fastRhockey_bad_request`: a value outside its
enumerated set, a field the site requires is missing, or HTTP 400/422.
`fastRhockey_asset_fetch`: any other non-2xx, a connection failure after
retries, or an empty, non-JSON, error-shaped, truncated or out-of-scope
200 (the answer is unknown, not empty).

## See also

Other HA Functions:
[`ha_game()`](https://fastRhockey.sportsdataverse.org/reference/ha_game.md),
[`ha_games()`](https://fastRhockey.sportsdataverse.org/reference/ha_games.md),
[`ha_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ha_leaders.md),
[`ha_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ha_pbp.md),
[`ha_players()`](https://fastRhockey.sportsdataverse.org/reference/ha_players.md),
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_goalie_stats(season = 2026, category = "summary"))
#> ── HA Goalie Stats Information from hockeyallsvenskan.se ───────────────────────
#> ℹ Data updated: 2026-10-13 01:54:11 UTC
#> # A tibble: 45 × 16
#>    season  rank player_name     team_statnet_id   svs svs_perc    gp   gpi    ga
#>     <int> <int> <chr>           <chr>           <int>    <dbl> <int> <int> <int>
#>  1   2026     1 Frans Tuohimaa  IFB               664     92.6    47    30    53
#>  2   2026     2 Jacob Crespin   KHC               821     92.1    51    36    70
#>  3   2026     3 Olle Eriksson … IFB               442     92.1    52    25    38
#>  4   2026     4 Love Härenstam  SSK               664     92.0    42    32    58
#>  5   2026     5 Herman Liv      AIS               620     91.6    43    25    57
#>  6   2026     6 Olof Lindbom    BIK               529     91.2    34    27    51
#>  7   2026     7 Carl Lidö       OSIK              970     90.9    50    40    97
#>  8   2026     8 Marcus Hellgre… MIK               893     90.8    49    36    90
#>  9   2026     9 Pontus Eltonius VHC               814     90.6    50    36    84
#> 10   2026    10 Adam Werner     MODO              792     90.4    49    38    84
#> # ℹ 35 more rows
#> # ℹ 7 more variables: gaa <dbl>, w <int>, t <int>, l <int>, so <int>,
#> #   mip <chr>, valid_ranking <int>
# }
```
