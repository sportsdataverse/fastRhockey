# **HockeyAllsvenskan Team stats**

Team stats of a season, one category per call. W / L are totals
including overtime and shootout (wins_total / losses_total).

`wins_total` / `losses_total` (the site's W / L) are TOTALS including
overtime and shootout results (`otw`, `sow`, `otl`, `sol` are subsets);
standings' `regulation_wins` count regulation wins only. `team_code` is
the display code. Only the sampled `results` column set is pinned in the
documented columns below.

## Usage

``` r
ha_team_stats(
  season = NULL,
  phase = "HA",
  category = "results",
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

  Stats category (column set), one of results, goals, power_play,
  penalty_kill, penalties, offensive, defensive, faceoffs, shootouts,
  corsi_fenwick_pdo, stoppage_reasons, attendance. Anything else answers
  zero rows. Allowed: `results`, `goals`, `power_play`, `penalty_kill`,
  `penalties`, `offensive`, `defensive`, `faceoffs`, `shootouts`,
  `corsi_fenwick_pdo`, `stoppage_reasons`, `attendance`.

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
| team_code | character | Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| gp | integer | Games played in the season and phase. |
| wins_total | integer | Wins in total, overtime and shootout wins included (otw and sow are subsets). |
| losses_total | integer | Losses in total, overtime and shootout losses included (otl and sol are subsets). |
| leading | integer | Games or situations leading (StatNet 'Leading'; meaning unverified). |
| trailing | integer | Games or situations trailing (StatNet 'Trailing'; meaning unverified). |
| g | integer | Goals scored by the team. |
| ga | integer | Goals against: goals allowed in the season and phase. |
| gfg | integer | Games in which the team scored the first goal (inferred from the label; unverified). |
| otw | integer | Overtime wins (a subset of wins_total). |
| otl | integer | Overtime losses. |
| sow | integer | Shootout wins (a subset of wins_total). |
| sol | integer | Shootout losses. |
| p_gp | numeric | Points per game. |
| gpg | numeric | Goals for per game (number). |
| gapg | numeric | Goals against per game (number). |
| points | integer | Standings points: 3 per regulation win, 2 per overtime/shootout win, 1 per overtime/shootout loss (2025 IFB: 102 + 14 + 3 = 119). |

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
[`ha_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_goalie_stats.md),
[`ha_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ha_leaders.md),
[`ha_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ha_pbp.md),
[`ha_players()`](https://fastRhockey.sportsdataverse.org/reference/ha_players.md),
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_team_stats(season = 2026, category = "results"))
#> ── HA Team Stats Information from hockeyallsvenskan.se ─────────────────────────
#> ℹ Data updated: 2026-10-13 01:54:20 UTC
#> # A tibble: 14 × 19
#>    season  rank team_code    gp wins_total losses_total leading trailing     g
#>     <int> <int> <chr>     <int>      <int>        <int>   <int>    <int> <int>
#>  1   2026     1 IFB          52         41           11      60       29   192
#>  2   2026     2 KHC          52         37           15      54       32   189
#>  3   2026     3 BIK          52         34           18      48       37   145
#>  4   2026     4 MoDo         52         31           21      50       35   141
#>  5   2026     5 AIK          52         29           23      54       42   150
#>  6   2026     6 AIS          52         25           27      36       49   127
#>  7   2026     7 NVIF         52         25           27      46       46   139
#>  8   2026     8 MORA         52         24           28      36       57   134
#>  9   2026     9 IKO          52         23           29      42       43   143
#> 10   2026    10 SSK          52         22           30      45       36   130
#> 11   2026    11 ÖIK          52         21           31      37       53   123
#> 12   2026    12 VHC          52         21           31      35       50   111
#> 13   2026    13 VIK          52         17           35      37       50   125
#> 14   2026    14 TRO          52         14           38      35       56   123
#> # ℹ 10 more variables: ga <int>, gfg <int>, otw <int>, otl <int>, sow <int>,
#> #   sol <int>, p_gp <dbl>, gpg <dbl>, gapg <dbl>, points <int>
# }
```
