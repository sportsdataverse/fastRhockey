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
#> Error in nothing("answered no rows (a season or phase without data, or an unknown filter value)") : 
#>   hockeyallsvenskan.se/api/all-time-teams-stats: answered no rows (a
#> season or phase without data, or an unknown filter value)
# }
```
