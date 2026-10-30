# **HockeyAllsvenskan Standings**

Standings of a season. Wins and losses are REGULATION only; overtime
counts include shootouts.

Wins and losses are REGULATION only (`regulation_wins` /
`regulation_losses`) and the overtime counts include shootouts
(`overtime_and_shootout_wins` / `_losses`); the team table's
`wins_total` / `losses_total` of
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md)
are totals including overtime and shootout results, so do not mix them.
`team_code` is the display code; the upstream `teamCode` mixes the
StatNet id (MODO) and the display code (MORA) and is kept only as
`team_code_mixed`. `power_play_perc` / `penalty_kill_perc` arrive as
`""` (NA).

## Usage

``` r
ha_standings(
  season = NULL,
  phase = "HA",
  location = "all",
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

- location:

  Home / away standings split. Allowed: `all`, `home`, `away`.

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
| team_code_mixed | character | Upstream standings teamCode: the StatNet id for some clubs (MODO), the display code for others (MORA, NVIF, ÖIK). Join on team_code instead. |
| team_code | character | Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| games_played | integer | Games played in the season and phase. |
| regulation_wins | integer | Regulation wins only (the team table's wins_total also counts OT and shootout wins). |
| regulation_losses | integer | Regulation losses only. |
| ties | integer | Ties (0 in the overtime era). |
| overtime_and_shootout_wins | integer | Overtime wins, shootout wins included. |
| overtime_and_shootout_losses | integer | Overtime losses, shootout losses included. |
| shootouts_wins | integer | Shootout wins as sent (0 in every sampled row: they are counted in overtime_and_shootout_wins). |
| shootouts_losses | integer | Shootout losses as sent (0 in every sampled row). |
| goals | integer | Goals scored by the team (goals for). |
| goals_against | integer | Goals conceded by the team. |
| goal_difference | integer | Goal difference. |
| total_points | integer | Standings points of the team. |
| power_play_perc | numeric | Power-play percentage; empty-string placeholder ” in every sample. |
| penalty_kill_perc | numeric | Penalty-kill percentage; empty-string placeholder ” in every sample. |

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
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_standings(season = 2026))
#> Error in nothing("answered no rows (a season or phase without data, or an unknown filter value)") : 
#>   hockeyallsvenskan.se/api/league-standings-all-time: answered no rows (a
#> season or phase without data, or an unknown filter value)
# }
```
