# **HockeyAllsvenskan Skater stats**

Skater stats of a season, one category (column set) per call. Names and
StatNet team ids, no player ids.

Names and StatNet team ids only: the table carries no player id (use
[`ha_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ha_leaders.md)
for the current season's StatNet player ids). The `season` column is the
request's END year (the table does not say). Only the sampled `summary`
column set is pinned in the documented columns below; other `category`
values bring their own count columns (sent as numbers, parsed from the
payload).

## Usage

``` r
ha_skater_stats(
  season = NULL,
  phase = "HA",
  category = "summary",
  position = "all",
  ages = "all",
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

  Stats category (column set), one of summary, points, goals,
  time_on_ice, power_play, penalty_kill, equal_strength, penalties,
  plus_minus, faceoffs, offensive, defensive, shootouts,
  corsi_fenwick_pdo. Anything else answers zero rows. Allowed:
  `summary`, `points`, `goals`, `time_on_ice`, `power_play`,
  `penalty_kill`, `equal_strength`, `penalties`, `plus_minus`,
  `faceoffs`, `offensive`, `defensive`, `shootouts`,
  `corsi_fenwick_pdo`.

- position:

  Position filter (back = defence). Allowed: `all`, `forward`, `back`,
  `goalkeeper`.

- ages:

  Age-group filter. Allowed: `all`, `junior`, `rookie`.

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
| gp | integer | Games played in the season and phase. |
| tp | integer | Points (goals + assists). |
| g | integer | Goals scored by the skater. |
| a | integer | Assists credited to the skater. |
| pim | integer | Penalty minutes. |
| gwg | integer | Game-winning goals. |
| ppg | integer | Power-play goals. |
| sog | integer | Shots on goal by the skater. |
| hits | integer | Hits (0 in seasons before tracking). |
| bks | integer | Blocked shots (0 in seasons before tracking). |
| plus | integer | Even-strength / short-handed goals for while on ice (plus count). |
| minus | integer | Goals against while on ice (minus count). |
| plus_minus | integer | Plus/minus: the Plus count less the Minus count. |
| toi_gp | character | Average time on ice per game, mm:ss string ('0:00' = not tracked, e.g. 2010). |
| tppg | numeric | Points per game. |

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
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_skater_stats(season = 2026, category = "summary"))
#> ── HA Skater Stats Information from hockeyallsvenskan.se ───────────────────────
#> ℹ Data updated: 2026-10-13 01:54:18 UTC
#> # A tibble: 511 × 19
#>    season  rank player_name  team_statnet_id    gp    tp     g     a   pim   gwg
#>     <int> <int> <chr>        <chr>           <int> <int> <int> <int> <int> <int>
#>  1   2026     1 Scott Pooley AIK                51    59    27    32     8     4
#>  2   2026     2 Liam Dower … IFB                51    57    21    36    22     4
#>  3   2026     3 George Diaco KHC                52    54    25    29    20     4
#>  4   2026     4 Teemu Lepaus IKO                52    51    15    36    16     2
#>  5   2026     5 Mikkel Øby-… AIS                52    48    28    20     6    10
#>  6   2026     6 Liam Hawel   KHC                51    45    13    32    12     3
#>  7   2026     7 Marcus Nils… IFB                50    45    12    33    14     4
#>  8   2026     8 Axel Ottoss… IFB                52    41    25    16    48     4
#>  9   2026     9 Herman Träff IKO                51    41    23    18    32     6
#> 10   2026    10 Oscar Tells… IFB                52    40    20    20    14     6
#> # ℹ 501 more rows
#> # ℹ 9 more variables: ppg <int>, sog <int>, hits <int>, bks <int>, plus <int>,
#> #   minus <int>, plus_minus <int>, toi_gp <chr>, tppg <dbl>
# }
```
