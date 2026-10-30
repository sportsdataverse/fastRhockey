# **HockeyAllsvenskan Games of a season**

Every game of a season, played or scheduled (is_played): date, display
team codes, score, periods, arena, attendance, shots and power play. No
game id.

The season in progress lists every scheduled game. An unplayed one has
`decided_in == ""`, a 0-0 score and a null attendance, so `is_played` is
`decided_in != ""`: filter on it rather than on the score.
`home_team_code` / `away_team_code` are display codes, while the
`most_points` JSON names StatNet team ids. The table carries no game id:
join CMS games
([`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md))
on `date` + team codes. `*_efficiency` is a percentage. The call is
refused as truncated unless the site answers one complete page.

## Usage

``` r
ha_games(
  season = NULL,
  phase = "HA",
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
| date | character | Game date (YYYY-MM-DD). |
| home_team_code | character | Display code of the home team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| away_team_code | character | Display code of the away team, not the StatNet id. |
| home_score | integer | Home goals, final. |
| away_score | integer | Away goals, final. |
| decided_in | character | How the game was decided (FT, OT, SO); empty for a game not yet played. |
| is_played | logical | True when the game has been played (decided_in is not empty). |
| arena | character | Arena the game is played in, as the games table names it. |
| attendance | integer | Attendance; null for an unplayed game (0 for a played one means unknown). |
| home_shots_total | integer | Home team: shots on goal. |
| home_shots_goal | integer | Home team: goals. |
| home_shots_efficiency | numeric | Home team: shooting percentage (goals per 100 shots); sent as text, 0 or null. |
| home_powerplay_goals | integer | Home team: power-play goals. |
| home_powerplay_num_pp | integer | Home team: power plays. |
| home_powerplay_efficiency | numeric | Home team: power-play percentage; sent as text, 0 or null. |
| home_expulsions_in_minutes | integer | Home team: penalty minutes. |
| away_shots_total | integer | Away team: shots on goal. |
| away_shots_goal | integer | Away team: goals. |
| away_shots_efficiency | numeric | Away team: shooting percentage (goals per 100 shots); sent as text, 0 or null. |
| away_powerplay_goals | integer | Away team: power-play goals. |
| away_powerplay_num_pp | integer | Away team: power plays. |
| away_powerplay_efficiency | numeric | Away team: power-play percentage; sent as text, 0 or null. |
| away_expulsions_in_minutes | integer | Away team: penalty minutes. |
| period_scores | character | Goals per period, JSON list of (Home, Away); null for an unplayed game. |
| most_points | character | Top point scorers of the game, JSON list of (player, team (StatNet id), points). |

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
[`ha_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_goalie_stats.md),
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
  try(ha_games(season = 2026))
#> Error in nothing("answered no rows (a season or phase without data, or an unknown filter value)") : 
#>   hockeyallsvenskan.se/api/all-time-games-stats: answered no rows (a
#> season or phase without data, or an unknown filter value)
# }
```
