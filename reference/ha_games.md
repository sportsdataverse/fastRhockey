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
#> ── HA Games Information from hockeyallsvenskan.se ──── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:54:10 UTC
#> # A tibble: 364 × 26
#>    season date    home_team_code away_team_code home_score away_score decided_in
#>     <int> <chr>   <chr>          <chr>               <int>      <int> <chr>     
#>  1   2026 2025-0… MoDo           ÖIK                     3          1 FT        
#>  2   2026 2025-0… SSK            BIK                     1          4 FT        
#>  3   2026 2025-0… AIK            VIK                     2          1 FT        
#>  4   2026 2025-0… MORA           IFB                     1          2 SO        
#>  5   2026 2025-0… NVIF           IKO                     3          2 FT        
#>  6   2026 2025-0… AIS            KHC                     4          1 FT        
#>  7   2026 2025-0… VHC            TRO                     1          2 OT        
#>  8   2026 2025-0… SSK            TRO                     5          4 FT        
#>  9   2026 2025-0… IFB            AIK                     5          1 FT        
#> 10   2026 2025-0… KHC            VHC                     6          1 FT        
#> # ℹ 354 more rows
#> # ℹ 19 more variables: is_played <lgl>, arena <chr>, attendance <int>,
#> #   home_shots_total <int>, home_shots_goal <int>, home_shots_efficiency <dbl>,
#> #   home_powerplay_goals <int>, home_powerplay_num_pp <int>,
#> #   home_powerplay_efficiency <dbl>, home_expulsions_in_minutes <int>,
#> #   away_shots_total <int>, away_shots_goal <int>, away_shots_efficiency <dbl>,
#> #   away_powerplay_goals <int>, away_powerplay_num_pp <int>, …
# }
```
