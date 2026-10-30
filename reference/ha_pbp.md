# **HockeyAllsvenskan Play-by-play**

One game's events (2025/26 onward): shots with outcome, goals with
assists and strength, penalties, goalkeeper changes, timeouts. No
coordinates, no on-ice players.

One row per event (Shot, Goal, Penalty, GoalkeeperEvent, Timeout);
`time` is seconds into the period. The body names no game, so
`statnet_game_number`, `home_statnet_id` and `away_statnet_id` come from
the request. `team_statnet_id` is the StatNet team id and `team_code`
the display code; `player_statnet_id` is the StatNet player id (the same
id space as SHL's isa id). Goals carry `running_score`, the strength in
`event_description` and the assists by NAME (`assist1_name`). No shot
coordinates, no on-ice players. Get the four required ids from a row of
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md)
or
[`ha_game()`](https://fastRhockey.sportsdataverse.org/reference/ha_game.md).

## Usage

``` r
ha_pbp(
  statnet_game_number,
  home_statnet_id,
  away_statnet_id,
  scheduled_date_time,
  played_date_time = NULL,
  return_parsed = TRUE
)
```

## Arguments

- statnet_game_number:

  StatNet game number (statnet_game_number in ha_schedule / ha_game),
  e.g. 23401.

- home_statnet_id:

  StatNet team id of the home team, e.g. AIK.

- away_statnet_id:

  StatNet team id of the away team, e.g. MODO.

- scheduled_date_time:

  Scheduled start (scheduled_date_time in ha_schedule), ISO 8601 UTC.
  Required by the site.

- played_date_time:

  Actual start (played_date_time), ISO 8601 UTC. Optional. `NULL` = not
  sent.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (stamped with the request under `_request` where
  the body does not say what it answers).

## Value

A tibble (`return_parsed = TRUE`), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| statnet_game_number | integer | StatNet game number (23401 = 2026/27 game 1); the play-by-play request key. |
| home_statnet_id | character | StatNet team id of the home team (AIK, MODO, MIK). |
| away_statnet_id | character | StatNet team id of the away team. |
| period | integer | Period number the event belongs to. |
| period_label | character | Period label ('Period 1'). |
| event_type | character | Event type: Shot, Goal, Penalty, GoalkeeperEvent or Timeout. |
| time | integer | Seconds into the period. |
| is_entering | logical | Goalkeeper event: true when the goalkeeper enters, false when he leaves. |
| team_name | character | Team label as shown on the card (the display team code). |
| team_code | character | Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| team_statnet_id | character | StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code. |
| player_statnet_id | integer | StatNet player id (the same id space as SHL's isa id, verified on one player). |
| player_first_name | character | Player: first name of the player. |
| player_family_name | character | Player: family name of the player. |
| player_jersey_number | integer | Player: jersey number of the player. |
| event_description | character | Shot outcome, goal strength (EQ), penalty text or timeout note. |
| running_score | character | Score after the goal, home-away. |
| scorer_season_goals | integer | Scorer's season goals after this goal. |
| is_penalty_shot | logical | Goal scored on a penalty shot. |
| assist1_name | character | First assist, by name (no id). |
| assist1_jersey_today | integer | First assist's jersey number in this game. |
| assist1_season_assists | integer | First assist's season assists after this goal. |
| assist2_name | character | Second assist, by name (no id). |
| assist2_jersey_today | integer | Second assist's jersey number in this game. |
| assist2_season_assists | integer | Second assist's season assists after this goal. |

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
[`ha_players()`](https://fastRhockey.sportsdataverse.org/reference/ha_players.md),
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_pbp(
    statnet_game_number = 23401, home_statnet_id = "AIK", away_statnet_id = "MODO",
    scheduled_date_time = "2026-09-18T17:00:00.000Z"
  ))
#> ── HA Pbp Information from hockeyallsvenskan.se ────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:12:14 UTC
#> # A tibble: 113 × 25
#>    statnet_game_number home_statnet_id away_statnet_id period period_label
#>                  <int> <chr>           <chr>            <int> <chr>       
#>  1               23401 AIK             MODO                 1 Period 1    
#>  2               23401 AIK             MODO                 1 Period 1    
#>  3               23401 AIK             MODO                 1 Period 1    
#>  4               23401 AIK             MODO                 1 Period 1    
#>  5               23401 AIK             MODO                 1 Period 1    
#>  6               23401 AIK             MODO                 1 Period 1    
#>  7               23401 AIK             MODO                 1 Period 1    
#>  8               23401 AIK             MODO                 1 Period 1    
#>  9               23401 AIK             MODO                 1 Period 1    
#> 10               23401 AIK             MODO                 1 Period 1    
#> # ℹ 103 more rows
#> # ℹ 20 more variables: event_type <chr>, time <int>, is_entering <lgl>,
#> #   team_name <chr>, team_code <chr>, team_statnet_id <chr>,
#> #   player_statnet_id <int>, player_first_name <chr>, player_family_name <chr>,
#> #   player_jersey_number <int>, event_description <chr>, running_score <chr>,
#> #   scorer_season_goals <int>, is_penalty_shot <lgl>, assist1_name <chr>,
#> #   assist1_jersey_today <int>, assist1_season_assists <int>, …
# }
```
