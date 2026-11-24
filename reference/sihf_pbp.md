# **SIHF Event timeline of one game**

Event timeline of one game: goals, penalties, goaltender changes,
revoked goals and shootout attempts.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_pbp(game_id, language = "en")
```

## Arguments

- game_id:

  Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 34 columns. A zero-row tibble with
these columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| event_index | integer | 1-based event order within the game. |
| period_number | integer | 1-based period (4 = first overtime); the shootout follows the last period. |
| period | character | Period name as SIHF prints it, German (e.g., "1. Drittel", "Shootout"). |
| event_type | character | Event kind: "goal", "penalty", "goalie", "revoked_goal" or "shootout". |
| time | character | Elapsed game time of the event, "mm:ss" (null on shootout rows). |
| game_seconds | integer | Elapsed game time of the event in seconds. |
| team_id | integer | SIHF team id of the team the event belongs to. |
| team_side | character | Side of the game the row belongs to: "home" or "away". |
| player_id | integer | Licence number of the scorer, penalized player, goaltender (goalie rows) or shooter. |
| player_name | character | Name of the scorer, penalized player, goaltender (goalie rows) or shooter. |
| player_jersey | integer | Jersey number of the player in player_id. |
| assist1_player_id | integer | Licence number of the first assist. |
| assist1_player_name | character | Name of the first assist. |
| assist2_player_id | integer | Licence number of the second assist. |
| assist2_player_name | character | Name of the second assist. |
| goalie_id | integer | Licence number of the goaltender faced (shootout rows). |
| goalie_name | character | Name of the goaltender faced (shootout rows). |
| strength | character | First goal tag: "EQ" even strength, "PP1" power play, "SH1" shorthanded, ... |
| goal_tags | character | Every goal tag, comma-joined as SIHF prints them (e.g., "PP1,GWG"). |
| score_home | integer | Home score after the goal. |
| score_away | integer | Away score after the goal. |
| plus_player_ids | list | Licence numbers credited +1 on the goal (listed only where plus-minus applies). |
| minus_player_ids | list | Licence numbers charged -1 on the goal (listed only where plus-minus applies). |
| penalty_id | integer | SIHF penalty-type id. |
| penalty_name | character | Penalty name, German whatever the language (e.g., "Haken"). |
| penalty_minutes | integer | Penalty length in minutes. |
| penalty_start | character | Game time the penalty starts, "mm:ss". |
| penalty_end | character | Game time the penalty ended, "mm:ss" (earlier than its full length when a goal ended it). |
| goalie_change | character | Goaltender change direction: "in" or "out". |
| revoke_reason | character | Why the goal was disallowed, as SIHF prints it (e.g., "CC-Offiside"). |
| shootout_number | integer | 1-based shootout attempt number. |
| shootout_scored | logical | Whether the shootout attempt scored. |
| text | character | Event text exactly as SIHF prints it (markdown bold included). |

## Errors

- `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a
  requested season, league or phase and served another one.

- `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is
  invalid.

- `fastRhockey_asset_fetch`: any other non-2xx answer or a dead
  connection after retries, an empty or unreadable body, a stub or
  redirect body, a table without its filter echo, a truncated table, or
  a `gameoverview` body for another game.

## See also

[family_sihf](https://fastRhockey.sportsdataverse.org/reference/family_sihf.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(sihf_pbp(game_id = 20261105000325))
#> Error in .euro_check_status(resp) : 
#>   data.sihf.ch/statistic/api/cms/gameoverview answered HTTP 403:
#> {"Message":"Public access to this API has been discontinued. Write to
#> support@sihf.ch for access."}
# }
```
