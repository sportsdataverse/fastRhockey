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

Other SIHF Functions:
[`sihf_game_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_goalie_stats.md),
[`sihf_game_info()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_info.md),
[`sihf_game_lineups()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_lineups.md),
[`sihf_game_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_player_stats.md),
[`sihf_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_game_team_stats.md),
[`sihf_goalie_shootouts()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_shootouts.md),
[`sihf_goalie_shots()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_shots.md),
[`sihf_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_stats.md),
[`sihf_goalie_wins()`](https://fastRhockey.sportsdataverse.org/reference/sihf_goalie_wins.md),
[`sihf_phases()`](https://fastRhockey.sportsdataverse.org/reference/sihf_phases.md),
[`sihf_player_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_faceoffs.md),
[`sihf_player_faceoffs_by_zone()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_faceoffs_by_zone.md),
[`sihf_player_faceoffs_by_zone_per_game()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_faceoffs_by_zone_per_game.md),
[`sihf_player_goals_assists()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_goals_assists.md),
[`sihf_player_penalties()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_penalties.md),
[`sihf_player_shootouts()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_shootouts.md),
[`sihf_player_shots()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_shots.md),
[`sihf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_stats.md),
[`sihf_player_time_on_ice()`](https://fastRhockey.sportsdataverse.org/reference/sihf_player_time_on_ice.md),
[`sihf_results()`](https://fastRhockey.sportsdataverse.org/reference/sihf_results.md),
[`sihf_standings()`](https://fastRhockey.sportsdataverse.org/reference/sihf_standings.md),
[`sihf_team_attendance()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_attendance.md),
[`sihf_team_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_faceoffs.md),
[`sihf_team_faceoffs_by_zone()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_faceoffs_by_zone.md),
[`sihf_team_faceoffs_by_zone_per_game()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_faceoffs_by_zone_per_game.md),
[`sihf_team_goals()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_goals.md),
[`sihf_team_goals_by_position()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_goals_by_position.md),
[`sihf_team_penalties()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_penalties.md),
[`sihf_team_penalty_kill()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_penalty_kill.md),
[`sihf_team_power_play()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_power_play.md),
[`sihf_team_shootouts()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_shootouts.md),
[`sihf_team_shots()`](https://fastRhockey.sportsdataverse.org/reference/sihf_team_shots.md)

## Examples

``` r
# \donttest{
  try(sihf_pbp(game_id = 20261105000325))
#> ── SIHF PBP Information from data.sihf.ch ──────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:12:01 UTC
#> # A tibble: 17 × 34
#>    game_id event_index period_number period     event_type   time  game_seconds
#>      <dbl>       <int>         <int> <chr>      <chr>        <chr>        <int>
#>  1 2.03e13           1             1 1. Drittel goal         08:22          502
#>  2 2.03e13           2             1 1. Drittel penalty      13:45          825
#>  3 2.03e13           3             2 2. Drittel goal         22:27         1347
#>  4 2.03e13           4             2 2. Drittel goal         22:54         1374
#>  5 2.03e13           5             2 2. Drittel goal         24:13         1453
#>  6 2.03e13           6             2 2. Drittel penalty      25:58         1558
#>  7 2.03e13           7             2 2. Drittel goal         26:47         1607
#>  8 2.03e13           8             2 2. Drittel penalty      29:58         1798
#>  9 2.03e13           9             2 2. Drittel goalie       36:24         2184
#> 10 2.03e13          10             2 2. Drittel goalie       36:24         2184
#> 11 2.03e13          11             3 3. Drittel goal         44:08         2648
#> 12 2.03e13          12             3 3. Drittel penalty      45:26         2726
#> 13 2.03e13          13             3 3. Drittel goal         46:23         2783
#> 14 2.03e13          14             3 3. Drittel penalty      54:03         3243
#> 15 2.03e13          15             3 3. Drittel goalie       57:57         3477
#> 16 2.03e13          16             3 3. Drittel penalty      59:19         3559
#> 17 2.03e13          17             3 3. Drittel revoked_goal 59:22         3562
#> # ℹ 27 more variables: team_id <int>, team_side <chr>, player_id <int>,
#> #   player_name <chr>, player_jersey <int>, assist1_player_id <int>,
#> #   assist1_player_name <chr>, assist2_player_id <int>,
#> #   assist2_player_name <chr>, goalie_id <int>, goalie_name <chr>,
#> #   strength <chr>, goal_tags <chr>, score_home <int>, score_away <int>,
#> #   plus_player_ids <list>, minus_player_ids <list>, penalty_id <int>,
#> #   penalty_name <chr>, penalty_minutes <int>, penalty_start <chr>, …
# }
```
