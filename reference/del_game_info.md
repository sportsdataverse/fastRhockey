# **DEL Game Info**

Game info of one match: state, arena, attendance, officials, coaches,
period scores.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/game-header.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/game-header.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_game_info(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in del_schedule). Not
  the league website's game id.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON body (a nested list).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per row of the file (zero rows, carrying the documented
columns, for an empty frame); the raw JSON list when
`return_parsed = FALSE`. Columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| actual_time_name | character | Display label of the game state, e.g. `end` / `Ende`. |
| actual_time_alias | character | Short game-state code: `K` = finished. |
| stadium | character | Name of the arena the game was played in. |
| number_of_viewers | integer | Spectators at the game (attendance). |
| referees_head_referee1_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_head_referee1_name | character | Official's full name. |
| referees_head_referee2_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_head_referee2_name | character | Official's full name. |
| referees_line_referee1_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_line_referee1_name | character | Official's full name. |
| referees_line_referee2_id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_line_referee2_name | character | Official's full name. |
| team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_info_home_name | character | Team display name. |
| team_info_home_shortcut | character | Three-letter team code. |
| team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_info_visitor_name | character | Team display name. |
| team_info_visitor_shortcut | character | Three-letter team code. |
| trainers_visitor_head_coach_id | integer | Coach person id, its own key space; not a player id; no other captured file joins to it. |
| trainers_visitor_head_coach_name | character | Head coach full name. |
| trainers_home_head_coach_id | integer | Coach person id, its own key space; not a player id; no other captured file joins to it. |
| trainers_home_head_coach_name | character | Head coach full name. |
| team_bilance_home_win | integer | Wins of that team in the head-to-head record (unverified). |
| team_bilance_visitor_win | integer | Wins of that team in the head-to-head record (unverified). |
| results_extra_time | logical | True when the game went to overtime. |
| results_shooting | logical | True when the game was decided by a shootout. |
| results_contumacy | logical | True when the result was awarded by forfeit. |
| results_score_final_score_home | integer | Home team goals in that span. |
| results_score_final_score_guest | integer | Visiting team goals in that span. |
| results_score_first_period_score_home | integer | Home team goals in that span. |
| results_score_first_period_score_guest | integer | Visiting team goals in that span. |
| results_score_second_period_score_home | integer | Home team goals in that span. |
| results_score_second_period_score_guest | integer | Visiting team goals in that span. |
| results_score_third_period_score_home | integer | Home team goals in that span. |
| results_score_third_period_score_guest | integer | Visiting team goals in that span. |
| best_players_home_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| best_players_home_name | character | Player full name. |
| best_players_visitor_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| best_players_visitor_name | character | Player full name. |
| overtime_count | integer | Number of overtime periods; null in the samples. |
| last_event_time | integer | Game second of the latest event (3600 = end of regulation). |

## Errors

Signalled as classed conditions: `fastRhockey_no_data` when the bucket
has no such key (S3 404 NoSuchKey: unknown season, competition, match or
team) or the file holds no rows (an unplayed game, an empty shot shell);
`fastRhockey_asset_fetch` for any other non-2xx answer (403
AccessDenied), a connection failure after retries, an empty or
unreadable 200 body, an error envelope, a body of the wrong shape, or a
body for another season or game; `fastRhockey_bad_request` for an HTTP
400/422 or an invalid argument. All three also inherit
`fastRhockey_error`. Wrappers raise; they never warn and return an empty
frame.

## See also

[DEL](https://www.penny-del.org/) (the league site whose widgets read
this public eSports bucket)

Other DEL eSports functions:
[`del_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/del_faceoffs.md),
[`del_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/del_goalie_box.md),
[`del_pbp()`](https://fastRhockey.sportsdataverse.org/reference/del_pbp.md),
[`del_player_box()`](https://fastRhockey.sportsdataverse.org/reference/del_player_box.md),
[`del_roster()`](https://fastRhockey.sportsdataverse.org/reference/del_roster.md),
[`del_schedule()`](https://fastRhockey.sportsdataverse.org/reference/del_schedule.md),
[`del_shifts()`](https://fastRhockey.sportsdataverse.org/reference/del_shifts.md),
[`del_shots()`](https://fastRhockey.sportsdataverse.org/reference/del_shots.md),
[`del_standings()`](https://fastRhockey.sportsdataverse.org/reference/del_standings.md),
[`del_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/del_team_player_stats.md),
[`del_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/del_top_scorers.md)

## Examples

``` r
# \donttest{
  try(del_game_info(match_id = 4444))
#> ── DEL game_info from the eSports hokejovyzapis bucket ─────────────────────────
#> ℹ Data updated: 2026-10-18 05:42:55 UTC
#> # A tibble: 1 × 41
#>   actual_time_name actual_time_alias stadium      number_of_viewers
#>   <chr>            <chr>             <chr>                    <int>
#> 1 Ende             K                 Helios Arena              4292
#> # ℹ 37 more variables: referees_head_referee1_id <int>,
#> #   referees_head_referee1_name <chr>, referees_head_referee2_id <int>,
#> #   referees_head_referee2_name <chr>, referees_line_referee1_id <int>,
#> #   referees_line_referee1_name <chr>, referees_line_referee2_id <int>,
#> #   referees_line_referee2_name <chr>, team_info_home_id <int>,
#> #   team_info_home_name <chr>, team_info_home_shortcut <chr>,
#> #   team_info_visitor_id <int>, team_info_visitor_name <chr>, …
# }
```
