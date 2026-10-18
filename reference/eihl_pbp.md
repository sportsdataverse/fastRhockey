# **EIHL Pbp**

Play-by-play of one match: goals, penalties, goalie changes and
timeouts, one row per event.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/{match_id}/period-events.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/4440/period-events.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_pbp(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in eihl_schedule). Not
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
| event_index | integer | Row order of the event within the game (0-based), across periods. |
| period | character | Period key as the body sends it: 1, 2, 3, OT (overtime) or SO (shootout). |
| period_number | integer | Period number: 1-3, 4 = overtime, 5 = shootout. |
| time | integer | Game second of the event from the opening face-off (0-3600 in regulation, continuing through overtime). |
| event_type | character | Event type: goal, penalty, goalkeeperChange, timeout, periodStart, periodEnd, videoReview, ... |
| event_id | integer | Event id within its event type. |
| team_side | character | Team of the row: home or visitor. |
| goalie_player_id | integer | Player id (playerId key space) of the goalie entering the game (goalkeeperChange). |
| goalie_jersey | integer | Jersey number of the goalie entering the game (goalkeeperChange). |
| goalie_name | character | Full name of the goalie entering the game (goalkeeperChange). |
| current_score | character | Score after the goal, as `home:visitor` text, e.g. `1:0`. |
| real_time | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
| scoreboard_time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
| balance | character | Goal strength: `EQ` even, `PP1` / `PP2` power play, `SH1` / `SH2` shorthanded. |
| extra_attacker | logical | True when the scoring team had an extra attacker on (goalie pulled). |
| empty_net | logical | True when the goal went into an empty net. |
| shooting | logical | True for a goal scored on a penalty shot. |
| technical | logical | True for a technical penalty (unverified). |
| scorer_player_id | integer | Player id (playerId key space) of the goal scorer. |
| scorer_jersey | integer | Jersey number of the goal scorer. |
| scorer_name | character | Full name of the goal scorer. |
| assist1_player_id | integer | Player id (playerId key space) of the first assist. |
| assist1_jersey | integer | Jersey number of the first assist. |
| assist1_name | character | Full name of the first assist. |
| assist2_player_id | integer | Player id (playerId key space) of the second assist. |
| assist2_jersey | integer | Jersey number of the second assist. |
| assist2_name | character | Full name of the second assist. |
| plus_player_ids | character | Comma-separated player ids (playerId key space) of the scoring team's players on ice. |
| minus_player_ids | character | Comma-separated player ids (playerId key space) of the conceding team's players on ice. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_shortcut | character | Three-letter team code. |
| goal_review | character | Video review subject, e.g. a goal (EIHL `videoReview` events). |
| on_ice_decision | character | Call on the ice before the review. |
| reason_for_review | character | Why the play was reviewed (free text). |
| decision_after_review | character | Final call after the review. |
| outgoing_goalie_player_id | integer | Player id (playerId key space) of the goalie leaving the game (goalkeeperChange). |
| outgoing_goalie_jersey | integer | Jersey number of the goalie leaving the game (goalkeeperChange). |
| outgoing_goalie_name | character | Full name of the goalie leaving the game (goalkeeperChange). |
| create_time | integer | Game second at which the event was entered (unverified). |
| penalized_player_id | integer | Player id (playerId key space) of the penalized player. |
| penalized_jersey | integer | Jersey number of the penalized player. |
| penalized_name | character | Full name of the penalized player. |
| penalty_from_real_time | character | Penalty start, local wall clock YYYY-MM-DD HH:MM:SS. |
| penalty_from_scoreboard_time | integer | Penalty start, game second. |
| penalty_to_real_time | character | Penalty end, local wall clock YYYY-MM-DD HH:MM:SS. |
| penalty_to_scoreboard_time | integer | Penalty end, game second. |
| duration | integer | Penalty length in seconds (120 = minor). |
| disciplined_functionary | logical | True when the penalty was assessed to a team official. |
| disciplined_bm | logical | True when the penalty was a bench minor. |
| codename | character | Penalty code, e.g. `HOOK`, `TRIP`, `ROUGH`, `BOARD`, `DELAY`. |
| replaced_player_id | integer | Player id (playerId key space) of the player serving the penalty in the penalized player's place. |
| replaced_jersey | integer | Jersey number of the player serving the penalty in the penalized player's place. |
| replaced_name | character | Full name of the player serving the penalty in the penalized player's place. |

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

[EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets
read this public eSports bucket)

Other EIHL eSports functions:
[`eihl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/eihl_game_info.md),
[`eihl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/eihl_goalie_box.md),
[`eihl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/eihl_player_box.md),
[`eihl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/eihl_schedule.md),
[`eihl_shots()`](https://fastRhockey.sportsdataverse.org/reference/eihl_shots.md),
[`eihl_standings()`](https://fastRhockey.sportsdataverse.org/reference/eihl_standings.md),
[`eihl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/eihl_team_player_stats.md),
[`eihl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/eihl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(eihl_pbp(match_id = 4440))
#> ── EIHL pbp from the eSports hokejovyzapis bucket ──── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:09:59 UTC
#> # A tibble: 16 × 53
#>    event_index period period_number  time event_type       event_id team_side
#>          <int> <chr>          <int> <int> <chr>               <int> <chr>    
#>  1           0 1                  1     0 goalkeeperChange    21568 home     
#>  2           1 1                  1     0 goalkeeperChange    21569 visitor  
#>  3           2 1                  1   367 goal                17919 home     
#>  4           3 1                  1   427 goal                17921 home     
#>  5           4 2                  2  1382 goal                17924 visitor  
#>  6           5 2                  2  1479 goal                17925 visitor  
#>  7           6 3                  3  2551 goal                17938 visitor  
#>  8           7 3                  3  2919 goal                17939 visitor  
#>  9           8 3                  3  3148 videoReview           330 home     
#> 10           9 3                  3  3404 goalkeeperChange    21592 home     
#> 11          10 3                  3  3458 goal                17941 visitor  
#> 12          11 3                  3  3458 goalkeeperChange    21593 home     
#> 13          12 3                  3  3478 goalkeeperChange    21594 home     
#> 14          13 3                  3  3518 timeout              1664 home     
#> 15          14 3                  3  3537 goal                17944 visitor  
#> 16          15 3                  3  3537 goalkeeperChange    21596 home     
#> # ℹ 46 more variables: goalie_player_id <int>, goalie_jersey <int>,
#> #   goalie_name <chr>, current_score <chr>, real_time <chr>,
#> #   scoreboard_time <int>, balance <chr>, extra_attacker <lgl>,
#> #   empty_net <lgl>, shooting <lgl>, technical <lgl>, scorer_player_id <int>,
#> #   scorer_jersey <int>, scorer_name <chr>, assist1_player_id <int>,
#> #   assist1_jersey <int>, assist1_name <chr>, assist2_player_id <int>,
#> #   assist2_jersey <int>, assist2_name <chr>, plus_player_ids <chr>, …
# }
```
