# **Metal Ligaen Game Info**

Game info of one match: state, arena, attendance, officials, coaches,
period scores.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/game-header.json`
of the Metal Ligaen eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/game-header.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
metal_game_info(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in metal_schedule). Not
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
| status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
| stadium | character | Name of the arena the game was played in. |
| number_of_viewers | integer | Spectators at the game (attendance). |
| referees_head_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
| referees_head_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
| referees_line_referee1 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
| referees_line_referee2 | character | Official's name (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
| referees_head_referee1number | integer | Official's sweater number; null when not recorded. |
| referees_head_referee2number | integer | Official's sweater number; null when not recorded. |
| referees_line_referee1number | integer | Official's sweater number; null when not recorded. |
| referees_line_referee2number | integer | Official's sweater number; null when not recorded. |
| referees_head_referee1id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_head_referee2id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_line_referee1id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| referees_line_referee2id | integer | Official (referee/linesman) person id, its own key space; not a player id; no other captured file joins to it. |
| team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_info_home_name | character | Team display name. |
| team_info_home_shortcut | character | Three-letter team code. |
| team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_info_visitor_name | character | Team display name. |
| team_info_visitor_shortcut | character | Three-letter team code. |
| trainers_home_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
| trainers_visitor_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
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

[Metal Ligaen](https://metalligaen.dk/) (the league site whose widgets
read this public eSports bucket)

Other Metal Ligaen eSports functions:
[`metal_game_center()`](https://fastRhockey.sportsdataverse.org/reference/metal_game_center.md),
[`metal_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_goalie_box.md),
[`metal_pbp()`](https://fastRhockey.sportsdataverse.org/reference/metal_pbp.md),
[`metal_player_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_player_box.md),
[`metal_schedule()`](https://fastRhockey.sportsdataverse.org/reference/metal_schedule.md),
[`metal_standings()`](https://fastRhockey.sportsdataverse.org/reference/metal_standings.md),
[`metal_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/metal_top_scorers.md)

## Examples

``` r
# \donttest{
  try(metal_game_info(match_id = 575))
#> ── Metal Ligaen game_info from the eSports hokejovyzapis bucket ────────────────
#> ℹ Data updated: 2026-10-30 00:17:18 UTC
#> # A tibble: 1 × 38
#>   actual_time_name actual_time_alias status      stadium number_of_viewers
#>   <chr>            <chr>             <chr>       <chr>               <int>
#> 1 end              K                 AFTER_MATCH ""                    916
#> # ℹ 33 more variables: referees_head_referee1 <chr>,
#> #   referees_head_referee2 <chr>, referees_line_referee1 <chr>,
#> #   referees_line_referee2 <chr>, referees_head_referee1number <int>,
#> #   referees_head_referee2number <int>, referees_line_referee1number <int>,
#> #   referees_line_referee2number <int>, referees_head_referee1id <int>,
#> #   referees_head_referee2id <int>, referees_line_referee1id <int>,
#> #   referees_line_referee2id <int>, team_info_home_id <int>, …
# }
```
