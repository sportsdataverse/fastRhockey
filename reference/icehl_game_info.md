# **ICE Hockey League Game Info**

Game info of one match: state, arena, attendance, officials, coaches,
period scores.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/game-header.json`
of the ICE Hockey League eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/game-header.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
icehl_game_info(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in icehl_schedule). Not
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
| referees_head_referee1number | character | Official's sweater number; null when not recorded. |
| referees_head_referee2number | character | Official's sweater number; null when not recorded. |
| referees_line_referee1number | character | Official's sweater number; null when not recorded. |
| referees_line_referee2number | character | Official's sweater number; null when not recorded. |
| team_info_home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_info_home_name | character | Team display name. |
| team_info_home_shortcut | character | Three-letter team code. |
| team_info_visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_info_visitor_name | character | Team display name. |
| team_info_visitor_shortcut | character | Three-letter team code. |
| trainers_visitor_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
| trainers_home_head_coach | character | Head coach (ICEHL/EIHL/Denmark: a full-name string; DEL: an object with `id` and `name`). |
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

[ICE Hockey League](https://www.ice.hockey/) (the league site whose
widgets read this public eSports bucket)

Other ICE Hockey League eSports functions:
[`icehl_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/icehl_faceoffs.md),
[`icehl_game_center()`](https://fastRhockey.sportsdataverse.org/reference/icehl_game_center.md),
[`icehl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/icehl_goalie_box.md),
[`icehl_league_part_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/icehl_league_part_player_stats.md),
[`icehl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/icehl_pbp.md),
[`icehl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/icehl_player_box.md),
[`icehl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/icehl_schedule.md),
[`icehl_shots()`](https://fastRhockey.sportsdataverse.org/reference/icehl_shots.md),
[`icehl_standings()`](https://fastRhockey.sportsdataverse.org/reference/icehl_standings.md),
[`icehl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/icehl_team_player_stats.md),
[`icehl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/icehl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(icehl_game_info(match_id = 8161))
#> ── ICE Hockey League game_info from the eSports hokejovyzapis bucket ───────────
#> ℹ Data updated: 2026-10-18 12:05:45 UTC
#> # A tibble: 1 × 34
#>   actual_time_name actual_time_alias status      stadium      number_of_viewers
#>   <chr>            <chr>             <chr>       <chr>                    <int>
#> 1 end              K                 AFTER_MATCH Tüskecsarnok              1268
#> # ℹ 29 more variables: referees_head_referee1 <chr>,
#> #   referees_head_referee2 <chr>, referees_line_referee1 <chr>,
#> #   referees_line_referee2 <chr>, referees_head_referee1number <chr>,
#> #   referees_head_referee2number <chr>, referees_line_referee1number <chr>,
#> #   referees_line_referee2number <chr>, team_info_home_id <int>,
#> #   team_info_home_name <chr>, team_info_home_shortcut <chr>,
#> #   team_info_visitor_id <int>, team_info_visitor_name <chr>, …
# }
```
