# **DEL Shifts**

Every shift of one match, one row per player shift.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/shiftsSC.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/shiftsSC.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_shifts(match_id, return_parsed = TRUE)
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
| shift_id | integer | Shift id within the shift files. |
| start_time | integer | Shift start, game second. |
| start_real_time | character | Shift start, local wall clock. |
| end_time | integer | Shift end, game second. |
| end_real_time | character | Shift end, local wall clock. |
| player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| player_name | character | Player full name. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| duration | integer | Shift length in seconds (end_time - start_time). |

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
[`del_game_info()`](https://fastRhockey.sportsdataverse.org/reference/del_game_info.md),
[`del_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/del_goalie_box.md),
[`del_pbp()`](https://fastRhockey.sportsdataverse.org/reference/del_pbp.md),
[`del_player_box()`](https://fastRhockey.sportsdataverse.org/reference/del_player_box.md),
[`del_roster()`](https://fastRhockey.sportsdataverse.org/reference/del_roster.md),
[`del_schedule()`](https://fastRhockey.sportsdataverse.org/reference/del_schedule.md),
[`del_shots()`](https://fastRhockey.sportsdataverse.org/reference/del_shots.md),
[`del_standings()`](https://fastRhockey.sportsdataverse.org/reference/del_standings.md),
[`del_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/del_team_player_stats.md),
[`del_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/del_top_scorers.md)

## Examples

``` r
# \donttest{
  try(del_shifts(match_id = 4444))
#> ── DEL shifts from the eSports hokejovyzapis bucket ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:02:06 UTC
#> # A tibble: 766 × 10
#>    shift_id start_time start_real_time     end_time end_real_time      player_id
#>       <int>      <int> <chr>                  <int> <chr>                  <int>
#>  1 11415946          0 2026-10-11 14:30:08       60 2026-10-11 14:31:…      1382
#>  2 11415947          0 2026-10-11 14:30:08       62 2026-10-11 14:31:…      1435
#>  3 11415948          0 2026-10-11 14:30:08       35 2026-10-11 14:31:…      2835
#>  4 11415949          0 2026-10-11 14:30:08       32 2026-10-11 14:31:…      3560
#>  5 11415950          0 2026-10-11 14:30:08       52 2026-10-11 14:31:…      4337
#>  6 11415951          0 2026-10-11 14:30:08       60 2026-10-11 14:31:…      3275
#>  7 11415952          0 2026-10-11 14:30:08       75 2026-10-11 14:32:…      3426
#>  8 11415953          0 2026-10-11 14:30:08       59 2026-10-11 14:31:…      3557
#>  9 11415954          0 2026-10-11 14:30:08       71 2026-10-11 14:32:…      4328
#> 10 11415955          0 2026-10-11 14:30:08       59 2026-10-11 14:31:…       550
#> # ℹ 756 more rows
#> # ℹ 4 more variables: player_name <chr>, team_id <int>, team_name <chr>,
#> #   duration <int>
# }
```
