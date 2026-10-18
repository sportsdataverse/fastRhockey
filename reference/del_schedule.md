# **DEL Schedule**

Schedule and results of one competition season, one row per match,
played or not.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-matches/{season}/{league_id}.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-matches/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_schedule(season, league_id = 1L, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026/27). The bucket keys seasons by
  their START year; the wrapper requests season - 1.

- league_id:

  Competition id. 1 = DEL regular season, 3 = DEL playoffs (also 2 =
  Test-Liga pre-season, 4 = MagentaSport Cup 2020/21, 5 = NHL Global
  Series 2022/23).

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
| match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
| start_date | character | Scheduled face-off, local `YYYY-MM-DD HH:MM:SS`; DEL playoff placeholders read `0000-00-00 ...`. |
| real_end_date | character | Actual end, local `YYYY-MM-DD HH:MM:SS`. |
| league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
| league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
| season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
| round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
| home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| home_name | character | Team display name. |
| home_shortcut | character | Three-letter team code. |
| guest_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| guest_name | character | Team display name. |
| guest_shortcut | character | Three-letter team code. |
| status | character | Match status: `BEFORE_MATCH`, `LIVE`, `AFTER_MATCH`. |
| overtime_count | integer | Number of overtime periods played; null in the samples. |
| extra_time | logical | True when the game went to overtime. |
| shooting | logical | True when the game was decided by a shootout. |
| contumacy | logical | True when the result was awarded by forfeit. |
| final_score_home | integer | Home team goals in that span. |
| final_score_guest | integer | Visiting team goals in that span. |
| first_period_score_home | integer | Home team goals in that span. |
| first_period_score_guest | integer | Visiting team goals in that span. |
| second_period_score_home | integer | Home team goals in that span. |
| second_period_score_guest | integer | Visiting team goals in that span. |
| third_period_score_home | integer | Home team goals in that span. |
| third_period_score_guest | integer | Visiting team goals in that span. |
| actual_time_name | character | Display label of the game state in the bucket's language, e.g. `end`, `Ende`, `before game`. |
| actual_time_alias | character | Short game-state code: `K` = finished (Czech `konec`), `0` = not started. |
| last_event_time | integer | Game second of the latest event, of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
| overtime_score_home | integer | Home team goals in overtime (present once a game of the season went to overtime). |
| overtime_score_guest | integer | Visiting team goals in overtime (present once a game of the season went to overtime). |
| shootout_score_home | integer | Home team shootout goals (present once a game of the season went to a shootout). |
| shootout_score_guest | integer | Visiting team shootout goals (present once a game of the season went to a shootout). |

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
[`del_shifts()`](https://fastRhockey.sportsdataverse.org/reference/del_shifts.md),
[`del_shots()`](https://fastRhockey.sportsdataverse.org/reference/del_shots.md),
[`del_standings()`](https://fastRhockey.sportsdataverse.org/reference/del_standings.md),
[`del_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/del_team_player_stats.md),
[`del_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/del_top_scorers.md)

## Examples

``` r
# \donttest{
  try(del_schedule(season = 2027, league_id = 1))
#> ── DEL schedule from the eSports hokejovyzapis bucket ──────────────────────────
#> ℹ Data updated: 2026-10-18 05:43:00 UTC
#> # A tibble: 364 × 33
#>    match_id start_date       real_end_date league_id league season round home_id
#>       <int> <chr>            <chr>             <int> <chr>   <int> <chr>   <int>
#>  1     4389 2026-09-17 19:3… 2026-09-17 2…         1 DEL      2027 1           3
#>  2     4390 2026-09-18 19:3… 2026-09-18 2…         1 DEL      2027 1          11
#>  3     4391 2026-09-18 19:3… 2026-09-18 2…         1 DEL      2027 1          14
#>  4     4392 2026-09-18 19:3… 2026-09-18 2…         1 DEL      2027 1           5
#>  5     4393 2026-09-18 19:3… 2026-09-18 2…         1 DEL      2027 1           7
#>  6     4394 2026-09-18 19:3… 2026-09-18 2…         1 DEL      2027 1          12
#>  7     4395 2026-09-18 19:3… 2026-09-18 2…         1 DEL      2027 1          15
#>  8     4396 2026-09-20 14:0… 2026-09-20 1…         1 DEL      2027 2           2
#>  9     4397 2026-09-20 16:3… 2026-09-20 1…         1 DEL      2027 2          13
#> 10     4398 2026-09-20 14:0… 2026-09-20 1…         1 DEL      2027 2           9
#> # ℹ 354 more rows
#> # ℹ 25 more variables: home_name <chr>, home_shortcut <chr>, guest_id <int>,
#> #   guest_name <chr>, guest_shortcut <chr>, status <chr>, overtime_count <int>,
#> #   extra_time <lgl>, shooting <lgl>, contumacy <lgl>, final_score_home <int>,
#> #   final_score_guest <int>, first_period_score_home <int>,
#> #   first_period_score_guest <int>, second_period_score_home <int>,
#> #   second_period_score_guest <int>, third_period_score_home <int>, …
# }
```
