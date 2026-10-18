# **EIHL Schedule**

Schedule and results of one competition season, one row per match,
played or not.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-matches/{season}/{league_id}.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-matches/2026/3.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_schedule(season, league_id = 3L, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026/27). The bucket keys seasons by
  their START year; the wrapper requests season - 1.

- league_id:

  Competition id. 3 = Elite League, 4 = Challenge Cup, 5 = playoffs.

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
| league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
| league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
| season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
| round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
| group | character | Group name in group stages; empty string or null otherwise. |
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
| arena | character | Arena name; empty string or null where not set. |

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
[`eihl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/eihl_pbp.md),
[`eihl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/eihl_player_box.md),
[`eihl_shots()`](https://fastRhockey.sportsdataverse.org/reference/eihl_shots.md),
[`eihl_standings()`](https://fastRhockey.sportsdataverse.org/reference/eihl_standings.md),
[`eihl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/eihl_team_player_stats.md),
[`eihl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/eihl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(eihl_schedule(season = 2027, league_id = 3))
#> ── EIHL schedule from the eSports hokejovyzapis bucket ─────────────────────────
#> ℹ Data updated: 2026-10-18 12:02:20 UTC
#> # A tibble: 270 × 32
#>    match_id start_date     league_id league season round group home_id home_name
#>       <int> <chr>              <int> <chr>   <int> <chr> <chr>   <int> <chr>    
#>  1     4401 2027-03-24 19…         3 EIHL     2027 249   ""         98 Manchest…
#>  2     4409 2026-09-20 16…         3 EIHL     2027 1     ""         96 Glasgow …
#>  3     4411 2026-09-20 17…         3 EIHL     2027 2     ""         94 Dundee S…
#>  4     4412 2026-09-23 19…         3 EIHL     2027 3     ""         97 Guildfor…
#>  5     4416 2026-09-26 19…         3 EIHL     2027 5     ""         99 Nottingh…
#>  6     4422 2026-09-30 19…         3 EIHL     2027 6     ""         96 Glasgow …
#>  7     4425 2026-10-03 19…         3 EIHL     2027 7     NA         92 Cardiff …
#>  8     4426 2026-10-03 18…         3 EIHL     2027 8     NA         97 Guildfor…
#>  9     4427 2026-10-03 19…         3 EIHL     2027 9     ""        100 Sheffiel…
#> 10     4428 2026-10-04 17…         3 EIHL     2027 10    NA         93 Coventry…
#> # ℹ 260 more rows
#> # ℹ 23 more variables: home_shortcut <chr>, guest_id <int>, guest_name <chr>,
#> #   guest_shortcut <chr>, status <chr>, overtime_count <int>, extra_time <lgl>,
#> #   shooting <lgl>, contumacy <lgl>, final_score_home <int>,
#> #   final_score_guest <int>, first_period_score_home <int>,
#> #   first_period_score_guest <int>, second_period_score_home <int>,
#> #   second_period_score_guest <int>, third_period_score_home <int>, …
# }
```
