# **Metal Ligaen Schedule**

Schedule and results of one competition season, one row per match,
played or not.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-matches/{season}/{league_id}.json`
of the Metal Ligaen eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-matches/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
metal_schedule(season, league_id = 1L, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026/27). The bucket keys seasons by
  their START year; the wrapper requests season - 1.

- league_id:

  Competition id. 1 = Metal Ligaen, regular season and playoffs in one
  file (split them by league_part_name); 2 = pre-season, 4 = Final-4
  cup.

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
| real_start_date | character | Actual start, local `YYYY-MM-DD HH:MM:SS`; `0000-00-00 00:00:00` placeholder before the game. |
| real_end_date | character | Actual end, local `YYYY-MM-DD HH:MM:SS`. |
| league_id | integer | League id (per-bucket competition key space); joins to the `{league_id}` path param. |
| league | character | League display name, e.g. `DEL`, `EIHL`, `Metal Ligaen`. |
| league_part_id | integer | League part id (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
| league_part_name | character | League part name, e.g. `ICEHL 2026/27` or `... - Playoffs`; splits regular season from playoffs where one league id holds both. |
| season | integer | Season by its END year (2027 = 2026/27); the body's START-year key plus one. |
| round | character | Round number as a string (`"1"`, `"249"`); DEL values carry a leading U+FEFF byte-order mark, empty string in Denmark. |
| group | character | Group name in group stages; empty string or null otherwise. |
| tickets_url | character | URL of the ticket shop for the match. |
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
| advanced_statistics | logical | True when the game has advanced stats files (shots, boxscore). |

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
[`metal_game_info()`](https://fastRhockey.sportsdataverse.org/reference/metal_game_info.md),
[`metal_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_goalie_box.md),
[`metal_pbp()`](https://fastRhockey.sportsdataverse.org/reference/metal_pbp.md),
[`metal_player_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_player_box.md),
[`metal_standings()`](https://fastRhockey.sportsdataverse.org/reference/metal_standings.md),
[`metal_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/metal_top_scorers.md)

## Examples

``` r
# \donttest{
  try(metal_schedule(season = 2027, league_id = 1))
#> ── Metal Ligaen schedule from the eSports hokejovyzapis bucket ─────────────────
#> ℹ Data updated: 2026-10-13 01:57:01 UTC
#> # A tibble: 216 × 40
#>    match_id start_date          real_start_date   real_end_date league_id league
#>       <int> <chr>               <chr>             <chr>             <int> <chr> 
#>  1      528 2026-08-28 19:00:00 0000-00-00 00:00… 2026-08-28 2…         1 Metal…
#>  2      529 2026-08-28 19:30:00 0000-00-00 00:00… 2026-08-28 2…         1 Metal…
#>  3      530 2026-08-28 19:30:00 0000-00-00 00:00… 2026-08-28 2…         1 Metal…
#>  4      531 2026-08-30 15:00:00 0000-00-00 00:00… 2026-08-30 1…         1 Metal…
#>  5      532 2026-08-30 15:00:00 0000-00-00 00:00… 2026-08-30 1…         1 Metal…
#>  6      533 2026-09-01 20:00:00 0000-00-00 00:00… 2026-09-01 2…         1 Metal…
#>  7      534 2026-09-01 19:30:00 0000-00-00 00:00… 2026-09-01 2…         1 Metal…
#>  8      535 2026-09-02 18:30:00 0000-00-00 00:00… 2026-09-02 2…         1 Metal…
#>  9      536 2026-09-07 19:00:00 0000-00-00 00:00… 2026-09-07 2…         1 Metal…
#> 10      537 2026-09-08 19:00:00 0000-00-00 00:00… 2026-09-08 2…         1 Metal…
#> # ℹ 206 more rows
#> # ℹ 34 more variables: league_part_id <int>, league_part_name <chr>,
#> #   season <int>, round <chr>, group <chr>, tickets_url <chr>, home_id <int>,
#> #   home_name <chr>, home_shortcut <chr>, guest_id <int>, guest_name <chr>,
#> #   guest_shortcut <chr>, status <chr>, overtime_count <int>, extra_time <lgl>,
#> #   shooting <lgl>, contumacy <lgl>, final_score_home <int>,
#> #   final_score_guest <int>, first_period_score_home <int>, …
# }
```
