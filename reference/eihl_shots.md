# **EIHL Shots**

Shots of one match with rink coordinates and result, one row per shot.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/visualization/shots/{match_id}.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/visualization/shots/4440.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_shots(match_id, return_parsed = TRUE)
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
| match_id | integer | Match id (per-bucket match key space); joins to the `{match_id}` path param of the game wrappers. |
| date | character | Game date `YYYY-MM-DD`. |
| home_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| home_name | character | Team display name. |
| home_shortcut | character | Three-letter team code. |
| visitor_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| visitor_name | character | Team display name. |
| visitor_shortcut | character | Three-letter team code. |
| status | character | Match status: `AFTER_MATCH`, `LIVE`, `BEFORE_MATCH`. |
| shot_id | integer | Shot id within the shot files. |
| player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| jersey | integer | Jersey number. |
| first_name | character | First name of the shooter. |
| last_name | character | Last name of the shooter. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| match_shot_resutl_id | integer | Shot result code (the provider's typo kept): `4` = goal; `1` / `2` / `3` probably on goal / missed / blocked (unverified). A code, not a joinable id. |
| time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
| coordinate_x | numeric | Shot x along the rink LENGTH, centre ice = 0, about -100..100 (units unverified, plausibly percent of half-length). Normalised to the attacking direction: each team keeps one sign for the whole game, the home team positive in the three sample games. DEL sends two-decimal floats, ICEHL and EIHL whole numbers (`-71.0`). |
| coordinate_y | numeric | Shot y across the rink WIDTH, centre line = 0, about -100..100 (units and which board is positive unverified). DEL two-decimal floats, ICEHL and EIHL whole numbers. |
| real_date | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
| xg | numeric | Expected-goals value of the shot (ICEHL and EIHL only; null elsewhere). |
| xg_type | character | Shot-danger class from the xG model, e.g. LD / MD / HD (ICEHL and EIHL only; null elsewhere). |
| shot_result_id | integer | Shot result code, the same value as match_shot_resutl_id: 4 = goal (verified); 1, 2 and 3 (on goal / missed / blocked in the widgets) are unverified. |
| is_goal | logical | True when the shot result code is 4 (goal). |
| team_side | character | Team of the row: home or visitor. |

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
[`eihl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/eihl_schedule.md),
[`eihl_standings()`](https://fastRhockey.sportsdataverse.org/reference/eihl_standings.md),
[`eihl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/eihl_team_player_stats.md),
[`eihl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/eihl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(eihl_shots(match_id = 4440))
#> ── EIHL shots from the eSports hokejovyzapis bucket ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:08:21 UTC
#> # A tibble: 114 × 25
#>    match_id date       home_id home_name   home_shortcut visitor_id visitor_name
#>       <int> <chr>        <int> <chr>       <chr>              <int> <chr>       
#>  1     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  2     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  3     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  4     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  5     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  6     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  7     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  8     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#>  9     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#> 10     4440 2026-10-11      96 Glasgow Cl… GLA                   97 Guildford F…
#> # ℹ 104 more rows
#> # ℹ 18 more variables: visitor_shortcut <chr>, status <chr>, shot_id <int>,
#> #   player_id <int>, jersey <int>, first_name <chr>, last_name <chr>,
#> #   team_id <int>, match_shot_resutl_id <int>, time <int>, coordinate_x <dbl>,
#> #   coordinate_y <dbl>, real_date <chr>, xg <dbl>, xg_type <chr>,
#> #   shot_result_id <int>, is_goal <lgl>, team_side <chr>
# }
```
