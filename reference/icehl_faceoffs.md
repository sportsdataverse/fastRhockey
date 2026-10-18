# **ICE Hockey League Faceoffs**

Every faceoff of one match: time, dot, winner and loser.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/{match_id}/faceoffs.json`
of the ICE Hockey League eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/matches/8161/faceoffs.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
icehl_faceoffs(match_id, return_parsed = TRUE)
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
| id | integer | Face-off id (face-off key space); no other captured file joins to it. |
| time | integer | Seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). |
| realtime | character | Wall-clock local time, `YYYY-MM-DD HH:MM:SS`. |
| position | character | Face-off dot: `BULY_CENTRAL` or `BULY_{HOME/AWAY}_{DEFENSE/BLUE}_{LEFT/RIGHT}`. |
| position_shortcut | character | Short dot code, e.g. `C`, `HDL` (home defence left), `HDR`. |
| winner_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| winner_name | character | Player full name. |
| loser_id | integer | Player id (playerId key space) of the faceoff loser (DEL spells the key losser, ICEHL looser). |
| loser_name | character | Full name of the faceoff loser. |

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
[`icehl_game_center()`](https://fastRhockey.sportsdataverse.org/reference/icehl_game_center.md),
[`icehl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/icehl_game_info.md),
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
  try(icehl_faceoffs(match_id = 8161))
#> ── ICE Hockey League faceoffs from the eSports hokejovyzapis bucket ────────────
#> ℹ Data updated: 2026-10-18 06:12:47 UTC
#> # A tibble: 62 × 9
#>       id  time realtime         position position_shortcut winner_id winner_name
#>    <int> <int> <chr>            <chr>    <chr>                 <int> <chr>      
#>  1 23364     0 2026-09-18 18:3… BULY_CE… C                      4701 David Silye
#>  2 23365    54 2026-09-18 18:3… BULY_HO… HDL                    4108 Topi Rönni 
#>  3 23366   118 2026-09-18 18:3… BULY_HO… HDR                    4109 Aku Kestila
#>  4 23367   307 2026-09-18 18:3… BULY_HO… HDR                    4109 Aku Kestila
#>  5 23368   322 2026-09-18 18:3… BULY_HO… HBL                    4115 Gergely Tó…
#>  6 23369   433 2026-09-18 18:4… BULY_CE… C                      4109 Aku Kestila
#>  7 23370   456 2026-09-18 18:4… BULY_AW… ADR                    4109 Aku Kestila
#>  8 23371   466 2026-09-18 18:4… BULY_AW… ADR                    4948 Christophe…
#>  9 23372   533 2026-09-18 18:4… BULY_CE… C                      1075 Žiga Jeglič
#> 10 23373   592 2026-09-18 18:4… BULY_CE… C                      3534 Zach Boych…
#> # ℹ 52 more rows
#> # ℹ 2 more variables: loser_id <int>, loser_name <chr>
# }
```
