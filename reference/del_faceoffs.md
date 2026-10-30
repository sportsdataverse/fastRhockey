# **DEL Faceoffs**

Every faceoff of one match: time, dot, winner and loser.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/faceoffs.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/faceoffs.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_faceoffs(match_id, return_parsed = TRUE)
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

[DEL](https://www.penny-del.org/) (the league site whose widgets read
this public eSports bucket)

Other DEL eSports functions:
[`del_game_info()`](https://fastRhockey.sportsdataverse.org/reference/del_game_info.md),
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
  try(del_faceoffs(match_id = 4444))
#> ── DEL faceoffs from the eSports hokejovyzapis bucket ──────────────────────────
#> ℹ Data updated: 2026-10-30 00:08:00 UTC
#> # A tibble: 60 × 9
#>        id  time realtime        position position_shortcut winner_id winner_name
#>     <int> <int> <chr>           <chr>    <chr>                 <int> <chr>      
#>  1 936176     0 2026-10-11 14:… BULY_CE… C                       550 Dominik Ka…
#>  2 936177    14 2026-10-11 14:… BULY_HO… HDR                     550 Dominik Ka…
#>  3 936179    24 2026-10-11 14:… BULY_HO… HDR                    4337 Curtis Valk
#>  4 936186   189 2026-10-11 14:… BULY_AW… ADR                    4329 Lucas Thal…
#>  5 936191   237 2026-10-11 14:… BULY_HO… HBL                     550 Dominik Ka…
#>  6 936195   264 2026-10-11 14:… BULY_HO… HDR                    1665 Eric Cornel
#>  7 936197   291 2026-10-11 14:… BULY_AW… ADR                      27 Mirko Höff…
#>  8 936202   338 2026-10-11 14:… BULY_CE… C                       550 Dominik Ka…
#>  9 936206   430 2026-10-11 14:… BULY_HO… HBR                    4329 Lucas Thal…
#> 10 936207   437 2026-10-11 14:… BULY_HO… HDL                    1665 Eric Cornel
#> # ℹ 50 more rows
#> # ℹ 2 more variables: loser_id <int>, loser_name <chr>
# }
```
