# **DEL Roster**

Game roster of one match by line slot, one row per dressed player.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/roster.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/roster.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_roster(match_id, return_parsed = TRUE)
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
| team_side | character | Team of the row: home or visitor. |
| slot | character | Line slot, three digits : group 1 = goalies (101, 102), 2 = defence pairs, 3 = forward lines. |
| player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| match_player_id | integer | Match-player id (one row per player per game; a different key space from the player id); joins to `matchPlayerId` in the same game's roster and events only. |
| name | character | Player first name. |
| surname | character | Player last name. |
| jersey | integer | Jersey number. |
| position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
| captain | logical | True for the team captain. |
| starting_six | logical | True for the six players who started the game. |

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
[`del_schedule()`](https://fastRhockey.sportsdataverse.org/reference/del_schedule.md),
[`del_shifts()`](https://fastRhockey.sportsdataverse.org/reference/del_shifts.md),
[`del_shots()`](https://fastRhockey.sportsdataverse.org/reference/del_shots.md),
[`del_standings()`](https://fastRhockey.sportsdataverse.org/reference/del_standings.md),
[`del_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/del_team_player_stats.md),
[`del_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/del_top_scorers.md)

## Examples

``` r
# \donttest{
  try(del_roster(match_id = 4444))
#> ── DEL roster from the eSports hokejovyzapis bucket ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:02:03 UTC
#> # A tibble: 42 × 10
#>    team_side slot  player_id match_player_id name      surname   jersey position
#>    <chr>     <chr>     <int>           <int> <chr>     <chr>      <int> <chr>   
#>  1 home      101        1594          274654 Joacim    Eriksson      60 GK      
#>  2 home      102        3424          274661 Simon     Wolf          35 GK      
#>  3 home      211        3560          274663 Eric      Martinss…     46 DE      
#>  4 home      212        2835          274660 Thomas    Schemits…     62 DE      
#>  5 home      221        4335          274666 Alexander Peters         6 DE      
#>  6 home      222         478          274644 Dominik   Bittner        7 DE      
#>  7 home      231        1343          274650 William   Weber         78 DE      
#>  8 home      232        1705          274655 Arkadiusz Dziambor      16 DE      
#>  9 home      241        1887          274657 Niklas    Hübner        72 DE      
#> 10 home      311         527          274648 Alexander Karachun      47 FO      
#> # ℹ 32 more rows
#> # ℹ 2 more variables: captain <lgl>, starting_six <lgl>
# }
```
