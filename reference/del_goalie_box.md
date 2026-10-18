# **DEL Goalie Box**

Goalie lines of one match, one row per goalie who played.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/top-goalies.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/top-goalies.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_goalie_box(match_id, return_parsed = TRUE)
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
| player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| name | character | Goalie first name. |
| surname | character | Goalie last name. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| jersey | integer | Jersey number. |
| matches | integer | Games played (1 in a per-game file). |
| won_matches | integer | Games the goalie won. |
| lost_matches | integer | Games the goalie lost. |
| saves | integer | Shots on goal the goalie stopped. |
| goals_against | integer | Goals the goalie allowed. |
| saves_percent | numeric | Save percentage x 100 (e.g. `85.71`). |
| average | numeric | Goals-against average per 60 minutes. |
| zeros | integer | Shutouts by the goalie (`zeros`: games with no goal allowed). |
| time_on_ice | integer | Time in goal, seconds. |
| seconds_per_game | numeric | Time in goal per game, seconds. |
| minutes_in_play | numeric | Share of the team's game time in goal, percent (100.0 = the whole game). |

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
  try(del_goalie_box(match_id = 4444))
#> ── DEL goalie_box from the eSports hokejovyzapis bucket ────────────────────────
#> ℹ Data updated: 2026-10-18 12:02:00 UTC
#> # A tibble: 7 × 18
#>   player_id name     surname      team_id team_name team_shortcut jersey matches
#>       <int> <chr>    <chr>          <int> <chr>     <chr>          <int>   <int>
#> 1      4154 Antoine  Bibeau            12 EHC Red … RBM               30       1
#> 2      1594 Joacim   Eriksson          15 Schwenni… SWW               60       1
#> 3      3424 Simon    Wolf              15 Schwenni… SWW               35       0
#> 4      4178 Gianluca Bäuml             15 Schwenni… SWW               51       0
#> 5       122 Mathias  Niederberger      12 EHC Red … RBM               35       0
#> 6      2831 Matthias Bittner           12 EHC Red … RBM               32       0
#> 7      4326 Max      Bolle             12 EHC Red … RBM                1       0
#> # ℹ 10 more variables: won_matches <int>, lost_matches <int>, saves <int>,
#> #   goals_against <int>, saves_percent <dbl>, average <dbl>, zeros <int>,
#> #   time_on_ice <int>, seconds_per_game <dbl>, minutes_in_play <dbl>
# }
```
