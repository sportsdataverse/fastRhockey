# **DEL Top Scorers**

Skater scoring leaderboard of one competition season.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/league-top-scorers/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_top_scorers(season, league_id = 1L, return_parsed = TRUE)
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
| player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| name | character | Player first name. |
| surname | character | Player last name. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| goals | integer | Goals the player scored in the league-season. |
| assists | integer | Assists credited to the player in the league-season. |
| points | integer | Points, goals + assists. |
| shots | integer | Shots on goal. |
| games | integer | Games played (two names for the same count). |
| penalty_minutes | integer | Penalty minutes assessed to the player in the league-season. |
| time_on_ice | integer | Time on ice, seconds; `0` placeholder in the samples. |

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
[`del_shifts()`](https://fastRhockey.sportsdataverse.org/reference/del_shifts.md),
[`del_shots()`](https://fastRhockey.sportsdataverse.org/reference/del_shots.md),
[`del_standings()`](https://fastRhockey.sportsdataverse.org/reference/del_standings.md),
[`del_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/del_team_player_stats.md)

## Examples

``` r
# \donttest{
  try(del_top_scorers(season = 2027, league_id = 1))
#> ── DEL top_scorers from the eSports hokejovyzapis bucket ───────────────────────
#> ℹ Data updated: 2026-10-13 01:51:40 UTC
#> # A tibble: 367 × 13
#>    player_id name   surname team_id team_name team_shortcut goals assists points
#>        <int> <chr>  <chr>     <int> <chr>     <chr>         <int>   <int>  <int>
#>  1      3410 Liam   Kirk          3 Eisbären… EBB               5      11     16
#>  2      2209 Ty     Ronning       3 Eisbären… EBB               7       7     14
#>  3      3950 Mathew Santos        5 Krefeld … KEV               7       6     13
#>  4       507 Marcel Noebels      11 Kölner H… KEC               7       5     12
#>  5      2935 Max    Newton        5 Krefeld … KEV               3       9     12
#>  6      3572 Patri… Russell      11 Kölner H… KEC               2      10     12
#>  7      3594 Kyle   Mayhew       13 Augsburg… AEV               2       9     11
#>  8      3550 Nicho… Hallor…       6 Straubin… STR               4       6     10
#>  9      3426 Taro   Hirose       12 EHC Red … RBM               1       9     10
#> 10      1799 Gregor MacLeod      11 Kölner H… KEC               1       9     10
#> # ℹ 357 more rows
#> # ℹ 4 more variables: shots <int>, games <int>, penalty_minutes <int>,
#> #   time_on_ice <int>
# }
```
