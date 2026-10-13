# **DEL Standings**

Standings of one competition season, one row per team.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/tables/{season}/{league_id}.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/tables/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_standings(season, league_id = 1L, return_parsed = TRUE)
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
| league_part_id | integer | League part id (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
| league_part_name | character | League part name, e.g. `DEL 2026/27`. |
| state | character | Standings state flag (e.g. live or final; values unverified). |
| datetime | character | Standings computed at, local time, `YYYY-MM-DD HH:MM:SS`. |
| team_id | integer | Team id (per-bucket team key space); joins to home_id / guest_id in the schedule. |
| part_team_id | integer | Team-in-league-part id (its own key space: one team in one league part); no other captured file carries it. |
| name | character | Team display name. |
| shortcut | character | Three-letter team code. |
| position | integer | Table position, 1 = first. |
| points | integer | Table points. |
| games | integer | Games played. |
| wins | integer | Wins in regulation (DEL, EIHL, Denmark) or all wins (ICEHL, see `winsReg`); definition per bucket unverified. |
| wins_ot | integer | Wins after overtime or shootout. |
| ties | integer | Ties (0 under current rules). |
| losses_ot | integer | Losses after overtime or shootout. |
| losses | integer | Losses in regulation time. |
| score1 | integer | Goals the team scored (goals for). |
| score2 | integer | Goals against. |
| only_so_wins | integer | Wins in a shootout. |
| only_so_losts | integer | Losses in a shootout. |
| only_ot_wins | integer | Wins in overtime. |
| only_ot_losts | integer | Losses in overtime. |
| bonus | integer | Bonus or deduction points (DEL). |
| ppg | numeric | Points per game. |

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
[`del_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/del_team_player_stats.md),
[`del_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/del_top_scorers.md)

## Examples

``` r
# \donttest{
  try(del_standings(season = 2027, league_id = 1))
#> ── DEL standings from the eSports hokejovyzapis bucket ─────────────────────────
#> ℹ Data updated: 2026-10-13 01:51:38 UTC
#> # A tibble: 14 × 24
#>    league_part_id league_part_name state   datetime   team_id part_team_id name 
#>             <int> <chr>            <chr>   <chr>        <int>        <int> <chr>
#>  1             44 DEL 2026/27      playing 2026-10-1…       3          495 Eisb…
#>  2             44 DEL 2026/27      playing 2026-10-1…       2          497 Adle…
#>  3             44 DEL 2026/27      playing 2026-10-1…      14          501 Nürn…
#>  4             44 DEL 2026/27      playing 2026-10-1…      12          498 EHC …
#>  5             44 DEL 2026/27      playing 2026-10-1…       6          500 Stra…
#>  6             44 DEL 2026/27      playing 2026-10-1…      13          506 Augs…
#>  7             44 DEL 2026/27      playing 2026-10-1…       8          504 Griz…
#>  8             44 DEL 2026/27      playing 2026-10-1…       5          507 Kref…
#>  9             44 DEL 2026/27      playing 2026-10-1…      11          499 Köln…
#> 10             44 DEL 2026/27      playing 2026-10-1…      44          503 Löwe…
#> 11             44 DEL 2026/27      playing 2026-10-1…       1          494 ERC …
#> 12             44 DEL 2026/27      playing 2026-10-1…       9          496 Ping…
#> 13             44 DEL 2026/27      playing 2026-10-1…       7          505 Iser…
#> 14             44 DEL 2026/27      playing 2026-10-1…      15          502 Schw…
#> # ℹ 17 more variables: shortcut <chr>, position <int>, points <int>,
#> #   games <int>, wins <int>, wins_ot <int>, ties <int>, losses_ot <int>,
#> #   losses <int>, score1 <int>, score2 <int>, only_so_wins <int>,
#> #   only_so_losts <int>, only_ot_wins <int>, only_ot_losts <int>, bonus <int>,
#> #   ppg <dbl>
# }
```
