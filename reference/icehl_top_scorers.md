# **ICE Hockey League Top Scorers**

Skater scoring leaderboard of one competition season.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json`
of the ICE Hockey League eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-top-scorers/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
icehl_top_scorers(season, league_id = 1L, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026/27). The bucket keys seasons by
  their START year; the wrapper requests season - 1.

- league_id:

  Competition id. 1 = ICE Hockey League, regular season and playoffs in
  one file (split them by league_part_name); 2 = pre-season (2025/26
  on).

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
| photo | character | Player photo URL; null when none. |
| birthdate | character | Birth date `YYYY-MM-DD`. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| goals | integer | Goals the player scored in the league-season. |
| assists | integer | Assists credited to the player in the league-season. |
| points | integer | Points, goals + assists. |
| shots | integer | Shots on goal. |
| penalty_minutes | integer | Penalty minutes assessed to the player in the league-season. |

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
[`icehl_faceoffs()`](https://fastRhockey.sportsdataverse.org/reference/icehl_faceoffs.md),
[`icehl_game_center()`](https://fastRhockey.sportsdataverse.org/reference/icehl_game_center.md),
[`icehl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/icehl_game_info.md),
[`icehl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/icehl_goalie_box.md),
[`icehl_league_part_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/icehl_league_part_player_stats.md),
[`icehl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/icehl_pbp.md),
[`icehl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/icehl_player_box.md),
[`icehl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/icehl_schedule.md),
[`icehl_shots()`](https://fastRhockey.sportsdataverse.org/reference/icehl_shots.md),
[`icehl_standings()`](https://fastRhockey.sportsdataverse.org/reference/icehl_standings.md),
[`icehl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/icehl_team_player_stats.md)

## Examples

``` r
# \donttest{
  try(icehl_top_scorers(season = 2027, league_id = 1))
#> ── ICE Hockey League top_scorers from the eSports hokejovyzapis bucket ─────────
#> ℹ Data updated: 2026-10-18 12:05:56 UTC
#> # A tibble: 420 × 13
#>    player_id name  surname photo birthdate team_id team_name team_shortcut goals
#>        <int> <chr> <chr>   <chr> <chr>       <int> <chr>     <chr>         <int>
#>  1      4795 Josh… Lawren… http… 2002-01-…      10 Olimpija… OLL               9
#>  2      4716 Brady Shaw    NA    1992-10-…      13 Steinbac… BWL               8
#>  3      3531 Marly Quince  http… 1994-01-…      10 Olimpija… OLL               3
#>  4      4024 Nick  Swaney  http… 1997-09-…       9 Moser Me… G99               7
#>  5      2979 Grah… Knott   http… 1997-01-…      13 Steinbac… BWL               6
#>  6      3494 Lind… Vey     http… 1991-07-…      12 Vienna C… VIC               3
#>  7      4816 Jakob Lilja   NA    1993-07-…      12 Vienna C… VIC               7
#>  8      4700 Cart… Savoie  http… 2002-01-…       4 FTC-Tele… FTC               5
#>  9      2943 Jere… Gregoi… http… 1995-09-…      12 Vienna C… VIC               5
#> 10      3499 Cole  Schnei… http… 1990-08-…       7 HCB Südt… HCB               2
#> # ℹ 410 more rows
#> # ℹ 4 more variables: assists <int>, points <int>, shots <int>,
#> #   penalty_minutes <int>
# }
```
