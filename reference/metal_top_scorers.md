# **Metal Ligaen Top Scorers**

Skater scoring leaderboard of one competition season.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json`
of the Metal Ligaen eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/league-top-scorers/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
metal_top_scorers(season, league_id = 1L, return_parsed = TRUE)
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
| player_id | integer | Player id (player key space, per bucket, stable across games and seasons); joins to `playerId` in rosters, events, boxscores and leaderboards. |
| name | character | Player first name. |
| surname | character | Player last name. |
| birthdate | character | Birth date `YYYY-MM-DD`. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| goals | integer | Goals the player scored in the league-season. |
| assists | integer | Assists credited to the player in the league-season. |
| points | integer | Points, goals + assists. |
| shots | integer | Shots on goal. |

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
[`metal_schedule()`](https://fastRhockey.sportsdataverse.org/reference/metal_schedule.md),
[`metal_standings()`](https://fastRhockey.sportsdataverse.org/reference/metal_standings.md)

## Examples

``` r
# \donttest{
  try(metal_top_scorers(season = 2027, league_id = 1))
#> ── Metal Ligaen top_scorers from the eSports hokejovyzapis bucket ──────────────
#> ℹ Data updated: 2026-10-13 01:57:02 UTC
#> # A tibble: 236 × 11
#>    player_id name      surname   birthdate team_id team_name team_shortcut goals
#>        <int> <chr>     <chr>     <chr>       <int> <chr>     <chr>         <int>
#>  1       428 "Troy"    Lajeunes… 1997-05-…       5 Esbjerg … ESB               7
#>  2       429 "Zane "   Franklin  1999-05-…       5 Esbjerg … ESB               4
#>  3       398 "Kevin"   O'Neil    1998-02-…       6 Herning … HER               4
#>  4        46 "Morten"  Poulsen   1988-09-…       6 Herning … HER               6
#>  5       210 "Oliver"  Kjær      2000-07-…       5 Esbjerg … ESB               4
#>  6        38 "Mathias" Bau Hans… 1993-07-…       6 Herning … HER               2
#>  7       496 "Olle"    Liss      1992-10-…       1 Rungsted… RUN               8
#>  8       391 "Atte"    Karppinen 1998-03-…       3 Frederik… FRE               6
#>  9       396 "Leo"     Ring      2001-10-…       3 Frederik… FRE               3
#> 10       146 "Nicolas" Werbik    1997-10-…       5 Esbjerg … ESB               6
#> # ℹ 226 more rows
#> # ℹ 3 more variables: assists <int>, points <int>, shots <int>
# }
```
