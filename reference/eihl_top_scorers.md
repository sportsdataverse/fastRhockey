# **EIHL Top Scorers**

Skater scoring leaderboard of one competition season.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-top-scorers/{season}/{league_id}.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-top-scorers/2026/3.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_top_scorers(season, league_id = 3L, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026/27). The bucket keys seasons by
  their START year; the wrapper requests season - 1.

- league_id:

  Competition id. 3 = Elite League, 4 = Challenge Cup, 5 = playoffs.

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
| jersey | integer | Jersey number. |
| birthdate | character | Birth date `YYYY-MM-DD`. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| matches | integer | Games played (two names for the same count). |
| goals | integer | Goals the player scored in the league-season. |
| assists | integer | Assists credited to the player in the league-season. |
| points | integer | Points, goals + assists. |
| shots | integer | Shots on goal. |
| positives | integer | Plus events of the player: on ice for a goal for (league-season). |
| negatives | integer | Minus events of the player: on ice for a goal against (league-season). |
| positives_negatives | integer | Plus/minus, positives - negatives. |

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
[`eihl_shots()`](https://fastRhockey.sportsdataverse.org/reference/eihl_shots.md),
[`eihl_standings()`](https://fastRhockey.sportsdataverse.org/reference/eihl_standings.md),
[`eihl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/eihl_team_player_stats.md)

## Examples

``` r
# \donttest{
  try(eihl_top_scorers(season = 2027, league_id = 3))
#> ── EIHL top_scorers from the eSports hokejovyzapis bucket ──────────────────────
#> ℹ Data updated: 2026-11-04 09:30:12 UTC
#> # A tibble: 250 × 16
#>    player_id name       surname jersey birthdate team_id team_name team_shortcut
#>        <int> <chr>      <chr>    <int> <chr>       <int> <chr>     <chr>        
#>  1      4843 "Josh"     MacDon…     92 1994-03-…      92 Cardiff … CAR          
#>  2      5171 "Olivier"  Archam…     93 1993-02-…     100 Sheffiel… SHE          
#>  3      4449 "Brandon"  Whistle     74 1997-12-…     100 Sheffiel… SHE          
#>  4      4973 "Brandon"  Cutler      29 2000-01-…      98 Manchest… MAN          
#>  5      4104 "Joshua"   Waller      91 1999-06-…      97 Guildfor… GUI          
#>  6      4757 "Mitchell" Balmas      92 1998-03-…     100 Sheffiel… SHE          
#>  7      4734 "Victor"   Bjorku…     10 1993-04-…      95 Fife Fly… FIF          
#>  8      5044 "Justin"   Duchar…     67 2000-02-…      97 Guildfor… GUI          
#>  9      5121 "Kaleb"    Pearson      8 2000-06-…      91 Belfast … BEL          
#> 10      5128 "Griffin " Ness        77 1999-12-…      93 Coventry… COV          
#> # ℹ 240 more rows
#> # ℹ 8 more variables: matches <int>, goals <int>, assists <int>, points <int>,
#> #   shots <int>, positives <int>, negatives <int>, positives_negatives <int>
# }
```
