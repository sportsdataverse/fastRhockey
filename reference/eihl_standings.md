# **EIHL Standings**

Standings of one competition season, one row per team.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/table/{season}/{league_id}.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/table/2026/3.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_standings(season, league_id = 3L, return_parsed = TRUE)
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
| group_id | integer | Group id (group key space); null when the stage has no groups, so unobserved here. |
| group_name | character | Group name; empty string when the stage has no groups. |
| complete | logical | True when the standings include every finished game. |
| team_id | integer | Team id (per-bucket team key space); joins to home_id / guest_id in the schedule. |
| part_team_id | integer | Team-in-league-part id (its own key space: one team in one league part); no other captured file carries it. |
| name | character | Team display name. |
| team_original | character | Team's registered name. |
| shortcut | character | Three-letter team code. |
| position | integer | Table position, 1 = first. |
| points | integer | Table points. |
| games | integer | Games played. |
| wins | integer | Wins in regulation (DEL, EIHL, Denmark) or all wins (ICEHL, see `winsReg`); definition per bucket unverified. |
| wins_ot | integer | Wins after overtime or shootout. |
| wins_reg | integer | Wins in regulation time. |
| ties | integer | Ties (0 under current rules). |
| losts_ot | integer | Losses after overtime or shootout. |
| losts | integer | Losses in regulation time. |
| score1 | integer | Goals the team scored (goals for). |
| score2 | integer | Goals against. |
| goals_away | integer | Goals scored in away games. |
| last10 | character | Form over the last ten games as a result string; empty string in the samples. |
| only_ot_wins | integer | Wins in overtime. |
| only_so_wins | integer | Wins in a shootout. |
| only_ot_losts | integer | Losses in overtime. |
| only_so_losts | integer | Losses in a shootout. |
| qualified | logical | True when the team has clinched qualification. |
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

[EIHL](https://www.eliteleague.co.uk/) (the league site whose widgets
read this public eSports bucket)

Other EIHL eSports functions:
[`eihl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/eihl_game_info.md),
[`eihl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/eihl_goalie_box.md),
[`eihl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/eihl_pbp.md),
[`eihl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/eihl_player_box.md),
[`eihl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/eihl_schedule.md),
[`eihl_shots()`](https://fastRhockey.sportsdataverse.org/reference/eihl_shots.md),
[`eihl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/eihl_team_player_stats.md),
[`eihl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/eihl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(eihl_standings(season = 2027, league_id = 3))
#> ── EIHL standings from the eSports hokejovyzapis bucket ────────────────────────
#> ℹ Data updated: 2026-10-13 01:51:51 UTC
#> # A tibble: 10 × 27
#>    group_id group_name complete team_id part_team_id name          team_original
#>       <int> <chr>      <lgl>      <int>        <int> <chr>         <chr>        
#>  1       NA ""         FALSE         92          545 Cardiff Devi… Cardiff Devi…
#>  2       NA ""         FALSE         93          547 Coventry Bla… Coventry Bla…
#>  3       NA ""         FALSE         97          555 Guildford Fl… Guildford Fl…
#>  4       NA ""         FALSE         95          551 Fife Flyers   Fife Flyers  
#>  5       NA ""         FALSE        100          561 Sheffield St… Sheffield St…
#>  6       NA ""         FALSE         98          557 Manchester S… Manchester S…
#>  7       NA ""         FALSE         99          559 Nottingham P… Nottingham P…
#>  8       NA ""         FALSE         91          543 Belfast Gian… Belfast Gian…
#>  9       NA ""         FALSE         94          549 Dundee Stars  Dundee Stars 
#> 10       NA ""         FALSE         96          553 Glasgow Clan  Glasgow Clan 
#> # ℹ 20 more variables: shortcut <chr>, position <int>, points <int>,
#> #   games <int>, wins <int>, wins_ot <int>, wins_reg <int>, ties <int>,
#> #   losts_ot <int>, losts <int>, score1 <int>, score2 <int>, goals_away <int>,
#> #   last10 <chr>, only_ot_wins <int>, only_so_wins <int>, only_ot_losts <int>,
#> #   only_so_losts <int>, qualified <lgl>, ppg <dbl>
# }
```
