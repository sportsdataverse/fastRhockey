# **ICE Hockey League Standings**

Standings of one competition season, one row per team.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/tables/{season}/{league_id}.json`
of the ICE Hockey League eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/tables/2026/1.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
icehl_standings(season, league_id = 1L, return_parsed = TRUE)
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
| points_away | integer | Table points from away games. |
| last10 | character | Form over the last ten games as a result string; empty string in the samples. |
| only_ot_wins | integer | Wins in overtime. |
| only_so_wins | integer | Wins in a shootout. |
| only_ot_losts | integer | Losses in overtime. |
| only_so_losts | integer | Losses in a shootout. |
| qualified | logical | True when the team has clinched qualification. |
| live | logical | True while the team is playing a live game. |
| live_game_score | character | Score of the live game; null when none, so unobserved here. |
| position_change | integer | Change in table position since the previous round. |
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
[`icehl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/icehl_team_player_stats.md),
[`icehl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/icehl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(icehl_standings(season = 2027, league_id = 1))
#> ── ICE Hockey League standings from the eSports hokejovyzapis bucket ───────────
#> ℹ Data updated: 2026-10-13 01:54:32 UTC
#> # A tibble: 14 × 31
#>    group_id group_name complete team_id part_team_id name          team_original
#>       <int> <chr>      <lgl>      <int>        <int> <chr>         <chr>        
#>  1       NA ""         TRUE           7          566 HCB Südtirol… HCB Südtirol…
#>  2       NA ""         TRUE          13          570 Steinbach Bl… Steinbach Bl…
#>  3       NA ""         TRUE           2          560 EC-KAC        EC-KAC       
#>  4       NA ""         TRUE          10          568 Olimpija Lju… Olimpija Lju…
#>  5       NA ""         TRUE          85          559 Milano HC     Milano HC    
#>  6       NA ""         TRUE           5          563 HC Falkenste… HC Falkenste…
#>  7       NA ""         TRUE          12          571 Vienna Capit… Vienna Capit…
#>  8       NA ""         TRUE           9          558 Moser Medica… Moser Medica…
#>  9       NA ""         TRUE           3          561 EC Red Bull … EC Red Bull …
#> 10       NA ""         TRUE           6          564 HC TIWAG Inn… HC TIWAG Inn…
#> 11       NA ""         TRUE           1          572 EC iDM Wärme… EC iDM Wärme…
#> 12       NA ""         TRUE           8          565 Hydro Feherv… Hydro Feherv…
#> 13       NA ""         TRUE           4          562 FTC-Telekom   FTC-Telekom  
#> 14       NA ""         TRUE          11          569 Pioneers Vor… Pioneers Vor…
#> # ℹ 24 more variables: shortcut <chr>, position <int>, points <int>,
#> #   games <int>, wins <int>, wins_ot <int>, wins_reg <int>, ties <int>,
#> #   losts_ot <int>, losts <int>, score1 <int>, score2 <int>, goals_away <int>,
#> #   points_away <int>, last10 <chr>, only_ot_wins <int>, only_so_wins <int>,
#> #   only_ot_losts <int>, only_so_losts <int>, qualified <lgl>, live <lgl>,
#> #   live_game_score <chr>, position_change <int>, ppg <dbl>
# }
```
