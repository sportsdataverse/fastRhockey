# **DEL Player Box**

Player boxscore of one team in one match, one row per player.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/{match_id}/team-stats/{team_id}.json`
of the DEL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/de.hokejovyzapis.cz/matches/4444/team-stats/12.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
del_player_box(match_id, team_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in del_schedule). Not
  the league website's game id.

- team_id:

  Team id (per-bucket team key space: home_id / guest_id in
  del_schedule).

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
| player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
| holema_id | integer | Unobserved (null in every sample); an external registry id by its name, key space unknown, joins to nothing captured. |
| name | character | Player full name. |
| firstname | character | First name of the player in this row. |
| surname | character | Player last name. |
| date_of_birth | character | Birth date `YYYY-MM-DD`. |
| active | logical | True when the player is active on the team. |
| position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
| stick | character | Handedness: `left` or `right`; null when unknown. |
| weight | integer | Weight in kg; `0` placeholder when unknown. |
| height | integer | Height in cm; `0` placeholder when unknown. |
| jersey | integer | Jersey number. |
| birth_country | character | Player's country of birth (name). |
| nationality | character | Nationality, country name. |
| nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
| team_shortcut | character | Three-letter team code. |
| games | integer | Games played. |
| goals | integer | goals in total (goals_home + goals_away: the body splits it by home and away games). |
| goals_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| goals_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| assists | integer | assists in total (assists_home + assists_away: the body splits it by home and away games). |
| assists_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| assists_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| points | integer | points in total (points_home + points_away: the body splits it by home and away games). |
| points_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| points_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| penalty_minutes | integer | Penalty minutes assessed to the player (this game, or to date in the season / league-part files). |
| positive | integer | Plus: on ice for goals for at even strength or shorthanded. |
| negative | integer | Minus: on ice for goals against at even strength or on the power play. |
| points_per_game | numeric | Average goals plus assists per game played. |
| shot_efficiency | numeric | Shooting percentage, goals / shots on goal x 100. |
| penalty_minutes_per_game | numeric | Average penalty minutes per game played. |
| pp_goals | integer | Power-play goals. |
| sh_goals | integer | Shorthanded goals. |
| gw_goals | integer | Game-winning goals. |
| shots_attempts | integer | Shot attempts (on goal + missed + blocked). |
| shots_on_goal | integer | shots_on_goal in total (shots_on_goal_home + shots_on_goal_away: the body splits it by home and away games). |
| shots_on_goal_home | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| shots_on_goal_away | integer | Split by venue: `home` = in the player's home games, `away` = in away games. |
| shots_missed | integer | This player's shot attempts that missed the net. |
| shots_blocked | integer | This player's shot attempts that were blocked. |
| blocked_shots_by_player | integer | Opponent shots this player blocked. |
| faceoffs_win | integer | Face-offs the player won (this game, or to date in the season / league-part files). |
| faceoffs_losses | integer | Face-offs the player lost (this game, or to date in the season / league-part files). |
| faceoffs_count | integer | Face-offs the player took (this game, or to date in the season / league-part files). |
| time_on_ice | integer | Time on ice, whole seconds (`0` when not tracked). |
| time_on_ice_pp | integer | Time on ice on the power play (`PP`) or shorthanded (`SH`), whole seconds. |
| time_on_ice_sh | integer | Time on ice on the power play (`PP`) or shorthanded (`SH`), whole seconds. |
| time_on_ice_per_game | numeric | Time on ice per game, seconds. |
| shifts | integer | Shifts the player took (this game, or to date in the season / league-part files). |
| shifts_per_game | numeric | Average shifts per game played. |

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
  try(del_player_box(match_id = 4444, team_id = 12))
#> ── DEL player_box from the eSports hokejovyzapis bucket ────────────────────────
#> ℹ Data updated: 2026-10-30 00:08:04 UTC
#> # A tibble: 27 × 51
#>    player_id holema_id name      firstname surname date_of_birth active position
#>        <int>     <int> <chr>     <chr>     <chr>   <chr>         <lgl>  <chr>   
#>  1       139        NA Patrick … Patrick   Hager   1988-09-08    TRUE   FO      
#>  2       122        NA Mathias … Mathias   Nieder… 1992-11-26    TRUE   GK      
#>  3        80        NA Yasin Eh… Yasin     Ehliz   1992-12-30    TRUE   FO      
#>  4        16        NA Fabio Wa… Fabio     Wagner  1995-09-17    TRUE   DE      
#>  5       551        NA Maximili… Maximili… Kastner 1993-01-03    TRUE   DE      
#>  6       550        NA Dominik … Dominik   Kahun   1995-07-02    TRUE   FO      
#>  7      1168        NA Maximili… Maximili… Daubner 1997-09-20    TRUE   DE      
#>  8      1364        NA Markus E… Markus    Eisens… 1995-01-22    TRUE   FO      
#>  9      3275        NA Tobias R… Tobias    Rieder  1993-01-10    TRUE   FO      
#> 10      2831        NA Matthias… Matthias  Bittner 2004-02-13    TRUE   GK      
#> # ℹ 17 more rows
#> # ℹ 43 more variables: stick <chr>, weight <int>, height <int>, jersey <int>,
#> #   birth_country <chr>, nationality <chr>, nationality_short <chr>,
#> #   team_shortcut <chr>, games <int>, goals <int>, goals_home <int>,
#> #   goals_away <int>, assists <int>, assists_home <int>, assists_away <int>,
#> #   points <int>, points_home <int>, points_away <int>, penalty_minutes <int>,
#> #   positive <int>, negative <int>, points_per_game <dbl>, …
# }
```
