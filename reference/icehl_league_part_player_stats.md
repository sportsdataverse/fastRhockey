# **ICE Hockey League League Part Player Stats**

Every skater's stats in one league part (a competition stage), one row
per player.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-part-players-stats/{league_part_id}.json`
of the ICE Hockey League eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/icehl.hokejovyzapis.cz/league-part-players-stats/56.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
icehl_league_part_player_stats(league_part_id, return_parsed = TRUE)
```

## Arguments

- league_part_id:

  League part id, one stage of a competition (league_part_id in
  icehl_schedule).

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
| player_id | integer | Player id (playerId key space, per bucket, stable across seasons). |
| name | character | Player full name. |
| firstname | character | First name of the player in this row. |
| surname | character | Player last name. |
| birthdate | character | Birth date `YYYY-MM-DD`. |
| position | character | Position code: `GK` goalie, `DE` defence, `FW` / `FO` forward (FO meaning unverified). |
| stick | character | Handedness: `left` or `right`; null when unknown. |
| weight | integer | Weight in kg; `0` placeholder when unknown. |
| height | integer | Height in cm; `0` placeholder when unknown. |
| jersey | integer | Jersey number. |
| birth_country | character | Player's country of birth (name). |
| nationality | character | Nationality, country name. |
| nationality_short | character | Nationality, three-letter code (`CAN`, `GER`; `UK` for Britain). |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
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
| timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
| timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
| timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
| timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
| hits | integer | Body checks delivered. |
| visible_games | logical | Widget flag: show the milestone for that stat. |
| visible_goals | logical | Widget flag: show the milestone for that stat. |
| visible_assists | logical | Widget flag: show the milestone for that stat. |
| visible_points | logical | Widget flag: show the milestone for that stat. |
| stars_games | integer | Widget star marker for that stat (unverified). |
| stars_goals | integer | Widget star marker for that stat (unverified). |
| stars_assists | integer | Widget star marker for that stat (unverified). |
| stars_points | integer | Widget star marker for that stat (unverified). |
| milestone_games | numeric | Widget value for the next milestone of that stat (unverified). |
| milestone_goals | numeric | Widget value for the next milestone of that stat (unverified). |
| milestone_assists | numeric | Widget value for the next milestone of that stat (unverified). |
| milestone_points | numeric | Widget value for the next milestone of that stat (unverified). |
| toi_gp | numeric | Time on ice per game played, seconds with decimals. |
| last_goal | character | Opaque date-like code of the player's latest goal, e.g. `2026-0-09-02640`; format unverified. |
| rank | integer | Rank of the player in the file's default sort (points). |

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
[`icehl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/icehl_pbp.md),
[`icehl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/icehl_player_box.md),
[`icehl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/icehl_schedule.md),
[`icehl_shots()`](https://fastRhockey.sportsdataverse.org/reference/icehl_shots.md),
[`icehl_standings()`](https://fastRhockey.sportsdataverse.org/reference/icehl_standings.md),
[`icehl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/icehl_team_player_stats.md),
[`icehl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/icehl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(icehl_league_part_player_stats(league_part_id = 56))
#> ── ICE Hockey League league_part_player_stats from the eSports hokejovyzapis buc
#> ℹ Data updated: 2026-10-18 06:12:53 UTC
#> # A tibble: 345 × 91
#>    player_id name       firstname surname birthdate position stick weight height
#>        <int> <chr>      <chr>     <chr>   <chr>     <chr>    <chr>  <int>  <int>
#>  1      4795 Joshua Da… Joshua D… Lawren… 2002-01-… FW       NA         0      0
#>  2      4716 Brady Shaw Brady     Shaw    1992-10-… FW       left      91    185
#>  3      3531 Marly Qui… Marly     Quince  1994-01-… FW       left      92    183
#>  4      4024 Nick Swan… Nick      Swaney  1997-09-… FW       left       0      0
#>  5      2979 Graham Kn… Graham    Knott   1997-01-… FW       left      87    191
#>  6      3494 Linden Vey Linden    Vey     1991-07-… FW       left       0      0
#>  7      4816 Jakob Lil… Jakob     Lilja   1993-07-… FW       NA        89    184
#>  8      4700 Carter Sa… Carter    Savoie  2002-01-… FW       left      86    178
#>  9      2943 Jeremy Gr… Jeremy    Gregoi… 1995-09-… FW       NA        88    183
#> 10      3499 Cole Schn… Cole      Schnei… 1990-08-… FW       NA         0      0
#> # ℹ 335 more rows
#> # ℹ 82 more variables: jersey <int>, birth_country <chr>, nationality <chr>,
#> #   nationality_short <chr>, team_id <int>, team_shortcut <chr>, games <int>,
#> #   goals <int>, goals_home <int>, goals_away <int>, assists <int>,
#> #   assists_home <int>, assists_away <int>, points <int>, points_home <int>,
#> #   points_away <int>, penalty_minutes <int>, positive <int>, negative <int>,
#> #   points_per_game <dbl>, shot_efficiency <dbl>, …
# }
```
