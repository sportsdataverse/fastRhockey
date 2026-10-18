# **EIHL Team Player Stats**

Season player stats of one team in one competition, one row per player.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-team-stats/{season}/{league_id}/{team_id}.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/league-team-stats/2026/3/96.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_team_player_stats(season, team_id, league_id = 3L, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026/27). The bucket keys seasons by
  their START year; the wrapper requests season - 1.

- team_id:

  Team id (per-bucket team key space: home_id / guest_id in
  eihl_schedule).

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
| player_id | integer | Player id (playerId key space, per bucket, stable across games and seasons). |
| name | character | Player full name. |
| firstname | character | First name of the player in this row. |
| surname | character | Player last name. |
| birthdate | character | Birth date `YYYY-MM-DD`. |
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
| timeonice | numeric | Time on ice, seconds with decimals (tracking-based; differs from `timeOnIce`). |
| timeonice_eq | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
| timeonice_pp | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
| timeonice_sh | numeric | Time on ice at even strength (`EQ`), on the power play (`PP`) or shorthanded (`SH`), seconds with decimals. |
| hits | integer | Body checks delivered. |

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
[`eihl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/eihl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(eihl_team_player_stats(season = 2027, team_id = 96, league_id = 3))
#> ── EIHL team_player_stats from the eSports hokejovyzapis bucket ────────────────
#> ℹ Data updated: 2026-10-18 05:43:16 UTC
#> # A tibble: 25 × 70
#>    player_id name       firstname surname birthdate active position stick weight
#>        <int> <chr>      <chr>     <chr>   <chr>     <lgl>  <chr>    <chr>  <int>
#>  1      4898 Cade Neil… Cade      Neilson 2001-05-… TRUE   FW       left      89
#>  2      5157 Josh Tarz… Josh      Tarzwe… 2000-02-… TRUE   FW       left      93
#>  3      5154 Cole Kodsi Cole      Kodsi   2000-06-… TRUE   FW       left      91
#>  4      5187 Keaton Pe… Keaton    Pehrson 1998-12-… TRUE   DE       right     97
#>  5      4894 Rylan Sch… Rylan     Schwar… 1990-01-… TRUE   FW       left      87
#>  6      5075 Brady Risk Brady     Risk    1999-03-… TRUE   FW       right      0
#>  7      4873 Matt Spen… Matt      Spencer 1997-03-… TRUE   DE       right     92
#>  8      4892 Steven Se… Steven    Seigo   1990-08-… TRUE   DE       right     85
#>  9      4048 Alex Forb… Alex      Forbes  1996-07-… TRUE   FW       right     86
#> 10      5153 Julius Re… Julius    Reini   2000-05-… TRUE   DE       left      92
#> # ℹ 15 more rows
#> # ℹ 61 more variables: height <int>, jersey <int>, birth_country <chr>,
#> #   nationality <chr>, nationality_short <chr>, team_shortcut <chr>,
#> #   games <int>, goals <int>, goals_home <int>, goals_away <int>,
#> #   assists <int>, assists_home <int>, assists_away <int>, points <int>,
#> #   points_home <int>, points_away <int>, penalty_minutes <int>,
#> #   positive <int>, negative <int>, points_per_game <dbl>, …
# }
```
