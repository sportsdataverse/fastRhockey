# **Metal Ligaen Player Box**

Player boxscore of one team in one match, one row per player.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/team-stats/{team_id}.json`
of the Metal Ligaen eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/team-stats/8.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
metal_player_box(match_id, team_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in metal_schedule). Not
  the league website's game id.

- team_id:

  Team id (per-bucket team key space: home_id / guest_id in
  metal_schedule).

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
| xg | numeric | Expected goals of the player's shots. |
| top5 | logical | True when the player ranks in the team's top five. |
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

[Metal Ligaen](https://metalligaen.dk/) (the league site whose widgets
read this public eSports bucket)

Other Metal Ligaen eSports functions:
[`metal_game_center()`](https://fastRhockey.sportsdataverse.org/reference/metal_game_center.md),
[`metal_game_info()`](https://fastRhockey.sportsdataverse.org/reference/metal_game_info.md),
[`metal_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_goalie_box.md),
[`metal_pbp()`](https://fastRhockey.sportsdataverse.org/reference/metal_pbp.md),
[`metal_schedule()`](https://fastRhockey.sportsdataverse.org/reference/metal_schedule.md),
[`metal_standings()`](https://fastRhockey.sportsdataverse.org/reference/metal_standings.md),
[`metal_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/metal_top_scorers.md)

## Examples

``` r
# \donttest{
  try(metal_player_box(match_id = 575, team_id = 8))
#> ── Metal Ligaen player_box from the eSports hokejovyzapis bucket ───────────────
#> ℹ Data updated: 2026-10-13 01:56:59 UTC
#> # A tibble: 24 × 79
#>    player_id name       firstname surname birthdate active position stick weight
#>        <int> <chr>      <chr>     <chr>   <chr>     <lgl>  <chr>    <chr>  <int>
#>  1       124 Nikolaj K… "Nikolaj" Krag C… 1998-08-… TRUE   FW       left      92
#>  2       405 Jerkko Rä… "Jerkko"  Rämö    2000-05-… TRUE   DE       left      80
#>  3       220 Miku Ronk… "Miku"    Ronkai… 1997-08-… TRUE   FW       left      74
#>  4       400 Mathieu C… "Mathieu" Caron   2000-03-… TRUE   GK       left      86
#>  5       113 Niclas Kr… "Niclas"  Krarup  2000-08-… TRUE   FW       left      85
#>  6       406 Jesse Vuo… "Jesse"   Vuorela 2002-01-… TRUE   DE       right     84
#>  7       322 Bertram W… "Bertram" Witoms… 2006-08-… TRUE   GK       left      72
#>  8       104 Frederick… "Frederi… Amonds… 2006-05-… TRUE   DE       left      91
#>  9       402 Sebastian… "Sebasti… Bergho… 1993-05-… TRUE   DE       left      83
#> 10       110 Mikey Ber… "Mikey"   Bergma… 2007-02-… TRUE   FW       left      80
#> # ℹ 14 more rows
#> # ℹ 70 more variables: height <int>, jersey <int>, birth_country <chr>,
#> #   nationality <chr>, nationality_short <chr>, team_shortcut <chr>,
#> #   games <int>, goals <int>, goals_home <int>, goals_away <int>,
#> #   assists <int>, assists_home <int>, assists_away <int>, points <int>,
#> #   points_home <int>, points_away <int>, penalty_minutes <int>,
#> #   positive <int>, negative <int>, points_per_game <dbl>, …
# }
```
