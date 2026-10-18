# **Metal Ligaen Goalie Box**

Goalie lines of one match, one row per goalie who played.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/{match_id}/top-goalies.json`
of the Metal Ligaen eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/matches/575/top-goalies.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
metal_goalie_box(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in metal_schedule). Not
  the league website's game id.

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
| name | character | Goalie first name. |
| surname | character | Goalie last name. |
| photo | character | Goalie photo URL; null when none. |
| birthdate | character | Birth date `YYYY-MM-DD`. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| team_type | character | Side: `home` or `visitor`; null in DEL and EIHL. |
| jersey | integer | Jersey number. |
| matches | integer | Games played (1 in a per-game file). |
| won_matches | integer | Games the goalie won. |
| lost_matches | integer | Games the goalie lost. |
| saves | integer | Shots on goal the goalie stopped. |
| goals_against | integer | Goals the goalie allowed. |
| shots_against | integer | Shots on goal faced. |
| saves_percent | numeric | Save percentage x 100 (e.g. `85.71`). |
| average | numeric | Goals-against average per 60 minutes. |
| zeros | integer | Shutouts by the goalie (`zeros`: games with no goal allowed). |
| time_on_ice | integer | Time in goal, seconds. |
| seconds_per_game | numeric | Time in goal per game, seconds. |
| minutes_in_play | numeric | Share of the team's game time in goal, percent (100.0 = the whole game). |
| visible_games | logical | Widget flag: show the milestone for that stat. |
| visible_shutouts | logical | Widget flag: show the milestone for that stat. |
| visible_wins | logical | Widget flag: show the milestone for that stat. |
| stars_games | integer | Widget star marker for that stat (unverified). |
| stars_shutouts | integer | Widget star marker for that stat (unverified). |
| stars_wins | integer | Widget star marker for that stat (unverified). |
| milestone_games | numeric | Widget value for the next milestone of that stat (unverified). |
| milestone_shutouts | numeric | Widget value for the next milestone of that stat (unverified). |
| milestone_wins | numeric | Widget value for the next milestone of that stat (unverified). |
| xg | numeric | Expected goals of the goalie's own team (ICEHL only). |
| xga | numeric | Expected goals against (ICEHL only; null elsewhere). |
| gsax | numeric | Goals saved above expected, xGA minus goals against (ICEHL only). |
| xs | numeric | Expected saves (ICEHL only; unverified). |
| gate_zone_ur_goals_against | integer | Goals allowed in that net zone. |
| gate_zone_ur_shots_against | integer | Shots on goal faced in that net zone. |
| gate_zone_ur_saves | integer | Saves in that net zone. |
| gate_zone_ur_saves_percent | numeric | Save percentage x 100 in that net zone. |
| gate_zone_ul_goals_against | integer | Goals allowed in that net zone. |
| gate_zone_ul_shots_against | integer | Shots on goal faced in that net zone. |
| gate_zone_ul_saves | integer | Saves in that net zone. |
| gate_zone_ul_saves_percent | numeric | Save percentage x 100 in that net zone. |
| gate_zone_br_goals_against | integer | Goals allowed in that net zone. |
| gate_zone_br_shots_against | integer | Shots on goal faced in that net zone. |
| gate_zone_br_saves | integer | Saves in that net zone. |
| gate_zone_br_saves_percent | numeric | Save percentage x 100 in that net zone. |
| gate_zone_fh_goals_against | integer | Goals allowed in that net zone. |
| gate_zone_fh_shots_against | integer | Shots on goal faced in that net zone. |
| gate_zone_fh_saves | integer | Saves in that net zone. |
| gate_zone_fh_saves_percent | numeric | Save percentage x 100 in that net zone. |
| gate_zone_bl_goals_against | integer | Goals allowed in that net zone. |
| gate_zone_bl_shots_against | integer | Shots on goal faced in that net zone. |
| gate_zone_bl_saves | integer | Saves in that net zone. |
| gate_zone_bl_saves_percent | numeric | Save percentage x 100 in that net zone. |
| gate_zone_ce_goals_against | integer | Goals allowed in that net zone. |
| gate_zone_ce_shots_against | integer | Shots on goal faced in that net zone. |
| gate_zone_ce_saves | integer | Saves in that net zone. |
| gate_zone_ce_saves_percent | numeric | Save percentage x 100 in that net zone. |
| rank | integer | Rank among the game's goalies; null in DEL and EIHL. |

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
[`metal_pbp()`](https://fastRhockey.sportsdataverse.org/reference/metal_pbp.md),
[`metal_player_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_player_box.md),
[`metal_schedule()`](https://fastRhockey.sportsdataverse.org/reference/metal_schedule.md),
[`metal_standings()`](https://fastRhockey.sportsdataverse.org/reference/metal_standings.md),
[`metal_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/metal_top_scorers.md)

## Examples

``` r
# \donttest{
  try(metal_goalie_box(match_id = 575))
#> ── Metal Ligaen goalie_box from the eSports hokejovyzapis bucket ───────────────
#> ℹ Data updated: 2026-10-18 12:08:42 UTC
#> # A tibble: 4 × 60
#>   player_id name     surname    photo birthdate  team_id team_name team_shortcut
#>       <int> <chr>    <chr>      <chr> <chr>        <int> <chr>     <chr>        
#> 1       400 Mathieu  Caron      NA    2000-03-29       8 "Rødovre… "ROD"        
#> 2       412 Kristers Steinbergs NA    2005-05-23       4 "Aalborg… "AAL"        
#> 3       322 Bertram  Witomski   NA    2006-08-11       0 ""        ""           
#> 4       413 Edvin    Olofsson   NA    1998-06-17       0 ""        ""           
#> # ℹ 52 more variables: team_type <chr>, jersey <int>, matches <int>,
#> #   won_matches <int>, lost_matches <int>, saves <int>, goals_against <int>,
#> #   shots_against <int>, saves_percent <dbl>, average <dbl>, zeros <int>,
#> #   time_on_ice <int>, seconds_per_game <dbl>, minutes_in_play <dbl>,
#> #   visible_games <lgl>, visible_shutouts <lgl>, visible_wins <lgl>,
#> #   stars_games <int>, stars_shutouts <int>, stars_wins <int>,
#> #   milestone_games <dbl>, milestone_shutouts <dbl>, milestone_wins <dbl>, …
# }
```
