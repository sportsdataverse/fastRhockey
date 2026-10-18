# **EIHL Goalie Box**

Goalie lines of one match, one row per goalie who played.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/{match_id}/top-goalies.json`
of the EIHL eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/eihl.hokejovyzapis.cz/matches/4440/top-goalies.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
eihl_goalie_box(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in eihl_schedule). Not
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
| birthdate | character | Birth date `YYYY-MM-DD`. |
| team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| team_name | character | Team display name. |
| team_shortcut | character | Three-letter team code. |
| jersey | integer | Jersey number. |
| matches | integer | Games played (1 in a per-game file). |
| won_matches | integer | Games the goalie won. |
| lost_matches | integer | Games the goalie lost. |
| saves | integer | Shots on goal the goalie stopped. |
| goals_against | integer | Goals the goalie allowed. |
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
[`eihl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/eihl_pbp.md),
[`eihl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/eihl_player_box.md),
[`eihl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/eihl_schedule.md),
[`eihl_shots()`](https://fastRhockey.sportsdataverse.org/reference/eihl_shots.md),
[`eihl_standings()`](https://fastRhockey.sportsdataverse.org/reference/eihl_standings.md),
[`eihl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/eihl_team_player_stats.md),
[`eihl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/eihl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(eihl_goalie_box(match_id = 4440))
#> ── EIHL goalie_box from the eSports hokejovyzapis bucket ───────────────────────
#> ℹ Data updated: 2026-10-18 12:02:16 UTC
#> # A tibble: 6 × 28
#>   player_id name     surname    birthdate team_id team_name team_shortcut jersey
#>       <int> <chr>    <chr>      <chr>       <int> <chr>     <chr>          <int>
#> 1      5136 "Brad"   "Arvaniti… 1997-08-…      97 "Guildfo… "GUI"              1
#> 2      5152 "Rasmus" "Tirronen" 1990-11-…      96 "Glasgow… "GLA"             32
#> 3      5083 "Liam"   "Soulière" 1999-03-…       0 ""        ""                 0
#> 4      5151 "Daniel" "Norton"   2005-05-…       0 ""        ""                 0
#> 5      4124 "Adam "  "Long "    1995-11-…       0 ""        ""                 0
#> 6      5052 "Justin" "Fazio"    1997-05-…       0 ""        ""                 0
#> # ℹ 20 more variables: matches <int>, won_matches <int>, lost_matches <int>,
#> #   saves <int>, goals_against <int>, saves_percent <dbl>, average <dbl>,
#> #   zeros <int>, time_on_ice <int>, seconds_per_game <dbl>,
#> #   minutes_in_play <dbl>, visible_games <lgl>, visible_shutouts <lgl>,
#> #   visible_wins <lgl>, stars_games <int>, stars_shutouts <int>,
#> #   stars_wins <int>, milestone_games <dbl>, milestone_shutouts <dbl>,
#> #   milestone_wins <dbl>
# }
```
