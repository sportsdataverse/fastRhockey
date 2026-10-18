# **National League play-by-play**

One game's event timeline: goals with assists, penalties, goaltender
changes, revoked goals.

## Usage

``` r
nl_pbp(game_id, return_parsed = TRUE)
```

## Arguments

- game_id:

  SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>`
  (`game_id` in
  [`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md);
  the same id as the SIHF family's `game_id`), e.g. `20261105000325`. A
  whole number or a digit string; 14 digits are exact as a double, so do
  not format them through `integer`.

- return_parsed:

  Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble;
  `FALSE` returns the raw JSON body as a nested list (api keys already
  dropped).

## Value

A `fastRhockey_data` tibble (34 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
| event_index | integer | 1-based position of the event in the game's timeline. |
| team_side | character | home or away: the side of the team this row belongs to. |
| game_seconds | integer | Elapsed game time of the event in seconds, from the MMSS `time` (822 = 8:22 -\> 502). |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| home_team | logical | Event belongs to the home team |
| time | character | Game clock elapsed as an MMSS string without separator ('822' = 8:22, '5954' = 59:54) |
| video_time | integer | Seconds into the game video; 0 when there is no clip |
| video_vimeo_id | numeric | Clip of this event. Vimeo video id (whole number): vimeo.com/`<id>`. |
| action | character | Event type: goal, foul (penalty), in / out (goaltender enters / leaves), revokedGoal |
| foul_type | integer | SIHF penalty code (116-125 in the sample); 0 on non-penalty events |
| foul_minutes | integer | Penalty minutes; 0 on non-penalty events |
| player_first_name | character | Shooter's first name, from the game's lineup. |
| player_last_name | character | Shooter's last name, from the game's lineup. |
| player_number | character | Shooter's sweater number, from the game's lineup (a string). |
| player_total | integer | Scorer's goals this season including this one; 0 on non-goal events |
| period | integer | Period number, an integer (the parent `third` is a string) |
| overtime | integer | Overtime period number; 0 in regulation |
| goal_situation | character | Goal kind; 'normal' on every sampled event |
| home_team_result | integer | Home score after the event |
| away_team_result | integer | Away score after the event |
| assist1_first_name | character | First assist: first name |
| assist1_last_name | character | First assist: last name |
| assist1_number | character | First assist: sweater number, a string |
| assist1_total | integer | First assister's assists this season including this one |
| assist2_first_name | character | Second assist: first name |
| assist2_last_name | character | Second assist: last name |
| assist2_number | character | Second assist: sweater number, a string |
| assist2_total | integer | Second assister's assists this season including this one |
| situation | character | Goal tags, comma-separated: EQ (even strength), PP1 (power play), GWG (game-winning goal); null on non-goals |
| revoked_goal_type | integer | Revoked-goal category code (1 on every sampled event) |
| revoked_goal_sub_type | integer | Revoked-goal reason code (1 on the revoked goal, 0 elsewhere) |
| review_successful | character | Video-review outcome; unobserved: null or empty in every sampled row |
| review_goal_counts | logical | Goal stood after review |

## Details

**robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows
`/api`. The owner approved building on it on 2026-10-12, for polite,
incremental use only: requests are spaced at least one second apart (on
every attempt, retries included), and a bulk backfill or a published
mirror needs the owner's sign-off first. Crawl incrementally: fetch a
finished game once (keyed by `game_id`), find new games with one
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md)
call per run, and fetch season tables at most once per game day.

Seasons are END years and go on the wire unchanged. Ids (`*_id`) are
whole-number `numeric` (a 14-digit `game_id` exceeds R's 32-bit
integer); every other whole-number column is `integer`. The club backend
key the API ships in `/teams/{id}/info` is dropped before parsing and
never returned.

Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.

The six game-page wrappers
([`nl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/nl_game_overview.md),
`nl_pbp()`,
[`nl_lineups()`](https://fastRhockey.sportsdataverse.org/reference/nl_lineups.md),
[`nl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_box.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md))
each read the same `/games/{gameId}` body, one request each.

## Errors

Raises a classed condition (all inherit `fastRhockey_error`):
`fastRhockey_no_data` for a 404 or an empty answer (no games or rows for
that season, team or phase), `fastRhockey_asset_fetch` for a failed
fetch, an error object in a 200, a body without the ids it must carry,
or an answer for another season, team, game or player, and
`fastRhockey_bad_request` for a 400/422 or an argument that is not a
whole number.

## See also

Other National League Functions:
[`nl_current_games()`](https://fastRhockey.sportsdataverse.org/reference/nl_current_games.md),
[`nl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/nl_game_overview.md),
[`nl_lineups()`](https://fastRhockey.sportsdataverse.org/reference/nl_lineups.md),
[`nl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_box.md),
[`nl_player_season()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_season.md),
[`nl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_stats.md),
[`nl_playoffs()`](https://fastRhockey.sportsdataverse.org/reference/nl_playoffs.md),
[`nl_playouts()`](https://fastRhockey.sportsdataverse.org/reference/nl_playouts.md),
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md),
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md),
[`nl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nl_standings.md),
[`nl_team_info()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_info.md),
[`nl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_player_stats.md),
[`nl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_schedule.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_pbp(game_id = 20261105000325))
#> ── National League Play-by-Play Information from nationalleague.ch ─────────────
#> ℹ Data updated: 2026-10-18 05:50:24 UTC
#> # A tibble: 17 × 34
#>    game_id event_index team_side game_seconds team_id home_team time  video_time
#>      <dbl>       <int> <chr>            <int>   <dbl> <lgl>     <chr>      <int>
#>  1 2.03e13           1 home               502  101151 TRUE      822          588
#>  2 2.03e13           2 away               825  102127 FALSE     1345           0
#>  3 2.03e13           3 away              1347  102127 FALSE     2227        2993
#>  4 2.03e13           4 home              1374  101151 TRUE      2254        3092
#>  5 2.03e13           5 away              1453  102127 FALSE     2413        3248
#>  6 2.03e13           6 home              1558  101151 TRUE      2558           0
#>  7 2.03e13           7 away              1607  102127 FALSE     2647        3581
#>  8 2.03e13           8 home              1798  101151 TRUE      2958           0
#>  9 2.03e13           9 away              2184  102127 FALSE     3624           0
#> 10 2.03e13          10 away              2184  102127 FALSE     3624           0
#> 11 2.03e13          11 home              2648  101151 TRUE      4408        6276
#> 12 2.03e13          12 away              2726  102127 FALSE     4526           0
#> 13 2.03e13          13 home              2783  101151 TRUE      4623        6529
#> 14 2.03e13          14 away              3243  102127 FALSE     5403           0
#> 15 2.03e13          15 away              3477  102127 FALSE     5757           0
#> 16 2.03e13          16 home              3559  101151 TRUE      5919           0
#> 17 2.03e13          17 home              3562  101151 TRUE      5922           0
#> # ℹ 26 more variables: video_vimeo_id <dbl>, action <chr>, foul_type <int>,
#> #   foul_minutes <int>, player_first_name <chr>, player_last_name <chr>,
#> #   player_number <chr>, player_total <int>, period <int>, overtime <int>,
#> #   goal_situation <chr>, home_team_result <int>, away_team_result <int>,
#> #   assist1_first_name <chr>, assist1_last_name <chr>, assist1_number <chr>,
#> #   assist1_total <int>, assist2_first_name <chr>, assist2_last_name <chr>,
#> #   assist2_number <chr>, assist2_total <int>, situation <chr>, …
# }
```
