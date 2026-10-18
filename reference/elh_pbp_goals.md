# **Czech Extraliga (ELH) players on the ice at each goal**

Players on the ice at each goal of one game (onlajny ids), from the full
play-by-play (`match/{season - 1}/full/{match_id}.json`). A goal entry's
on-ice block lists both teams' skaters and goalies (up to six players
each) and the strength. Every player is a row tagged with the scoring
team, so plus/minus and on-ice counts fall out of a group-by. A goal
without that block (every 2016-17 goal) is ONE row with `NA` on-ice
columns, so no goal is dropped.

## Usage

``` r
elh_pbp_goals(season, match_id, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026-27); the bucket files it under the
  START year (`match/2026/`), mapped here. Play-by-play starts in
  2014-15 (season 2015).

- match_id:

  hokej.cz game id (the S3 file name; `hokejcz_game_id` of
  [`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md)
  /
  [`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md)),
  e.g. 2928321. Not the onlajny game id.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (XML-converted: every attribute a string).

## Value

A tibble (`return_parsed = TRUE`; goals oldest first, players in team
then slot order; zero rows for a malformed body), else the raw JSON
list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| comment_id | integer | Onlajny commentary entry id (integer); equals the mobile file's comment id. |
| event_id | integer | Onlajny event id of a goal or penalty (integer); null on other entries. |
| clock | character | Elapsed game clock mm:ss (00:01 = game start); null on a clockless entry. |
| game_seconds | integer | Seconds from the start of the game. |
| period | integer | Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry. |
| strength | character | Strength of a goal: EQ (read as even strength; inferred, the only value observed). Null where the on-ice block is absent (all of 2016-17). |
| home_score | integer | Home goals (after the entry on play-by-play rows; final on the scoreboard). |
| away_score | integer | Away goals (after the entry on play-by-play rows; final on the scoreboard). |
| scoring_onlajny_team_id | integer | Onlajny team id of the scoring team. |
| scoring_team_code | character | Team code of the scoring team, e.g. SPA. |
| on_ice_onlajny_team_id | integer | Onlajny team id of the on-ice player's team; null for a goal without the on-ice block (2016-17). |
| on_ice_team_code | character | Team code of the on-ice player's team. |
| is_scoring_team | logical | True when the on-ice player is on the scoring team. |
| slot | integer | Player slot within the team's on-ice block (1-6). |
| onlajny_player_id | integer | Onlajny player id of the on-ice player (per competition, inferred); NOT the hokej.cz player id of the shots. |
| onlajny_player_global_id | integer | Onlajny GLOBAL player id of the on-ice player; can differ from onlajny_player_id for the same player. |
| player_name | character | On-ice player's display name (surname). |

## Errors

As
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_pbp_goals(season = 2027, match_id = 2928321))
#> ── ELH Pbp Goals Information from hokej.cz ─────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:02:30 UTC
#> # A tibble: 36 × 17
#>    comment_id event_id clock game_seconds period strength home_score away_score
#>         <int>    <int> <chr>        <int>  <int> <chr>         <int>      <int>
#>  1   70328418 13774488 05:06          306      1 EQ                1          0
#>  2   70328418 13774488 05:06          306      1 EQ                1          0
#>  3   70328418 13774488 05:06          306      1 EQ                1          0
#>  4   70328418 13774488 05:06          306      1 EQ                1          0
#>  5   70328418 13774488 05:06          306      1 EQ                1          0
#>  6   70328418 13774488 05:06          306      1 EQ                1          0
#>  7   70328418 13774488 05:06          306      1 EQ                1          0
#>  8   70328418 13774488 05:06          306      1 EQ                1          0
#>  9   70328418 13774488 05:06          306      1 EQ                1          0
#> 10   70328418 13774488 05:06          306      1 EQ                1          0
#> # ℹ 26 more rows
#> # ℹ 9 more variables: scoring_onlajny_team_id <int>, scoring_team_code <chr>,
#> #   on_ice_onlajny_team_id <int>, on_ice_team_code <chr>,
#> #   is_scoring_team <lgl>, slot <int>, onlajny_player_id <int>,
#> #   onlajny_player_global_id <int>, player_name <chr>
# }
```
