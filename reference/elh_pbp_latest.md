# **Czech Extraliga (ELH) latest play-by-play entries**

The newest 20 play-by-play entries of one game
(`match/{season - 1}/short/{match_id}.json`); same columns as
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).

## Usage

``` r
elh_pbp_latest(season, match_id, return_parsed = TRUE)
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

A tibble (`return_parsed = TRUE`; zero rows for a malformed body), else
the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| comment_id | integer | Onlajny commentary entry id (integer); equals the mobile file's comment id. |
| sort_order | numeric | Provider sort key (integer-valued double: values exceed the 32-bit integer range); ascending = chronological, the order rows are returned in. |
| event_type | character | Entry kind: goal, penalty, time (period start/end), important, normal, expertComment. |
| label2 | character | Secondary label; empty (null) in every sample. |
| time_type | character | For time entries: begin or end of a period; null otherwise. |
| phase | character | Phase of a clockless entry: 0 pre-game, 1INT / 2INT intermissions, 3PER third period, ERT after the game (meanings inferred from the entries, undocumented); null when the entry has a clock. |
| clock | character | Elapsed game clock mm:ss (00:01 = game start); null on a clockless entry. |
| game_seconds | integer | Seconds from the start of the game. |
| period | integer | Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry. |
| home_score | integer | Home goals (after the entry on play-by-play rows; final on the scoreboard). |
| away_score | integer | Away goals (after the entry on play-by-play rows; final on the scoreboard). |
| home_shootout_score | integer | Home shootout goals after the entry (0 without a shootout). |
| away_shootout_score | integer | Away shootout goals after the entry (0 without a shootout). |
| event_id | integer | Onlajny event id of a goal or penalty (integer); null on other entries. |
| onlajny_team_id | integer | Onlajny team id of the team credited with the goal or penalty (Sparta = 156); NOT the hokej.cz team id. The scoreboard maps it. |
| onlajny_team_global_id | integer | Onlajny global team id (equal to onlajny_team_id in every sample). |
| team_code | character | Team code of the credited team, e.g. SPA (same as the shots / scoreboard codes). |
| onlajny_player1_id | integer | Onlajny player id of slot 1 (goal scorer / penalised player); NOT the hokej.cz id of the shots; null for an unused slot. |
| onlajny_player1_global_id | integer | Onlajny GLOBAL player id of slot 1 (goal scorer / penalised player); can differ from onlajny_player1_id (Hradec 48287 / 76559). |
| player1_name | character | Display name of slot 1 (goal scorer / penalised player). |
| player1_first_name | character | Given name of slot 1 (goal scorer / penalised player); null in 2016-17. |
| player1_last_name | character | Family name of slot 1 (goal scorer / penalised player); null in 2016-17. |
| onlajny_player2_id | integer | Onlajny player id of slot 2 (first assist); NOT the hokej.cz id of the shots; null for an unused slot. |
| onlajny_player2_global_id | integer | Onlajny GLOBAL player id of slot 2 (first assist); can differ from onlajny_player2_id (Hradec 48287 / 76559). |
| player2_name | character | Display name of slot 2 (first assist). |
| player2_first_name | character | Given name of slot 2 (first assist); null in 2016-17. |
| player2_last_name | character | Family name of slot 2 (first assist); null in 2016-17. |
| onlajny_player3_id | integer | Onlajny player id of slot 3 (second assist); NOT the hokej.cz id of the shots; null for an unused slot. |
| onlajny_player3_global_id | integer | Onlajny GLOBAL player id of slot 3 (second assist); can differ from onlajny_player3_id (Hradec 48287 / 76559). |
| player3_name | character | Display name of slot 3 (second assist). |
| player3_first_name | character | Given name of slot 3 (second assist); null in 2016-17. |
| player3_last_name | character | Family name of slot 3 (second assist); null in 2016-17. |
| no_assist | integer | Unassisted flag on slot 1 (0 in every sample; 1 = unassisted is inferred, unobserved). Absent before 2026-27 samples. |
| penalty_minutes | integer | Penalty minutes of the penalised player (player1); two penalties are summed. Null in 2016-17. |
| penalty_offense | character | Czech offense code(s), e.g. VYSH (high sticking), PODR (tripping); comma-joined for two penalties. Null in 2016-17. |
| strength | character | Strength of a goal: EQ (read as even strength; inferred, the only value observed). Null where the on-ice block is absent (all of 2016-17). |
| message | character | Czech commentary text (may contain `<br />`). |
| written | character | When the entry was written, local time YYYY-MM-DD HH:MM:SS. |
| last_change | character | Last edit of the entry, local time YYYY-MM-DD HH:MM:SS. |

## Errors

As
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_pbp_latest(season = 2027, match_id = 2928321))
#> ── ELH Pbp Latest Information from hokej.cz ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-01 23:53:14 UTC
#> # A tibble: 20 × 39
#>    comment_id  sort_order event_type   label2 time_type phase clock game_seconds
#>         <int>       <dbl> <chr>        <chr>  <chr>     <chr> <chr>        <int>
#>  1   70333891 70333891000 normal       NA     NA        NA    50:06         3006
#>  2   70333921 70333921000 normal       NA     NA        NA    50:59         3059
#>  3   70334016 70334016000 important    NA     NA        NA    51:35         3095
#>  4   70334081 70334041000 normal       NA     NA        NA    52:12         3132
#>  5   70334041 70334081000 expertComme… NA     NA        3PER  NA              NA
#>  6   70334158 70334158000 normal       NA     NA        NA    52:43         3163
#>  7   70334217 70334217000 normal       NA     NA        NA    53:54         3234
#>  8   70334219 70334311100 goal         NA     NA        NA    54:18         3258
#>  9   70334333 70334333000 normal       NA     NA        NA    55:00         3300
#> 10   70334386 70334386000 normal       NA     NA        NA    56:27         3387
#> 11   70334504 70334504000 normal       NA     NA        NA    57:46         3466
#> 12   70334535 70334535000 normal       NA     NA        NA    58:00         3480
#> 13   70334575 70334575000 normal       NA     NA        NA    58:47         3527
#> 14   70334608 70334608000 normal       NA     NA        NA    59:27         3567
#> 15   70334662 70334662000 normal       NA     NA        NA    59:49         3589
#> 16   70334666 70334666000 time         NA     end       ERT   NA              NA
#> 17   70335226 70334688000 normal       NA     NA        ERT   NA              NA
#> 18   70334741 70334704000 normal       NA     NA        ERT   NA              NA
#> 19   70334688 70334741000 normal       NA     NA        ERT   NA              NA
#> 20   70334704 70335226000 normal       NA     NA        ERT   NA              NA
#> # ℹ 31 more variables: period <int>, home_score <int>, away_score <int>,
#> #   home_shootout_score <int>, away_shootout_score <int>, event_id <int>,
#> #   onlajny_team_id <int>, onlajny_team_global_id <int>, team_code <chr>,
#> #   onlajny_player1_id <int>, onlajny_player1_global_id <int>,
#> #   player1_name <chr>, player1_first_name <chr>, player1_last_name <chr>,
#> #   onlajny_player2_id <int>, onlajny_player2_global_id <int>,
#> #   player2_name <chr>, player2_first_name <chr>, player2_last_name <chr>, …
# }
```
