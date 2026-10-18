# **Czech Extraliga (ELH) shot attempts**

Shot attempts of one game with rink coordinates and result code
(`visualization/shots/{match_id}.json`).

Ids are **hokej.cz** ids (`hokejcz_player_id`, `hokejcz_team_id`), not
the onlajny ids of
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).
The provider's `match_shot_resutl_id` (sic) is kept and aliased
`shot_result_id`: `4` = goal (`is_goal`, verified against the
play-by-play); `1`/`2`/`3`/`5` are unknown. `time` is seconds from the
start of the game, EXCEPT in 2016-17, where it is only the period's
start offset (0/1200/2400; use `real_date`). `period` is derived from
`time` and holds in both eras. The faceoff win percentage block of the
file is left out (`return_parsed = FALSE` keeps it).

## Usage

``` r
elh_shots(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  hokej.cz game id (the S3 file name; `hokejcz_game_id` of
  [`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md)
  /
  [`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md)),
  e.g. 2928321. Not the onlajny game id.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list.

## Value

A tibble (`return_parsed = TRUE`; zero rows for a malformed body), else
the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| hokejcz_game_id | integer | hokej.cz game id (the S3 file name; shots match.id, scoreboard hokejcz_id). integer. |
| game_date | character | Game date (shots: YYYY-MM-DD string; scoreboard: Date). |
| hokejcz_home_team_id | integer | hokej.cz team id of the home team (Sparta = 8); NOT the onlajny id of the play-by-play. |
| home_team_name | character | Home team full name. |
| home_team_code | character | Home team code, e.g. SPA. |
| hokejcz_away_team_id | integer | hokej.cz team id of the away team (Vitkovice = 7); NOT the onlajny id of the play-by-play. |
| away_team_name | character | Away team full name. |
| away_team_code | character | Away team code, e.g. VIT. |
| shot_id | integer | Feed id of the shot event. |
| hokejcz_player_id | integer | hokej.cz player id of the shooter (Hradec = 10093378); NOT an onlajny id. No player crosswalk is published. |
| jersey | integer | Shooter's jersey number (the join key for a per-game crosswalk with the play-by-play names). |
| first_name | character | Shooter's given name. |
| last_name | character | Shooter's family name. |
| hokejcz_team_id | integer | hokej.cz team id of the shooting team (equals the home or away team id). |
| team_side | character | The team's side: home or away. |
| match_shot_resutl_id | integer | Shot result code, provider's typo kept: 4 = goal (verified against the play-by-play); 1/2/3/5 unknown. |
| shot_result_id | integer | Alias of match_shot_resutl_id: 4 = goal; 1/2/3/5 unknown. |
| is_goal | logical | True when the result code is 4 (goal). |
| time | integer | Seconds from the start of the game; in 2016-17 only the period's start offset (0 / 1200 / 2400), use real_date. |
| period | integer | Period number derived from the game clock (20-minute periods; 4 = overtime); null on a clockless entry. |
| coordinate_x | numeric | Shot position along the rink LENGTH (goal to goal), centred at 0, normalised so the home team shoots at +x and the away team at -x; about -100..100. The scaling (units) and the orientation are inferred from the data, not documented by the provider. |
| coordinate_y | numeric | Shot position across the rink WIDTH (board to board), centred at 0; about -100..100. The scaling (each axis looks independently scaled to -100..100) and which board is positive are inferred from the data, not documented by the provider. |
| real_date | character | Wall-clock time of the shot, local YYYY-MM-DD HH:MM:SS. |

## Details

**Coordinate caveat (unverified).** `coordinate_x` runs along the rink
LENGTH (goal to goal), centred at 0 and normalised so the home team
shoots at +x and the away team at -x; `coordinate_y` runs across the
WIDTH; both are about -100..100. That scaling and orientation are
inferred from the data: the provider documents neither, and they have
not been verified against a rink diagram or an independent source. Treat
the coordinates as relative positions, not rink feet, until verified.

## Errors

`fastRhockey_no_data`: a missing file (404) or a file without shots.
`fastRhockey_asset_fetch`: an error envelope, a body for another game,
an empty or non-JSON 200, or any other non-2xx.
`fastRhockey_bad_request`: HTTP 400/422 or a bad `match_id`.

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md)

## Examples

``` r
# \donttest{
  try(elh_shots(match_id = 2928321))
#> ── ELH Shots Information from hokej.cz ─────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:02:34 UTC
#> # A tibble: 121 × 23
#>    hokejcz_game_id game_date  hokejcz_home_team_id home_team_name home_team_code
#>              <int> <chr>                     <int> <chr>          <chr>         
#>  1         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  2         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  3         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  4         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  5         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  6         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  7         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  8         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#>  9         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#> 10         2928321 2026-10-11                    8 HC Sparta Pra… SPA           
#> # ℹ 111 more rows
#> # ℹ 18 more variables: hokejcz_away_team_id <int>, away_team_name <chr>,
#> #   away_team_code <chr>, shot_id <int>, hokejcz_player_id <int>, jersey <int>,
#> #   first_name <chr>, last_name <chr>, hokejcz_team_id <int>, team_side <chr>,
#> #   match_shot_resutl_id <int>, shot_result_id <int>, is_goal <lgl>,
#> #   time <int>, period <int>, coordinate_x <dbl>, coordinate_y <dbl>,
#> #   real_date <chr>
# }
```
