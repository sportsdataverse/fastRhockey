# Post-game team comparison

Wraps
`GET https://www.shl.se/api/gameday/post-game-data/team-stats/{game_uuid}`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_post_game_team_stats(
  game_uuid,
  ssgt_uuid,
  home_team_uuid,
  away_team_uuid,
  return_parsed = TRUE
)
```

## Arguments

- game_uuid:

  Game uuid (gameInfo\[\].uuid from /sports-v2/game-schedule).
  gameday/\* has data from 2023/24 on; older games answer 200 with an
  empty body.

- ssgt_uuid:

  Required. ssgt uuid.

- home_team_uuid:

  Required. Home team uuid; without it the home side comes back blank.

- away_team_uuid:

  Required. Away team uuid; without it the away side comes back blank.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| home_team_side_translate_key | character | Translation key of the side. |
| home_team_left_type | character | Stat code compared on the left (e.g. G, PPG). |
| home_team_left_value | integer | Value (string-encoded). |
| home_team_right_type | character | Stat code compared on the right (e.g. SOG, PPOpp). |
| home_team_right_value | integer | Value (string-encoded). |
| home_team_center_type | character | Stat code compared in the middle (e.g. SEff, PPPerc). |
| home_team_center_value | character | Value (string-encoded). Numeric, but sent as a string in some rows and as a number in others. |
| away_team_side_translate_key | character | Translation key of the side. |
| away_team_left_type | character | Stat code compared on the left (e.g. G, PPG). |
| away_team_left_value | integer | Value (string-encoded). |
| away_team_right_type | character | Stat code compared on the right (e.g. SOG, PPOpp). |
| away_team_right_value | integer | Value (string-encoded). |
| away_team_center_type | character | Stat code compared in the middle (e.g. SEff, PPPerc). |
| away_team_center_value | character | Value (string-encoded). Numeric, but sent as a string in some rows and as a number in others. |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

## See also

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_post_game_team_stats(game_uuid = "bdhvuc5tex", ssgt_uuid = "iuzqg7dqk9",
    home_team_uuid = "087a-087aTQv9u", away_team_uuid = "41c4-41c4BiYZU"))
#> ── SHL Post Game Team Stats Information from www.shl.se ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:54:07 UTC
#> # A tibble: 3 × 14
#>   home_team_side_translate_key home_team_left_type home_team_left_value
#>   <chr>                        <chr>                              <int>
#> 1 G                            G                                      2
#> 2 PPG                          PPG                                    1
#> 3 PIM                          PIM                                    4
#> # ℹ 11 more variables: home_team_right_type <chr>, home_team_right_value <int>,
#> #   home_team_center_type <chr>, home_team_center_value <chr>,
#> #   away_team_side_translate_key <chr>, away_team_left_type <chr>,
#> #   away_team_left_value <int>, away_team_right_type <chr>,
#> #   away_team_right_value <int>, away_team_center_type <chr>,
#> #   away_team_center_value <chr>
# }
```
