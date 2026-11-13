# Goals by period, head-to-head summary, best scorers

Wraps `GET https://www.shl.se/api/gameday/game-info/{game_uuid}` of the
SHL site API (www.shl.se). Unofficial use of a public source; seasons
are END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_game_summary(game_uuid, return_parsed = TRUE)
```

## Arguments

- game_uuid:

  Game uuid (gameInfo\[\].uuid from /sports-v2/game-schedule).
  gameday/\* has data from 2023/24 on; older games answer 200 with an
  empty body.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| team_head2_head | character | Previous meetings of the two teams. |
| team_period_caption | character | Caption text of the image. |
| team_period_overtime | logical | Whether the game went to overtime. |
| team_period_shootout | logical | Whether the game went to a shootout. |
| team_period_stats | character | Team stats per period: rows with home and away values. |
| best_scorers | character | Top scorers of the game. |
| statistics_provider | character | Stats provider of the data (statnet seen). |

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
  try(shl_game_summary(game_uuid = "bdhvuc5tex"))
#> ── SHL Game Summary Information from www.shl.se ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:22:59 UTC
#> # A tibble: 1 × 7
#>   team_head2_head  team_period_caption team_period_overtime team_period_shootout
#>   <chr>            <chr>               <lgl>                <lgl>               
#> 1 "[{\"awayTeam\"… G                   FALSE                FALSE               
#> # ℹ 3 more variables: team_period_stats <chr>, best_scorers <chr>,
#> #   statistics_provider <chr>
# }
```
