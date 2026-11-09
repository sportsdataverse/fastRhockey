# Team stats per period and total (2023/24 on)

Wraps `GET https://www.shl.se/api/gameday/team-stats/{game_uuid}` of the
SHL site API (www.shl.se). Unofficial use of a public source; seasons
are END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_team_stats(game_uuid, return_parsed = TRUE)
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
| team_side | character | Side of the game: home or away. |
| game_source_id | character | StatNet game key, date-HOME-AWAY (e.g. 20250913-FHC-LHC); the same game as gameId. |
| game_id | integer | StatNet game number (StatNet key space, e.g. 22003); keys stats-info games and equals sports-v2 game-info extId. Not the game uuid. |
| place | character | Side of the game: home or away. |
| score | integer | Goals scored by the team in the game. |
| team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |
| team_name | character | Name of the team. |
| team_code | character | StatNet team code (SAIK, FHC). |
| game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
| period | integer | Period: 0 = the whole game, 1-3 the periods, 4 and up overtime. |
| g | numeric | Number of goals scored. |
| pim | numeric | Penalty minutes. |
| fow | numeric | Number of face-offs won. |

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
  try(shl_team_stats(game_uuid = "bdhvuc5tex"))
#> ── SHL Team Stats Information from www.shl.se ───────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:54:17 UTC
#> # A tibble: 8 × 30
#>   team_side game_source_id   game_id place score team_id team_name     team_code
#>   <chr>     <chr>              <int> <chr> <int> <chr>   <chr>         <chr>    
#> 1 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 2 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 3 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 4 home      20250913-FHC-LHC   22003 home      0 FHC     Frölunda HC   FHC      
#> 5 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> 6 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> 7 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> 8 away      20250913-FHC-LHC   22003 away      0 LHC     Linköpings HC LHC      
#> # ℹ 22 more variables: game_uuid <chr>, period <int>, g <dbl>, pim <dbl>,
#> #   fow <dbl>, sog <dbl>, spg <dbl>, ppsog <dbl>, saves <dbl>, ga <dbl>,
#> #   saves_per_shot <dbl>, pp_perc <dbl>, sh_perc <dbl>, ppg <dbl>, shga <dbl>,
#> #   shg <dbl>, ppga <dbl>, num_pp <dbl>, num_sh <dbl>, hits <dbl>, bk_s <dbl>,
#> #   si_bk <dbl>
# }
```
