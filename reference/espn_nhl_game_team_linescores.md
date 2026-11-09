# **Get ESPN NHL Game Team Linescores (core-v2)**

**Get ESPN NHL Game Team Linescores (core-v2)**

## Usage

``` r
espn_nhl_game_team_linescores(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per period (3 rows for a
regulation game, up to 4+ in overtime):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| team_id | character | ESPN team id (echoed from arg). |
| period | integer | Period number (1 = 1st, 2 = 2nd, 3 = 3rd, 4+ = OT). |
| value | numeric | Goals scored in this period. |
| display_value | character | Display string for goals scored. |
| source_id | character | Data source identifier. |
| source_desc | character | Data source description (e.g. "feed"). |
| linescore_ref | character | `$ref` URL for this period linescore object. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_nhl_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_nhl_game_teams(event_id = eid)
    espn_nhl_game_team_linescores(event_id = eid, team_id = tms$team_id[1])
  })
#> ── NHL Game Team Linescores data from ESPN core-v2 ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:52 UTC
#> # A tibble: 3 × 9
#>   event_id  cid       team_id period value display_value source_id source_desc
#>   <chr>     <chr>     <chr>    <int> <dbl> <chr>         <chr>     <chr>      
#> 1 401688263 401688263 5            1     1 1             2         feed       
#> 2 401688263 401688263 5            2     1 1             2         feed       
#> 3 401688263 401688263 5            3     3 3             2         feed       
#> # ℹ 1 more variable: linescore_ref <chr>
# }
```
