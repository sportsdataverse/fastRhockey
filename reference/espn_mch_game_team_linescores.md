# **Get ESPN MCH / WCH Game Team Linescores (core-v2)**

**Get ESPN MCH / WCH Game Team Linescores (core-v2)**

## Usage

``` r
espn_mch_game_team_linescores(event_id, cid = event_id, team_id, ...)

espn_wch_game_team_linescores(event_id, cid = event_id, team_id, ...)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_team_linescores()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_team_linescores.md)
(columns inherited); a collection may be empty.

`espn_wch_game_team_linescores()` is the same wrapper for women's
college hockey (league slug `womens-college-hockey`): same arguments,
same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_mch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_mch_game_teams(event_id = eid)
    espn_mch_game_team_linescores(event_id = eid, team_id = tms$team_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game Team Linescores data from ESPN core-v2 ─────────────
#> ℹ Data updated: 2026-11-09 17:18:24 UTC
#> # A tibble: 3 × 9
#>   event_id  cid       team_id period value display_value source_id source_desc 
#>   <chr>     <chr>     <chr>    <int> <dbl> <chr>         <chr>     <chr>       
#> 1 401715296 401715296 194          1     1 1             1         basic/manual
#> 2 401715296 401715296 194          2     1 1             1         basic/manual
#> 3 401715296 401715296 194          3     3 3             1         basic/manual
#> # ℹ 1 more variable: linescore_ref <chr>
# }
# \donttest{
  try({
    sb  <- espn_wch_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_wch_game_teams(event_id = eid)
    espn_wch_game_team_linescores(event_id = eid, team_id = tms$team_id[1])
  })
#> ✖ 2026-11-09 17:18:24.75564: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> ✖ 2026-11-09 17:18:24.776107: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL, team_id = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
