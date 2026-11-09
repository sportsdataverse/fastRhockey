# **Get ESPN NHL Game Teams (Competitors Collection, core-v2)**

**Get ESPN NHL Game Teams (Competitors Collection, core-v2)**

## Usage

``` r
espn_nhl_game_teams(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per competitor (2 rows for a
regular NHL game):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| team_id | character | ESPN competitor/team id. |
| uid | character | Competitor uid string. |
| type | character | Competitor type (e.g. "team"). |
| order | integer | Competitor display order. |
| home_away | character | `"home"` or `"away"`. |
| winner | logical | Whether this competitor won the game. |
| team_ref | character | `$ref` URL for the team-in-season object. |
| score_ref | character | `$ref` URL for the score object. |
| linescores_ref | character | `$ref` URL for the per-period linescores. |
| roster_ref | character | `$ref` URL for the game roster. |
| statistics_ref | character | `$ref` URL for the team game statistics. |
| leaders_ref | character | `$ref` URL for the team game leaders. |
| record_ref | character | `$ref` URL for the team record at the time of the game. |
| competitor_ref | character | `$ref` URL for this competitor object. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_nhl_scoreboard(dates = "20250110")
    espn_nhl_game_teams(event_id = sb$game_id[1])
  })
#> ── NHL Game Teams data from ESPN core-v2 ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:13 UTC
#> # A tibble: 2 × 16
#>   event_id  cid    team_id uid   type  order home_away winner team_ref score_ref
#>   <chr>     <chr>  <chr>   <chr> <chr> <int> <chr>     <lgl>  <chr>    <chr>    
#> 1 401688263 40168… 5       s:70… team      0 home      TRUE   http://… http://s…
#> 2 401688263 40168… 4       s:70… team      1 away      FALSE  http://… http://s…
#> # ℹ 6 more variables: linescores_ref <chr>, roster_ref <chr>,
#> #   statistics_ref <chr>, leaders_ref <chr>, record_ref <chr>,
#> #   competitor_ref <chr>
# }
```
