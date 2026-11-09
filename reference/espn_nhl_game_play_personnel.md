# **Get ESPN NHL Game Play Personnel (core-v2)**

**Get ESPN NHL Game Play Personnel (core-v2)**

## Usage

``` r
espn_nhl_game_play_personnel(event_id, cid = event_id, play_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- play_id:

  ESPN play identifier.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (competitor, athlete) pair
on the play. For NHL this endpoint is typically sparse: the competitor
block is present but the `entries` array is empty, yielding one row per
team with `NA` athlete fields. Returns an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) if the endpoint
is not available.

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| play_id | character | ESPN play id (echoed from arg). |
| competitor_id | character | ESPN team/competitor id for this personnel block. |
| competitor_ref | character | `$ref` URL for the competitor object. |
| athlete_ref | character | `$ref` URL for the athlete (NA if entries are empty). |
| athlete_id | character | Athlete id parsed from the `$ref` (NA if entries empty). |

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
    pls <- espn_nhl_game_plays(event_id = eid)
    espn_nhl_game_play_personnel(event_id = eid, play_id = pls$id[1])
  })
#> ── NHL Game Play Personnel data from ESPN core-v2 ───────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:30 UTC
#> # A tibble: 2 × 7
#>   event_id  cid      play_id competitor_id competitor_ref athlete_ref athlete_id
#>   <chr>     <chr>    <chr>   <chr>         <chr>          <chr>       <chr>     
#> 1 401688263 4016882… 401688… 5             http://sports… NA          NA        
#> 2 401688263 4016882… 401688… 4             http://sports… NA          NA        
# }
```
