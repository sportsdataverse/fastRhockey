# **Get ESPN NHL Games (core-v2)**

**Get ESPN NHL Games (core-v2)**

## Usage

``` r
espn_nhl_games(dates = NULL, limit = 100, ...)
```

## Arguments

- dates:

  Optional date string `"YYYYMMDD"` or range `"YYYYMMDD-YYYYMMDD"`. When
  `NULL` (default) the most recent games are returned.

- limit:

  Maximum number of events per page (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game event reference:

|            |           |                                              |
|------------|-----------|----------------------------------------------|
| col_name   | types     | description                                  |
| ref        | character | `$ref` URL for the event object.             |
| event_id   | character | ESPN event id parsed from the `$ref` URL.    |
| count      | integer   | Total events in the collection.              |
| page_count | integer   | Total pages in the collection.               |
| dates      | character | Dates filter (echoed from arg, if supplied). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_games(limit = 10))
#> ── NHL Games data from ESPN core-v2 ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:13 UTC
#> # A tibble: 3 × 4
#>   ref                                                  event_id count page_count
#>   <chr>                                                <chr>    <int>      <int>
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/le… 4018926…     3          1
#> 2 http://sports.core.api.espn.com/v2/sports/hockey/le… 4018926…     3          1
#> 3 http://sports.core.api.espn.com/v2/sports/hockey/le… 4018926…     3          1
# }
```
