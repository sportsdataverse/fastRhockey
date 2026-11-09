# **Get ESPN NHL Coaches (core-v2)**

**Get ESPN NHL Coaches (core-v2)**

## Usage

``` r
espn_nhl_coaches(season = most_recent_nhl_season(), limit = 100, page = 1, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).
  The league-level coaches collection (`/leagues/nhl/coaches`) returns
  HTTP 404 for NHL; this function uses the season-scoped path
  (`seasons/{season}/coaches`) which is confirmed to work.

- limit:

  Maximum number of coaches to return per page (default `100`). The NHL
  typically lists ~30 head coaches per season.

- page:

  Page number (1-indexed, default `1`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per coach reference:

|            |           |                                            |
|------------|-----------|--------------------------------------------|
| col_name   | types     | description                                |
| ref        | character | `$ref` URL for the coach-in-season object. |
| coach_id   | character | ESPN coach id parsed from the `$ref` URL.  |
| season     | integer   | Season year (echoed from arg).             |
| page       | integer   | Page number (echoed from arg).             |
| count      | integer   | Total coaches in the collection.           |
| page_count | integer   | Total pages in the collection.             |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_coaches(season = 2026))
#> ── NHL Coaches data from ESPN core-v2 ───────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:23 UTC
#> # A tibble: 32 × 6
#>    ref                                    coach_id season  page count page_count
#>    <chr>                                  <chr>     <int> <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sp… 900        2026     1    32          1
#>  2 http://sports.core.api.espn.com/v2/sp… 2018453    2026     1    32          1
#>  3 http://sports.core.api.espn.com/v2/sp… 4626       2026     1    32          1
#>  4 http://sports.core.api.espn.com/v2/sp… 2591437    2026     1    32          1
#>  5 http://sports.core.api.espn.com/v2/sp… 2017787    2026     1    32          1
#>  6 http://sports.core.api.espn.com/v2/sp… 5013       2026     1    32          1
#>  7 http://sports.core.api.espn.com/v2/sp… 105        2026     1    32          1
#>  8 http://sports.core.api.espn.com/v2/sp… 2017380    2026     1    32          1
#>  9 http://sports.core.api.espn.com/v2/sp… 2560932    2026     1    32          1
#> 10 http://sports.core.api.espn.com/v2/sp… 883        2026     1    32          1
#> # ℹ 22 more rows
# }
```
