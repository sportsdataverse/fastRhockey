# **Get ESPN NHL Season Weeks (core-v2)**

**Get ESPN NHL Season Weeks (core-v2)**

## Usage

``` r
espn_nhl_season_weeks(season = most_recent_nhl_season(), season_type = 2, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season.
  Defaults to `2`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per week, or an empty tibble
when the endpoint returns no items. **Note**: The NHL does not use
weekly scheduling in the ESPN core-v2 API - this endpoint consistently
returns `count=0` for NHL. An empty tibble is returned gracefully:

|             |           |                                            |
|-------------|-----------|--------------------------------------------|
| col_name    | types     | description                                |
| ref         | character | `$ref` URL for the week object.            |
| week        | integer   | Week number parsed from the `$ref` URL.    |
| season      | integer   | Season year (echoed from arg).             |
| season_type | integer   | Season type code (echoed from arg).        |
| count       | integer   | Total weeks in the collection (0 for NHL). |
| page_count  | integer   | Total pages in the collection.             |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not populate weeks for hockey)
  try(espn_nhl_season_weeks(season = 2026, season_type = 2))
#> ── NHL Season Weeks data from ESPN core-v2 ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:51:35 UTC
#> # A tibble: 25 × 6
#>    ref                                  week season season_type count page_count
#>    <chr>                               <int>  <int>       <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2…     1   2026           2    28          2
#>  2 http://sports.core.api.espn.com/v2…     2   2026           2    28          2
#>  3 http://sports.core.api.espn.com/v2…     3   2026           2    28          2
#>  4 http://sports.core.api.espn.com/v2…     4   2026           2    28          2
#>  5 http://sports.core.api.espn.com/v2…     5   2026           2    28          2
#>  6 http://sports.core.api.espn.com/v2…     6   2026           2    28          2
#>  7 http://sports.core.api.espn.com/v2…     7   2026           2    28          2
#>  8 http://sports.core.api.espn.com/v2…     8   2026           2    28          2
#>  9 http://sports.core.api.espn.com/v2…     9   2026           2    28          2
#> 10 http://sports.core.api.espn.com/v2…    10   2026           2    28          2
#> # ℹ 15 more rows
# }
```
