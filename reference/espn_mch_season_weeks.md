# **Get ESPN MCH / WCH Season Weeks (core-v2)**

**Get ESPN MCH / WCH Season Weeks (core-v2)**

## Usage

``` r
espn_mch_season_weeks(season = most_recent_nhl_season(), season_type = 2, ...)

espn_wch_season_weeks(season = most_recent_nhl_season(), season_type = 2, ...)
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
when the endpoint returns no items. **Note**: The MCH does not use
weekly scheduling in the ESPN core-v2 API - this endpoint consistently
returns `count=0` for MCH. An empty tibble is returned gracefully:

|             |           |                                            |
|-------------|-----------|--------------------------------------------|
| col_name    | types     | description                                |
| ref         | character | `$ref` URL for the week object.            |
| week        | integer   | Week number parsed from the `$ref` URL.    |
| season      | integer   | Season year (echoed from arg).             |
| season_type | integer   | Season type code (echoed from arg).        |
| count       | integer   | Total weeks in the collection (0 for MCH). |
| page_count  | integer   | Total pages in the collection.             |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_weeks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_weeks.md)
(columns inherited); a collection may be empty.

`espn_wch_season_weeks()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for MCH (ESPN does not populate weeks for hockey)
  try(espn_mch_season_weeks(season = 2026, season_type = 2))
#> ── MENS-COLLEGE-HOCKEY Season Weeks data from ESPN core-v2 ─────────────────────
#> ℹ Data updated: 2026-11-13 20:16:15 UTC
#> # A tibble: 25 × 6
#>    ref                                  week season season_type count page_count
#>    <chr>                               <int>  <int>       <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2…     1   2026           2    27          2
#>  2 http://sports.core.api.espn.com/v2…     2   2026           2    27          2
#>  3 http://sports.core.api.espn.com/v2…     3   2026           2    27          2
#>  4 http://sports.core.api.espn.com/v2…     4   2026           2    27          2
#>  5 http://sports.core.api.espn.com/v2…     5   2026           2    27          2
#>  6 http://sports.core.api.espn.com/v2…     6   2026           2    27          2
#>  7 http://sports.core.api.espn.com/v2…     7   2026           2    27          2
#>  8 http://sports.core.api.espn.com/v2…     8   2026           2    27          2
#>  9 http://sports.core.api.espn.com/v2…     9   2026           2    27          2
#> 10 http://sports.core.api.espn.com/v2…    10   2026           2    27          2
#> # ℹ 15 more rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN does not populate weeks for hockey)
  try(espn_wch_season_weeks(season = 2026, season_type = 2))
#> ── WOMENS-COLLEGE-HOCKEY Season Weeks data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-13 20:16:15 UTC
#> # A tibble: 25 × 6
#>    ref                                  week season season_type count page_count
#>    <chr>                               <int>  <int>       <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2…     1   2026           2    25          1
#>  2 http://sports.core.api.espn.com/v2…     2   2026           2    25          1
#>  3 http://sports.core.api.espn.com/v2…     3   2026           2    25          1
#>  4 http://sports.core.api.espn.com/v2…     4   2026           2    25          1
#>  5 http://sports.core.api.espn.com/v2…     5   2026           2    25          1
#>  6 http://sports.core.api.espn.com/v2…     6   2026           2    25          1
#>  7 http://sports.core.api.espn.com/v2…     7   2026           2    25          1
#>  8 http://sports.core.api.espn.com/v2…     8   2026           2    25          1
#>  9 http://sports.core.api.espn.com/v2…     9   2026           2    25          1
#> 10 http://sports.core.api.espn.com/v2…    10   2026           2    25          1
#> # ℹ 15 more rows
# }
```
