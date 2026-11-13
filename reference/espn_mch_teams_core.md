# **Get ESPN MCH / WCH Teams Core (core-v2 Collection)**

**Get ESPN MCH / WCH Teams Core (core-v2 Collection)**

## Usage

``` r
espn_mch_teams_core(limit = 1000, ...)

espn_wch_teams_core(limit = 1000, ...)
```

## Arguments

- limit:

  Maximum number of teams to return (default `1000`; the MCH has 32
  active teams so the default returns all).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|            |           |                                                     |
|------------|-----------|-----------------------------------------------------|
| col_name   | types     | description                                         |
| ref        | character | Core-v2 `$ref` URL for the team resource.           |
| team_id    | character | Team identifier (trailing path segment of ref URL). |
| count      | integer   | Total number of teams in the collection.            |
| page_count | integer   | Number of pages in the paginated response.          |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_teams_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams_core.md)
(columns inherited); a collection may be empty.

`espn_wch_teams_core()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_teams_core(limit = 100))
#> ── MENS-COLLEGE-HOCKEY Teams Core data from ESPN core-v2 ── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:45 UTC
#> # A tibble: 100 × 4
#>    ref                                                  team_id count page_count
#>    <chr>                                                <chr>   <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/le… 1         116          2
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/le… 9         116          2
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/le… 12        116          2
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/le… 41        116          2
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/le… 43        116          2
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/le… 87        116          2
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/le… 103       116          2
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/le… 104       116          2
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/le… 107       116          2
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/le… 108       116          2
#> # ℹ 90 more rows
# }
# \donttest{
  try(espn_wch_teams_core(limit = 100))
#> ── WOMENS-COLLEGE-HOCKEY Teams Core data from ESPN core-v2 ─────────────────────
#> ℹ Data updated: 2026-11-13 21:26:45 UTC
#> # A tibble: 47 × 4
#>    ref                                                  team_id count page_count
#>    <chr>                                                <chr>   <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/le… 41         47          1
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/le… 43         47          1
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/le… 103        47          1
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/le… 104        47          1
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/le… 107        47          1
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/le… 108        47          1
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/le… 111        47          1
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/le… 132        47          1
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/le… 134        47          1
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/le… 135        47          1
#> # ℹ 37 more rows
# }
```
