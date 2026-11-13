# **Get ESPN NHL Teams Core (core-v2 Collection)**

**Get ESPN NHL Teams Core (core-v2 Collection)**

## Usage

``` r
espn_nhl_teams_core(limit = 100, ...)
```

## Arguments

- limit:

  Maximum number of teams to return (default `100`; the NHL has 32
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_teams_core(limit = 100))
#> ── NHL Teams Core data from ESPN core-v2 ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:17:15 UTC
#> # A tibble: 32 × 4
#>    ref                                                  team_id count page_count
#>    <chr>                                                <chr>   <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/le… 1          32          1
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/le… 2          32          1
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/le… 3          32          1
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/le… 4          32          1
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/le… 5          32          1
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/le… 6          32          1
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/le… 7          32          1
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/le… 8          32          1
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/le… 9          32          1
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/le… 10         32          1
#> # ℹ 22 more rows
# }
```
