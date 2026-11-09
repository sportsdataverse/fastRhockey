# **Get ESPN NHL Season Teams (core-v2)**

**Get ESPN NHL Season Teams (core-v2)**

## Usage

``` r
espn_nhl_season_teams(season = most_recent_nhl_season(), limit = 100, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- limit:

  Maximum number of teams to return per page (default `100`). The NHL
  has 32 active teams; `100` returns all in one request.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team in the season:

|            |           |                                           |
|------------|-----------|-------------------------------------------|
| col_name   | types     | description                               |
| ref        | character | `$ref` URL for the team-in-season object. |
| team_id    | character | ESPN team id parsed from the `$ref` URL.  |
| season     | integer   | Season year (echoed from arg).            |
| count      | integer   | Total teams in the collection.            |
| page_count | integer   | Total pages in the collection.            |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_teams(season = 2026))
#> ── NHL Season Teams data from ESPN core-v2 ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:06 UTC
#> # A tibble: 32 × 5
#>    ref                                           team_id season count page_count
#>    <chr>                                         <chr>    <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/ho… 1         2026    32          1
#>  2 http://sports.core.api.espn.com/v2/sports/ho… 2         2026    32          1
#>  3 http://sports.core.api.espn.com/v2/sports/ho… 3         2026    32          1
#>  4 http://sports.core.api.espn.com/v2/sports/ho… 4         2026    32          1
#>  5 http://sports.core.api.espn.com/v2/sports/ho… 5         2026    32          1
#>  6 http://sports.core.api.espn.com/v2/sports/ho… 6         2026    32          1
#>  7 http://sports.core.api.espn.com/v2/sports/ho… 7         2026    32          1
#>  8 http://sports.core.api.espn.com/v2/sports/ho… 8         2026    32          1
#>  9 http://sports.core.api.espn.com/v2/sports/ho… 9         2026    32          1
#> 10 http://sports.core.api.espn.com/v2/sports/ho… 10        2026    32          1
#> # ℹ 22 more rows
# }
```
