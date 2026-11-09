# **Get ESPN MCH / WCH Season Teams (core-v2)**

**Get ESPN MCH / WCH Season Teams (core-v2)**

## Usage

``` r
espn_mch_season_teams(season = most_recent_nhl_season(), limit = 100, ...)

espn_wch_season_teams(season = most_recent_nhl_season(), limit = 100, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- limit:

  Maximum number of teams to return per page (default `100`). The MCH
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_teams.md)
(columns inherited); a collection may be empty.

`espn_wch_season_teams()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_teams(season = 2026))
#> ── MENS-COLLEGE-HOCKEY Season Teams data from ESPN core-v2 ─────────────────────
#> ℹ Data updated: 2026-11-09 19:34:07 UTC
#> # A tibble: 100 × 5
#>    ref                                           team_id season count page_count
#>    <chr>                                         <chr>    <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/ho… 1         2026   116          2
#>  2 http://sports.core.api.espn.com/v2/sports/ho… 9         2026   116          2
#>  3 http://sports.core.api.espn.com/v2/sports/ho… 12        2026   116          2
#>  4 http://sports.core.api.espn.com/v2/sports/ho… 41        2026   116          2
#>  5 http://sports.core.api.espn.com/v2/sports/ho… 43        2026   116          2
#>  6 http://sports.core.api.espn.com/v2/sports/ho… 87        2026   116          2
#>  7 http://sports.core.api.espn.com/v2/sports/ho… 103       2026   116          2
#>  8 http://sports.core.api.espn.com/v2/sports/ho… 104       2026   116          2
#>  9 http://sports.core.api.espn.com/v2/sports/ho… 107       2026   116          2
#> 10 http://sports.core.api.espn.com/v2/sports/ho… 108       2026   116          2
#> # ℹ 90 more rows
# }
# \donttest{
  try(espn_wch_season_teams(season = 2026))
#> ── WOMENS-COLLEGE-HOCKEY Season Teams data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-09 19:34:07 UTC
#> # A tibble: 47 × 5
#>    ref                                           team_id season count page_count
#>    <chr>                                         <chr>    <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/ho… 41        2026    47          1
#>  2 http://sports.core.api.espn.com/v2/sports/ho… 43        2026    47          1
#>  3 http://sports.core.api.espn.com/v2/sports/ho… 103       2026    47          1
#>  4 http://sports.core.api.espn.com/v2/sports/ho… 104       2026    47          1
#>  5 http://sports.core.api.espn.com/v2/sports/ho… 107       2026    47          1
#>  6 http://sports.core.api.espn.com/v2/sports/ho… 108       2026    47          1
#>  7 http://sports.core.api.espn.com/v2/sports/ho… 111       2026    47          1
#>  8 http://sports.core.api.espn.com/v2/sports/ho… 132       2026    47          1
#>  9 http://sports.core.api.espn.com/v2/sports/ho… 134       2026    47          1
#> 10 http://sports.core.api.espn.com/v2/sports/ho… 135       2026    47          1
#> # ℹ 37 more rows
# }
```
