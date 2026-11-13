# **Get ESPN NHL Standings**

**Get ESPN NHL Standings**

## Usage

``` r
espn_nhl_standings(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|                    |           |                                     |
|--------------------|-----------|-------------------------------------|
| col_name           | types     | description                         |
| group_name         | character | Group name (conference / division). |
| group_abbreviation | character | Group abbreviation.                 |
| team_id            | character | ESPN team identifier.               |
| team_name          | character | Team name.                          |
| team_abbreviation  | character | Team abbreviation.                  |
| team_display_name  | character | Team display name.                  |
| team_location      | character | Team location (city).               |
| team_logo          | character | Team logo URL.                      |
| season             | integer   | Season end-year (echoed from arg).  |
| ot_losses          | numeric   | Overtime losses.                    |
| games_played       | numeric   | Games played.                       |
| losses             | numeric   | Regulation losses.                  |
| points             | numeric   | Points.                             |
| wins               | numeric   | Wins.                               |
| points_for         | numeric   | Goals/points scored.                |
| points_against     | numeric   | Goals/points allowed.               |
| playoff_seed       | numeric   | Current playoff seed.               |
| streak             | numeric   | Current streak value.               |
| overall            | numeric   | Overall record stat.                |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_standings(season = 2025))
#> ── NHL Standings data from ESPN.com ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:29 UTC
#> # A tibble: 32 × 36
#>    group_name         group_abbreviation team_id team_name    team_abbreviation
#>    <chr>              <chr>              <chr>   <chr>        <chr>            
#>  1 Eastern Conference East               23      Capitals     WSH              
#>  2 Eastern Conference East               21      Maple Leafs  TOR              
#>  3 Eastern Conference East               20      Lightning    TB               
#>  4 Eastern Conference East               7       Hurricanes   CAR              
#>  5 Eastern Conference East               26      Panthers     FLA              
#>  6 Eastern Conference East               14      Senators     OTT              
#>  7 Eastern Conference East               11      Devils       NJ               
#>  8 Eastern Conference East               10      Canadiens    MTL              
#>  9 Eastern Conference East               29      Blue Jackets CBJ              
#> 10 Eastern Conference East               5       Red Wings    DET              
#> # ℹ 22 more rows
#> # ℹ 31 more variables: team_display_name <chr>, team_location <chr>,
#> #   team_logo <chr>, ot_losses <dbl>, clincher <dbl>, differential <dbl>,
#> #   games_behind <dbl>, games_played <dbl>, losses <dbl>, playoff_seed <dbl>,
#> #   point_differential <dbl>, points <dbl>, points_against <dbl>,
#> #   points_for <dbl>, streak <dbl>, wins <dbl>, overtime_losses <dbl>,
#> #   overtime_wins <dbl>, points_diff <dbl>, reg_losses <dbl>, reg_wins <dbl>, …
# }
```
