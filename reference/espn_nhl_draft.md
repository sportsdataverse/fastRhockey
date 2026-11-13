# **Get ESPN NHL Draft**

**Get ESPN NHL Draft**

## Usage

``` r
espn_nhl_draft(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per pick on ESPN's draft board
(`site/v2/sports/hockey/nhl/draft`), or an empty tibble when the
endpoint is unavailable:

|                   |           |                                                |
|-------------------|-----------|------------------------------------------------|
| col_name          | types     | description                                    |
| year              | integer   | Draft year.                                    |
| overall           | integer   | Overall pick number.                           |
| round             | integer   | Round number.                                  |
| pick              | integer   | Pick number within the round.                  |
| status            | character | Pick status (e.g. `ON_THE_CLOCK`, `SELECTED`). |
| traded            | logical   | Whether the pick was traded.                   |
| team_id           | character | ESPN id of the team holding the pick.          |
| team_abbreviation | character | Team abbreviation.                             |
| team_display_name | character | Team display name.                             |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_draft())
#> ── NHL Draft data from ESPN.com ─────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:51 UTC
#> # A tibble: 40 × 9
#>     year overall round  pick status       traded team_id team_abbreviation
#>    <int>   <int> <int> <int> <chr>        <lgl>  <chr>   <chr>            
#>  1  2026       1     1     1 ON_THE_CLOCK FALSE  21      TOR              
#>  2  2026       2     1     2 ON_THE_CLOCK FALSE  18      SJ               
#>  3  2026       3     1     3 ON_THE_CLOCK FALSE  22      VAN              
#>  4  2026       4     1     4 ON_THE_CLOCK FALSE  2       BUF              
#>  5  2026       5     1     5 ON_THE_CLOCK FALSE  13      NYR              
#>  6  2026       6     1     6 ON_THE_CLOCK FALSE  3       CGY              
#>  7  2026       7     1     7 ON_THE_CLOCK FALSE  124292  SEA              
#>  8  2026       8     1     8 ON_THE_CLOCK FALSE  28      WPG              
#>  9  2026       9     1     9 ON_THE_CLOCK FALSE  18      SJ               
#> 10  2026      10     1    10 ON_THE_CLOCK FALSE  27      NSH              
#> # ℹ 30 more rows
#> # ℹ 1 more variable: team_display_name <chr>
# }
```
