# **Get ESPN NHL Transactions**

**Get ESPN NHL Transactions**

## Usage

``` r
espn_nhl_transactions(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per transaction:

|                   |           |                              |
|-------------------|-----------|------------------------------|
| col_name          | types     | description                  |
| date              | character | Transaction date (ISO 8601). |
| description       | character | Transaction description.     |
| team_id           | character | ESPN team identifier.        |
| team_location     | character | Team city/location.          |
| team_name         | character | Team mascot/name.            |
| team_abbreviation | character | Team abbreviation.           |
| team_display_name | character | Team display name.           |
| team_color        | character | Team primary color hex.      |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_transactions())
#> ── NHL Transactions data from ESPN.com ──────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:17:16 UTC
#> # A tibble: 25 × 8
#>    date            description team_id team_location team_name team_abbreviation
#>    <chr>           <chr>       <chr>   <chr>         <chr>     <chr>            
#>  1 2026-11-13T08:… Recalled D… 28      Winnipeg      Jets      WPG              
#>  2 2026-11-13T08:… Recalled F… 17      Colorado      Avalanche COL              
#>  3 2026-11-13T08:… Assigned G… 14      Ottawa        Senators  OTT              
#>  4 2026-11-13T08:… Recalled D… 11      New Jersey    Devils    NJ               
#>  5 2026-11-13T08:… Recalled D… 9       Dallas        Stars     DAL              
#>  6 2026-11-12T08:… Loaned F J… 26      Florida       Panthers  FLA              
#>  7 2026-11-12T08:… Placed F C… 21      Toronto       Maple Le… TOR              
#>  8 2026-11-12T08:… Activated … 13      New York      Rangers   NYR              
#>  9 2026-11-12T08:… Recalled D… 11      New Jersey    Devils    NJ               
#> 10 2026-11-12T08:… Assigned F… 8       Los Angeles   Kings     LA               
#> # ℹ 15 more rows
#> # ℹ 2 more variables: team_display_name <chr>, team_color <chr>
# }
```
