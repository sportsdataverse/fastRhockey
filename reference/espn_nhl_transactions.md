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
#> ℹ Data updated: 2026-11-25 00:37:48 UTC
#> # A tibble: 25 × 8
#>    date            description team_id team_location team_name team_abbreviation
#>    <chr>           <chr>       <chr>   <chr>         <chr>     <chr>            
#>  1 2026-11-24T08:… Placed G C… 30      Minnesota     Wild      MIN              
#>  2 2026-11-24T08:… Placed F M… 29      Columbus      Blue Jac… CBJ              
#>  3 2026-11-24T08:… Placed F C… 26      Florida       Panthers  FLA              
#>  4 2026-11-24T08:… Assigned D… 25      Anaheim       Ducks     ANA              
#>  5 2026-11-24T08:… Claimed G … 23      Washington    Capitals  WSH              
#>  6 2026-11-24T08:… Assigned F… 21      Toronto       Maple Le… TOR              
#>  7 2026-11-24T08:… Placed F H… 14      Ottawa        Senators  OTT              
#>  8 2026-11-24T08:… Placed F K… 12      New York      Islanders NYI              
#>  9 2026-11-24T08:… Recalled F… 10      Montreal      Canadiens MTL              
#> 10 2026-11-23T08:… Activated … 30      Minnesota     Wild      MIN              
#> # ℹ 15 more rows
#> # ℹ 2 more variables: team_display_name <chr>, team_color <chr>
# }
```
