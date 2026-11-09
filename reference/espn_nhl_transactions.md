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
#> ℹ Data updated: 2026-11-09 22:52:05 UTC
#> # A tibble: 25 × 8
#>    date            description team_id team_location team_name team_abbreviation
#>    <chr>           <chr>       <chr>   <chr>         <chr>     <chr>            
#>  1 2026-11-09T08:… Recalled D… 129764  Utah          Mammoth   UTA              
#>  2 2026-11-09T08:… Placed F T… 30      Minnesota     Wild      MIN              
#>  3 2026-11-09T08:… Recalled D… 23      Washington    Capitals  WSH              
#>  4 2026-11-09T08:… Reinstated… 19      St. Louis     Blues     STL              
#>  5 2026-11-09T08:… Assigned F… 18      San Jose      Sharks    SJ               
#>  6 2026-11-09T08:… Recalled D… 17      Colorado      Avalanche COL              
#>  7 2026-11-09T08:… Recalled F… 11      New Jersey    Devils    NJ               
#>  8 2026-11-09T08:… Claimed G … 9       Dallas        Stars     DAL              
#>  9 2026-11-09T08:… Recalled G… 4       Chicago       Blackhaw… CHI              
#> 10 2026-11-08T08:… Assigned D… 129764  Utah          Mammoth   UTA              
#> # ℹ 15 more rows
#> # ℹ 2 more variables: team_display_name <chr>, team_color <chr>
# }
```
