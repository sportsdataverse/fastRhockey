# **Load fastRhockey NHL penalty summary**

Helper that loads multiple seasons of NHL penalty event data from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_penalties(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with one row per penalty and the
following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| timeInPeriod | character | Time within the period the penalty occurred. |
| type | character | Penalty type (e.g. minor, major). |
| duration | integer | Penalty duration in minutes. |
| committedByPlayer | list | Player who committed the penalty (localized list). |
| teamAbbrev | list | Penalized team abbreviation (localized list). |
| drawnBy | list | Player who drew the penalty (localized list). |
| descKey | character | Penalty description key. |
| period_number | integer | Period number the penalty occurred in. |
| period_type | character | Period type (REG/OT/SO). |

## Examples

``` r
# \donttest{
  try(load_nhl_penalties(2026))
#> ── NHL penalty summary ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-22 23:49:22 UTC
#> # A tibble: 10,445 × 45
#>    timeInPeriod type  duration committedByPlayer.firstN…¹ committedByPlayer.fi…²
#>    <chr>        <chr>    <int> <chr>                      <chr>                 
#>  1 06:58        MIN          2 Artyom                     NA                    
#>  2 12:26        MIN          2 Artyom                     NA                    
#>  3 16:50        MAJ          5 Nick                       NA                    
#>  4 16:50        MAJ          5 A.J.                       NA                    
#>  5 01:12        MIN          2 Brad                       NA                    
#>  6 08:17        MIN          2 Seth                       NA                    
#>  7 05:56        MIN          2 Luke                       NA                    
#>  8 00:59        MIN          2 Harrison                   NA                    
#>  9 07:33        MIN          2 Adam                       NA                    
#> 10 18:55        MIN          2 Kris                       NA                    
#> # ℹ 10,435 more rows
#> # ℹ abbreviated names: ¹​committedByPlayer.firstName.default,
#> #   ²​committedByPlayer.firstName.cs
#> # ℹ 40 more variables: committedByPlayer.firstName.de <chr>,
#> #   committedByPlayer.firstName.es <chr>, committedByPlayer.firstName.fi <chr>,
#> #   committedByPlayer.firstName.sk <chr>, committedByPlayer.firstName.sv <chr>,
#> #   committedByPlayer.firstName.fr <chr>, …
# }
```
