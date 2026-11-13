# **Get ESPN NHL Conferences**

**Get ESPN NHL Conferences**

## Usage

``` r
espn_nhl_conferences(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per conference/division
combination (4 rows for NHL: 2 conferences x 2 divisions each):

|  |  |  |
|----|----|----|
| col_name | types | description |
| conference_name | character | Conference name (e.g. "Eastern Conference"). |
| conference_abbreviation | character | Conference abbreviation. |
| division_name | character | Division name (e.g. "Atlantic Division"). |
| division_abbreviation | character | Division abbreviation. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_conferences())
#> ── NHL Conferences data from ESPN.com ───────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:26 UTC
#> # A tibble: 4 × 4
#>   conference_name    conference_abbreviation division_name division_abbreviation
#>   <chr>              <chr>                   <chr>         <chr>                
#> 1 Eastern Conference East                    Atlantic Div… ATL                  
#> 2 Eastern Conference East                    Metropolitan… MET                  
#> 3 Western Conference West                    Central Divi… CEN                  
#> 4 Western Conference West                    Pacific Divi… PAC                  
# }
```
