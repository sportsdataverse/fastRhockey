# **Get ESPN NHL League Leaders (core-v2)**

**Get ESPN NHL League Leaders (core-v2)**

## Usage

``` r
espn_nhl_leaders_core(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (category, leader)
combination. For a typical NHL season there are 7 stat categories with
25 leaders each (175 rows). For season-specific / season-type leaders
use
[`espn_nhl_season_type_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type_leaders.md).

|  |  |  |
|----|----|----|
| col_name | types | description |
| category_name | character | Stat category type (e.g. "offensive"). |
| category_display_name | character | Category display name (e.g. "Goals"). |
| category_short_display_name | character | Category short display name. |
| category_abbreviation | character | Category abbreviation. |
| display_value | character | Leader stat display value. |
| value | numeric | Leader stat numeric value. |
| rel | character | Relation type (e.g. "athlete"). |
| athlete_ref | character | `$ref` URL for the athlete on core-v2. |
| statistics_ref | character | `$ref` URL for the athlete statistics. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_leaders_core())
#> ── NHL Leaders (core-v2) data from ESPN.com ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:54 UTC
#> # A tibble: 175 × 9
#>    category_name category_display_name category_short_display_name
#>    <chr>         <chr>                 <chr>                      
#>  1 offensive     Goals                 G                          
#>  2 offensive     Goals                 G                          
#>  3 offensive     Goals                 G                          
#>  4 offensive     Goals                 G                          
#>  5 offensive     Goals                 G                          
#>  6 offensive     Goals                 G                          
#>  7 offensive     Goals                 G                          
#>  8 offensive     Goals                 G                          
#>  9 offensive     Goals                 G                          
#> 10 offensive     Goals                 G                          
#> # ℹ 165 more rows
#> # ℹ 6 more variables: category_abbreviation <chr>, display_value <chr>,
#> #   value <dbl>, rel <chr>, athlete_ref <chr>, statistics_ref <chr>
# }
```
