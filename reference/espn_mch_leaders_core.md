# **Get ESPN MCH / WCH League Leaders (core-v2)**

**Get ESPN MCH / WCH League Leaders (core-v2)**

## Usage

``` r
espn_mch_leaders_core(...)

espn_wch_leaders_core(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (category, leader)
combination. For a typical MCH season there are 7 stat categories with
25 leaders each (175 rows). For season-specific / season-type leaders
use
[`espn_mch_season_type_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_type_leaders.md).

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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_leaders_core()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_leaders_core.md)
(columns inherited); a collection may be empty.

`espn_wch_leaders_core()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_leaders_core())
#> ── MENS-COLLEGE-HOCKEY Leaders (core-v2) data from ESPN.com ────────────────────
#> ℹ Data updated: 2026-11-09 19:33:51 UTC
#> # A tibble: 7 × 9
#>   category_name category_display_name category_short_display_name
#>   <chr>         <chr>                 <chr>                      
#> 1 offensive     Goals                 G                          
#> 2 offensive     Assists               A                          
#> 3 offensive     Points                PTS                        
#> 4 penalties     Penalty Minutes       PIM                        
#> 5 general       Wins                  W                          
#> 6 defensive     Defensive             SO                         
#> 7 general       Games Played          GP                         
#> # ℹ 6 more variables: category_abbreviation <chr>, display_value <chr>,
#> #   value <dbl>, rel <chr>, athlete_ref <chr>, statistics_ref <chr>
# }
# \donttest{
  try(espn_wch_leaders_core())
#> ── WOMENS-COLLEGE-HOCKEY Leaders (core-v2) data from ESPN.com ──────────────────
#> ℹ Data updated: 2026-11-09 19:33:51 UTC
#> # A tibble: 7 × 9
#>   category_name category_display_name category_short_display_name
#>   <chr>         <chr>                 <chr>                      
#> 1 offensive     Goals                 G                          
#> 2 offensive     Assists               A                          
#> 3 offensive     Points                PTS                        
#> 4 penalties     Penalty Minutes       PIM                        
#> 5 general       Wins                  W                          
#> 6 defensive     Defensive             SO                         
#> 7 general       Games Played          GP                         
#> # ℹ 6 more variables: category_abbreviation <chr>, display_value <chr>,
#> #   value <dbl>, rel <chr>, athlete_ref <chr>, statistics_ref <chr>
# }
```
