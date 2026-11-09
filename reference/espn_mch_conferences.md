# **Get ESPN MCH / WCH Conferences**

**Get ESPN MCH / WCH Conferences**

## Usage

``` r
espn_mch_conferences(...)

espn_wch_conferences(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per conference/division
combination (4 rows for MCH: 2 conferences x 2 divisions each):

|  |  |  |
|----|----|----|
| col_name | type | description |
| conference_name | character | Conference name (e.g. "Eastern Conference"). |
| conference_abbreviation | character | Conference abbreviation. |
| division_name | character | Division name (e.g. "Atlantic Division"). |
| division_abbreviation | character | Division abbreviation. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_conferences()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_conferences.md);
columns verified against a captured MCH body.

`espn_wch_conferences()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_conferences())
#> ── MENS-COLLEGE-HOCKEY Conferences data from ESPN.com ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:05 UTC
#> # A tibble: 10 × 4
#>    conference_name    conference_abbreviat…¹ division_name division_abbreviation
#>    <chr>              <chr>                  <chr>         <chr>                
#>  1 Atlantic Hockey A… AHA                    NA            NA                   
#>  2 Big Ten Conference Big Ten                NA            NA                   
#>  3 Central Collegiat… CCHA                   NA            NA                   
#>  4 College Hockey Am… CHA                    NA            NA                   
#>  5 East Coast Athlet… ECAC                   NA            NA                   
#>  6 Hockey East        HE                     NA            NA                   
#>  7 Independent        IND                    NA            NA                   
#>  8 Metro Atlantic At… MAAC                   NA            NA                   
#>  9 National Collegia… NCHC                   NA            NA                   
#> 10 Northeast 10 Conf… NE10                   NA            NA                   
#> # ℹ abbreviated name: ¹​conference_abbreviation
# }
# \donttest{
  try(espn_wch_conferences())
#> data frame with 0 columns and 0 rows
# }
```
