# **Get ESPN MCH / WCH Recruiting Years (core-v2)**

**Get ESPN MCH / WCH Recruiting Years (core-v2)**

## Usage

``` r
espn_mch_recruiting_years(...)

espn_wch_recruiting_years(...)
```

## Arguments

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per recruiting year:

|          |           |                                             |
|----------|-----------|---------------------------------------------|
| col_name | types     | description                                 |
| ref      | character | `$ref` URL of the recruiting-year object.   |
| year     | character | Trailing id of the `$ref` (the class year). |

## Details

League slug `mens-college-hockey`. Core-v2 `recruiting` collection (one
item per recruiting class year). Mirrors sportsdataverse-py
`espn_mch_recruiting_years()`.

`espn_wch_recruiting_years()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_recruiting_years())
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_recruiting_years())
#> data frame with 0 columns and 0 rows
# }
```
