# **Get ESPN MCH / WCH Recruiting Rankings (core-v2)**

**Get ESPN MCH / WCH Recruiting Rankings (core-v2)**

## Usage

``` r
espn_mch_recruiting_rankings(year, ...)

espn_wch_recruiting_rankings(year, ...)
```

## Arguments

- year:

  Recruiting class year.

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per ranking set:

|            |           |                                          |
|------------|-----------|------------------------------------------|
| col_name   | types     | description                              |
| ref        | character | `$ref` URL of the ranking set.           |
| ranking_id | character | Trailing id of the `$ref`.               |
| year       | integer   | Recruiting class year (echoed from arg). |

## Details

League slug `mens-college-hockey`. Core-v2 `recruiting/{year}/rankings`;
`$ref`-only items become `ref` + `ranking_id`. Mirrors
sportsdataverse-py `espn_mch_recruiting_rankings()`.

`espn_wch_recruiting_rankings()` is the same wrapper for women's college
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
  try(espn_mch_recruiting_rankings(year = 2026))
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_recruiting_rankings(year = 2026))
#> data frame with 0 columns and 0 rows
# }
```
