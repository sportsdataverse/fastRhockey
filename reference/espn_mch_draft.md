# **Get ESPN MCH / WCH Draft**

**Get ESPN MCH / WCH Draft**

## Usage

``` r
espn_mch_draft(...)

espn_wch_draft(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with draft data, or an empty tibble when the
endpoint is unavailable. **Note**: The MCH draft endpoint
(`site/v2/sports/hockey/nhl/draft`) currently returns HTTP 500 for MCH -
an empty tibble is returned gracefully:

|          |           |                                       |
|----------|-----------|---------------------------------------|
| col_name | types     | description                           |
| id       | character | Draft entry identifier (if returned). |
| name     | character | Draft entry name (if returned).       |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_draft.md)
(columns inherited); a collection may be empty.

`espn_wch_draft()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for MCH (ESPN returns HTTP 500 for this endpoint)
  try(espn_mch_draft())
#> ! ESPN MENS-COLLEGE-HOCKEY draft endpoint is not available (HTTP 500 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN returns HTTP 500 for this endpoint)
  try(espn_wch_draft())
#> ! ESPN WOMENS-COLLEGE-HOCKEY draft endpoint is not available (HTTP 500 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
```
