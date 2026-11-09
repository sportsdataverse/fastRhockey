# **Get ESPN MCH / WCH Talent Picks (core-v2)**

**Get ESPN MCH / WCH Talent Picks (core-v2)**

## Usage

``` r
espn_mch_talentpicks(...)

espn_wch_talentpicks(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per talent pick reference, or
an empty tibble when the endpoint is unavailable. **Note**: The MCH
talent picks endpoint (`core_v2/talentpicks`) currently returns
`count=0`

- an empty tibble is returned gracefully:

|            |           |                                                     |
|------------|-----------|-----------------------------------------------------|
| col_name   | types     | description                                         |
| ref        | character | `$ref` URL for the talent pick object.              |
| pick_id    | character | ESPN pick id parsed from the `$ref` URL.            |
| count      | integer   | Total talent picks in the collection (typically 0). |
| page_count | integer   | Total pages in the collection.                      |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_talentpicks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_talentpicks.md)
(columns inherited); a collection may be empty.

`espn_wch_talentpicks()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for MCH (ESPN does not populate this endpoint)
  try(espn_mch_talentpicks())
#> ! ESPN MENS-COLLEGE-HOCKEY talentpicks endpoint returned no items (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN does not populate this endpoint)
  try(espn_wch_talentpicks())
#> ! ESPN WOMENS-COLLEGE-HOCKEY talentpicks endpoint returned no items (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
```
