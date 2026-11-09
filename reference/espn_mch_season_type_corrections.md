# **Get ESPN MCH / WCH Season Type Corrections (core-v2)**

**Get ESPN MCH / WCH Season Type Corrections (core-v2)**

## Usage

``` r
espn_mch_season_type_corrections(
  season = most_recent_nhl_season(),
  season_type = 2,
  ...
)

espn_wch_season_type_corrections(
  season = most_recent_nhl_season(),
  season_type = 2,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season.
  Defaults to `2`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per correction entry, or an
empty tibble when no corrections are recorded. **Note**: ESPN does not
publish score corrections for MCH - this endpoint returns empty for all
tested seasons:

|               |           |                                           |
|---------------|-----------|-------------------------------------------|
| col_name      | types     | description                               |
| ref           | character | `$ref` URL for the correction object.     |
| correction_id | character | Correction id parsed from the `$ref` URL. |
| season        | integer   | Season year (echoed from arg).            |
| season_type   | integer   | Season type code (echoed from arg).       |
| count         | integer   | Total corrections in the collection.      |
| page_count    | integer   | Total pages in the collection.            |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_type_corrections()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type_corrections.md)
(columns inherited); a collection may be empty.

`espn_wch_season_type_corrections()` is the same wrapper for women's
college hockey (league slug `womens-college-hockey`): same arguments,
same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for MCH (ESPN does not publish corrections for hockey)
  try(espn_mch_season_type_corrections(season = 2026, season_type = 2))
#> ! ESPN MENS-COLLEGE-HOCKEY seasons/2026/types/2/corrections returned no items.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN does not publish corrections for hockey)
  try(espn_wch_season_type_corrections(season = 2026, season_type = 2))
#> ! ESPN WOMENS-COLLEGE-HOCKEY seasons/2026/types/2/corrections returned no items.
#> data frame with 0 columns and 0 rows
# }
```
