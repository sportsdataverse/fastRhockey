# **Get ESPN NHL Season Type Corrections (core-v2)**

**Get ESPN NHL Season Type Corrections (core-v2)**

## Usage

``` r
espn_nhl_season_type_corrections(
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
publish score corrections for NHL - this endpoint returns empty for all
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not publish corrections for hockey)
  try(espn_nhl_season_type_corrections(season = 2026, season_type = 2))
#> ! ESPN NHL seasons/2026/types/2/corrections returned no items.
#> data frame with 0 columns and 0 rows
# }
```
