# **Get ESPN MCH / WCH Season Power Index Leaders (core-v2)**

**Get ESPN MCH / WCH Season Power Index Leaders (core-v2)**

## Usage

``` r
espn_mch_season_powerindex_leaders(season = most_recent_nhl_season(), ...)

espn_wch_season_powerindex_leaders(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per power-index leader, or an
empty tibble when no data is available. **Note**: ESPN does not publish
power-index data for MCH:

|            |           |                                               |
|------------|-----------|-----------------------------------------------|
| col_name   | types     | description                                   |
| ref        | character | `$ref` URL for the power-index leader object. |
| leader_id  | character | Leader id parsed from the `$ref` URL.         |
| season     | integer   | Season year (echoed from arg).                |
| count      | integer   | Total entries in the collection.              |
| page_count | integer   | Total pages in the collection.                |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_powerindex_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_powerindex_leaders.md)
(columns inherited); a collection may be empty.

`espn_wch_season_powerindex_leaders()` is the same wrapper for women's
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
  # NOTE: Returns empty tibble for MCH (ESPN does not publish powerindex for hockey)
  try(espn_mch_season_powerindex_leaders(season = 2026))
#> ! ESPN MENS-COLLEGE-HOCKEY seasons/2026/powerindex/leaders returned no items (not published for NHL).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN does not publish powerindex for hockey)
  try(espn_wch_season_powerindex_leaders(season = 2026))
#> ! ESPN WOMENS-COLLEGE-HOCKEY seasons/2026/powerindex/leaders returned no items (not published for NHL).
#> data frame with 0 columns and 0 rows
# }
```
