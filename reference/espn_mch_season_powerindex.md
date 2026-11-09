# **Get ESPN MCH / WCH Season Power Index (core-v2)**

**Get ESPN MCH / WCH Season Power Index (core-v2)**

## Usage

``` r
espn_mch_season_powerindex(
  season = most_recent_nhl_season(),
  team_id = NULL,
  ...
)

espn_wch_season_powerindex(
  season = most_recent_nhl_season(),
  team_id = NULL,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- team_id:

  Optional ESPN team identifier (character or numeric). When provided,
  fetches power-index data for a single team. Defaults to `NULL`
  (collection for all teams).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per power-index entry, or an
empty tibble when no data is available. **Note**: ESPN does not publish
power-index data for MCH - this endpoint consistently returns empty
results for hockey:

|               |           |                                            |
|---------------|-----------|--------------------------------------------|
| col_name      | types     | description                                |
| ref           | character | `$ref` URL for the power-index object.     |
| powerindex_id | character | Power-index id parsed from the `$ref` URL. |
| season        | integer   | Season year (echoed from arg).             |
| team_id       | character | Team id (echoed from arg, if supplied).    |
| count         | integer   | Total entries in the collection.           |
| page_count    | integer   | Total pages in the collection.             |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_powerindex.md)
(columns inherited); a collection may be empty.

`espn_wch_season_powerindex()` is the same wrapper for women's college
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
  # NOTE: Returns empty tibble for MCH (ESPN does not publish powerindex for hockey)
  try(espn_mch_season_powerindex(season = 2026))
#> ! 2026-11-09 22:50:44.22065: ESPN mens-college-hockey season powerindex for 2026 not available (not published for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN does not publish powerindex for hockey)
  try(espn_wch_season_powerindex(season = 2026))
#> ! 2026-11-09 22:50:44.285624: ESPN womens-college-hockey season powerindex for 2026 not available (not published for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
