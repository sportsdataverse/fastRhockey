# **Get ESPN NHL Season Power Index Leaders (core-v2)**

**Get ESPN NHL Season Power Index Leaders (core-v2)**

## Usage

``` r
espn_nhl_season_powerindex_leaders(season = most_recent_nhl_season(), ...)
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
power-index data for NHL:

|            |           |                                               |
|------------|-----------|-----------------------------------------------|
| col_name   | types     | description                                   |
| ref        | character | `$ref` URL for the power-index leader object. |
| leader_id  | character | Leader id parsed from the `$ref` URL.         |
| season     | integer   | Season year (echoed from arg).                |
| count      | integer   | Total entries in the collection.              |
| page_count | integer   | Total pages in the collection.                |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not publish powerindex for hockey)
  try(espn_nhl_season_powerindex_leaders(season = 2026))
#> ! ESPN NHL seasons/2026/powerindex/leaders returned no items (not published for NHL).
#> data frame with 0 columns and 0 rows
# }
```
