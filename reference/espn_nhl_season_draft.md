# **Get ESPN NHL Season Draft (core-v2)**

**Get ESPN NHL Season Draft (core-v2)**

## Usage

``` r
espn_nhl_season_draft(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with draft round references, or an empty
tibble when the endpoint is unavailable (HTTP 404 is normal for NHL -
the draft sub-tree is not served by this core-v2 path):

|          |           |                                                      |
|----------|-----------|------------------------------------------------------|
| col_name | types     | description                                          |
| ref      | character | `$ref` URL for the draft round object (if returned). |
| draft_id | character | Draft/round id parsed from the `$ref` URL.           |
| season   | integer   | Season year (echoed from arg).                       |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try(espn_nhl_season_draft(season = 2025))
#> ! 2026-11-09 17:19:29.457627: ESPN nhl season draft for 2025 is not available (HTTP 404 is normal for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
