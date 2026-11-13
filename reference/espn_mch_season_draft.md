# **Get ESPN MCH / WCH Season Draft (core-v2)**

**Get ESPN MCH / WCH Season Draft (core-v2)**

## Usage

``` r
espn_mch_season_draft(season = most_recent_nhl_season(), ...)

espn_wch_season_draft(season = most_recent_nhl_season(), ...)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_draft()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_draft.md)
(columns inherited); a collection may be empty.

`espn_wch_season_draft()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try(espn_mch_season_draft(season = 2025))
#> ! 2026-11-13 20:16:09.239299: ESPN mens-college-hockey season draft for 2025 is not available (HTTP 404 is normal for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try(espn_wch_season_draft(season = 2025))
#> ! 2026-11-13 20:16:09.256226: ESPN womens-college-hockey season draft for 2025 is not available (HTTP 404 is normal for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
