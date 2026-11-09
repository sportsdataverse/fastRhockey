# **Get ESPN MCH / WCH Season Type Leaders (core-v2)**

**Get ESPN MCH / WCH Season Type Leaders (core-v2)**

## Usage

``` r
espn_mch_season_type_leaders(
  season = most_recent_nhl_season(),
  season_type = 2,
  ...
)

espn_wch_season_type_leaders(
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

A `fastRhockey_data` tibble with one row per (category, leader)
combination. For a typical MCH regular season there are 9 categories
with 25 leaders each (225 rows):

|  |  |  |
|----|----|----|
| col_name | types | description |
| category_name | character | Stat category name (e.g. "goals", "assists"). |
| category_display_name | character | Category display name. |
| category_short_display_name | character | Category short display name. |
| category_abbreviation | character | Category abbreviation. |
| display_value | character | Leader stat display value. |
| value | numeric | Leader stat numeric value. |
| rel | character | Relation type (e.g. "athlete"). |
| athlete_ref | character | `$ref` URL for the athlete. |
| team_ref | character | `$ref` URL for the athlete's team. |
| statistics_ref | character | `$ref` URL for the athlete's season statistics. |
| season | integer | Season year (echoed from arg). |
| season_type | integer | Season type code (echoed from arg). |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_type_leaders()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type_leaders.md)
(columns inherited); a collection may be empty.

`espn_wch_season_type_leaders()` is the same wrapper for women's college
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
  try(espn_mch_season_type_leaders(season = 2026, season_type = 2))
#> ✖ 2026-11-09 19:34:08.738457: Invalid arguments or no ESPN mens-college-hockey season type leaders for 2026 type 2 available!
#> ✖ Args: league = "mens-college-hockey", season = 2026, season_type = 2
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_season_type_leaders(season = 2026, season_type = 2))
#> ✖ 2026-11-09 19:34:08.762222: Invalid arguments or no ESPN womens-college-hockey season type leaders for 2026 type 2 available!
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
