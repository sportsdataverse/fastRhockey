# **Get ESPN MCH / WCH Season Coaches (core-v2)**

**Get ESPN MCH / WCH Season Coaches (core-v2)**

## Usage

``` r
espn_mch_season_coaches(season = most_recent_nhl_season(), limit = 100, ...)

espn_wch_season_coaches(season = most_recent_nhl_season(), limit = 100, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- limit:

  Maximum number of coaches to return (default `100`). The MCH typically
  lists ~30 head coaches per season.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per coach:

|            |           |                                            |
|------------|-----------|--------------------------------------------|
| col_name   | types     | description                                |
| ref        | character | `$ref` URL for the coach-in-season object. |
| coach_id   | character | ESPN coach id parsed from the `$ref` URL.  |
| season     | integer   | Season year (echoed from arg).             |
| count      | integer   | Total coaches in the collection.           |
| page_count | integer   | Total pages in the collection.             |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_coaches.md)
(columns inherited); a collection may be empty.

`espn_wch_season_coaches()` is the same wrapper for women's college
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
  try(espn_mch_season_coaches(season = 2026))
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_season_coaches(season = 2026))
#> data frame with 0 columns and 0 rows
# }
```
