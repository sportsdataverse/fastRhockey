# **Get ESPN MCH / WCH Season Awards (core-v2)**

**Get ESPN MCH / WCH Season Awards (core-v2)**

## Usage

``` r
espn_mch_season_awards(season = most_recent_nhl_season(), limit = 200, ...)

espn_wch_season_awards(season = most_recent_nhl_season(), limit = 200, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- limit:

  Maximum number of awards (ESPN `limit`; default `200`, as
  sportsdataverse-py).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per award reference, or an
empty tibble when no awards are available for the season. Awards are
typically populated after the regular season ends:

|            |           |                                           |
|------------|-----------|-------------------------------------------|
| col_name   | types     | description                               |
| ref        | character | `$ref` URL for the award object.          |
| award_id   | character | ESPN award id parsed from the `$ref` URL. |
| season     | integer   | Season year (echoed from arg).            |
| count      | integer   | Total awards in the collection.           |
| page_count | integer   | Total pages in the collection.            |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_awards.md)
(columns inherited); a collection may be empty.

`espn_wch_season_awards()` is the same wrapper for women's college
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
  try(espn_mch_season_awards(season = 2025))
#> ! ESPN MENS-COLLEGE-HOCKEY seasons/2025/awards returned no items.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_season_awards(season = 2025))
#> ! ESPN WOMENS-COLLEGE-HOCKEY seasons/2025/awards returned no items.
#> data frame with 0 columns and 0 rows
# }
```
