# **Get ESPN MCH / WCH Season Groups (core-v2)**

**Get ESPN MCH / WCH Season Groups (core-v2)**

## Usage

``` r
espn_mch_season_groups(season = most_recent_nhl_season(), season_type = 2, ...)

espn_wch_season_groups(season = most_recent_nhl_season(), season_type = 2, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season,
  `4`=off-season. Defaults to `2`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per group (conference or
division):

|             |           |                                      |
|-------------|-----------|--------------------------------------|
| col_name    | types     | description                          |
| ref         | character | `$ref` URL for the group object.     |
| group_id    | integer   | Group id parsed from the `$ref` URL. |
| season      | integer   | Season year (echoed from arg).       |
| season_type | integer   | Season type code (echoed from arg).  |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_groups.md)
(columns inherited); a collection may be empty.

`espn_wch_season_groups()` is the same wrapper for women's college
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
  try(espn_mch_season_groups(season = 2026, season_type = 2))
#> ── MENS-COLLEGE-HOCKEY Season Groups data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-11-13 20:16:10 UTC
#> # A tibble: 10 × 4
#>    ref                                               group_id season season_type
#>    <chr>                                                <int>  <int>       <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey…       61   2026           2
#>  2 http://sports.core.api.espn.com/v2/sports/hockey…       62   2026           2
#>  3 http://sports.core.api.espn.com/v2/sports/hockey…       54   2026           2
#>  4 http://sports.core.api.espn.com/v2/sports/hockey…       57   2026           2
#>  5 http://sports.core.api.espn.com/v2/sports/hockey…       53   2026           2
#>  6 http://sports.core.api.espn.com/v2/sports/hockey…       52   2026           2
#>  7 http://sports.core.api.espn.com/v2/sports/hockey…       59   2026           2
#>  8 http://sports.core.api.espn.com/v2/sports/hockey…       56   2026           2
#>  9 http://sports.core.api.espn.com/v2/sports/hockey…       63   2026           2
#> 10 http://sports.core.api.espn.com/v2/sports/hockey…       60   2026           2
# }
# \donttest{
  try(espn_wch_season_groups(season = 2026, season_type = 2))
#> ✖ 2026-11-13 20:16:10.839616: Invalid arguments or no ESPN womens-college-hockey season groups for 2026 type 2 available!
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2
#> ✖ Error: replacement has 1 row, data has 0
#> data frame with 0 columns and 0 rows
# }
```
