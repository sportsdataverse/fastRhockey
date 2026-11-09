# **Get ESPN NHL Season Groups (core-v2)**

**Get ESPN NHL Season Groups (core-v2)**

## Usage

``` r
espn_nhl_season_groups(season = most_recent_nhl_season(), season_type = 2, ...)
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_groups(season = 2026, season_type = 2))
#> ── NHL Season Groups data from ESPN core-v2 ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:31 UTC
#> # A tibble: 2 × 4
#>   ref                                                group_id season season_type
#>   <chr>                                                 <int>  <int>       <int>
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/…        7   2026           2
#> 2 http://sports.core.api.espn.com/v2/sports/hockey/…        8   2026           2
# }
```
