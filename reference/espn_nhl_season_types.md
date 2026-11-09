# **Get ESPN NHL Season Types (core-v2)**

**Get ESPN NHL Season Types (core-v2)**

## Usage

``` r
espn_nhl_season_types(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season type (pre-season,
regular, post-season, off-season):

|                |           |                                               |
|----------------|-----------|-----------------------------------------------|
| col_name       | types     | description                                   |
| ref            | character | `$ref` URL for the season-type object.        |
| season_type_id | integer   | Season type id (1=pre, 2=reg, 3=post, 4=off). |
| season         | integer   | Season year (echoed from arg).                |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_types(season = 2026))
#> ── NHL Season Types data from ESPN core-v2 ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:33 UTC
#> # A tibble: 4 × 3
#>   ref                                                      season_type_id season
#>   <chr>                                                             <int>  <int>
#> 1 http://sports.core.api.espn.com/v2/sports/hockey/league…              1   2026
#> 2 http://sports.core.api.espn.com/v2/sports/hockey/league…              2   2026
#> 3 http://sports.core.api.espn.com/v2/sports/hockey/league…              3   2026
#> 4 http://sports.core.api.espn.com/v2/sports/hockey/league…              4   2026
# }
```
