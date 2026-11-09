# **Get ESPN NHL Season Type Leaders (core-v2)**

**Get ESPN NHL Season Type Leaders (core-v2)**

## Usage

``` r
espn_nhl_season_type_leaders(
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
combination. For a typical NHL regular season there are 9 categories
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_type_leaders(season = 2026, season_type = 2))
#> ── NHL Season Type Leaders data from ESPN core-v2 ───────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:07 UTC
#> # A tibble: 225 × 12
#>    category_name category_display_name category_short_display_name
#>    <chr>         <chr>                 <chr>                      
#>  1 goals         Goals                 G                          
#>  2 goals         Goals                 G                          
#>  3 goals         Goals                 G                          
#>  4 goals         Goals                 G                          
#>  5 goals         Goals                 G                          
#>  6 goals         Goals                 G                          
#>  7 goals         Goals                 G                          
#>  8 goals         Goals                 G                          
#>  9 goals         Goals                 G                          
#> 10 goals         Goals                 G                          
#> # ℹ 215 more rows
#> # ℹ 9 more variables: category_abbreviation <chr>, display_value <chr>,
#> #   value <dbl>, rel <chr>, athlete_ref <chr>, team_ref <chr>,
#> #   statistics_ref <chr>, season <int>, season_type <int>
# }
```
