# **NHL Seasons**

Returns a list of all NHL seasons with their metadata.

## Usage

``` r
nhl_seasons()
```

## Value

A data frame (`fastRhockey_data`) with the following columns:

|           |         |                                     |
|-----------|---------|-------------------------------------|
| col_name  | types   | description                         |
| season_id | integer | Season identifier (e.g., 20232024). |

## Examples

``` r
# \donttest{
  try(nhl_seasons())
#> ── NHL Seasons ──────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:48:28 UTC
#> # A tibble: 109 × 1
#>    season_id
#>        <int>
#>  1  19171918
#>  2  19181919
#>  3  19191920
#>  4  19201921
#>  5  19211922
#>  6  19221923
#>  7  19231924
#>  8  19241925
#>  9  19251926
#> 10  19261927
#> # ℹ 99 more rows
# }
```
