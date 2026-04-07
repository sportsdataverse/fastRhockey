# **PWHL Season IDs**

PWHL Season IDs lookup

## Usage

``` r
pwhl_season_id(season, game_type = "regular")
```

## Value

A data frame with season ID data

## Examples

``` r
# \donttest{
  try(pwhl_season_id())
#>   season_yr game_type_label season_id
#> 1      2024       preseason         2
#> 2      2024         regular         1
#> 3      2024        playoffs         3
#> 4      2025       preseason         4
#> 5      2025         regular         5
#> 6      2025        playoffs         6
# }
```
