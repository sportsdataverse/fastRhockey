# **PWHL Schedule**

PWHL Schedule lookup

## Usage

``` r
pwhl_schedule(season, game_type = "regular")
```

## Arguments

- season:

  Season (YYYY) to pull the schedule from, the concluding year in
  XXXX-YY format

## Value

A data frame with schedule data

## Examples

``` r
# \donttest{
  try(pwhl_schedule(season = 2023))
#> Error in parse_url(url) : length(url) == 1 is not TRUE
# }
```
