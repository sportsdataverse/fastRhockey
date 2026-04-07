# **PHF League Information**

PHF League Information per year

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_league_info(season = most_recent_phf_season())
```

## Arguments

- season:

  Season (YYYY) to pull the league info and IDs for. Season is the
  concluding year in XXXX-YY format

## Value

A named list of data frames: seasons, divisions, teams, league,
officials, brackets

## Examples

``` r
# \donttest{
  try(phf_league_info(season = 2023))
#> Error : `phf_league_info()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
  try(phf_league_info(season = 2016))
#> Error : `phf_league_info()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
