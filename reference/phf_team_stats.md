# **PHF Team Stats**

PHF Team Stats lookup

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_team_stats(team, season = most_recent_phf_season())
```

## Arguments

- team:

  Team name with nickname (e.g. Boston Pride, Buffalo Beauts)

- season:

  Season (YYYY) to pull the team stats from, the concluding year in
  XXXX-YY format

## Value

A named list of data frames: skaters, goalies

## Examples

``` r
# \donttest{
  try(phf_team_stats(team = "Boston Pride", season = 2022))
#> Error : `phf_team_stats()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
