# **PHF Standings**

phf_standings: pull in the standings data for a game_id from the
PHF/NWHL API

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_standings(season = most_recent_phf_season())
```

## Arguments

- season:

  Season (YYYY) to pull the standings from, the concluding year in
  XXXX-YY format

## Value

A data frame of standings data

## Examples

``` r
# \donttest{
  try(phf_standings(season = most_recent_phf_season()))
#> Error : `phf_standings()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
