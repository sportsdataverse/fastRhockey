# **PHF Schedule**

PHF Schedule lookup

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_schedule(season = most_recent_phf_season())
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
  try(phf_schedule(season = 2023))
#> Error : `phf_schedule()` was deprecated in fastRhockey 1.0.0 and is now defunct.
#> ℹ The PHF has ceased operations.
# }
```
