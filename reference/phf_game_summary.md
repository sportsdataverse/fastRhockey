# **PHF Game Summary**

phf_game_summary: pull in the raw data for a game_id from the PHF/NWHL
API

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_game_summary(game_id)
```

## Arguments

- game_id:

  The unique ID code for the game that you are interested in viewing the
  data for

## Value

A named list of data frames: scoring_summary,shootout_summary,
penalty_summary, officials, team_staff, timeouts

## Examples

``` r
# \donttest{
  try(phf_game_summary(game_id = 612254))
#> Error : `phf_game_summary()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
