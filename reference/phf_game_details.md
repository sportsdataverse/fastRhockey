# **PHF Game Details**

phf_game_details: pull in the raw data for a game_id from the PHF/NWHL
API

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_game_details(game_id)
```

## Arguments

- game_id:

  The unique ID code for the game that you are interested in viewing the
  data for

## Value

A data frame with game team details

## Examples

``` r
# \donttest{
  try(phf_game_details(game_id = 612254))
#> Error : `phf_game_details()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
