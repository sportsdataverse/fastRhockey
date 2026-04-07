# phf_pbp

phf_pbp: pull in the raw data for a game_id from the PHF/NWHL API

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_pbp(game_id)
```

## Arguments

- game_id:

  The unique ID code for the game that you are interested in viewing the
  data for

## Value

A data frame of play by play information

## Examples

``` r
# \donttest{
  try(phf_pbp(game_id = 268127))
#> Error : `phf_pbp()` was deprecated in fastRhockey 1.0.0 and is now defunct.
#> ℹ The PHF has ceased operations.
# }
```
