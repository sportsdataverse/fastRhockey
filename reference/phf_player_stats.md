# **PHF Player Stats**

phf_player_stats: loads the player stats

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_player_stats(player_id)
```

## Arguments

- player_id:

  The unique ID code for the player that you are interested in viewing
  the data for

## Value

A named list of data frames: career, game_log

## Examples

``` r
# \donttest{
  try(phf_player_stats(player_id = 431611))
#> Error : `phf_player_stats()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
  try(phf_player_stats(player_id = 532475))
#> Error : `phf_player_stats()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
