# **PHF Player Boxscore**

phf_player_box: loads the player boxscore

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_player_box(game_id)
```

## Arguments

- game_id:

  The unique ID code for the game that you are interested in viewing the
  data for

## Value

A named list of data frames: skaters, goalies

## Examples

``` r
# \donttest{
  try(phf_player_box(game_id = 420339))
#> Error : `phf_player_box()` was deprecated in fastRhockey 1.0.0 and is now
#> defunct.
#> ℹ The PHF has ceased operations.
# }
```
