# **PHF Team Boxscore**

phf_team_box: loads the team boxscore and shot/score data for a game
into one data frame through just one function

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_team_box(game_id)
```

## Arguments

- game_id:

  The unique ID code for the game that you are interested in viewing the
  data for

## Value

A dataframe of team-level box score information

## Examples

``` r
# \donttest{
  try(phf_team_box(game_id = 420339))
#> Error : `phf_team_box()` was deprecated in fastRhockey 1.0.0 and is now defunct.
#> ℹ The PHF has ceased operations.
# }
```
