# **PHF Game All**

phf_game_all: pull in the raw data for a game_id from the PHF/NWHL API

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_game_all(game_id)
```

## Arguments

- game_id:

  The unique ID code for the game that you are interested in viewing the
  data for

## Value

A named list of data frames: plays, team_box, skaters, goalies,
game_details, scoring_summary, shootout_summary, penalty_summary,
officials, team_staff, timeouts
