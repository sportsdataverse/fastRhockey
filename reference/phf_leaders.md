# **PHF Player Leaderboards**

PHF Player Leaderboards

**\[deprecated\]**

The PHF has ceased operations. This function is deprecated and will be
removed in a future release.

## Usage

``` r
phf_leaders(
  player_type,
  season = most_recent_phf_season(),
  season_type = "Regular Season"
)
```

## Arguments

- player_type:

  Player type: skaters, goalies

- season:

  Season (YYYY) to pull the team stats from, the concluding year in
  XXXX-YY format

- season_type:

  Season type: Regular Season or Playoffs

## Value

A data frame of stat leaders
