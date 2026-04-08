# **PWHL Standings**

PWHL Standings lookup

## Usage

``` r
pwhl_standings(season = 2023, regular = TRUE)
```

## Arguments

- season:

  Season (YYYY) to pull the roster from, the concluding year in XXXX-YY
  format

- regular:

  Bool for whether to pull regular or pre-season rosters

## Value

A data frame with columns including: team_rank, team, team_code,
games_played, points, wins, losses, goals_for, goals_against, ot_wins,
ot_losses, power_play_pct, penalty_kill_pct.

## Examples

``` r
# \donttest{
  try(pwhl_standings(season = 2024, regular = TRUE))
#>   team_rank               team team_code games_played points wins non_reg_wins
#> 1         1   x - PWHL Toronto   x - TOR           24     47   13            4
#> 2         2  x - PWHL Montreal   x - MTL           24     41   10            3
#> 3         3    x - PWHL Boston   x - BOS           24     35    8            4
#> 4         4 x - PWHL Minnesota   x - MIN           24     35    8            4
#> 5         5    e - PWHL Ottawa   e - OTT           24     32    8            1
#> 6         6  e - PWHL New York    e - NY           24     26    5            4
#>   losses non_reg_losses goals_for goals_against games_remaining ot_wins
#> 1      7              0        69            50               0       2
#> 2      6              5        60            57               0       3
#> 3      9              3        50            57               0       3
#> 4      9              3        54            54               0       2
#> 5      9              6        62            63               0       0
#> 6     12              3        53            67               0       3
#>   ot_losses so_wins so_losses power_play_goals power_play_goals_against
#> 1         0       2         0               11                        6
#> 2         2       0         3               13                       12
#> 3         2       1         1                4                       13
#> 4         3       2         0                5                       20
#> 5         4       1         2               16                        9
#> 6         2       1         1               19                        8
#>   power_plays power_play_pct short_handed_goals short_handed_goals_against
#> 1          70          15.7%                  3                          2
#> 2          82          15.9%                  1                          4
#> 3          53           7.5%                  2                          1
#> 4          61           8.2%                  2                          0
#> 5          64          25.0%                  3                          3
#> 6          78          24.4%                  2                          3
#>   times_short_handed penalty_kill_pct
#> 1                 73            91.8%
#> 2                 75            84.0%
#> 3                 65            80.0%
#> 4                 61            67.2%
#> 5                 64            85.9%
#> 6                 70            88.6%
# }
```
