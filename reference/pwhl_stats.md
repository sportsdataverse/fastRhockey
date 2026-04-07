# **PWHL Stats**

PWHL Stats lookup

## Usage

``` r
pwhl_stats(position = "goalie", team = "all", season = 2024, regular = TRUE)
```

## Arguments

- position:

  either goalie or skater.

- team:

  Team abbreviation to filter skaters by (e.g., "BOS"). Ignored for
  goalies.

- season:

  Season (YYYY) to pull the stats from, the concluding year in XXXX-YY
  format

- regular:

  Bool for whether to pull regular or pre-season stats

## Value

A data frame with player stats

## Examples

``` r
# \donttest{
  try(pwhl_stats(position = "goalie", season = 2024))
#>    player_id         player_name team games_played minutes minute second
#> 1        154        Lindsey Post   NY            2   74:31     74     31
#> 2         85        Elaine Chuli  MTL            8  483:41    483     41
#> 3         70          Erica Howe  TOR            3  158:24    158     24
#> 4         64    Kristen Campbell  TOR           22 1293:57   1293     57
#> 5          6       Aerin Frankel  BOS           18 1050:52   1050     52
#> 6        123       Maddie Rooney  MIN           10  605:34    605     34
#> 7         22      Nicole Hensley  MIN           14  849:22    849     22
#> 8         28  Ann-Renée Desbiens  MTL           16  975:33    975     33
#> 9         59 Emerance Maschmeyer  OTT           23 1332:07   1332     07
#> 10       155   Corinne Schroeder   NY           15  901:24    901     24
#> 11        19      Emma Söderberg  BOS            8  401:24    401     24
#> 12        41          Abbey Levy   NY            8  469:19    469     19
#> 13        48   Sandra Abstreiter  OTT            3  116:42    116     42
#>    shots_faced goals_against goals_against_avg save_percentage shutouts wins
#> 1           37             2              1.61           0.946        0    1
#> 2          253            13              1.61           0.949        0    6
#> 3           63             5              1.89           0.921        0    1
#> 4          587            43              1.99           0.927        3   16
#> 5          490            35              2.00           0.929        1    8
#> 6          246            21              2.08           0.915        2    5
#> 7          383            31              2.19           0.919        1    7
#> 8          478            37              2.28           0.923        1    7
#> 9          599            51              2.30           0.915        2    9
#> 10         511            36              2.40           0.930        1    7
#> 11         170            17              2.54           0.900        0    4
#> 12         254            24              3.07           0.906        0    1
#> 13          69             6              3.08           0.913        0    0
#>    losses so_att so_goals_against so_save_percentage
#> 1       0      0                0              0.000
#> 2       1      5                3              0.400
#> 3       1      0                0              0.000
#> 4       6     10                1              0.900
#> 5       6     10                3              0.700
#> 6       3      3                1              0.667
#> 7       6      4                1              0.750
#> 8       5      9                4              0.556
#> 9       9      8                3              0.625
#> 10      7      5                2              0.600
#> 11      3      0                0              0.000
#> 12      5      1                0              1.000
#> 13      0      4                3              0.250
# }
```
