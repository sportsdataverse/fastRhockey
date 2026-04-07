# **PWHL Player Game Box Scores**

PWHL Player Box Scores

## Usage

``` r
pwhl_player_box(game_id)
```

## Arguments

- game_id:

  Game ID that you want play-by-play for

## Value

A data frame with play-by-play data from the PWHL

## Examples

``` r
# \donttest{
  try(pwhl_player_box(game_id = 27))
#> $skaters
#>    player_id   first_name   last_name position team_id game_id league   toi
#> 1          2        Emily       Brown       LD       1      27   pwhl 21:18
#> 2         12        Megan      Keller       LD       1      27   pwhl 25:31
#> 3        167       Sidney       Morin       RD       1      27   pwhl 19:26
#> 4         18       Sophie     Shirley       RW       1      27   pwhl 15:02
#> 5         15        Alina      Müller        C       1      27   pwhl 19:19
#> 6        111       Taylor Wenczkowski       RW       1      27   pwhl 12:46
#> 7          7      Kaleigh     Fratkin       RD       1      27   pwhl 21:52
#> 8        110       Amanda      Pelkey       RW       1      27   pwhl  7:45
#> 9          9       Taylor      Girard       LW       1      27   pwhl  9:32
#> 10        11       Sophie      Jaques       LD       1      27   pwhl 15:54
#> 11       109         Gigi      Marvin        C       1      27   pwhl 18:07
#> 12         1       Hannah      Brandt        C       1      27   pwhl 15:51
#> 13        13       Hilary      Knight       LW       1      27   pwhl 18:09
#> 14         5      Jessica  DiGirolamo       RD       1      27   pwhl 21:44
#> 15         4       Shiann  Darkangelo        C       1      27   pwhl 15:34
#> 16         8        Loren       Gabel       RW       1      27   pwhl 13:12
#> 17        17      Theresa   Schafzahl       LW       1      27   pwhl 15:15
#> 18        16    Jamie Lee     Rattray       LW       1      27   pwhl 17:49
#> 19        10         Jess      Healey        D       1      27   pwhl  0:00
#> 20        99       Mariah     Keopple       LD       3      27   pwhl 23:27
#> 21       163    Catherine      Daoust       RD       3      27   pwhl 19:12
#> 22       162      Madison       Bizal       RD       3      27   pwhl 10:54
#> 23        32        Laura      Stacey       RW       3      27   pwhl 19:16
#> 24       156    Gabrielle       David        C       3      27   pwhl 15:59
#> 25        84         Kati       Tabin       LD       3      27   pwhl 22:57
#> 26       164     Brigitte   Laganière       LD       3      27   pwhl  4:35
#> 27       159         Leah         Lum       LD       3      27   pwhl 11:51
#> 28        27      Jillian     Dempsey        C       3      27   pwhl  7:05
#> 29        98      Maureen      Murphy       RW       3      27   pwhl 18:23
#> 30       158        Sarah      Lefort       LW       3      27   pwhl 13:22
#> 31       161       Tereza    Vanišová       LW       3      27   pwhl 17:17
#> 32        29      Kennedy   Marchment       RW       3      27   pwhl 17:11
#> 33        83         Erin     Ambrose       RD       3      27   pwhl 26:44
#> 34        80   Ann-Sophie      Bettez       LW       3      27   pwhl 16:04
#> 35        81        Sarah      Bujold        C       3      27   pwhl 16:31
#> 36       157    Catherine      Dubois       LW       3      27   pwhl  7:41
#> 37        31 Marie-Philip      Poulin        C       3      27   pwhl 19:26
#> 38        26       Claire      Dalton       RW       3      27   pwhl 10:26
#>    time_on_ice goals assists points shots hits blocked_shots penalty_minutes
#> 1         21.3     0       0      0     1    0             0               0
#> 2         25.5     0       0      0     1    0             0               0
#> 3         19.4     0       0      0     5    0             0               0
#> 4         15.0     1       0      1     4    0             0               0
#> 5         19.3     0       0      0     1    0             0               0
#> 6         12.8     0       0      0     0    0             0               0
#> 7         21.9     0       0      0     0    0             0               0
#> 8          7.8     0       0      0     1    0             0               0
#> 9          9.5     0       0      0     1    0             0               0
#> 10        15.9     0       0      0     2    0             0               0
#> 11        18.1     0       0      0     1    0             0               0
#> 12        15.8     0       1      1     0    0             0               2
#> 13        18.1     0       0      0     1    0             0               0
#> 14        21.7     0       0      0     1    0             0               2
#> 15        15.6     0       0      0     2    0             0               0
#> 16        13.2     0       0      0     3    0             0               0
#> 17        15.2     0       0      0     0    0             0               0
#> 18        17.8     0       0      0     3    0             0               0
#> 19         0.0     0       0      0     0    0             0               0
#> 20        23.4     0       0      0     1    0             0               0
#> 21        19.2     0       0      0     0    0             0               0
#> 22        10.9     0       0      0     1    0             0               2
#> 23        19.3     1       0      1     5    0             0               0
#> 24        16.0     0       0      0     3    0             0               0
#> 25        23.0     0       0      0     7    0             0               0
#> 26         4.6     0       0      0     1    0             0               0
#> 27        11.8     0       0      0     0    0             0               0
#> 28         7.1     0       0      0     1    0             0               2
#> 29        18.4     0       1      1     2    0             0               0
#> 30        13.4     0       0      0     1    0             0               0
#> 31        17.3     0       0      0     2    0             0               0
#> 32        17.2     0       0      0     2    0             0               0
#> 33        26.7     1       1      2     2    0             0               0
#> 34        16.1     0       0      0     0    0             0               0
#> 35        16.5     0       0      0     2    0             0               0
#> 36         7.7     0       0      0     0    0             0               0
#> 37        19.4     0       2      2     0    0             0               0
#> 38        10.4     0       0      0     0    0             0               0
#>    plus_minus faceoff_attempts faceoff_wins faceoff_losses faceoff_pct starting
#> 1          -1                0            0              0         NaN        0
#> 2          -1                0            0              0         NaN        1
#> 3           1                0            0              0         NaN        0
#> 4           1                0            0              0         NaN        0
#> 5          -2               16            8              8   0.5000000        1
#> 6           0                1            1              0   1.0000000        0
#> 7           0                0            0              0         NaN        1
#> 8          -1                0            0              0         NaN        0
#> 9           0                0            0              0         NaN        0
#> 10          1                0            0              0         NaN        0
#> 11          0               11            6              5   0.5454545        0
#> 12          1               12           10              2   0.8333333        0
#> 13          0                0            0              0         NaN        0
#> 14         -1                0            0              0         NaN        0
#> 15          0                6            3              3   0.5000000        0
#> 16          0                0            0              0         NaN        1
#> 17          0                0            0              0         NaN        0
#> 18         -1                2            2              0   1.0000000        1
#> 19          0                0            0              0         NaN        0
#> 20          1                0            0              0         NaN        1
#> 21         -1                0            0              0         NaN        0
#> 22         -1                0            0              0         NaN        0
#> 23          1                0            0              0         NaN        0
#> 24          0               10            3              7   0.3000000        0
#> 25          0                0            0              0         NaN        0
#> 26          0                0            0              0         NaN        0
#> 27          0                0            0              0         NaN        0
#> 28          0                4            1              3   0.2500000        1
#> 29          2                4            3              1   0.7500000        0
#> 30         -1                0            0              0         NaN        0
#> 31          1                0            0              0         NaN        0
#> 32         -1                0            0              0         NaN        0
#> 33          2                0            0              0         NaN        1
#> 34          0                0            0              0         NaN        0
#> 35         -1                8            4              4   0.5000000        0
#> 36          0                0            0              0         NaN        1
#> 37          1               22            7             15   0.3181818        0
#> 38          0                0            0              0         NaN        1
#> 
#> $goalies
#>   player_id first_name last_name position team_id game_id league   toi
#> 1        19       Emma Söderberg        G       1      27   pwhl     0
#> 2         6      Aerin   Frankel        G       1      27   pwhl 62:36
#> 3        85     Elaine     Chuli        G       3      27   pwhl 62:36
#> 4        28  Ann-Renée  Desbiens        G       3      27   pwhl     0
#>   time_on_ice saves goals_against shots_against goals assists points
#> 1          NA     0             0             0     0       0      0
#> 2        62.6    28             2            30     0       0      0
#> 3        62.6    26             1            27     0       0      0
#> 4          NA     0             0             0     0       0      0
#>   penalty_minutes faceoff_attempts faceoff_wins faceoff_losses faceoff_pct
#> 1               0                0            0              0         NaN
#> 2               0                0            0              0         NaN
#> 3               0                0            0              0         NaN
#> 4               0                0            0              0         NaN
#>   starting
#> 1        0
#> 2        1
#> 3        1
#> 4        0
#> 
# }
```
