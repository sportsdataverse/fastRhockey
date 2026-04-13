# **PWHL Play-by-play**

PWHL Play-by-play

## Usage

``` r
pwhl_pbp(game_id)
```

## Arguments

- game_id:

  Game ID that you want play-by-play for

## Value

A data frame with play-by-play event data including columns: game_id,
event, team_id, period_of_game, time_of_period, player_id,
player_name_first, player_name_last, player_position, x_coord, y_coord,
and additional event-specific columns (shot_quality, goal,
penalty_length, home_win, etc.). Coordinate columns are provided in
multiple projections.

## Examples

``` r
# \donttest{
try(pwhl_pbp(game_id = 27))
#>     game_id   event team_id period_of_game time_of_period player_id
#> 1        27 faceoff      NA              1           0:00        15
#> 2        27 faceoff      NA              1           0:31        15
#> 3        27    shot       3              1           1:14        84
#> 4        27 faceoff      NA              1           1:22       109
#> 5        27 faceoff      NA              1           1:58         4
#> 6        27    shot       1              1           3:49       167
#> 7        27    shot       3              1           4:27        84
#> 8        27    shot       3              1           4:41        32
#> 9        27    shot       3              1           6:21       161
#> 10       27    shot       1              1           6:41       110
#> 11       27    shot       3              1           7:17        84
#> 12       27 faceoff      NA              1           8:35         1
#> 13       27 faceoff      NA              1           9:02         1
#> 14       27 penalty       1              1           9:27         1
#> 15       27 faceoff      NA              1           9:27         4
#> 16       27    shot       3              1          10:24        98
#> 17       27 faceoff      NA              1          11:20       109
#> 18       27    shot       1              1          12:43        18
#> 19       27 faceoff      NA              1          12:54         1
#> 20       27    shot       1              1          13:39         4
#> 21       27    shot       1              1          15:19       167
#> 22       27 faceoff      NA              1          15:30        15
#> 23       27    shot       1              1          15:33        18
#> 24       27    shot       3              1          15:59        83
#> 25       27    goal       3              1          15:59        83
#> 26       27    shot       3              1          16:12        32
#> 27       27    shot       1              1          17:18        11
#> 28       27 faceoff      NA              1          17:25        15
#> 29       27 faceoff      NA              1          17:33        15
#> 30       27    shot       3              1          17:52        99
#> 31       27 faceoff      NA              1          17:52         1
#> 32       27    shot       3              1          18:23        84
#> 33       27    shot       1              1          19:21        16
#> 34       27 faceoff      NA              2           0:00        15
#> 35       27    shot       1              2           1:30        18
#> 36       27    shot       1              2           4:04        16
#> 37       27    shot       1              2           4:26        11
#> 38       27 faceoff      NA              2           4:32       109
#> 39       27 penalty       3              2           5:39        27
#> 40       27 faceoff      NA              2           5:39        15
#> 41       27 faceoff      NA              2           6:44        16
#> 42       27    shot       1              2           6:45         8
#> 43       27    shot       1              2           7:04       167
#> 44       27    shot       1              2           7:44         8
#> 45       27    shot       3              2           8:38        27
#> 46       27 faceoff      NA              2           8:38         1
#> 47       27    shot       3              2           9:05       164
#> 48       27    shot       3              2           9:25       162
#> 49       27 faceoff      NA              2          10:26       109
#> 50       27    shot       3              2          10:38       158
#> 51       27    shot       3              2          11:30        84
#> 52       27 penalty       1              2          11:53         5
#> 53       27 faceoff      NA              2          11:53         1
#> 54       27 faceoff      NA              2          12:49         4
#> 55       27 penalty       3              2          14:24       162
#> 56       27 faceoff      NA              2          14:24        16
#> 57       27    shot       1              2          15:03       167
#> 58       27 faceoff      NA              2          16:20        15
#> 59       27    shot       1              2          16:36         5
#> 60       27    shot       3              2          17:43        84
#> 61       27 faceoff      NA              2          17:43       109
#> 62       27 faceoff      NA              2          18:40         1
#> 63       27 faceoff      NA              2          19:36        15
#> 64       27 faceoff      NA              3           0:00        15
#> 65       27 faceoff      NA              3           0:47         1
#> 66       27    shot       1              3           0:54         8
#> 67       27    shot       3              3           1:27        29
#> 68       27 faceoff      NA              3           1:49       109
#> 69       27    shot       3              3           2:20        83
#> 70       27 faceoff      NA              3           2:20        15
#> 71       27 faceoff      NA              3           3:19         4
#> 72       27    shot       3              3           3:43        32
#> 73       27 faceoff      NA              3           3:43         1
#> 74       27    shot       1              3           4:12        18
#> 75       27    goal       1              3           4:12        18
#> 76       27 faceoff      NA              3           4:12       109
#> 77       27 faceoff      NA              3           4:21       109
#> 78       27    shot       1              3           4:39       109
#> 79       27 faceoff      NA              3           4:39        15
#> 80       27    shot       1              3           5:06         9
#> 81       27    shot       1              3           5:20        16
#> 82       27    shot       3              3           6:02        29
#> 83       27 faceoff      NA              3           6:02         1
#> 84       27    shot       3              3           6:22        84
#> 85       27    shot       3              3           6:45       161
#> 86       27    shot       1              3           8:37       167
#> 87       27    shot       3              3           9:46        81
#> 88       27 faceoff      NA              3           9:46         1
#> 89       27 faceoff      NA              3          10:18       109
#> 90       27    shot       3              3          10:34       156
#> 91       27 faceoff      NA              3          11:15         4
#> 92       27 faceoff      NA              3          11:24         4
#> 93       27    shot       1              3          13:15         2
#> 94       27 faceoff      NA              3          13:15       109
#> 95       27 faceoff      NA              3          13:55        15
#> 96       27    shot       3              3          14:38        81
#> 97       27 faceoff      NA              3          14:38         1
#> 98       27    shot       3              3          15:11        32
#> 99       27 faceoff      NA              3          15:32       109
#> 100      27 faceoff      NA              3          17:44       111
#> 101      27 faceoff      NA              3          18:00        15
#> 102      27    shot       1              3          18:47        12
#> 103      27    shot       3              3          19:00       156
#> 104      27 faceoff      NA              4           0:00        15
#> 105      27    shot       3              4           0:25        98
#> 106      27    shot       1              4           1:05        15
#> 107      27    shot       1              4           1:22        13
#> 108      27    shot       3              4           2:00       156
#> 109      27    shot       1              4           2:09         4
#> 110      27 faceoff      NA              4           2:09        15
#> 111      27    shot       3              4           2:36        32
#> 112      27    goal       3              4           2:36        32
#>     player_name_first player_name_last player_position player_two_id
#> 1               Alina           Müller               C            NA
#> 2               Alina           Müller               C            NA
#> 3                Kati            Tabin              LD            NA
#> 4                Gigi           Marvin               C            NA
#> 5              Shiann       Darkangelo               C            NA
#> 6              Sidney            Morin              RD            NA
#> 7                Kati            Tabin              LD            NA
#> 8               Laura           Stacey              RW            NA
#> 9              Tereza         Vanišová              LW            NA
#> 10             Amanda           Pelkey              RW            NA
#> 11               Kati            Tabin              LD            NA
#> 12             Hannah           Brandt               C            NA
#> 13             Hannah           Brandt               C            NA
#> 14             Hannah           Brandt               C             1
#> 15             Shiann       Darkangelo               C            NA
#> 16            Maureen           Murphy              RW            NA
#> 17               Gigi           Marvin               C            NA
#> 18             Sophie          Shirley              RW            NA
#> 19             Hannah           Brandt               C            NA
#> 20             Shiann       Darkangelo               C            NA
#> 21             Sidney            Morin              RD            NA
#> 22              Alina           Müller               C            NA
#> 23             Sophie          Shirley              RW            NA
#> 24               Erin          Ambrose              RD            NA
#> 25               Erin          Ambrose              RD            31
#> 26              Laura           Stacey              RW            NA
#> 27             Sophie           Jaques              LD            NA
#> 28              Alina           Müller               C            NA
#> 29              Alina           Müller               C            NA
#> 30             Mariah          Keopple              LD            NA
#> 31             Hannah           Brandt               C            NA
#> 32               Kati            Tabin              LD            NA
#> 33          Jamie Lee          Rattray              LW            NA
#> 34              Alina           Müller               C            NA
#> 35             Sophie          Shirley              RW            NA
#> 36          Jamie Lee          Rattray              LW            NA
#> 37             Sophie           Jaques              LD            NA
#> 38               Gigi           Marvin               C            NA
#> 39            Jillian          Dempsey               C            27
#> 40              Alina           Müller               C            NA
#> 41          Jamie Lee          Rattray              LW            NA
#> 42              Loren            Gabel              RW            NA
#> 43             Sidney            Morin              RD            NA
#> 44              Loren            Gabel              RW            NA
#> 45            Jillian          Dempsey               C            NA
#> 46             Hannah           Brandt               C            NA
#> 47           Brigitte        Laganière              LD            NA
#> 48            Madison            Bizal              RD            NA
#> 49               Gigi           Marvin               C            NA
#> 50              Sarah           Lefort              LW            NA
#> 51               Kati            Tabin              LD            NA
#> 52            Jessica       DiGirolamo              RD             5
#> 53             Hannah           Brandt               C            NA
#> 54             Shiann       Darkangelo               C            NA
#> 55            Madison            Bizal              RD           162
#> 56          Jamie Lee          Rattray              LW            NA
#> 57             Sidney            Morin              RD            NA
#> 58              Alina           Müller               C            NA
#> 59            Jessica       DiGirolamo              RD            NA
#> 60               Kati            Tabin              LD            NA
#> 61               Gigi           Marvin               C            NA
#> 62             Hannah           Brandt               C            NA
#> 63              Alina           Müller               C            NA
#> 64              Alina           Müller               C            NA
#> 65             Hannah           Brandt               C            NA
#> 66              Loren            Gabel              RW            NA
#> 67            Kennedy        Marchment              RW            NA
#> 68               Gigi           Marvin               C            NA
#> 69               Erin          Ambrose              RD            NA
#> 70              Alina           Müller               C            NA
#> 71             Shiann       Darkangelo               C            NA
#> 72              Laura           Stacey              RW            NA
#> 73             Hannah           Brandt               C            NA
#> 74             Sophie          Shirley              RW            NA
#> 75             Sophie          Shirley              RW             1
#> 76               Gigi           Marvin               C            NA
#> 77               Gigi           Marvin               C            NA
#> 78               Gigi           Marvin               C            NA
#> 79              Alina           Müller               C            NA
#> 80             Taylor           Girard              LW            NA
#> 81          Jamie Lee          Rattray              LW            NA
#> 82            Kennedy        Marchment              RW            NA
#> 83             Hannah           Brandt               C            NA
#> 84               Kati            Tabin              LD            NA
#> 85             Tereza         Vanišová              LW            NA
#> 86             Sidney            Morin              RD            NA
#> 87              Sarah           Bujold               C            NA
#> 88             Hannah           Brandt               C            NA
#> 89               Gigi           Marvin               C            NA
#> 90          Gabrielle            David               C            NA
#> 91             Shiann       Darkangelo               C            NA
#> 92             Shiann       Darkangelo               C            NA
#> 93              Emily            Brown              LD            NA
#> 94               Gigi           Marvin               C            NA
#> 95              Alina           Müller               C            NA
#> 96              Sarah           Bujold               C            NA
#> 97             Hannah           Brandt               C            NA
#> 98              Laura           Stacey              RW            NA
#> 99               Gigi           Marvin               C            NA
#> 100            Taylor      Wenczkowski              RW            NA
#> 101             Alina           Müller               C            NA
#> 102             Megan           Keller              LD            NA
#> 103         Gabrielle            David               C            NA
#> 104             Alina           Müller               C            NA
#> 105           Maureen           Murphy              RW            NA
#> 106             Alina           Müller               C            NA
#> 107            Hilary           Knight              LW            NA
#> 108         Gabrielle            David               C            NA
#> 109            Shiann       Darkangelo               C            NA
#> 110             Alina           Müller               C            NA
#> 111             Laura           Stacey              RW            NA
#> 112             Laura           Stacey              RW            83
#>     player_two_name_first player_two_name_last player_two_position   x_coord
#> 1                    <NA>                 <NA>                <NA>   0.00000
#> 2                    <NA>                 <NA>                <NA>  52.33333
#> 3                    <NA>                 <NA>                <NA>  60.00000
#> 4                    <NA>                 <NA>                <NA> -66.66667
#> 5                    <NA>                 <NA>                <NA> -32.33333
#> 6                    <NA>                 <NA>                <NA> -58.66667
#> 7                    <NA>                 <NA>                <NA>  65.33333
#> 8                    <NA>                 <NA>                <NA>  51.00000
#> 9                    <NA>                 <NA>                <NA>  45.33333
#> 10                   <NA>                 <NA>                <NA> -52.33333
#> 11                   <NA>                 <NA>                <NA>  54.66667
#> 12                   <NA>                 <NA>                <NA>  52.33333
#> 13                   <NA>                 <NA>                <NA>  52.33333
#> 14                 Hannah               Brandt                   C        NA
#> 15                   <NA>                 <NA>                <NA>   0.00000
#> 16                   <NA>                 <NA>                <NA>  61.00000
#> 17                   <NA>                 <NA>                <NA>  52.33333
#> 18                   <NA>                 <NA>                <NA> -55.33333
#> 19                   <NA>                 <NA>                <NA>  52.33333
#> 20                   <NA>                 <NA>                <NA> -54.33333
#> 21                   <NA>                 <NA>                <NA> -46.33333
#> 22                   <NA>                 <NA>                <NA>   0.00000
#> 23                   <NA>                 <NA>                <NA> -66.66667
#> 24                   <NA>                 <NA>                <NA>  67.00000
#> 25           Marie-Philip               Poulin                   C  67.00000
#> 26                   <NA>                 <NA>                <NA>  51.00000
#> 27                   <NA>                 <NA>                <NA> -67.66667
#> 28                   <NA>                 <NA>                <NA> -66.66667
#> 29                   <NA>                 <NA>                <NA> -32.33333
#> 30                   <NA>                 <NA>                <NA>  55.33333
#> 31                   <NA>                 <NA>                <NA>  52.33333
#> 32                   <NA>                 <NA>                <NA> -64.66667
#> 33                   <NA>                 <NA>                <NA>  -9.00000
#> 34                   <NA>                 <NA>                <NA>   0.00000
#> 35                   <NA>                 <NA>                <NA> -59.00000
#> 36                   <NA>                 <NA>                <NA> -56.33333
#> 37                   <NA>                 <NA>                <NA> -55.00000
#> 38                   <NA>                 <NA>                <NA> -17.66667
#> 39                Jillian              Dempsey                   C        NA
#> 40                   <NA>                 <NA>                <NA>   0.00000
#> 41                   <NA>                 <NA>                <NA>   0.00000
#> 42                   <NA>                 <NA>                <NA> -61.33333
#> 43                   <NA>                 <NA>                <NA> -62.66667
#> 44                   <NA>                 <NA>                <NA> -73.33333
#> 45                   <NA>                 <NA>                <NA>  66.33333
#> 46                   <NA>                 <NA>                <NA>  66.66667
#> 47                   <NA>                 <NA>                <NA>  39.00000
#> 48                   <NA>                 <NA>                <NA> -31.00000
#> 49                   <NA>                 <NA>                <NA> -52.33333
#> 50                   <NA>                 <NA>                <NA>  55.33333
#> 51                   <NA>                 <NA>                <NA>  56.66667
#> 52                Jessica           DiGirolamo                  RD        NA
#> 53                   <NA>                 <NA>                <NA>   0.00000
#> 54                   <NA>                 <NA>                <NA>  32.33333
#> 55                Madison                Bizal                  RD        NA
#> 56                   <NA>                 <NA>                <NA> -52.33333
#> 57                   <NA>                 <NA>                <NA> -53.00000
#> 58                   <NA>                 <NA>                <NA> -52.33333
#> 59                   <NA>                 <NA>                <NA> -37.00000
#> 60                   <NA>                 <NA>                <NA>  38.00000
#> 61                   <NA>                 <NA>                <NA>  66.66667
#> 62                   <NA>                 <NA>                <NA> -17.66667
#> 63                   <NA>                 <NA>                <NA>  66.66667
#> 64                   <NA>                 <NA>                <NA>   0.00000
#> 65                   <NA>                 <NA>                <NA> -66.66667
#> 66                   <NA>                 <NA>                <NA> -59.00000
#> 67                   <NA>                 <NA>                <NA>  21.33333
#> 68                   <NA>                 <NA>                <NA> -66.66667
#> 69                   <NA>                 <NA>                <NA>  55.33333
#> 70                   <NA>                 <NA>                <NA>  52.33333
#> 71                   <NA>                 <NA>                <NA>  17.66667
#> 72                   <NA>                 <NA>                <NA>  79.66667
#> 73                   <NA>                 <NA>                <NA>  52.33333
#> 74                   <NA>                 <NA>                <NA> -58.33333
#> 75                 Hannah               Brandt                   C -58.33333
#> 76                   <NA>                 <NA>                <NA>   0.00000
#> 77                   <NA>                 <NA>                <NA>   0.00000
#> 78                   <NA>                 <NA>                <NA> -55.33333
#> 79                   <NA>                 <NA>                <NA> -66.66667
#> 80                   <NA>                 <NA>                <NA> -57.00000
#> 81                   <NA>                 <NA>                <NA> -59.00000
#> 82                   <NA>                 <NA>                <NA>  82.00000
#> 83                   <NA>                 <NA>                <NA>  52.33333
#> 84                   <NA>                 <NA>                <NA>  37.00000
#> 85                   <NA>                 <NA>                <NA>  65.00000
#> 86                   <NA>                 <NA>                <NA> -39.33333
#> 87                   <NA>                 <NA>                <NA>  45.00000
#> 88                   <NA>                 <NA>                <NA>  52.33333
#> 89                   <NA>                 <NA>                <NA>  52.33333
#> 90                   <NA>                 <NA>                <NA>  59.33333
#> 91                   <NA>                 <NA>                <NA> -32.33333
#> 92                   <NA>                 <NA>                <NA>   0.00000
#> 93                   <NA>                 <NA>                <NA> -43.00000
#> 94                   <NA>                 <NA>                <NA> -66.66667
#> 95                   <NA>                 <NA>                <NA> -32.33333
#> 96                   <NA>                 <NA>                <NA>  82.00000
#> 97                   <NA>                 <NA>                <NA> -32.33333
#> 98                   <NA>                 <NA>                <NA>  77.33333
#> 99                   <NA>                 <NA>                <NA> -66.66667
#> 100                  <NA>                 <NA>                <NA> -66.66667
#> 101                  <NA>                 <NA>                <NA>  52.33333
#> 102                  <NA>                 <NA>                <NA> -39.00000
#> 103                  <NA>                 <NA>                <NA>  75.00000
#> 104                  <NA>                 <NA>                <NA>   0.00000
#> 105                  <NA>                 <NA>                <NA>  59.66667
#> 106                  <NA>                 <NA>                <NA> -80.66667
#> 107                  <NA>                 <NA>                <NA> -60.33333
#> 108                  <NA>                 <NA>                <NA>  58.33333
#> 109                  <NA>                 <NA>                <NA> -75.00000
#> 110                  <NA>                 <NA>                <NA> -52.33333
#> 111                  <NA>                 <NA>                <NA>  69.00000
#> 112                  Erin              Ambrose                  RD  69.00000
#>        y_coord home_win player_team_id event_type       shot_quality  goal
#> 1     0.000000        1             NA       <NA>               <NA>    NA
#> 2    28.616667        1             NA       <NA>               <NA>    NA
#> 3    17.000000       NA              3    Default     Quality on net FALSE
#> 4   -15.866667        0             NA       <NA>               <NA>    NA
#> 5   -21.816667        1             NA       <NA>               <NA>    NA
#> 6   -12.750000       NA              1    Default     Quality on net FALSE
#> 7    11.050000       NA              3    Default     Quality on net FALSE
#> 8     1.416667       NA              3    Default Non quality on net FALSE
#> 9    15.300000       NA              3    Default Non quality on net FALSE
#> 10  -14.450000       NA              1    Default     Quality on net FALSE
#> 11   17.283333       NA              3    Default     Quality on net FALSE
#> 12   28.616667        1             NA       <NA>               <NA>    NA
#> 13   28.616667        1             NA       <NA>               <NA>    NA
#> 14          NA       NA             NA    Hooking               <NA>    NA
#> 15    0.000000        0             NA       <NA>               <NA>    NA
#> 16    2.266667       NA              3    Default     Quality on net FALSE
#> 17   28.616667        1             NA       <NA>               <NA>    NA
#> 18  -26.916667       NA              1    Default Non quality on net FALSE
#> 19   28.616667        1             NA       <NA>               <NA>    NA
#> 20  -11.050000       NA              1    Default     Quality on net FALSE
#> 21    8.783333       NA              1    Default Non quality on net FALSE
#> 22    0.000000        1             NA       <NA>               <NA>    NA
#> 23  -17.566667       NA              1    Default     Quality on net FALSE
#> 24   -9.066667       NA              3    Default       Quality goal  TRUE
#> 25   -9.066667       NA              3       <NA>               <NA>    NA
#> 26   19.833333       NA              3    Default Non quality on net FALSE
#> 27   -2.833333       NA              1    Default     Quality on net FALSE
#> 28   28.616667        0             NA       <NA>               <NA>    NA
#> 29   34.850000        0             NA       <NA>               <NA>    NA
#> 30   31.450000       NA              3    Default Non quality on net FALSE
#> 31   28.616667        1             NA       <NA>               <NA>    NA
#> 32  -28.333333       NA              3    Default Non quality on net FALSE
#> 33   21.816667       NA              1    Default Non quality on net FALSE
#> 34    0.000000        1             NA       <NA>               <NA>    NA
#> 35    4.250000       NA              1    Default     Quality on net FALSE
#> 36    9.916667       NA              1    Default     Quality on net FALSE
#> 37   16.150000       NA              1    Default     Quality on net FALSE
#> 38   21.816667        1             NA       <NA>               <NA>    NA
#> 39          NA       NA             NA   Slashing               <NA>    NA
#> 40    0.000000        1             NA       <NA>               <NA>    NA
#> 41    0.000000        1             NA       <NA>               <NA>    NA
#> 42  -18.133333       NA              1    Default     Quality on net FALSE
#> 43   24.083333       NA              1    Default Non quality on net FALSE
#> 44   13.600000       NA              1    Default     Quality on net FALSE
#> 45   21.250000       NA              3    Default     Quality on net FALSE
#> 46   15.866667        1             NA       <NA>               <NA>    NA
#> 47   -7.650000       NA              3    Default Non quality on net FALSE
#> 48  -27.483333       NA              3    Default Non quality on net FALSE
#> 49   15.866667        1             NA       <NA>               <NA>    NA
#> 50   13.316667       NA              3    Default     Quality on net FALSE
#> 51   -9.633333       NA              3    Default     Quality on net FALSE
#> 52          NA       NA             NA   Roughing               <NA>    NA
#> 53    0.000000        0             NA       <NA>               <NA>    NA
#> 54  -34.850000        1             NA       <NA>               <NA>    NA
#> 55          NA       NA             NA   Tripping               <NA>    NA
#> 56  -28.616667        1             NA       <NA>               <NA>    NA
#> 57   26.066667       NA              1    Default Non quality on net FALSE
#> 58   15.866667        0             NA       <NA>               <NA>    NA
#> 59   20.683333       NA              1    Default Non quality on net FALSE
#> 60   18.133333       NA              3    Default Non quality on net FALSE
#> 61   15.866667        1             NA       <NA>               <NA>    NA
#> 62  -34.850000        0             NA       <NA>               <NA>    NA
#> 63   15.866667        1             NA       <NA>               <NA>    NA
#> 64    0.000000        0             NA       <NA>               <NA>    NA
#> 65   28.616667        1             NA       <NA>               <NA>    NA
#> 66  -18.700000       NA              1    Default     Quality on net FALSE
#> 67   19.833333       NA              3    Default Non quality on net FALSE
#> 68   28.616667        0             NA       <NA>               <NA>    NA
#> 69   -3.400000       NA              3    Default     Quality on net FALSE
#> 70  -15.866667        0             NA       <NA>               <NA>    NA
#> 71  -21.816667        1             NA       <NA>               <NA>    NA
#> 72   -7.650000       NA              3    Default     Quality on net FALSE
#> 73  -15.866667        1             NA       <NA>               <NA>    NA
#> 74   -0.850000       NA              1    Default       Quality goal  TRUE
#> 75   -0.850000       NA              1       <NA>               <NA>    NA
#> 76    0.000000        1             NA       <NA>               <NA>    NA
#> 77    0.000000        0             NA       <NA>               <NA>    NA
#> 78   14.450000       NA              1    Default     Quality on net FALSE
#> 79   28.616667        0             NA       <NA>               <NA>    NA
#> 80   -1.416667       NA              1    Default     Quality on net FALSE
#> 81   -5.100000       NA              1    Default     Quality on net FALSE
#> 82   -6.800000       NA              3    Default     Quality on net FALSE
#> 83  -15.866667        1             NA       <NA>               <NA>    NA
#> 84   24.650000       NA              3    Default Non quality on net FALSE
#> 85   24.933333       NA              3    Default Non quality on net FALSE
#> 86   -4.250000       NA              1    Default Non quality on net FALSE
#> 87    1.983333       NA              3    Default Non quality on net FALSE
#> 88   28.616667        1             NA       <NA>               <NA>    NA
#> 89   28.616667        1             NA       <NA>               <NA>    NA
#> 90  -12.183333       NA              3    Default     Quality on net FALSE
#> 91   34.850000        0             NA       <NA>               <NA>    NA
#> 92    0.000000        0             NA       <NA>               <NA>    NA
#> 93   12.183333       NA              1    Default Non quality on net FALSE
#> 94   28.616667        0             NA       <NA>               <NA>    NA
#> 95   34.850000        0             NA       <NA>               <NA>    NA
#> 96    6.233333       NA              3    Default     Quality on net FALSE
#> 97  -21.816667        1             NA       <NA>               <NA>    NA
#> 98    7.650000       NA              3    Default     Quality on net FALSE
#> 99  -15.866667        0             NA       <NA>               <NA>    NA
#> 100 -15.866667        1             NA       <NA>               <NA>    NA
#> 101 -15.866667        0             NA       <NA>               <NA>    NA
#> 102 -20.683333       NA              1    Default Non quality on net FALSE
#> 103 -16.150000       NA              3    Default Non quality on net FALSE
#> 104   0.000000        1             NA       <NA>               <NA>    NA
#> 105  -1.416667       NA              3    Default     Quality on net FALSE
#> 106   6.233333       NA              1    Default     Quality on net FALSE
#> 107 -16.716667       NA              1    Default     Quality on net FALSE
#> 108 -15.016667       NA              3    Default     Quality on net FALSE
#> 109  -7.366667       NA              1    Default     Quality on net FALSE
#> 110 -28.616667        1             NA       <NA>               <NA>    NA
#> 111  -4.250000       NA              3    Default       Quality goal  TRUE
#> 112  -4.250000       NA              3       <NA>               <NA>    NA
#>     goalie_id goalie_first goalie_last penalty_length power_play
#> 1          NA         <NA>        <NA>           <NA>         NA
#> 2          NA         <NA>        <NA>           <NA>         NA
#> 3           6        Aerin     Frankel           <NA>         NA
#> 4          NA         <NA>        <NA>           <NA>         NA
#> 5          NA         <NA>        <NA>           <NA>         NA
#> 6          85       Elaine       Chuli           <NA>         NA
#> 7           6        Aerin     Frankel           <NA>         NA
#> 8           6        Aerin     Frankel           <NA>         NA
#> 9           6        Aerin     Frankel           <NA>         NA
#> 10         85       Elaine       Chuli           <NA>         NA
#> 11          6        Aerin     Frankel           <NA>         NA
#> 12         NA         <NA>        <NA>           <NA>         NA
#> 13         NA         <NA>        <NA>           <NA>         NA
#> 14         NA         <NA>        <NA>           2.00          1
#> 15         NA         <NA>        <NA>           <NA>         NA
#> 16          6        Aerin     Frankel           <NA>          1
#> 17         NA         <NA>        <NA>           <NA>         NA
#> 18         85       Elaine       Chuli           <NA>         NA
#> 19         NA         <NA>        <NA>           <NA>         NA
#> 20         85       Elaine       Chuli           <NA>         NA
#> 21         85       Elaine       Chuli           <NA>         NA
#> 22         NA         <NA>        <NA>           <NA>         NA
#> 23         85       Elaine       Chuli           <NA>         NA
#> 24          6        Aerin     Frankel           <NA>         NA
#> 25         NA         <NA>        <NA>           <NA>          0
#> 26          6        Aerin     Frankel           <NA>         NA
#> 27         85       Elaine       Chuli           <NA>         NA
#> 28         NA         <NA>        <NA>           <NA>         NA
#> 29         NA         <NA>        <NA>           <NA>         NA
#> 30          6        Aerin     Frankel           <NA>         NA
#> 31         NA         <NA>        <NA>           <NA>         NA
#> 32          6        Aerin     Frankel           <NA>         NA
#> 33         85       Elaine       Chuli           <NA>         NA
#> 34         NA         <NA>        <NA>           <NA>         NA
#> 35         85       Elaine       Chuli           <NA>         NA
#> 36         85       Elaine       Chuli           <NA>         NA
#> 37         85       Elaine       Chuli           <NA>         NA
#> 38         NA         <NA>        <NA>           <NA>         NA
#> 39         NA         <NA>        <NA>           2.00          1
#> 40         NA         <NA>        <NA>           <NA>         NA
#> 41         NA         <NA>        <NA>           <NA>         NA
#> 42         85       Elaine       Chuli           <NA>          1
#> 43         85       Elaine       Chuli           <NA>          1
#> 44         85       Elaine       Chuli           <NA>         NA
#> 45          6        Aerin     Frankel           <NA>         NA
#> 46         NA         <NA>        <NA>           <NA>         NA
#> 47          6        Aerin     Frankel           <NA>         NA
#> 48          6        Aerin     Frankel           <NA>         NA
#> 49         NA         <NA>        <NA>           <NA>         NA
#> 50          6        Aerin     Frankel           <NA>         NA
#> 51          6        Aerin     Frankel           <NA>         NA
#> 52         NA         <NA>        <NA>           2.00          1
#> 53         NA         <NA>        <NA>           <NA>         NA
#> 54         NA         <NA>        <NA>           <NA>         NA
#> 55         NA         <NA>        <NA>           2.00          1
#> 56         NA         <NA>        <NA>           <NA>         NA
#> 57         85       Elaine       Chuli           <NA>          1
#> 58         NA         <NA>        <NA>           <NA>         NA
#> 59         85       Elaine       Chuli           <NA>         NA
#> 60          6        Aerin     Frankel           <NA>         NA
#> 61         NA         <NA>        <NA>           <NA>         NA
#> 62         NA         <NA>        <NA>           <NA>         NA
#> 63         NA         <NA>        <NA>           <NA>         NA
#> 64         NA         <NA>        <NA>           <NA>         NA
#> 65         NA         <NA>        <NA>           <NA>         NA
#> 66         85       Elaine       Chuli           <NA>         NA
#> 67          6        Aerin     Frankel           <NA>         NA
#> 68         NA         <NA>        <NA>           <NA>         NA
#> 69          6        Aerin     Frankel           <NA>         NA
#> 70         NA         <NA>        <NA>           <NA>         NA
#> 71         NA         <NA>        <NA>           <NA>         NA
#> 72          6        Aerin     Frankel           <NA>         NA
#> 73         NA         <NA>        <NA>           <NA>         NA
#> 74         85       Elaine       Chuli           <NA>         NA
#> 75         NA         <NA>        <NA>           <NA>          0
#> 76         NA         <NA>        <NA>           <NA>         NA
#> 77         NA         <NA>        <NA>           <NA>         NA
#> 78         85       Elaine       Chuli           <NA>         NA
#> 79         NA         <NA>        <NA>           <NA>         NA
#> 80         85       Elaine       Chuli           <NA>         NA
#> 81         85       Elaine       Chuli           <NA>         NA
#> 82          6        Aerin     Frankel           <NA>         NA
#> 83         NA         <NA>        <NA>           <NA>         NA
#> 84          6        Aerin     Frankel           <NA>         NA
#> 85          6        Aerin     Frankel           <NA>         NA
#> 86         85       Elaine       Chuli           <NA>         NA
#> 87          6        Aerin     Frankel           <NA>         NA
#> 88         NA         <NA>        <NA>           <NA>         NA
#> 89         NA         <NA>        <NA>           <NA>         NA
#> 90          6        Aerin     Frankel           <NA>         NA
#> 91         NA         <NA>        <NA>           <NA>         NA
#> 92         NA         <NA>        <NA>           <NA>         NA
#> 93         85       Elaine       Chuli           <NA>         NA
#> 94         NA         <NA>        <NA>           <NA>         NA
#> 95         NA         <NA>        <NA>           <NA>         NA
#> 96          6        Aerin     Frankel           <NA>         NA
#> 97         NA         <NA>        <NA>           <NA>         NA
#> 98          6        Aerin     Frankel           <NA>         NA
#> 99         NA         <NA>        <NA>           <NA>         NA
#> 100        NA         <NA>        <NA>           <NA>         NA
#> 101        NA         <NA>        <NA>           <NA>         NA
#> 102        85       Elaine       Chuli           <NA>         NA
#> 103         6        Aerin     Frankel           <NA>         NA
#> 104        NA         <NA>        <NA>           <NA>         NA
#> 105         6        Aerin     Frankel           <NA>         NA
#> 106        85       Elaine       Chuli           <NA>         NA
#> 107        85       Elaine       Chuli           <NA>         NA
#> 108         6        Aerin     Frankel           <NA>         NA
#> 109        85       Elaine       Chuli           <NA>         NA
#> 110        NA         <NA>        <NA>           <NA>         NA
#> 111         6        Aerin     Frankel           <NA>         NA
#> 112        NA         <NA>        <NA>           <NA>          0
#>     player_three_id player_three_name_first player_three_name_last
#> 1                NA                    <NA>                   <NA>
#> 2                NA                    <NA>                   <NA>
#> 3                NA                    <NA>                   <NA>
#> 4                NA                    <NA>                   <NA>
#> 5                NA                    <NA>                   <NA>
#> 6                NA                    <NA>                   <NA>
#> 7                NA                    <NA>                   <NA>
#> 8                NA                    <NA>                   <NA>
#> 9                NA                    <NA>                   <NA>
#> 10               NA                    <NA>                   <NA>
#> 11               NA                    <NA>                   <NA>
#> 12               NA                    <NA>                   <NA>
#> 13               NA                    <NA>                   <NA>
#> 14               NA                    <NA>                   <NA>
#> 15               NA                    <NA>                   <NA>
#> 16               NA                    <NA>                   <NA>
#> 17               NA                    <NA>                   <NA>
#> 18               NA                    <NA>                   <NA>
#> 19               NA                    <NA>                   <NA>
#> 20               NA                    <NA>                   <NA>
#> 21               NA                    <NA>                   <NA>
#> 22               NA                    <NA>                   <NA>
#> 23               NA                    <NA>                   <NA>
#> 24               NA                    <NA>                   <NA>
#> 25               98                 Maureen                 Murphy
#> 26               NA                    <NA>                   <NA>
#> 27               NA                    <NA>                   <NA>
#> 28               NA                    <NA>                   <NA>
#> 29               NA                    <NA>                   <NA>
#> 30               NA                    <NA>                   <NA>
#> 31               NA                    <NA>                   <NA>
#> 32               NA                    <NA>                   <NA>
#> 33               NA                    <NA>                   <NA>
#> 34               NA                    <NA>                   <NA>
#> 35               NA                    <NA>                   <NA>
#> 36               NA                    <NA>                   <NA>
#> 37               NA                    <NA>                   <NA>
#> 38               NA                    <NA>                   <NA>
#> 39               NA                    <NA>                   <NA>
#> 40               NA                    <NA>                   <NA>
#> 41               NA                    <NA>                   <NA>
#> 42               NA                    <NA>                   <NA>
#> 43               NA                    <NA>                   <NA>
#> 44               NA                    <NA>                   <NA>
#> 45               NA                    <NA>                   <NA>
#> 46               NA                    <NA>                   <NA>
#> 47               NA                    <NA>                   <NA>
#> 48               NA                    <NA>                   <NA>
#> 49               NA                    <NA>                   <NA>
#> 50               NA                    <NA>                   <NA>
#> 51               NA                    <NA>                   <NA>
#> 52               NA                    <NA>                   <NA>
#> 53               NA                    <NA>                   <NA>
#> 54               NA                    <NA>                   <NA>
#> 55               NA                    <NA>                   <NA>
#> 56               NA                    <NA>                   <NA>
#> 57               NA                    <NA>                   <NA>
#> 58               NA                    <NA>                   <NA>
#> 59               NA                    <NA>                   <NA>
#> 60               NA                    <NA>                   <NA>
#> 61               NA                    <NA>                   <NA>
#> 62               NA                    <NA>                   <NA>
#> 63               NA                    <NA>                   <NA>
#> 64               NA                    <NA>                   <NA>
#> 65               NA                    <NA>                   <NA>
#> 66               NA                    <NA>                   <NA>
#> 67               NA                    <NA>                   <NA>
#> 68               NA                    <NA>                   <NA>
#> 69               NA                    <NA>                   <NA>
#> 70               NA                    <NA>                   <NA>
#> 71               NA                    <NA>                   <NA>
#> 72               NA                    <NA>                   <NA>
#> 73               NA                    <NA>                   <NA>
#> 74               NA                    <NA>                   <NA>
#> 75               NA                    <NA>                   <NA>
#> 76               NA                    <NA>                   <NA>
#> 77               NA                    <NA>                   <NA>
#> 78               NA                    <NA>                   <NA>
#> 79               NA                    <NA>                   <NA>
#> 80               NA                    <NA>                   <NA>
#> 81               NA                    <NA>                   <NA>
#> 82               NA                    <NA>                   <NA>
#> 83               NA                    <NA>                   <NA>
#> 84               NA                    <NA>                   <NA>
#> 85               NA                    <NA>                   <NA>
#> 86               NA                    <NA>                   <NA>
#> 87               NA                    <NA>                   <NA>
#> 88               NA                    <NA>                   <NA>
#> 89               NA                    <NA>                   <NA>
#> 90               NA                    <NA>                   <NA>
#> 91               NA                    <NA>                   <NA>
#> 92               NA                    <NA>                   <NA>
#> 93               NA                    <NA>                   <NA>
#> 94               NA                    <NA>                   <NA>
#> 95               NA                    <NA>                   <NA>
#> 96               NA                    <NA>                   <NA>
#> 97               NA                    <NA>                   <NA>
#> 98               NA                    <NA>                   <NA>
#> 99               NA                    <NA>                   <NA>
#> 100              NA                    <NA>                   <NA>
#> 101              NA                    <NA>                   <NA>
#> 102              NA                    <NA>                   <NA>
#> 103              NA                    <NA>                   <NA>
#> 104              NA                    <NA>                   <NA>
#> 105              NA                    <NA>                   <NA>
#> 106              NA                    <NA>                   <NA>
#> 107              NA                    <NA>                   <NA>
#> 108              NA                    <NA>                   <NA>
#> 109              NA                    <NA>                   <NA>
#> 110              NA                    <NA>                   <NA>
#> 111              NA                    <NA>                   <NA>
#> 112              31            Marie-Philip                 Poulin
#>     player_three_position empty_net game_winner penalty_shot insurance
#> 1                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 2                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 3                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 4                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 5                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 6                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 7                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 8                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 9                    <NA>      <NA>        <NA>         <NA>      <NA>
#> 10                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 11                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 12                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 13                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 14                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 15                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 16                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 17                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 18                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 19                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 20                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 21                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 22                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 23                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 24                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 25                     RW         0           0            0         0
#> 26                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 27                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 28                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 29                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 30                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 31                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 32                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 33                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 34                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 35                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 36                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 37                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 38                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 39                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 40                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 41                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 42                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 43                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 44                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 45                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 46                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 47                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 48                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 49                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 50                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 51                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 52                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 53                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 54                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 55                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 56                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 57                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 58                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 59                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 60                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 61                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 62                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 63                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 64                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 65                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 66                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 67                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 68                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 69                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 70                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 71                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 72                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 73                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 74                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 75                   <NA>         0           0            0         0
#> 76                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 77                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 78                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 79                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 80                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 81                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 82                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 83                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 84                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 85                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 86                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 87                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 88                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 89                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 90                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 91                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 92                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 93                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 94                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 95                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 96                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 97                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 98                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 99                   <NA>      <NA>        <NA>         <NA>      <NA>
#> 100                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 101                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 102                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 103                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 104                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 105                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 106                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 107                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 108                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 109                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 110                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 111                  <NA>      <NA>        <NA>         <NA>      <NA>
#> 112                     C         0           1            0         0
#>     short_handed plus_player_one_id plus_player_one_first plus_player_one_last
#> 1           <NA>                 NA                  <NA>                 <NA>
#> 2           <NA>                 NA                  <NA>                 <NA>
#> 3           <NA>                 NA                  <NA>                 <NA>
#> 4           <NA>                 NA                  <NA>                 <NA>
#> 5           <NA>                 NA                  <NA>                 <NA>
#> 6           <NA>                 NA                  <NA>                 <NA>
#> 7           <NA>                 NA                  <NA>                 <NA>
#> 8           <NA>                 NA                  <NA>                 <NA>
#> 9           <NA>                 NA                  <NA>                 <NA>
#> 10          <NA>                 NA                  <NA>                 <NA>
#> 11          <NA>                 NA                  <NA>                 <NA>
#> 12          <NA>                 NA                  <NA>                 <NA>
#> 13          <NA>                 NA                  <NA>                 <NA>
#> 14          <NA>                 NA                  <NA>                 <NA>
#> 15          <NA>                 NA                  <NA>                 <NA>
#> 16             0                 NA                  <NA>                 <NA>
#> 17          <NA>                 NA                  <NA>                 <NA>
#> 18          <NA>                 NA                  <NA>                 <NA>
#> 19          <NA>                 NA                  <NA>                 <NA>
#> 20          <NA>                 NA                  <NA>                 <NA>
#> 21          <NA>                 NA                  <NA>                 <NA>
#> 22          <NA>                 NA                  <NA>                 <NA>
#> 23          <NA>                 NA                  <NA>                 <NA>
#> 24          <NA>                 NA                  <NA>                 <NA>
#> 25             0                 99                Mariah              Keopple
#> 26          <NA>                 NA                  <NA>                 <NA>
#> 27          <NA>                 NA                  <NA>                 <NA>
#> 28          <NA>                 NA                  <NA>                 <NA>
#> 29          <NA>                 NA                  <NA>                 <NA>
#> 30          <NA>                 NA                  <NA>                 <NA>
#> 31          <NA>                 NA                  <NA>                 <NA>
#> 32          <NA>                 NA                  <NA>                 <NA>
#> 33          <NA>                 NA                  <NA>                 <NA>
#> 34          <NA>                 NA                  <NA>                 <NA>
#> 35          <NA>                 NA                  <NA>                 <NA>
#> 36          <NA>                 NA                  <NA>                 <NA>
#> 37          <NA>                 NA                  <NA>                 <NA>
#> 38          <NA>                 NA                  <NA>                 <NA>
#> 39          <NA>                 NA                  <NA>                 <NA>
#> 40          <NA>                 NA                  <NA>                 <NA>
#> 41          <NA>                 NA                  <NA>                 <NA>
#> 42             0                 NA                  <NA>                 <NA>
#> 43             0                 NA                  <NA>                 <NA>
#> 44          <NA>                 NA                  <NA>                 <NA>
#> 45          <NA>                 NA                  <NA>                 <NA>
#> 46          <NA>                 NA                  <NA>                 <NA>
#> 47          <NA>                 NA                  <NA>                 <NA>
#> 48          <NA>                 NA                  <NA>                 <NA>
#> 49          <NA>                 NA                  <NA>                 <NA>
#> 50          <NA>                 NA                  <NA>                 <NA>
#> 51          <NA>                 NA                  <NA>                 <NA>
#> 52          <NA>                 NA                  <NA>                 <NA>
#> 53          <NA>                 NA                  <NA>                 <NA>
#> 54          <NA>                 NA                  <NA>                 <NA>
#> 55          <NA>                 NA                  <NA>                 <NA>
#> 56          <NA>                 NA                  <NA>                 <NA>
#> 57             0                 NA                  <NA>                 <NA>
#> 58          <NA>                 NA                  <NA>                 <NA>
#> 59          <NA>                 NA                  <NA>                 <NA>
#> 60          <NA>                 NA                  <NA>                 <NA>
#> 61          <NA>                 NA                  <NA>                 <NA>
#> 62          <NA>                 NA                  <NA>                 <NA>
#> 63          <NA>                 NA                  <NA>                 <NA>
#> 64          <NA>                 NA                  <NA>                 <NA>
#> 65          <NA>                 NA                  <NA>                 <NA>
#> 66          <NA>                 NA                  <NA>                 <NA>
#> 67          <NA>                 NA                  <NA>                 <NA>
#> 68          <NA>                 NA                  <NA>                 <NA>
#> 69          <NA>                 NA                  <NA>                 <NA>
#> 70          <NA>                 NA                  <NA>                 <NA>
#> 71          <NA>                 NA                  <NA>                 <NA>
#> 72          <NA>                 NA                  <NA>                 <NA>
#> 73          <NA>                 NA                  <NA>                 <NA>
#> 74          <NA>                 NA                  <NA>                 <NA>
#> 75             0                167                Sidney                Morin
#> 76          <NA>                 NA                  <NA>                 <NA>
#> 77          <NA>                 NA                  <NA>                 <NA>
#> 78          <NA>                 NA                  <NA>                 <NA>
#> 79          <NA>                 NA                  <NA>                 <NA>
#> 80          <NA>                 NA                  <NA>                 <NA>
#> 81          <NA>                 NA                  <NA>                 <NA>
#> 82          <NA>                 NA                  <NA>                 <NA>
#> 83          <NA>                 NA                  <NA>                 <NA>
#> 84          <NA>                 NA                  <NA>                 <NA>
#> 85          <NA>                 NA                  <NA>                 <NA>
#> 86          <NA>                 NA                  <NA>                 <NA>
#> 87          <NA>                 NA                  <NA>                 <NA>
#> 88          <NA>                 NA                  <NA>                 <NA>
#> 89          <NA>                 NA                  <NA>                 <NA>
#> 90          <NA>                 NA                  <NA>                 <NA>
#> 91          <NA>                 NA                  <NA>                 <NA>
#> 92          <NA>                 NA                  <NA>                 <NA>
#> 93          <NA>                 NA                  <NA>                 <NA>
#> 94          <NA>                 NA                  <NA>                 <NA>
#> 95          <NA>                 NA                  <NA>                 <NA>
#> 96          <NA>                 NA                  <NA>                 <NA>
#> 97          <NA>                 NA                  <NA>                 <NA>
#> 98          <NA>                 NA                  <NA>                 <NA>
#> 99          <NA>                 NA                  <NA>                 <NA>
#> 100         <NA>                 NA                  <NA>                 <NA>
#> 101         <NA>                 NA                  <NA>                 <NA>
#> 102         <NA>                 NA                  <NA>                 <NA>
#> 103         <NA>                 NA                  <NA>                 <NA>
#> 104         <NA>                 NA                  <NA>                 <NA>
#> 105         <NA>                 NA                  <NA>                 <NA>
#> 106         <NA>                 NA                  <NA>                 <NA>
#> 107         <NA>                 NA                  <NA>                 <NA>
#> 108         <NA>                 NA                  <NA>                 <NA>
#> 109         <NA>                 NA                  <NA>                 <NA>
#> 110         <NA>                 NA                  <NA>                 <NA>
#> 111         <NA>                 NA                  <NA>                 <NA>
#> 112            0                 32                 Laura               Stacey
#>     plus_player_one_position plus_player_two_id plus_player_two_first
#> 1                       <NA>                 NA                  <NA>
#> 2                       <NA>                 NA                  <NA>
#> 3                       <NA>                 NA                  <NA>
#> 4                       <NA>                 NA                  <NA>
#> 5                       <NA>                 NA                  <NA>
#> 6                       <NA>                 NA                  <NA>
#> 7                       <NA>                 NA                  <NA>
#> 8                       <NA>                 NA                  <NA>
#> 9                       <NA>                 NA                  <NA>
#> 10                      <NA>                 NA                  <NA>
#> 11                      <NA>                 NA                  <NA>
#> 12                      <NA>                 NA                  <NA>
#> 13                      <NA>                 NA                  <NA>
#> 14                      <NA>                 NA                  <NA>
#> 15                      <NA>                 NA                  <NA>
#> 16                      <NA>                 NA                  <NA>
#> 17                      <NA>                 NA                  <NA>
#> 18                      <NA>                 NA                  <NA>
#> 19                      <NA>                 NA                  <NA>
#> 20                      <NA>                 NA                  <NA>
#> 21                      <NA>                 NA                  <NA>
#> 22                      <NA>                 NA                  <NA>
#> 23                      <NA>                 NA                  <NA>
#> 24                      <NA>                 NA                  <NA>
#> 25                        LD                 98               Maureen
#> 26                      <NA>                 NA                  <NA>
#> 27                      <NA>                 NA                  <NA>
#> 28                      <NA>                 NA                  <NA>
#> 29                      <NA>                 NA                  <NA>
#> 30                      <NA>                 NA                  <NA>
#> 31                      <NA>                 NA                  <NA>
#> 32                      <NA>                 NA                  <NA>
#> 33                      <NA>                 NA                  <NA>
#> 34                      <NA>                 NA                  <NA>
#> 35                      <NA>                 NA                  <NA>
#> 36                      <NA>                 NA                  <NA>
#> 37                      <NA>                 NA                  <NA>
#> 38                      <NA>                 NA                  <NA>
#> 39                      <NA>                 NA                  <NA>
#> 40                      <NA>                 NA                  <NA>
#> 41                      <NA>                 NA                  <NA>
#> 42                      <NA>                 NA                  <NA>
#> 43                      <NA>                 NA                  <NA>
#> 44                      <NA>                 NA                  <NA>
#> 45                      <NA>                 NA                  <NA>
#> 46                      <NA>                 NA                  <NA>
#> 47                      <NA>                 NA                  <NA>
#> 48                      <NA>                 NA                  <NA>
#> 49                      <NA>                 NA                  <NA>
#> 50                      <NA>                 NA                  <NA>
#> 51                      <NA>                 NA                  <NA>
#> 52                      <NA>                 NA                  <NA>
#> 53                      <NA>                 NA                  <NA>
#> 54                      <NA>                 NA                  <NA>
#> 55                      <NA>                 NA                  <NA>
#> 56                      <NA>                 NA                  <NA>
#> 57                      <NA>                 NA                  <NA>
#> 58                      <NA>                 NA                  <NA>
#> 59                      <NA>                 NA                  <NA>
#> 60                      <NA>                 NA                  <NA>
#> 61                      <NA>                 NA                  <NA>
#> 62                      <NA>                 NA                  <NA>
#> 63                      <NA>                 NA                  <NA>
#> 64                      <NA>                 NA                  <NA>
#> 65                      <NA>                 NA                  <NA>
#> 66                      <NA>                 NA                  <NA>
#> 67                      <NA>                 NA                  <NA>
#> 68                      <NA>                 NA                  <NA>
#> 69                      <NA>                 NA                  <NA>
#> 70                      <NA>                 NA                  <NA>
#> 71                      <NA>                 NA                  <NA>
#> 72                      <NA>                 NA                  <NA>
#> 73                      <NA>                 NA                  <NA>
#> 74                      <NA>                 NA                  <NA>
#> 75                        RD                 18                Sophie
#> 76                      <NA>                 NA                  <NA>
#> 77                      <NA>                 NA                  <NA>
#> 78                      <NA>                 NA                  <NA>
#> 79                      <NA>                 NA                  <NA>
#> 80                      <NA>                 NA                  <NA>
#> 81                      <NA>                 NA                  <NA>
#> 82                      <NA>                 NA                  <NA>
#> 83                      <NA>                 NA                  <NA>
#> 84                      <NA>                 NA                  <NA>
#> 85                      <NA>                 NA                  <NA>
#> 86                      <NA>                 NA                  <NA>
#> 87                      <NA>                 NA                  <NA>
#> 88                      <NA>                 NA                  <NA>
#> 89                      <NA>                 NA                  <NA>
#> 90                      <NA>                 NA                  <NA>
#> 91                      <NA>                 NA                  <NA>
#> 92                      <NA>                 NA                  <NA>
#> 93                      <NA>                 NA                  <NA>
#> 94                      <NA>                 NA                  <NA>
#> 95                      <NA>                 NA                  <NA>
#> 96                      <NA>                 NA                  <NA>
#> 97                      <NA>                 NA                  <NA>
#> 98                      <NA>                 NA                  <NA>
#> 99                      <NA>                 NA                  <NA>
#> 100                     <NA>                 NA                  <NA>
#> 101                     <NA>                 NA                  <NA>
#> 102                     <NA>                 NA                  <NA>
#> 103                     <NA>                 NA                  <NA>
#> 104                     <NA>                 NA                  <NA>
#> 105                     <NA>                 NA                  <NA>
#> 106                     <NA>                 NA                  <NA>
#> 107                     <NA>                 NA                  <NA>
#> 108                     <NA>                 NA                  <NA>
#> 109                     <NA>                 NA                  <NA>
#> 110                     <NA>                 NA                  <NA>
#> 111                     <NA>                 NA                  <NA>
#> 112                       RW                 98               Maureen
#>     plus_player_two_last plus_player_two_position plus_player_three_id
#> 1                   <NA>                     <NA>                   NA
#> 2                   <NA>                     <NA>                   NA
#> 3                   <NA>                     <NA>                   NA
#> 4                   <NA>                     <NA>                   NA
#> 5                   <NA>                     <NA>                   NA
#> 6                   <NA>                     <NA>                   NA
#> 7                   <NA>                     <NA>                   NA
#> 8                   <NA>                     <NA>                   NA
#> 9                   <NA>                     <NA>                   NA
#> 10                  <NA>                     <NA>                   NA
#> 11                  <NA>                     <NA>                   NA
#> 12                  <NA>                     <NA>                   NA
#> 13                  <NA>                     <NA>                   NA
#> 14                  <NA>                     <NA>                   NA
#> 15                  <NA>                     <NA>                   NA
#> 16                  <NA>                     <NA>                   NA
#> 17                  <NA>                     <NA>                   NA
#> 18                  <NA>                     <NA>                   NA
#> 19                  <NA>                     <NA>                   NA
#> 20                  <NA>                     <NA>                   NA
#> 21                  <NA>                     <NA>                   NA
#> 22                  <NA>                     <NA>                   NA
#> 23                  <NA>                     <NA>                   NA
#> 24                  <NA>                     <NA>                   NA
#> 25                Murphy                       RW                  161
#> 26                  <NA>                     <NA>                   NA
#> 27                  <NA>                     <NA>                   NA
#> 28                  <NA>                     <NA>                   NA
#> 29                  <NA>                     <NA>                   NA
#> 30                  <NA>                     <NA>                   NA
#> 31                  <NA>                     <NA>                   NA
#> 32                  <NA>                     <NA>                   NA
#> 33                  <NA>                     <NA>                   NA
#> 34                  <NA>                     <NA>                   NA
#> 35                  <NA>                     <NA>                   NA
#> 36                  <NA>                     <NA>                   NA
#> 37                  <NA>                     <NA>                   NA
#> 38                  <NA>                     <NA>                   NA
#> 39                  <NA>                     <NA>                   NA
#> 40                  <NA>                     <NA>                   NA
#> 41                  <NA>                     <NA>                   NA
#> 42                  <NA>                     <NA>                   NA
#> 43                  <NA>                     <NA>                   NA
#> 44                  <NA>                     <NA>                   NA
#> 45                  <NA>                     <NA>                   NA
#> 46                  <NA>                     <NA>                   NA
#> 47                  <NA>                     <NA>                   NA
#> 48                  <NA>                     <NA>                   NA
#> 49                  <NA>                     <NA>                   NA
#> 50                  <NA>                     <NA>                   NA
#> 51                  <NA>                     <NA>                   NA
#> 52                  <NA>                     <NA>                   NA
#> 53                  <NA>                     <NA>                   NA
#> 54                  <NA>                     <NA>                   NA
#> 55                  <NA>                     <NA>                   NA
#> 56                  <NA>                     <NA>                   NA
#> 57                  <NA>                     <NA>                   NA
#> 58                  <NA>                     <NA>                   NA
#> 59                  <NA>                     <NA>                   NA
#> 60                  <NA>                     <NA>                   NA
#> 61                  <NA>                     <NA>                   NA
#> 62                  <NA>                     <NA>                   NA
#> 63                  <NA>                     <NA>                   NA
#> 64                  <NA>                     <NA>                   NA
#> 65                  <NA>                     <NA>                   NA
#> 66                  <NA>                     <NA>                   NA
#> 67                  <NA>                     <NA>                   NA
#> 68                  <NA>                     <NA>                   NA
#> 69                  <NA>                     <NA>                   NA
#> 70                  <NA>                     <NA>                   NA
#> 71                  <NA>                     <NA>                   NA
#> 72                  <NA>                     <NA>                   NA
#> 73                  <NA>                     <NA>                   NA
#> 74                  <NA>                     <NA>                   NA
#> 75               Shirley                       RW                   11
#> 76                  <NA>                     <NA>                   NA
#> 77                  <NA>                     <NA>                   NA
#> 78                  <NA>                     <NA>                   NA
#> 79                  <NA>                     <NA>                   NA
#> 80                  <NA>                     <NA>                   NA
#> 81                  <NA>                     <NA>                   NA
#> 82                  <NA>                     <NA>                   NA
#> 83                  <NA>                     <NA>                   NA
#> 84                  <NA>                     <NA>                   NA
#> 85                  <NA>                     <NA>                   NA
#> 86                  <NA>                     <NA>                   NA
#> 87                  <NA>                     <NA>                   NA
#> 88                  <NA>                     <NA>                   NA
#> 89                  <NA>                     <NA>                   NA
#> 90                  <NA>                     <NA>                   NA
#> 91                  <NA>                     <NA>                   NA
#> 92                  <NA>                     <NA>                   NA
#> 93                  <NA>                     <NA>                   NA
#> 94                  <NA>                     <NA>                   NA
#> 95                  <NA>                     <NA>                   NA
#> 96                  <NA>                     <NA>                   NA
#> 97                  <NA>                     <NA>                   NA
#> 98                  <NA>                     <NA>                   NA
#> 99                  <NA>                     <NA>                   NA
#> 100                 <NA>                     <NA>                   NA
#> 101                 <NA>                     <NA>                   NA
#> 102                 <NA>                     <NA>                   NA
#> 103                 <NA>                     <NA>                   NA
#> 104                 <NA>                     <NA>                   NA
#> 105                 <NA>                     <NA>                   NA
#> 106                 <NA>                     <NA>                   NA
#> 107                 <NA>                     <NA>                   NA
#> 108                 <NA>                     <NA>                   NA
#> 109                 <NA>                     <NA>                   NA
#> 110                 <NA>                     <NA>                   NA
#> 111                 <NA>                     <NA>                   NA
#> 112               Murphy                       RW                   83
#>     plus_player_three_first plus_player_three_last plus_player_three_position
#> 1                      <NA>                   <NA>                       <NA>
#> 2                      <NA>                   <NA>                       <NA>
#> 3                      <NA>                   <NA>                       <NA>
#> 4                      <NA>                   <NA>                       <NA>
#> 5                      <NA>                   <NA>                       <NA>
#> 6                      <NA>                   <NA>                       <NA>
#> 7                      <NA>                   <NA>                       <NA>
#> 8                      <NA>                   <NA>                       <NA>
#> 9                      <NA>                   <NA>                       <NA>
#> 10                     <NA>                   <NA>                       <NA>
#> 11                     <NA>                   <NA>                       <NA>
#> 12                     <NA>                   <NA>                       <NA>
#> 13                     <NA>                   <NA>                       <NA>
#> 14                     <NA>                   <NA>                       <NA>
#> 15                     <NA>                   <NA>                       <NA>
#> 16                     <NA>                   <NA>                       <NA>
#> 17                     <NA>                   <NA>                       <NA>
#> 18                     <NA>                   <NA>                       <NA>
#> 19                     <NA>                   <NA>                       <NA>
#> 20                     <NA>                   <NA>                       <NA>
#> 21                     <NA>                   <NA>                       <NA>
#> 22                     <NA>                   <NA>                       <NA>
#> 23                     <NA>                   <NA>                       <NA>
#> 24                     <NA>                   <NA>                       <NA>
#> 25                   Tereza               Vanišová                         LW
#> 26                     <NA>                   <NA>                       <NA>
#> 27                     <NA>                   <NA>                       <NA>
#> 28                     <NA>                   <NA>                       <NA>
#> 29                     <NA>                   <NA>                       <NA>
#> 30                     <NA>                   <NA>                       <NA>
#> 31                     <NA>                   <NA>                       <NA>
#> 32                     <NA>                   <NA>                       <NA>
#> 33                     <NA>                   <NA>                       <NA>
#> 34                     <NA>                   <NA>                       <NA>
#> 35                     <NA>                   <NA>                       <NA>
#> 36                     <NA>                   <NA>                       <NA>
#> 37                     <NA>                   <NA>                       <NA>
#> 38                     <NA>                   <NA>                       <NA>
#> 39                     <NA>                   <NA>                       <NA>
#> 40                     <NA>                   <NA>                       <NA>
#> 41                     <NA>                   <NA>                       <NA>
#> 42                     <NA>                   <NA>                       <NA>
#> 43                     <NA>                   <NA>                       <NA>
#> 44                     <NA>                   <NA>                       <NA>
#> 45                     <NA>                   <NA>                       <NA>
#> 46                     <NA>                   <NA>                       <NA>
#> 47                     <NA>                   <NA>                       <NA>
#> 48                     <NA>                   <NA>                       <NA>
#> 49                     <NA>                   <NA>                       <NA>
#> 50                     <NA>                   <NA>                       <NA>
#> 51                     <NA>                   <NA>                       <NA>
#> 52                     <NA>                   <NA>                       <NA>
#> 53                     <NA>                   <NA>                       <NA>
#> 54                     <NA>                   <NA>                       <NA>
#> 55                     <NA>                   <NA>                       <NA>
#> 56                     <NA>                   <NA>                       <NA>
#> 57                     <NA>                   <NA>                       <NA>
#> 58                     <NA>                   <NA>                       <NA>
#> 59                     <NA>                   <NA>                       <NA>
#> 60                     <NA>                   <NA>                       <NA>
#> 61                     <NA>                   <NA>                       <NA>
#> 62                     <NA>                   <NA>                       <NA>
#> 63                     <NA>                   <NA>                       <NA>
#> 64                     <NA>                   <NA>                       <NA>
#> 65                     <NA>                   <NA>                       <NA>
#> 66                     <NA>                   <NA>                       <NA>
#> 67                     <NA>                   <NA>                       <NA>
#> 68                     <NA>                   <NA>                       <NA>
#> 69                     <NA>                   <NA>                       <NA>
#> 70                     <NA>                   <NA>                       <NA>
#> 71                     <NA>                   <NA>                       <NA>
#> 72                     <NA>                   <NA>                       <NA>
#> 73                     <NA>                   <NA>                       <NA>
#> 74                     <NA>                   <NA>                       <NA>
#> 75                   Sophie                 Jaques                         LD
#> 76                     <NA>                   <NA>                       <NA>
#> 77                     <NA>                   <NA>                       <NA>
#> 78                     <NA>                   <NA>                       <NA>
#> 79                     <NA>                   <NA>                       <NA>
#> 80                     <NA>                   <NA>                       <NA>
#> 81                     <NA>                   <NA>                       <NA>
#> 82                     <NA>                   <NA>                       <NA>
#> 83                     <NA>                   <NA>                       <NA>
#> 84                     <NA>                   <NA>                       <NA>
#> 85                     <NA>                   <NA>                       <NA>
#> 86                     <NA>                   <NA>                       <NA>
#> 87                     <NA>                   <NA>                       <NA>
#> 88                     <NA>                   <NA>                       <NA>
#> 89                     <NA>                   <NA>                       <NA>
#> 90                     <NA>                   <NA>                       <NA>
#> 91                     <NA>                   <NA>                       <NA>
#> 92                     <NA>                   <NA>                       <NA>
#> 93                     <NA>                   <NA>                       <NA>
#> 94                     <NA>                   <NA>                       <NA>
#> 95                     <NA>                   <NA>                       <NA>
#> 96                     <NA>                   <NA>                       <NA>
#> 97                     <NA>                   <NA>                       <NA>
#> 98                     <NA>                   <NA>                       <NA>
#> 99                     <NA>                   <NA>                       <NA>
#> 100                    <NA>                   <NA>                       <NA>
#> 101                    <NA>                   <NA>                       <NA>
#> 102                    <NA>                   <NA>                       <NA>
#> 103                    <NA>                   <NA>                       <NA>
#> 104                    <NA>                   <NA>                       <NA>
#> 105                    <NA>                   <NA>                       <NA>
#> 106                    <NA>                   <NA>                       <NA>
#> 107                    <NA>                   <NA>                       <NA>
#> 108                    <NA>                   <NA>                       <NA>
#> 109                    <NA>                   <NA>                       <NA>
#> 110                    <NA>                   <NA>                       <NA>
#> 111                    <NA>                   <NA>                       <NA>
#> 112                    Erin                Ambrose                         RD
#>     plus_player_four_id plus_player_four_first plus_player_four_last
#> 1                    NA                   <NA>                  <NA>
#> 2                    NA                   <NA>                  <NA>
#> 3                    NA                   <NA>                  <NA>
#> 4                    NA                   <NA>                  <NA>
#> 5                    NA                   <NA>                  <NA>
#> 6                    NA                   <NA>                  <NA>
#> 7                    NA                   <NA>                  <NA>
#> 8                    NA                   <NA>                  <NA>
#> 9                    NA                   <NA>                  <NA>
#> 10                   NA                   <NA>                  <NA>
#> 11                   NA                   <NA>                  <NA>
#> 12                   NA                   <NA>                  <NA>
#> 13                   NA                   <NA>                  <NA>
#> 14                   NA                   <NA>                  <NA>
#> 15                   NA                   <NA>                  <NA>
#> 16                   NA                   <NA>                  <NA>
#> 17                   NA                   <NA>                  <NA>
#> 18                   NA                   <NA>                  <NA>
#> 19                   NA                   <NA>                  <NA>
#> 20                   NA                   <NA>                  <NA>
#> 21                   NA                   <NA>                  <NA>
#> 22                   NA                   <NA>                  <NA>
#> 23                   NA                   <NA>                  <NA>
#> 24                   NA                   <NA>                  <NA>
#> 25                   83                   Erin               Ambrose
#> 26                   NA                   <NA>                  <NA>
#> 27                   NA                   <NA>                  <NA>
#> 28                   NA                   <NA>                  <NA>
#> 29                   NA                   <NA>                  <NA>
#> 30                   NA                   <NA>                  <NA>
#> 31                   NA                   <NA>                  <NA>
#> 32                   NA                   <NA>                  <NA>
#> 33                   NA                   <NA>                  <NA>
#> 34                   NA                   <NA>                  <NA>
#> 35                   NA                   <NA>                  <NA>
#> 36                   NA                   <NA>                  <NA>
#> 37                   NA                   <NA>                  <NA>
#> 38                   NA                   <NA>                  <NA>
#> 39                   NA                   <NA>                  <NA>
#> 40                   NA                   <NA>                  <NA>
#> 41                   NA                   <NA>                  <NA>
#> 42                   NA                   <NA>                  <NA>
#> 43                   NA                   <NA>                  <NA>
#> 44                   NA                   <NA>                  <NA>
#> 45                   NA                   <NA>                  <NA>
#> 46                   NA                   <NA>                  <NA>
#> 47                   NA                   <NA>                  <NA>
#> 48                   NA                   <NA>                  <NA>
#> 49                   NA                   <NA>                  <NA>
#> 50                   NA                   <NA>                  <NA>
#> 51                   NA                   <NA>                  <NA>
#> 52                   NA                   <NA>                  <NA>
#> 53                   NA                   <NA>                  <NA>
#> 54                   NA                   <NA>                  <NA>
#> 55                   NA                   <NA>                  <NA>
#> 56                   NA                   <NA>                  <NA>
#> 57                   NA                   <NA>                  <NA>
#> 58                   NA                   <NA>                  <NA>
#> 59                   NA                   <NA>                  <NA>
#> 60                   NA                   <NA>                  <NA>
#> 61                   NA                   <NA>                  <NA>
#> 62                   NA                   <NA>                  <NA>
#> 63                   NA                   <NA>                  <NA>
#> 64                   NA                   <NA>                  <NA>
#> 65                   NA                   <NA>                  <NA>
#> 66                   NA                   <NA>                  <NA>
#> 67                   NA                   <NA>                  <NA>
#> 68                   NA                   <NA>                  <NA>
#> 69                   NA                   <NA>                  <NA>
#> 70                   NA                   <NA>                  <NA>
#> 71                   NA                   <NA>                  <NA>
#> 72                   NA                   <NA>                  <NA>
#> 73                   NA                   <NA>                  <NA>
#> 74                   NA                   <NA>                  <NA>
#> 75                    1                 Hannah                Brandt
#> 76                   NA                   <NA>                  <NA>
#> 77                   NA                   <NA>                  <NA>
#> 78                   NA                   <NA>                  <NA>
#> 79                   NA                   <NA>                  <NA>
#> 80                   NA                   <NA>                  <NA>
#> 81                   NA                   <NA>                  <NA>
#> 82                   NA                   <NA>                  <NA>
#> 83                   NA                   <NA>                  <NA>
#> 84                   NA                   <NA>                  <NA>
#> 85                   NA                   <NA>                  <NA>
#> 86                   NA                   <NA>                  <NA>
#> 87                   NA                   <NA>                  <NA>
#> 88                   NA                   <NA>                  <NA>
#> 89                   NA                   <NA>                  <NA>
#> 90                   NA                   <NA>                  <NA>
#> 91                   NA                   <NA>                  <NA>
#> 92                   NA                   <NA>                  <NA>
#> 93                   NA                   <NA>                  <NA>
#> 94                   NA                   <NA>                  <NA>
#> 95                   NA                   <NA>                  <NA>
#> 96                   NA                   <NA>                  <NA>
#> 97                   NA                   <NA>                  <NA>
#> 98                   NA                   <NA>                  <NA>
#> 99                   NA                   <NA>                  <NA>
#> 100                  NA                   <NA>                  <NA>
#> 101                  NA                   <NA>                  <NA>
#> 102                  NA                   <NA>                  <NA>
#> 103                  NA                   <NA>                  <NA>
#> 104                  NA                   <NA>                  <NA>
#> 105                  NA                   <NA>                  <NA>
#> 106                  NA                   <NA>                  <NA>
#> 107                  NA                   <NA>                  <NA>
#> 108                  NA                   <NA>                  <NA>
#> 109                  NA                   <NA>                  <NA>
#> 110                  NA                   <NA>                  <NA>
#> 111                  NA                   <NA>                  <NA>
#> 112                  NA                   <NA>                  <NA>
#>     plus_player_four_position plus_player_five_id plus_player_five_first
#> 1                        <NA>                  NA                   <NA>
#> 2                        <NA>                  NA                   <NA>
#> 3                        <NA>                  NA                   <NA>
#> 4                        <NA>                  NA                   <NA>
#> 5                        <NA>                  NA                   <NA>
#> 6                        <NA>                  NA                   <NA>
#> 7                        <NA>                  NA                   <NA>
#> 8                        <NA>                  NA                   <NA>
#> 9                        <NA>                  NA                   <NA>
#> 10                       <NA>                  NA                   <NA>
#> 11                       <NA>                  NA                   <NA>
#> 12                       <NA>                  NA                   <NA>
#> 13                       <NA>                  NA                   <NA>
#> 14                       <NA>                  NA                   <NA>
#> 15                       <NA>                  NA                   <NA>
#> 16                       <NA>                  NA                   <NA>
#> 17                       <NA>                  NA                   <NA>
#> 18                       <NA>                  NA                   <NA>
#> 19                       <NA>                  NA                   <NA>
#> 20                       <NA>                  NA                   <NA>
#> 21                       <NA>                  NA                   <NA>
#> 22                       <NA>                  NA                   <NA>
#> 23                       <NA>                  NA                   <NA>
#> 24                       <NA>                  NA                   <NA>
#> 25                    Ambrose                  31           Marie-Philip
#> 26                       <NA>                  NA                   <NA>
#> 27                       <NA>                  NA                   <NA>
#> 28                       <NA>                  NA                   <NA>
#> 29                       <NA>                  NA                   <NA>
#> 30                       <NA>                  NA                   <NA>
#> 31                       <NA>                  NA                   <NA>
#> 32                       <NA>                  NA                   <NA>
#> 33                       <NA>                  NA                   <NA>
#> 34                       <NA>                  NA                   <NA>
#> 35                       <NA>                  NA                   <NA>
#> 36                       <NA>                  NA                   <NA>
#> 37                       <NA>                  NA                   <NA>
#> 38                       <NA>                  NA                   <NA>
#> 39                       <NA>                  NA                   <NA>
#> 40                       <NA>                  NA                   <NA>
#> 41                       <NA>                  NA                   <NA>
#> 42                       <NA>                  NA                   <NA>
#> 43                       <NA>                  NA                   <NA>
#> 44                       <NA>                  NA                   <NA>
#> 45                       <NA>                  NA                   <NA>
#> 46                       <NA>                  NA                   <NA>
#> 47                       <NA>                  NA                   <NA>
#> 48                       <NA>                  NA                   <NA>
#> 49                       <NA>                  NA                   <NA>
#> 50                       <NA>                  NA                   <NA>
#> 51                       <NA>                  NA                   <NA>
#> 52                       <NA>                  NA                   <NA>
#> 53                       <NA>                  NA                   <NA>
#> 54                       <NA>                  NA                   <NA>
#> 55                       <NA>                  NA                   <NA>
#> 56                       <NA>                  NA                   <NA>
#> 57                       <NA>                  NA                   <NA>
#> 58                       <NA>                  NA                   <NA>
#> 59                       <NA>                  NA                   <NA>
#> 60                       <NA>                  NA                   <NA>
#> 61                       <NA>                  NA                   <NA>
#> 62                       <NA>                  NA                   <NA>
#> 63                       <NA>                  NA                   <NA>
#> 64                       <NA>                  NA                   <NA>
#> 65                       <NA>                  NA                   <NA>
#> 66                       <NA>                  NA                   <NA>
#> 67                       <NA>                  NA                   <NA>
#> 68                       <NA>                  NA                   <NA>
#> 69                       <NA>                  NA                   <NA>
#> 70                       <NA>                  NA                   <NA>
#> 71                       <NA>                  NA                   <NA>
#> 72                       <NA>                  NA                   <NA>
#> 73                       <NA>                  NA                   <NA>
#> 74                       <NA>                  NA                   <NA>
#> 75                     Brandt                   8                  Loren
#> 76                       <NA>                  NA                   <NA>
#> 77                       <NA>                  NA                   <NA>
#> 78                       <NA>                  NA                   <NA>
#> 79                       <NA>                  NA                   <NA>
#> 80                       <NA>                  NA                   <NA>
#> 81                       <NA>                  NA                   <NA>
#> 82                       <NA>                  NA                   <NA>
#> 83                       <NA>                  NA                   <NA>
#> 84                       <NA>                  NA                   <NA>
#> 85                       <NA>                  NA                   <NA>
#> 86                       <NA>                  NA                   <NA>
#> 87                       <NA>                  NA                   <NA>
#> 88                       <NA>                  NA                   <NA>
#> 89                       <NA>                  NA                   <NA>
#> 90                       <NA>                  NA                   <NA>
#> 91                       <NA>                  NA                   <NA>
#> 92                       <NA>                  NA                   <NA>
#> 93                       <NA>                  NA                   <NA>
#> 94                       <NA>                  NA                   <NA>
#> 95                       <NA>                  NA                   <NA>
#> 96                       <NA>                  NA                   <NA>
#> 97                       <NA>                  NA                   <NA>
#> 98                       <NA>                  NA                   <NA>
#> 99                       <NA>                  NA                   <NA>
#> 100                      <NA>                  NA                   <NA>
#> 101                      <NA>                  NA                   <NA>
#> 102                      <NA>                  NA                   <NA>
#> 103                      <NA>                  NA                   <NA>
#> 104                      <NA>                  NA                   <NA>
#> 105                      <NA>                  NA                   <NA>
#> 106                      <NA>                  NA                   <NA>
#> 107                      <NA>                  NA                   <NA>
#> 108                      <NA>                  NA                   <NA>
#> 109                      <NA>                  NA                   <NA>
#> 110                      <NA>                  NA                   <NA>
#> 111                      <NA>                  NA                   <NA>
#> 112                      <NA>                  NA                   <NA>
#>     plus_player_five_last plus_player_five_position minus_player_one_id
#> 1                    <NA>                      <NA>                  NA
#> 2                    <NA>                      <NA>                  NA
#> 3                    <NA>                      <NA>                  NA
#> 4                    <NA>                      <NA>                  NA
#> 5                    <NA>                      <NA>                  NA
#> 6                    <NA>                      <NA>                  NA
#> 7                    <NA>                      <NA>                  NA
#> 8                    <NA>                      <NA>                  NA
#> 9                    <NA>                      <NA>                  NA
#> 10                   <NA>                      <NA>                  NA
#> 11                   <NA>                      <NA>                  NA
#> 12                   <NA>                      <NA>                  NA
#> 13                   <NA>                      <NA>                  NA
#> 14                   <NA>                      <NA>                  NA
#> 15                   <NA>                      <NA>                  NA
#> 16                   <NA>                      <NA>                  NA
#> 17                   <NA>                      <NA>                  NA
#> 18                   <NA>                      <NA>                  NA
#> 19                   <NA>                      <NA>                  NA
#> 20                   <NA>                      <NA>                  NA
#> 21                   <NA>                      <NA>                  NA
#> 22                   <NA>                      <NA>                  NA
#> 23                   <NA>                      <NA>                  NA
#> 24                   <NA>                      <NA>                  NA
#> 25                 Poulin                         C                   2
#> 26                   <NA>                      <NA>                  NA
#> 27                   <NA>                      <NA>                  NA
#> 28                   <NA>                      <NA>                  NA
#> 29                   <NA>                      <NA>                  NA
#> 30                   <NA>                      <NA>                  NA
#> 31                   <NA>                      <NA>                  NA
#> 32                   <NA>                      <NA>                  NA
#> 33                   <NA>                      <NA>                  NA
#> 34                   <NA>                      <NA>                  NA
#> 35                   <NA>                      <NA>                  NA
#> 36                   <NA>                      <NA>                  NA
#> 37                   <NA>                      <NA>                  NA
#> 38                   <NA>                      <NA>                  NA
#> 39                   <NA>                      <NA>                  NA
#> 40                   <NA>                      <NA>                  NA
#> 41                   <NA>                      <NA>                  NA
#> 42                   <NA>                      <NA>                  NA
#> 43                   <NA>                      <NA>                  NA
#> 44                   <NA>                      <NA>                  NA
#> 45                   <NA>                      <NA>                  NA
#> 46                   <NA>                      <NA>                  NA
#> 47                   <NA>                      <NA>                  NA
#> 48                   <NA>                      <NA>                  NA
#> 49                   <NA>                      <NA>                  NA
#> 50                   <NA>                      <NA>                  NA
#> 51                   <NA>                      <NA>                  NA
#> 52                   <NA>                      <NA>                  NA
#> 53                   <NA>                      <NA>                  NA
#> 54                   <NA>                      <NA>                  NA
#> 55                   <NA>                      <NA>                  NA
#> 56                   <NA>                      <NA>                  NA
#> 57                   <NA>                      <NA>                  NA
#> 58                   <NA>                      <NA>                  NA
#> 59                   <NA>                      <NA>                  NA
#> 60                   <NA>                      <NA>                  NA
#> 61                   <NA>                      <NA>                  NA
#> 62                   <NA>                      <NA>                  NA
#> 63                   <NA>                      <NA>                  NA
#> 64                   <NA>                      <NA>                  NA
#> 65                   <NA>                      <NA>                  NA
#> 66                   <NA>                      <NA>                  NA
#> 67                   <NA>                      <NA>                  NA
#> 68                   <NA>                      <NA>                  NA
#> 69                   <NA>                      <NA>                  NA
#> 70                   <NA>                      <NA>                  NA
#> 71                   <NA>                      <NA>                  NA
#> 72                   <NA>                      <NA>                  NA
#> 73                   <NA>                      <NA>                  NA
#> 74                   <NA>                      <NA>                  NA
#> 75                  Gabel                        RW                 163
#> 76                   <NA>                      <NA>                  NA
#> 77                   <NA>                      <NA>                  NA
#> 78                   <NA>                      <NA>                  NA
#> 79                   <NA>                      <NA>                  NA
#> 80                   <NA>                      <NA>                  NA
#> 81                   <NA>                      <NA>                  NA
#> 82                   <NA>                      <NA>                  NA
#> 83                   <NA>                      <NA>                  NA
#> 84                   <NA>                      <NA>                  NA
#> 85                   <NA>                      <NA>                  NA
#> 86                   <NA>                      <NA>                  NA
#> 87                   <NA>                      <NA>                  NA
#> 88                   <NA>                      <NA>                  NA
#> 89                   <NA>                      <NA>                  NA
#> 90                   <NA>                      <NA>                  NA
#> 91                   <NA>                      <NA>                  NA
#> 92                   <NA>                      <NA>                  NA
#> 93                   <NA>                      <NA>                  NA
#> 94                   <NA>                      <NA>                  NA
#> 95                   <NA>                      <NA>                  NA
#> 96                   <NA>                      <NA>                  NA
#> 97                   <NA>                      <NA>                  NA
#> 98                   <NA>                      <NA>                  NA
#> 99                   <NA>                      <NA>                  NA
#> 100                  <NA>                      <NA>                  NA
#> 101                  <NA>                      <NA>                  NA
#> 102                  <NA>                      <NA>                  NA
#> 103                  <NA>                      <NA>                  NA
#> 104                  <NA>                      <NA>                  NA
#> 105                  <NA>                      <NA>                  NA
#> 106                  <NA>                      <NA>                  NA
#> 107                  <NA>                      <NA>                  NA
#> 108                  <NA>                      <NA>                  NA
#> 109                  <NA>                      <NA>                  NA
#> 110                  <NA>                      <NA>                  NA
#> 111                  <NA>                      <NA>                  NA
#> 112                  <NA>                      <NA>                  12
#>     minus_player_one_first minus_player_one_last minus_player_one_position
#> 1                     <NA>                  <NA>                      <NA>
#> 2                     <NA>                  <NA>                      <NA>
#> 3                     <NA>                  <NA>                      <NA>
#> 4                     <NA>                  <NA>                      <NA>
#> 5                     <NA>                  <NA>                      <NA>
#> 6                     <NA>                  <NA>                      <NA>
#> 7                     <NA>                  <NA>                      <NA>
#> 8                     <NA>                  <NA>                      <NA>
#> 9                     <NA>                  <NA>                      <NA>
#> 10                    <NA>                  <NA>                      <NA>
#> 11                    <NA>                  <NA>                      <NA>
#> 12                    <NA>                  <NA>                      <NA>
#> 13                    <NA>                  <NA>                      <NA>
#> 14                    <NA>                  <NA>                      <NA>
#> 15                    <NA>                  <NA>                      <NA>
#> 16                    <NA>                  <NA>                      <NA>
#> 17                    <NA>                  <NA>                      <NA>
#> 18                    <NA>                  <NA>                      <NA>
#> 19                    <NA>                  <NA>                      <NA>
#> 20                    <NA>                  <NA>                      <NA>
#> 21                    <NA>                  <NA>                      <NA>
#> 22                    <NA>                  <NA>                      <NA>
#> 23                    <NA>                  <NA>                      <NA>
#> 24                    <NA>                  <NA>                      <NA>
#> 25                   Emily                 Brown                        LD
#> 26                    <NA>                  <NA>                      <NA>
#> 27                    <NA>                  <NA>                      <NA>
#> 28                    <NA>                  <NA>                      <NA>
#> 29                    <NA>                  <NA>                      <NA>
#> 30                    <NA>                  <NA>                      <NA>
#> 31                    <NA>                  <NA>                      <NA>
#> 32                    <NA>                  <NA>                      <NA>
#> 33                    <NA>                  <NA>                      <NA>
#> 34                    <NA>                  <NA>                      <NA>
#> 35                    <NA>                  <NA>                      <NA>
#> 36                    <NA>                  <NA>                      <NA>
#> 37                    <NA>                  <NA>                      <NA>
#> 38                    <NA>                  <NA>                      <NA>
#> 39                    <NA>                  <NA>                      <NA>
#> 40                    <NA>                  <NA>                      <NA>
#> 41                    <NA>                  <NA>                      <NA>
#> 42                    <NA>                  <NA>                      <NA>
#> 43                    <NA>                  <NA>                      <NA>
#> 44                    <NA>                  <NA>                      <NA>
#> 45                    <NA>                  <NA>                      <NA>
#> 46                    <NA>                  <NA>                      <NA>
#> 47                    <NA>                  <NA>                      <NA>
#> 48                    <NA>                  <NA>                      <NA>
#> 49                    <NA>                  <NA>                      <NA>
#> 50                    <NA>                  <NA>                      <NA>
#> 51                    <NA>                  <NA>                      <NA>
#> 52                    <NA>                  <NA>                      <NA>
#> 53                    <NA>                  <NA>                      <NA>
#> 54                    <NA>                  <NA>                      <NA>
#> 55                    <NA>                  <NA>                      <NA>
#> 56                    <NA>                  <NA>                      <NA>
#> 57                    <NA>                  <NA>                      <NA>
#> 58                    <NA>                  <NA>                      <NA>
#> 59                    <NA>                  <NA>                      <NA>
#> 60                    <NA>                  <NA>                      <NA>
#> 61                    <NA>                  <NA>                      <NA>
#> 62                    <NA>                  <NA>                      <NA>
#> 63                    <NA>                  <NA>                      <NA>
#> 64                    <NA>                  <NA>                      <NA>
#> 65                    <NA>                  <NA>                      <NA>
#> 66                    <NA>                  <NA>                      <NA>
#> 67                    <NA>                  <NA>                      <NA>
#> 68                    <NA>                  <NA>                      <NA>
#> 69                    <NA>                  <NA>                      <NA>
#> 70                    <NA>                  <NA>                      <NA>
#> 71                    <NA>                  <NA>                      <NA>
#> 72                    <NA>                  <NA>                      <NA>
#> 73                    <NA>                  <NA>                      <NA>
#> 74                    <NA>                  <NA>                      <NA>
#> 75               Catherine                Daoust                        RD
#> 76                    <NA>                  <NA>                      <NA>
#> 77                    <NA>                  <NA>                      <NA>
#> 78                    <NA>                  <NA>                      <NA>
#> 79                    <NA>                  <NA>                      <NA>
#> 80                    <NA>                  <NA>                      <NA>
#> 81                    <NA>                  <NA>                      <NA>
#> 82                    <NA>                  <NA>                      <NA>
#> 83                    <NA>                  <NA>                      <NA>
#> 84                    <NA>                  <NA>                      <NA>
#> 85                    <NA>                  <NA>                      <NA>
#> 86                    <NA>                  <NA>                      <NA>
#> 87                    <NA>                  <NA>                      <NA>
#> 88                    <NA>                  <NA>                      <NA>
#> 89                    <NA>                  <NA>                      <NA>
#> 90                    <NA>                  <NA>                      <NA>
#> 91                    <NA>                  <NA>                      <NA>
#> 92                    <NA>                  <NA>                      <NA>
#> 93                    <NA>                  <NA>                      <NA>
#> 94                    <NA>                  <NA>                      <NA>
#> 95                    <NA>                  <NA>                      <NA>
#> 96                    <NA>                  <NA>                      <NA>
#> 97                    <NA>                  <NA>                      <NA>
#> 98                    <NA>                  <NA>                      <NA>
#> 99                    <NA>                  <NA>                      <NA>
#> 100                   <NA>                  <NA>                      <NA>
#> 101                   <NA>                  <NA>                      <NA>
#> 102                   <NA>                  <NA>                      <NA>
#> 103                   <NA>                  <NA>                      <NA>
#> 104                   <NA>                  <NA>                      <NA>
#> 105                   <NA>                  <NA>                      <NA>
#> 106                   <NA>                  <NA>                      <NA>
#> 107                   <NA>                  <NA>                      <NA>
#> 108                   <NA>                  <NA>                      <NA>
#> 109                   <NA>                  <NA>                      <NA>
#> 110                   <NA>                  <NA>                      <NA>
#> 111                   <NA>                  <NA>                      <NA>
#> 112                  Megan                Keller                        LD
#>     minus_player_two_id minus_player_two_first minus_player_two_last
#> 1                    NA                   <NA>                  <NA>
#> 2                    NA                   <NA>                  <NA>
#> 3                    NA                   <NA>                  <NA>
#> 4                    NA                   <NA>                  <NA>
#> 5                    NA                   <NA>                  <NA>
#> 6                    NA                   <NA>                  <NA>
#> 7                    NA                   <NA>                  <NA>
#> 8                    NA                   <NA>                  <NA>
#> 9                    NA                   <NA>                  <NA>
#> 10                   NA                   <NA>                  <NA>
#> 11                   NA                   <NA>                  <NA>
#> 12                   NA                   <NA>                  <NA>
#> 13                   NA                   <NA>                  <NA>
#> 14                   NA                   <NA>                  <NA>
#> 15                   NA                   <NA>                  <NA>
#> 16                   NA                   <NA>                  <NA>
#> 17                   NA                   <NA>                  <NA>
#> 18                   NA                   <NA>                  <NA>
#> 19                   NA                   <NA>                  <NA>
#> 20                   NA                   <NA>                  <NA>
#> 21                   NA                   <NA>                  <NA>
#> 22                   NA                   <NA>                  <NA>
#> 23                   NA                   <NA>                  <NA>
#> 24                   NA                   <NA>                  <NA>
#> 25                   15                  Alina                Müller
#> 26                   NA                   <NA>                  <NA>
#> 27                   NA                   <NA>                  <NA>
#> 28                   NA                   <NA>                  <NA>
#> 29                   NA                   <NA>                  <NA>
#> 30                   NA                   <NA>                  <NA>
#> 31                   NA                   <NA>                  <NA>
#> 32                   NA                   <NA>                  <NA>
#> 33                   NA                   <NA>                  <NA>
#> 34                   NA                   <NA>                  <NA>
#> 35                   NA                   <NA>                  <NA>
#> 36                   NA                   <NA>                  <NA>
#> 37                   NA                   <NA>                  <NA>
#> 38                   NA                   <NA>                  <NA>
#> 39                   NA                   <NA>                  <NA>
#> 40                   NA                   <NA>                  <NA>
#> 41                   NA                   <NA>                  <NA>
#> 42                   NA                   <NA>                  <NA>
#> 43                   NA                   <NA>                  <NA>
#> 44                   NA                   <NA>                  <NA>
#> 45                   NA                   <NA>                  <NA>
#> 46                   NA                   <NA>                  <NA>
#> 47                   NA                   <NA>                  <NA>
#> 48                   NA                   <NA>                  <NA>
#> 49                   NA                   <NA>                  <NA>
#> 50                   NA                   <NA>                  <NA>
#> 51                   NA                   <NA>                  <NA>
#> 52                   NA                   <NA>                  <NA>
#> 53                   NA                   <NA>                  <NA>
#> 54                   NA                   <NA>                  <NA>
#> 55                   NA                   <NA>                  <NA>
#> 56                   NA                   <NA>                  <NA>
#> 57                   NA                   <NA>                  <NA>
#> 58                   NA                   <NA>                  <NA>
#> 59                   NA                   <NA>                  <NA>
#> 60                   NA                   <NA>                  <NA>
#> 61                   NA                   <NA>                  <NA>
#> 62                   NA                   <NA>                  <NA>
#> 63                   NA                   <NA>                  <NA>
#> 64                   NA                   <NA>                  <NA>
#> 65                   NA                   <NA>                  <NA>
#> 66                   NA                   <NA>                  <NA>
#> 67                   NA                   <NA>                  <NA>
#> 68                   NA                   <NA>                  <NA>
#> 69                   NA                   <NA>                  <NA>
#> 70                   NA                   <NA>                  <NA>
#> 71                   NA                   <NA>                  <NA>
#> 72                   NA                   <NA>                  <NA>
#> 73                   NA                   <NA>                  <NA>
#> 74                   NA                   <NA>                  <NA>
#> 75                  162                Madison                 Bizal
#> 76                   NA                   <NA>                  <NA>
#> 77                   NA                   <NA>                  <NA>
#> 78                   NA                   <NA>                  <NA>
#> 79                   NA                   <NA>                  <NA>
#> 80                   NA                   <NA>                  <NA>
#> 81                   NA                   <NA>                  <NA>
#> 82                   NA                   <NA>                  <NA>
#> 83                   NA                   <NA>                  <NA>
#> 84                   NA                   <NA>                  <NA>
#> 85                   NA                   <NA>                  <NA>
#> 86                   NA                   <NA>                  <NA>
#> 87                   NA                   <NA>                  <NA>
#> 88                   NA                   <NA>                  <NA>
#> 89                   NA                   <NA>                  <NA>
#> 90                   NA                   <NA>                  <NA>
#> 91                   NA                   <NA>                  <NA>
#> 92                   NA                   <NA>                  <NA>
#> 93                   NA                   <NA>                  <NA>
#> 94                   NA                   <NA>                  <NA>
#> 95                   NA                   <NA>                  <NA>
#> 96                   NA                   <NA>                  <NA>
#> 97                   NA                   <NA>                  <NA>
#> 98                   NA                   <NA>                  <NA>
#> 99                   NA                   <NA>                  <NA>
#> 100                  NA                   <NA>                  <NA>
#> 101                  NA                   <NA>                  <NA>
#> 102                  NA                   <NA>                  <NA>
#> 103                  NA                   <NA>                  <NA>
#> 104                  NA                   <NA>                  <NA>
#> 105                  NA                   <NA>                  <NA>
#> 106                  NA                   <NA>                  <NA>
#> 107                  NA                   <NA>                  <NA>
#> 108                  NA                   <NA>                  <NA>
#> 109                  NA                   <NA>                  <NA>
#> 110                  NA                   <NA>                  <NA>
#> 111                  NA                   <NA>                  <NA>
#> 112                  15                  Alina                Müller
#>     minus_player_two_position minus_player_three_id minus_player_three_first
#> 1                        <NA>                    NA                     <NA>
#> 2                        <NA>                    NA                     <NA>
#> 3                        <NA>                    NA                     <NA>
#> 4                        <NA>                    NA                     <NA>
#> 5                        <NA>                    NA                     <NA>
#> 6                        <NA>                    NA                     <NA>
#> 7                        <NA>                    NA                     <NA>
#> 8                        <NA>                    NA                     <NA>
#> 9                        <NA>                    NA                     <NA>
#> 10                       <NA>                    NA                     <NA>
#> 11                       <NA>                    NA                     <NA>
#> 12                       <NA>                    NA                     <NA>
#> 13                       <NA>                    NA                     <NA>
#> 14                       <NA>                    NA                     <NA>
#> 15                       <NA>                    NA                     <NA>
#> 16                       <NA>                    NA                     <NA>
#> 17                       <NA>                    NA                     <NA>
#> 18                       <NA>                    NA                     <NA>
#> 19                       <NA>                    NA                     <NA>
#> 20                       <NA>                    NA                     <NA>
#> 21                       <NA>                    NA                     <NA>
#> 22                       <NA>                    NA                     <NA>
#> 23                       <NA>                    NA                     <NA>
#> 24                       <NA>                    NA                     <NA>
#> 25                          C                     5                  Jessica
#> 26                       <NA>                    NA                     <NA>
#> 27                       <NA>                    NA                     <NA>
#> 28                       <NA>                    NA                     <NA>
#> 29                       <NA>                    NA                     <NA>
#> 30                       <NA>                    NA                     <NA>
#> 31                       <NA>                    NA                     <NA>
#> 32                       <NA>                    NA                     <NA>
#> 33                       <NA>                    NA                     <NA>
#> 34                       <NA>                    NA                     <NA>
#> 35                       <NA>                    NA                     <NA>
#> 36                       <NA>                    NA                     <NA>
#> 37                       <NA>                    NA                     <NA>
#> 38                       <NA>                    NA                     <NA>
#> 39                       <NA>                    NA                     <NA>
#> 40                       <NA>                    NA                     <NA>
#> 41                       <NA>                    NA                     <NA>
#> 42                       <NA>                    NA                     <NA>
#> 43                       <NA>                    NA                     <NA>
#> 44                       <NA>                    NA                     <NA>
#> 45                       <NA>                    NA                     <NA>
#> 46                       <NA>                    NA                     <NA>
#> 47                       <NA>                    NA                     <NA>
#> 48                       <NA>                    NA                     <NA>
#> 49                       <NA>                    NA                     <NA>
#> 50                       <NA>                    NA                     <NA>
#> 51                       <NA>                    NA                     <NA>
#> 52                       <NA>                    NA                     <NA>
#> 53                       <NA>                    NA                     <NA>
#> 54                       <NA>                    NA                     <NA>
#> 55                       <NA>                    NA                     <NA>
#> 56                       <NA>                    NA                     <NA>
#> 57                       <NA>                    NA                     <NA>
#> 58                       <NA>                    NA                     <NA>
#> 59                       <NA>                    NA                     <NA>
#> 60                       <NA>                    NA                     <NA>
#> 61                       <NA>                    NA                     <NA>
#> 62                       <NA>                    NA                     <NA>
#> 63                       <NA>                    NA                     <NA>
#> 64                       <NA>                    NA                     <NA>
#> 65                       <NA>                    NA                     <NA>
#> 66                       <NA>                    NA                     <NA>
#> 67                       <NA>                    NA                     <NA>
#> 68                       <NA>                    NA                     <NA>
#> 69                       <NA>                    NA                     <NA>
#> 70                       <NA>                    NA                     <NA>
#> 71                       <NA>                    NA                     <NA>
#> 72                       <NA>                    NA                     <NA>
#> 73                       <NA>                    NA                     <NA>
#> 74                       <NA>                    NA                     <NA>
#> 75                         RD                   158                    Sarah
#> 76                       <NA>                    NA                     <NA>
#> 77                       <NA>                    NA                     <NA>
#> 78                       <NA>                    NA                     <NA>
#> 79                       <NA>                    NA                     <NA>
#> 80                       <NA>                    NA                     <NA>
#> 81                       <NA>                    NA                     <NA>
#> 82                       <NA>                    NA                     <NA>
#> 83                       <NA>                    NA                     <NA>
#> 84                       <NA>                    NA                     <NA>
#> 85                       <NA>                    NA                     <NA>
#> 86                       <NA>                    NA                     <NA>
#> 87                       <NA>                    NA                     <NA>
#> 88                       <NA>                    NA                     <NA>
#> 89                       <NA>                    NA                     <NA>
#> 90                       <NA>                    NA                     <NA>
#> 91                       <NA>                    NA                     <NA>
#> 92                       <NA>                    NA                     <NA>
#> 93                       <NA>                    NA                     <NA>
#> 94                       <NA>                    NA                     <NA>
#> 95                       <NA>                    NA                     <NA>
#> 96                       <NA>                    NA                     <NA>
#> 97                       <NA>                    NA                     <NA>
#> 98                       <NA>                    NA                     <NA>
#> 99                       <NA>                    NA                     <NA>
#> 100                      <NA>                    NA                     <NA>
#> 101                      <NA>                    NA                     <NA>
#> 102                      <NA>                    NA                     <NA>
#> 103                      <NA>                    NA                     <NA>
#> 104                      <NA>                    NA                     <NA>
#> 105                      <NA>                    NA                     <NA>
#> 106                      <NA>                    NA                     <NA>
#> 107                      <NA>                    NA                     <NA>
#> 108                      <NA>                    NA                     <NA>
#> 109                      <NA>                    NA                     <NA>
#> 110                      <NA>                    NA                     <NA>
#> 111                      <NA>                    NA                     <NA>
#> 112                         C                   110                   Amanda
#>     minus_player_three_last minus_player_three_position minus_player_four_id
#> 1                      <NA>                        <NA>                   NA
#> 2                      <NA>                        <NA>                   NA
#> 3                      <NA>                        <NA>                   NA
#> 4                      <NA>                        <NA>                   NA
#> 5                      <NA>                        <NA>                   NA
#> 6                      <NA>                        <NA>                   NA
#> 7                      <NA>                        <NA>                   NA
#> 8                      <NA>                        <NA>                   NA
#> 9                      <NA>                        <NA>                   NA
#> 10                     <NA>                        <NA>                   NA
#> 11                     <NA>                        <NA>                   NA
#> 12                     <NA>                        <NA>                   NA
#> 13                     <NA>                        <NA>                   NA
#> 14                     <NA>                        <NA>                   NA
#> 15                     <NA>                        <NA>                   NA
#> 16                     <NA>                        <NA>                   NA
#> 17                     <NA>                        <NA>                   NA
#> 18                     <NA>                        <NA>                   NA
#> 19                     <NA>                        <NA>                   NA
#> 20                     <NA>                        <NA>                   NA
#> 21                     <NA>                        <NA>                   NA
#> 22                     <NA>                        <NA>                   NA
#> 23                     <NA>                        <NA>                   NA
#> 24                     <NA>                        <NA>                   NA
#> 25               DiGirolamo                          RD                    8
#> 26                     <NA>                        <NA>                   NA
#> 27                     <NA>                        <NA>                   NA
#> 28                     <NA>                        <NA>                   NA
#> 29                     <NA>                        <NA>                   NA
#> 30                     <NA>                        <NA>                   NA
#> 31                     <NA>                        <NA>                   NA
#> 32                     <NA>                        <NA>                   NA
#> 33                     <NA>                        <NA>                   NA
#> 34                     <NA>                        <NA>                   NA
#> 35                     <NA>                        <NA>                   NA
#> 36                     <NA>                        <NA>                   NA
#> 37                     <NA>                        <NA>                   NA
#> 38                     <NA>                        <NA>                   NA
#> 39                     <NA>                        <NA>                   NA
#> 40                     <NA>                        <NA>                   NA
#> 41                     <NA>                        <NA>                   NA
#> 42                     <NA>                        <NA>                   NA
#> 43                     <NA>                        <NA>                   NA
#> 44                     <NA>                        <NA>                   NA
#> 45                     <NA>                        <NA>                   NA
#> 46                     <NA>                        <NA>                   NA
#> 47                     <NA>                        <NA>                   NA
#> 48                     <NA>                        <NA>                   NA
#> 49                     <NA>                        <NA>                   NA
#> 50                     <NA>                        <NA>                   NA
#> 51                     <NA>                        <NA>                   NA
#> 52                     <NA>                        <NA>                   NA
#> 53                     <NA>                        <NA>                   NA
#> 54                     <NA>                        <NA>                   NA
#> 55                     <NA>                        <NA>                   NA
#> 56                     <NA>                        <NA>                   NA
#> 57                     <NA>                        <NA>                   NA
#> 58                     <NA>                        <NA>                   NA
#> 59                     <NA>                        <NA>                   NA
#> 60                     <NA>                        <NA>                   NA
#> 61                     <NA>                        <NA>                   NA
#> 62                     <NA>                        <NA>                   NA
#> 63                     <NA>                        <NA>                   NA
#> 64                     <NA>                        <NA>                   NA
#> 65                     <NA>                        <NA>                   NA
#> 66                     <NA>                        <NA>                   NA
#> 67                     <NA>                        <NA>                   NA
#> 68                     <NA>                        <NA>                   NA
#> 69                     <NA>                        <NA>                   NA
#> 70                     <NA>                        <NA>                   NA
#> 71                     <NA>                        <NA>                   NA
#> 72                     <NA>                        <NA>                   NA
#> 73                     <NA>                        <NA>                   NA
#> 74                     <NA>                        <NA>                   NA
#> 75                   Lefort                          LW                   29
#> 76                     <NA>                        <NA>                   NA
#> 77                     <NA>                        <NA>                   NA
#> 78                     <NA>                        <NA>                   NA
#> 79                     <NA>                        <NA>                   NA
#> 80                     <NA>                        <NA>                   NA
#> 81                     <NA>                        <NA>                   NA
#> 82                     <NA>                        <NA>                   NA
#> 83                     <NA>                        <NA>                   NA
#> 84                     <NA>                        <NA>                   NA
#> 85                     <NA>                        <NA>                   NA
#> 86                     <NA>                        <NA>                   NA
#> 87                     <NA>                        <NA>                   NA
#> 88                     <NA>                        <NA>                   NA
#> 89                     <NA>                        <NA>                   NA
#> 90                     <NA>                        <NA>                   NA
#> 91                     <NA>                        <NA>                   NA
#> 92                     <NA>                        <NA>                   NA
#> 93                     <NA>                        <NA>                   NA
#> 94                     <NA>                        <NA>                   NA
#> 95                     <NA>                        <NA>                   NA
#> 96                     <NA>                        <NA>                   NA
#> 97                     <NA>                        <NA>                   NA
#> 98                     <NA>                        <NA>                   NA
#> 99                     <NA>                        <NA>                   NA
#> 100                    <NA>                        <NA>                   NA
#> 101                    <NA>                        <NA>                   NA
#> 102                    <NA>                        <NA>                   NA
#> 103                    <NA>                        <NA>                   NA
#> 104                    <NA>                        <NA>                   NA
#> 105                    <NA>                        <NA>                   NA
#> 106                    <NA>                        <NA>                   NA
#> 107                    <NA>                        <NA>                   NA
#> 108                    <NA>                        <NA>                   NA
#> 109                    <NA>                        <NA>                   NA
#> 110                    <NA>                        <NA>                   NA
#> 111                    <NA>                        <NA>                   NA
#> 112                  Pelkey                          RW                   NA
#>     minus_player_four_first minus_player_four_last minus_player_four_position
#> 1                      <NA>                   <NA>                       <NA>
#> 2                      <NA>                   <NA>                       <NA>
#> 3                      <NA>                   <NA>                       <NA>
#> 4                      <NA>                   <NA>                       <NA>
#> 5                      <NA>                   <NA>                       <NA>
#> 6                      <NA>                   <NA>                       <NA>
#> 7                      <NA>                   <NA>                       <NA>
#> 8                      <NA>                   <NA>                       <NA>
#> 9                      <NA>                   <NA>                       <NA>
#> 10                     <NA>                   <NA>                       <NA>
#> 11                     <NA>                   <NA>                       <NA>
#> 12                     <NA>                   <NA>                       <NA>
#> 13                     <NA>                   <NA>                       <NA>
#> 14                     <NA>                   <NA>                       <NA>
#> 15                     <NA>                   <NA>                       <NA>
#> 16                     <NA>                   <NA>                       <NA>
#> 17                     <NA>                   <NA>                       <NA>
#> 18                     <NA>                   <NA>                       <NA>
#> 19                     <NA>                   <NA>                       <NA>
#> 20                     <NA>                   <NA>                       <NA>
#> 21                     <NA>                   <NA>                       <NA>
#> 22                     <NA>                   <NA>                       <NA>
#> 23                     <NA>                   <NA>                       <NA>
#> 24                     <NA>                   <NA>                       <NA>
#> 25                    Loren                  Gabel                      Gabel
#> 26                     <NA>                   <NA>                       <NA>
#> 27                     <NA>                   <NA>                       <NA>
#> 28                     <NA>                   <NA>                       <NA>
#> 29                     <NA>                   <NA>                       <NA>
#> 30                     <NA>                   <NA>                       <NA>
#> 31                     <NA>                   <NA>                       <NA>
#> 32                     <NA>                   <NA>                       <NA>
#> 33                     <NA>                   <NA>                       <NA>
#> 34                     <NA>                   <NA>                       <NA>
#> 35                     <NA>                   <NA>                       <NA>
#> 36                     <NA>                   <NA>                       <NA>
#> 37                     <NA>                   <NA>                       <NA>
#> 38                     <NA>                   <NA>                       <NA>
#> 39                     <NA>                   <NA>                       <NA>
#> 40                     <NA>                   <NA>                       <NA>
#> 41                     <NA>                   <NA>                       <NA>
#> 42                     <NA>                   <NA>                       <NA>
#> 43                     <NA>                   <NA>                       <NA>
#> 44                     <NA>                   <NA>                       <NA>
#> 45                     <NA>                   <NA>                       <NA>
#> 46                     <NA>                   <NA>                       <NA>
#> 47                     <NA>                   <NA>                       <NA>
#> 48                     <NA>                   <NA>                       <NA>
#> 49                     <NA>                   <NA>                       <NA>
#> 50                     <NA>                   <NA>                       <NA>
#> 51                     <NA>                   <NA>                       <NA>
#> 52                     <NA>                   <NA>                       <NA>
#> 53                     <NA>                   <NA>                       <NA>
#> 54                     <NA>                   <NA>                       <NA>
#> 55                     <NA>                   <NA>                       <NA>
#> 56                     <NA>                   <NA>                       <NA>
#> 57                     <NA>                   <NA>                       <NA>
#> 58                     <NA>                   <NA>                       <NA>
#> 59                     <NA>                   <NA>                       <NA>
#> 60                     <NA>                   <NA>                       <NA>
#> 61                     <NA>                   <NA>                       <NA>
#> 62                     <NA>                   <NA>                       <NA>
#> 63                     <NA>                   <NA>                       <NA>
#> 64                     <NA>                   <NA>                       <NA>
#> 65                     <NA>                   <NA>                       <NA>
#> 66                     <NA>                   <NA>                       <NA>
#> 67                     <NA>                   <NA>                       <NA>
#> 68                     <NA>                   <NA>                       <NA>
#> 69                     <NA>                   <NA>                       <NA>
#> 70                     <NA>                   <NA>                       <NA>
#> 71                     <NA>                   <NA>                       <NA>
#> 72                     <NA>                   <NA>                       <NA>
#> 73                     <NA>                   <NA>                       <NA>
#> 74                     <NA>                   <NA>                       <NA>
#> 75                  Kennedy              Marchment                  Marchment
#> 76                     <NA>                   <NA>                       <NA>
#> 77                     <NA>                   <NA>                       <NA>
#> 78                     <NA>                   <NA>                       <NA>
#> 79                     <NA>                   <NA>                       <NA>
#> 80                     <NA>                   <NA>                       <NA>
#> 81                     <NA>                   <NA>                       <NA>
#> 82                     <NA>                   <NA>                       <NA>
#> 83                     <NA>                   <NA>                       <NA>
#> 84                     <NA>                   <NA>                       <NA>
#> 85                     <NA>                   <NA>                       <NA>
#> 86                     <NA>                   <NA>                       <NA>
#> 87                     <NA>                   <NA>                       <NA>
#> 88                     <NA>                   <NA>                       <NA>
#> 89                     <NA>                   <NA>                       <NA>
#> 90                     <NA>                   <NA>                       <NA>
#> 91                     <NA>                   <NA>                       <NA>
#> 92                     <NA>                   <NA>                       <NA>
#> 93                     <NA>                   <NA>                       <NA>
#> 94                     <NA>                   <NA>                       <NA>
#> 95                     <NA>                   <NA>                       <NA>
#> 96                     <NA>                   <NA>                       <NA>
#> 97                     <NA>                   <NA>                       <NA>
#> 98                     <NA>                   <NA>                       <NA>
#> 99                     <NA>                   <NA>                       <NA>
#> 100                    <NA>                   <NA>                       <NA>
#> 101                    <NA>                   <NA>                       <NA>
#> 102                    <NA>                   <NA>                       <NA>
#> 103                    <NA>                   <NA>                       <NA>
#> 104                    <NA>                   <NA>                       <NA>
#> 105                    <NA>                   <NA>                       <NA>
#> 106                    <NA>                   <NA>                       <NA>
#> 107                    <NA>                   <NA>                       <NA>
#> 108                    <NA>                   <NA>                       <NA>
#> 109                    <NA>                   <NA>                       <NA>
#> 110                    <NA>                   <NA>                       <NA>
#> 111                    <NA>                   <NA>                       <NA>
#> 112                    <NA>                   <NA>                       <NA>
#>     minus_player_five_id minus_player_five_first minus_player_five_last
#> 1                     NA                    <NA>                   <NA>
#> 2                     NA                    <NA>                   <NA>
#> 3                     NA                    <NA>                   <NA>
#> 4                     NA                    <NA>                   <NA>
#> 5                     NA                    <NA>                   <NA>
#> 6                     NA                    <NA>                   <NA>
#> 7                     NA                    <NA>                   <NA>
#> 8                     NA                    <NA>                   <NA>
#> 9                     NA                    <NA>                   <NA>
#> 10                    NA                    <NA>                   <NA>
#> 11                    NA                    <NA>                   <NA>
#> 12                    NA                    <NA>                   <NA>
#> 13                    NA                    <NA>                   <NA>
#> 14                    NA                    <NA>                   <NA>
#> 15                    NA                    <NA>                   <NA>
#> 16                    NA                    <NA>                   <NA>
#> 17                    NA                    <NA>                   <NA>
#> 18                    NA                    <NA>                   <NA>
#> 19                    NA                    <NA>                   <NA>
#> 20                    NA                    <NA>                   <NA>
#> 21                    NA                    <NA>                   <NA>
#> 22                    NA                    <NA>                   <NA>
#> 23                    NA                    <NA>                   <NA>
#> 24                    NA                    <NA>                   <NA>
#> 25                    16               Jamie Lee                Rattray
#> 26                    NA                    <NA>                   <NA>
#> 27                    NA                    <NA>                   <NA>
#> 28                    NA                    <NA>                   <NA>
#> 29                    NA                    <NA>                   <NA>
#> 30                    NA                    <NA>                   <NA>
#> 31                    NA                    <NA>                   <NA>
#> 32                    NA                    <NA>                   <NA>
#> 33                    NA                    <NA>                   <NA>
#> 34                    NA                    <NA>                   <NA>
#> 35                    NA                    <NA>                   <NA>
#> 36                    NA                    <NA>                   <NA>
#> 37                    NA                    <NA>                   <NA>
#> 38                    NA                    <NA>                   <NA>
#> 39                    NA                    <NA>                   <NA>
#> 40                    NA                    <NA>                   <NA>
#> 41                    NA                    <NA>                   <NA>
#> 42                    NA                    <NA>                   <NA>
#> 43                    NA                    <NA>                   <NA>
#> 44                    NA                    <NA>                   <NA>
#> 45                    NA                    <NA>                   <NA>
#> 46                    NA                    <NA>                   <NA>
#> 47                    NA                    <NA>                   <NA>
#> 48                    NA                    <NA>                   <NA>
#> 49                    NA                    <NA>                   <NA>
#> 50                    NA                    <NA>                   <NA>
#> 51                    NA                    <NA>                   <NA>
#> 52                    NA                    <NA>                   <NA>
#> 53                    NA                    <NA>                   <NA>
#> 54                    NA                    <NA>                   <NA>
#> 55                    NA                    <NA>                   <NA>
#> 56                    NA                    <NA>                   <NA>
#> 57                    NA                    <NA>                   <NA>
#> 58                    NA                    <NA>                   <NA>
#> 59                    NA                    <NA>                   <NA>
#> 60                    NA                    <NA>                   <NA>
#> 61                    NA                    <NA>                   <NA>
#> 62                    NA                    <NA>                   <NA>
#> 63                    NA                    <NA>                   <NA>
#> 64                    NA                    <NA>                   <NA>
#> 65                    NA                    <NA>                   <NA>
#> 66                    NA                    <NA>                   <NA>
#> 67                    NA                    <NA>                   <NA>
#> 68                    NA                    <NA>                   <NA>
#> 69                    NA                    <NA>                   <NA>
#> 70                    NA                    <NA>                   <NA>
#> 71                    NA                    <NA>                   <NA>
#> 72                    NA                    <NA>                   <NA>
#> 73                    NA                    <NA>                   <NA>
#> 74                    NA                    <NA>                   <NA>
#> 75                    81                   Sarah                 Bujold
#> 76                    NA                    <NA>                   <NA>
#> 77                    NA                    <NA>                   <NA>
#> 78                    NA                    <NA>                   <NA>
#> 79                    NA                    <NA>                   <NA>
#> 80                    NA                    <NA>                   <NA>
#> 81                    NA                    <NA>                   <NA>
#> 82                    NA                    <NA>                   <NA>
#> 83                    NA                    <NA>                   <NA>
#> 84                    NA                    <NA>                   <NA>
#> 85                    NA                    <NA>                   <NA>
#> 86                    NA                    <NA>                   <NA>
#> 87                    NA                    <NA>                   <NA>
#> 88                    NA                    <NA>                   <NA>
#> 89                    NA                    <NA>                   <NA>
#> 90                    NA                    <NA>                   <NA>
#> 91                    NA                    <NA>                   <NA>
#> 92                    NA                    <NA>                   <NA>
#> 93                    NA                    <NA>                   <NA>
#> 94                    NA                    <NA>                   <NA>
#> 95                    NA                    <NA>                   <NA>
#> 96                    NA                    <NA>                   <NA>
#> 97                    NA                    <NA>                   <NA>
#> 98                    NA                    <NA>                   <NA>
#> 99                    NA                    <NA>                   <NA>
#> 100                   NA                    <NA>                   <NA>
#> 101                   NA                    <NA>                   <NA>
#> 102                   NA                    <NA>                   <NA>
#> 103                   NA                    <NA>                   <NA>
#> 104                   NA                    <NA>                   <NA>
#> 105                   NA                    <NA>                   <NA>
#> 106                   NA                    <NA>                   <NA>
#> 107                   NA                    <NA>                   <NA>
#> 108                   NA                    <NA>                   <NA>
#> 109                   NA                    <NA>                   <NA>
#> 110                   NA                    <NA>                   <NA>
#> 111                   NA                    <NA>                   <NA>
#> 112                   NA                    <NA>                   <NA>
#>     minus_player_five_position                 game_date game_season
#> 1                         <NA> Sunday, February 04, 2024        2024
#> 2                         <NA> Sunday, February 04, 2024        2024
#> 3                         <NA> Sunday, February 04, 2024        2024
#> 4                         <NA> Sunday, February 04, 2024        2024
#> 5                         <NA> Sunday, February 04, 2024        2024
#> 6                         <NA> Sunday, February 04, 2024        2024
#> 7                         <NA> Sunday, February 04, 2024        2024
#> 8                         <NA> Sunday, February 04, 2024        2024
#> 9                         <NA> Sunday, February 04, 2024        2024
#> 10                        <NA> Sunday, February 04, 2024        2024
#> 11                        <NA> Sunday, February 04, 2024        2024
#> 12                        <NA> Sunday, February 04, 2024        2024
#> 13                        <NA> Sunday, February 04, 2024        2024
#> 14                        <NA> Sunday, February 04, 2024        2024
#> 15                        <NA> Sunday, February 04, 2024        2024
#> 16                        <NA> Sunday, February 04, 2024        2024
#> 17                        <NA> Sunday, February 04, 2024        2024
#> 18                        <NA> Sunday, February 04, 2024        2024
#> 19                        <NA> Sunday, February 04, 2024        2024
#> 20                        <NA> Sunday, February 04, 2024        2024
#> 21                        <NA> Sunday, February 04, 2024        2024
#> 22                        <NA> Sunday, February 04, 2024        2024
#> 23                        <NA> Sunday, February 04, 2024        2024
#> 24                        <NA> Sunday, February 04, 2024        2024
#> 25                          LW Sunday, February 04, 2024        2024
#> 26                        <NA> Sunday, February 04, 2024        2024
#> 27                        <NA> Sunday, February 04, 2024        2024
#> 28                        <NA> Sunday, February 04, 2024        2024
#> 29                        <NA> Sunday, February 04, 2024        2024
#> 30                        <NA> Sunday, February 04, 2024        2024
#> 31                        <NA> Sunday, February 04, 2024        2024
#> 32                        <NA> Sunday, February 04, 2024        2024
#> 33                        <NA> Sunday, February 04, 2024        2024
#> 34                        <NA> Sunday, February 04, 2024        2024
#> 35                        <NA> Sunday, February 04, 2024        2024
#> 36                        <NA> Sunday, February 04, 2024        2024
#> 37                        <NA> Sunday, February 04, 2024        2024
#> 38                        <NA> Sunday, February 04, 2024        2024
#> 39                        <NA> Sunday, February 04, 2024        2024
#> 40                        <NA> Sunday, February 04, 2024        2024
#> 41                        <NA> Sunday, February 04, 2024        2024
#> 42                        <NA> Sunday, February 04, 2024        2024
#> 43                        <NA> Sunday, February 04, 2024        2024
#> 44                        <NA> Sunday, February 04, 2024        2024
#> 45                        <NA> Sunday, February 04, 2024        2024
#> 46                        <NA> Sunday, February 04, 2024        2024
#> 47                        <NA> Sunday, February 04, 2024        2024
#> 48                        <NA> Sunday, February 04, 2024        2024
#> 49                        <NA> Sunday, February 04, 2024        2024
#> 50                        <NA> Sunday, February 04, 2024        2024
#> 51                        <NA> Sunday, February 04, 2024        2024
#> 52                        <NA> Sunday, February 04, 2024        2024
#> 53                        <NA> Sunday, February 04, 2024        2024
#> 54                        <NA> Sunday, February 04, 2024        2024
#> 55                        <NA> Sunday, February 04, 2024        2024
#> 56                        <NA> Sunday, February 04, 2024        2024
#> 57                        <NA> Sunday, February 04, 2024        2024
#> 58                        <NA> Sunday, February 04, 2024        2024
#> 59                        <NA> Sunday, February 04, 2024        2024
#> 60                        <NA> Sunday, February 04, 2024        2024
#> 61                        <NA> Sunday, February 04, 2024        2024
#> 62                        <NA> Sunday, February 04, 2024        2024
#> 63                        <NA> Sunday, February 04, 2024        2024
#> 64                        <NA> Sunday, February 04, 2024        2024
#> 65                        <NA> Sunday, February 04, 2024        2024
#> 66                        <NA> Sunday, February 04, 2024        2024
#> 67                        <NA> Sunday, February 04, 2024        2024
#> 68                        <NA> Sunday, February 04, 2024        2024
#> 69                        <NA> Sunday, February 04, 2024        2024
#> 70                        <NA> Sunday, February 04, 2024        2024
#> 71                        <NA> Sunday, February 04, 2024        2024
#> 72                        <NA> Sunday, February 04, 2024        2024
#> 73                        <NA> Sunday, February 04, 2024        2024
#> 74                        <NA> Sunday, February 04, 2024        2024
#> 75                           C Sunday, February 04, 2024        2024
#> 76                        <NA> Sunday, February 04, 2024        2024
#> 77                        <NA> Sunday, February 04, 2024        2024
#> 78                        <NA> Sunday, February 04, 2024        2024
#> 79                        <NA> Sunday, February 04, 2024        2024
#> 80                        <NA> Sunday, February 04, 2024        2024
#> 81                        <NA> Sunday, February 04, 2024        2024
#> 82                        <NA> Sunday, February 04, 2024        2024
#> 83                        <NA> Sunday, February 04, 2024        2024
#> 84                        <NA> Sunday, February 04, 2024        2024
#> 85                        <NA> Sunday, February 04, 2024        2024
#> 86                        <NA> Sunday, February 04, 2024        2024
#> 87                        <NA> Sunday, February 04, 2024        2024
#> 88                        <NA> Sunday, February 04, 2024        2024
#> 89                        <NA> Sunday, February 04, 2024        2024
#> 90                        <NA> Sunday, February 04, 2024        2024
#> 91                        <NA> Sunday, February 04, 2024        2024
#> 92                        <NA> Sunday, February 04, 2024        2024
#> 93                        <NA> Sunday, February 04, 2024        2024
#> 94                        <NA> Sunday, February 04, 2024        2024
#> 95                        <NA> Sunday, February 04, 2024        2024
#> 96                        <NA> Sunday, February 04, 2024        2024
#> 97                        <NA> Sunday, February 04, 2024        2024
#> 98                        <NA> Sunday, February 04, 2024        2024
#> 99                        <NA> Sunday, February 04, 2024        2024
#> 100                       <NA> Sunday, February 04, 2024        2024
#> 101                       <NA> Sunday, February 04, 2024        2024
#> 102                       <NA> Sunday, February 04, 2024        2024
#> 103                       <NA> Sunday, February 04, 2024        2024
#> 104                       <NA> Sunday, February 04, 2024        2024
#> 105                       <NA> Sunday, February 04, 2024        2024
#> 106                       <NA> Sunday, February 04, 2024        2024
#> 107                       <NA> Sunday, February 04, 2024        2024
#> 108                       <NA> Sunday, February 04, 2024        2024
#> 109                       <NA> Sunday, February 04, 2024        2024
#> 110                       <NA> Sunday, February 04, 2024        2024
#> 111                       <NA> Sunday, February 04, 2024        2024
#> 112                       <NA> Sunday, February 04, 2024        2024
#>     game_season_id home_team_id   home_team away_team_id     away_team
#> 1                1            1 PWHL Boston            3 PWHL Montreal
#> 2                1            1 PWHL Boston            3 PWHL Montreal
#> 3                1            1 PWHL Boston            3 PWHL Montreal
#> 4                1            1 PWHL Boston            3 PWHL Montreal
#> 5                1            1 PWHL Boston            3 PWHL Montreal
#> 6                1            1 PWHL Boston            3 PWHL Montreal
#> 7                1            1 PWHL Boston            3 PWHL Montreal
#> 8                1            1 PWHL Boston            3 PWHL Montreal
#> 9                1            1 PWHL Boston            3 PWHL Montreal
#> 10               1            1 PWHL Boston            3 PWHL Montreal
#> 11               1            1 PWHL Boston            3 PWHL Montreal
#> 12               1            1 PWHL Boston            3 PWHL Montreal
#> 13               1            1 PWHL Boston            3 PWHL Montreal
#> 14               1            1 PWHL Boston            3 PWHL Montreal
#> 15               1            1 PWHL Boston            3 PWHL Montreal
#> 16               1            1 PWHL Boston            3 PWHL Montreal
#> 17               1            1 PWHL Boston            3 PWHL Montreal
#> 18               1            1 PWHL Boston            3 PWHL Montreal
#> 19               1            1 PWHL Boston            3 PWHL Montreal
#> 20               1            1 PWHL Boston            3 PWHL Montreal
#> 21               1            1 PWHL Boston            3 PWHL Montreal
#> 22               1            1 PWHL Boston            3 PWHL Montreal
#> 23               1            1 PWHL Boston            3 PWHL Montreal
#> 24               1            1 PWHL Boston            3 PWHL Montreal
#> 25               1            1 PWHL Boston            3 PWHL Montreal
#> 26               1            1 PWHL Boston            3 PWHL Montreal
#> 27               1            1 PWHL Boston            3 PWHL Montreal
#> 28               1            1 PWHL Boston            3 PWHL Montreal
#> 29               1            1 PWHL Boston            3 PWHL Montreal
#> 30               1            1 PWHL Boston            3 PWHL Montreal
#> 31               1            1 PWHL Boston            3 PWHL Montreal
#> 32               1            1 PWHL Boston            3 PWHL Montreal
#> 33               1            1 PWHL Boston            3 PWHL Montreal
#> 34               1            1 PWHL Boston            3 PWHL Montreal
#> 35               1            1 PWHL Boston            3 PWHL Montreal
#> 36               1            1 PWHL Boston            3 PWHL Montreal
#> 37               1            1 PWHL Boston            3 PWHL Montreal
#> 38               1            1 PWHL Boston            3 PWHL Montreal
#> 39               1            1 PWHL Boston            3 PWHL Montreal
#> 40               1            1 PWHL Boston            3 PWHL Montreal
#> 41               1            1 PWHL Boston            3 PWHL Montreal
#> 42               1            1 PWHL Boston            3 PWHL Montreal
#> 43               1            1 PWHL Boston            3 PWHL Montreal
#> 44               1            1 PWHL Boston            3 PWHL Montreal
#> 45               1            1 PWHL Boston            3 PWHL Montreal
#> 46               1            1 PWHL Boston            3 PWHL Montreal
#> 47               1            1 PWHL Boston            3 PWHL Montreal
#> 48               1            1 PWHL Boston            3 PWHL Montreal
#> 49               1            1 PWHL Boston            3 PWHL Montreal
#> 50               1            1 PWHL Boston            3 PWHL Montreal
#> 51               1            1 PWHL Boston            3 PWHL Montreal
#> 52               1            1 PWHL Boston            3 PWHL Montreal
#> 53               1            1 PWHL Boston            3 PWHL Montreal
#> 54               1            1 PWHL Boston            3 PWHL Montreal
#> 55               1            1 PWHL Boston            3 PWHL Montreal
#> 56               1            1 PWHL Boston            3 PWHL Montreal
#> 57               1            1 PWHL Boston            3 PWHL Montreal
#> 58               1            1 PWHL Boston            3 PWHL Montreal
#> 59               1            1 PWHL Boston            3 PWHL Montreal
#> 60               1            1 PWHL Boston            3 PWHL Montreal
#> 61               1            1 PWHL Boston            3 PWHL Montreal
#> 62               1            1 PWHL Boston            3 PWHL Montreal
#> 63               1            1 PWHL Boston            3 PWHL Montreal
#> 64               1            1 PWHL Boston            3 PWHL Montreal
#> 65               1            1 PWHL Boston            3 PWHL Montreal
#> 66               1            1 PWHL Boston            3 PWHL Montreal
#> 67               1            1 PWHL Boston            3 PWHL Montreal
#> 68               1            1 PWHL Boston            3 PWHL Montreal
#> 69               1            1 PWHL Boston            3 PWHL Montreal
#> 70               1            1 PWHL Boston            3 PWHL Montreal
#> 71               1            1 PWHL Boston            3 PWHL Montreal
#> 72               1            1 PWHL Boston            3 PWHL Montreal
#> 73               1            1 PWHL Boston            3 PWHL Montreal
#> 74               1            1 PWHL Boston            3 PWHL Montreal
#> 75               1            1 PWHL Boston            3 PWHL Montreal
#> 76               1            1 PWHL Boston            3 PWHL Montreal
#> 77               1            1 PWHL Boston            3 PWHL Montreal
#> 78               1            1 PWHL Boston            3 PWHL Montreal
#> 79               1            1 PWHL Boston            3 PWHL Montreal
#> 80               1            1 PWHL Boston            3 PWHL Montreal
#> 81               1            1 PWHL Boston            3 PWHL Montreal
#> 82               1            1 PWHL Boston            3 PWHL Montreal
#> 83               1            1 PWHL Boston            3 PWHL Montreal
#> 84               1            1 PWHL Boston            3 PWHL Montreal
#> 85               1            1 PWHL Boston            3 PWHL Montreal
#> 86               1            1 PWHL Boston            3 PWHL Montreal
#> 87               1            1 PWHL Boston            3 PWHL Montreal
#> 88               1            1 PWHL Boston            3 PWHL Montreal
#> 89               1            1 PWHL Boston            3 PWHL Montreal
#> 90               1            1 PWHL Boston            3 PWHL Montreal
#> 91               1            1 PWHL Boston            3 PWHL Montreal
#> 92               1            1 PWHL Boston            3 PWHL Montreal
#> 93               1            1 PWHL Boston            3 PWHL Montreal
#> 94               1            1 PWHL Boston            3 PWHL Montreal
#> 95               1            1 PWHL Boston            3 PWHL Montreal
#> 96               1            1 PWHL Boston            3 PWHL Montreal
#> 97               1            1 PWHL Boston            3 PWHL Montreal
#> 98               1            1 PWHL Boston            3 PWHL Montreal
#> 99               1            1 PWHL Boston            3 PWHL Montreal
#> 100              1            1 PWHL Boston            3 PWHL Montreal
#> 101              1            1 PWHL Boston            3 PWHL Montreal
#> 102              1            1 PWHL Boston            3 PWHL Montreal
#> 103              1            1 PWHL Boston            3 PWHL Montreal
#> 104              1            1 PWHL Boston            3 PWHL Montreal
#> 105              1            1 PWHL Boston            3 PWHL Montreal
#> 106              1            1 PWHL Boston            3 PWHL Montreal
#> 107              1            1 PWHL Boston            3 PWHL Montreal
#> 108              1            1 PWHL Boston            3 PWHL Montreal
#> 109              1            1 PWHL Boston            3 PWHL Montreal
#> 110              1            1 PWHL Boston            3 PWHL Montreal
#> 111              1            1 PWHL Boston            3 PWHL Montreal
#> 112              1            1 PWHL Boston            3 PWHL Montreal
#>     x_coord_original y_coord_original x_coord_neutral y_coord_neutral
#> 1                300              150               0               0
#> 2                457               49             157            -101
#> 3                480               90             180             -60
#> 4                100              206            -200              56
#> 5                203              227             -97              77
#> 6                124              195            -176              45
#> 7                496              111             196             -39
#> 8                453              145             153              -5
#> 9                436               96             136             -54
#> 10               143              201            -157              51
#> 11               464               89             164             -61
#> 12               457               49             157            -101
#> 13               457               49             157            -101
#> 14                NA               NA              NA              NA
#> 15               300              150               0               0
#> 16               483              142             183              -8
#> 17               457               49             157            -101
#> 18               134              245            -166              95
#> 19               457               49             157            -101
#> 20               137              189            -163              39
#> 21               161              119            -139             -31
#> 22               300              150               0               0
#> 23               100              212            -200              62
#> 24               501              182             201              32
#> 25               501              182             201              32
#> 26               453               80             153             -70
#> 27                97              160            -203              10
#> 28               100               49            -200            -101
#> 29               203               27             -97            -123
#> 30               466               39             166            -111
#> 31               457               49             157            -101
#> 32               106              250            -194             100
#> 33               273               73             -27             -77
#> 34               300              150               0               0
#> 35               123              135            -177             -15
#> 36               131              115            -169             -35
#> 37               135               93            -165             -57
#> 38               247               73             -53             -77
#> 39                NA               NA              NA              NA
#> 40               300              150               0               0
#> 41               300              150               0               0
#> 42               116              214            -184              64
#> 43               112               65            -188             -85
#> 44                80              102            -220             -48
#> 45               499               75             199             -75
#> 46               500               94             200             -56
#> 47               417              177             117              27
#> 48               207              247             -93              97
#> 49               143               94            -157             -56
#> 50               466              103             166             -47
#> 51               470              184             170              34
#> 52                NA               NA              NA              NA
#> 53               300              150               0               0
#> 54               397              273              97             123
#> 55                NA               NA              NA              NA
#> 56               143              251            -157             101
#> 57               141               58            -159             -92
#> 58               143               94            -157             -56
#> 59               189               77            -111             -73
#> 60               414               86             114             -64
#> 61               500               94             200             -56
#> 62               247              273             -53             123
#> 63               500               94             200             -56
#> 64               300              150               0               0
#> 65               100               49            -200            -101
#> 66               123              216            -177              66
#> 67               364               80              64             -70
#> 68               100               49            -200            -101
#> 69               466              162             166              12
#> 70               457              206             157              56
#> 71               353              227              53              77
#> 72               539              177             239              27
#> 73               457              206             157              56
#> 74               125              153            -175               3
#> 75               125              153            -175               3
#> 76               300              150               0               0
#> 77               300              150               0               0
#> 78               134               99            -166             -51
#> 79               100               49            -200            -101
#> 80               129              155            -171               5
#> 81               123              168            -177              18
#> 82               546              174             246              24
#> 83               457              206             157              56
#> 84               411               63             111             -87
#> 85               495               62             195             -88
#> 86               182              165            -118              15
#> 87               435              143             135              -7
#> 88               457               49             157            -101
#> 89               457               49             157            -101
#> 90               478              193             178              43
#> 91               203               27             -97            -123
#> 92               300              150               0               0
#> 93               171              107            -129             -43
#> 94               100               49            -200            -101
#> 95               203               27             -97            -123
#> 96               546              128             246             -22
#> 97               203              227             -97              77
#> 98               532              123             232             -27
#> 99               100              206            -200              56
#> 100              100              206            -200              56
#> 101              457              206             157              56
#> 102              183              223            -117              73
#> 103              525              207             225              57
#> 104              300              150               0               0
#> 105              479              155             179               5
#> 106               58              128            -242             -22
#> 107              119              209            -181              59
#> 108              475              203             175              53
#> 109               75              176            -225              26
#> 110              143              251            -157             101
#> 111              507              165             207              15
#> 112              507              165             207              15
#>     x_coord_fixed y_coord_fixed x_coord_right y_coord_right x_coord_vertical
#> 1        0.000000      85.00000            NA            NA               NA
#> 2       17.444444      76.89194            NA            NA               NA
#> 3       20.000000      80.18333      60.00000     17.000000        68.000000
#> 4      -22.222222      89.49556            NA            NA               NA
#> 5      -10.777778      91.18139            NA            NA               NA
#> 6      -19.555556      88.61250     258.66667     97.750000       -12.750000
#> 7       21.777778      81.86917      65.33333     11.050000        73.950000
#> 8       17.000000      84.59861      51.00000      1.416667        83.583333
#> 9       15.111111      80.66500      45.33333     15.300000        69.700000
#> 10     -17.444444      89.09417     252.33333     99.450000       -14.450000
#> 11      18.222222      80.10306      54.66667     17.283333        67.716667
#> 12      17.444444      76.89194            NA            NA               NA
#> 13      17.444444      76.89194            NA            NA               NA
#> 14             NA            NA            NA            NA               NA
#> 15       0.000000      85.00000            NA            NA               NA
#> 16      20.333333      84.35778      61.00000      2.266667        82.733333
#> 17      17.444444      76.89194            NA            NA               NA
#> 18     -18.444444      92.62639     255.33333    111.916667       -26.916667
#> 19      17.444444      76.89194            NA            NA               NA
#> 20     -18.111111      88.13083     254.33333     96.050000       -11.050000
#> 21     -15.444444      82.51139     246.33333     76.216667         8.783333
#> 22       0.000000      85.00000            NA            NA               NA
#> 23     -22.222222      89.97722     266.66667    102.566667       -17.566667
#> 24      22.333333      87.56889      67.00000     -9.066667        94.066667
#> 25      22.333333      87.56889      67.00000     -9.066667        94.066667
#> 26      17.000000      79.38056      51.00000     19.833333        65.166667
#> 27     -22.555556      85.80278     267.66667     87.833333        -2.833333
#> 28     -22.222222      76.89194            NA            NA               NA
#> 29     -10.777778      75.12583            NA            NA               NA
#> 30      18.444444      76.08917      55.33333     31.450000        53.550000
#> 31      17.444444      76.89194            NA            NA               NA
#> 32     -21.555556      93.02778     -64.66667    -28.333333       113.333333
#> 33      -3.000000      78.81861     209.00000     63.183333        21.816667
#> 34       0.000000      85.00000            NA            NA               NA
#> 35     -19.666667      83.79583     259.00000     80.750000         4.250000
#> 36     -18.777778      82.19028     256.33333     75.083333         9.916667
#> 37     -18.333333      80.42417     255.00000     68.850000        16.150000
#> 38      -5.888889      78.81861            NA            NA               NA
#> 39             NA            NA            NA            NA               NA
#> 40       0.000000      85.00000            NA            NA               NA
#> 41       0.000000      85.00000            NA            NA               NA
#> 42     -20.444444      90.13778     261.33333    103.133333       -18.133333
#> 43     -20.888889      78.17639     262.66667     60.916667        24.083333
#> 44     -24.444444      81.14667     273.33333     71.400000        13.600000
#> 45      22.111111      78.97917      66.33333     21.250000        63.750000
#> 46      22.222222      80.50444            NA            NA               NA
#> 47      13.000000      87.16750      39.00000     -7.650000        92.650000
#> 48     -10.333333      92.78694     -31.00000    -27.483333       112.483333
#> 49     -17.444444      80.50444            NA            NA               NA
#> 50      18.444444      81.22694      55.33333     13.316667        71.683333
#> 51      18.888889      87.72944      56.66667     -9.633333        94.633333
#> 52             NA            NA            NA            NA               NA
#> 53       0.000000      85.00000            NA            NA               NA
#> 54      10.777778      94.87417            NA            NA               NA
#> 55             NA            NA            NA            NA               NA
#> 56     -17.444444      93.10806            NA            NA               NA
#> 57     -17.666667      77.61444     253.00000     58.933333        26.066667
#> 58     -17.444444      80.50444            NA            NA               NA
#> 59     -12.333333      79.13972     237.00000     64.316667        20.683333
#> 60      12.666667      79.86222      38.00000     18.133333        66.866667
#> 61      22.222222      80.50444            NA            NA               NA
#> 62      -5.888889      94.87417            NA            NA               NA
#> 63      22.222222      80.50444            NA            NA               NA
#> 64       0.000000      85.00000            NA            NA               NA
#> 65     -22.222222      76.89194            NA            NA               NA
#> 66     -19.666667      90.29833     259.00000    103.700000       -18.700000
#> 67       7.111111      79.38056      21.33333     19.833333        65.166667
#> 68     -22.222222      76.89194            NA            NA               NA
#> 69      18.444444      85.96333      55.33333     -3.400000        88.400000
#> 70      17.444444      89.49556            NA            NA               NA
#> 71       5.888889      91.18139            NA            NA               NA
#> 72      26.555556      87.16750      79.66667     -7.650000        92.650000
#> 73      17.444444      89.49556            NA            NA               NA
#> 74     -19.444444      85.24083     258.33333     85.850000        -0.850000
#> 75     -19.444444      85.24083     258.33333     85.850000        -0.850000
#> 76       0.000000      85.00000            NA            NA               NA
#> 77       0.000000      85.00000            NA            NA               NA
#> 78     -18.444444      80.90583     255.33333     70.550000        14.450000
#> 79     -22.222222      76.89194            NA            NA               NA
#> 80     -19.000000      85.40139     257.00000     86.416667        -1.416667
#> 81     -19.666667      86.44500     259.00000     90.100000        -5.100000
#> 82      27.333333      86.92667      82.00000     -6.800000        91.800000
#> 83      17.444444      89.49556            NA            NA               NA
#> 84      12.333333      78.01583      37.00000     24.650000        60.350000
#> 85      21.666667      77.93556      65.00000     24.933333        60.066667
#> 86     -13.111111      86.20417     239.33333     89.250000        -4.250000
#> 87      15.000000      84.43806      45.00000      1.983333        83.016667
#> 88      17.444444      76.89194            NA            NA               NA
#> 89      17.444444      76.89194            NA            NA               NA
#> 90      19.777778      88.45194      59.33333    -12.183333        97.183333
#> 91     -10.777778      75.12583            NA            NA               NA
#> 92       0.000000      85.00000            NA            NA               NA
#> 93     -14.333333      81.54806     243.00000     72.816667        12.183333
#> 94     -22.222222      76.89194            NA            NA               NA
#> 95     -10.777778      75.12583            NA            NA               NA
#> 96      27.333333      83.23389      82.00000      6.233333        78.766667
#> 97     -10.777778      91.18139            NA            NA               NA
#> 98      25.777778      82.83250      77.33333      7.650000        77.350000
#> 99     -22.222222      89.49556            NA            NA               NA
#> 100    -22.222222      89.49556            NA            NA               NA
#> 101     17.444444      89.49556            NA            NA               NA
#> 102    -13.000000      90.86028     239.00000    105.683333       -20.683333
#> 103     25.000000      89.57583      75.00000    -16.150000       101.150000
#> 104      0.000000      85.00000            NA            NA               NA
#> 105     19.888889      85.40139      59.66667     -1.416667        86.416667
#> 106    -26.888889      83.23389     280.66667     78.766667         6.233333
#> 107    -20.111111      89.73639     260.33333    101.716667       -16.716667
#> 108     19.444444      89.25472      58.33333    -15.016667       100.016667
#> 109    -25.000000      87.08722     275.00000     92.366667        -7.366667
#> 110    -17.444444      93.10806            NA            NA               NA
#> 111     23.000000      86.20417      69.00000     -4.250000        89.250000
#> 112     23.000000      86.20417      69.00000     -4.250000        89.250000
#>     y_coord_vertical minute_start second_start clock sec_from_start
#> 1                 NA            0            0 20:00              0
#> 2                 NA            0           31 19:29             31
#> 3           60.00000            1           14 18:46             74
#> 4                 NA            1           22 18:38             82
#> 5                 NA            1           58 18:02            118
#> 6          258.66667            3           49 16:11            229
#> 7           65.33333            4           27 15:33            267
#> 8           51.00000            4           41 15:19            281
#> 9           45.33333            6           21 13:39            381
#> 10         252.33333            6           41 13:19            401
#> 11          54.66667            7           17 12:43            437
#> 12                NA            8           35 11:25            515
#> 13                NA            9            2 10:58            542
#> 14                NA            9           27 10:33            567
#> 15                NA            9           27 10:33            567
#> 16          61.00000           10           24  9:36            624
#> 17                NA           11           20  8:40            680
#> 18         255.33333           12           43  7:17            763
#> 19                NA           12           54  7:06            774
#> 20         254.33333           13           39  6:21            819
#> 21         246.33333           15           19  4:41            919
#> 22                NA           15           30  4:30            930
#> 23         266.66667           15           33  4:27            933
#> 24          67.00000           15           59  4:01            959
#> 25          67.00000           15           59  4:01            959
#> 26          51.00000           16           12  3:48            972
#> 27         267.66667           17           18  2:42           1038
#> 28                NA           17           25  2:35           1045
#> 29                NA           17           33  2:27           1053
#> 30          55.33333           17           52  2:08           1072
#> 31                NA           17           52  2:08           1072
#> 32         -64.66667           18           23  1:37           1103
#> 33         209.00000           19           21  0:39           1161
#> 34                NA            0            0 20:00           1200
#> 35         259.00000            1           30 18:30           1290
#> 36         256.33333            4            4 15:56           1444
#> 37         255.00000            4           26 15:34           1466
#> 38                NA            4           32 15:28           1472
#> 39                NA            5           39 14:21           1539
#> 40                NA            5           39 14:21           1539
#> 41                NA            6           44 13:16           1604
#> 42         261.33333            6           45 13:15           1605
#> 43         262.66667            7            4 12:56           1624
#> 44         273.33333            7           44 12:16           1664
#> 45          66.33333            8           38 11:22           1718
#> 46                NA            8           38 11:22           1718
#> 47          39.00000            9            5 10:55           1745
#> 48         -31.00000            9           25 10:35           1765
#> 49                NA           10           26  9:34           1826
#> 50          55.33333           10           38  9:22           1838
#> 51          56.66667           11           30  8:30           1890
#> 52                NA           11           53  8:07           1913
#> 53                NA           11           53  8:07           1913
#> 54                NA           12           49  7:11           1969
#> 55                NA           14           24  5:36           2064
#> 56                NA           14           24  5:36           2064
#> 57         253.00000           15            3  4:57           2103
#> 58                NA           16           20  3:40           2180
#> 59         237.00000           16           36  3:24           2196
#> 60          38.00000           17           43  2:17           2263
#> 61                NA           17           43  2:17           2263
#> 62                NA           18           40  1:20           2320
#> 63                NA           19           36  0:24           2376
#> 64                NA            0            0 20:00           2400
#> 65                NA            0           47 19:13           2447
#> 66         259.00000            0           54 19:06           2454
#> 67          21.33333            1           27 18:33           2487
#> 68                NA            1           49 18:11           2509
#> 69          55.33333            2           20 17:40           2540
#> 70                NA            2           20 17:40           2540
#> 71                NA            3           19 16:41           2599
#> 72          79.66667            3           43 16:17           2623
#> 73                NA            3           43 16:17           2623
#> 74         258.33333            4           12 15:48           2652
#> 75         258.33333            4           12 15:48           2652
#> 76                NA            4           12 15:48           2652
#> 77                NA            4           21 15:39           2661
#> 78         255.33333            4           39 15:21           2679
#> 79                NA            4           39 15:21           2679
#> 80         257.00000            5            6 14:54           2706
#> 81         259.00000            5           20 14:40           2720
#> 82          82.00000            6            2 13:58           2762
#> 83                NA            6            2 13:58           2762
#> 84          37.00000            6           22 13:38           2782
#> 85          65.00000            6           45 13:15           2805
#> 86         239.33333            8           37 11:23           2917
#> 87          45.00000            9           46 10:14           2986
#> 88                NA            9           46 10:14           2986
#> 89                NA           10           18  9:42           3018
#> 90          59.33333           10           34  9:26           3034
#> 91                NA           11           15  8:45           3075
#> 92                NA           11           24  8:36           3084
#> 93         243.00000           13           15  6:45           3195
#> 94                NA           13           15  6:45           3195
#> 95                NA           13           55  6:05           3235
#> 96          82.00000           14           38  5:22           3278
#> 97                NA           14           38  5:22           3278
#> 98          77.33333           15           11  4:49           3311
#> 99                NA           15           32  4:28           3332
#> 100               NA           17           44  2:16           3464
#> 101               NA           18            0  1:00           3480
#> 102        239.00000           18           47  1:13           3527
#> 103         75.00000           19            0  0:00           3540
#> 104               NA            0            0 20:00           3600
#> 105         59.66667            0           25 19:35           3625
#> 106        280.66667            1            5 18:55           3665
#> 107        260.33333            1           22 18:38           3682
#> 108         58.33333            2            0 17:00           3720
#> 109        275.00000            2            9 17:51           3729
#> 110               NA            2            9 17:51           3729
#> 111         69.00000            2           36 17:24           3756
#> 112         69.00000            2           36 17:24           3756
# }
```
