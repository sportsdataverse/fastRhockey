# **PWHL Schedule**

PWHL Schedule lookup

## Usage

``` r
pwhl_schedule(season, game_type = "both")
```

## Arguments

- season:

  Season (YYYY) to pull the schedule from, the concluding year in
  XXXX-YY format

- game_type:

  Game type: `"both"` (default), `"regular"`, `"playoffs"`, or
  `"preseason"`. When `"both"`, regular-season and playoff games for the
  season are combined into a single, chronologically ordered data frame
  (preseason is excluded from `"both"` – request it explicitly with
  `game_type = "preseason"`).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|              |           |                                  |
|--------------|-----------|----------------------------------|
| col_name     | types     | description                      |
| game_id      | character | Unique game identifier.          |
| season       | numeric   | Season (concluding year, YYYY).  |
| game_date    | character | Game date.                       |
| game_status  | character | Status of the game.              |
| home_team    | character | Home team name.                  |
| home_team_id | character | Home team identifier.            |
| away_team    | character | Away team name.                  |
| away_team_id | character | Away team identifier.            |
| home_score   | character | Home team score.                 |
| away_score   | character | Away team score.                 |
| winner       | character | Winning team.                    |
| venue        | character | Venue where the game was played. |
| venue_url    | character | URL for the venue.               |
| game_type    | character | Game type the row belongs to.    |

## Examples

``` r
# \donttest{
  try(pwhl_schedule(season = 2024))
#>    game_id season   game_date game_status home_team home_team_id away_team
#> 1        2   2024  Mon, Jan 1       Final   Toronto            6  New York
#> 2        3   2024  Tue, Jan 2    Final OT    Ottawa            5  Montreal
#> 3        4   2024  Wed, Jan 3       Final    Boston            1 Minnesota
#> 4        5   2024  Fri, Jan 5       Final  New York            4   Toronto
#> 5        6   2024  Sat, Jan 6       Final Minnesota            2  Montreal
#> 6        8   2024 Wed, Jan 10       Final Minnesota            2   Toronto
#> 7        9   2024 Wed, Jan 10       Final  New York            4  Montreal
#> 8       10   2024 Sat, Jan 13       Final   Toronto            6    Ottawa
#> 9       11   2024 Sat, Jan 13    Final OT  Montreal            3    Boston
#> 10      12   2024 Sun, Jan 14    Final OT Minnesota            2  New York
#> 11      13   2024 Tue, Jan 16       Final  Montreal            3  New York
#> 12      14   2024 Wed, Jan 17       Final   Toronto            6    Boston
#> 13      15   2024 Wed, Jan 17    Final OT    Ottawa            5 Minnesota
#> 14      16   2024 Sat, Jan 20       Final    Boston            1  New York
#> 15      17   2024 Sat, Jan 20    Final SO  Montreal            3   Toronto
#> 16      18   2024 Tue, Jan 23       Final    Ottawa            5   Toronto
#> 17      19   2024 Wed, Jan 24       Final    Ottawa            5    Boston
#> 18      20   2024 Wed, Jan 24       Final Minnesota            2  Montreal
#> 19      21   2024 Fri, Jan 26       Final   Toronto            6  New York
#> 20      22   2024 Sat, Jan 27    Final OT  Montreal            3    Ottawa
#> 21      23   2024 Sat, Jan 27    Final OT    Boston            1 Minnesota
#> 22      24   2024 Sun, Jan 28    Final OT  New York            4 Minnesota
#> 23      25   2024  Sat, Feb 3       Final   Toronto            6 Minnesota
#> 24      26   2024  Sun, Feb 4    Final OT    Ottawa            5  New York
#> 25      27   2024  Sun, Feb 4    Final OT    Boston            1  Montreal
#> 26      28   2024 Wed, Feb 14       Final Minnesota            2    Ottawa
#> 27      29   2024 Wed, Feb 14       Final    Boston            1   Toronto
#> 28      30   2024 Fri, Feb 16       Final   Toronto            6  Montreal
#> 29      31   2024 Sat, Feb 17       Final    Ottawa            5 Minnesota
#> 30      32   2024 Sat, Feb 17    Final OT    Boston            1  New York
#> 31      33   2024 Sun, Feb 18       Final  Montreal            3 Minnesota
#> 32      83   2024 Mon, Feb 19       Final    Boston            1    Ottawa
#> 33      34   2024 Wed, Feb 21       Final    Boston            1    Ottawa
#> 34      35   2024 Wed, Feb 21    Final SO  New York            4  Montreal
#> 35      36   2024 Fri, Feb 23    Final SO   Toronto            6  New York
#> 36      37   2024 Sat, Feb 24       Final  Montreal            3    Ottawa
#> 37      38   2024 Sun, Feb 25       Final Minnesota            2    Boston
#> 38      40   2024 Tue, Feb 27    Final OT Minnesota            2   Toronto
#> 39      39   2024 Wed, Feb 28       Final    Ottawa            5  New York
#> 40      41   2024  Sat, Mar 2       Final    Ottawa            5   Toronto
#> 41      42   2024  Sat, Mar 2       Final  Montreal            3    Boston
#> 42      43   2024  Sun, Mar 3       Final  New York            4 Minnesota
#> 43      44   2024  Tue, Mar 5    Final SO Minnesota            2    Ottawa
#> 44      45   2024  Wed, Mar 6       Final   Toronto            6    Boston
#> 45      46   2024  Wed, Mar 6       Final  New York            4  Montreal
#> 46      47   2024  Fri, Mar 8       Final   Toronto            6  Montreal
#> 47      48   2024 Sun, Mar 10    Final OT    Boston            1  New York
#> 48      49   2024 Sun, Mar 10       Final  Montreal            3    Ottawa
#> 49      50   2024 Wed, Mar 13       Final Minnesota            2    Boston
#> 50      51   2024 Sat, Mar 16       Final Minnesota            2  New York
#> 51      65   2024 Sat, Mar 16    Final SO    Boston            1    Ottawa
#> 52      66   2024 Sun, Mar 17       Final  Montreal            3   Toronto
#> 53      52   2024 Wed, Mar 20       Final   Toronto            6    Boston
#> 54      53   2024 Wed, Mar 20       Final  New York            4    Ottawa
#> 55      54   2024 Sat, Mar 23       Final    Ottawa            5   Toronto
#> 56      55   2024 Sun, Mar 24    Final SO Minnesota            2  Montreal
#> 57      56   2024 Mon, Mar 25       Final  New York            4    Boston
#> 58      57   2024 Thu, Apr 18       Final    Boston            1   Toronto
#> 59      58   2024 Thu, Apr 18       Final  Montreal            3 Minnesota
#> 60      59   2024 Sat, Apr 20       Final    Ottawa            5 Minnesota
#> 61      67   2024 Sat, Apr 20    Final OT  Montreal            3   Toronto
#> 62      68   2024 Sat, Apr 20       Final  New York            4    Boston
#> 63      60   2024 Wed, Apr 24       Final  Montreal            3  New York
#> 64      69   2024 Wed, Apr 24    Final SO    Ottawa            5    Boston
#> 65      62   2024 Sat, Apr 27       Final    Ottawa            5  Montreal
#> 66      70   2024 Sat, Apr 27       Final Minnesota            2    Boston
#> 67      61   2024 Sun, Apr 28       Final  New York            4   Toronto
#> 68      71   2024 Tue, Apr 30       Final  New York            4    Ottawa
#> 69      63   2024  Wed, May 1       Final   Toronto            6 Minnesota
#> 70      64   2024  Sat, May 4       Final    Boston            1  Montreal
#> 71      73   2024  Sat, May 4       Final  New York            4 Minnesota
#> 72      72   2024  Sun, May 5       Final   Toronto            6    Ottawa
#> 73      84   2024  Wed, May 8       Final   Toronto            6 Minnesota
#> 74      85   2024  Thu, May 9    Final OT  Montreal            3    Boston
#> 75      86   2024 Fri, May 10       Final   Toronto            6 Minnesota
#> 76      87   2024 Sat, May 11   Final OT3  Montreal            3    Boston
#> 77      88   2024 Mon, May 13       Final Minnesota            2   Toronto
#> 78      89   2024 Tue, May 14    Final OT    Boston            1  Montreal
#> 79      90   2024 Wed, May 15   Final OT2 Minnesota            2   Toronto
#> 80      92   2024 Fri, May 17       Final   Toronto            6 Minnesota
#> 81      94   2024 Sun, May 19       Final    Boston            1 Minnesota
#> 82      95   2024 Tue, May 21       Final    Boston            1 Minnesota
#> 83      96   2024 Fri, May 24       Final Minnesota            2    Boston
#> 84      97   2024 Sun, May 26   Final OT2 Minnesota            2    Boston
#> 85      98   2024 Wed, May 29       Final    Boston            1 Minnesota
#>    away_team_id home_score away_score    winner
#> 1             4          0          4  New York
#> 2             3          2          3  Montreal
#> 3             2          2          3 Minnesota
#> 4             6          2          3   Toronto
#> 5             3          3          0 Minnesota
#> 6             6          3          1 Minnesota
#> 7             3          2          5  Montreal
#> 8             5          1          5    Ottawa
#> 9             1          2          3    Boston
#> 10            4          2          3  New York
#> 11            4          3          2  Montreal
#> 12            1          2          3    Boston
#> 13            2          2          3 Minnesota
#> 14            4          1          4  New York
#> 15            6          3          4   Toronto
#> 16            6          3          1    Ottawa
#> 17            1          2          3    Boston
#> 18            3          1          2  Montreal
#> 19            4          2          0   Toronto
#> 20            5          2          1  Montreal
#> 21            2          4          3    Boston
#> 22            2          1          2 Minnesota
#> 23            2          4          1   Toronto
#> 24            4          3          4  New York
#> 25            3          1          2  Montreal
#> 26            5          2          1 Minnesota
#> 27            6          3          5   Toronto
#> 28            3          3          0   Toronto
#> 29            2          1          2 Minnesota
#> 30            4          1          2  New York
#> 31            2          2          1  Montreal
#> 32            5          2          4    Ottawa
#> 33            5          3          1    Boston
#> 34            3          3          2  New York
#> 35            4          2          1   Toronto
#> 36            5          6          3  Montreal
#> 37            1          0          2    Boston
#> 38            6          3          4   Toronto
#> 39            4          4          2    Ottawa
#> 40            6          2          5   Toronto
#> 41            1          3          1  Montreal
#> 42            2          0          2 Minnesota
#> 43            5          4          3 Minnesota
#> 44            1          3          1   Toronto
#> 45            3          3          4  Montreal
#> 46            3          3          0   Toronto
#> 47            4          3          2    Boston
#> 48            5          2          4    Ottawa
#> 49            1          4          0 Minnesota
#> 50            4          5          1 Minnesota
#> 51            5          2          1    Boston
#> 52            6          1          2   Toronto
#> 53            1          2          1   Toronto
#> 54            5          0          3    Ottawa
#> 55            6          5          3    Ottawa
#> 56            3          3          2 Minnesota
#> 57            1          3          2  New York
#> 58            6          2          1    Boston
#> 59            2          4          3  Montreal
#> 60            2          4          0    Ottawa
#> 61            6          2          3   Toronto
#> 62            1          1          2    Boston
#> 63            4          5          2  Montreal
#> 64            1          3          2    Ottawa
#> 65            3          0          2  Montreal
#> 66            1          1          2    Boston
#> 67            6          2          6   Toronto
#> 68            5          4          3  New York
#> 69            2          4          1   Toronto
#> 70            3          4          3    Boston
#> 71            2          5          2  New York
#> 72            5          5          2   Toronto
#> 73            2          4          0   Toronto
#> 74            1          1          2    Boston
#> 75            2          2          0   Toronto
#> 76            1          1          2    Boston
#> 77            6          2          0 Minnesota
#> 78            3          3          2    Boston
#> 79            6          1          0 Minnesota
#> 80            2          1          4 Minnesota
#> 81            2          4          3    Boston
#> 82            2          0          3 Minnesota
#> 83            1          4          1 Minnesota
#> 84            1          0          1    Boston
#> 85            2          0          3 Minnesota
#>                                 venue
#> 1   Mattamy Athletic Centre | Toronto
#> 2                   TD Place | Ottawa
#> 3             Tsongas Center | Lowell
#> 4   Total Mortgage Arena | Bridgeport
#> 5       Xcel Energy Center | St. Paul
#> 6       Xcel Energy Center | St. Paul
#> 7                  UBS Arena | Elmont
#> 8   Mattamy Athletic Centre | Toronto
#> 9        Verdun Auditorium | Montreal
#> 10      Xcel Energy Center | St. Paul
#> 11                 Place Bell | Laval
#> 12  Mattamy Athletic Centre | Toronto
#> 13                  TD Place | Ottawa
#> 14            Tsongas Center | Lowell
#> 15       Verdun Auditorium | Montreal
#> 16                  TD Place | Ottawa
#> 17                  TD Place | Ottawa
#> 18      Xcel Energy Center | St. Paul
#> 19  Mattamy Athletic Centre | Toronto
#> 20                 Place Bell | Laval
#> 21            Tsongas Center | Lowell
#> 22  Total Mortgage Arena | Bridgeport
#> 23  Mattamy Athletic Centre | Toronto
#> 24                  TD Place | Ottawa
#> 25            Tsongas Center | Lowell
#> 26      Xcel Energy Center | St. Paul
#> 27            Tsongas Center | Lowell
#> 28         Scotiabank Arena | Toronto
#> 29                  TD Place | Ottawa
#> 30            Tsongas Center | Lowell
#> 31                 Place Bell | Laval
#> 32            Tsongas Center | Lowell
#> 33            Tsongas Center | Lowell
#> 34                 UBS Arena | Elmont
#> 35  Mattamy Athletic Centre | Toronto
#> 36       Verdun Auditorium | Montreal
#> 37      Xcel Energy Center | St. Paul
#> 38 3M Arena at Mariucci | Minneapolis
#> 39                  TD Place | Ottawa
#> 40                  TD Place | Ottawa
#> 41       Verdun Auditorium | Montreal
#> 42                 UBS Arena | Elmont
#> 43      Xcel Energy Center | St. Paul
#> 44  Mattamy Athletic Centre | Toronto
#> 45  Total Mortgage Arena | Bridgeport
#> 46  Mattamy Athletic Centre | Toronto
#> 47            Tsongas Center | Lowell
#> 48                 Place Bell | Laval
#> 49      Xcel Energy Center | St. Paul
#> 50      Xcel Energy Center | St. Paul
#> 51     Little Caesars Arena | Detroit
#> 52      PPG Paints Arena | Pittsburgh
#> 53  Mattamy Athletic Centre | Toronto
#> 54  Total Mortgage Arena | Bridgeport
#> 55                  TD Place | Ottawa
#> 56      Xcel Energy Center | St. Paul
#> 57                 UBS Arena | Elmont
#> 58            Tsongas Center | Lowell
#> 59       Verdun Auditorium | Montreal
#> 60                  TD Place | Ottawa
#> 61             Bell Centre | Montréal
#> 62         Prudential Center | Newark
#> 63       Verdun Auditorium | Montreal
#> 64                  TD Place | Ottawa
#> 65                  TD Place | Ottawa
#> 66      Xcel Energy Center | St. Paul
#> 67                 UBS Arena | Elmont
#> 68         Prudential Center | Newark
#> 69  Mattamy Athletic Centre | Toronto
#> 70            Tsongas Center | Lowell
#> 71                 UBS Arena | Elmont
#> 72  Mattamy Athletic Centre | Toronto
#> 73       Coca-Cola Coliseum | Toronto
#> 74                 Place Bell | Laval
#> 75       Coca-Cola Coliseum | Toronto
#> 76                 Place Bell | Laval
#> 77      Xcel Energy Center | St. Paul
#> 78            Tsongas Center | Lowell
#> 79      Xcel Energy Center | St. Paul
#> 80       Coca-Cola Coliseum | Toronto
#> 81            Tsongas Center | Lowell
#> 82            Tsongas Center | Lowell
#> 83      Xcel Energy Center | St. Paul
#> 84      Xcel Energy Center | St. Paul
#> 85            Tsongas Center | Lowell
#>                                                 venue_url game_type
#> 1                   https://www.mattamyathleticcentre.ca/   regular
#> 2                                 https://www.tdplace.ca/   regular
#> 3                              https://tsongascenter.com/   regular
#> 4                     https://www.totalmortgagearena.com/   regular
#> 5                       https://www.xcelenergycenter.com/   regular
#> 6                       https://www.xcelenergycenter.com/   regular
#> 7                                   https://ubsarena.com/   regular
#> 8                   https://www.mattamyathleticcentre.ca/   regular
#> 9      https://montreal.ca/en/places/auditorium-de-verdun   regular
#> 10                      https://www.xcelenergycenter.com/   regular
#> 11                                https://placebell.ca/en   regular
#> 12                  https://www.mattamyathleticcentre.ca/   regular
#> 13                                https://www.tdplace.ca/   regular
#> 14                             https://tsongascenter.com/   regular
#> 15     https://montreal.ca/en/places/auditorium-de-verdun   regular
#> 16                                https://www.tdplace.ca/   regular
#> 17                                https://www.tdplace.ca/   regular
#> 18                      https://www.xcelenergycenter.com/   regular
#> 19                  https://www.mattamyathleticcentre.ca/   regular
#> 20                                https://placebell.ca/en   regular
#> 21                             https://tsongascenter.com/   regular
#> 22                    https://www.totalmortgagearena.com/   regular
#> 23                  https://www.mattamyathleticcentre.ca/   regular
#> 24                                https://www.tdplace.ca/   regular
#> 25                             https://tsongascenter.com/   regular
#> 26                      https://www.xcelenergycenter.com/   regular
#> 27                             https://tsongascenter.com/   regular
#> 28                                                    TBD   regular
#> 29                                https://www.tdplace.ca/   regular
#> 30                             https://tsongascenter.com/   regular
#> 31                                https://placebell.ca/en   regular
#> 32                             https://tsongascenter.com/   regular
#> 33                             https://tsongascenter.com/   regular
#> 34                                  https://ubsarena.com/   regular
#> 35                  https://www.mattamyathleticcentre.ca/   regular
#> 36     https://montreal.ca/en/places/auditorium-de-verdun   regular
#> 37                      https://www.xcelenergycenter.com/   regular
#> 38                                                    TBD   regular
#> 39                                https://www.tdplace.ca/   regular
#> 40                                https://www.tdplace.ca/   regular
#> 41     https://montreal.ca/en/places/auditorium-de-verdun   regular
#> 42                                  https://ubsarena.com/   regular
#> 43                      https://www.xcelenergycenter.com/   regular
#> 44                  https://www.mattamyathleticcentre.ca/   regular
#> 45                    https://www.totalmortgagearena.com/   regular
#> 46                  https://www.mattamyathleticcentre.ca/   regular
#> 47                             https://tsongascenter.com/   regular
#> 48                                https://placebell.ca/en   regular
#> 49                      https://www.xcelenergycenter.com/   regular
#> 50                      https://www.xcelenergycenter.com/   regular
#> 51 https://www.313presents.com/venue/little-caesars-arena   regular
#> 52                        https://www.ppgpaintsarena.com/   regular
#> 53                  https://www.mattamyathleticcentre.ca/   regular
#> 54                    https://www.totalmortgagearena.com/   regular
#> 55                                https://www.tdplace.ca/   regular
#> 56                      https://www.xcelenergycenter.com/   regular
#> 57                                  https://ubsarena.com/   regular
#> 58                             https://tsongascenter.com/   regular
#> 59     https://montreal.ca/en/places/auditorium-de-verdun   regular
#> 60                                https://www.tdplace.ca/   regular
#> 61                                                    TBD   regular
#> 62                                                    TBD   regular
#> 63     https://montreal.ca/en/places/auditorium-de-verdun   regular
#> 64                                https://www.tdplace.ca/   regular
#> 65                                https://www.tdplace.ca/   regular
#> 66                      https://www.xcelenergycenter.com/   regular
#> 67                                  https://ubsarena.com/   regular
#> 68                                                    TBD   regular
#> 69                  https://www.mattamyathleticcentre.ca/   regular
#> 70                             https://tsongascenter.com/   regular
#> 71                                  https://ubsarena.com/   regular
#> 72                  https://www.mattamyathleticcentre.ca/   regular
#> 73                     https://www.coca-colacoliseum.com/  playoffs
#> 74                                https://placebell.ca/en  playoffs
#> 75                     https://www.coca-colacoliseum.com/  playoffs
#> 76                                https://placebell.ca/en  playoffs
#> 77                      https://www.xcelenergycenter.com/  playoffs
#> 78                             https://tsongascenter.com/  playoffs
#> 79                      https://www.xcelenergycenter.com/  playoffs
#> 80                     https://www.coca-colacoliseum.com/  playoffs
#> 81                             https://tsongascenter.com/  playoffs
#> 82                             https://tsongascenter.com/  playoffs
#> 83                      https://www.xcelenergycenter.com/  playoffs
#> 84                      https://www.xcelenergycenter.com/  playoffs
#> 85                             https://tsongascenter.com/  playoffs
  try(pwhl_schedule(season = 2024, game_type = "playoffs"))
#>    game_id season   game_date game_status home_team home_team_id away_team
#> 1       84   2024  Wed, May 8       Final   Toronto            6 Minnesota
#> 2       85   2024  Thu, May 9    Final OT  Montreal            3    Boston
#> 3       86   2024 Fri, May 10       Final   Toronto            6 Minnesota
#> 4       87   2024 Sat, May 11   Final OT3  Montreal            3    Boston
#> 5       88   2024 Mon, May 13       Final Minnesota            2   Toronto
#> 6       89   2024 Tue, May 14    Final OT    Boston            1  Montreal
#> 7       90   2024 Wed, May 15   Final OT2 Minnesota            2   Toronto
#> 8       92   2024 Fri, May 17       Final   Toronto            6 Minnesota
#> 9       94   2024 Sun, May 19       Final    Boston            1 Minnesota
#> 10      95   2024 Tue, May 21       Final    Boston            1 Minnesota
#> 11      96   2024 Fri, May 24       Final Minnesota            2    Boston
#> 12      97   2024 Sun, May 26   Final OT2 Minnesota            2    Boston
#> 13      98   2024 Wed, May 29       Final    Boston            1 Minnesota
#>    away_team_id home_score away_score    winner                         venue
#> 1             2          4          0   Toronto  Coca-Cola Coliseum | Toronto
#> 2             1          1          2    Boston            Place Bell | Laval
#> 3             2          2          0   Toronto  Coca-Cola Coliseum | Toronto
#> 4             1          1          2    Boston            Place Bell | Laval
#> 5             6          2          0 Minnesota Xcel Energy Center | St. Paul
#> 6             3          3          2    Boston       Tsongas Center | Lowell
#> 7             6          1          0 Minnesota Xcel Energy Center | St. Paul
#> 8             2          1          4 Minnesota  Coca-Cola Coliseum | Toronto
#> 9             2          4          3    Boston       Tsongas Center | Lowell
#> 10            2          0          3 Minnesota       Tsongas Center | Lowell
#> 11            1          4          1 Minnesota Xcel Energy Center | St. Paul
#> 12            1          0          1    Boston Xcel Energy Center | St. Paul
#> 13            2          0          3 Minnesota       Tsongas Center | Lowell
#>                             venue_url game_type
#> 1  https://www.coca-colacoliseum.com/  playoffs
#> 2             https://placebell.ca/en  playoffs
#> 3  https://www.coca-colacoliseum.com/  playoffs
#> 4             https://placebell.ca/en  playoffs
#> 5   https://www.xcelenergycenter.com/  playoffs
#> 6          https://tsongascenter.com/  playoffs
#> 7   https://www.xcelenergycenter.com/  playoffs
#> 8  https://www.coca-colacoliseum.com/  playoffs
#> 9          https://tsongascenter.com/  playoffs
#> 10         https://tsongascenter.com/  playoffs
#> 11  https://www.xcelenergycenter.com/  playoffs
#> 12  https://www.xcelenergycenter.com/  playoffs
#> 13         https://tsongascenter.com/  playoffs
# }
```
