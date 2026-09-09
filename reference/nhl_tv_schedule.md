# **NHL TV Schedule**

Returns the TV schedule for NHL games on a given date.

## Usage

``` r
nhl_tv_schedule(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current.

## Value

A named list of data frames: `broadcasts`.

**broadcasts**

|                   |           |                                             |
|-------------------|-----------|---------------------------------------------|
| col_name          | types     | description                                 |
| startTime         | character | Broadcast start time (UTC).                 |
| endTime           | character | Broadcast end time (UTC).                   |
| durationSeconds   | integer   | Broadcast duration in seconds.              |
| title             | character | Broadcast title.                            |
| description       | character | Broadcast description.                      |
| houseNumber       | character | Internal broadcast house number identifier. |
| broadcastType     | character | Type of broadcast.                          |
| broadcastStatus   | character | Broadcast status.                           |
| broadcastImageUrl | character | URL to the broadcast image.                 |

## Examples

``` r
# \donttest{
  try(nhl_tv_schedule())
#> $date
#> [1] "2026-09-09"
#> 
#> $startDate
#> [1] "2026-08-26"
#> 
#> $endDate
#> [1] "2026-09-23"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-09-09T00:00:00 2026-09-09T01:00:00            3600
#> 2  2026-09-09T01:00:00 2026-09-09T02:00:00            3600
#> 3  2026-09-09T02:00:00 2026-09-09T04:00:00            7200
#> 4  2026-09-09T04:00:00 2026-09-09T05:00:00            3600
#> 5  2026-09-09T05:00:00 2026-09-09T06:00:00            3600
#> 6  2026-09-09T06:00:00 2026-09-09T07:00:00            3600
#> 7  2026-09-09T07:00:00 2026-09-09T08:00:00            3600
#> 8  2026-09-09T08:00:00 2026-09-09T09:00:00            3600
#> 9  2026-09-09T09:00:00 2026-09-09T10:00:00            3600
#> 10 2026-09-09T10:00:00 2026-09-09T11:00:00            3600
#> 11 2026-09-09T11:00:00 2026-09-09T12:00:00            3600
#> 12 2026-09-09T12:00:00 2026-09-09T14:00:00            7200
#> 13 2026-09-09T14:00:00 2026-09-09T16:00:00            7200
#> 14 2026-09-09T16:00:00 2026-09-09T18:00:00            7200
#> 15 2026-09-09T18:00:00 2026-09-09T19:00:00            3600
#> 16 2026-09-09T19:00:00 2026-09-09T20:00:00            3600
#> 17 2026-09-09T20:00:00 2026-09-10T00:00:00           14400
#>                                                                                                     title
#> 1                                                                                             NHL Tonight
#> 2                                                                                             NHL Tonight
#> 3                                                                                                NHL Game
#> 4                                                                                             NHL Tonight
#> 5                                                                                             NHL Tonight
#> 6                                                                                             NHL Tonight
#> 7                                                                                             NHL Tonight
#> 8                                                                                             NHL Tonight
#> 9                                                                                             NHL Tonight
#> 10                                                                                            NHL Tonight
#> 11                                                                                            NHL Tonight
#> 12              Raising The Cup: 2015 Stanley Cup Final Tampa Bay Lightning at Chicago Blackhawks, Game 6
#> 13     Raising The Cup: 2016 Stanley Cup Final Pittsburgh Penguins at San Jose Sharks Game 6 on 6/12/2016
#> 14 Raising The Cup: 2017 Stanley Cup Final Pittsburgh Penguins at Nashville Predators Game 6 on 6/11/2017
#> 15                                                                       Top 50 Players Right Now (50-41)
#> 16                                                                       Top 50 Players Right Now (40-31)
#> 17                                                                                 2026 NHL Draft Round 1
#>                                                                                               description
#> 1                                                                                             NHL Tonight
#> 2                                                                                             NHL Tonight
#> 3                          SuperStar Summer: Bryan Trottier - 1981 Playoffs RD 1 GM 2 NYI @ TOR on 4/9/81
#> 4                                                                                             NHL Tonight
#> 5                                                                                             NHL Tonight
#> 6                                                                                             NHL Tonight
#> 7                                                                                             NHL Tonight
#> 8                                                                                             NHL Tonight
#> 9                                                                                             NHL Tonight
#> 10                                                                                            NHL Tonight
#> 11                                                                                            NHL Tonight
#> 12              Raising The Cup: 2015 Stanley Cup Final Tampa Bay Lightning at Chicago Blackhawks, Game 6
#> 13     Raising The Cup: 2016 Stanley Cup Final Pittsburgh Penguins at San Jose Sharks Game 6 on 6/12/2016
#> 14 Raising The Cup: 2017 Stanley Cup Final Pittsburgh Penguins at Nashville Predators Game 6 on 6/11/2017
#> 15                                                                       Top 50 Players Right Now (50-41)
#> 16                                                                       Top 50 Players Right Now (40-31)
#> 17                                                                                 2026 NHL Draft Round 1
#>               houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1        HNHLTS26090826CC            HD                    nhltonight.png
#> 2        HNHLTS26090826CC            HD                    nhltonight.png
#> 3      H120SSNYITOR040981            HD                           nhl.png
#> 4        HNHLTS26090826CC            HD                    nhltonight.png
#> 5        HNHLTS26090826CC            HD                    nhltonight.png
#> 6        HNHLTS26090826CC            HD                    nhltonight.png
#> 7        HNHLTS26090826CC            HD                    nhltonight.png
#> 8        HNHLTS26090826CC            HD                    nhltonight.png
#> 9        HNHLTS26090826CC            HD                    nhltonight.png
#> 10       HNHLTS26090826CC            HD                    nhltonight.png
#> 11       HNHLTS26090826CC            HD                    nhltonight.png
#> 12    H120TBLCHI061515NBC            HD                           rtc.png
#> 13             HRTC2016CC            HD                           rtc.png
#> 14             HRTC2017CC            HD                           rtc.png
#> 15   H60S26T50PLYRS5041CC            HD                    nhlnetwork.png
#> 16   H60S26T50PLYRS4031CC            HD                    nhlnetwork.png
#> 17 HNHLDFTRD1062626ESPNCC            HD                     2016draft.png
#> 
# }
```
