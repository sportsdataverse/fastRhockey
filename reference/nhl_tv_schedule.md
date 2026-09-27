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
#> [1] "2026-09-27"
#> 
#> $startDate
#> [1] "2026-09-13"
#> 
#> $endDate
#> [1] "2026-10-10"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-09-27T01:00:00 2026-09-27T02:00:00            3600
#> 2  2026-09-27T02:00:00 2026-09-27T04:00:00            7200
#> 3  2026-09-27T04:00:00 2026-09-27T05:00:00            3600
#> 4  2026-09-27T05:00:00 2026-09-27T06:00:00            3600
#> 5  2026-09-27T06:00:00 2026-09-27T08:00:00            7200
#> 6  2026-09-27T08:00:00 2026-09-27T08:30:00            1800
#> 7  2026-09-27T08:30:00 2026-09-27T09:00:00            1800
#> 8  2026-09-27T09:00:00 2026-09-27T09:30:00            1800
#> 9  2026-09-27T09:30:00 2026-09-27T10:00:00            1800
#> 10 2026-09-27T10:00:00 2026-09-27T11:00:00            3600
#> 11 2026-09-27T11:00:00 2026-09-27T13:00:00            7200
#> 12 2026-09-27T13:00:00 2026-09-27T15:00:00            7200
#> 13 2026-09-27T15:00:00 2026-09-27T16:00:00            3600
#> 14 2026-09-27T16:00:00 2026-09-27T17:00:00            3600
#> 15 2026-09-27T17:00:00 2026-09-27T18:00:00            3600
#> 16 2026-09-27T18:00:00 2026-09-27T19:00:00            3600
#> 17 2026-09-27T19:00:00 2026-09-27T20:00:00            3600
#> 18 2026-09-27T20:00:00 2026-09-27T21:00:00            3600
#> 19 2026-09-27T21:00:00 2026-09-27T22:00:00            3600
#> 20 2026-09-27T22:00:00 2026-09-27T23:00:00            3600
#> 21 2026-09-27T23:00:00 2026-09-28T00:00:00            3600
#>                                                            title
#> 1  NHL Network Countdown: 50 Most Bizarre Moments in NHL History
#> 2                                                       NHL Game
#> 3                               Top 50 Players Right Now (30-21)
#> 4                               Top 50 Players Right Now (20-11)
#> 5                                                       NHL Game
#> 6       Breaking Down Barriers: S3 - The Future Of Girls' Hockey
#> 7          Breaking Down Barriers: S3 - Best Of The Season PT. 1
#> 8                          Breaking Down Barriers: S3- Connected
#> 9                      Breaking Down Barriers: S3 - First Assist
#> 10                                      Top 10 Goalies Right Now
#> 11                                                      NHL Game
#> 12                                                      NHL Game
#> 13                              Top 50 Players Right Now (50-41)
#> 14                              Top 50 Players Right Now (40-31)
#> 15                              Top 50 Players Right Now (30-21)
#> 16                              Top 50 Players Right Now (20-11)
#> 17                               Top 50 Players Right Now (10-1)
#> 18                                NHL Tonight: Analytics Special
#> 19                               Top 50 Players Right Now (10-1)
#> 20                                NHL Tonight: Analytics Special
#> 21                               Top 50 Players Right Now (10-1)
#>                                                                                                                                                                                                                      description
#> 1                                                                                                                                                                  NHL Network Countdown: 50 Most Bizarre Moments in NHL History
#> 2                                                                                                                                                         Pittsburgh Penguins at Buffalo Sabres on 9/26/2026 From KeyBank Center
#> 3                                                                                                                                                                                               Top 50 Players Right Now (30-21)
#> 4                                                                                                                                                                                               Top 50 Players Right Now (20-11)
#> 5                                                                                                                                                          St. Louis Blues at Chicago Blackhawks on 9/26/2026 From United Center
#> 6                                                                     Exploring the challenges girls and women face in hockey, including ice time and retention issues, while highlighting Gillian Apps' leadership in the sport
#> 7                                                                                                                                                     A countdown of the top hockey moments in BREAKING DOWN BARRIERS Season 03.
#> 8                              The PWHL showcases its connection to Indigenous communities by creating a trip of a lifetime for a group of young female hockey players from remote Nain, Newfoundland, to attend a playoff game.
#> 9  First Assist, founded by former NHL player John Chabot, aims to help Indigenous students succeed in school by using sports as a motivational tool to boost attendance, classroom engagement, and promote healthy life habits.
#> 10                                                                                                                                                                                                      Top 10 Goalies Right Now
#> 11                                                                                                                                                      San Jose Sharks at Vegas Golden Knights on 9/26/2026 From T-Mobile Arena
#> 12                                                                                                                                                Carolina Hurricanes at Nashville Predators on 9/26/2026 From Bridgestone Arena
#> 13                                                                                                                                                                                              Top 50 Players Right Now (50-41)
#> 14                                                                                                                                                                                              Top 50 Players Right Now (40-31)
#> 15                                                                                                                                                                                              Top 50 Players Right Now (30-21)
#> 16                                                                                                                                                                                              Top 50 Players Right Now (20-11)
#> 17                                                                                                                                                                                               Top 50 Players Right Now (10-1)
#> 18                                                                                                                                                                                                NHL Tonight: Analytics Special
#> 19                                                                                                                                                                                               Top 50 Players Right Now (10-1)
#> 20                                                                                                                                                                                                NHL Tonight: Analytics Special
#> 21                                                                                                                                                                                               Top 50 Players Right Now (10-1)
#>             houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1           HNHLNCTDWN8            HD                 nhlncountdown.png
#> 2    H120PITBUF09262026            HD                           nhl.png
#> 3  H60S26T50PLYRS3021CC            HD                    nhlnetwork.png
#> 4  H60S26T50PLYRS2011CC            HD                    nhlnetwork.png
#> 5    H120STLCHI09262026            HD                           nhl.png
#> 6         HTSNBDBS3EP08            HD                    nhlnetwork.png
#> 7         HTSNBDBS3EP09            HD                    nhlnetwork.png
#> 8         HTSNBDBS3EP01            HD                    nhlnetwork.png
#> 9         HTSNBDBS3EP02            HD                    nhlnetwork.png
#> 10    H60S26T10GOALRNCC            HD                    nhlnetwork.png
#> 11   H120SJSVGK09262026            HD                           nhl.png
#> 12   H120CARNSH09262026            HD                           nhl.png
#> 13 H60S26T50PLYRS5041CC            HD                    nhlnetwork.png
#> 14 H60S26T50PLYRS4031CC            HD                    nhlnetwork.png
#> 15 H60S26T50PLYRS3021CC            HD                    nhlnetwork.png
#> 16 H60S26T50PLYRS2011CC            HD                    nhlnetwork.png
#> 17  H60S26T50PLYRS101PT            HD                    nhlnetwork.png
#> 18     HNHLTS26AYSPECPT            HD                    nhltonight.png
#> 19  H60S26T50PLYRS101CC            HD                    nhlnetwork.png
#> 20     HNHLTS26AYSPECCC            HD                    nhltonight.png
#> 21  H60S26T50PLYRS101CC            HD                    nhlnetwork.png
#> 
# }
```
