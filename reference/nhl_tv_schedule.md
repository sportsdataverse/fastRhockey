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
#> [1] "2026-11-04"
#> 
#> $startDate
#> [1] "2026-10-21"
#> 
#> $endDate
#> [1] "2026-11-17"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-04T00:00:00 2026-11-04T01:00:00            3600
#> 2  2026-11-04T01:00:00 2026-11-04T02:00:00            3600
#> 3  2026-11-04T02:00:00 2026-11-04T03:00:00            3600
#> 4  2026-11-04T03:00:00 2026-11-04T04:00:00            3600
#> 5  2026-11-04T04:00:00 2026-11-04T05:00:00            3600
#> 6  2026-11-04T05:00:00 2026-11-04T06:00:00            3600
#> 7  2026-11-04T06:00:00 2026-11-04T07:00:00            3600
#> 8  2026-11-04T07:00:00 2026-11-04T08:00:00            3600
#> 9  2026-11-04T08:00:00 2026-11-04T09:00:00            3600
#> 10 2026-11-04T09:00:00 2026-11-04T10:00:00            3600
#> 11 2026-11-04T10:00:00 2026-11-04T11:00:00            3600
#> 12 2026-11-04T11:00:00 2026-11-04T12:00:00            3600
#> 13 2026-11-04T12:00:00 2026-11-04T14:00:00            7200
#> 14 2026-11-04T14:00:00 2026-11-04T16:00:00            7200
#> 15 2026-11-04T16:00:00 2026-11-04T17:00:00            3600
#> 16 2026-11-04T17:00:00 2026-11-04T19:30:00            9000
#> 17 2026-11-04T19:30:00 2026-11-04T20:00:00            1800
#> 18 2026-11-04T20:00:00 2026-11-04T20:30:00            1800
#> 19 2026-11-04T20:30:00 2026-11-04T21:00:00            1800
#> 20 2026-11-04T21:00:00 2026-11-04T21:30:00            1800
#> 21 2026-11-04T21:30:00 2026-11-04T22:00:00            1800
#> 22 2026-11-04T22:00:00 2026-11-04T22:30:00            1800
#> 23 2026-11-04T22:30:00 2026-11-04T23:00:00            1800
#> 24 2026-11-04T23:00:00 2026-11-04T23:30:00            1800
#> 25 2026-11-04T23:30:00 2026-11-05T00:00:00            1800
#>                       title
#> 1                On The Fly
#> 2                On The Fly
#> 3                On The Fly
#> 4                On The Fly
#> 5                On The Fly
#> 6                On The Fly
#> 7                On The Fly
#> 8                On The Fly
#> 9                On The Fly
#> 10               On The Fly
#> 11               On The Fly
#> 12               On The Fly
#> 13                 NHL Game
#> 14                 NHL Game
#> 15 NHL Tonight: First Shift
#> 16                  NHL Now
#> 17                  NHL Now
#> 18                  NHL Now
#> 19                  NHL Now
#> 20                  NHL Now
#> 21                  NHL Now
#> 22                  NHL Now
#> 23               On The Fly
#> 24               On The Fly
#> 25               On The Fly
#>                                                                     description
#> 1                                                                    On The Fly
#> 2                                                                    On The Fly
#> 3                                                                    On The Fly
#> 4                                                                    On The Fly
#> 5                                                                    On The Fly
#> 6                                                                    On The Fly
#> 7                                                                    On The Fly
#> 8                                                                    On The Fly
#> 9                                                                    On The Fly
#> 10                                                                   On The Fly
#> 11                                                                   On The Fly
#> 12                                                                   On The Fly
#> 13 Minnesota Wild at Philadelphia Flyers on 11/3/2026 From Xfinity Mobile Arena
#> 14      Dallas Stars at Washington Capitals on 11/3/2026 From Capital One Arena
#> 15                                                     NHL Tonight: First Shift
#> 16                                                                      NHL Now
#> 17                                                                      NHL Now
#> 18                                                                      NHL Now
#> 19                                                                      NHL Now
#> 20                                                                      NHL Now
#> 21                                                                      NHL Now
#> 22                                                                      NHL Now
#> 23                                                                   On The Fly
#> 24                                                                   On The Fly
#> 25                                                                   On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1    HOTF27R110326SOR            HD            LIVE      onthefly.png
#> 2     HOTF27R110326CC            HD                      onthefly.png
#> 3     HOTF27R110326CC            HD                      onthefly.png
#> 4     HOTF27R110326CC            HD                      onthefly.png
#> 5     HOTF27R110326CC            HD                      onthefly.png
#> 6     HOTF27R110326CC            HD                      onthefly.png
#> 7     HOTF27R110326CC            HD                      onthefly.png
#> 8     HOTF27R110326CC            HD                      onthefly.png
#> 9     HOTF27R110326CC            HD                      onthefly.png
#> 10    HOTF27R110326CC            HD                      onthefly.png
#> 11    HOTF27R110326CC            HD                      onthefly.png
#> 12    HOTF27R110326CC            HD                      onthefly.png
#> 13 H120MINPHI11032026            HD                           nhl.png
#> 14 H120DALWSH11032026            HD                           nhl.png
#> 15  HNHLTFS27110426LV            HD            LIVE                  
#> 16    HNOW27R110426LV            HD            LIVE        nhlnow.png
#> 17    HNOW27R110426CC            HD                        nhlnow.png
#> 18    HNOW27R110426CC            HD                        nhlnow.png
#> 19    HNOW27R110426CC            HD                        nhlnow.png
#> 20    HNOW27R110426CC            HD                        nhlnow.png
#> 21    HNOW27R110426CC            HD                        nhlnow.png
#> 22    HNOW27R110426CC            HD                        nhlnow.png
#> 23   HOTF27R110426LVA            HD            LIVE      onthefly.png
#> 24   HOTF27R110426ACC            HD                      onthefly.png
#> 25   HOTF27R110426ACC            HD                      onthefly.png
#> 
# }
```
