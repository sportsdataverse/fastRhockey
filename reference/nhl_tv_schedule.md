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
#> [1] "2026-11-06"
#> 
#> $startDate
#> [1] "2026-10-23"
#> 
#> $endDate
#> [1] "2026-11-19"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-06T00:00:00 2026-11-06T00:30:00            1800
#> 2  2026-11-06T00:30:00 2026-11-06T01:30:00            3600
#> 3  2026-11-06T01:30:00 2026-11-06T02:30:00            3600
#> 4  2026-11-06T02:30:00 2026-11-06T03:30:00            3600
#> 5  2026-11-06T03:30:00 2026-11-06T04:00:00            1800
#> 6  2026-11-06T04:00:00 2026-11-06T05:00:00            3600
#> 7  2026-11-06T05:00:00 2026-11-06T06:00:00            3600
#> 8  2026-11-06T06:00:00 2026-11-06T07:00:00            3600
#> 9  2026-11-06T07:00:00 2026-11-06T08:00:00            3600
#> 10 2026-11-06T08:00:00 2026-11-06T09:00:00            3600
#> 11 2026-11-06T09:00:00 2026-11-06T11:30:00            9000
#> 12 2026-11-06T11:30:00 2026-11-06T12:00:00            1800
#> 13 2026-11-06T12:00:00 2026-11-06T13:00:00            3600
#> 14 2026-11-06T13:00:00 2026-11-06T15:30:00            9000
#> 15 2026-11-06T15:30:00 2026-11-06T16:00:00            1800
#> 16 2026-11-06T16:00:00 2026-11-06T17:00:00            3600
#> 17 2026-11-06T17:00:00 2026-11-06T19:00:00            7200
#> 18 2026-11-06T19:00:00 2026-11-06T20:00:00            3600
#> 19 2026-11-06T20:00:00 2026-11-06T23:00:00           10800
#> 20 2026-11-06T23:00:00 2026-11-06T23:30:00            1800
#> 21 2026-11-06T23:30:00 2026-11-07T00:00:00            1800
#>                                                                          title
#> 1                                                                   On The Fly
#> 2                                                                   On The Fly
#> 3                                                                   On The Fly
#> 4                                                                   On The Fly
#> 5                                                                   On The Fly
#> 6                                                                   On The Fly
#> 7                                                                   On The Fly
#> 8                                                                   On The Fly
#> 9                                                                   On The Fly
#> 10                                                                  On The Fly
#> 11                                             IIHF Women's World Championship
#> 12                                   Breaking Down Barriers: S3 - Bench Bosses
#> 13                                                                  On The Fly
#> 14                                             IIHF Women's World Championship
#> 15 Breaking Down Barriers: A New Era Unveiled: The PWHL's Groundbreaking Debut
#> 16                                                    NHL Tonight: First Shift
#> 17                                                                     NHL Now
#> 18                                                                 NHL Tonight
#> 19                                        Regular Season Hockey on NHL Network
#> 20                                                                  On The Fly
#> 21                                                                  On The Fly
#>                                                                                                                                                                                                                                                                                                                                                                                                      description
#> 1                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 2                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                     On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                             2026 IIHF Women's World Championship: Sweden vs Canada on 11/6/26
#> 12                                                                                                                                                                                          The story of Jessica Campbell and other pioneering women who are breaking the coaching glass ceiling in hockey, from grassroots to professional levels, proving that the future of coaching is diverse and inclusive
#> 13                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 14                                                                                                                                                                                                                                                                                                                                           2026 IIHF Women's World Championship: Switzerland vs USA on 11/6/26
#> 15 "BREAKING DOWN BARRIERS - A NEW ERA UNVEILED" promises an exclusive and in-depth exploration of the launch of the inaugural season of the Professional Women's Hockey League. The episode explores the challenges faced, the sacrifices made, the palpable excitement felt in each city in the league, and the meticulous preparations that have culminated in a historic season opener on January 1st, 2024.
#> 16                                                                                                                                                                                                                                                                                                                                                                                      NHL Tonight: First Shift
#> 17                                                                                                                                                                                                                                                                                                                                                                                                       NHL Now
#> 18                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight
#> 19                                                                                                                                                                                                                                                                                                                                        San Jose Sharks at Minnesota Wild on 11/6/2026 From Grand Casino Arena
#> 20                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 21                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#>             houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1      HOTF27R110526ACC            HD                      onthefly.png
#> 2      HOTF27R110526SOR            HD            LIVE      onthefly.png
#> 3       HOTF27R110526CC            HD                      onthefly.png
#> 4       HOTF27R110526CC            HD                      onthefly.png
#> 5       HOTF27R110526CC            HD                      onthefly.png
#> 6       HOTF27R110526CC            HD                      onthefly.png
#> 7       HOTF27R110526CC            HD                      onthefly.png
#> 8       HOTF27R110526CC            HD                      onthefly.png
#> 9       HOTF27R110526CC            HD                      onthefly.png
#> 10      HOTF27R110526CC            HD                      onthefly.png
#> 11 H150WW26SWECAN110626            HD            LIVE    nhlnetwork.png
#> 12        HTSNBDBS3EP06            HD                    nhlnetwork.png
#> 13      HOTF27R110526CC            HD                      onthefly.png
#> 14 H150WW26SUIUSA110626            HD            LIVE    nhlnetwork.png
#> 15          HTSNBDBEP13            HD                    nhlnetwork.png
#> 16    HNHLTFS27110626LV            HD            LIVE                  
#> 17      HNOW27R110626CC            HD            LIVE        nhlnow.png
#> 18    HNHLT27RS110626LV            HD            LIVE    nhltonight.png
#> 19   H180SJSMIN11062026            HD            LIVE           nhl.png
#> 20     HOTF27R110626SOR            HD            LIVE      onthefly.png
#> 21      HOTF27R110626CC            HD                      onthefly.png
#> 
# }
```
