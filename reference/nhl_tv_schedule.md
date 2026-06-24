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
#> [1] "2026-06-24"
#> 
#> $startDate
#> [1] "2026-06-10"
#> 
#> $endDate
#> [1] "2026-07-07"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-06-24T00:00:00 2026-06-24T01:00:00            3600
#> 2  2026-06-24T01:00:00 2026-06-24T02:00:00            3600
#> 3  2026-06-24T02:00:00 2026-06-24T03:00:00            3600
#> 4  2026-06-24T03:00:00 2026-06-24T04:00:00            3600
#> 5  2026-06-24T04:00:00 2026-06-24T05:00:00            3600
#> 6  2026-06-24T05:00:00 2026-06-24T06:00:00            3600
#> 7  2026-06-24T06:00:00 2026-06-24T07:00:00            3600
#> 8  2026-06-24T07:00:00 2026-06-24T08:00:00            3600
#> 9  2026-06-24T08:00:00 2026-06-24T09:00:00            3600
#> 10 2026-06-24T09:00:00 2026-06-24T10:00:00            3600
#> 11 2026-06-24T10:00:00 2026-06-24T11:00:00            3600
#> 12 2026-06-24T11:00:00 2026-06-24T12:00:00            3600
#> 13 2026-06-24T12:00:00 2026-06-24T13:00:00            3600
#> 14 2026-06-24T13:00:00 2026-06-24T14:00:00            3600
#> 15 2026-06-24T14:00:00 2026-06-24T16:00:00            7200
#> 16 2026-06-24T16:00:00 2026-06-24T18:00:00            7200
#> 17 2026-06-24T18:00:00 2026-06-24T19:00:00            3600
#> 18 2026-06-24T19:00:00 2026-06-24T20:00:00            3600
#> 19 2026-06-24T20:00:00 2026-06-24T21:00:00            3600
#> 20 2026-06-24T21:00:00 2026-06-24T23:00:00            7200
#> 21 2026-06-24T21:30:00 2026-06-24T22:00:00            1800
#> 22 2026-06-24T22:00:00 2026-06-24T22:30:00            1800
#> 23 2026-06-24T22:30:00 2026-06-24T23:00:00            1800
#> 24 2026-06-24T23:00:00 2026-06-25T00:00:00            3600
#>                                                         title
#> 1                                                 NHL Tonight
#> 2                                                 NHL Tonight
#> 3                                                 NHL Tonight
#> 4                                                 NHL Tonight
#> 5                                                 NHL Tonight
#> 6                                                 NHL Tonight
#> 7                                                 NHL Tonight
#> 8                                                 NHL Tonight
#> 9                                                 NHL Tonight
#> 10                                                NHL Tonight
#> 11                                                NHL Tonight
#> 12                                                NHL Tonight
#> 13                                                NHL Tonight
#> 14                                                NHL Tonight
#> 15                                                   NHL Game
#> 16                                                   NHL Game
#> 17                                                NHL Tonight
#> 18                                         Welcome to the NHL
#> 19                                                NHL Tonight
#> 20                                                   NHL Game
#> 21          Top Shelf: 2026 Playoffs - Plays of the 2nd Round
#> 22  Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 23 Top Shelf: 2026 Playoffs - Plays of the Stanley Cup Finals
#> 24                                                NHL Tonight
#>                                                                                                                                                                                                                                                 description
#> 1                                                                                                                                                                                                                                               NHL Tonight
#> 2                                                                                                                                                                                                                                               NHL Tonight
#> 3                                                                                                                                                                                                                                               NHL Tonight
#> 4                                                                                                                                                                                                                                               NHL Tonight
#> 5                                                                                                                                                                                                                                               NHL Tonight
#> 6                                                                                                                                                                                                                                               NHL Tonight
#> 7                                                                                                                                                                                                                                               NHL Tonight
#> 8                                                                                                                                                                                                                                               NHL Tonight
#> 9                                                                                                                                                                                                                                               NHL Tonight
#> 10                                                                                                                                                                                                                                              NHL Tonight
#> 11                                                                                                                                                                                                                                              NHL Tonight
#> 12                                                                                                                                                                                                                                              NHL Tonight
#> 13                                                                                                                                                                                                                                              NHL Tonight
#> 14                                                                                                                                                                                                                                              NHL Tonight
#> 15                                                                                                                                                                              Vladimir Tarasenko Debut: Detroit Red Wings at St. Louis Blues on 1/19/2013
#> 16                                                                                                                                                                           Alex Ovechkin Debut: Columbus Blue Jackets at Washington Capitals on 10/5/2005
#> 17                                                                                                                                                                                                                                              NHL Tonight
#> 18 "Welcome to the NHL presented by BodyArmor SportsDrink" is a one-hour special that introduces the top prospects of the 2026 Upper Deck NHL Draft through exclusive access at home with their families and behind the scenes at the NHL Scouting Combine.
#> 19                                                                                                                                                                                                                                              NHL Tonight
#> 20                                                                                                                                                                           Alex Ovechkin Debut: Columbus Blue Jackets at Washington Capitals on 10/5/2005
#> 21                                                                                                                                                                                                        Top Shelf: 2026 Playoffs - Plays of the 2nd Round
#> 22                                                                                                                                                                                                Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 23                                                                                                                                                                                               Top Shelf: 2026 Playoffs - Plays of the Stanley Cup Finals
#> 24                                                                                                                                                                                                                                              NHL Tonight
#>               houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1        HNHLTS26062326CC            HD                    nhltonight.png
#> 2        HNHLTS26062326CC            HD                    nhltonight.png
#> 3        HNHLTS26062326CC            HD                    nhltonight.png
#> 4        HNHLTS26062326CC            HD                    nhltonight.png
#> 5        HNHLTS26062326CC            HD                    nhltonight.png
#> 6        HNHLTS26062326CC            HD                    nhltonight.png
#> 7        HNHLTS26062326CC            HD                    nhltonight.png
#> 8        HNHLTS26062326CC            HD                    nhltonight.png
#> 9        HNHLTS26062326CC            HD                    nhltonight.png
#> 10       HNHLTS26062326CC            HD                    nhltonight.png
#> 11       HNHLTS26062326CC            HD                    nhltonight.png
#> 12       HNHLTS26062326CC            HD                    nhltonight.png
#> 13       HNHLTS26062326CC            HD                    nhltonight.png
#> 14       HNHLTS26062326CC            HD                    nhltonight.png
#> 15 H120DETSTL01192013VLAD            HD                           nhl.png
#> 16   H120CBJWSH10052005CC            HD                           nhl.png
#> 17       HNHLTS26062426LV            HD            LIVE    nhltonight.png
#> 18      H6026WELCOMENHLPT            HD                    nhlnetwork.png
#> 19       HNHLTS26062426CC            HD                    nhltonight.png
#> 20   H120CBJWSH10052005CC            HD                           nhl.png
#> 21          HTSS262NDRDCC            HD                      topshelf.png
#> 22             HTSS26CFCC            HD                      topshelf.png
#> 23            HTSS26SCFCC            HD                      topshelf.png
#> 24       HNHLTS26062426CC            HD                    nhltonight.png
#> 
# }
```
