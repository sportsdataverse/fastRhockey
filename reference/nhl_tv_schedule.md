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
#> [1] "2026-08-25"
#> 
#> $startDate
#> [1] "2026-08-11"
#> 
#> $endDate
#> [1] "2026-09-08"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-08-25T01:00:00 2026-08-25T03:00:00            7200
#> 2  2026-08-25T03:00:00 2026-08-25T05:00:00            7200
#> 3  2026-08-25T05:00:00 2026-08-25T06:00:00            3600
#> 4  2026-08-25T06:00:00 2026-08-25T07:00:00            3600
#> 5  2026-08-25T07:00:00 2026-08-25T10:00:00           10800
#> 6  2026-08-25T10:00:00 2026-08-25T12:00:00            7200
#> 7  2026-08-25T12:00:00 2026-08-25T14:00:00            7200
#> 8  2026-08-25T14:00:00 2026-08-25T16:00:00            7200
#> 9  2026-08-25T16:00:00 2026-08-25T18:00:00            7200
#> 10 2026-08-25T18:00:00 2026-08-25T19:00:00            3600
#> 11 2026-08-25T19:00:00 2026-08-25T20:00:00            3600
#> 12 2026-08-25T20:00:00 2026-08-25T21:00:00            3600
#> 13 2026-08-25T21:00:00 2026-08-25T22:00:00            3600
#> 14 2026-08-25T22:00:00 2026-08-26T00:00:00            7200
#>                       title
#> 1                  NHL Game
#> 2                  NHL Game
#> 3  Top 10 Goalies Right Now
#> 4  Top 20 Centers Right Now
#> 5                3ICE: Ep 5
#> 6                  NHL Game
#> 7                  NHL Game
#> 8                  NHL Game
#> 9                  NHL Game
#> 10              NHL Tonight
#> 11              NHL Tonight
#> 12              NHL Tonight
#> 13              NHL Tonight
#> 14                 NHL Game
#>                                                                                               description
#> 1  Stanley Cup Final: Carolina Hurricanes at Vegas Golden Knights, Game 3 on 6/6/2026 From T-Mobile Arena
#> 2  Stanley Cup Final: Carolina Hurricanes at Vegas Golden Knights, Game 4 on 6/9/2026 From T-Mobile Arena
#> 3                                                                                Top 10 Goalies Right Now
#> 4                                                                                Top 20 Centers Right Now
#> 5                                                                                              3ICE: Ep 5
#> 6                                      SuperStar Summer: Brett Hull - Hull Hat Trick DET @ STL on 1/26/02
#> 7                            SuperStar Summer: Brett Hull - 2002 WCQF GM 6 Hat Trick DET @ VAN on 4/27/02
#> 8                                       SuperStar Summer: Brett Hull - 1999 SCF GM 6 DAL @ BUF on 6/19/99
#> 9                                  SuperStar Summer: Brett Hull - Hull's 4 Goal Game DET @ STL on 4/16/95
#> 10                                                                                            NHL Tonight
#> 11                                                                                            NHL Tonight
#> 12                                                                                            NHL Tonight
#> 13                                                                                            NHL Tonight
#> 14                           SuperStar Summer: Brett Hull - 2002 WCQF GM 6 Hat Trick DET @ VAN on 4/27/02
#>               houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1      H120CARVGK06062026            HD                           nhl.png
#> 2      H120CARVGK06092026            HD                           nhl.png
#> 3       H60S26T10GOALRNCC            HD                    nhlnetwork.png
#> 4       H60S26T20CTRSRNCC            HD                    nhlnetwork.png
#> 5        HNHL263ICE082126            HD                    nhlnetwork.png
#> 6      H120SSDETSTL012602            HD                           nhl.png
#> 7      H120SSDETVAN042702            HD                           nhl.png
#> 8  H120SSDALBUF061999HULL            HD                           nhl.png
#> 9      H120SSDETSTL041695            HD                           nhl.png
#> 10       HNHLTS26082526LV            HD            LIVE    nhltonight.png
#> 11       HNHLTS26082526CC            HD                    nhltonight.png
#> 12       HNHLTS26082526CC            HD                    nhltonight.png
#> 13       HNHLTS26082526CC            HD                    nhltonight.png
#> 14     H120SSDETVAN042702            HD                           nhl.png
#> 
# }
```
