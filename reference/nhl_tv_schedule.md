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
#> [1] "2026-08-27"
#> 
#> $startDate
#> [1] "2026-08-13"
#> 
#> $endDate
#> [1] "2026-09-10"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-08-27T01:00:00 2026-08-27T02:00:00            3600
#> 2  2026-08-27T02:00:00 2026-08-27T03:00:00            3600
#> 3  2026-08-27T03:00:00 2026-08-27T04:00:00            3600
#> 4  2026-08-27T04:00:00 2026-08-27T05:00:00            3600
#> 5  2026-08-27T05:00:00 2026-08-27T06:00:00            3600
#> 6  2026-08-27T06:00:00 2026-08-27T07:00:00            3600
#> 7  2026-08-27T07:00:00 2026-08-27T08:00:00            3600
#> 8  2026-08-27T08:00:00 2026-08-27T09:00:00            3600
#> 9  2026-08-27T09:00:00 2026-08-27T10:00:00            3600
#> 10 2026-08-27T10:00:00 2026-08-27T12:00:00            7200
#> 11 2026-08-27T12:00:00 2026-08-27T14:00:00            7200
#> 12 2026-08-27T14:00:00 2026-08-27T16:00:00            7200
#> 13 2026-08-27T16:00:00 2026-08-27T18:00:00            7200
#> 14 2026-08-27T18:00:00 2026-08-27T19:00:00            3600
#> 15 2026-08-27T19:00:00 2026-08-27T20:00:00            3600
#> 16 2026-08-27T20:00:00 2026-08-27T21:00:00            3600
#> 17 2026-08-27T21:00:00 2026-08-27T22:00:00            3600
#> 18 2026-08-27T22:00:00 2026-08-28T00:00:00            7200
#>                          title
#> 1       Top 20 Wings Right Now
#> 2     Top 10 Goalies Right Now
#> 3  Top 20 Defensemen Right Now
#> 4     Top 20 Centers Right Now
#> 5       Top 20 Wings Right Now
#> 6     Top 10 Goalies Right Now
#> 7  Top 20 Defensemen Right Now
#> 8     Top 20 Centers Right Now
#> 9       Top 20 Wings Right Now
#> 10                    NHL Game
#> 11                    NHL Game
#> 12                    NHL Game
#> 13                    NHL Game
#> 14                 NHL Tonight
#> 15                 NHL Tonight
#> 16                 NHL Tonight
#> 17                 NHL Tonight
#> 18                    NHL Game
#>                                                                                                              description
#> 1                                                                                                 Top 20 Wings Right Now
#> 2                                                                                               Top 10 Goalies Right Now
#> 3                                                                                            Top 20 Defensemen Right Now
#> 4                                                                                               Top 20 Centers Right Now
#> 5                                                                                                 Top 20 Wings Right Now
#> 6                                                                                               Top 10 Goalies Right Now
#> 7                                                                                            Top 20 Defensemen Right Now
#> 8                                                                                               Top 20 Centers Right Now
#> 9                                                                                                 Top 20 Wings Right Now
#> 10   2025 Hart Trophy Winner Conner Hellebuyck: Winnipeg Jets at Calgary Flames on 10/20/2025 From Scotiabank Saddledome
#> 11 2026 Hart Trophy Winner Nikita Kucherov: Tampa Bay Lightning at Seattle Kraken on 3/17/2026 From Climate Pledge Arena
#> 12             2023 Hart Trophy Winner Connor McDavid: Calgary Flames at Edmonton Oilers on 12/23/2025 From Rogers Place
#> 13  2024 Hart Trophy Winner Nathan MacKinnon: Colorado Avalanche at Minnesota Wild on 12/21/2025 From Grand Casino Arena
#> 14                                                                                                           NHL Tonight
#> 15                                                                                                           NHL Tonight
#> 16                                                                                                           NHL Tonight
#> 17                                                                                                           NHL Tonight
#> 18             2023 Hart Trophy Winner Connor McDavid: Calgary Flames at Edmonton Oilers on 12/23/2025 From Rogers Place
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  H60S26T20WINGSRNCC            HD                    nhlnetwork.png
#> 2   H60S26T10GOALRNCC            HD                    nhlnetwork.png
#> 3    H60S26T20DEFRNCC            HD                    nhlnetwork.png
#> 4   H60S26T20CTRSRNCC            HD                    nhlnetwork.png
#> 5  H60S26T20WINGSRNCC            HD                    nhlnetwork.png
#> 6   H60S26T10GOALRNCC            HD                    nhlnetwork.png
#> 7    H60S26T20DEFRNCC            HD                    nhlnetwork.png
#> 8   H60S26T20CTRSRNCC            HD                    nhlnetwork.png
#> 9  H60S26T20WINGSRNCC            HD                    nhlnetwork.png
#> 10 H120WPGCGY10202025            HD                           nhl.png
#> 11 H120TBLSEA03172026            HD                           nhl.png
#> 12 H120CGYEDM12232025            HD                           nhl.png
#> 13 H120COLMIN12212025            HD                           nhl.png
#> 14   HNHLTS26082726LV            HD            LIVE    nhltonight.png
#> 15   HNHLTS26082726CC            HD                    nhltonight.png
#> 16   HNHLTS26082726CC            HD                    nhltonight.png
#> 17   HNHLTS26082726CC            HD                    nhltonight.png
#> 18 H120CGYEDM12232025            HD                           nhl.png
#> 
# }
```
