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
#> [1] "2026-09-10"
#> 
#> $startDate
#> [1] "2026-08-27"
#> 
#> $endDate
#> [1] "2026-09-23"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-09-10T00:00:00 2026-09-10T02:00:00            7200
#> 2  2026-09-10T02:00:00 2026-09-10T03:00:00            3600
#> 3  2026-09-10T03:00:00 2026-09-10T04:00:00            3600
#> 4  2026-09-10T04:00:00 2026-09-10T06:00:00            7200
#> 5  2026-09-10T06:00:00 2026-09-10T08:00:00            7200
#> 6  2026-09-10T08:00:00 2026-09-10T09:00:00            3600
#> 7  2026-09-10T09:00:00 2026-09-10T10:00:00            3600
#> 8  2026-09-10T10:00:00 2026-09-10T12:00:00            7200
#> 9  2026-09-10T12:00:00 2026-09-10T14:00:00            7200
#> 10 2026-09-10T14:00:00 2026-09-10T16:00:00            7200
#> 11 2026-09-10T16:00:00 2026-09-10T18:00:00            7200
#> 12 2026-09-10T18:00:00 2026-09-10T19:00:00            3600
#> 13 2026-09-10T19:00:00 2026-09-10T20:00:00            3600
#> 14 2026-09-10T20:00:00 2026-09-10T21:00:00            3600
#> 15 2026-09-10T21:00:00 2026-09-10T22:00:00            3600
#> 16 2026-09-10T22:00:00 2026-09-11T00:00:00            7200
#>                                                                                                     title
#> 1               Raising The Cup: 2015 Stanley Cup Final Tampa Bay Lightning at Chicago Blackhawks, Game 6
#> 2                                             NHL Network Countdown: Top 40 Memorable Stanley Cup Moments
#> 3                                                         NHL Network Countdown: Top 50 Greatest Finishes
#> 4      Raising The Cup: 2016 Stanley Cup Final Pittsburgh Penguins at San Jose Sharks Game 6 on 6/12/2016
#> 5  Raising The Cup: 2017 Stanley Cup Final Pittsburgh Penguins at Nashville Predators Game 6 on 6/11/2017
#> 6                                                                                  Top 20 Wings Right Now
#> 7                                                                                Top 20 Centers Right Now
#> 8                                                                                                NHL Game
#> 9                                                                                                NHL Game
#> 10                                                                                               NHL Game
#> 11                                                                                               NHL Game
#> 12                                                                                            NHL Tonight
#> 13                                                                                            NHL Tonight
#> 14                                                                                            NHL Tonight
#> 15                                                                                            NHL Tonight
#> 16                                                                                               NHL Game
#>                                                                                                  description
#> 1                  Raising The Cup: 2015 Stanley Cup Final Tampa Bay Lightning at Chicago Blackhawks, Game 6
#> 2                                                NHL Network Countdown: Top 40 Memorable Stanley Cup Moments
#> 3                                                            NHL Network Countdown: Top 50 Greatest Finishes
#> 4         Raising The Cup: 2016 Stanley Cup Final Pittsburgh Penguins at San Jose Sharks Game 6 on 6/12/2016
#> 5     Raising The Cup: 2017 Stanley Cup Final Pittsburgh Penguins at Nashville Predators Game 6 on 6/11/2017
#> 6                                                                                     Top 20 Wings Right Now
#> 7                                                                                   Top 20 Centers Right Now
#> 8  Macklin Celebrini Hat Trick: San Jose Sharks at New York Rangers on 10/23/2025 From Madison Square Garden
#> 9          2026 Stadium Series - Boston Bruins at Tampa Bay Lightning on 2/1/2026 From Raymond James Stadium
#> 10                                Pittsburgh Penguins at Carolina Hurricanes on 3/18/2026 From Lenovo Center
#> 11                            Columbus Blue Jackets at Florida Panthers on 12/6/2025 From Amerant Bank Arena
#> 12                                                                                               NHL Tonight
#> 13                                                                                               NHL Tonight
#> 14                                                                                               NHL Tonight
#> 15                                                                                               NHL Tonight
#> 16                                Pittsburgh Penguins at Carolina Hurricanes on 3/18/2026 From Lenovo Center
#>            houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  H120TBLCHI061515NBC            HD                           rtc.png
#> 2          HNHLNCTDWN3            HD                 nhlncountdown.png
#> 3          HNHLNCTDWN4            HD                 nhlncountdown.png
#> 4           HRTC2016CC            HD                           rtc.png
#> 5           HRTC2017CC            HD                           rtc.png
#> 6   H60S26T20WINGSRNCC            HD                    nhlnetwork.png
#> 7    H60S26T20CTRSRNCC            HD                    nhlnetwork.png
#> 8   H120SJSNYR10232025            HD                           nhl.png
#> 9   H120BOSTBL02012026            HD                           nhl.png
#> 10  H120PITCAR03182026            HD                           nhl.png
#> 11  H120CBJFLA12062025            HD                           nhl.png
#> 12    HNHLTS26091026LV            HD            LIVE    nhltonight.png
#> 13    HNHLTS26091026CC            HD                    nhltonight.png
#> 14    HNHLTS26091026CC            HD                    nhltonight.png
#> 15    HNHLTS26091026CC            HD                    nhltonight.png
#> 16  H120PITCAR03182026            HD                           nhl.png
#> 
# }
```
