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
#> [1] "2026-07-18"
#> 
#> $startDate
#> [1] "2026-07-04"
#> 
#> $endDate
#> [1] "2026-08-01"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-07-18T00:00:00 2026-07-18T01:00:00            3600
#> 2  2026-07-18T01:00:00 2026-07-18T02:00:00            3600
#> 3  2026-07-18T02:00:00 2026-07-18T03:00:00            3600
#> 4  2026-07-18T03:00:00 2026-07-18T04:00:00            3600
#> 5  2026-07-18T04:00:00 2026-07-18T05:00:00            3600
#> 6  2026-07-18T05:00:00 2026-07-18T06:00:00            3600
#> 7  2026-07-18T06:00:00 2026-07-18T07:00:00            3600
#> 8  2026-07-18T07:00:00 2026-07-18T08:00:00            3600
#> 9  2026-07-18T08:00:00 2026-07-18T09:00:00            3600
#> 10 2026-07-18T09:00:00 2026-07-18T10:00:00            3600
#> 11 2026-07-18T10:00:00 2026-07-18T11:00:00            3600
#> 12 2026-07-18T11:00:00 2026-07-18T12:00:00            3600
#> 13 2026-07-18T12:00:00 2026-07-18T15:00:00           10800
#> 14 2026-07-18T15:00:00 2026-07-18T17:00:00            7200
#> 15 2026-07-18T17:00:00 2026-07-18T20:00:00           10800
#> 16 2026-07-18T20:00:00 2026-07-18T22:00:00            7200
#> 17 2026-07-18T22:00:00 2026-07-18T22:30:00            1800
#> 18 2026-07-18T22:30:00 2026-07-19T01:30:00           10800
#>                                                                title
#> 1                                                        NHL Tonight
#> 2                                                        NHL Tonight
#> 3                                                        NHL Tonight
#> 4                                                        NHL Tonight
#> 5                                                        NHL Tonight
#> 6                                                        NHL Tonight
#> 7                                                        NHL Tonight
#> 8                                                        NHL Tonight
#> 9                                                        NHL Tonight
#> 10                                                       NHL Tonight
#> 11                                                       NHL Tonight
#> 12                                                       NHL Tonight
#> 13                                    IIHF World Junior Championship
#> 14                                    IIHF World Junior Championship
#> 15                                    IIHF World Junior Championship
#> 16                                    IIHF World Junior Championship
#> 17 NHL Network Countdown: World Junior Championship All-Time Moments
#> 18                                    IIHF World Junior Championship
#>                                                                                                     description
#> 1                                                                                                   NHL Tonight
#> 2                                                                                                   NHL Tonight
#> 3                                                                                                   NHL Tonight
#> 4                                                                                                   NHL Tonight
#> 5                                                                                                   NHL Tonight
#> 6                                                                                                   NHL Tonight
#> 7                                                                                                   NHL Tonight
#> 8                                                                                                   NHL Tonight
#> 9                                                                                                   NHL Tonight
#> 10                                                                                                  NHL Tonight
#> 11                                                                                                  NHL Tonight
#> 12                                                                                                  NHL Tonight
#> 13                                          2017 IIHF World Junior Championship Gold Medal Game: Canada vs. USA
#> 14 2021 IIHF World Junior Championship: Gold Medal Game - United States at Canada on 1/5/2021 From Rogers Place
#> 15                                    2024 World Junior Championship Gold Medal Game: USA at Sweden on 1/5/2024
#> 16                                             2025 World Junior Gold Game: Finland at USA on 1/5/2025 From CTC
#> 17                                            NHL Network Countdown: World Junior Championship All-Time Moments
#> 18                                    2024 World Junior Championship Gold Medal Game: USA at Sweden on 1/5/2024
#>              houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1       HNHLTS26071726CC            HD                    nhltonight.png
#> 2       HNHLTS26071726CC            HD                    nhltonight.png
#> 3       HNHLTS26071726CC            HD                    nhltonight.png
#> 4       HNHLTS26071726CC            HD                    nhltonight.png
#> 5       HNHLTS26071726CC            HD                    nhltonight.png
#> 6       HNHLTS26071726CC            HD                    nhltonight.png
#> 7       HNHLTS26071726CC            HD                    nhltonight.png
#> 8       HNHLTS26071726CC            HD                    nhltonight.png
#> 9       HNHLTS26071726CC            HD                    nhltonight.png
#> 10      HNHLTS26071726CC            HD                    nhltonight.png
#> 11      HNHLTS26071726CC            HD                    nhltonight.png
#> 12      HNHLTS26071726CC            HD                    nhltonight.png
#> 13       H180WJC2017GOLD            HD                    nhlnetwork.png
#> 14    H120USACAN01052021            HD                    nhlnetwork.png
#> 15    H180USASWE01052024            HD                    nhlnetwork.png
#> 16 H120WJCGOLD01052025CC            HD                    nhlnetwork.png
#> 17        HNHLNCTDWN1805            HD                 nhlncountdown.png
#> 18    H180USASWE01052024            HD                    nhlnetwork.png
#> 
# }
```
