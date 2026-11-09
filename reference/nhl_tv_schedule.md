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
#> [1] "2026-11-09"
#> 
#> $startDate
#> [1] "2026-10-26"
#> 
#> $endDate
#> [1] "2026-11-23"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-09T00:00:00 2026-11-09T00:30:00            1800
#> 2  2026-11-09T00:30:00 2026-11-09T01:00:00            1800
#> 3  2026-11-09T01:00:00 2026-11-09T01:30:00            1800
#> 4  2026-11-09T01:30:00 2026-11-09T02:00:00            1800
#> 5  2026-11-09T02:00:00 2026-11-09T02:30:00            1800
#> 6  2026-11-09T02:30:00 2026-11-09T03:00:00            1800
#> 7  2026-11-09T03:00:00 2026-11-09T03:30:00            1800
#> 8  2026-11-09T03:30:00 2026-11-09T04:00:00            1800
#> 9  2026-11-09T04:00:00 2026-11-09T04:30:00            1800
#> 10 2026-11-09T04:30:00 2026-11-09T05:00:00            1800
#> 11 2026-11-09T05:00:00 2026-11-09T05:30:00            1800
#> 12 2026-11-09T05:30:00 2026-11-09T06:00:00            1800
#> 13 2026-11-09T06:00:00 2026-11-09T06:30:00            1800
#> 14 2026-11-09T06:30:00 2026-11-09T07:00:00            1800
#> 15 2026-11-09T07:00:00 2026-11-09T07:30:00            1800
#> 16 2026-11-09T07:30:00 2026-11-09T08:00:00            1800
#> 17 2026-11-09T08:00:00 2026-11-09T08:30:00            1800
#> 18 2026-11-09T08:30:00 2026-11-09T09:00:00            1800
#> 19 2026-11-09T09:00:00 2026-11-09T09:30:00            1800
#> 20 2026-11-09T09:30:00 2026-11-09T10:00:00            1800
#> 21 2026-11-09T10:00:00 2026-11-09T10:30:00            1800
#> 22 2026-11-09T10:30:00 2026-11-09T11:00:00            1800
#> 23 2026-11-09T11:00:00 2026-11-09T11:30:00            1800
#> 24 2026-11-09T11:30:00 2026-11-09T12:00:00            1800
#> 25 2026-11-09T12:00:00 2026-11-09T12:30:00            1800
#> 26 2026-11-09T12:30:00 2026-11-09T13:00:00            1800
#> 27 2026-11-09T13:00:00 2026-11-09T15:30:00            9000
#> 28 2026-11-09T15:30:00 2026-11-09T16:00:00            1800
#> 29 2026-11-09T16:00:00 2026-11-09T17:00:00            3600
#> 30 2026-11-09T17:00:00 2026-11-09T19:00:00            7200
#> 31 2026-11-09T19:00:00 2026-11-09T20:00:00            3600
#> 32 2026-11-09T20:00:00 2026-11-09T22:00:00            7200
#> 33 2026-11-09T22:00:00 2026-11-10T00:00:00            7200
#>                                                      title
#> 1                                               On The Fly
#> 2                                               On The Fly
#> 3                                               On The Fly
#> 4                                               On The Fly
#> 5                                               On The Fly
#> 6                                               On The Fly
#> 7                                               On The Fly
#> 8                                               On The Fly
#> 9                                               On The Fly
#> 10                                              On The Fly
#> 11                                              On The Fly
#> 12                                              On The Fly
#> 13                                              On The Fly
#> 14                                              On The Fly
#> 15                                              On The Fly
#> 16                                              On The Fly
#> 17                                              On The Fly
#> 18                                              On The Fly
#> 19                                              On The Fly
#> 20                                              On The Fly
#> 21                                              On The Fly
#> 22                                              On The Fly
#> 23                                              On The Fly
#> 24                                              On The Fly
#> 25                                              On The Fly
#> 26                                              On The Fly
#> 27                         IIHF Women's World Championship
#> 28                                               Top Shelf
#> 29                                NHL Tonight: First Shift
#> 30                                                 NHL Now
#> 31 NHL Tonight: 2026 Hockey Hall of Fame Induction Preview
#> 32             2026 Hockey Hall of Fame Induction Ceremony
#> 33                                             NHL Tonight
#>                                                        description
#> 1                                                       On The Fly
#> 2                                                       On The Fly
#> 3                                                       On The Fly
#> 4                                                       On The Fly
#> 5                                                       On The Fly
#> 6                                                       On The Fly
#> 7                                                       On The Fly
#> 8                                                       On The Fly
#> 9                                                       On The Fly
#> 10                                                      On The Fly
#> 11                                                      On The Fly
#> 12                                                      On The Fly
#> 13                                                      On The Fly
#> 14                                                      On The Fly
#> 15                                                      On The Fly
#> 16                                                      On The Fly
#> 17                                                      On The Fly
#> 18                                                      On The Fly
#> 19                                                      On The Fly
#> 20                                                      On The Fly
#> 21                                                      On The Fly
#> 22                                                      On The Fly
#> 23                                                      On The Fly
#> 24                                                      On The Fly
#> 25                                                      On The Fly
#> 26                                                      On The Fly
#> 27 2026 IIHF Women's World Championship: Austria vs USA on 11/9/26
#> 28                                                       Top Shelf
#> 29                                        NHL Tonight: First Shift
#> 30                                                         NHL Now
#> 31         NHL Tonight: 2026 Hockey Hall of Fame Induction Preview
#> 32                     2026 Hockey Hall of Fame Induction Ceremony
#> 33                                                     NHL Tonight
#>             houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1       HOTF27R110826CC            HD                      onthefly.png
#> 2       HOTF27R110826CC            HD                      onthefly.png
#> 3       HOTF27R110826CC            HD                      onthefly.png
#> 4       HOTF27R110826CC            HD                      onthefly.png
#> 5       HOTF27R110826CC            HD                      onthefly.png
#> 6       HOTF27R110826CC            HD                      onthefly.png
#> 7       HOTF27R110826CC            HD                      onthefly.png
#> 8       HOTF27R110826CC            HD                      onthefly.png
#> 9       HOTF27R110826CC            HD                      onthefly.png
#> 10      HOTF27R110826CC            HD                      onthefly.png
#> 11      HOTF27R110826CC            HD                      onthefly.png
#> 12      HOTF27R110826CC            HD                      onthefly.png
#> 13      HOTF27R110826CC            HD                      onthefly.png
#> 14      HOTF27R110826CC            HD                      onthefly.png
#> 15      HOTF27R110826CC            HD                      onthefly.png
#> 16      HOTF27R110826CC            HD                      onthefly.png
#> 17      HOTF27R110826CC            HD                      onthefly.png
#> 18      HOTF27R110826CC            HD                      onthefly.png
#> 19      HOTF27R110826CC            HD                      onthefly.png
#> 20      HOTF27R110826CC            HD                      onthefly.png
#> 21      HOTF27R110826CC            HD                      onthefly.png
#> 22      HOTF27R110826CC            HD                      onthefly.png
#> 23      HOTF27R110826CC            HD                      onthefly.png
#> 24      HOTF27R110826CC            HD                      onthefly.png
#> 25      HOTF27R110826CC            HD                      onthefly.png
#> 26      HOTF27R110826CC            HD                      onthefly.png
#> 27 H150WW26AUSUSA110926            HD            LIVE    nhlnetwork.png
#> 28        HTS27110726CC            HD                      topshelf.png
#> 29    HNHLTFS27110926LV            HD            LIVE                  
#> 30      HNOW27R110926LV            HD            LIVE        nhlnow.png
#> 31      HNHLT27RSHHOFLV            HD            LIVE          hhof.png
#> 32      H26HHOFCEREMONY            HD            LIVE          hhof.png
#> 33    HNHLT27RS110926LV            HD            LIVE    nhltonight.png
#> 
# }
```
