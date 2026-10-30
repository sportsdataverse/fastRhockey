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
#> [1] "2026-10-30"
#> 
#> $startDate
#> [1] "2026-10-16"
#> 
#> $endDate
#> [1] "2026-11-12"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-10-30T00:00:00 2026-10-30T01:00:00            3600
#> 2  2026-10-30T01:00:00 2026-10-30T02:00:00            3600
#> 3  2026-10-30T02:00:00 2026-10-30T03:00:00            3600
#> 4  2026-10-30T03:00:00 2026-10-30T04:00:00            3600
#> 5  2026-10-30T04:00:00 2026-10-30T05:00:00            3600
#> 6  2026-10-30T05:00:00 2026-10-30T06:00:00            3600
#> 7  2026-10-30T06:00:00 2026-10-30T07:00:00            3600
#> 8  2026-10-30T07:00:00 2026-10-30T08:00:00            3600
#> 9  2026-10-30T08:00:00 2026-10-30T09:00:00            3600
#> 10 2026-10-30T09:00:00 2026-10-30T10:00:00            3600
#> 11 2026-10-30T10:00:00 2026-10-30T11:00:00            3600
#> 12 2026-10-30T11:00:00 2026-10-30T13:00:00            7200
#> 13 2026-10-30T13:00:00 2026-10-30T15:00:00            7200
#> 14 2026-10-30T15:00:00 2026-10-30T16:00:00            3600
#> 15 2026-10-30T16:00:00 2026-10-30T17:00:00            3600
#> 16 2026-10-30T17:00:00 2026-10-30T19:00:00            7200
#> 17 2026-10-30T19:00:00 2026-10-30T21:30:00            9000
#> 18 2026-10-30T21:30:00 2026-10-30T22:00:00            1800
#> 19 2026-10-30T22:00:00 2026-10-30T23:00:00            3600
#> 20 2026-10-30T23:00:00 2026-10-30T23:30:00            1800
#> 21 2026-10-30T23:30:00 2026-10-31T00:00:00            1800
#>                                    title
#> 1         On The Fly With Bonus Coverage
#> 2                             On The Fly
#> 3                             On The Fly
#> 4                             On The Fly
#> 5                             On The Fly
#> 6                             On The Fly
#> 7                             On The Fly
#> 8                             On The Fly
#> 9                             On The Fly
#> 10                            On The Fly
#> 11                            On The Fly
#> 12                              NHL Game
#> 13                              NHL Game
#> 14 Never Offside with Julie & Cat - Ep 5
#> 15              NHL Tonight: First Shift
#> 16                               NHL Now
#> 17                 Hockey on NHL Network
#> 18                            On The Fly
#> 19        On The Fly With Bonus Coverage
#> 20                            On The Fly
#> 21                            On The Fly
#>                                                                                                                                                                                                                                  description
#> 1  Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                 On The Fly
#> 3                                                                                                                                                                                                                                 On The Fly
#> 4                                                                                                                                                                                                                                 On The Fly
#> 5                                                                                                                                                                                                                                 On The Fly
#> 6                                                                                                                                                                                                                                 On The Fly
#> 7                                                                                                                                                                                                                                 On The Fly
#> 8                                                                                                                                                                                                                                 On The Fly
#> 9                                                                                                                                                                                                                                 On The Fly
#> 10                                                                                                                                                                                                                                On The Fly
#> 11                                                                                                                                                                                                                                On The Fly
#> 12                                                                                                                                                     Winnipeg Jets at Tampa Bay Lightning on 10/29/2026 From Benchmark International Arena
#> 13                                                                                                                                                                     Boston Bruins at St. Louis Blues on 10/29/2026 From Enterprise Center
#> 14                                                                                                                                           Julie and Cat are joined by Jason Demers and Jillian Mueller for a little hockey Halloween fun!
#> 15                                                                                                                                                                                                                  NHL Tonight: First Shift
#> 16                                                                                                                                                                                                                                   NHL Now
#> 17                                                                                                                                                              2026 Women's World Championship Pre-Tournament GM2: CAN vs USA on 10/30/2026
#> 18                                                                                                                                                                                                                                On The Fly
#> 19 Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 20                                                                                                                                                                                                                                On The Fly
#> 21                                                                                                                                                                                                                                On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60VANSJS10292026            HD            LIVE      onthefly.png
#> 2    HOTF27R102926SOR            HD            LIVE      onthefly.png
#> 3     HOTF27R102926CC            HD                      onthefly.png
#> 4     HOTF27R102926CC            HD                      onthefly.png
#> 5     HOTF27R102926CC            HD                      onthefly.png
#> 6     HOTF27R102926CC            HD                      onthefly.png
#> 7     HOTF27R102926CC            HD                      onthefly.png
#> 8     HOTF27R102926CC            HD                      onthefly.png
#> 9     HOTF27R102926CC            HD                      onthefly.png
#> 10    HOTF27R102926CC            HD                      onthefly.png
#> 11    HOTF27R102926CC            HD                      onthefly.png
#> 12 H120WPGTBL10292026            HD                           nhl.png
#> 13 H120BOSSTL10292026            HD                           nhl.png
#> 14 HNHLNEVOFFSIDES3E5            HD                    nhlnetwork.png
#> 15  HNHLTFS27103026LV            HD            LIVE                  
#> 16    HNOW27R103026LV            HD            LIVE        nhlnow.png
#> 17 H150USWNTCAN103026            HD            LIVE    nhlnetwork.png
#> 18   HOTF27R103026LVA            HD            LIVE      onthefly.png
#> 19  H60SEACGY10302026            HD            LIVE      onthefly.png
#> 20   HOTF27R103026SOR            HD            LIVE      onthefly.png
#> 21    HOTF27R103026CC            HD                      onthefly.png
#> 
# }
```
