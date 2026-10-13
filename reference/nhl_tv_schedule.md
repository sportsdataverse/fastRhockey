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
#> [1] "2026-10-13"
#> 
#> $startDate
#> [1] "2026-09-29"
#> 
#> $endDate
#> [1] "2026-10-26"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-10-13T00:00:00 2026-10-13T00:30:00            1800
#> 2  2026-10-13T00:30:00 2026-10-13T01:00:00            1800
#> 3  2026-10-13T01:00:00 2026-10-13T01:30:00            1800
#> 4  2026-10-13T01:30:00 2026-10-13T02:00:00            1800
#> 5  2026-10-13T02:00:00 2026-10-13T02:30:00            1800
#> 6  2026-10-13T02:30:00 2026-10-13T03:00:00            1800
#> 7  2026-10-13T03:00:00 2026-10-13T03:30:00            1800
#> 8  2026-10-13T03:30:00 2026-10-13T04:00:00            1800
#> 9  2026-10-13T04:00:00 2026-10-13T04:30:00            1800
#> 10 2026-10-13T04:30:00 2026-10-13T05:00:00            1800
#> 11 2026-10-13T05:00:00 2026-10-13T05:30:00            1800
#> 12 2026-10-13T05:30:00 2026-10-13T06:00:00            1800
#> 13 2026-10-13T06:00:00 2026-10-13T06:30:00            1800
#> 14 2026-10-13T06:30:00 2026-10-13T07:00:00            1800
#> 15 2026-10-13T07:00:00 2026-10-13T07:30:00            1800
#> 16 2026-10-13T07:30:00 2026-10-13T08:00:00            1800
#> 17 2026-10-13T08:00:00 2026-10-13T08:30:00            1800
#> 18 2026-10-13T08:30:00 2026-10-13T09:00:00            1800
#> 19 2026-10-13T09:00:00 2026-10-13T09:30:00            1800
#> 20 2026-10-13T09:30:00 2026-10-13T10:00:00            1800
#> 21 2026-10-13T10:00:00 2026-10-13T10:30:00            1800
#> 22 2026-10-13T10:30:00 2026-10-13T11:00:00            1800
#> 23 2026-10-13T11:00:00 2026-10-13T11:30:00            1800
#> 24 2026-10-13T11:30:00 2026-10-13T12:00:00            1800
#> 25 2026-10-13T12:00:00 2026-10-13T14:00:00            7200
#> 26 2026-10-13T14:00:00 2026-10-13T16:00:00            7200
#> 27 2026-10-13T16:00:00 2026-10-13T17:00:00            3600
#> 28 2026-10-13T17:00:00 2026-10-13T19:00:00            7200
#> 29 2026-10-13T19:00:00 2026-10-13T22:00:00           10800
#> 30 2026-10-13T22:00:00 2026-10-13T23:00:00            3600
#> 31 2026-10-13T23:00:00 2026-10-14T00:00:00            3600
#>                              title
#> 1                       On The Fly
#> 2                       On The Fly
#> 3                       On The Fly
#> 4                       On The Fly
#> 5                       On The Fly
#> 6                       On The Fly
#> 7                       On The Fly
#> 8                       On The Fly
#> 9                       On The Fly
#> 10                      On The Fly
#> 11                      On The Fly
#> 12                      On The Fly
#> 13                      On The Fly
#> 14                      On The Fly
#> 15                      On The Fly
#> 16                      On The Fly
#> 17                      On The Fly
#> 18                      On The Fly
#> 19                      On The Fly
#> 20                      On The Fly
#> 21                      On The Fly
#> 22                      On The Fly
#> 23                      On The Fly
#> 24                      On The Fly
#> 25                        NHL Game
#> 26                        NHL Game
#> 27        NHL Tonight: First Shift
#> 28                         NHL Now
#> 29                     NHL Tonight
#> 30 NHL Tonight With Bonus Coverage
#> 31  On The Fly With Bonus Coverage
#>                                                                                                                                                                                                                                  description
#> 1                                                                                                                                                                                                                                 On The Fly
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
#> 12                                                                                                                                                                                                                                On The Fly
#> 13                                                                                                                                                                                                                                On The Fly
#> 14                                                                                                                                                                                                                                On The Fly
#> 15                                                                                                                                                                                                                                On The Fly
#> 16                                                                                                                                                                                                                                On The Fly
#> 17                                                                                                                                                                                                                                On The Fly
#> 18                                                                                                                                                                                                                                On The Fly
#> 19                                                                                                                                                                                                                                On The Fly
#> 20                                                                                                                                                                                                                                On The Fly
#> 21                                                                                                                                                                                                                                On The Fly
#> 22                                                                                                                                                                                                                                On The Fly
#> 23                                                                                                                                                                                                                                On The Fly
#> 24                                                                                                                                                                                                                                On The Fly
#> 25                                                                                                                                                                 Ottawa Senators at New Jersey Devils on 10/12/2026 From Prudential Center
#> 26                                                                                                                                                                      Florida Panthers at Buffalo Sabres on 10/12/2026 From KeyBank Center
#> 27                                                                                                                                                                                                                  NHL Tonight: First Shift
#> 28                                                                                                                                                                                                                                   NHL Now
#> 29                                                                                                                                                                                                                               NHL Tonight
#> 30                                                     Takes you on the ice with live look-ins, breaking news, real-time scores, and expert analysis. NHL Veterans breakdown highlights and demo game situations. (Live with bonus coverage)
#> 31 Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1     HOTF27R101226CC            HD                      onthefly.png
#> 2     HOTF27R101226CC            HD                      onthefly.png
#> 3     HOTF27R101226CC            HD                      onthefly.png
#> 4     HOTF27R101226CC            HD                      onthefly.png
#> 5     HOTF27R101226CC            HD                      onthefly.png
#> 6     HOTF27R101226CC            HD                      onthefly.png
#> 7     HOTF27R101226CC            HD                      onthefly.png
#> 8     HOTF27R101226CC            HD                      onthefly.png
#> 9     HOTF27R101226CC            HD                      onthefly.png
#> 10    HOTF27R101226CC            HD                      onthefly.png
#> 11    HOTF27R101226CC            HD                      onthefly.png
#> 12    HOTF27R101226CC            HD                      onthefly.png
#> 13    HOTF27R101226CC            HD                      onthefly.png
#> 14    HOTF27R101226CC            HD                      onthefly.png
#> 15    HOTF27R101226CC            HD                      onthefly.png
#> 16    HOTF27R101226CC            HD                      onthefly.png
#> 17    HOTF27R101226CC            HD                      onthefly.png
#> 18    HOTF27R101226CC            HD                      onthefly.png
#> 19    HOTF27R101226CC            HD                      onthefly.png
#> 20    HOTF27R101226CC            HD                      onthefly.png
#> 21    HOTF27R101226CC            HD                      onthefly.png
#> 22    HOTF27R101226CC            HD                      onthefly.png
#> 23    HOTF27R101226CC            HD                      onthefly.png
#> 24    HOTF27R101226CC            HD                      onthefly.png
#> 25 H120OTTNJD10122026            HD                           nhl.png
#> 26 H120FLABUF10122026            HD                           nhl.png
#> 27  HNHLTFS27101326LV            HD            LIVE                  
#> 28    HNOW27R101326LV            HD            LIVE        nhlnow.png
#> 29  HNHLT27RS101326LV            HD            LIVE    nhltonight.png
#> 30  H60MINWPG10132026            HD            LIVE    nhltonight.png
#> 31  H60TORUTA10132026            HD            LIVE      onthefly.png
#> 
# }
```
