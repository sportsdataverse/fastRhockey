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
#> [1] "2026-11-24"
#> 
#> $startDate
#> [1] "2026-11-10"
#> 
#> $endDate
#> [1] "2026-12-08"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-24T00:00:00 2026-11-24T01:00:00            3600
#> 2  2026-11-24T01:00:00 2026-11-24T01:30:00            1800
#> 3  2026-11-24T01:30:00 2026-11-24T02:00:00            1800
#> 4  2026-11-24T02:00:00 2026-11-24T02:30:00            1800
#> 5  2026-11-24T02:30:00 2026-11-24T03:00:00            1800
#> 6  2026-11-24T03:00:00 2026-11-24T03:30:00            1800
#> 7  2026-11-24T03:30:00 2026-11-24T04:00:00            1800
#> 8  2026-11-24T04:00:00 2026-11-24T04:30:00            1800
#> 9  2026-11-24T04:30:00 2026-11-24T05:00:00            1800
#> 10 2026-11-24T05:00:00 2026-11-24T05:30:00            1800
#> 11 2026-11-24T05:30:00 2026-11-24T06:00:00            1800
#> 12 2026-11-24T06:00:00 2026-11-24T06:30:00            1800
#> 13 2026-11-24T06:30:00 2026-11-24T07:00:00            1800
#> 14 2026-11-24T07:00:00 2026-11-24T07:30:00            1800
#> 15 2026-11-24T07:30:00 2026-11-24T08:00:00            1800
#> 16 2026-11-24T08:00:00 2026-11-24T08:30:00            1800
#> 17 2026-11-24T08:30:00 2026-11-24T09:00:00            1800
#> 18 2026-11-24T09:00:00 2026-11-24T09:30:00            1800
#> 19 2026-11-24T09:30:00 2026-11-24T10:00:00            1800
#> 20 2026-11-24T10:00:00 2026-11-24T10:30:00            1800
#> 21 2026-11-24T10:30:00 2026-11-24T11:00:00            1800
#> 22 2026-11-24T11:00:00 2026-11-24T11:30:00            1800
#> 23 2026-11-24T11:30:00 2026-11-24T12:00:00            1800
#> 24 2026-11-24T12:00:00 2026-11-24T14:00:00            7200
#> 25 2026-11-24T14:00:00 2026-11-24T16:00:00            7200
#> 26 2026-11-24T16:00:00 2026-11-24T17:00:00            3600
#> 27 2026-11-24T17:00:00 2026-11-24T18:00:00            3600
#> 28 2026-11-24T18:00:00 2026-11-24T19:00:00            3600
#> 29 2026-11-24T19:00:00 2026-11-24T20:00:00            3600
#> 30 2026-11-24T20:00:00 2026-11-24T21:30:00            5400
#> 31 2026-11-24T21:30:00 2026-11-24T23:00:00            5400
#> 32 2026-11-24T23:00:00 2026-11-24T23:30:00            1800
#> 33 2026-11-24T23:30:00 2026-11-25T00:00:00            1800
#>                                                           title
#> 1                                On The Fly With Bonus Coverage
#> 2                                                    On The Fly
#> 3                                                    On The Fly
#> 4                                                    On The Fly
#> 5                                                    On The Fly
#> 6                                                    On The Fly
#> 7                                                    On The Fly
#> 8                                                    On The Fly
#> 9                                                    On The Fly
#> 10                                                   On The Fly
#> 11                                                   On The Fly
#> 12                                                   On The Fly
#> 13                                                   On The Fly
#> 14                                                   On The Fly
#> 15                                                   On The Fly
#> 16                                                   On The Fly
#> 17                                                   On The Fly
#> 18                                                   On The Fly
#> 19                                                   On The Fly
#> 20                                                   On The Fly
#> 21                                                   On The Fly
#> 22                                                   On The Fly
#> 23                                                   On The Fly
#> 24                                                     NHL Game
#> 25                                                     NHL Game
#> 26                                     NHL Tonight: First Shift
#> 27                                                      NHL Now
#> 28                                                      NHL Now
#> 29                                                  NHL Tonight
#> 30 Fight For Life with Tony Granato: Ep 5 - The Journey Forward
#> 31 Fight For Life with Tony Granato: Ep 5 - The Journey Forward
#> 32                                                   On The Fly
#> 33                                                   On The Fly
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     description
#> 1                                                                                                                                                                                                                                                                                     Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 24                                                                                                                                                                                                                                                                                                                                                                                                                                              Columbus Blue Jackets at New Jersey Devils on 11/23/2026 From Prudential Center
#> 25                                                                                                                                                                                                                                                                                                                                                                                                                                                          Utah Mammoth at Winnipeg Jets on 11/23/2026 From Canada Life Center
#> 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NHL Tonight: First Shift
#> 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NHL Now
#> 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NHL Now
#> 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  NHL Tonight
#> 30 Denna, Matt, AJ, and Jake each experienced life-altering spinal cord injuries that changed far more than their hockey careers. In an instant, the futures they had imagined looked completely different, and each was faced with the challenge of rebuilding a life on new terms. In this episode of Fight for Life, they share the realities of that journey, the unique gym that brought them together, and the friendships that have helped them navigate the challenges, find community, and discover a new way forward.
#> 31 Denna, Matt, AJ, and Jake each experienced life-altering spinal cord injuries that changed far more than their hockey careers. In an instant, the futures they had imagined looked completely different, and each was faced with the challenge of rebuilding a life on new terms. In this episode of Fight for Life, they share the realities of that journey, the unique gym that brought them together, and the friendships that have helped them navigate the challenges, find community, and discover a new way forward.
#> 32                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 33                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60MINSJS11232026            HD            LIVE      onthefly.png
#> 2    HOTF27R112326SOR            HD            LIVE      onthefly.png
#> 3     HOTF27R112326CC            HD                      onthefly.png
#> 4     HOTF27R112326CC            HD                      onthefly.png
#> 5     HOTF27R112326CC            HD                      onthefly.png
#> 6     HOTF27R112326CC            HD                      onthefly.png
#> 7     HOTF27R112326CC            HD                      onthefly.png
#> 8     HOTF27R112326CC            HD                      onthefly.png
#> 9     HOTF27R112326CC            HD                      onthefly.png
#> 10    HOTF27R112326CC            HD                      onthefly.png
#> 11    HOTF27R112326CC            HD                      onthefly.png
#> 12    HOTF27R112326CC            HD                      onthefly.png
#> 13    HOTF27R112326CC            HD                      onthefly.png
#> 14    HOTF27R112326CC            HD                      onthefly.png
#> 15    HOTF27R112326CC            HD                      onthefly.png
#> 16    HOTF27R112326CC            HD                      onthefly.png
#> 17    HOTF27R112326CC            HD                      onthefly.png
#> 18    HOTF27R112326CC            HD                      onthefly.png
#> 19    HOTF27R112326CC            HD                      onthefly.png
#> 20    HOTF27R112326CC            HD                      onthefly.png
#> 21    HOTF27R112326CC            HD                      onthefly.png
#> 22    HOTF27R112326CC            HD                      onthefly.png
#> 23    HOTF27R112326CC            HD                      onthefly.png
#> 24 H120CBJNJD11232026            HD                           nhl.png
#> 25 H120UTAWPG11232026            HD                           nhl.png
#> 26  HNHLTFS27112426LV            HD            LIVE                  
#> 27    HNOW27R112426LV            HD            LIVE        nhlnow.png
#> 28    HNOW27R112426CC            HD                        nhlnow.png
#> 29  HNHLT27RS112426LV            HD            LIVE    nhltonight.png
#> 30        HNHLFFLEP05            HD                    nhlnetwork.png
#> 31        HNHLFFLEP05            HD                    nhlnetwork.png
#> 32   HOTF27R112426SOR            HD            LIVE      onthefly.png
#> 33    HOTF27R112426CC            HD                      onthefly.png
#> 
# }
```
