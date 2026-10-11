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
#> [1] "2026-10-11"
#> 
#> $startDate
#> [1] "2026-09-27"
#> 
#> $endDate
#> [1] "2026-10-25"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-10-11T00:00:00 2026-10-11T01:00:00            3600
#> 2  2026-10-11T01:00:00 2026-10-11T02:00:00            3600
#> 3  2026-10-11T02:00:00 2026-10-11T03:00:00            3600
#> 4  2026-10-11T03:00:00 2026-10-11T04:00:00            3600
#> 5  2026-10-11T04:00:00 2026-10-11T05:00:00            3600
#> 6  2026-10-11T05:00:00 2026-10-11T06:00:00            3600
#> 7  2026-10-11T06:00:00 2026-10-11T07:00:00            3600
#> 8  2026-10-11T07:00:00 2026-10-11T08:00:00            3600
#> 9  2026-10-11T08:00:00 2026-10-11T09:00:00            3600
#> 10 2026-10-11T09:00:00 2026-10-11T10:00:00            3600
#> 11 2026-10-11T10:00:00 2026-10-11T11:00:00            3600
#> 12 2026-10-11T11:00:00 2026-10-11T12:00:00            3600
#> 13 2026-10-11T12:00:00 2026-10-11T13:00:00            3600
#> 14 2026-10-11T13:00:00 2026-10-11T15:00:00            7200
#> 15 2026-10-11T15:00:00 2026-10-11T17:00:00            7200
#> 16 2026-10-11T17:00:00 2026-10-11T17:30:00            1800
#> 17 2026-10-11T17:30:00 2026-10-11T18:30:00            3600
#> 18 2026-10-11T18:30:00 2026-10-11T19:00:00            1800
#> 19 2026-10-11T19:00:00 2026-10-11T22:00:00           10800
#> 20 2026-10-11T22:00:00 2026-10-11T22:30:00            1800
#> 21 2026-10-11T22:30:00 2026-10-11T23:00:00            1800
#> 22 2026-10-11T23:00:00 2026-10-11T23:30:00            1800
#> 23 2026-10-11T23:30:00 2026-10-12T00:00:00            1800
#>                                             title
#> 1                  On The Fly With Bonus Coverage
#> 2                                      On The Fly
#> 3                                      On The Fly
#> 4                                      On The Fly
#> 5                                      On The Fly
#> 6                                      On The Fly
#> 7                                      On The Fly
#> 8                                      On The Fly
#> 9                                      On The Fly
#> 10                                     On The Fly
#> 11                                     On The Fly
#> 12                                     On The Fly
#> 13          Never Offside with Julie & Cat - Ep 2
#> 14                                       NHL Game
#> 15                                       NHL Game
#> 16                          Pioneers: Gordie Howe
#> 17 Fight For Life with Tony Granato: Tony Granato
#> 18                                      Top Shelf
#> 19           Regular Season Hockey on NHL Network
#> 20                                     On The Fly
#> 21                                     On The Fly
#> 22                                     On The Fly
#> 23                                     On The Fly
#>                                                                                                                                                                                                                                                                                                                                                                                                  description
#> 1                                                                                                                                                                  Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#> 12                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#> 13                                                                                                                                                                                                                                                                                                                                                               Julie and Cat are joined by Rick Celebrini.
#> 14                                                                                                                                                                                                                                                                                                                                   Toronto Maple Leafs at Colorado Avalanche on 10/10/2026 From Ball Arena
#> 15                                                                                                                                                                                                                                                                                                                                         Philadelphia Flyers at Boston Bruins on 10/10/2026 From TD Garden
#> 16                                                                                                                                                                                                                                                                                                   An engaging interview with one of the greatest players to ever play the game - Gordie Howe, Mr. Hockey.
#> 17 On this episode of Fight For Life, Tony Granato and his wife, Linda, turn the camera on themselves. Together, they share what it was like to face a brain injury that threatened Tony's hockey career and, years later, a non-Hodgkin lymphoma diagnosis. From both of their perspectives, they tell the story of how those challenges shaped their lives, hoping to encourage others to share their own.
#> 18                                                                                                                                                                                                                                                                                                                                                                                                 Top Shelf
#> 19                                                                                                                                                                                                                                                                                                                        Carolina Hurricanes at Philadelphia Flyers on 10/11/2026 From Xfinity Mobile Arena
#> 20                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#> 21                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#> 22                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#> 23                                                                                                                                                                                                                                                                                                                                                                                                On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60LAKVGK10102026            HD            LIVE      onthefly.png
#> 2    HOTF27R101026SOR            HD            LIVE      onthefly.png
#> 3     HOTF27R101026CC            HD                      onthefly.png
#> 4     HOTF27R101026CC            HD                      onthefly.png
#> 5     HOTF27R101026CC            HD                      onthefly.png
#> 6     HOTF27R101026CC            HD                      onthefly.png
#> 7     HOTF27R101026CC            HD                      onthefly.png
#> 8     HOTF27R101026CC            HD                      onthefly.png
#> 9     HOTF27R101026CC            HD                      onthefly.png
#> 10    HOTF27R101026CC            HD                      onthefly.png
#> 11    HOTF27R101026CC            HD                      onthefly.png
#> 12    HOTF27R101026CC            HD                      onthefly.png
#> 13 HNHLNEVOFFSIDES3E2            HD                    nhlnetwork.png
#> 14 H120TORCOL10102026            HD                           nhl.png
#> 15 H120PHIBOS10102026            HD                           nhl.png
#> 16              H0140            HD                    nhlnetwork.png
#> 17        HNHLFFLEP04            HD                    nhlnetwork.png
#> 18      HTS27101026CC            HD                      topshelf.png
#> 19 H180CARPHI10112026            HD            LIVE           nhl.png
#> 20   HOTF27R101126SOR            HD            LIVE      onthefly.png
#> 21    HOTF27R101126CC            HD                      onthefly.png
#> 22    HOTF27R101126CC            HD                      onthefly.png
#> 23    HOTF27R101126CC            HD                      onthefly.png
#> 
# }
```
