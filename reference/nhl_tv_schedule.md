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
#> [1] "2026-06-09"
#> 
#> $startDate
#> [1] "2026-05-26"
#> 
#> $endDate
#> [1] "2026-06-23"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-06-09T00:00:00 2026-06-09T01:00:00            3600
#> 2  2026-06-09T01:00:00 2026-06-09T02:00:00            3600
#> 3  2026-06-09T02:00:00 2026-06-09T03:00:00            3600
#> 4  2026-06-09T03:00:00 2026-06-09T04:00:00            3600
#> 5  2026-06-09T04:00:00 2026-06-09T05:00:00            3600
#> 6  2026-06-09T05:00:00 2026-06-09T06:00:00            3600
#> 7  2026-06-09T06:00:00 2026-06-09T07:00:00            3600
#> 8  2026-06-09T07:00:00 2026-06-09T08:00:00            3600
#> 9  2026-06-09T08:00:00 2026-06-09T09:00:00            3600
#> 10 2026-06-09T09:00:00 2026-06-09T10:00:00            3600
#> 11 2026-06-09T10:00:00 2026-06-09T11:00:00            3600
#> 12 2026-06-09T11:00:00 2026-06-09T13:00:00            7200
#> 13 2026-06-09T13:00:00 2026-06-09T15:00:00            7200
#> 14 2026-06-09T15:00:00 2026-06-09T15:30:00            1800
#> 15 2026-06-09T15:30:00 2026-06-09T16:00:00            1800
#> 16 2026-06-09T16:00:00 2026-06-09T17:00:00            3600
#> 17 2026-06-09T17:00:00 2026-06-09T18:00:00            3600
#> 18 2026-06-09T18:00:00 2026-06-09T20:00:00            7200
#> 19 2026-06-09T20:00:00 2026-06-09T21:00:00            3600
#> 20 2026-06-09T21:00:00 2026-06-09T22:00:00            3600
#> 21 2026-06-09T21:30:00 2026-06-09T22:00:00            1800
#> 22 2026-06-09T22:00:00 2026-06-09T23:00:00            3600
#> 23 2026-06-09T23:00:00 2026-06-10T00:00:00            3600
#>                                                          title
#> 1                       NHL Tonight: Stanley Cup Final Edition
#> 2                       NHL Tonight: Stanley Cup Final Edition
#> 3                       NHL Tonight: Stanley Cup Final Edition
#> 4                       NHL Tonight: Stanley Cup Final Edition
#> 5                       NHL Tonight: Stanley Cup Final Edition
#> 6                       NHL Tonight: Stanley Cup Final Edition
#> 7                       NHL Tonight: Stanley Cup Final Edition
#> 8                       NHL Tonight: Stanley Cup Final Edition
#> 9                       NHL Tonight: Stanley Cup Final Edition
#> 10                      NHL Tonight: Stanley Cup Final Edition
#> 11                      NHL Tonight: Stanley Cup Final Edition
#> 12                                                    NHL Game
#> 13                                                    NHL Game
#> 14                       Top Shelf: Best of the Regular Season
#> 15           Top Shelf: 2026 Playoffs - Plays of the 1st Round
#> 16                                    NHL Tonight: First Shift
#> 17                                                     NHL Now
#> 18                     NHL Tonight: Stanley Cup Final Pre Game
#> 19                         NHL Tonight: Top 32 Draft Prospects
#> 20              Fight For Life with Tony Granato: Eddie Olczyk
#> 21         NHL Network Countdown Top 25 Playoff Overtime Games
#> 22 NHL Network Countdown: Top 40 Memorable Stanley Cup Moments
#> 23                    NHL Tonight: Stanley Cup Final Post Game
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    description
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NHL Tonight: Stanley Cup Final Edition
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NHL Tonight: Stanley Cup Final Edition
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NHL Tonight: Stanley Cup Final Edition
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Stanley Cup Final: Vegas Golden Knights at Carolina Hurricanes, Game 2 on 6/4/2026 From Lenovo Center
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Stanley Cup Final: Carolina Hurricanes at Vegas Golden Knights, Game 3 on 6/6/2026 From T-Mobile Arena
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Top Shelf: Best of the Regular Season
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Top Shelf: 2026 Playoffs - Plays of the 1st Round
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: First Shift
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NHL Now
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NHL Tonight: Stanley Cup Final Pre Game
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         NHL Tonight: Top 32 Draft Prospects
#> 20 Eddie Olczyk's NHL career took off when he was still a teenager, leading to decades in the spotlight as a player and later as abroadcaster. But in 2017, his life was upended by a stage 3 colon cancer diagnosis. In this episode of Fight for Life, Eddie reflects on the highs of his hockey journey, the shock of his diagnosis, and the resilience it took to fight his way back - sharing how the experience reshaped his perspective on life, family, and purpose. His wife, Diana, joins the conversation, offering her perspective on the fight they faced together and the strength it took to support him every step of the way.
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         NHL Network Countdown Top 25 Playoff Overtime Games
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 NHL Network Countdown: Top 40 Memorable Stanley Cup Moments
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#>             houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 2   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 3   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 4   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 5   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 6   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 7   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 8   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 9   HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 10  HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 11  HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 12   H120VGKCAR06042026            HD                           nhl.png
#> 13   H120CARVGK06062026            HD                           nhl.png
#> 14        HTS26BORS26CC            HD                      topshelf.png
#> 15         HTS261STRDCC            HD                      topshelf.png
#> 16    HNHLTFS26060926LV            HD            LIVE                  
#> 17     HNOW26PE060926LV            HD            LIVE        nhlnow.png
#> 18   HNHLT26SCF060926LV            HD            LIVE    nhltonight.png
#> 19     HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 20          HNHLFFLEP03            HD                    nhlnetwork.png
#> 21          HNHLNCTDWN2            HD                 nhlncountdown.png
#> 22          HNHLNCTDWN3            HD                 nhlncountdown.png
#> 23 HNHLT26SCPG060926SOR            HD            LIVE    nhltonight.png
#> 
# }
```
