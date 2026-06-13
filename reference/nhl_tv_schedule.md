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
#> [1] "2026-06-13"
#> 
#> $startDate
#> [1] "2026-05-30"
#> 
#> $endDate
#> [1] "2026-06-26"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-06-13T00:00:00 2026-06-13T01:00:00            3600
#> 2  2026-06-13T01:00:00 2026-06-13T02:30:00            5400
#> 3  2026-06-13T02:30:00 2026-06-13T03:00:00            1800
#> 4  2026-06-13T03:00:00 2026-06-13T04:00:00            3600
#> 5  2026-06-13T04:00:00 2026-06-13T05:00:00            3600
#> 6  2026-06-13T05:00:00 2026-06-13T06:00:00            3600
#> 7  2026-06-13T06:00:00 2026-06-13T07:00:00            3600
#> 8  2026-06-13T07:00:00 2026-06-13T08:00:00            3600
#> 9  2026-06-13T08:00:00 2026-06-13T09:00:00            3600
#> 10 2026-06-13T09:00:00 2026-06-13T10:00:00            3600
#> 11 2026-06-13T10:00:00 2026-06-13T11:00:00            3600
#> 12 2026-06-13T11:00:00 2026-06-13T12:00:00            3600
#> 13 2026-06-13T12:00:00 2026-06-13T12:30:00            1800
#> 14 2026-06-13T12:30:00 2026-06-13T13:00:00            1800
#> 15 2026-06-13T13:00:00 2026-06-13T13:30:00            1800
#> 16 2026-06-13T13:30:00 2026-06-13T14:00:00            1800
#> 17 2026-06-13T14:00:00 2026-06-13T16:00:00            7200
#> 18 2026-06-13T16:00:00 2026-06-13T16:30:00            1800
#> 19 2026-06-13T16:30:00 2026-06-13T17:00:00            1800
#> 20 2026-06-13T17:00:00 2026-06-13T18:00:00            3600
#> 21 2026-06-13T18:00:00 2026-06-13T19:00:00            3600
#> 22 2026-06-13T19:00:00 2026-06-13T20:00:00            3600
#> 23 2026-06-13T20:00:00 2026-06-13T21:00:00            3600
#> 24 2026-06-13T21:00:00 2026-06-13T22:00:00            3600
#> 25 2026-06-13T22:00:00 2026-06-13T23:00:00            3600
#> 26 2026-06-13T23:00:00 2026-06-14T00:00:00            3600
#>                                                        title
#> 1                     NHL Tonight: Stanley Cup Final Edition
#> 2                                           2026 Stanley Pup
#> 3  Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 4                     NHL Tonight: Stanley Cup Final Edition
#> 5                     NHL Tonight: Stanley Cup Final Edition
#> 6                     NHL Tonight: Stanley Cup Final Edition
#> 7                     NHL Tonight: Stanley Cup Final Edition
#> 8                     NHL Tonight: Stanley Cup Final Edition
#> 9                     NHL Tonight: Stanley Cup Final Edition
#> 10                    NHL Tonight: Stanley Cup Final Edition
#> 11                    NHL Tonight: Stanley Cup Final Edition
#> 12                    NHL Tonight: Stanley Cup Final Edition
#> 13                   One Winona: The Making of a Hockey Town
#> 14                                    Inside the NBHL - Ep 2
#> 15         Top Shelf: 2026 Playoffs - Plays of the 2nd Round
#> 16 Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 17                                          Names On The Cup
#> 18 Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 19                                    Inside the NBHL - Ep 2
#> 20                       NHL Tonight: Top 32 Draft Prospects
#> 21                    NHL Tonight: Stanley Cup Final Edition
#> 22                    NHL Tonight: Stanley Cup Final Edition
#> 23                    NHL Tonight: Stanley Cup Final Edition
#> 24                       NHL Tonight: Top 32 Draft Prospects
#> 25                     Hawkeytown: Portland to the Pros Ep 1
#> 26                    NHL Tonight: Stanley Cup Final Edition
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 description
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          2026 Stanley Pup
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Edition
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#> 13 In the southeastern corner of the State of Hockey, sits a small town with big hockey dreams. Winona, Minnesota is one of eleven cities selected to receive the inaugural Minnesota Wild Foundation's Skate It Forward grants, created in 2025 to help strengthen, preserve, and celebrate community hockey in the State of Minnesota. ONE WINONA: The Making of a Hockey Town is an intimate portrait of what it takes to become a hockey town, and more importantly, why you'd want to be one in the first place. Winona isn't just trying to make hockey players, they're trying to make an entire community fall in love with hockey.
#> 14                                                                                                                                                                                                                                                                                                                         Inside the NBHL is the National Ball Hockey League's monthly show featuring league-wide power rankings, top plays, and the biggest storylines from across the NBHL. From championship contenders to viral moments, Inside the NBHL delivers an in-depth look at the players, teams, and action shaping the NBHL.
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Top Shelf: 2026 Playoffs - Plays of the 2nd Round
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Names On The Cup
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Top Shelf: 2026 Playoffs - Plays of the Conference Finals
#> 19                                                                                                                                                                                                                                                                                                                         Inside the NBHL is the National Ball Hockey League's monthly show featuring league-wide power rankings, top plays, and the biggest storylines from across the NBHL. From championship contenders to viral moments, Inside the NBHL delivers an in-depth look at the players, teams, and action shaping the NBHL.
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NHL Tonight: Top 32 Draft Prospects
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#> 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NHL Tonight: Top 32 Draft Prospects
#> 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Hawkeytown: Portland to the Pros Ep 1
#> 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Edition
#>            houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 2     HNHL26STANLEYPUP            HD                    nhlnetwork.png
#> 3           HTSS26CFCC            HD                      topshelf.png
#> 4  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 5  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 6  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 7  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 8  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 9  HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 10 HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 11 HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 12 HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 13          HNHLWINONA            HD                    nhlnetwork.png
#> 14        HNHLNBHLEP02            HD                    nhlnetwork.png
#> 15       HTSS262NDRDCC            HD                      topshelf.png
#> 16          HTSS26CFCC            HD                      topshelf.png
#> 17     HNHLDOCNAMESCUP            HD                    nhlnetwork.png
#> 18          HTSS26CFCC            HD                      topshelf.png
#> 19        HNHLNBHLEP02            HD                    nhlnetwork.png
#> 20    HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 21 HNHLT26SCFE061326LV            HD            LIVE    nhltonight.png
#> 22 HNHLT26SCFE061326CC            HD                    nhltonight.png
#> 23 HNHLT26SCFE061326CC            HD                    nhltonight.png
#> 24    HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 25   HNHLWINTERHAWKSE1            HD                    nhlnetwork.png
#> 26 HNHLT26SCFE061326CC            HD                    nhltonight.png
#> 
# }
```
