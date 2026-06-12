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
#> [1] "2026-06-12"
#> 
#> $startDate
#> [1] "2026-05-29"
#> 
#> $endDate
#> [1] "2026-06-26"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-06-12T00:00:00 2026-06-12T01:00:00            3600
#> 2  2026-06-12T01:00:00 2026-06-12T02:00:00            3600
#> 3  2026-06-12T02:00:00 2026-06-12T03:00:00            3600
#> 4  2026-06-12T03:00:00 2026-06-12T04:00:00            3600
#> 5  2026-06-12T04:00:00 2026-06-12T05:00:00            3600
#> 6  2026-06-12T05:00:00 2026-06-12T06:00:00            3600
#> 7  2026-06-12T06:00:00 2026-06-12T07:00:00            3600
#> 8  2026-06-12T07:00:00 2026-06-12T08:00:00            3600
#> 9  2026-06-12T08:00:00 2026-06-12T09:00:00            3600
#> 10 2026-06-12T09:00:00 2026-06-12T10:00:00            3600
#> 11 2026-06-12T10:00:00 2026-06-12T11:00:00            3600
#> 12 2026-06-12T11:00:00 2026-06-12T12:00:00            3600
#> 13 2026-06-12T12:00:00 2026-06-12T13:00:00            3600
#> 14 2026-06-12T13:00:00 2026-06-12T14:00:00            3600
#> 15 2026-06-12T14:00:00 2026-06-12T15:00:00            3600
#> 16 2026-06-12T15:00:00 2026-06-12T17:00:00            7200
#> 17 2026-06-12T17:00:00 2026-06-12T18:00:00            3600
#> 18 2026-06-12T18:00:00 2026-06-12T19:00:00            3600
#> 19 2026-06-12T19:00:00 2026-06-12T20:00:00            3600
#> 20 2026-06-12T20:00:00 2026-06-12T21:00:00            3600
#> 21 2026-06-12T21:00:00 2026-06-12T23:00:00            7200
#> 22 2026-06-12T23:00:00 2026-06-13T00:00:00            3600
#>                                                                       title
#> 1                                  NHL Tonight: Stanley Cup Final Post Game
#> 2                                  NHL Tonight: Stanley Cup Final Post Game
#> 3                                  NHL Tonight: Stanley Cup Final Post Game
#> 4                                  NHL Tonight: Stanley Cup Final Post Game
#> 5                                  NHL Tonight: Stanley Cup Final Post Game
#> 6                                  NHL Tonight: Stanley Cup Final Post Game
#> 7                                  NHL Tonight: Stanley Cup Final Post Game
#> 8                                  NHL Tonight: Stanley Cup Final Post Game
#> 9                                  NHL Tonight: Stanley Cup Final Post Game
#> 10                                 NHL Tonight: Stanley Cup Final Post Game
#> 11                                 NHL Tonight: Stanley Cup Final Post Game
#> 12                                 NHL Tonight: Stanley Cup Final Post Game
#> 13 NHL Network Originals: The First NHL Winter Classic, Hockey Goes Outside
#> 14                                 NHL Tonight: Stanley Cup Final Post Game
#> 15                                 NHL Tonight: Stanley Cup Final Post Game
#> 16                                                                 NHL Game
#> 17                                      NHL Tonight: Top 32 Draft Prospects
#> 18                                   NHL Tonight: Stanley Cup Final Edition
#> 19                                   NHL Tonight: Stanley Cup Final Edition
#> 20                                   NHL Tonight: Stanley Cup Final Edition
#> 21                                                                 NHL Game
#> 22                                   NHL Tonight: Stanley Cup Final Edition
#>                                                                                                                                                                                                                                                                                                                                                                                   description
#> 1                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 2                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 3                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 4                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 5                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 6                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 7                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 8                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 9                                                                                                                                                                                                                                                                                                                                                    NHL Tonight: Stanley Cup Final Post Game
#> 10                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Post Game
#> 11                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Post Game
#> 12                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Post Game
#> 13 Marking the anniversary of the first-ever outdoor regular season NHL game, featuring the Pittsburgh Penguins and Buffalo Sabres.  The program takes a look back at Sidney Crosby's break-out performance as the Penguins' captain, the remarkable snow globe-like atmosphere during the game, and the overwhelmingly positive response to whether an outdoor NHL game could be pulled off.
#> 14                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Post Game
#> 15                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: Stanley Cup Final Post Game
#> 16                                                                                                                                                                                                                                                                                     Stanley Cup Final: Vegas Golden Knights at Carolina Hurricanes, Game 5 on 6/11/2026 From Lenovo Center
#> 17                                                                                                                                                                                                                                                                                                                                                        NHL Tonight: Top 32 Draft Prospects
#> 18                                                                                                                                                                                                                                                                                                                                                     NHL Tonight: Stanley Cup Final Edition
#> 19                                                                                                                                                                                                                                                                                                                                                     NHL Tonight: Stanley Cup Final Edition
#> 20                                                                                                                                                                                                                                                                                                                                                     NHL Tonight: Stanley Cup Final Edition
#> 21                                                                                                                                                                                                                                                                                     Stanley Cup Final: Vegas Golden Knights at Carolina Hurricanes, Game 5 on 6/11/2026 From Lenovo Center
#> 22                                                                                                                                                                                                                                                                                                                                                     NHL Tonight: Stanley Cup Final Edition
#>            houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 2  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 3  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 4  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 5  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 6  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 7  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 8  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 9  HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 10 HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 11 HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 12 HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 13   HNHLNORIG18WCAT10            HD                    nhlnetwork.png
#> 14 HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 15 HNHLT26SCPG061126CC            HD                    nhltonight.png
#> 16  H120VGKCAR06112026            HD                           nhl.png
#> 17    HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 18 HNHLT26SCFE061226LV            HD            LIVE    nhltonight.png
#> 19 HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 20 HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 21  H120VGKCAR06112026            HD                           nhl.png
#> 22 HNHLT26SCFE061226CC            HD                    nhltonight.png
#> 
# }
```
