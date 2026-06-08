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
#> [1] "2026-06-08"
#> 
#> $startDate
#> [1] "2026-05-25"
#> 
#> $endDate
#> [1] "2026-06-22"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-06-08T01:00:00 2026-06-08T02:00:00            3600
#> 2  2026-06-08T02:00:00 2026-06-08T03:00:00            3600
#> 3  2026-06-08T03:00:00 2026-06-08T04:00:00            3600
#> 4  2026-06-08T04:00:00 2026-06-08T05:00:00            3600
#> 5  2026-06-08T05:00:00 2026-06-08T06:00:00            3600
#> 6  2026-06-08T06:00:00 2026-06-08T07:00:00            3600
#> 7  2026-06-08T07:00:00 2026-06-08T08:00:00            3600
#> 8  2026-06-08T08:00:00 2026-06-08T09:00:00            3600
#> 9  2026-06-08T09:00:00 2026-06-08T10:00:00            3600
#> 10 2026-06-08T10:00:00 2026-06-08T11:00:00            3600
#> 11 2026-06-08T11:00:00 2026-06-08T12:00:00            3600
#> 12 2026-06-08T12:00:00 2026-06-08T13:00:00            3600
#> 13 2026-06-08T13:00:00 2026-06-08T15:00:00            7200
#> 14 2026-06-08T15:00:00 2026-06-08T17:00:00            7200
#> 15 2026-06-08T17:00:00 2026-06-08T18:00:00            3600
#> 16 2026-06-08T18:00:00 2026-06-08T19:00:00            3600
#> 17 2026-06-08T19:00:00 2026-06-08T20:00:00            3600
#> 18 2026-06-08T20:00:00 2026-06-08T21:00:00            3600
#> 19 2026-06-08T21:00:00 2026-06-08T23:00:00            7200
#> 20 2026-06-08T23:00:00 2026-06-09T00:00:00            3600
#>                                     title
#> 1  NHL Tonight: Stanley Cup Final Edition
#> 2     NHL Tonight: Top 32 Draft Prospects
#> 3  NHL Tonight: Stanley Cup Final Edition
#> 4     NHL Tonight: Top 32 Draft Prospects
#> 5  NHL Tonight: Stanley Cup Final Edition
#> 6  NHL Tonight: Stanley Cup Final Edition
#> 7  NHL Tonight: Stanley Cup Final Edition
#> 8  NHL Tonight: Stanley Cup Final Edition
#> 9  NHL Tonight: Stanley Cup Final Edition
#> 10 NHL Tonight: Stanley Cup Final Edition
#> 11 NHL Tonight: Stanley Cup Final Edition
#> 12 NHL Tonight: Stanley Cup Final Edition
#> 13                               NHL Game
#> 14                               NHL Game
#> 15                                NHL Now
#> 16 NHL Tonight: Stanley Cup Final Edition
#> 17    NHL Tonight: Top 32 Draft Prospects
#> 18 NHL Tonight: Stanley Cup Final Edition
#> 19                               NHL Game
#> 20    NHL Tonight: Top 32 Draft Prospects
#>                                                                                               description
#> 1                                                                  NHL Tonight: Stanley Cup Final Edition
#> 2                                                                     NHL Tonight: Top 32 Draft Prospects
#> 3                                                                  NHL Tonight: Stanley Cup Final Edition
#> 4                                                                     NHL Tonight: Top 32 Draft Prospects
#> 5                                                                  NHL Tonight: Stanley Cup Final Edition
#> 6                                                                  NHL Tonight: Stanley Cup Final Edition
#> 7                                                                  NHL Tonight: Stanley Cup Final Edition
#> 8                                                                  NHL Tonight: Stanley Cup Final Edition
#> 9                                                                  NHL Tonight: Stanley Cup Final Edition
#> 10                                                                 NHL Tonight: Stanley Cup Final Edition
#> 11                                                                 NHL Tonight: Stanley Cup Final Edition
#> 12                                                                 NHL Tonight: Stanley Cup Final Edition
#> 13   Conference Finals: Montreal Canadiens at Carolina Hurricanes, Game 5 on 5/29/2026 From Lenovo Center
#> 14 Conference Finals: Colorado Avalanche at Vegas Golden Knights, Game 4 on 5/26/2026 From T-Mobile Arena
#> 15                                                                                                NHL Now
#> 16                                                                 NHL Tonight: Stanley Cup Final Edition
#> 17                                                                    NHL Tonight: Top 32 Draft Prospects
#> 18                                                                 NHL Tonight: Stanley Cup Final Edition
#> 19 Stanley Cup Final: Carolina Hurricanes at Vegas Golden Knights, Game 3 on 6/6/2026 From T-Mobile Arena
#> 20                                                                    NHL Tonight: Top 32 Draft Prospects
#>            houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 2     HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 3  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 4     HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 5  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 6  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 7  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 8  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 9  HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 10 HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 11 HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 12 HNHLT26SCFE060726CC            HD                    nhltonight.png
#> 13  H120MTLCAR05292026            HD                           nhl.png
#> 14  H120COLVGK05262026            HD                           nhl.png
#> 15    HNOW26PE060826LV            HD            LIVE        nhlnow.png
#> 16 HNHLT26SCFE060826LV            HD            LIVE    nhltonight.png
#> 17    HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 18 HNHLT26SCFE060826CC            HD                    nhltonight.png
#> 19  H120CARVGK06062026            HD                           nhl.png
#> 20    HNHLT26PE32DFTCC            HD                    nhltonight.png
#> 
# }
```
