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

Returns a list with TV schedule data.

## Examples

``` r
# \donttest{
  try(nhl_tv_schedule())
#> $date
#> [1] "2026-05-11"
#> 
#> $startDate
#> [1] "2026-04-27"
#> 
#> $endDate
#> [1] "2026-05-25"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-05-11T00:00:00 2026-05-11T00:30:00            1800
#> 2  2026-05-11T00:30:00 2026-05-11T01:30:00            3600
#> 3  2026-05-11T01:00:00 2026-05-11T02:00:00            3600
#> 4  2026-05-11T01:30:00 2026-05-11T02:30:00            3600
#> 5  2026-05-11T02:00:00 2026-05-11T03:00:00            3600
#> 6  2026-05-11T02:30:00 2026-05-11T03:30:00            3600
#> 7  2026-05-11T03:00:00 2026-05-11T04:00:00            3600
#> 8  2026-05-11T03:30:00 2026-05-11T04:00:00            1800
#> 9  2026-05-11T04:00:00 2026-05-11T05:00:00            3600
#> 10 2026-05-11T05:00:00 2026-05-11T06:00:00            3600
#> 11 2026-05-11T06:00:00 2026-05-11T07:00:00            3600
#> 12 2026-05-11T07:00:00 2026-05-11T08:00:00            3600
#> 13 2026-05-11T08:00:00 2026-05-11T09:00:00            3600
#> 14 2026-05-11T09:00:00 2026-05-11T10:00:00            3600
#> 15 2026-05-11T10:00:00 2026-05-11T11:00:00            3600
#> 16 2026-05-11T11:00:00 2026-05-11T12:00:00            3600
#> 17 2026-05-11T12:00:00 2026-05-11T14:00:00            7200
#> 18 2026-05-11T14:00:00 2026-05-11T16:00:00            7200
#> 19 2026-05-11T16:00:00 2026-05-11T17:00:00            3600
#> 20 2026-05-11T17:00:00 2026-05-11T19:00:00            7200
#> 21 2026-05-11T19:00:00 2026-05-11T20:00:00            3600
#> 22 2026-05-11T19:30:00 2026-05-11T20:00:00            1800
#> 23 2026-05-11T20:00:00 2026-05-11T20:30:00            1800
#> 24 2026-05-11T20:30:00 2026-05-11T21:00:00            1800
#> 25 2026-05-11T21:00:00 2026-05-11T21:30:00            1800
#> 26 2026-05-11T21:30:00 2026-05-11T22:00:00            1800
#> 27 2026-05-11T22:00:00 2026-05-11T22:30:00            1800
#> 28 2026-05-11T22:30:00 2026-05-11T23:00:00            1800
#> 29 2026-05-11T23:00:00 2026-05-12T00:00:00            3600
#> 30 2026-05-11T23:30:00 2026-05-12T00:00:00            1800
#>                           title
#> 1  NHL Tonight: Playoff Edition
#> 2  NHL Tonight: Playoff Edition
#> 3  NHL Tonight: Playoff Edition
#> 4  NHL Tonight: Playoff Edition
#> 5  NHL Tonight: Playoff Edition
#> 6  NHL Tonight: Playoff Edition
#> 7  NHL Tonight: Playoff Edition
#> 8  NHL Tonight: Playoff Edition
#> 9  NHL Tonight: Playoff Edition
#> 10 NHL Tonight: Playoff Edition
#> 11 NHL Tonight: Playoff Edition
#> 12 NHL Tonight: Playoff Edition
#> 13 NHL Tonight: Playoff Edition
#> 14 NHL Tonight: Playoff Edition
#> 15 NHL Tonight: Playoff Edition
#> 16 NHL Tonight: Playoff Edition
#> 17                     NHL Game
#> 18                     NHL Game
#> 19     NHL Tonight: First Shift
#> 20                      NHL Now
#> 21 NHL Tonight: Playoff Edition
#> 22                      NHL Now
#> 23 NHL Tonight: Playoff Edition
#> 24 NHL Tonight: Playoff Edition
#> 25 NHL Tonight: Playoff Edition
#> 26 NHL Tonight: Playoff Edition
#> 27 NHL Tonight: Playoff Edition
#> 28 NHL Tonight: Playoff Edition
#> 29 NHL Tonight: Playoff Edition
#> 30 NHL Tonight: Playoff Edition
#>                                                                                description
#> 1                                                             NHL Tonight: Playoff Edition
#> 2                                                             NHL Tonight: Playoff Edition
#> 3                                                             NHL Tonight: Playoff Edition
#> 4                                                             NHL Tonight: Playoff Edition
#> 5                                                             NHL Tonight: Playoff Edition
#> 6                                                             NHL Tonight: Playoff Edition
#> 7                                                             NHL Tonight: Playoff Edition
#> 8                                                             NHL Tonight: Playoff Edition
#> 9                                                             NHL Tonight: Playoff Edition
#> 10                                                            NHL Tonight: Playoff Edition
#> 11                                                            NHL Tonight: Playoff Edition
#> 12                                                            NHL Tonight: Playoff Edition
#> 13                                                            NHL Tonight: Playoff Edition
#> 14                                                            NHL Tonight: Playoff Edition
#> 15                                                            NHL Tonight: Playoff Edition
#> 16                                                            NHL Tonight: Playoff Edition
#> 17   2nd Round: Buffalo Sabres at Montreal Canadiens, Game 3 on 5/10/2026 From Centre Bell
#> 18 2nd Round: Vegas Golden Knights at Anaheim Ducks, Game 4 on 5/10/2026 From Honda Center
#> 19                                                                NHL Tonight: First Shift
#> 20                                                                                 NHL Now
#> 21                                                            NHL Tonight: Playoff Edition
#> 22                                                                                 NHL Now
#> 23                                                            NHL Tonight: Playoff Edition
#> 24                                                            NHL Tonight: Playoff Edition
#> 25                                                            NHL Tonight: Playoff Edition
#> 26                                                            NHL Tonight: Playoff Edition
#> 27                                                            NHL Tonight: Playoff Edition
#> 28                                                            NHL Tonight: Playoff Edition
#> 29                                                            NHL Tonight: Playoff Edition
#> 30                                                            NHL Tonight: Playoff Edition
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  HNHLT26PE051026DCC            HD                    nhltonight.png
#> 2  HNHLT26PE051026SOR            HD            LIVE    nhltonight.png
#> 3   HNHLT26PE051026CC            HD                    nhltonight.png
#> 4   HNHLT26PE051026CC            HD                    nhltonight.png
#> 5   HNHLT26PE051026CC            HD                    nhltonight.png
#> 6   HNHLT26PE051026CC            HD                    nhltonight.png
#> 7   HNHLT26PE051026CC            HD                    nhltonight.png
#> 8   HNHLT26PE051026CC            HD                    nhltonight.png
#> 9   HNHLT26PE051026CC            HD                    nhltonight.png
#> 10  HNHLT26PE051026CC            HD                    nhltonight.png
#> 11  HNHLT26PE051026CC            HD                    nhltonight.png
#> 12  HNHLT26PE051026CC            HD                    nhltonight.png
#> 13  HNHLT26PE051026CC            HD                    nhltonight.png
#> 14  HNHLT26PE051026CC            HD                    nhltonight.png
#> 15  HNHLT26PE051026CC            HD                    nhltonight.png
#> 16  HNHLT26PE051026CC            HD                    nhltonight.png
#> 17 H120BUFMTL05102026            HD                           nhl.png
#> 18 H120VGKANA05102026            HD                           nhl.png
#> 19  HNHLTFS26051126LV            HD            LIVE                  
#> 20   HNOW26PE051126LV            HD            LIVE        nhlnow.png
#> 21 HNHLT26PE051126LVA            HD            LIVE    nhltonight.png
#> 22   HNOW26PE051126CC            HD                        nhlnow.png
#> 23 HNHLT26PE051126LVB            HD            LIVE    nhltonight.png
#> 24 HNHLT26PE051126BCC            HD                    nhltonight.png
#> 25 HNHLT26PE051126BCC            HD                    nhltonight.png
#> 26 HNHLT26PE051126BCC            HD                    nhltonight.png
#> 27 HNHLT26PE051126BCC            HD                    nhltonight.png
#> 28 HNHLT26PE051126BCC            HD                    nhltonight.png
#> 29 HNHLT26PE051126SOR            HD            LIVE    nhltonight.png
#> 30 HNHLT26PE051126BCC            HD                    nhltonight.png
#> 
# }
```
