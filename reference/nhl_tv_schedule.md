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
#> [1] "2026-05-29"
#> 
#> $startDate
#> [1] "2026-05-15"
#> 
#> $endDate
#> [1] "2026-06-10"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-05-29T00:00:00 2026-05-29T01:00:00            3600
#> 2  2026-05-29T01:00:00 2026-05-29T02:00:00            3600
#> 3  2026-05-29T02:00:00 2026-05-29T03:00:00            3600
#> 4  2026-05-29T03:00:00 2026-05-29T04:00:00            3600
#> 5  2026-05-29T04:00:00 2026-05-29T05:00:00            3600
#> 6  2026-05-29T05:00:00 2026-05-29T06:00:00            3600
#> 7  2026-05-29T06:00:00 2026-05-29T07:00:00            3600
#> 8  2026-05-29T07:00:00 2026-05-29T08:00:00            3600
#> 9  2026-05-29T08:00:00 2026-05-29T09:00:00            3600
#> 10 2026-05-29T09:00:00 2026-05-29T10:00:00            3600
#> 11 2026-05-29T10:00:00 2026-05-29T11:00:00            3600
#> 12 2026-05-29T11:00:00 2026-05-29T12:00:00            3600
#> 13 2026-05-29T12:00:00 2026-05-29T14:00:00            7200
#> 14 2026-05-29T14:00:00 2026-05-29T16:00:00            7200
#> 15 2026-05-29T16:00:00 2026-05-29T17:00:00            3600
#> 16 2026-05-29T17:00:00 2026-05-29T19:00:00            7200
#> 17 2026-05-29T19:00:00 2026-05-29T20:00:00            3600
#> 18 2026-05-29T20:00:00 2026-05-29T20:30:00            1800
#> 19 2026-05-29T20:30:00 2026-05-29T21:00:00            1800
#> 20 2026-05-29T21:00:00 2026-05-29T23:30:00            9000
#> 21 2026-05-29T23:30:00 2026-05-30T00:30:00            3600
#>                                      title
#> 1    NHL Tonight: Conference Final Edition
#> 2    NHL Tonight: Conference Final Edition
#> 3    NHL Tonight: Conference Final Edition
#> 4    NHL Tonight: Conference Final Edition
#> 5    NHL Tonight: Conference Final Edition
#> 6    NHL Tonight: Conference Final Edition
#> 7    NHL Tonight: Conference Final Edition
#> 8    NHL Tonight: Conference Final Edition
#> 9    NHL Tonight: Conference Final Edition
#> 10   NHL Tonight: Conference Final Edition
#> 11   NHL Tonight: Conference Final Edition
#> 12   NHL Tonight: Conference Final Edition
#> 13            IIHF World Championship Game
#> 14            IIHF World Championship Game
#> 15                NHL Tonight: First Shift
#> 16                                 NHL Now
#> 17  NHL Tonight: Conference Final Pre-Game
#> 18   NHL Tonight: Conference Final Edition
#> 19   NHL Tonight: Conference Final Edition
#> 20             Canadian Hockey League Game
#> 21 NHL Tonight: Conference Final Post Game
#>                                                    description
#> 1                        NHL Tonight: Conference Final Edition
#> 2                        NHL Tonight: Conference Final Edition
#> 3                        NHL Tonight: Conference Final Edition
#> 4                        NHL Tonight: Conference Final Edition
#> 5                        NHL Tonight: Conference Final Edition
#> 6                        NHL Tonight: Conference Final Edition
#> 7                        NHL Tonight: Conference Final Edition
#> 8                        NHL Tonight: Conference Final Edition
#> 9                        NHL Tonight: Conference Final Edition
#> 10                       NHL Tonight: Conference Final Edition
#> 11                       NHL Tonight: Conference Final Edition
#> 12                       NHL Tonight: Conference Final Edition
#> 13  QF1: Finland at Czechia on 5/28/2026 From Swiss Life Arena
#> 14           QF4: Norway at Latvia on 5/28/2026 From BCR Arena
#> 15                                    NHL Tonight: First Shift
#> 16                                                     NHL Now
#> 17                      NHL Tonight: Conference Final Pre-Game
#> 18                       NHL Tonight: Conference Final Edition
#> 19                       NHL Tonight: Conference Final Edition
#> 20 2026 CHL Memorial Cup: SF: Chicoutimi vs Everett on 5/29/26
#> 21                     NHL Tonight: Conference Final Post Game
#>             houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 2    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 3    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 4    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 5    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 6    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 7    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 8    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 9    HNHLT26CFE052826CC            HD                    nhltonight.png
#> 10   HNHLT26CFE052826CC            HD                    nhltonight.png
#> 11   HNHLT26CFE052826CC            HD                    nhltonight.png
#> 12   HNHLT26CFE052826CC            HD                    nhltonight.png
#> 13   H120QF1QF105282026            HD                    nhlnetwork.png
#> 14   H120QF4QF405282026            HD                    nhlnetwork.png
#> 15    HNHLTFS26052926LV            HD            LIVE                  
#> 16     HNOW26PE052926LV            HD            LIVE        nhlnow.png
#> 17    HNHLT26CF052926LV            HD            LIVE    nhltonight.png
#> 18   HNHLT26CFE052926LV            HD            LIVE    nhltonight.png
#> 19   HNHLT26CFE052926CC            HD                    nhltonight.png
#> 20   H15026MEMCUP052926            HD            LIVE           chl.png
#> 21 HNHLT26CFPG052926SOR            HD            LIVE    nhltonight.png
#> 
# }
```
