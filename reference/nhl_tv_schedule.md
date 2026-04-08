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
#> [1] "2026-04-08"
#> 
#> $startDate
#> [1] "2026-03-25"
#> 
#> $endDate
#> [1] "2026-04-21"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-04-08T00:00:00 2026-04-08T01:00:00            3600
#> 2  2026-04-08T01:00:00 2026-04-08T02:00:00            3600
#> 3  2026-04-08T02:00:00 2026-04-08T03:00:00            3600
#> 4  2026-04-08T03:00:00 2026-04-08T04:00:00            3600
#> 5  2026-04-08T04:00:00 2026-04-08T05:00:00            3600
#> 6  2026-04-08T05:00:00 2026-04-08T06:00:00            3600
#> 7  2026-04-08T06:00:00 2026-04-08T07:00:00            3600
#> 8  2026-04-08T07:00:00 2026-04-08T08:00:00            3600
#> 9  2026-04-08T08:00:00 2026-04-08T09:00:00            3600
#> 10 2026-04-08T09:00:00 2026-04-08T10:00:00            3600
#> 11 2026-04-08T10:00:00 2026-04-08T11:00:00            3600
#> 12 2026-04-08T11:00:00 2026-04-08T12:00:00            3600
#> 13 2026-04-08T12:00:00 2026-04-08T14:00:00            7200
#> 14 2026-04-08T14:00:00 2026-04-08T16:00:00            7200
#> 15 2026-04-08T16:00:00 2026-04-08T17:00:00            3600
#> 16 2026-04-08T17:00:00 2026-04-08T19:30:00            9000
#> 17 2026-04-08T19:30:00 2026-04-08T20:00:00            1800
#> 18 2026-04-08T20:00:00 2026-04-08T20:30:00            1800
#> 19 2026-04-08T20:30:00 2026-04-08T21:00:00            1800
#> 20 2026-04-08T21:00:00 2026-04-08T21:30:00            1800
#> 21 2026-04-08T21:30:00 2026-04-08T22:00:00            1800
#> 22 2026-04-08T22:00:00 2026-04-08T22:30:00            1800
#> 23 2026-04-08T22:30:00 2026-04-08T23:00:00            1800
#> 24 2026-04-08T23:00:00 2026-04-08T23:30:00            1800
#> 25 2026-04-08T23:30:00 2026-04-09T00:00:00            1800
#>                             title
#> 1  On The Fly With Bonus Coverage
#> 2                      On The Fly
#> 3                      On The Fly
#> 4                      On The Fly
#> 5                      On The Fly
#> 6                      On The Fly
#> 7                      On The Fly
#> 8                      On The Fly
#> 9                      On The Fly
#> 10                     On The Fly
#> 11                     On The Fly
#> 12                     On The Fly
#> 13                       NHL Game
#> 14                       NHL Game
#> 15       NHL Tonight: First Shift
#> 16                        NHL Now
#> 17                        NHL Now
#> 18                        NHL Now
#> 19                        NHL Now
#> 20                        NHL Now
#> 21                        NHL Now
#> 22                     On The Fly
#> 23                     On The Fly
#> 24                     On The Fly
#> 25                     On The Fly
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
#> 12                                                                                                                                                                                                                                On The Fly
#> 13                                                                                                                                                                       Boston Bruins at Carolina Hurricanes on 4/7/2026 From Lenovo Center
#> 14                                                                                                                                                          Columbus Blue Jackets at Detroit Red Wings on 4/7/2026 From Little Caesars Arena
#> 15                                                                                                                                                                                                                  NHL Tonight: First Shift
#> 16                                                                                                                                                                                                                                   NHL Now
#> 17                                                                                                                                                                                                                                   NHL Now
#> 18                                                                                                                                                                                                                                   NHL Now
#> 19                                                                                                                                                                                                                                   NHL Now
#> 20                                                                                                                                                                                                                                   NHL Now
#> 21                                                                                                                                                                                                                                   NHL Now
#> 22                                                                                                                                                                                                                                On The Fly
#> 23                                                                                                                                                                                                                                On The Fly
#> 24                                                                                                                                                                                                                                On The Fly
#> 25                                                                                                                                                                                                                                On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60NSHANA04072026            HD            LIVE      onthefly.png
#> 2    HOTF26R040726SOR            HD            LIVE      onthefly.png
#> 3     HOTF26R040726CC            HD                      onthefly.png
#> 4     HOTF26R040726CC            HD                      onthefly.png
#> 5     HOTF26R040726CC            HD                      onthefly.png
#> 6     HOTF26R040726CC            HD                      onthefly.png
#> 7     HOTF26R040726CC            HD                      onthefly.png
#> 8     HOTF26R040726CC            HD                      onthefly.png
#> 9     HOTF26R040726CC            HD                      onthefly.png
#> 10    HOTF26R040726CC            HD                      onthefly.png
#> 11    HOTF26R040726CC            HD                      onthefly.png
#> 12    HOTF26R040726CC            HD                      onthefly.png
#> 13 H120BOSCAR04072026            HD                           nhl.png
#> 14 H120CBJDET04072026            HD                           nhl.png
#> 15  HNHLTFS26040826LV            HD            LIVE                  
#> 16    HNOW26R040826LV            HD            LIVE        nhlnow.png
#> 17    HNOW26R040826CC            HD                        nhlnow.png
#> 18    HNOW26R040826CC            HD                        nhlnow.png
#> 19    HNOW26R040826CC            HD                        nhlnow.png
#> 20    HNOW26R040826CC            HD                        nhlnow.png
#> 21    HNOW26R040826CC            HD                        nhlnow.png
#> 22   HOTF26R040826LVA            HD            LIVE      onthefly.png
#> 23   HOTF26R040826ACC            HD                      onthefly.png
#> 24   HOTF26R040826ACC            HD                      onthefly.png
#> 25   HOTF26R040826ACC            HD                      onthefly.png
#> 
# }
```
