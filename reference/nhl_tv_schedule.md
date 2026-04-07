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
#> [1] "2026-04-07"
#> 
#> $startDate
#> [1] "2026-03-24"
#> 
#> $endDate
#> [1] "2026-04-21"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-04-07T00:00:00 2026-04-07T01:00:00            3600
#> 2  2026-04-07T01:00:00 2026-04-07T01:30:00            1800
#> 3  2026-04-07T01:30:00 2026-04-07T02:00:00            1800
#> 4  2026-04-07T02:00:00 2026-04-07T02:30:00            1800
#> 5  2026-04-07T02:30:00 2026-04-07T03:00:00            1800
#> 6  2026-04-07T03:00:00 2026-04-07T03:30:00            1800
#> 7  2026-04-07T03:30:00 2026-04-07T04:00:00            1800
#> 8  2026-04-07T04:00:00 2026-04-07T04:30:00            1800
#> 9  2026-04-07T04:30:00 2026-04-07T05:00:00            1800
#> 10 2026-04-07T05:00:00 2026-04-07T05:30:00            1800
#> 11 2026-04-07T05:30:00 2026-04-07T06:00:00            1800
#> 12 2026-04-07T06:00:00 2026-04-07T06:30:00            1800
#> 13 2026-04-07T06:30:00 2026-04-07T07:00:00            1800
#> 14 2026-04-07T07:00:00 2026-04-07T07:30:00            1800
#> 15 2026-04-07T07:30:00 2026-04-07T08:00:00            1800
#> 16 2026-04-07T08:00:00 2026-04-07T08:30:00            1800
#> 17 2026-04-07T08:30:00 2026-04-07T09:00:00            1800
#> 18 2026-04-07T09:00:00 2026-04-07T09:30:00            1800
#> 19 2026-04-07T09:30:00 2026-04-07T10:00:00            1800
#> 20 2026-04-07T10:00:00 2026-04-07T10:30:00            1800
#> 21 2026-04-07T10:30:00 2026-04-07T11:00:00            1800
#> 22 2026-04-07T11:00:00 2026-04-07T11:30:00            1800
#> 23 2026-04-07T11:30:00 2026-04-07T12:00:00            1800
#> 24 2026-04-07T12:00:00 2026-04-07T14:00:00            7200
#> 25 2026-04-07T14:00:00 2026-04-07T16:00:00            7200
#> 26 2026-04-07T16:00:00 2026-04-07T17:00:00            3600
#> 27 2026-04-07T17:00:00 2026-04-07T19:00:00            7200
#> 28 2026-04-07T19:00:00 2026-04-07T22:00:00           10800
#> 29 2026-04-07T22:00:00 2026-04-07T23:00:00            3600
#> 30 2026-04-07T23:00:00 2026-04-08T00:00:00            3600
#>                              title
#> 1   On The Fly With Bonus Coverage
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
#> 24                        NHL Game
#> 25                        NHL Game
#> 26        NHL Tonight: First Shift
#> 27                         NHL Now
#> 28                     NHL Tonight
#> 29 NHL Tonight With Bonus Coverage
#> 30                      On The Fly
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
#> 24                                                                                                                                                             Chicago Blackhawks at San Jose Sharks on 4/6/2026 From SAP Center at San Jose
#> 25                                                                                                                                                                     Tampa Bay Lightning at Buffalo Sabres on 4/6/2026 From KeyBank Center
#> 26                                                                                                                                                                                                                  NHL Tonight: First Shift
#> 27                                                                                                                                                                                                                                   NHL Now
#> 28                                                                                                                                                                                                                               NHL Tonight
#> 29                                                     Takes you on the ice with live look-ins, breaking news, real-time scores, and expert analysis. NHL Veterans breakdown highlights and demo game situations. (Live with bonus coverage)
#> 30                                                                                                                                                                                                                                On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60CHISJS04062026            HD            LIVE      onthefly.png
#> 2    HOTF26R040626SOR            HD            LIVE      onthefly.png
#> 3     HOTF26R040626CC            HD                      onthefly.png
#> 4     HOTF26R040626CC            HD                      onthefly.png
#> 5     HOTF26R040626CC            HD                      onthefly.png
#> 6     HOTF26R040626CC            HD                      onthefly.png
#> 7     HOTF26R040626CC            HD                      onthefly.png
#> 8     HOTF26R040626CC            HD                      onthefly.png
#> 9     HOTF26R040626CC            HD                      onthefly.png
#> 10    HOTF26R040626CC            HD                      onthefly.png
#> 11    HOTF26R040626CC            HD                      onthefly.png
#> 12    HOTF26R040626CC            HD                      onthefly.png
#> 13    HOTF26R040626CC            HD                      onthefly.png
#> 14    HOTF26R040626CC            HD                      onthefly.png
#> 15    HOTF26R040626CC            HD                      onthefly.png
#> 16    HOTF26R040626CC            HD                      onthefly.png
#> 17    HOTF26R040626CC            HD                      onthefly.png
#> 18    HOTF26R040626CC            HD                      onthefly.png
#> 19    HOTF26R040626CC            HD                      onthefly.png
#> 20    HOTF26R040626CC            HD                      onthefly.png
#> 21    HOTF26R040626CC            HD                      onthefly.png
#> 22    HOTF26R040626CC            HD                      onthefly.png
#> 23    HOTF26R040626CC            HD                      onthefly.png
#> 24 H120CHISJS04062026            HD                           nhl.png
#> 25 H120TBLBUF04062026            HD                           nhl.png
#> 26  HNHLTFS26040726LV            HD            LIVE                  
#> 27    HNOW26R040726LV            HD            LIVE        nhlnow.png
#> 28  HNHLT26RS040726LV            HD            LIVE    nhltonight.png
#> 29  H60CGYDAL04072026            HD            LIVE    nhltonight.png
#> 30   HOTF26R040726LVA            HD            LIVE      onthefly.png
#> 
# }
```
