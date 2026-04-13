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
#> [1] "2026-04-13"
#> 
#> $startDate
#> [1] "2026-03-30"
#> 
#> $endDate
#> [1] "2026-04-27"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-04-13T00:00:00 2026-04-13T00:30:00            1800
#> 2  2026-04-13T00:30:00 2026-04-13T01:00:00            1800
#> 3  2026-04-13T01:00:00 2026-04-13T01:30:00            1800
#> 4  2026-04-13T01:30:00 2026-04-13T02:00:00            1800
#> 5  2026-04-13T02:00:00 2026-04-13T02:30:00            1800
#> 6  2026-04-13T02:30:00 2026-04-13T03:00:00            1800
#> 7  2026-04-13T03:00:00 2026-04-13T03:30:00            1800
#> 8  2026-04-13T03:30:00 2026-04-13T04:00:00            1800
#> 9  2026-04-13T04:00:00 2026-04-13T04:30:00            1800
#> 10 2026-04-13T04:30:00 2026-04-13T05:00:00            1800
#> 11 2026-04-13T05:00:00 2026-04-13T05:30:00            1800
#> 12 2026-04-13T05:30:00 2026-04-13T06:00:00            1800
#> 13 2026-04-13T06:00:00 2026-04-13T06:30:00            1800
#> 14 2026-04-13T06:30:00 2026-04-13T07:00:00            1800
#> 15 2026-04-13T07:00:00 2026-04-13T07:30:00            1800
#> 16 2026-04-13T07:30:00 2026-04-13T08:00:00            1800
#> 17 2026-04-13T08:00:00 2026-04-13T08:30:00            1800
#> 18 2026-04-13T08:30:00 2026-04-13T09:00:00            1800
#> 19 2026-04-13T09:00:00 2026-04-13T09:30:00            1800
#> 20 2026-04-13T09:30:00 2026-04-13T10:00:00            1800
#> 21 2026-04-13T10:00:00 2026-04-13T12:00:00            7200
#> 22 2026-04-13T12:00:00 2026-04-13T14:00:00            7200
#> 23 2026-04-13T14:00:00 2026-04-13T16:00:00            7200
#> 24 2026-04-13T16:00:00 2026-04-13T17:00:00            3600
#> 25 2026-04-13T17:00:00 2026-04-13T19:00:00            7200
#> 26 2026-04-13T19:00:00 2026-04-13T22:00:00           10800
#> 27 2026-04-13T22:00:00 2026-04-13T23:00:00            3600
#> 28 2026-04-13T23:00:00 2026-04-14T00:00:00            3600
#>                              title
#> 1                       On The Fly
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
#> 21                        NHL Game
#> 22                        NHL Game
#> 23                     The Whalers
#> 24        NHL Tonight: First Shift
#> 25                         NHL Now
#> 26                     NHL Tonight
#> 27 NHL Tonight With Bonus Coverage
#> 28                      On The Fly
#>                                                                                                                                                                                                            description
#> 1                                                                                                                                                                                                           On The Fly
#> 2                                                                                                                                                                                                           On The Fly
#> 3                                                                                                                                                                                                           On The Fly
#> 4                                                                                                                                                                                                           On The Fly
#> 5                                                                                                                                                                                                           On The Fly
#> 6                                                                                                                                                                                                           On The Fly
#> 7                                                                                                                                                                                                           On The Fly
#> 8                                                                                                                                                                                                           On The Fly
#> 9                                                                                                                                                                                                           On The Fly
#> 10                                                                                                                                                                                                          On The Fly
#> 11                                                                                                                                                                                                          On The Fly
#> 12                                                                                                                                                                                                          On The Fly
#> 13                                                                                                                                                                                                          On The Fly
#> 14                                                                                                                                                                                                          On The Fly
#> 15                                                                                                                                                                                                          On The Fly
#> 16                                                                                                                                                                                                          On The Fly
#> 17                                                                                                                                                                                                          On The Fly
#> 18                                                                                                                                                                                                          On The Fly
#> 19                                                                                                                                                                                                          On The Fly
#> 20                                                                                                                                                                                                          On The Fly
#> 21                                                                                                                                           Boston Bruins at Columbus Blue Jackets on 4/12/2026 From Nationwide Arena
#> 22                                                                                                                                      Pittsburgh Penguins at Washington Capitals on 4/12/2026 From Capital One Arena
#> 23 Almost three decades after leaving Connecticut, the Hartford Whalers remain a fixture in the sports world. This is the story of how the team came to be, why they moved and what makes them a throwback phenomenon.
#> 24                                                                                                                                                                                            NHL Tonight: First Shift
#> 25                                                                                                                                                                                                             NHL Now
#> 26                                                                                                                                                                                                         NHL Tonight
#> 27                               Takes you on the ice with live look-ins, breaking news, real-time scores, and expert analysis. NHL Veterans breakdown highlights and demo game situations. (Live with bonus coverage)
#> 28                                                                                                                                                                                                          On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1    HOTF26R041226SOR            HD            LIVE      onthefly.png
#> 2     HOTF26R041226CC            HD                      onthefly.png
#> 3     HOTF26R041226CC            HD                      onthefly.png
#> 4     HOTF26R041226CC            HD                      onthefly.png
#> 5     HOTF26R041226CC            HD                      onthefly.png
#> 6     HOTF26R041226CC            HD                      onthefly.png
#> 7     HOTF26R041226CC            HD                      onthefly.png
#> 8     HOTF26R041226CC            HD                      onthefly.png
#> 9     HOTF26R041226CC            HD                      onthefly.png
#> 10    HOTF26R041226CC            HD                      onthefly.png
#> 11    HOTF26R041226CC            HD                      onthefly.png
#> 12    HOTF26R041226CC            HD                      onthefly.png
#> 13    HOTF26R041226CC            HD                      onthefly.png
#> 14    HOTF26R041226CC            HD                      onthefly.png
#> 15    HOTF26R041226CC            HD                      onthefly.png
#> 16    HOTF26R041226CC            HD                      onthefly.png
#> 17    HOTF26R041226CC            HD                      onthefly.png
#> 18    HOTF26R041226CC            HD                      onthefly.png
#> 19    HOTF26R041226CC            HD                      onthefly.png
#> 20    HOTF26R041226CC            HD                      onthefly.png
#> 21 H120BOSCBJ04122026            HD                           nhl.png
#> 22 H120PITWSH04122026            HD                           nhl.png
#> 23        HNHLWHALERS            HD                    nhlnetwork.png
#> 24  HNHLTFS26041326LV            HD            LIVE                  
#> 25    HNOW26R041326LV            HD            LIVE        nhlnow.png
#> 26  HNHLT26RS041326LV            HD            LIVE    nhltonight.png
#> 27  H60SJSNSH04132026            HD            LIVE    nhltonight.png
#> 28   HOTF26R041326LVA            HD            LIVE      onthefly.png
#> 
# }
```
