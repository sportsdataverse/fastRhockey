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
#> [1] "2026-09-26"
#> 
#> $startDate
#> [1] "2026-09-12"
#> 
#> $endDate
#> [1] "2026-10-09"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-09-26T00:00:00 2026-09-26T00:30:00            1800
#> 2  2026-09-26T00:30:00 2026-09-26T02:30:00            7200
#> 3  2026-09-26T02:30:00 2026-09-26T03:30:00            3600
#> 4  2026-09-26T03:30:00 2026-09-26T04:00:00            1800
#> 5  2026-09-26T04:00:00 2026-09-26T06:00:00            7200
#> 6  2026-09-26T06:00:00 2026-09-26T07:00:00            3600
#> 7  2026-09-26T07:00:00 2026-09-26T08:00:00            3600
#> 8  2026-09-26T08:00:00 2026-09-26T09:00:00            3600
#> 9  2026-09-26T09:00:00 2026-09-26T10:00:00            3600
#> 10 2026-09-26T10:00:00 2026-09-26T11:00:00            3600
#> 11 2026-09-26T11:00:00 2026-09-26T12:00:00            3600
#> 12 2026-09-26T12:00:00 2026-09-26T14:00:00            7200
#> 13 2026-09-26T14:00:00 2026-09-26T15:00:00            3600
#> 14 2026-09-26T15:00:00 2026-09-26T18:00:00           10800
#> 15 2026-09-26T18:00:00 2026-09-26T19:00:00            3600
#> 16 2026-09-26T19:00:00 2026-09-26T22:00:00           10800
#> 17 2026-09-26T22:00:00 2026-09-27T01:00:00           10800
#>                                                    title
#> 1           NHL Network Countdown: Top Lines of All-Time
#> 2                                               NHL Game
#> 3                               Top 10 Goalies Right Now
#> 4  NHL Network Countdown: Top Goal Scorers of the 2000's
#> 5                                               NHL Game
#> 6                  Hawkeytown: Portland to the Pros Ep 1
#> 7                  Hawkeytown: Portland to the Pros Ep 2
#> 8                  Hawkeytown: Portland to the Pros Ep 3
#> 9                  Hawkeytown: Portland to the Pros Ep 4
#> 10                NHL Network Countdown: Top Draft Picks
#> 11       NHL Network Countdown: Top Captains of All-Time
#> 12                                              NHL Game
#> 13                           Top 20 Defensemen Right Now
#> 14                      Pre-Season Hockey on NHL Network
#> 15                              Top 20 Centers Right Now
#> 16                      Pre-Season Hockey on NHL Network
#> 17                      Pre-Season Hockey on NHL Network
#>                                                                 description
#> 1                              NHL Network Countdown: Top Lines of All-Time
#> 2  Boston Bruins at Washington Capitals on 9/25/2026 From Capital One Arena
#> 3                                                  Top 10 Goalies Right Now
#> 4                     NHL Network Countdown: Top Goal Scorers of the 2000's
#> 5        New York Rangers at New York Islanders on 9/25/2026 From UBS Arena
#> 6                                     Hawkeytown: Portland to the Pros Ep 1
#> 7                                     Hawkeytown: Portland to the Pros Ep 2
#> 8                                     Hawkeytown: Portland to the Pros Ep 3
#> 9                                     Hawkeytown: Portland to the Pros Ep 4
#> 10                                   NHL Network Countdown: Top Draft Picks
#> 11                          NHL Network Countdown: Top Captains of All-Time
#> 12      Dallas Stars at Minnesota Wild on 9/25/2026 From Grand Casino Arena
#> 13                                              Top 20 Defensemen Right Now
#> 14   Pittsburgh Penguins at Buffalo Sabres on 9/26/2026 From KeyBank Center
#> 15                                                 Top 20 Centers Right Now
#> 16    St. Louis Blues at Chicago Blackhawks on 9/26/2026 From United Center
#> 17 San Jose Sharks at Vegas Golden Knights on 9/26/2026 From T-Mobile Arena
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1        HNHLNCTDWN12            HD                 nhlncountdown.png
#> 2  H120BOSWSH09252026            HD                           nhl.png
#> 3   H60S26T10GOALRNCC            HD                    nhlnetwork.png
#> 4      HNHLNCTDWN1803            HD                 nhlncountdown.png
#> 5  H120NYRNYI09252026            HD                           nhl.png
#> 6   HNHLWINTERHAWKSE1            HD                    nhlnetwork.png
#> 7   HNHLWINTERHAWKSE2            HD                    nhlnetwork.png
#> 8   HNHLWINTERHAWKSE3            HD                    nhlnetwork.png
#> 9   HNHLWINTERHAWKSE4            HD                    nhlnetwork.png
#> 10       HNHLNCTDWN20            HD                 nhlncountdown.png
#> 11     HNHLNCTDWN1807            HD                 nhlncountdown.png
#> 12 H120DALMIN09252026            HD                           nhl.png
#> 13   H60S26T20DEFRNCC            HD                    nhlnetwork.png
#> 14 H180PITBUF09262026            HD            LIVE           nhl.png
#> 15  H60S26T20CTRSRNCC            HD                    nhlnetwork.png
#> 16 H180STLCHI09262026            HD            LIVE           nhl.png
#> 17 H180SJSVGK09262026            HD            LIVE           nhl.png
#> 
# }
```
