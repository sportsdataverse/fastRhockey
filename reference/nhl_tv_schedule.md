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
#> [1] "2026-05-30"
#> 
#> $startDate
#> [1] "2026-05-16"
#> 
#> $endDate
#> [1] "2026-06-10"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-05-30T00:30:00 2026-05-30T01:30:00            3600
#> 2  2026-05-30T01:30:00 2026-05-30T02:30:00            3600
#> 3  2026-05-30T02:30:00 2026-05-30T03:30:00            3600
#> 4  2026-05-30T03:30:00 2026-05-30T04:00:00            1800
#> 5  2026-05-30T04:00:00 2026-05-30T05:00:00            3600
#> 6  2026-05-30T05:00:00 2026-05-30T06:00:00            3600
#> 7  2026-05-30T06:00:00 2026-05-30T07:00:00            3600
#> 8  2026-05-30T07:00:00 2026-05-30T08:00:00            3600
#> 9  2026-05-30T08:00:00 2026-05-30T09:00:00            3600
#> 10 2026-05-30T09:00:00 2026-05-30T11:30:00            9000
#> 11 2026-05-30T11:30:00 2026-05-30T12:30:00            3600
#> 12 2026-05-30T12:30:00 2026-05-30T13:30:00            3600
#> 13 2026-05-30T13:30:00 2026-05-30T14:00:00            1800
#> 14 2026-05-30T14:00:00 2026-05-30T16:30:00            9000
#> 15 2026-05-30T16:30:00 2026-05-30T17:00:00            1800
#> 16 2026-05-30T17:00:00 2026-05-30T18:00:00            3600
#> 17 2026-05-30T18:00:00 2026-05-30T19:00:00            3600
#> 18 2026-05-30T19:00:00 2026-05-30T20:00:00            3600
#> 19 2026-05-30T20:00:00 2026-05-30T21:00:00            3600
#> 20 2026-05-30T20:30:00 2026-05-30T21:00:00            1800
#> 21 2026-05-30T21:00:00 2026-05-30T22:00:00            3600
#> 22 2026-05-30T21:30:00 2026-05-30T22:00:00            1800
#> 23 2026-05-30T22:00:00 2026-05-30T23:00:00            3600
#> 24 2026-05-30T22:30:00 2026-05-30T23:00:00            1800
#> 25 2026-05-30T23:00:00 2026-05-31T00:00:00            3600
#>                                                          title
#> 1                      NHL Tonight: Conference Final Post Game
#> 2                      NHL Tonight: Conference Final Post Game
#> 3                      NHL Tonight: Conference Final Post Game
#> 4                      NHL Tonight: Conference Final Post Game
#> 5                      NHL Tonight: Conference Final Post Game
#> 6                      NHL Tonight: Conference Final Post Game
#> 7                      NHL Tonight: Conference Final Post Game
#> 8                      NHL Tonight: Conference Final Post Game
#> 9                      NHL Tonight: Conference Final Post Game
#> 10                                IIHF World Championship Game
#> 11                     NHL Tonight: Conference Final Post Game
#> 12                     NHL Tonight: Conference Final Post Game
#> 13               NHL Network Countdown: Greatest Rookie Debuts
#> 14                                     IIHF World Championship
#> 15 NHL Network Originals: Picture Perfect - The 1992-93 Season
#> 16                       Hawkeytown: Portland to the Pros Ep 3
#> 17                       Hawkeytown: Portland to the Pros Ep 4
#> 18                       NHL Tonight: Conference Final Edition
#> 19                       NHL Tonight: Conference Final Edition
#> 20                       NHL Tonight: Conference Final Edition
#> 21                       NHL Tonight: Conference Final Edition
#> 22                       NHL Tonight: Conference Final Edition
#> 23                       Hawkeytown: Portland to the Pros Ep 3
#> 24                       NHL Tonight: Conference Final Edition
#> 25                       Hawkeytown: Portland to the Pros Ep 4
#>                                                                                                                                                                                                                                                                                                                                                                                                          description
#> 1                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 2                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 3                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 4                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 5                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 6                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 7                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 8                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 9                                                                                                                                                                                                                                                                                                                                                                            NHL Tonight: Conference Final Post Game
#> 10                                                                                                                                                                                                                                                                                                                                                     SF1: Switzerland at Norway on 5/30/2026 From Swiss Life Arena
#> 11                                                                                                                                                                                                                                                                                                                                                                           NHL Tonight: Conference Final Post Game
#> 12                                                                                                                                                                                                                                                                                                                                                                           NHL Tonight: Conference Final Post Game
#> 13                                                                                                                                                                                                                                                                                                                                                                     NHL Network Countdown: Greatest Rookie Debuts
#> 14                                                                                                                                                                                                                                                                                                                                                         SF2: Canada at Finland on 5/30/2026 From Swiss Life Arena
#> 15 The 1992-92 NHL season is regarded by many to be the greatest season in NHL history.  Highlighted by thrilling performances from some of the league's all-time greats in Wayne Gretzky, Mario Lemieux, and Patrick Roy and the emergence of rookie sensations Eric Lindros and Teemu Selanne.  The documentary teaches the greatest of the season to new fans and fosters nostalgia to fans who lived through it.
#> 16                                                                                                                                                                                                                                                                                                                                                                             Hawkeytown: Portland to the Pros Ep 3
#> 17                                                                                                                                                                                                                                                                                                                                                                             Hawkeytown: Portland to the Pros Ep 4
#> 18                                                                                                                                                                                                                                                                                                                                                                             NHL Tonight: Conference Final Edition
#> 19                                                                                                                                                                                                                                                                                                                                                                             NHL Tonight: Conference Final Edition
#> 20                                                                                                                                                                                                                                                                                                                                                                             NHL Tonight: Conference Final Edition
#> 21                                                                                                                                                                                                                                                                                                                                                                             NHL Tonight: Conference Final Edition
#> 22                                                                                                                                                                                                                                                                                                                                                                             NHL Tonight: Conference Final Edition
#> 23                                                                                                                                                                                                                                                                                                                                                                             Hawkeytown: Portland to the Pros Ep 3
#> 24                                                                                                                                                                                                                                                                                                                                                                             NHL Tonight: Conference Final Edition
#> 25                                                                                                                                                                                                                                                                                                                                                                             Hawkeytown: Portland to the Pros Ep 4
#>            houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 2  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 3  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 4  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 5  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 6  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 7  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 8  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 9  HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 10  H150SF1SF105302026            HD            LIVE    nhlnetwork.png
#> 11 HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 12 HNHLT26CFPG052926CC            HD                    nhltonight.png
#> 13    HNHLNCTDWN181901            HD                 nhlncountdown.png
#> 14  H150SF2SF205302026            HD            LIVE    nhlnetwork.png
#> 15     H30NHLNORIG9293            HD                    nhlnetwork.png
#> 16   HNHLWINTERHAWKSE3            HD                    nhlnetwork.png
#> 17   HNHLWINTERHAWKSE4            HD                    nhlnetwork.png
#> 18  HNHLT26CFE053026LV            HD            LIVE    nhltonight.png
#> 19  HNHLT26CFE053026CC            HD                    nhltonight.png
#> 20  HNHLT26CFE053026CC            HD                    nhltonight.png
#> 21  HNHLT26CFE053026CC            HD                    nhltonight.png
#> 22  HNHLT26CFE053026CC            HD                    nhltonight.png
#> 23   HNHLWINTERHAWKSE3            HD                    nhlnetwork.png
#> 24  HNHLT26CFE053026CC            HD                    nhltonight.png
#> 25   HNHLWINTERHAWKSE4            HD                    nhlnetwork.png
#> 
# }
```
