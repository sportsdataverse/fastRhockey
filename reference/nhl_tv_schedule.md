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
#> [1] "2026-07-12"
#> 
#> $startDate
#> [1] "2026-06-28"
#> 
#> $endDate
#> [1] "2026-07-26"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-07-12T01:00:00 2026-07-12T03:00:00            7200
#> 2  2026-07-12T03:00:00 2026-07-12T04:00:00            3600
#> 3  2026-07-12T04:00:00 2026-07-12T06:00:00            7200
#> 4  2026-07-12T06:00:00 2026-07-12T08:00:00            7200
#> 5  2026-07-12T08:00:00 2026-07-12T08:30:00            1800
#> 6  2026-07-12T08:30:00 2026-07-12T09:30:00            3600
#> 7  2026-07-12T09:30:00 2026-07-12T11:30:00            7200
#> 8  2026-07-12T11:30:00 2026-07-12T12:00:00            1800
#> 9  2026-07-12T12:00:00 2026-07-12T12:30:00            1800
#> 10 2026-07-12T12:30:00 2026-07-12T13:00:00            1800
#> 11 2026-07-12T13:00:00 2026-07-12T13:30:00            1800
#> 12 2026-07-12T13:30:00 2026-07-12T14:00:00            1800
#> 13 2026-07-12T14:00:00 2026-07-12T16:00:00            7200
#> 14 2026-07-12T16:00:00 2026-07-12T16:30:00            1800
#> 15 2026-07-12T16:30:00 2026-07-12T17:00:00            1800
#> 16 2026-07-12T17:00:00 2026-07-12T17:30:00            1800
#> 17 2026-07-12T17:30:00 2026-07-12T19:30:00            7200
#> 18 2026-07-12T19:30:00 2026-07-12T21:30:00            7200
#> 19 2026-07-12T21:30:00 2026-07-12T23:30:00            7200
#> 20 2026-07-12T23:30:00 2026-07-13T00:00:00            1800
#>                                                                       title
#> 1                                                                  NHL Game
#> 2  NHL Network Originals: The First NHL Winter Classic, Hockey Goes Outside
#> 3                                                                  NHL Game
#> 4                                                                  NHL Game
#> 5                      Becoming Wild: Season 15 - Ep 4 Wild Outdoor Classic
#> 6  NHL Network Originals: The First NHL Winter Classic, Hockey Goes Outside
#> 7                                                                  NHL Game
#> 8                                                    Inside the NBHL - Ep 3
#> 9                                                     Pioneers: Gordie Howe
#> 10                                                    Pioneers: Ted Lindsay
#> 11                                Stanley Cup Film: 1954 - Detroit/Montreal
#> 12                                Stanley Cup Film: 1955 - Detroit/Montreal
#> 13                                                                 NHL Game
#> 14                                                   NHL Trophy Case - Ep 1
#> 15                                                   NHL Trophy Case - Ep 2
#> 16                                                   NHL Trophy Case - Ep 3
#> 17                                                                 NHL Game
#> 18                                                                 NHL Game
#> 19                                                                 NHL Game
#> 20                                                    Pioneers: Gordie Howe
#>                                                                                                                                                                                                                                                                                                                                                                                               description
#> 1                                                                                                                                                                                                                                                                                                            2025 Winter Classic - St. Louis Blues at Chicago Blackhawks on 12/31/2024 From Wrigley Field
#> 2              Marking the anniversary of the first-ever outdoor regular season NHL game, featuring the Pittsburgh Penguins and Buffalo Sabres.  The program takes a look back at Sidney Crosby's break-out performance as the Penguins' captain, the remarkable snow globe-like atmosphere during the game, and the overwhelmingly positive response to whether an outdoor NHL game could be pulled off.
#> 3                                                                                                                                                                                                                                                                                                         2024 Stadium Series - New York Rangers at New York Islanders on 2/18/2024 From MetLife Statdium
#> 4                                                                                                                                                                                                                                                                                                          2025 Stadium Series - Detroit Red Wings at Columbus Blue Jackets on 3/1/2025 From Ohio Stadium
#> 5  The Iowa Wild had January's outdoor game in Hastings, Minnesota circled on the calendar from the beginning of the season. Back in September, however, they had no idea that it would make professional hockey history. In this episode of Becoming Wild--presented by Toyota--follow the team as they travel north to the State of Hockey for a once in a lifetime experience at Hockey Day Minnesota.
#> 6              Marking the anniversary of the first-ever outdoor regular season NHL game, featuring the Pittsburgh Penguins and Buffalo Sabres.  The program takes a look back at Sidney Crosby's break-out performance as the Penguins' captain, the remarkable snow globe-like atmosphere during the game, and the overwhelmingly positive response to whether an outdoor NHL game could be pulled off.
#> 7                                                                                                                                                                                                                                                                                                       2026 Stadium Series - Boston Bruins at Tampa Bay Lightning on 2/1/2026 From Raymond James Stadium
#> 8                                                                                        Inside the NBHL is the National Ball Hockey League's monthly show featuring league-wide power rankings, top plays, and the biggest storylines from across the NBHL. From championship contenders to viral moments, Inside the NBHL delivers an in-depth look at the players, teams, and action shaping the NBHL.
#> 9                                                                                                                                                                                                                                                                                                 An engaging interview with one of the greatest players to ever play the game - Gordie Howe, Mr. Hockey.
#> 10                                                                                                                                                                                                                                                                                                     The legendary Ted Lindsay, a 4-time Stanley Cup Champion, sits down to discuss his storied career.
#> 11                                                                                                                                                                                                                                                                               Led by captain Ted Lindsay, the Red Wings survive a tremendous seven game series with the Canadiens to earn Stanley Cup.
#> 12                                                                                                                                                                                                                                                                              Another seven game series, and another victory for the Red Wings - back-to-back Stanley Cup victories over the Canadiens.
#> 13                                                                                                                                                                                                                                                                                                                                      SuperStar Summer: Mark Messier - Hat Trick CGY @ EDM on 4/14/1983
#> 14    NHL Trophy Case is a limited-run documentary series that takes a deep dive into the history, lore, and special meaning of the NHL's most storied trophies. Episode one of NHL Trophy Case dives into the Hart Trophy, the Vezina Trophy and the Lady Byng, and features bites from marquee NHL players Tom Wilson, Leon Draisaitl, Connor McDavid, Robert Thomas, David Pastrnak and Morgan Rielly.
#> 15                                                                                                                                                                                                                                                                                                                                                                                 NHL Trophy Case - Ep 2
#> 16                                                                                                                                                                                                                                                                                                                                                                                 NHL Trophy Case - Ep 3
#> 17                                                                                                                                                                                                                                                                                                          1st Round: Colorado Avalanche at Los Angeles Kings, Game 4 on 4/26/2026 From Crypto.com Arena
#> 18                                                                                                                                                                                                                                                                                                                    1st Round: Boston Bruins at Buffalo Sabres, Game 5 on 4/28/2026 From KeyBank Center
#> 19                                                                                                                                                                                                                                                                                                                          1st Round: Buffalo Sabres at Boston Bruins, Game 6 on 5/1/2026 From TD Garden
#> 20                                                                                                                                                                                                                                                                                                An engaging interview with one of the greatest players to ever play the game - Gordie Howe, Mr. Hockey.
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1  H120STLCHI12312024            HD                           nhl.png
#> 2   HNHLNORIG18WCAT10            HD                    nhlnetwork.png
#> 3  H120NYRNYI02182024            HD                           nhl.png
#> 4  H120DETCBJ03012025            HD                           nhl.png
#> 5           HBWS15EP4            HD                    nhlnetwork.png
#> 6   HNHLNORIG18WCAT10            HD                    nhlnetwork.png
#> 7  H120BOSTBL02012026            HD                           nhl.png
#> 8        HNHLNBHLEP03            HD                    nhlnetwork.png
#> 9               H0140            HD                    nhlnetwork.png
#> 10              H0285            HD                    nhlnetwork.png
#> 11              N0530            HD                    nhlnetwork.png
#> 12              N0531            HD                    nhlnetwork.png
#> 13 H120SSCGYEDM041483            HD                           nhl.png
#> 14          HNHLTCEP1            HD                    nhlnetwork.png
#> 15          HNHLTCEP2            HD                    nhlnetwork.png
#> 16          HNHLTCEP3            HD                    nhlnetwork.png
#> 17 H120COLLAK04262026            HD                           nhl.png
#> 18 H120BOSBUF04282026            HD                           nhl.png
#> 19 H120BUFBOS05012026            HD                           nhl.png
#> 20              H0140            HD                    nhlnetwork.png
#> 
# }
```
