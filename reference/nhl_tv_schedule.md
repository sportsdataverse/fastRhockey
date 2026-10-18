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
#> [1] "2026-10-18"
#> 
#> $startDate
#> [1] "2026-10-04"
#> 
#> $endDate
#> [1] "2026-10-31"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-10-18T00:00:00 2026-10-18T01:00:00            3600
#> 2  2026-10-18T01:00:00 2026-10-18T02:00:00            3600
#> 3  2026-10-18T02:00:00 2026-10-18T03:00:00            3600
#> 4  2026-10-18T03:00:00 2026-10-18T04:00:00            3600
#> 5  2026-10-18T04:00:00 2026-10-18T05:00:00            3600
#> 6  2026-10-18T05:00:00 2026-10-18T06:00:00            3600
#> 7  2026-10-18T06:00:00 2026-10-18T07:00:00            3600
#> 8  2026-10-18T07:00:00 2026-10-18T08:00:00            3600
#> 9  2026-10-18T08:00:00 2026-10-18T09:00:00            3600
#> 10 2026-10-18T09:00:00 2026-10-18T10:00:00            3600
#> 11 2026-10-18T10:00:00 2026-10-18T11:00:00            3600
#> 12 2026-10-18T11:00:00 2026-10-18T12:00:00            3600
#> 13 2026-10-18T12:00:00 2026-10-18T13:00:00            3600
#> 14 2026-10-18T13:00:00 2026-10-18T14:00:00            3600
#> 15 2026-10-18T14:00:00 2026-10-18T15:00:00            3600
#> 16 2026-10-18T15:00:00 2026-10-18T17:00:00            7200
#> 17 2026-10-18T17:00:00 2026-10-18T19:00:00            7200
#> 18 2026-10-18T19:00:00 2026-10-18T20:00:00            3600
#> 19 2026-10-18T19:30:00 2026-10-18T20:00:00            1800
#> 20 2026-10-18T20:00:00 2026-10-18T21:00:00            3600
#> 21 2026-10-18T21:00:00 2026-10-18T21:30:00            1800
#> 22 2026-10-18T21:30:00 2026-10-18T22:00:00            1800
#> 23 2026-10-18T22:00:00 2026-10-18T23:00:00            3600
#> 24 2026-10-18T23:00:00 2026-10-18T23:30:00            1800
#> 25 2026-10-18T23:30:00 2026-10-19T00:00:00            1800
#>                                    title
#> 1         On The Fly With Bonus Coverage
#> 2                             On The Fly
#> 3                             On The Fly
#> 4                             On The Fly
#> 5                             On The Fly
#> 6                             On The Fly
#> 7                             On The Fly
#> 8                             On The Fly
#> 9                             On The Fly
#> 10                            On The Fly
#> 11                            On The Fly
#> 12                            On The Fly
#> 13                            On The Fly
#> 14                            On The Fly
#> 15 Never Offside with Julie & Cat - Ep 3
#> 16                              NHL Game
#> 17                              NHL Game
#> 18                Inside the NBHL - Ep 5
#> 19                Inside the NBHL - Ep 6
#> 20        On The Fly With Bonus Coverage
#> 21                            On The Fly
#> 22                            On The Fly
#> 23        On The Fly With Bonus Coverage
#> 24                            On The Fly
#> 25                            On The Fly
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             description
#> 1                                                                                                                                                                                                                                                                                                                                                                             Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 15 Cat is joined by Sydney Martin, making her guest co-hosting debut, for an interview with Todd Schaefer, father of New York Islanders Calder Trophy-winning defenseman Matthew Schaefer. Todd shares stories about life as a hockey dad, how Matthew found a home with the Martins on Long Island and his newfound celebrity status at Islanders games. Sydney shares behind-the-scenes stories of life at home with two hockey players, including the infamous laundry story that earned Schaef a ban from the laundry room. At the end of the show, Matthew makes a special guest appearance, just before nap time.
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Buffalo Sabres at Montreal Canadiens on 10/17/2026 From Centre Bell
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        New Jersey Devils at Washington Capitals on 10/17/2026 From Capital One Arena
#> 18                                                                                                                                                                                                                                                                                                     Inside the NBHL is the National Ball Hockey League's monthly show featuring league-wide power rankings, top plays, and the biggest storylines from across the NBHL. From championship contenders to viral moments, Inside the NBHL delivers an in-depth look at the players, teams, and action shaping the NBHL.
#> 19                                                                                                                                                                                                                                                                                                     Inside the NBHL is the National Ball Hockey League's monthly show featuring league-wide power rankings, top plays, and the biggest storylines from across the NBHL. From championship contenders to viral moments, Inside the NBHL delivers an in-depth look at the players, teams, and action shaping the NBHL.
#> 20                                                                                                                                                                                                                                                                                                                                                                            Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 23                                                                                                                                                                                                                                                                                                                                                                            Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#> 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60PITVGK10172026            HD            LIVE      onthefly.png
#> 2    HOTF27R101726SOR            HD            LIVE      onthefly.png
#> 3     HOTF27R101726CC            HD                      onthefly.png
#> 4     HOTF27R101726CC            HD                      onthefly.png
#> 5     HOTF27R101726CC            HD                      onthefly.png
#> 6     HOTF27R101726CC            HD                      onthefly.png
#> 7     HOTF27R101726CC            HD                      onthefly.png
#> 8     HOTF27R101726CC            HD                      onthefly.png
#> 9     HOTF27R101726CC            HD                      onthefly.png
#> 10    HOTF27R101726CC            HD                      onthefly.png
#> 11    HOTF27R101726CC            HD                      onthefly.png
#> 12    HOTF27R101726CC            HD                      onthefly.png
#> 13    HOTF27R101726CC            HD                      onthefly.png
#> 14    HOTF27R101726CC            HD                      onthefly.png
#> 15 HNHLNEVOFFSIDES3E3            HD                    nhlnetwork.png
#> 16 H120BUFMTL10172026            HD                           nhl.png
#> 17 H120NJDWSH10172026            HD                           nhl.png
#> 18       HNHLNBHLEP05            HD                    nhlnetwork.png
#> 19       HNHLNBHLEP06            HD                    nhlnetwork.png
#> 20  H60CBJMIN10182026            HD            LIVE      onthefly.png
#> 21   HOTF27R101826LVA            HD            LIVE      onthefly.png
#> 22   HOTF27R101826ACC            HD                      onthefly.png
#> 23  H60CARCGY10182026            HD            LIVE      onthefly.png
#> 24   HOTF27R101826SOR            HD            LIVE      onthefly.png
#> 25    HOTF27R101826CC            HD                      onthefly.png
#> 
# }
```
