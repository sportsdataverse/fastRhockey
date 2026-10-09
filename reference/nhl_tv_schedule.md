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
#> [1] "2026-10-09"
#> 
#> $startDate
#> [1] "2026-09-25"
#> 
#> $endDate
#> [1] "2026-10-22"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-10-09T00:00:00 2026-10-09T01:00:00            3600
#> 2  2026-10-09T01:00:00 2026-10-09T02:00:00            3600
#> 3  2026-10-09T02:00:00 2026-10-09T03:00:00            3600
#> 4  2026-10-09T03:00:00 2026-10-09T04:00:00            3600
#> 5  2026-10-09T04:00:00 2026-10-09T05:00:00            3600
#> 6  2026-10-09T05:00:00 2026-10-09T06:00:00            3600
#> 7  2026-10-09T06:00:00 2026-10-09T07:00:00            3600
#> 8  2026-10-09T07:00:00 2026-10-09T08:00:00            3600
#> 9  2026-10-09T08:00:00 2026-10-09T09:00:00            3600
#> 10 2026-10-09T09:00:00 2026-10-09T10:00:00            3600
#> 11 2026-10-09T10:00:00 2026-10-09T12:00:00            7200
#> 12 2026-10-09T11:00:00 2026-10-09T12:00:00            3600
#> 13 2026-10-09T12:00:00 2026-10-09T14:00:00            7200
#> 14 2026-10-09T14:00:00 2026-10-09T15:00:00            3600
#> 15 2026-10-09T15:00:00 2026-10-09T16:00:00            3600
#> 16 2026-10-09T16:00:00 2026-10-09T17:00:00            3600
#> 17 2026-10-09T17:00:00 2026-10-09T19:00:00            7200
#> 18 2026-10-09T19:00:00 2026-10-09T22:00:00           10800
#> 19 2026-10-09T22:00:00 2026-10-09T23:00:00            3600
#> 20 2026-10-09T23:00:00 2026-10-09T23:30:00            1800
#> 21 2026-10-09T23:30:00 2026-10-10T00:00:00            1800
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
#> 11                              NHL Game
#> 12                            On The Fly
#> 13                              NHL Game
#> 14 Never Offside with Julie & Cat - Ep 1
#> 15 Never Offside with Julie & Cat - Ep 2
#> 16              NHL Tonight: First Shift
#> 17                               NHL Now
#> 18  Regular Season Hockey on NHL Network
#> 19        On The Fly With Bonus Coverage
#> 20                            On The Fly
#> 21                            On The Fly
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   description
#> 1                                                                                                                                                                                                                                                                                                                   Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Chicago Blackhawks at New York Islanders on 10/8/2026 From UBS Arena
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Toronto Maple Leafs at Vegas Golden Knights on 10/8/2026 From T-Mobile Arena
#> 14 Julie and Cat are back for Season 3 and are joined by former NHLer and current ESPN analyst PK Subban, who talks about the start of the 2026 season, fashion and some of his favorite players: Jack Hughes, David Pastrnak and Will Smith. He also shares with Julie & Cat some incredible stories about his former teammate, Carey Price, and offers up some great nuggets during "Hockey Superlatives". The girls start the episode with a little recap of their summer and they close it out with a BIG REVEAL for this upcoming season of the podcast.
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Julie and Cat are joined by Rick Celebrini.
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   NHL Tonight: First Shift
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    NHL Now
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                New York Rangers at Washington Capitals on 10/9/2026 From Capital One Arena
#> 19                                                                                                                                                                                                                                                                                                                  Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60TORVGK10082026            HD            LIVE      onthefly.png
#> 2    HOTF27R100826SOR            HD            LIVE      onthefly.png
#> 3     HOTF27R100826CC            HD                      onthefly.png
#> 4     HOTF27R100826CC            HD                      onthefly.png
#> 5     HOTF27R100826CC            HD                      onthefly.png
#> 6     HOTF27R100826CC            HD                      onthefly.png
#> 7     HOTF27R100826CC            HD                      onthefly.png
#> 8     HOTF27R100826CC            HD                      onthefly.png
#> 9     HOTF27R100826CC            HD                      onthefly.png
#> 10    HOTF27R100826CC            HD                      onthefly.png
#> 11 H120CHINYI10082026            HD                           nhl.png
#> 12    HOTF27R100826CC            HD                      onthefly.png
#> 13 H120TORVGK10082026            HD                           nhl.png
#> 14 HNHLNEVOFFSIDES3E1            HD                    nhlnetwork.png
#> 15 HNHLNEVOFFSIDES3E2            HD                    nhlnetwork.png
#> 16  HNHLTFS27100926LV            HD            LIVE                  
#> 17    HNOW27R100926LV            HD            LIVE        nhlnow.png
#> 18 H180NYRWSH10092026            HD            LIVE           nhl.png
#> 19  H60ANAWPG10092026            HD            LIVE      onthefly.png
#> 20   HOTF27R100926SOR            HD            LIVE      onthefly.png
#> 21    HOTF27R100926CC            HD                      onthefly.png
#> 
# }
```
