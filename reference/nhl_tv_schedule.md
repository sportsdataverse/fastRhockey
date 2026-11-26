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
#> [1] "2026-11-26"
#> 
#> $startDate
#> [1] "2026-11-12"
#> 
#> $endDate
#> [1] "2026-12-10"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-26T00:00:00 2026-11-26T01:00:00            3600
#> 2  2026-11-26T01:00:00 2026-11-26T02:00:00            3600
#> 3  2026-11-26T02:00:00 2026-11-26T03:00:00            3600
#> 4  2026-11-26T03:00:00 2026-11-26T04:00:00            3600
#> 5  2026-11-26T04:00:00 2026-11-26T05:00:00            3600
#> 6  2026-11-26T05:00:00 2026-11-26T06:00:00            3600
#> 7  2026-11-26T06:00:00 2026-11-26T07:00:00            3600
#> 8  2026-11-26T07:00:00 2026-11-26T08:00:00            3600
#> 9  2026-11-26T08:00:00 2026-11-26T09:00:00            3600
#> 10 2026-11-26T09:00:00 2026-11-26T10:00:00            3600
#> 11 2026-11-26T10:00:00 2026-11-26T11:00:00            3600
#> 12 2026-11-26T11:00:00 2026-11-26T12:00:00            3600
#> 13 2026-11-26T12:00:00 2026-11-26T13:00:00            3600
#> 14 2026-11-26T13:00:00 2026-11-26T15:00:00            7200
#> 15 2026-11-26T15:00:00 2026-11-26T17:00:00            7200
#> 16 2026-11-26T17:00:00 2026-11-26T19:00:00            7200
#> 17 2026-11-26T19:00:00 2026-11-26T20:00:00            3600
#> 18 2026-11-26T20:00:00 2026-11-26T21:00:00            3600
#> 19 2026-11-26T21:00:00 2026-11-26T22:00:00            3600
#> 20 2026-11-26T22:00:00 2026-11-27T00:00:00            7200
#>                                                 title
#> 1                      On The Fly With Bonus Coverage
#> 2                                          On The Fly
#> 3                                          On The Fly
#> 4                                          On The Fly
#> 5                                          On The Fly
#> 6                                          On The Fly
#> 7                                          On The Fly
#> 8                                          On The Fly
#> 9                                          On The Fly
#> 10                                         On The Fly
#> 11                                         On The Fly
#> 12 Never Offside with Julie & Cat - Ep 8 - Jenny Taft
#> 13                                         On The Fly
#> 14                                           NHL Game
#> 15                                           NHL Game
#> 16                                           NHL Game
#> 17                    Top Shelf: Thanksgiving Special
#> 18                    Top Shelf: Thanksgiving Special
#> 19  Never Offside with Julie & Cat - Ep 1 - PK Subban
#> 20                                           NHL Game
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           description
#> 1                                                                                                                                                                                                                                                                                                                                           Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         On The Fly
#> 12 Julie and Cat are joined by Fox Sports sideline reporter Jenny Taft to talk all about her hockey life, from growing up in Minnesota as a high school hockey player and daughter of former NHL defenseman John Taft, to life as the wife of former NHLer and current college hockey coach Matt Gilroy, to that time she shared a line with Justin Bieber in a celebrity game while playing for Coach Wayne Gretzky. Near the end, Colorado Avalanche goalie, Wedges Book Wall founder, and recent Never Offside guest Scott Wedgewood, provides his debut book review for the show.
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         On The Fly
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     New Jersey Devils at Columbus Blue Jackets on 11/25/2026 From Nationwide Arena
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Philadelphia Flyers at Washington Capitals on 11/25/2026 From Capital One Arena
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Carolina Hurricanes at Tampa Bay Lightning on 11/25/2026 From Benchmark International Arena
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Top Shelf: Thanksgiving Special
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Top Shelf: Thanksgiving Special
#> 19                         Julie and Cat are back for Season 3 and are joined by former NHLer and current ESPN analyst PK Subban, who talks about the start of the 2026 season, fashion and some of his favorite players: Jack Hughes, David Pastrnak and Will Smith. He also shares with Julie & Cat some incredible stories about his former teammate, Carey Price, and offers up some great nuggets during "Hockey Superlatives". The girls start the episode with a little recap of their summer and they close it out with a BIG REVEAL for this upcoming season of the podcast.
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   San Jose Sharks at Anaheim Ducks on 11/25/2026 From Honda Center
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60SJSANA11252026            HD            LIVE      onthefly.png
#> 2    HOTF27R112526SOR            HD            LIVE      onthefly.png
#> 3     HOTF27R112526CC            HD                      onthefly.png
#> 4     HOTF27R112526CC            HD                      onthefly.png
#> 5     HOTF27R112526CC            HD                      onthefly.png
#> 6     HOTF27R112526CC            HD                      onthefly.png
#> 7     HOTF27R112526CC            HD                      onthefly.png
#> 8     HOTF27R112526CC            HD                      onthefly.png
#> 9     HOTF27R112526CC            HD                      onthefly.png
#> 10    HOTF27R112526CC            HD                      onthefly.png
#> 11    HOTF27R112526CC            HD                      onthefly.png
#> 12 HNHLNEVOFFSIDES3E8            HD                    nhlnetwork.png
#> 13    HOTF27R112526CC            HD                      onthefly.png
#> 14 H120NJDCBJ11252026            HD                           nhl.png
#> 15 H120PHIWSH11252026            HD                           nhl.png
#> 16 H120CARTBL11252026            HD                           nhl.png
#> 17      HTS27112826PT            HD                      topshelf.png
#> 18      HTS27112826CC            HD                      topshelf.png
#> 19 HNHLNEVOFFSIDES3E1            HD                    nhlnetwork.png
#> 20 H120SJSANA11252026            HD                           nhl.png
#> 
# }
```
