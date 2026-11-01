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
#> [1] "2026-11-01"
#> 
#> $startDate
#> [1] "2026-10-18"
#> 
#> $endDate
#> [1] "2026-11-15"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-01T00:00:00 2026-11-01T01:00:00            3600
#> 2  2026-11-01T01:00:00 2026-11-01T02:00:00            3600
#> 3  2026-11-01T02:00:00 2026-11-01T03:00:00            3600
#> 4  2026-11-01T03:00:00 2026-11-01T04:00:00            3600
#> 5  2026-11-01T04:00:00 2026-11-01T05:00:00            3600
#> 6  2026-11-01T05:00:00 2026-11-01T06:00:00            3600
#> 7  2026-11-01T06:00:00 2026-11-01T07:00:00            3600
#> 8  2026-11-01T07:00:00 2026-11-01T08:00:00            3600
#> 9  2026-11-01T08:00:00 2026-11-01T09:00:00            3600
#> 10 2026-11-01T09:00:00 2026-11-01T10:00:00            3600
#> 11 2026-11-01T10:00:00 2026-11-01T11:00:00            3600
#> 12 2026-11-01T11:00:00 2026-11-01T12:00:00            3600
#> 13 2026-11-01T12:00:00 2026-11-01T13:00:00            3600
#> 14 2026-11-01T13:00:00 2026-11-01T16:00:00           10800
#> 15 2026-11-01T16:00:00 2026-11-01T16:30:00            1800
#> 16 2026-11-01T16:30:00 2026-11-01T17:00:00            1800
#> 17 2026-11-01T17:00:00 2026-11-01T18:00:00            3600
#> 18 2026-11-01T18:00:00 2026-11-01T21:00:00           10800
#> 19 2026-11-01T21:00:00 2026-11-01T21:30:00            1800
#> 20 2026-11-01T21:30:00 2026-11-01T22:00:00            1800
#> 21 2026-11-01T22:00:00 2026-11-01T23:00:00            3600
#> 22 2026-11-01T23:00:00 2026-11-01T23:30:00            1800
#> 23 2026-11-01T23:30:00 2026-11-02T00:00:00            1800
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
#> 14                  NHL Network Showcase
#> 15                             Top Shelf
#> 16                Inside the NBHL - Ep 6
#> 17 Never Offside with Julie & Cat - Ep 5
#> 18  Regular Season Hockey on NHL Network
#> 19                            On The Fly
#> 20                            On The Fly
#> 21        On The Fly With Bonus Coverage
#> 22                            On The Fly
#> 23                            On The Fly
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     description
#> 1                                                                                                                                                                                                                                                                                                                                                     Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Winnipeg Jets at Carolina Hurricanes on 11/1/2026 From Lenovo Center
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Top Shelf
#> 16                                                                                                                                                                                                                                                                             Inside the NBHL is the National Ball Hockey League's monthly show featuring league-wide power rankings, top plays, and the biggest storylines from across the NBHL. From championship contenders to viral moments, Inside the NBHL delivers an in-depth look at the players, teams, and action shaping the NBHL.
#> 17 Julie and Cat are joined by Jason Demers and his wife Jillian Mueller to talk about their love for Hockey Halloween and Jillian's #31daysofhalloween. The girls start the episode with some exciting news... Never Offside manifests yet again!!! They talk about what Matthew Schaefer and the Islanders might be cooking up for their Halloween party. The two talk about how team Halloween parties can bring some teams together and how costume choices can reveal things about players. They close out the episode with Sydney Martin and rank some of this years best Halloween fits.
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Edmonton Oilers at New York Rangers on 11/1/2026 From Madison Square Garden
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 21                                                                                                                                                                                                                                                                                                                                                    Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   On The Fly
#>           houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1   H60CGYCOL10312026            HD            LIVE      onthefly.png
#> 2     HOTF27R103126CC            HD                      onthefly.png
#> 3     HOTF27R103126CC            HD                      onthefly.png
#> 4     HOTF27R103126CC            HD                      onthefly.png
#> 5     HOTF27R103126CC            HD                      onthefly.png
#> 6     HOTF27R103126CC            HD                      onthefly.png
#> 7     HOTF27R103126CC            HD                      onthefly.png
#> 8     HOTF27R103126CC            HD                      onthefly.png
#> 9     HOTF27R103126CC            HD                      onthefly.png
#> 10    HOTF27R103126CC            HD                      onthefly.png
#> 11    HOTF27R103126CC            HD                      onthefly.png
#> 12    HOTF27R103126CC            HD                      onthefly.png
#> 13    HOTF27R103126CC            HD                      onthefly.png
#> 14 H180WPGCAR11012026            HD            LIVE           nhl.png
#> 15      HTS27103126CC            HD                      topshelf.png
#> 16       HNHLNBHLEP06            HD                    nhlnetwork.png
#> 17 HNHLNEVOFFSIDES3E5            HD                    nhlnetwork.png
#> 18 H180EDMNYR11012026            HD            LIVE           nhl.png
#> 19   HOTF27R110126LVA            HD            LIVE      onthefly.png
#> 20   HOTF27R110126ACC            HD                      onthefly.png
#> 21  H60OTTANA11012026            HD            LIVE      onthefly.png
#> 22   HOTF27R110126SOR            HD            LIVE      onthefly.png
#> 23    HOTF27R110126CC            HD                      onthefly.png
#> 
# }
```
