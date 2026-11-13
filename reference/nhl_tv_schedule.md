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
#> [1] "2026-11-13"
#> 
#> $startDate
#> [1] "2026-10-30"
#> 
#> $endDate
#> [1] "2026-11-27"
#> 
#> $broadcasts
#>              startTime             endTime durationSeconds
#> 1  2026-11-13T00:00:00 2026-11-13T01:00:00            3600
#> 2  2026-11-13T01:00:00 2026-11-13T02:00:00            3600
#> 3  2026-11-13T02:00:00 2026-11-13T03:00:00            3600
#> 4  2026-11-13T03:00:00 2026-11-13T04:00:00            3600
#> 5  2026-11-13T04:00:00 2026-11-13T05:00:00            3600
#> 6  2026-11-13T05:00:00 2026-11-13T06:00:00            3600
#> 7  2026-11-13T06:00:00 2026-11-13T07:00:00            3600
#> 8  2026-11-13T07:00:00 2026-11-13T08:00:00            3600
#> 9  2026-11-13T08:00:00 2026-11-13T09:00:00            3600
#> 10 2026-11-13T09:00:00 2026-11-13T11:30:00            9000
#> 11 2026-11-13T11:30:00 2026-11-13T12:00:00            1800
#> 12 2026-11-13T12:00:00 2026-11-13T13:00:00            3600
#> 13 2026-11-13T13:00:00 2026-11-13T15:30:00            9000
#> 14 2026-11-13T15:30:00 2026-11-13T16:00:00            1800
#> 15 2026-11-13T16:00:00 2026-11-13T17:00:00            3600
#> 16 2026-11-13T17:00:00 2026-11-13T19:00:00            7200
#> 17 2026-11-13T19:00:00 2026-11-13T20:00:00            3600
#> 18 2026-11-13T20:00:00 2026-11-13T22:00:00            7200
#> 19 2026-11-13T22:00:00 2026-11-13T23:00:00            3600
#> 20 2026-11-13T23:00:00 2026-11-14T00:00:00            3600
#>                                                       title
#> 1                                                On The Fly
#> 2                                                On The Fly
#> 3                                                On The Fly
#> 4                                                On The Fly
#> 5                                                On The Fly
#> 6                                                On The Fly
#> 7                                                On The Fly
#> 8                                                On The Fly
#> 9                                                On The Fly
#> 10                          IIHF Women's World Championship
#> 11        Breaking Down Barriers: Girls Hockey In The North
#> 12                                               On The Fly
#> 13                          IIHF Women's World Championship
#> 14 Breaking Down Barriers: S3 - The Future Of Girls' Hockey
#> 15                                 NHL Tonight: First Shift
#> 16                                                  NHL Now
#> 17                    Never Offside with Julie & Cat - Ep 6
#> 18              2026 Hockey Hall of Fame Induction Ceremony
#> 19                                               On The Fly
#> 20                           On The Fly With Bonus Coverage
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                description
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               On The Fly
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               2026 IIHF Women's World Championship: QF1 - Canada vs Czechia on 11/13/26
#> 11                                                                                                                                                                                                                                                                         In this episode, viewers travel to Canada's far North to meet the Kuugaruk Dynamite Girls Hockey Team. Thanks to the combined efforts of local teacher Emma Keagan and Project North, girls hockey has been re-established in this small Nunavut community for the first time in over 30 years.
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              On The Fly
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  2026 IIHF Women's World Championship: QF2 - USA vs Denmark on 11/13/26
#> 14                                                                                                                                                                                                                                                                                                                                                                                                              Exploring the challenges girls and women face in hockey, including ice time and retention issues, while highlighting Gillian Apps' leadership in the sport
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NHL Tonight: First Shift
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 NHL Now
#> 17 Cat and Julie are joined by Colorado Avalanche goaltender Scott Wedgewood and his wife, Brittany, to discuss life as an NHL goalie, superstitions of a goalie's wife, and their fast-growing book club Wedge's Book Wall, a favorite among Avs fans. The conversation covers the origin of their love story, how the couple bonded over reading, Scott's favorite times to squeeze in a few game-day chapters, the founding of their book club, and upcoming Book Nights at Ball Arena, while Cat and Julie enjoy a few late-night sips of their new favorite beverage.
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             2026 Hockey Hall of Fame Induction Ceremony
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              On The Fly
#> 20                                                                                                                                                                                                                                                                                                                               Missed the game? On The Fly conveniently recaps all games, every night. Post game interviews, highlights, expert analysis, and press conferences keep you in touch with the latest headlines after every game. (Live with bonus coverage)
#>             houseNumber broadcastType broadcastStatus broadcastImageUrl
#> 1      HOTF27R111226LVA            HD            LIVE      onthefly.png
#> 2      HOTF27R111226SOR            HD            LIVE      onthefly.png
#> 3       HOTF27R111226CC            HD                      onthefly.png
#> 4       HOTF27R111226CC            HD                      onthefly.png
#> 5       HOTF27R111226CC            HD                      onthefly.png
#> 6       HOTF27R111226CC            HD                      onthefly.png
#> 7       HOTF27R111226CC            HD                      onthefly.png
#> 8       HOTF27R111226CC            HD                      onthefly.png
#> 9       HOTF27R111226CC            HD                      onthefly.png
#> 10 H150WW26QF1QF1111326            HD            LIVE    nhlnetwork.png
#> 11          HTSNBDBEP03            HD                    nhlnetwork.png
#> 12      HOTF27R111226CC            HD                      onthefly.png
#> 13 H150WW26QF2QF2111326            HD            LIVE    nhlnetwork.png
#> 14        HTSNBDBS3EP08            HD                    nhlnetwork.png
#> 15    HNHLTFS27111326LV            HD            LIVE                  
#> 16      HNOW27R111326LV            HD            LIVE        nhlnow.png
#> 17   HNHLNEVOFFSIDES3E6            HD                    nhlnetwork.png
#> 18    H26HHOFCEREMONYCC            HD                          hhof.png
#> 19     HOTF27R111326LVA            HD            LIVE      onthefly.png
#> 20    H60NYRVAN11132026            HD            LIVE      onthefly.png
#> 
# }
```
