# **NHL Team Scoreboard**

Returns current scoreboard information for a specific team, including
upcoming and recent games.

## Usage

``` r
nhl_team_scoreboard(team_abbr)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TOR", "BOS")

## Value

A named list of data frames: `gamesByDate`.

**gamesByDate**

|          |           |                                       |
|----------|-----------|---------------------------------------|
| col_name | types     | description                           |
| date     | character | Date the games are scheduled for.     |
| games    | list      | List of games scheduled on that date. |

## Examples

``` r
# \donttest{
  try(nhl_team_scoreboard(team_abbr = "TOR"))
#> $focusedDate
#> [1] "2026-11-27"
#> 
#> $focusedDateCount
#> [1] 10
#> 
#> $clubTimeZone
#> [1] "America/Toronto"
#> 
#> $clubUTCOffset
#> [1] "-05:00"
#> 
#> $clubScheduleLink
#> [1] "/mapleleafs/schedule"
#> 
#> $gamesByDate
#>          date
#> 1  2026-11-19
#> 2  2026-11-21
#> 3  2026-11-23
#> 4  2026-11-27
#> 5  2026-11-28
#> 6  2026-11-30
#> 7  2026-12-03
#> 8  2026-12-05
#> 9  2026-12-09
#> 10 2026-12-12
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             games
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                    2026020347, 20262027, 2, 2026-11-19, /gamecenter/lak-vs-tor/2026/11/19/2026020347, 2026-11-20T00:00:00Z, -05:00, -05:00, 282, 651, N, A, CA, US, SN, ABTV , 21, 202, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, OFF, OK, 5, /video/lak-at-tor-recap-6408632908112, /fr/video/lak-vs-tor-19-11-2026-resume-6408632385112, Scotiabank Arena, 26, LAK, 4, https://assets.nhle.com/logos/nhl/svg/LAK_light.svg, Los Angeles Kings, Kings de Los Angeles, Kings, Los Angeles, de Los Angeles, 10, TOR, 3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 5, SO, 3
#> 2                                                                                                                                                                                                                                                                                                                                                                                                 2026020364, 20262027, 2, 2026-11-21, /gamecenter/cbj-vs-tor/2026/11/21/2026020364, 2026-11-22T00:00:00Z, -05:00, -05:00, 282, 755, 647, N, A, A, CA, US, US, SN, CBJHN, CBJNHL, 21, 229, 435, en, NA, NA, 282, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, OFF, OK, 3, /video/cbj-at-tor-recap-6408721465112, /fr/video/cbj-vs-tor-21-11-2026-resume-6408722351112, Scotiabank Arena, 29, CBJ, 4, https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg, Columbus Blue Jackets, Blue Jackets de Columbus, Blue Jackets, Columbus, de Columbus, 10, TOR, 3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                  2026020381, 20262027, 2, 2026-11-23, /gamecenter/tor-vs-nyi/2026/11/23/2026020381, 2026-11-24T00:30:00Z, -05:00, -05:00, 282, 409, N, H, CA, US, SN, MSGSN, 107, 424, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, OFF, OK, 3, /video/tor-at-nyi-recap-6408794492112, /fr/video/tor-vs-nyi-23-11-2026-resume-6408795562112, UBS Arena, 10, TOR, 4, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 2, NYI, 2, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York, 3, REG, 3
#> 4                                                                                                            2026020400, 20262027, 2, 2026-11-27, /gamecenter/tor-vs-bos/2026/11/27/2026020400, 2026-11-27T18:00:00Z, -05:00, -05:00, 281, 293, 31, N, A, H, CA, CA, US, TVAS, TSN4, NESN, 120, 137, 418, https://assets.nhle.com/logos/broadcast/281-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/281-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.com/event/010064EDC52C7AD3?brand=bostonbruins&artistid=805902&wt.mc_id=NHL_TEAM_BOS_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_BOS&utm_content=SCOREBOARD_GM15, https://www.ticketmaster.com/event/010064EDC52C7AD3?brand=bostonbruins&artistid=805902&wt.mc_id=NHL_TEAM_BOS_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_BOS&utm_content=SCOREBOARD_GM15, TD Garden, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 6, BOS, 15-7-1, https://assets.nhle.com/logos/nhl/svg/BOS_light.svg, Boston Bruins, Bruins de Boston, Bruins, Boston, de Boston
#> 5                             2026020420, 20262027, 2, 2026-11-28, /gamecenter/tor-vs-pit/2026/11/28/2026020420, 2026-11-29T00:00:00Z, -05:00, -05:00, 282, 528, N, H, CA, US, SN, SN-PIT, 21, 397, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.com/event/160064EAF5A4B8EA?brand=penguins&artistid=806005&wt.mc_id=NHL_TEAM_PIT_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_PIT&utm_content=SCOREBOARD_GM12&camefrom=NHLPENGUINS_WEB_SCOREBOARD, https://www.ticketmaster.com/event/160064EAF5A4B8EA?brand=penguins&artistid=806005&wt.mc_id=NHL_TEAM_PIT_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_PIT&utm_content=SCOREBOARD_GM12&camefrom=NHLPENGUINS_WEB_SCOREBOARD, PPG Paints Arena, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 5, PIT, 7-14-2, https://assets.nhle.com/logos/nhl/svg/PIT_light.svg, Pittsburgh Penguins, Penguins de Pittsburgh, Penguins, Pittsburgh, de Pittsburgh
#> 6                                                                                             2026020433, 20262027, 2, 2026-11-30, /gamecenter/phi-vs-tor/2026/11/30/2026020433, 2026-12-01T00:30:00Z, -05:00, -05:00, 549, 322, N, A, CA, US, SN+, NBCSP, 117, 402, https://assets.nhle.com/logos/broadcast/549-light.svg, NA, https://assets.nhle.com/logos/broadcast/549-dark.svg, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD17F894?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM17&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM17, https://www.ticketmaster.ca/event/100064FECD17F894?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM17&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM17, Scotiabank Arena, 4, PHI, 9-11-6, https://assets.nhle.com/logos/nhl/svg/PHI_light.svg, Philadelphia Flyers, Flyers de Philadelphie, Flyers, Philadelphia, de Philadelphie, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 7                                                                  2026020448, 20262027, 2, 2026-12-03, /gamecenter/fla-vs-tor/2026/12/03/2026020448, 2026-12-04T00:00:00Z, -05:00, -05:00, 281, 293, 521, N, H, A, CA, CA, US, TVAS, TSN4, SCRIPPS, 120, 137, 657, https://assets.nhle.com/logos/broadcast/281-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/281-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD1CF8A6?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM18&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM18, https://www.ticketmaster.ca/event/100064FECD1CF8A6?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM18&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM18, Scotiabank Arena, 13, FLA, 13-8-3, https://assets.nhle.com/logos/nhl/svg/FLA_light.svg, Florida Panthers, Panthers de la Floride, Panthers, Florida, de la Floride, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 8  2026020471, 20262027, 2, 2026-12-05, /gamecenter/tor-vs-car/2026/12/05/2026020471, 2026-12-06T00:00:00Z, -05:00, -05:00, 282, 754, 784, 646, N, H, H, H, CA, US, US, US, SN, HHN, WRAL, CARNHL, 21, 207, 212, 434, en, NA, NA, NA, 282, NA, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, NA, FUT, OK, https://www.ticketmaster.com/event/2D0064F4BD7DD208?brand=carolinahurricanes&artistid=805908&wt.mc_id=NHL_TEAM_CAR_SCOREBOARD_GM13&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_CAR&utm_content=SCOREBOARD_GM13, https://www.ticketmaster.com/event/2D0064F4BD7DD208?brand=carolinahurricanes&artistid=805908&wt.mc_id=NHL_TEAM_CAR_SCOREBOARD_GM13&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_CAR&utm_content=SCOREBOARD_GM13, Lenovo Center, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 12, CAR, 18-3-3, https://assets.nhle.com/logos/nhl/svg/CAR_light.svg, Carolina Hurricanes, Hurricanes de la Caroline, Hurricanes, Carolina, de la Caroline
#> 9                                                                                                                       2026020497, 20262027, 2, 2026-12-09, /gamecenter/tor-vs-dal/2026/12/09/2026020497, 2026-12-10T00:30:00Z, -05:00, -06:00, 548, 652, 23, N, H, H, CA, US, US, Prime, Prime Video, KDFI, 116, 331, 610, https://assets.nhle.com/logos/broadcast/548-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/548-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.com/event/0C0064EDD6129D2F?brand=dallas_stars&wt.mc_id=NHL_TEAM_DAL_SCOREBOARD_GM17&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_DAL&utm_content=SCOREBOARD_GM17, https://www.ticketmaster.com/event/0C0064EDD6129D2F?brand=dallas_stars&wt.mc_id=NHL_TEAM_DAL_SCOREBOARD_GM17&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_DAL&utm_content=SCOREBOARD_GM17, American Airlines Center, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 25, DAL, 15-6-2, https://assets.nhle.com/logos/nhl/svg/DAL_light.svg, Dallas Stars, Stars de Dallas, Stars, Dallas, de Dallas
#> 10                                                                              2026020519, 20262027, 2, 2026-12-12, /gamecenter/vgk-vs-tor/2026/12/12/2026020519, 2026-12-13T00:00:00Z, -05:00, -05:00, 282, 521, N, A, CA, US, SN, SCRIPPS, 21, 657, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD21F8BE?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM19&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM19, https://www.ticketmaster.ca/event/100064FECD21F8BE?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM19&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM19, Scotiabank Arena, 54, VGK, 14-6-5, https://assets.nhle.com/logos/nhl/svg/VGK_light.svg, Vegas Golden Knights, Golden Knights de Vegas, Golden Knights, Vegas, de Vegas, 10, TOR, 9-14-3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 
# }
```
