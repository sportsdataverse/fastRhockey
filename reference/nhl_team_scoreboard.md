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
#> [1] "2026-11-14"
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
#> 1  2026-11-08
#> 2  2026-11-10
#> 3  2026-11-12
#> 4  2026-11-14
#> 5  2026-11-17
#> 6  2026-11-19
#> 7  2026-11-21
#> 8  2026-11-23
#> 9  2026-11-27
#> 10 2026-11-28
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   games
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                           2026020279, 20262027, 2, 2026-11-08, /gamecenter/njd-vs-tor/2026/11/08/2026020279, 2026-11-09T00:00:00Z, -05:00, -05:00, 324, 293, 409, N, H, A, US, CA, US, NHLN, TSN4, MSGSN, 35, 137, 424, https://assets.nhle.com/logos/broadcast/324-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/324-dark.svg, NA, NA, OFF, OK, 3, /video/njd-at-tor-recap-6408117667112, /fr/video/njd-vs-tor-08-11-2026-resume-6408118479112, Scotiabank Arena, 1, NJD, 4, https://assets.nhle.com/logos/nhl/svg/NJD_light.svg, New Jersey Devils, Devils du New Jersey, Devils, New Jersey, du New Jersey, 10, TOR, 0, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   2026020286, 20262027, 2, 2026-11-10, /gamecenter/col-vs-tor/2026/11/10/2026020286, 2026-11-11T00:00:00Z, -05:00, -05:00, 281, 293, 2, N, H, A, CA, CA, US, TVAS, TSN4, ALT, 120, 137, 398, https://assets.nhle.com/logos/broadcast/281-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/281-dark.svg, NA, NA, OFF, OK, 3, /fr/video/col-vs-tor-10-11-2026-resume-6408215698112, Scotiabank Arena, 21, COL, 6, https://assets.nhle.com/logos/nhl/svg/COL_light.svg, Colorado Avalanche, Avalanche du Colorado, Avalanche, Colorado, du Colorado, 10, TOR, 3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                      2026020300, 20262027, 2, 2026-11-12, /gamecenter/min-vs-tor/2026/11/12/2026020300, 2026-11-13T00:00:00Z, -05:00, -05:00, 282, 758, 648, N, A, A, CA, US, US, SN, Wild+, MINNHL, 107, 211, 436, 282, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, OFF, OK, 3, /video/min-at-tor-recap-6408316953112, /fr/video/min-vs-tor-12-11-2026-resume-6408317240112, Scotiabank Arena, 30, MIN, 5, https://assets.nhle.com/logos/nhl/svg/MIN_light.svg, Minnesota Wild, Wild du Minnesota, Wild, Minnesota, du Minnesota, 10, TOR, 2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 4  2026020314, 20262027, 2, 2026-11-14, /gamecenter/edm-vs-tor/2026/11/14/2026020314, 2026-11-15T00:00:00Z, -05:00, -05:00, 282, 324, N, N, CA, US, SN, NHLN, 21, 35, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/324-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, https://assets.nhle.com/logos/broadcast/324-dark.svg, FUT, OK, https://www.ticketmaster.ca/event/100064FECD07F864?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM14&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM14, https://www.ticketmaster.ca/event/100064FECD07F864?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM14&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM14, Scotiabank Arena, 22, EDM, 14-3-3, https://assets.nhle.com/logos/nhl/svg/EDM_light.svg, Edmonton Oilers, Oilers d'Edmonton, Oilers, Edmonton, d'Edmonton, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 5                                                                                                                                                                                 2026020338, 20262027, 2, 2026-11-17, /gamecenter/tor-vs-chi/2026/11/17/2026020338, 2026-11-18T00:30:00Z, -05:00, -06:00, 385, 578, 293, N, N, A, US, US, CA, TNT, HBO MAX, TSN4, 11, 19, 137, https://assets.nhle.com/logos/broadcast/385-light.svg, https://assets.nhle.com/logos/broadcast/578-light.svg, NA, https://assets.nhle.com/logos/broadcast/385-dark.svg, https://assets.nhle.com/logos/broadcast/578-dark.svg, NA, FUT, OK, https://www.ticketmaster.com/event/040064F09063786B?brand=blackhawks&artistid=805913&wt.mc_id=NHL_TEAM_CHI_SCOREBOARD_GM12, https://www.ticketmaster.com/event/040064F09063786B?brand=blackhawks&artistid=805913&wt.mc_id=NHL_TEAM_CHI_SCOREBOARD_GM12, United Center, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 16, CHI, 5-13-1, https://assets.nhle.com/logos/nhl/svg/CHI_light.svg, Chicago Blackhawks, Blackhawks de Chicago, Blackhawks, Chicago, de Chicago
#> 6                                                                                          2026020347, 20262027, 2, 2026-11-19, /gamecenter/lak-vs-tor/2026/11/19/2026020347, 2026-11-20T00:00:00Z, -05:00, -05:00, 282, 651, N, A, CA, US, SN, ABTV , 21, 202, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD0DF879?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM15, https://www.ticketmaster.ca/event/100064FECD0DF879?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM15, Scotiabank Arena, 26, LAK, 5-10-2, https://assets.nhle.com/logos/nhl/svg/LAK_light.svg, Los Angeles Kings, Kings de Los Angeles, Kings, Los Angeles, de Los Angeles, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 7                                        2026020364, 20262027, 2, 2026-11-21, /gamecenter/cbj-vs-tor/2026/11/21/2026020364, 2026-11-22T00:00:00Z, -05:00, -05:00, 282, 755, 647, N, A, A, CA, US, US, SN, CBJHN, CBJNHL, 21, 229, 435, en, NA, NA, 282, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD12F887?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM16&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM16, https://www.ticketmaster.ca/event/100064FECD12F887?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM16&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM16, Scotiabank Arena, 29, CBJ, 7-10-0, https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg, Columbus Blue Jackets, Blue Jackets de Columbus, Blue Jackets, Columbus, de Columbus, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 8                                                                                                          2026020381, 20262027, 2, 2026-11-23, /gamecenter/tor-vs-nyi/2026/11/23/2026020381, 2026-11-24T00:30:00Z, -05:00, -05:00, 282, 409, N, H, CA, US, SN, MSGSN, 107, 424, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.com/event/300064E2D27E61D8?brand=nyislanders&artistid=805986&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM11&utm_source=nyi_website&utm_medium=scoreboard&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM11, https://www.ticketmaster.com/event/300064E2D27E61D8?brand=nyislanders&artistid=805986&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM11&utm_source=nyi_website&utm_medium=scoreboard&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM11, UBS Arena, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 2, NYI, 8-6-4, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York
#> 9                                                                                                                  2026020400, 20262027, 2, 2026-11-27, /gamecenter/tor-vs-bos/2026/11/27/2026020400, 2026-11-27T18:00:00Z, -05:00, -05:00, 281, 293, 31, N, A, H, CA, CA, US, TVAS, TSN4, NESN, 120, 137, 418, https://assets.nhle.com/logos/broadcast/281-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/281-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.com/event/010064EDC52C7AD3?brand=bostonbruins&artistid=805902&wt.mc_id=NHL_TEAM_BOS_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_BOS&utm_content=SCOREBOARD_GM15, https://www.ticketmaster.com/event/010064EDC52C7AD3?brand=bostonbruins&artistid=805902&wt.mc_id=NHL_TEAM_BOS_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_BOS&utm_content=SCOREBOARD_GM15, TD Garden, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 6, BOS, 13-5-1, https://assets.nhle.com/logos/nhl/svg/BOS_light.svg, Boston Bruins, Bruins de Boston, Bruins, Boston, de Boston
#> 10                                  2026020420, 20262027, 2, 2026-11-28, /gamecenter/tor-vs-pit/2026/11/28/2026020420, 2026-11-29T00:00:00Z, -05:00, -05:00, 282, 528, N, H, CA, US, SN, SN-PIT, 21, 397, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.com/event/160064EAF5A4B8EA?brand=penguins&artistid=806005&wt.mc_id=NHL_TEAM_PIT_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_PIT&utm_content=SCOREBOARD_GM12&camefrom=NHLPENGUINS_WEB_SCOREBOARD, https://www.ticketmaster.com/event/160064EAF5A4B8EA?brand=penguins&artistid=806005&wt.mc_id=NHL_TEAM_PIT_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_PIT&utm_content=SCOREBOARD_GM12&camefrom=NHLPENGUINS_WEB_SCOREBOARD, PPG Paints Arena, 10, TOR, 8-11-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 5, PIT, 7-10-2, https://assets.nhle.com/logos/nhl/svg/PIT_light.svg, Pittsburgh Penguins, Penguins de Pittsburgh, Penguins, Pittsburgh, de Pittsburgh
#> 
# }
```
