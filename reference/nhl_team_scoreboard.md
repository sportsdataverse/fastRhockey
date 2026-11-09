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
#> [1] "2026-11-10"
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
#> 1  2026-11-03
#> 2  2026-11-07
#> 3  2026-11-08
#> 4  2026-11-10
#> 5  2026-11-12
#> 6  2026-11-14
#> 7  2026-11-17
#> 8  2026-11-19
#> 9  2026-11-21
#> 10 2026-11-23
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  games
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 2026020236, 20262027, 2, 2026-11-03, /gamecenter/uta-vs-tor/2026/11/03/2026020236, 2026-11-04T00:00:00Z, -05:00, -05:00, 293, 783, 558, H, A, A, CA, US, US, TSN4, Mammoth+, Utah16, 137, 210, 394, OFF, OK, 3, /video/uta-at-tor-recap-6407880463112, /fr/video/uta-vs-tor-03-11-2026-resume-6407879997112, Scotiabank Arena, 68, UTA, 3, https://assets.nhle.com/logos/nhl/svg/UTA_light.svg, Utah Mammoth, Mammoth de l'Utah, Mammoth, Utah, de l'Utah, 10, TOR, 2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 2                                                                                                                                                                                                                                                                                                                                                            2026020268, 20262027, 2, 2026-11-07, /gamecenter/mtl-vs-tor/2026/11/07/2026020268, 2026-11-08T00:00:00Z, -05:00, -05:00, 282, 281, N, N, CA, CA, SN, TVAS, 107, 120, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/281-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, https://assets.nhle.com/logos/broadcast/281-dark.svg, OFF, OK, 3, /video/mtl-at-tor-recap-6408087532112, /fr/video/mtl-vs-tor-07-11-2026-resume-6408085969112, Scotiabank Arena, 8, MTL, 4, https://assets.nhle.com/logos/nhl/svg/MTL_light.svg, Montréal Canadiens, Canadiens de Montréal, Canadiens, Montréal, de Montréal, 10, TOR, 2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                          2026020279, 20262027, 2, 2026-11-08, /gamecenter/njd-vs-tor/2026/11/08/2026020279, 2026-11-09T00:00:00Z, -05:00, -05:00, 324, 293, 409, N, H, A, US, CA, US, NHLN, TSN4, MSGSN, 35, 137, 424, https://assets.nhle.com/logos/broadcast/324-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/324-dark.svg, NA, NA, OFF, OK, 3, /video/njd-at-tor-recap-6408117667112, /fr/video/njd-vs-tor-08-11-2026-resume-6408118479112, Scotiabank Arena, 1, NJD, 4, https://assets.nhle.com/logos/nhl/svg/NJD_light.svg, New Jersey Devils, Devils du New Jersey, Devils, New Jersey, du New Jersey, 10, TOR, 0, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 4                                                                             2026020286, 20262027, 2, 2026-11-10, /gamecenter/col-vs-tor/2026/11/10/2026020286, 2026-11-11T00:00:00Z, -05:00, -05:00, 281, 293, 2, N, H, A, CA, CA, US, TVAS, TSN4, ALT, 120, 137, 398, https://assets.nhle.com/logos/broadcast/281-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/281-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECCFCF845?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM12, https://www.ticketmaster.ca/event/100064FECCFCF845?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM12, Scotiabank Arena, 21, COL, 10-6-2, https://assets.nhle.com/logos/nhl/svg/COL_light.svg, Colorado Avalanche, Avalanche du Colorado, Avalanche, Colorado, du Colorado, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 5                                                                       2026020300, 20262027, 2, 2026-11-12, /gamecenter/min-vs-tor/2026/11/12/2026020300, 2026-11-13T00:00:00Z, -05:00, -05:00, 282, 758, 648, N, A, A, CA, US, US, SN, Wild+, MINNHL, 107, 211, 436, 282, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD01F852?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM13&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM13, https://www.ticketmaster.ca/event/100064FECD01F852?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM13&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM13, Scotiabank Arena, 30, MIN, 12-3-4, https://assets.nhle.com/logos/nhl/svg/MIN_light.svg, Minnesota Wild, Wild du Minnesota, Wild, Minnesota, du Minnesota, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 6  2026020314, 20262027, 2, 2026-11-14, /gamecenter/edm-vs-tor/2026/11/14/2026020314, 2026-11-15T00:00:00Z, -05:00, -05:00, 282, 324, N, N, CA, US, SN, NHLN, 21, 35, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/324-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, https://assets.nhle.com/logos/broadcast/324-dark.svg, FUT, OK, https://www.ticketmaster.ca/event/100064FECD07F864?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM14&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM14, https://www.ticketmaster.ca/event/100064FECD07F864?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM14&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM14, Scotiabank Arena, 22, EDM, 12-3-3, https://assets.nhle.com/logos/nhl/svg/EDM_light.svg, Edmonton Oilers, Oilers d'Edmonton, Oilers, Edmonton, d'Edmonton, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 7                                                                                                                                                                                 2026020338, 20262027, 2, 2026-11-17, /gamecenter/tor-vs-chi/2026/11/17/2026020338, 2026-11-18T00:30:00Z, -05:00, -06:00, 385, 578, 293, N, N, A, US, US, CA, TNT, HBO MAX, TSN4, 11, 19, 137, https://assets.nhle.com/logos/broadcast/385-light.svg, https://assets.nhle.com/logos/broadcast/578-light.svg, NA, https://assets.nhle.com/logos/broadcast/385-dark.svg, https://assets.nhle.com/logos/broadcast/578-dark.svg, NA, FUT, OK, https://www.ticketmaster.com/event/040064F09063786B?brand=blackhawks&artistid=805913&wt.mc_id=NHL_TEAM_CHI_SCOREBOARD_GM12, https://www.ticketmaster.com/event/040064F09063786B?brand=blackhawks&artistid=805913&wt.mc_id=NHL_TEAM_CHI_SCOREBOARD_GM12, United Center, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 16, CHI, 4-13-1, https://assets.nhle.com/logos/nhl/svg/CHI_light.svg, Chicago Blackhawks, Blackhawks de Chicago, Blackhawks, Chicago, de Chicago
#> 8                                                                                           2026020347, 20262027, 2, 2026-11-19, /gamecenter/lak-vs-tor/2026/11/19/2026020347, 2026-11-20T00:00:00Z, -05:00, -05:00, 282, 651, N, A, CA, US, SN, ABTV , 21, 202, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD0DF879?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM15, https://www.ticketmaster.ca/event/100064FECD0DF879?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM15&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM15, Scotiabank Arena, 26, LAK, 5-8-2, https://assets.nhle.com/logos/nhl/svg/LAK_light.svg, Los Angeles Kings, Kings de Los Angeles, Kings, Los Angeles, de Los Angeles, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 9                                         2026020364, 20262027, 2, 2026-11-21, /gamecenter/cbj-vs-tor/2026/11/21/2026020364, 2026-11-22T00:00:00Z, -05:00, -05:00, 282, 755, 647, N, A, A, CA, US, US, SN, CBJHN, CBJNHL, 21, 229, 435, en, NA, NA, 282, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD12F887?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM16&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM16, https://www.ticketmaster.ca/event/100064FECD12F887?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM16&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM16, Scotiabank Arena, 29, CBJ, 7-8-0, https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg, Columbus Blue Jackets, Blue Jackets de Columbus, Blue Jackets, Columbus, de Columbus, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 10                                                                                                         2026020381, 20262027, 2, 2026-11-23, /gamecenter/tor-vs-nyi/2026/11/23/2026020381, 2026-11-24T00:30:00Z, -05:00, -05:00, 282, 409, N, H, CA, US, SN, MSGSN, 107, 424, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, FUT, OK, https://www.ticketmaster.com/event/300064E2D27E61D8?brand=nyislanders&artistid=805986&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM11&utm_source=nyi_website&utm_medium=scoreboard&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM11, https://www.ticketmaster.com/event/300064E2D27E61D8?brand=nyislanders&artistid=805986&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM11&utm_source=nyi_website&utm_medium=scoreboard&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM11, UBS Arena, 10, TOR, 8-9-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 2, NYI, 7-6-3, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York
#> 
# }
```
