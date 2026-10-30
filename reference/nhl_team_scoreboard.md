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
#> [1] "2026-10-31"
#> 
#> $focusedDateCount
#> [1] 10
#> 
#> $clubTimeZone
#> [1] "America/Toronto"
#> 
#> $clubUTCOffset
#> [1] "-04:00"
#> 
#> $clubScheduleLink
#> [1] "/mapleleafs/schedule"
#> 
#> $gamesByDate
#>          date
#> 1  2026-10-24
#> 2  2026-10-26
#> 3  2026-10-28
#> 4  2026-10-31
#> 5  2026-11-03
#> 6  2026-11-07
#> 7  2026-11-08
#> 8  2026-11-10
#> 9  2026-11-12
#> 10 2026-11-14
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     games
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  2026020176, 20262027, 2, 2026-10-24, /gamecenter/tor-vs-edm/2026/10/24/2026020176, 2026-10-24T23:00:00Z, -04:00, -06:00, 282, N, CA, SN, 21, en, 282, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, OFF, OK, 3, /video/tor-at-edm-recap-6407364054112, /fr/video/tor-vs-edm-24-10-2026-resume-6407362789112, Rogers Place, 10, TOR, 7, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 22, EDM, 2, https://assets.nhle.com/logos/nhl/svg/EDM_light.svg, Edmonton Oilers, Oilers d'Edmonton, Oilers, Edmonton, d'Edmonton, 3, REG, 3
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  2026020186, 20262027, 2, 2026-10-26, /gamecenter/tor-vs-cgy/2026/10/26/2026020186, 2026-10-27T00:30:00Z, -04:00, -06:00, 549, N, CA, SN+, 117, https://assets.nhle.com/logos/broadcast/549-light.svg, https://assets.nhle.com/logos/broadcast/549-dark.svg, OFF, OK, 3, /video/tor-at-cgy-recap-6407452646112, /fr/video/tor-vs-cgy-26-10-2026-resume-6407450879112, Scotiabank Saddledome, 10, TOR, 2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 20, CGY, 5, https://assets.nhle.com/logos/nhl/svg/CGY_light.svg, Calgary Flames, Flames de Calgary, Flames, Calgary, de Calgary, 3, REG, 3
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    2026020202, 20262027, 2, 2026-10-28, /gamecenter/tor-vs-sea/2026/10/28/2026020202, 2026-10-29T02:00:00Z, -04:00, -07:00, 288, 652, 387, 388, A, H, H, H, CA, US, US, US, SNO, Prime Video, KING, KONG, 31, 117, 615, 654, 282, NA, NA, NA, OFF, OK, 3, /video/tor-at-sea-recap-6407574698112, /fr/video/tor-vs-sea-28-10-2026-resume-6407575092112, Climate Pledge Arena, 10, TOR, 4, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 55, SEA, 5, https://assets.nhle.com/logos/nhl/svg/SEA_light.svg, Seattle Kraken, Kraken de Seattle, Kraken, Seattle, de Seattle, 3, REG, 3
#> 4                                                                                                                                                                   2026020224, 20262027, 2, 2026-10-31, /gamecenter/tor-vs-van/2026/10/31/2026020224, 2026-10-31T23:00:00Z, -04:00, -07:00, 282, N, CA, SN, 21, en, 282, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, FUT, OK, https://www.ticketmaster.ca/event/11006503F09CBB6E?brand=canucks&artistid=806037&wt.mc_id=NHL_TEAM_VAN_SCOREBOARD_GM7&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_VAN&utm_content=SCOREBOARD_GM7, https://www.ticketmaster.ca/event/11006503F09CBB6E?brand=canucks&artistid=806037&wt.mc_id=NHL_TEAM_VAN_SCOREBOARD_GM7&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_VAN&utm_content=SCOREBOARD_GM7, Rogers Arena, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 23, VAN, 3-10-1, https://assets.nhle.com/logos/nhl/svg/VAN_light.svg, Vancouver Canucks, Canucks de Vancouver, Canucks, Vancouver, de Vancouver
#> 5                                                                                                                                                                                                                             2026020236, 20262027, 2, 2026-11-03, /gamecenter/uta-vs-tor/2026/11/03/2026020236, 2026-11-04T00:00:00Z, -05:00, -05:00, 293, 783, 558, H, A, A, CA, US, US, TSN4, Mammoth+, Utah16, 137, 1, 381, FUT, OK, https://www.ticketmaster.ca/event/100064FECCECF818?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM9&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM9, https://www.ticketmaster.ca/event/100064FECCECF818?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM9&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM9, Scotiabank Arena, 68, UTA, 8-5-0, https://assets.nhle.com/logos/nhl/svg/UTA_light.svg, Utah Mammoth, Mammoth de l'Utah, Mammoth, Utah, de l'Utah, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 6  2026020268, 20262027, 2, 2026-11-07, /gamecenter/mtl-vs-tor/2026/11/07/2026020268, 2026-11-08T00:00:00Z, -05:00, -05:00, 282, 281, N, N, CA, CA, SN, TVAS, 107, 120, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/281-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, https://assets.nhle.com/logos/broadcast/281-dark.svg, FUT, OK, https://www.ticketmaster.ca/event/100064FECCF1F826?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM10&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM10, https://www.ticketmaster.ca/event/100064FECCF1F826?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM10&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM10, Scotiabank Arena, 8, MTL, 7-3-2, https://assets.nhle.com/logos/nhl/svg/MTL_light.svg, Montréal Canadiens, Canadiens de Montréal, Canadiens, Montréal, de Montréal, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 7                                                                                2026020279, 20262027, 2, 2026-11-08, /gamecenter/njd-vs-tor/2026/11/08/2026020279, 2026-11-09T00:00:00Z, -05:00, -05:00, 324, 293, 409, N, H, A, US, CA, US, NHLN, TSN4, MSGSN, 35, 137, 411, https://assets.nhle.com/logos/broadcast/324-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/324-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECCF7F83B?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM11&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM11, https://www.ticketmaster.ca/event/100064FECCF7F83B?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM11&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM11, Scotiabank Arena, 1, NJD, 8-5-0, https://assets.nhle.com/logos/nhl/svg/NJD_light.svg, New Jersey Devils, Devils du New Jersey, Devils, New Jersey, du New Jersey, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 8                                                                                 2026020286, 20262027, 2, 2026-11-10, /gamecenter/col-vs-tor/2026/11/10/2026020286, 2026-11-11T00:00:00Z, -05:00, -05:00, 281, 293, 2, N, H, A, CA, CA, US, TVAS, TSN4, ALT, 120, 137, 385, https://assets.nhle.com/logos/broadcast/281-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/281-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECCFCF845?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM12, https://www.ticketmaster.ca/event/100064FECCFCF845?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM12&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM12, Scotiabank Arena, 21, COL, 7-5-1, https://assets.nhle.com/logos/nhl/svg/COL_light.svg, Colorado Avalanche, Avalanche du Colorado, Avalanche, Colorado, du Colorado, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 9                                                                               2026020300, 20262027, 2, 2026-11-12, /gamecenter/min-vs-tor/2026/11/12/2026020300, 2026-11-13T00:00:00Z, -05:00, -05:00, 282, 648, 758, N, A, A, CA, US, US, SN, MINNHL, Wild+, 107, 1, 1, 282, NA, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, NA, NA, https://assets.nhle.com/logos/broadcast/282-dark.svg, NA, NA, FUT, OK, https://www.ticketmaster.ca/event/100064FECD01F852?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM13&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM13, https://www.ticketmaster.ca/event/100064FECD01F852?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM13&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM13, Scotiabank Arena, 30, MIN, 9-3-2, https://assets.nhle.com/logos/nhl/svg/MIN_light.svg, Minnesota Wild, Wild du Minnesota, Wild, Minnesota, du Minnesota, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 10     2026020314, 20262027, 2, 2026-11-14, /gamecenter/edm-vs-tor/2026/11/14/2026020314, 2026-11-15T00:00:00Z, -05:00, -05:00, 282, 324, N, N, CA, US, SN, NHLN, 21, 35, en, NA, 282, NA, https://assets.nhle.com/logos/broadcast/282-light.svg, https://assets.nhle.com/logos/broadcast/324-light.svg, https://assets.nhle.com/logos/broadcast/282-dark.svg, https://assets.nhle.com/logos/broadcast/324-dark.svg, FUT, OK, https://www.ticketmaster.ca/event/100064FECD07F864?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM14&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM14, https://www.ticketmaster.ca/event/100064FECD07F864?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM14&utm_source=NHL.com&utm_medium=client_PM&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM14, Scotiabank Arena, 22, EDM, 9-2-2, https://assets.nhle.com/logos/nhl/svg/EDM_light.svg, Edmonton Oilers, Oilers d'Edmonton, Oilers, Edmonton, d'Edmonton, 10, TOR, 7-6-2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 
# }
```
