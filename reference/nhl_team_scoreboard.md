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
#> [1] "2026-09-19"
#> 
#> $focusedDateCount
#> [1] 7
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
#>         date
#> 1 2026-09-19
#> 2 2026-09-23
#> 3 2026-09-29
#> 4 2026-09-30
#> 5 2026-10-03
#> 6 2026-10-06
#> 7 2026-10-08
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         games
#> 1 2026010006, 2026010007, 20262027, 20262027, 1, 1, 2026-09-19, 2026-09-19, /gamecenter/mtl-vs-tor/2026/09/19/2026010006, /gamecenter/tor-vs-mtl/2026/09/19/2026010007, 2026-09-19T23:00:00Z, 2026-09-19T23:00:00Z, -04:00, -04:00, -04:00, -04:00, FUT, FUT, OK, OK, https://www.ticketmaster.ca/event/100064EDD6F5121D, https://www.ticketmaster.ca/event/310064FACA9986E8?lang=en-ca&brand=canadiens&artistid=805975&wt.mc_id=NHL_TEAM_MTL_SCOREBOARD_PR1&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_MTL&utm_content=SCOREBOARD_PR1, https://www.ticketmaster.ca/event/100064EDD6F5121D, https://www.ticketmaster.ca/event/310064FACA9986E8?lang=fr-ca&brand=canadiens&artistid=805975&wt.mc_id=NHL_TEAM_MTL_SCOREBOARD_PR1&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_MTL&utm_content=SCOREBOARD_PR1, Scotiabank Arena, Centre Bell, 8, 10, MTL, TOR, 48-24-10, 32-36-14, https://assets.nhle.com/logos/nhl/svg/MTL_light.svg, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Montréal Canadiens, Toronto Maple Leafs, Canadiens de Montréal, Maple Leafs de Toronto, Canadiens, Maple Leafs, Montréal, Toronto, de Montréal, de Toronto, 10, 8, TOR, MTL, 32-36-14, 48-24-10, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, https://assets.nhle.com/logos/nhl/svg/MTL_light.svg, Toronto Maple Leafs, Montréal Canadiens, Maple Leafs de Toronto, Canadiens de Montréal, Maple Leafs, Canadiens, Toronto, Montréal, de Toronto, de Montréal
#> 2                                                                                                                                                                                                                                                                                                 2026010035, 2026010036, 20262027, 20262027, 1, 1, 2026-09-23, 2026-09-23, /gamecenter/ott-vs-tor/2026/09/23/2026010035, /gamecenter/tor-vs-ott/2026/09/23/2026010036, 2026-09-23T23:00:00Z, 2026-09-23T23:00:00Z, -04:00, -04:00, -04:00, -04:00, FUT, FUT, OK, OK, https://www.ticketmaster.ca/event/100064EDD6FC1231, https://ticketmaster.ca/ottawa-senators-vs-toronto-maple-leafs-ottawa-ontario-09-23-2026/event/310064D3867C359C?language=en-ca&landing=ss, https://www.ticketmaster.ca/event/100064EDD6FC1231, , Scotiabank Arena, Canadian Tire Centre, 9, 10, OTT, TOR, 44-27-11, 32-36-14, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Ottawa Senators, Toronto Maple Leafs, Sénateurs d'Ottawa, Maple Leafs de Toronto, Senators, Maple Leafs, Sénateurs, NA, Ottawa, Toronto, d'Ottawa, de Toronto, 10, 9, TOR, OTT, 32-36-14, 44-27-11, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, Toronto Maple Leafs, Ottawa Senators, Maple Leafs de Toronto, Sénateurs d'Ottawa, Maple Leafs, Senators, NA, Sénateurs, Toronto, Ottawa, de Toronto, d'Ottawa
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        2026020002, 20262027, 2, 2026-09-29, /gamecenter/mtl-vs-tor/2026/09/29/2026020002, 2026-09-29T23:00:00Z, -04:00, -04:00, 282, N, CA, SN, 107, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 8, MTL, 48-24-10, https://assets.nhle.com/logos/nhl/svg/MTL_light.svg, Montréal Canadiens, Canadiens de Montréal, Canadiens, Montréal, de Montréal, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2026020008, 20262027, 2, 2026-09-30, /gamecenter/nyi-vs-tor/2026/09/30/2026020008, 2026-09-30T23:30:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 2, NYI, 43-34-5, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2026020023, 20262027, 2, 2026-10-03, /gamecenter/ott-vs-tor/2026/10/03/2026020023, 2026-10-03T23:00:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 9, OTT, 44-27-11, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, Ottawa Senators, Sénateurs d'Ottawa, Senators, Sénateurs, Ottawa, d'Ottawa, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        2026020044, 20262027, 2, 2026-10-06, /gamecenter/nsh-vs-tor/2026/10/06/2026020044, 2026-10-06T23:00:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 18, NSH, 38-34-10, https://assets.nhle.com/logos/nhl/svg/NSH_light.svg, Nashville Predators, Predators de Nashville, Predators, Nashville, de Nashville, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     2026020065, 20262027, 2, 2026-10-08, /gamecenter/tor-vs-vgk/2026/10/08/2026020065, 2026-10-09T02:00:00Z, -04:00, -07:00, FUT, OK, https://www.nhl.com/goldenknights/tickets/, https://www.nhl.com/goldenknights/tickets/, T-Mobile Arena, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 54, VGK, 39-26-17, https://assets.nhle.com/logos/nhl/svg/VGK_light.svg, Vegas Golden Knights, Golden Knights de Vegas, Golden Knights, Vegas, de Vegas
#> 
# }
```
