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
#> [1] "2026-09-29"
#> 
#> $focusedDateCount
#> [1] 8
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
#> 1 2026-09-29
#> 2 2026-09-30
#> 3 2026-10-03
#> 4 2026-10-06
#> 5 2026-10-08
#> 6 2026-10-10
#> 7 2026-10-13
#> 8 2026-10-15
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  games
#> 1 2026020002, 20262027, 2, 2026-09-29, /gamecenter/mtl-vs-tor/2026/09/29/2026020002, 2026-09-29T23:00:00Z, -04:00, -04:00, 282, N, CA, SN, 107, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 8, MTL, 48-24-10, https://assets.nhle.com/logos/nhl/svg/MTL_light.svg, Montréal Canadiens, Canadiens de Montréal, Canadiens, Montréal, de Montréal, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 2                       2026020008, 20262027, 2, 2026-09-30, /gamecenter/nyi-vs-tor/2026/09/30/2026020008, 2026-09-30T23:30:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 2, NYI, 43-34-5, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 3                       2026020023, 20262027, 2, 2026-10-03, /gamecenter/ott-vs-tor/2026/10/03/2026020023, 2026-10-03T23:00:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 9, OTT, 44-27-11, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, Ottawa Senators, Sénateurs d'Ottawa, Senators, Sénateurs, Ottawa, d'Ottawa, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 4                 2026020044, 20262027, 2, 2026-10-06, /gamecenter/nsh-vs-tor/2026/10/06/2026020044, 2026-10-06T23:00:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 18, NSH, 38-34-10, https://assets.nhle.com/logos/nhl/svg/NSH_light.svg, Nashville Predators, Predators de Nashville, Predators, Nashville, de Nashville, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 5              2026020065, 20262027, 2, 2026-10-08, /gamecenter/tor-vs-vgk/2026/10/08/2026020065, 2026-10-09T02:00:00Z, -04:00, -07:00, FUT, OK, https://www.nhl.com/goldenknights/tickets/, https://www.nhl.com/goldenknights/tickets/, T-Mobile Arena, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 54, VGK, 39-26-17, https://assets.nhle.com/logos/nhl/svg/VGK_light.svg, Vegas Golden Knights, Golden Knights de Vegas, Golden Knights, Vegas, de Vegas
#> 6                             2026020080, 20262027, 2, 2026-10-10, /gamecenter/tor-vs-col/2026/10/10/2026020080, 2026-10-10T23:00:00Z, -04:00, -06:00, FUT, OK, https://www.nhl.com/avalanche/tickets/, https://www.nhl.com/avalanche/tickets/, Ball Arena, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 21, COL, 55-16-11, https://assets.nhle.com/logos/nhl/svg/COL_light.svg, Colorado Avalanche, Avalanche du Colorado, Avalanche, Colorado, du Colorado
#> 7                                                        2026020102, 20262027, 2, 2026-10-13, /gamecenter/tor-vs-uta/2026/10/13/2026020102, 2026-10-14T01:00:00Z, -04:00, -06:00, FUT, OK, https://www.nhl.com/utah/tickets/, https://www.nhl.com/utah/tickets/, Delta Center, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 68, UTA, 43-33-6, https://assets.nhle.com/logos/nhl/svg/UTA_light.svg, Utah Mammoth, Mammoth de l'Utah, Mammoth, Utah, de l'Utah
#> 8                                    2026020108, 20262027, 2, 2026-10-15, /gamecenter/buf-vs-tor/2026/10/15/2026020108, 2026-10-15T23:00:00Z, -04:00, -04:00, FUT, OK, https://www.nhl.com/mapleleafs/tickets/, https://www.nhl.com/mapleleafs/tickets/, Scotiabank Arena, 7, BUF, 50-23-9, https://assets.nhle.com/logos/nhl/svg/BUF_light.svg, Buffalo Sabres, Sabres de Buffalo, Sabres, Buffalo, de Buffalo, 10, TOR, 32-36-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 
# }
```
