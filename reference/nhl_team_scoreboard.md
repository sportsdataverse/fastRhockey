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
#> [1] "2026-05-29"
#> 
#> $focusedDateCount
#> [1] 3
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
#> 1 2026-04-11
#> 2 2026-04-13
#> 3 2026-04-15
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  games
#> 1      2025021270, 20252026, 2, 2026-04-11, /gamecenter/fla-vs-tor/2026/04/11/2025021270, 2026-04-11T23:00:00Z, -04:00, -04:00, 282, 4, 521, N, N, A, CA, CA, US, SN, CBC, SCRIPPS, 21, 101, 657, OFF, OK, https://www.ticketmaster.ca/event/100062F7ACA231D1?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM40&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM40, https://www.ticketmaster.ca/event/100062F7ACA231D1?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM40&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM40, 3, /fr/video/fla-vs-tor-11-04-2026-resume-6392951941112, Scotiabank Arena, 13, FLA, 6, https://assets.nhle.com/logos/nhl/svg/FLA_light.svg, Florida Panthers, Panthers de la Floride, Panthers, Florida, de la Floride, 10, TOR, 2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 2 2025021285, 20252026, 2, 2026-04-13, /gamecenter/dal-vs-tor/2026/04/13/2025021285, 2026-04-13T23:30:00Z, -04:00, -04:00, 548, 553, N, A, CA, US, Prime, Victory+, 116, 382, OFF, OK, https://www.ticketmaster.ca/event/100062F7ACA631E2?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM41&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM41, https://www.ticketmaster.ca/event/100062F7ACA631E2?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM41&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM41, 3, /video/dal-at-tor-recap-6393059496112, /fr/video/dal-vs-tor-13-04-2026-resume-6393060850112, Scotiabank Arena, 25, DAL, 6, https://assets.nhle.com/logos/nhl/svg/DAL_light.svg, Dallas Stars, Stars de Dallas, Stars, Dallas, de Dallas, 10, TOR, 5, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 3                                                                                                                                                                                      2025021304, 20252026, 2, 2026-04-15, /gamecenter/tor-vs-ott/2026/04/15/2025021304, 2026-04-15T23:30:00Z, -04:00, -04:00, 282, 281, N, N, CA, CA, SN, TVAS, 107, 120, OFF, OK, https://www.ticketmaster.ca/event/31006305A16822DE?brand=ottawasenators&artistid=805997&landing=s&wt.mc_id=NHL_TEAM_OTT_SGT_PG_GM41&&utm_source=sens_website&utm_medium=schedule&utm_campaign=04152026_tor, https://www.nhl.com/fr/senators/tickets/, 3, /video/tor-at-ott-recap-6393196343112, /fr/video/tor-vs-ott-15-04-2026-resume-6393197022112, Canadian Tire Centre, 10, TOR, 1, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 9, OTT, 3, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, Ottawa Senators, Sénateurs d'Ottawa, Senators, Sénateurs, Ottawa, d'Ottawa, 3, REG, 3
#> 
# }
```
