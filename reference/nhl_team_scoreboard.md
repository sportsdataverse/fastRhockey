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

Returns a list with scoreboard data including game info.

## Examples

``` r
# \donttest{
  try(nhl_team_scoreboard(team_abbr = "TOR"))
#> $focusedDate
#> [1] "2026-04-13"
#> 
#> $focusedDateCount
#> [1] 5
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
#> 1 2026-04-08
#> 2 2026-04-09
#> 3 2026-04-11
#> 4 2026-04-13
#> 5 2026-04-15
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     games
#> 1 2025021245, 20252026, 2, 2026-04-08, /gamecenter/wsh-vs-tor/2026/04/08/2025021245, 2026-04-08T23:30:00Z, -04:00, -04:00, 282, 281, 517, N, N, A, CA, CA, US, SN, TVAS, MNMT, 107, 120, 396, OFF, OK, https://www.ticketmaster.ca/event/100062F7AC9E31C7?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM39&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM39, https://www.ticketmaster.ca/event/100062F7AC9E31C7?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM39&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM39, 3, /video/wsh-at-tor-recap-6392783439112, /fr/video/wsh-vs-tor-08-04-2026-resume-6392783335112, Scotiabank Arena, 15, WSH, 4, https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg, Washington Capitals, Capitals de Washington, Capitals, Washington, de Washington, 10, TOR, 0, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 2                                                                   2025021252, 20252026, 2, 2026-04-09, /gamecenter/tor-vs-nyi/2026/04/09/2025021252, 2026-04-09T22:45:00Z, -04:00, -04:00, 329, 391, 293, N, N, A, US, US, CA, ESPN+, HULU, TSN4, 16, 17, 137, OFF, OK, https://www.ticketmaster.com/event/300062EEE6093401?brand=&artistid=&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM37&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM37, https://www.ticketmaster.com/event/300062EEE6093401?brand=&artistid=&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM37&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM37, 3, /video/tor-at-nyi-recap-6392841471112, /fr/video/tor-vs-nyi-09-04-2026-resume-6392840772112, UBS Arena, 10, TOR, 3, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 2, NYI, 5, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York, 3, REG, 3
#> 3                                                         2025021270, 20252026, 2, 2026-04-11, /gamecenter/fla-vs-tor/2026/04/11/2025021270, 2026-04-11T23:00:00Z, -04:00, -04:00, 282, 4, 521, N, N, A, CA, CA, US, SN, CBC, SCRIPPS, 21, 101, 657, OFF, OK, https://www.ticketmaster.ca/event/100062F7ACA231D1?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM40&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM40, https://www.ticketmaster.ca/event/100062F7ACA231D1?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM40&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM40, 3, /fr/video/fla-vs-tor-11-04-2026-resume-6392951941112, Scotiabank Arena, 13, FLA, 6, https://assets.nhle.com/logos/nhl/svg/FLA_light.svg, Florida Panthers, Panthers de la Floride, Panthers, Florida, de la Floride, 10, TOR, 2, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 3, REG, 3
#> 4                                                                                                                                                 2025021285, 20252026, 2, 2026-04-13, /gamecenter/dal-vs-tor/2026/04/13/2025021285, 2026-04-13T23:30:00Z, -04:00, -04:00, 548, 553, N, A, CA, US, Prime, Victory+, 116, 382, FUT, OK, https://www.ticketmaster.ca/event/100062F7ACA631E2?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM41&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM41, https://www.ticketmaster.ca/event/100062F7ACA631E2?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM41&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM41, Scotiabank Arena, 25, DAL, 48-20-12, https://assets.nhle.com/logos/nhl/svg/DAL_light.svg, Dallas Stars, Stars de Dallas, Stars, Dallas, de Dallas, 10, TOR, 32-34-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 5                                                                                                                                                                                                                                                                                                                                      2025021304, 20252026, 2, 2026-04-15, /gamecenter/tor-vs-ott/2026/04/15/2025021304, 2026-04-15T23:30:00Z, -04:00, -04:00, 282, 281, N, N, CA, CA, SN, TVAS, 107, 120, FUT, OK, https://www.ticketmaster.ca/event/31006305A16822DE?brand=ottawasenators&artistid=805997&landing=s&wt.mc_id=NHL_TEAM_OTT_SGT_PG_GM41&&utm_source=sens_website&utm_medium=schedule&utm_campaign=04152026_tor, https://www.nhl.com/fr/senators/tickets/, Canadian Tire Centre, 10, TOR, 32-34-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 9, OTT, 43-27-11, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, Ottawa Senators, Sénateurs d'Ottawa, Senators, Sénateurs, Ottawa, d'Ottawa
#> 
# }
```
