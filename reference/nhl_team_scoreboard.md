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
#> [1] "2026-04-07"
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
#> 1 2026-03-30
#> 2 2026-04-02
#> 3 2026-04-04
#> 4 2026-04-08
#> 5 2026-04-09
#> 6 2026-04-11
#> 7 2026-04-13
#> 8 2026-04-15
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  games
#> 1                                                                            2025021176, 20252026, 2, 2026-03-30, /gamecenter/tor-vs-ana/2026/03/30/2025021176, 2026-03-31T02:00:00Z, -04:00, -07:00, 281, 288, 553, 219, N, A, H, H, CA, CA, US, US, TVAS, SNO, Victory+, KCOP-13, 120, 31, 382, 656, OFF, OK, https://www.ticketmaster.com/event/090062EC0D245F06?brand=ducks&artistid=805893&wt.mc_id=NHL_TEAM_ANA_SCOREBOARD_GM36&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_ANA&utm_content=SCOREBOARD_GM36, https://www.ticketmaster.com/event/090062EC0D245F06?brand=ducks&artistid=805893&wt.mc_id=NHL_TEAM_ANA_SCOREBOARD_GM36&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_ANA&utm_content=SCOREBOARD_GM36, 4, /video/tor-at-ana-recap-6392221648112, /fr/video/tor-vs-ana-30-03-2026-resume-6392222034112, Honda Center, 10, TOR, 5, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 24, ANA, 4, https://assets.nhle.com/logos/nhl/svg/ANA_light.svg, Anaheim Ducks, Ducks d'Anaheim, Ducks, Anaheim, d'Anaheim, 4, OT, 3
#> 2                                                                                                       2025021202, 20252026, 2, 2026-04-02, /gamecenter/tor-vs-sjs/2026/04/02/2025021202, 2026-04-03T02:00:00Z, -04:00, -07:00, 293, 314, A, H, CA, US, TSN4, NBCSCA, 137, 393, OFF, OK, https://www.ticketmaster.com/event/1C0062DEEE157250?brand=sharks&artistid=806018&wt.mc_id=NHL_TEAM_SJS_SCOREBOARD_GM37&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_SJS&utm_content=SCOREBOARD_GM37, https://www.ticketmaster.com/event/1C0062DEEE157250?brand=sharks&artistid=806018&wt.mc_id=NHL_TEAM_SJS_SCOREBOARD_GM37&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_SJS&utm_content=SCOREBOARD_GM37, 3, /video/tor-at-sjs-recap-6392488853112, /fr/video/tor-vs-sjs-02-04-2026-resume-6392490712112, SAP Center at San Jose, 10, TOR, 1, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 28, SJS, 4, https://assets.nhle.com/logos/nhl/svg/SJS_light.svg, San Jose Sharks, Sharks de San Jose, Sharks, San Jose, de San Jose, 3, REG, 3
#> 3 2025021217, 20252026, 2, 2026-04-04, /gamecenter/tor-vs-lak/2026/04/04/2025021217, 2026-04-04T23:00:00Z, -04:00, -07:00, 288, 4, 577, N, N, H, CA, CA, US, SNO, CBC, FDSNSC, 31, 101, 209, OFF, OK, https://www.axs.com/events/1044300/39-la-kings-vs-Toronto-Maple-Leafs-tickets?skin=lakings&cpch=LAKSCHED&cpdn=website&cpdate=LAK260404&cpsrc=single-game&utm_source=LAKSCHED&utm_medium=website&utm_campaign=single-game&utm_content=LAK250405&tags=LAKWEB, https://www.axs.com/events/1044300/39-la-kings-vs-Toronto-Maple-Leafs-tickets?skin=lakings&cpch=LAKSCHED&cpdn=website&cpdate=LAK260404&cpsrc=single-game&utm_source=LAKSCHED&utm_medium=website&utm_campaign=single-game&utm_content=LAK250405&tags=LAKWEB, 4, /video/tor-at-lak-recap-6392589427112, /fr/video/tor-vs-lak-04-04-2026-resume-6392589329112, Crypto.com Arena, 10, TOR, 6, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 26, LAK, 7, https://assets.nhle.com/logos/nhl/svg/LAK_light.svg, Los Angeles Kings, Kings de Los Angeles, Kings, Los Angeles, de Los Angeles, 4, OT, 3
#> 4                                                                                                                                            2025021245, 20252026, 2, 2026-04-08, /gamecenter/wsh-vs-tor/2026/04/08/2025021245, 2026-04-08T23:30:00Z, -04:00, -04:00, 282, 281, 517, N, N, A, CA, CA, US, SN, TVAS, MNMT, 107, 120, 396, FUT, OK, https://www.ticketmaster.ca/event/100062F7AC9E31C7?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM39&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM39, https://www.ticketmaster.ca/event/100062F7AC9E31C7?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM39&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM39, Scotiabank Arena, 15, WSH, 39-30-9, https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg, Washington Capitals, Capitals de Washington, Capitals, Washington, de Washington, 10, TOR, 32-31-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 5                                                                                                                                                                                                              2025021252, 20252026, 2, 2026-04-09, /gamecenter/tor-vs-nyi/2026/04/09/2025021252, 2026-04-09T22:45:00Z, -04:00, -04:00, 329, 391, 293, N, N, A, US, US, CA, ESPN+, HULU, TSN4, 16, 17, 137, FUT, OK, https://www.ticketmaster.com/event/300062EEE6093401?brand=&artistid=&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM37&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM37, https://www.ticketmaster.com/event/300062EEE6093401?brand=&artistid=&wt.mc_id=NHL_TEAM_NYI_SCOREBOARD_GM37&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_NYI&utm_content=SCOREBOARD_GM37, UBS Arena, 10, TOR, 32-31-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 2, NYI, 42-31-5, https://assets.nhle.com/logos/nhl/svg/NYI_light.svg, New York Islanders, Islanders de New York, Islanders, New York, de New York
#> 6                                                                                                                                                             2025021270, 20252026, 2, 2026-04-11, /gamecenter/fla-vs-tor/2026/04/11/2025021270, 2026-04-11T23:00:00Z, -04:00, -04:00, 282, 4, 521, N, N, A, CA, CA, US, SN, CBC, SCRIPPS, 21, 101, 657, FUT, OK, https://www.ticketmaster.ca/event/100062F7ACA231D1?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM40&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM40, https://www.ticketmaster.ca/event/100062F7ACA231D1?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM40&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM40, Scotiabank Arena, 13, FLA, 37-37-4, https://assets.nhle.com/logos/nhl/svg/FLA_light.svg, Florida Panthers, Panthers de la Floride, Panthers, Florida, de la Floride, 10, TOR, 32-31-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 7                                                                                                                                                                                              2025021285, 20252026, 2, 2026-04-13, /gamecenter/dal-vs-tor/2026/04/13/2025021285, 2026-04-13T23:30:00Z, -04:00, -04:00, 548, 553, N, A, CA, US, Prime, Victory+, 116, 382, FUT, OK, https://www.ticketmaster.ca/event/100062F7ACA631E2?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM41&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM41, https://www.ticketmaster.ca/event/100062F7ACA631E2?brand=torontomapleleafs&artistid=806033&wt.mc_id=NHL_TEAM_TOR_SCOREBOARD_GM41&utm_source=NHL.com&utm_medium=client&utm_campaign=NHL_TEAM_TOR&utm_content=SCOREBOARD_GM41, Scotiabank Arena, 25, DAL, 46-20-12, https://assets.nhle.com/logos/nhl/svg/DAL_light.svg, Dallas Stars, Stars de Dallas, Stars, Dallas, de Dallas, 10, TOR, 32-31-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto
#> 8                                                                                                                                                                                                                                                                                                                                                                                   2025021304, 20252026, 2, 2026-04-15, /gamecenter/tor-vs-ott/2026/04/15/2025021304, 2026-04-15T23:30:00Z, -04:00, -04:00, 282, 281, N, N, CA, CA, SN, TVAS, 107, 120, FUT, OK, https://www.ticketmaster.ca/event/31006305A16822DE?brand=ottawasenators&artistid=805997&landing=s&wt.mc_id=NHL_TEAM_OTT_SGT_PG_GM41&&utm_source=sens_website&utm_medium=schedule&utm_campaign=04152026_tor, https://www.nhl.com/fr/senators/tickets/, Canadian Tire Centre, 10, TOR, 32-31-14, https://assets.nhle.com/logos/nhl/svg/TOR_light.svg, Toronto Maple Leafs, Maple Leafs de Toronto, Maple Leafs, Toronto, de Toronto, 9, OTT, 41-27-10, https://assets.nhle.com/logos/nhl/svg/OTT_light.svg, Ottawa Senators, Sénateurs d'Ottawa, Senators, Sénateurs, Ottawa, d'Ottawa
#> 
# }
```
