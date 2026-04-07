# **NHL Playoff Schedule**

Returns the playoff schedule for a specific round/series.

## Usage

``` r
nhl_playoff_schedule(season, series_letter)
```

## Arguments

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season).

- series_letter:

  Character series letter (e.g., "a", "b", "c", "d").

## Value

Returns a list with playoff schedule data for the series.

## Examples

``` r
# \donttest{
  try(nhl_playoff_schedule(season = 2024, series_letter = "a"))
#> $round
#> [1] 1
#> 
#> $roundAbbrev
#> [1] "R1"
#> 
#> $roundLabel
#> [1] "1st-round"
#> 
#> $seriesLetter
#> [1] "A"
#> 
#> $seriesLogo
#> [1] "https://assets.nhle.com/logos/playoffs/png/stanley_cup_playoffs_secondary_wordmark_dark.png"
#> 
#> $seriesLogoFr
#> [1] "https://assets.nhle.com/logos/playoffs/png/stanley_cup_playoffs_secondary_wordmark_fr_dark.png"
#> 
#> $neededToWin
#> [1] 4
#> 
#> $length
#> [1] 7
#> 
#> $bottomSeedTeam
#> $bottomSeedTeam$id
#> [1] 9
#> 
#> $bottomSeedTeam$name
#> $bottomSeedTeam$name$default
#> [1] "Senators"
#> 
#> $bottomSeedTeam$name$fr
#> [1] "Sénateurs"
#> 
#> 
#> $bottomSeedTeam$abbrev
#> [1] "OTT"
#> 
#> $bottomSeedTeam$placeName
#> $bottomSeedTeam$placeName$default
#> [1] "Ottawa"
#> 
#> 
#> $bottomSeedTeam$placeNameWithPreposition
#> $bottomSeedTeam$placeNameWithPreposition$default
#> [1] "Ottawa"
#> 
#> $bottomSeedTeam$placeNameWithPreposition$fr
#> [1] "d'Ottawa"
#> 
#> 
#> $bottomSeedTeam$conference
#> $bottomSeedTeam$conference$name
#> [1] "Eastern"
#> 
#> $bottomSeedTeam$conference$abbrev
#> [1] "E"
#> 
#> 
#> $bottomSeedTeam$record
#> [1] "2-4"
#> 
#> $bottomSeedTeam$seriesWins
#> [1] 2
#> 
#> $bottomSeedTeam$divisionAbbrev
#> [1] "A"
#> 
#> $bottomSeedTeam$seed
#> [1] 4
#> 
#> $bottomSeedTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/OTT_light.svg"
#> 
#> $bottomSeedTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/OTT_dark.svg"
#> 
#> 
#> $topSeedTeam
#> $topSeedTeam$id
#> [1] 10
#> 
#> $topSeedTeam$name
#> $topSeedTeam$name$default
#> [1] "Maple Leafs"
#> 
#> 
#> $topSeedTeam$abbrev
#> [1] "TOR"
#> 
#> $topSeedTeam$placeName
#> $topSeedTeam$placeName$default
#> [1] "Toronto"
#> 
#> 
#> $topSeedTeam$placeNameWithPreposition
#> $topSeedTeam$placeNameWithPreposition$default
#> [1] "Toronto"
#> 
#> $topSeedTeam$placeNameWithPreposition$fr
#> [1] "de Toronto"
#> 
#> 
#> $topSeedTeam$conference
#> $topSeedTeam$conference$name
#> [1] "Eastern"
#> 
#> $topSeedTeam$conference$abbrev
#> [1] "E"
#> 
#> 
#> $topSeedTeam$record
#> [1] "7-6"
#> 
#> $topSeedTeam$seriesWins
#> [1] 4
#> 
#> $topSeedTeam$divisionAbbrev
#> [1] "A"
#> 
#> $topSeedTeam$seed
#> [1] 1
#> 
#> $topSeedTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/TOR_light.svg"
#> 
#> $topSeedTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/TOR_dark.svg"
#> 
#> 
#> $games
#>           id   season gameType gameNumber ifNecessary neutralSite
#> 1 2024030111 20242025        3          1       FALSE       FALSE
#> 2 2024030112 20242025        3          2       FALSE       FALSE
#> 3 2024030113 20242025        3          3       FALSE       FALSE
#> 4 2024030114 20242025        3          4       FALSE       FALSE
#> 5 2024030115 20242025        3          5       FALSE       FALSE
#> 6 2024030116 20242025        3          6       FALSE       FALSE
#>           startTimeUTC easternUTCOffset venueUTCOffset   venueTimezone
#> 1 2025-04-20T23:00:00Z           -04:00         -04:00 America/Toronto
#> 2 2025-04-22T23:30:00Z           -04:00         -04:00 America/Toronto
#> 3 2025-04-24T23:00:00Z           -04:00         -04:00      US/Eastern
#> 4 2025-04-26T23:00:00Z           -04:00         -04:00      US/Eastern
#> 5 2025-04-29T23:00:00Z           -04:00         -04:00 America/Toronto
#> 6 2025-05-01T23:00:00Z           -04:00         -04:00      US/Eastern
#>   gameState gameScheduleState
#> 1       OFF                OK
#> 2       OFF                OK
#> 3       OFF                OK
#> 4       OFF                OK
#> 5       OFF                OK
#> 6       OFF                OK
#>                                                                                                                      tvBroadcasts
#> 1                                           4, 281, 282, 307, N, N, N, N, CA, CA, CA, US, CBC, TVAS, SN, ESPN2, 101, 120, 107, 12
#> 2                                           4, 281, 282, 307, N, N, N, N, CA, CA, CA, US, CBC, TVAS, SN, ESPN2, 101, 120, 107, 12
#> 3 4, 281, 287, 288, 290, 307, N, N, N, N, N, N, CA, CA, CA, CA, CA, US, CBC, TVAS, SNE, SNO, SNP, ESPN2, 101, 120, 30, 31, 33, 12
#> 4 4, 281, 282, 403, 501, 519, N, N, N, N, N, N, CA, CA, CA, US, US, US, CBC, TVAS, SN, TBS, truTV, MAX, 101, 120, 107, 13, 14, 18
#> 5                                            4, 281, 282, 309, N, N, N, N, CA, CA, CA, US, CBC, TVAS, SN, ESPN, 101, 120, 107, 10
#> 6                        4, 281, 282, 403, 519, N, N, N, N, N, CA, CA, CA, US, US, CBC, TVAS, SN, TBS, MAX, 101, 120, 107, 13, 18
#>                                 gameCenterLink        venue.default awayTeam.id
#> 1 /gamecenter/ott-vs-tor/2025/04/20/2024030111     Scotiabank Arena           9
#> 2 /gamecenter/ott-vs-tor/2025/04/22/2024030112     Scotiabank Arena           9
#> 3 /gamecenter/tor-vs-ott/2025/04/24/2024030113 Canadian Tire Centre          10
#> 4 /gamecenter/tor-vs-ott/2025/04/26/2024030114 Canadian Tire Centre          10
#> 5 /gamecenter/ott-vs-tor/2025/04/29/2024030115     Scotiabank Arena           9
#> 6 /gamecenter/tor-vs-ott/2025/05/01/2024030116 Canadian Tire Centre          10
#>   awayTeam.abbrev awayTeam.score awayTeam.commonName.default
#> 1             OTT              2                    Senators
#> 2             OTT              2                    Senators
#> 3             TOR              3                 Maple Leafs
#> 4             TOR              3                 Maple Leafs
#> 5             OTT              4                    Senators
#> 6             TOR              4                 Maple Leafs
#>   awayTeam.commonName.fr awayTeam.placeName.default
#> 1              Sénateurs                     Ottawa
#> 2              Sénateurs                     Ottawa
#> 3                   <NA>                    Toronto
#> 4                   <NA>                    Toronto
#> 5              Sénateurs                     Ottawa
#> 6                   <NA>                    Toronto
#>   awayTeam.placeNameWithPreposition.default
#> 1                                    Ottawa
#> 2                                    Ottawa
#> 3                                   Toronto
#> 4                                   Toronto
#> 5                                    Ottawa
#> 6                                   Toronto
#>   awayTeam.placeNameWithPreposition.fr homeTeam.id homeTeam.abbrev
#> 1                             d'Ottawa          10             TOR
#> 2                             d'Ottawa          10             TOR
#> 3                           de Toronto           9             OTT
#> 4                           de Toronto           9             OTT
#> 5                             d'Ottawa          10             TOR
#> 6                           de Toronto           9             OTT
#>   homeTeam.score homeTeam.commonName.default homeTeam.commonName.fr
#> 1              6                 Maple Leafs                   <NA>
#> 2              3                 Maple Leafs                   <NA>
#> 3              2                    Senators              Sénateurs
#> 4              4                    Senators              Sénateurs
#> 5              0                 Maple Leafs                   <NA>
#> 6              2                    Senators              Sénateurs
#>   homeTeam.placeName.default homeTeam.placeNameWithPreposition.default
#> 1                    Toronto                                   Toronto
#> 2                    Toronto                                   Toronto
#> 3                     Ottawa                                    Ottawa
#> 4                     Ottawa                                    Ottawa
#> 5                    Toronto                                   Toronto
#> 6                     Ottawa                                    Ottawa
#>   homeTeam.placeNameWithPreposition.fr periodDescriptor.number
#> 1                           de Toronto                       3
#> 2                           de Toronto                       4
#> 3                             d'Ottawa                       4
#> 4                             d'Ottawa                       4
#> 5                           de Toronto                       3
#> 6                             d'Ottawa                       3
#>   periodDescriptor.periodType periodDescriptor.maxRegulationPeriods
#> 1                         REG                                     3
#> 2                          OT                                     3
#> 3                          OT                                     3
#> 4                          OT                                     3
#> 5                         REG                                     3
#> 6                         REG                                     3
#>   seriesStatus.topSeedWins seriesStatus.bottomSeedWins
#> 1                        1                           0
#> 2                        2                           0
#> 3                        3                           0
#> 4                        3                           1
#> 5                        3                           2
#> 6                        4                           2
#>   gameOutcome.lastPeriodType gameOutcome.otPeriods
#> 1                        REG                    NA
#> 2                         OT                     1
#> 3                         OT                     1
#> 4                         OT                     1
#> 5                        REG                    NA
#> 6                        REG                    NA
#> 
#> $fullCoverageUrl
#> $fullCoverageUrl$cs
#> [1] "https://www.nhl.com/cs/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$de
#> [1] "https://www.nhl.com/de/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$fi
#> [1] "https://www.nhl.com/fi/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$sv
#> [1] "https://www.nhl.com/sv/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$sk
#> [1] "https://www.nhl.com/sk/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$en
#> [1] "https://www.nhl.com/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$fr
#> [1] "https://www.nhl.com/fr/playoffs/2025/series-a-coverage"
#> 
#> $fullCoverageUrl$es
#> [1] "https://www.nhl.com/es/playoffs/2025/series-a-coverage"
#> 
#> 
# }
```
