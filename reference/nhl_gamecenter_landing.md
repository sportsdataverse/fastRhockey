# **NHL Gamecenter Landing**

Returns the gamecenter landing page data for a given game, including
rosters, linescore, game state, and summary information.

## Usage

``` r
nhl_gamecenter_landing(game_id)
```

## Arguments

- game_id:

  Integer or character game ID (e.g., 2024020001)

## Value

Returns a list with gamecenter landing data.

## Examples

``` r
# \donttest{
  try(nhl_gamecenter_landing(game_id = 2024020001))
#> $id
#> [1] 2024020001
#> 
#> $season
#> [1] 20242025
#> 
#> $gameType
#> [1] 2
#> 
#> $limitedScoring
#> [1] FALSE
#> 
#> $gameDate
#> [1] "2024-10-04"
#> 
#> $venue
#> $venue$default
#> [1] "O2 Czech Republic"
#> 
#> 
#> $venueLocation
#> $venueLocation$default
#> [1] "Prague"
#> 
#> $venueLocation$cs
#> [1] "Praha"
#> 
#> $venueLocation$de
#> [1] "Prag"
#> 
#> $venueLocation$fi
#> [1] "Praha"
#> 
#> $venueLocation$sk
#> [1] "Praha"
#> 
#> $venueLocation$sv
#> [1] "Prag"
#> 
#> 
#> $startTimeUTC
#> [1] "2024-10-04T17:00:00Z"
#> 
#> $easternUTCOffset
#> [1] "-04:00"
#> 
#> $venueUTCOffset
#> [1] "+02:00"
#> 
#> $venueTimezone
#> [1] "Europe/Prague"
#> 
#> $periodDescriptor
#> $periodDescriptor$number
#> [1] 3
#> 
#> $periodDescriptor$periodType
#> [1] "REG"
#> 
#> $periodDescriptor$maxRegulationPeriods
#> [1] 3
#> 
#> 
#> $tvBroadcasts
#>    id market countryCode network sequenceNumber
#> 1 324      N          US    NHLN             35
#> 2 282      N          CA      SN            107
#> 3  28      H          US   MSG-B            392
#> 4 409      A          US   MSGSN            411
#> 
#> $gameState
#> [1] "OFF"
#> 
#> $gameScheduleState
#> [1] "OK"
#> 
#> $specialEvent
#> $specialEvent$parentId
#> [1] 36
#> 
#> $specialEvent$name
#> $specialEvent$name$default
#> [1] "2024 NHL Global Series"
#> 
#> 
#> $specialEvent$lightLogoUrl
#> $specialEvent$lightLogoUrl$default
#> [1] "https://assets.nhle.com/special_event_season/20242025/svg/gs-cz-2024-extwm-en_light.svg"
#> 
#> 
#> 
#> $awayTeam
#> $awayTeam$id
#> [1] 1
#> 
#> $awayTeam$commonName
#> $awayTeam$commonName$default
#> [1] "Devils"
#> 
#> 
#> $awayTeam$abbrev
#> [1] "NJD"
#> 
#> $awayTeam$placeName
#> $awayTeam$placeName$default
#> [1] "New Jersey"
#> 
#> 
#> $awayTeam$placeNameWithPreposition
#> $awayTeam$placeNameWithPreposition$default
#> [1] "New Jersey"
#> 
#> $awayTeam$placeNameWithPreposition$fr
#> [1] "du New Jersey"
#> 
#> 
#> $awayTeam$score
#> [1] 4
#> 
#> $awayTeam$sog
#> [1] 23
#> 
#> $awayTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/NJD_light.svg"
#> 
#> $awayTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/NJD_dark.svg"
#> 
#> 
#> $homeTeam
#> $homeTeam$id
#> [1] 7
#> 
#> $homeTeam$commonName
#> $homeTeam$commonName$default
#> [1] "Sabres"
#> 
#> 
#> $homeTeam$abbrev
#> [1] "BUF"
#> 
#> $homeTeam$placeName
#> $homeTeam$placeName$default
#> [1] "Buffalo"
#> 
#> 
#> $homeTeam$placeNameWithPreposition
#> $homeTeam$placeNameWithPreposition$default
#> [1] "Buffalo"
#> 
#> $homeTeam$placeNameWithPreposition$fr
#> [1] "de Buffalo"
#> 
#> 
#> $homeTeam$score
#> [1] 1
#> 
#> $homeTeam$sog
#> [1] 31
#> 
#> $homeTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/BUF_light.svg"
#> 
#> $homeTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/BUF_dark.svg"
#> 
#> 
#> $shootoutInUse
#> [1] TRUE
#> 
#> $maxPeriods
#> [1] 5
#> 
#> $regPeriods
#> [1] 3
#> 
#> $otInUse
#> [1] TRUE
#> 
#> $tiesInUse
#> [1] FALSE
#> 
#> $summary
#> $summary$scoring
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        goals
#> 1 1551, 1551, 274, 1730620, ev, ev, 8476474, 8480192, https://assets.nhle.com/mugs/nhl/20242025/NJD/8476474.png, https://assets.nhle.com/mugs/nhl/20242025/NJD/8480192.png, https://nhl.com/video/njd-buf-noesen-scores-goal-against-ukko-pekka-luukkonen-6362848229112, https://nhl.com/video/njd-buf-kovacevic-scores-ppg-against-ukko-pekka-luukkonen-6362847541112, 6362848229112, 6362847541112, 6362846260112, 6362848928112, 1, 1, 1, 2, 0, 0, 08:39, 15:38, snap, wrist, none, none, 8480192, 1, 8, Johnathan, Kovacevic, J. Kovacevic, 8478399, 8482110, 1, 1, 71, 91, Jonas, Dawson, Siegenthaler, Mercer, J. Siegenthaler, D. Mercer, https://wsr.nhle.com/sprites/20242025/2024020001/ev274.json, https://wsr.nhle.com/sprites/20242025/2024020001/ev1730620.json, right, right, FALSE, FALSE, Stefan, Johnathan, Noesen, Kovacevic, S. Noesen, J. Kovacevic, NJD, NJD, NJD, NJD
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                  1551, 1730882, ev, 8480002, https://assets.nhle.com/mugs/nhl/20242025/NJD/8480002.png, https://nhl.com/video/njd-buf-hischier-scores-ppg-against-ukko-pekka-luukkonen-6362848582112, 6362848582112, 6362847992112, 1, 3, 0, 03:29, wrist, none, 8479414, 8481032, 1, 1, 14, 47, Nathan, Paul, Bastian, Cotter, N. Bastian, P. Cotter, https://wsr.nhle.com/sprites/20242025/2024020001/ev1730882.json, left, FALSE, Nico, Hischier, N. Hischier, NJD, NJD
#> 3                                      1551, 1560, 1731224, 1731328, ev, ev, 8482671, 8481032, https://assets.nhle.com/mugs/nhl/20242025/BUF/8482671.png, https://assets.nhle.com/mugs/nhl/20242025/NJD/8481032.png, https://nhl.com/video/njd-buf-power-scores-shg-against-jacob-markstrom-6362851617112, https://nhl.com/video/njd-buf-cotter-scores-goal-against-buffalo-sabres-6362851720112, 6362851617112, 6362851720112, 6362853182112, 6362854469112, 1, 1, 3, 4, 1, 1, 10:07, 17:28, wrist, backhand, none, empty-net, 8482175, 8481524, 1, 1, 77, 4, JJ, Bowen, Peterka, Byram, J. Peterka, B. Byram, 8479414, 2, 14, Nathan, Bastian, N. Bastian, https://wsr.nhle.com/sprites/20242025/2024020001/ev1731224.json, https://wsr.nhle.com/sprites/20242025/2024020001/ev1731328.json, right, right, TRUE, FALSE, Owen, Paul, Power, Cotter, O. Power, P. Cotter, BUF, NJD, NJD, NJD
#>   periodDescriptor.number periodDescriptor.periodType
#> 1                       1                         REG
#> 2                       2                         REG
#> 3                       3                         REG
#>   periodDescriptor.maxRegulationPeriods
#> 1                                     3
#> 2                                     3
#> 3                                     3
#> 
#> $summary$threeStars
#>   star playerId teamAbbrev
#> 1    1  8480192        NJD
#> 2    2  8474593        NJD
#> 3    3  8482671        BUF
#>                                                    headshot sweaterNo position
#> 1 https://assets.nhle.com/mugs/nhl/20242025/NJD/8480192.png         8        D
#> 2 https://assets.nhle.com/mugs/nhl/20242025/NJD/8474593.png        25        G
#> 3 https://assets.nhle.com/mugs/nhl/20242025/BUF/8482671.png        25        D
#>   goals assists points goalsAgainstAverage savePctg name.default      name.cs
#> 1     1       1      2                  NA       NA J. Kovacevic         <NA>
#> 2    NA      NA     NA                1.01    0.968 J. Markstrom J. Markström
#> 3     1       0      1                  NA       NA     O. Power         <NA>
#>        name.fi      name.sk      name.sv
#> 1         <NA>         <NA>         <NA>
#> 2 J. Markström J. Markström J. Markström
#> 3         <NA>         <NA>         <NA>
#> 
#> $summary$penalties
#>                                                                                                                                                                                                             penalties
#> 1 01:37, 10:38, 17:37, MIN, MIN, MIN, 2, 2, 2, slashing, interference, slashing, 56, 26, 96, Erik, Rasmus, Nicolas, Haula, Dahlin, Aube-Kubel, NJD, BUF, BUF, 72, 63, 86, Tage, Jesper, Jack, Thompson, Bratt, Hughes
#> 2                                                          06:15, 15:07, MIN, MIN, 2, 2, holding, holding, 42, 17, Curtis, Simon, NA, Šimon, NA, Šimon, Lazar, Nemec, NJD, NJD, 77, 12, JJ, Jordan, Peterka, Greenway
#> 3                                                                                                                                                   05:43, MIN, 2, holding, 14, Nathan, Bastian, NJD, 22, Jack, Quinn
#>   periodDescriptor.number periodDescriptor.periodType
#> 1                       1                         REG
#> 2                       2                         REG
#> 3                       3                         REG
#>   periodDescriptor.maxRegulationPeriods
#> 1                                     3
#> 2                                     3
#> 3                                     3
#> 
#> 
#> $clock
#> $clock$timeRemaining
#> [1] "00:00"
#> 
#> $clock$secondsRemaining
#> [1] 0
#> 
#> $clock$running
#> [1] FALSE
#> 
#> $clock$inIntermission
#> [1] FALSE
#> 
#> 
# }
```
