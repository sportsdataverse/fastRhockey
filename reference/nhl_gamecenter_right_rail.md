# **NHL Gamecenter Right Rail**

Returns the gamecenter right rail data for a given game, including 3
stars, team leaders, and season series.

## Usage

``` r
nhl_gamecenter_right_rail(game_id)
```

## Arguments

- game_id:

  Integer or character game ID (e.g., 2024020001)

## Value

Returns a list with gamecenter right rail data.

## Examples

``` r
# \donttest{
  try(nhl_gamecenter_right_rail(game_id = 2024020001))
#> $seasonSeries
#>           id   season gameType   gameDate         startTimeUTC easternUTCOffset
#> 1 2024020001 20242025        2 2024-10-04 2024-10-04T17:00:00Z           -04:00
#> 2 2024020002 20242025        2 2024-10-05 2024-10-05T14:00:00Z           -04:00
#> 3 2024020835 20242025        2 2025-02-02 2025-02-02T18:00:00Z           -05:00
#>   venueUTCOffset gameState gameScheduleState
#> 1         +02:00       OFF                OK
#> 2         +02:00       OFF                OK
#> 3         -05:00       OFF                OK
#>                                 gameCenterLink awayTeam.id awayTeam.abbrev
#> 1 /gamecenter/buf-vs-njd/2024/10/04/2024020001           1             NJD
#> 2 /gamecenter/njd-vs-buf/2024/10/05/2024020002           7             BUF
#> 3 /gamecenter/buf-vs-njd/2025/02/02/2024020835           1             NJD
#>                                         awayTeam.logo awayTeam.score
#> 1 https://assets.nhle.com/logos/nhl/svg/NJD_light.svg              4
#> 2 https://assets.nhle.com/logos/nhl/svg/BUF_light.svg              1
#> 3 https://assets.nhle.com/logos/nhl/svg/NJD_light.svg              3
#>   homeTeam.id homeTeam.abbrev
#> 1           7             BUF
#> 2           1             NJD
#> 3           7             BUF
#>                                         homeTeam.logo homeTeam.score
#> 1 https://assets.nhle.com/logos/nhl/svg/BUF_light.svg              1
#> 2 https://assets.nhle.com/logos/nhl/svg/NJD_light.svg              3
#> 3 https://assets.nhle.com/logos/nhl/svg/BUF_light.svg              4
#>   periodDescriptor.number periodDescriptor.periodType
#> 1                       3                         REG
#> 2                       3                         REG
#> 3                       3                         REG
#>   periodDescriptor.maxRegulationPeriods gameOutcome.lastPeriodType
#> 1                                     3                        REG
#> 2                                     3                        REG
#> 3                                     3                        REG
#> 
#> $seasonSeriesWins
#> $seasonSeriesWins$awayTeamWins
#> [1] 2
#> 
#> $seasonSeriesWins$homeTeamWins
#> [1] 1
#> 
#> 
#> $gameInfo
#> $gameInfo$referees
#>           default
#> 1 Ghislain Hebert
#> 2       Chris Lee
#> 
#> $gameInfo$linesmen
#>          default
#> 1  Brad Kovachik
#> 2 Michel Cormier
#> 
#> $gameInfo$awayTeam
#> $gameInfo$awayTeam$headCoach
#> $gameInfo$awayTeam$headCoach$default
#> [1] "Sheldon Keefe"
#> 
#> 
#> $gameInfo$awayTeam$scratches
#>         id firstName.default firstName.cs firstName.de firstName.es
#> 1  8475413            Justin         <NA>         <NA>         <NA>
#> 2  8477073            Kurtis         <NA>         <NA>         <NA>
#> 3  8477488             Brett         <NA>         <NA>         <NA>
#> 4  8478051               Max      Maxwell      Maxwell      Maxwell
#> 5  8478841            Colton         <NA>         <NA>         <NA>
#> 6  8478956            Samuel         <NA>         <NA>         <NA>
#> 7  8480032             Shane         <NA>         <NA>         <NA>
#> 8  8480084              Nick         <NA>         <NA>         <NA>
#> 9  8481115              Ryan         <NA>         <NA>         <NA>
#> 10 8481518             Nolan         <NA>         <NA>         <NA>
#> 11 8481536            Daniil         <NA>         <NA>         <NA>
#> 12 8481550              Adam         <NA>         <NA>         <NA>
#> 13 8481594            Nathan         <NA>         <NA>         <NA>
#> 14 8481701           Santeri         <NA>         <NA>         <NA>
#> 15 8482076              Nico         <NA>         <NA>         <NA>
#> 16 8482635              Mike         <NA>         <NA>         <NA>
#> 17 8482684              Luke         <NA>         <NA>         <NA>
#> 18 8482714             Chase         <NA>         <NA>         <NA>
#> 19 8482873            Topias         <NA>         <NA>         <NA>
#> 20 8483085            Mikael         <NA>         <NA>         <NA>
#> 21 8483439              Josh         <NA>         <NA>         <NA>
#> 22 8483531             Brian         <NA>         <NA>         <NA>
#> 23 8483659             Isaac         <NA>         <NA>         <NA>
#> 24 8484405               Cam         <NA>         <NA>         <NA>
#>    firstName.fi firstName.sk firstName.sv lastName.default lastName.cs
#> 1          <NA>         <NA>         <NA>          Dowling        <NA>
#> 2          <NA>         <NA>         <NA>        MacDermid        <NA>
#> 3          <NA>         <NA>         <NA>            Pesce        <NA>
#> 4       Maxwell      Maxwell      Maxwell          Willman        <NA>
#> 5          <NA>         <NA>         <NA>            White        <NA>
#> 6          <NA>         <NA>         <NA>          Laberge        <NA>
#> 7          <NA>         <NA>         <NA>           Bowers        <NA>
#> 8          <NA>         <NA>         <NA>         DeSimone        <NA>
#> 9          <NA>         <NA>         <NA>        Schmelzer        <NA>
#> 10         <NA>         <NA>         <NA>            Foote        <NA>
#> 11         <NA>         <NA>         <NA>           Misyul      Misjul
#> 12         <NA>         <NA>         <NA>          Beckman        <NA>
#> 13         <NA>         <NA>         <NA>           Légaré        <NA>
#> 14         <NA>         <NA>         <NA>          Hatakka        <NA>
#> 15         <NA>         <NA>         <NA>             Daws        <NA>
#> 16         <NA>         <NA>         <NA>          Hardman        <NA>
#> 17         <NA>         <NA>         <NA>           Hughes        <NA>
#> 18         <NA>         <NA>         <NA>         Stillman        <NA>
#> 19         <NA>         <NA>         <NA>            Vilen        <NA>
#> 20         <NA>         <NA>         <NA>           Diotte        <NA>
#> 21         <NA>         <NA>         <NA>           Filmon        <NA>
#> 22         <NA>         <NA>         <NA>          Halonen        <NA>
#> 23         <NA>         <NA>         <NA>          Poulter        <NA>
#> 24         <NA>         <NA>         <NA>          Squires        <NA>
#>    lastName.fi lastName.sk
#> 1         <NA>        <NA>
#> 2         <NA>        <NA>
#> 3         <NA>        <NA>
#> 4         <NA>        <NA>
#> 5         <NA>        <NA>
#> 6         <NA>        <NA>
#> 7         <NA>        <NA>
#> 8         <NA>        <NA>
#> 9         <NA>        <NA>
#> 10        <NA>        <NA>
#> 11      Misjul      Misjul
#> 12        <NA>        <NA>
#> 13        <NA>        <NA>
#> 14        <NA>        <NA>
#> 15        <NA>        <NA>
#> 16        <NA>        <NA>
#> 17        <NA>        <NA>
#> 18        <NA>        <NA>
#> 19        <NA>        <NA>
#> 20        <NA>        <NA>
#> 21        <NA>        <NA>
#> 22        <NA>        <NA>
#> 23        <NA>        <NA>
#> 24        <NA>        <NA>
#> 
#> 
#> $gameInfo$homeTeam
#> $gameInfo$homeTeam$headCoach
#> $gameInfo$homeTeam$headCoach$default
#> [1] "Lindy Ruff"
#> 
#> 
#> $gameInfo$homeTeam$scratches
#>        id firstName.default firstName.cs firstName.sk lastName.default
#> 1 8473503             James         <NA>         <NA>           Reimer
#> 2 8478502            Dennis         <NA>         <NA>          Gilbert
#> 3 8479348              Kale         <NA>         <NA>           Clague
#> 4 8480196             Jacob         <NA>         <NA>           Bryson
#> 5 8481522            Peyton         <NA>         <NA>            Krebs
#> 6 8481564              Ryan         <NA>         <NA>          Johnson
#> 7 8481751             Lukas         <NA>         <NA>           Rousek
#> 8 8483468              Jiri         Jiří         Jiří           Kulich
#> 
#> 
#> 
#> $linescore
#> $linescore$byPeriod
#>   away home periodDescriptor.number periodDescriptor.periodType
#> 1    2    0                       1                         REG
#> 2    1    0                       2                         REG
#> 3    1    1                       3                         REG
#>   periodDescriptor.maxRegulationPeriods
#> 1                                     3
#> 2                                     3
#> 3                                     3
#> 
#> $linescore$totals
#> $linescore$totals$away
#> [1] 4
#> 
#> $linescore$totals$home
#> [1] 1
#> 
#> 
#> 
#> $shotsByPeriod
#>   away home periodDescriptor.number periodDescriptor.periodType
#> 1   11    7                       1                         REG
#> 2    8   11                       2                         REG
#> 3    4   13                       3                         REG
#>   periodDescriptor.maxRegulationPeriods
#> 1                                     3
#> 2                                     3
#> 3                                     3
#> 
#> $teamGameStats
#>              category awayValue homeValue
#> 1                 sog        23        31
#> 2  faceoffWinningPctg  0.423729  0.576271
#> 3         faceoffWins     25/59     34/59
#> 4           powerPlay       0/2       0/4
#> 5       powerPlayPctg         0         0
#> 6                 pim         8         4
#> 7                hits        34        29
#> 8        blockedShots        18        15
#> 9           giveaways        22        12
#> 10          takeaways         2         5
#> 
#> $gameReports
#> $gameReports$gameSummary
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/GS020001.HTM"
#> 
#> $gameReports$eventSummary
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/ES020001.HTM"
#> 
#> $gameReports$playByPlay
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/PL020001.HTM"
#> 
#> $gameReports$faceoffSummary
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/FS020001.HTM"
#> 
#> $gameReports$faceoffComparison
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/FC020001.HTM"
#> 
#> $gameReports$rosters
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/RO020001.HTM"
#> 
#> $gameReports$shotSummary
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/SS020001.HTM"
#> 
#> $gameReports$shiftChart
#> [1] "https://www.nhl.com/stats/shiftcharts?id=2024020001"
#> 
#> $gameReports$toiAway
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/TV020001.HTM"
#> 
#> $gameReports$toiHome
#> [1] "https://www.nhl.com/scores/htmlreports/20242025/TH020001.HTM"
#> 
#> 
# }
```
