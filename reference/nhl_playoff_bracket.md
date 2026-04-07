# **NHL Playoff Bracket**

Returns the complete playoff bracket for a given year.

## Usage

``` r
nhl_playoff_bracket(year)
```

## Arguments

- year:

  Integer 4-digit year (e.g., 2024 for the 2024 playoffs).

## Value

Returns a list with complete bracket data (series, matchups, results).

## Examples

``` r
# \donttest{
  try(nhl_playoff_bracket(year = 2024))
#> $bracketLogo
#> [1] "https://assets.nhle.com/logos/playoffs/png/scp-20232024-horizontal-banner-en.png"
#> 
#> $bracketLogoFr
#> [1] "https://assets.nhle.com/logos/playoffs/png/scp-20232024-horizontal-banner-fr.png"
#> 
#> $series
#>                                                         seriesUrl
#> 1    /schedule/playoff-series/2024/series-a/lightning-vs-panthers
#> 2     /schedule/playoff-series/2024/series-b/mapleleafs-vs-bruins
#> 3      /schedule/playoff-series/2024/series-c/capitals-vs-rangers
#> 4  /schedule/playoff-series/2024/series-d/islanders-vs-hurricanes
#> 5   /schedule/playoff-series/2024/series-e/goldenknights-vs-stars
#> 6        /schedule/playoff-series/2024/series-f/avalanche-vs-jets
#> 7     /schedule/playoff-series/2024/series-g/predators-vs-canucks
#> 8          /schedule/playoff-series/2024/series-h/kings-vs-oilers
#> 9       /schedule/playoff-series/2024/series-i/bruins-vs-panthers
#> 10   /schedule/playoff-series/2024/series-j/hurricanes-vs-rangers
#> 11      /schedule/playoff-series/2024/series-k/avalanche-vs-stars
#> 12       /schedule/playoff-series/2024/series-l/oilers-vs-canucks
#> 13     /schedule/playoff-series/2024/series-m/panthers-vs-rangers
#> 14         /schedule/playoff-series/2024/series-n/oilers-vs-stars
#> 15      /schedule/playoff-series/2024/series-o/oilers-vs-panthers
#>                  seriesTitle seriesAbbrev seriesLetter playoffRound topSeedRank
#> 1                  1st Round           R1            A            1           1
#> 2                  1st Round           R1            B            1           2
#> 3                  1st Round           R1            C            1           1
#> 4                  1st Round           R1            D            1           2
#> 5                  1st Round           R1            E            1           1
#> 6                  1st Round           R1            F            1           2
#> 7                  1st Round           R1            G            1           1
#> 8                  1st Round           R1            H            1           2
#> 9                  2nd Round           R2            I            2           1
#> 10                 2nd Round           R2            J            2           1
#> 11                 2nd Round           R2            K            2           1
#> 12                 2nd Round           R2            L            2           1
#> 13 Eastern Conference Finals          ECF            M            3           1
#> 14 Western Conference Finals          WCF            N            3           1
#> 15         Stanley Cup Final          SCF            O            4           1
#>    topSeedRankAbbrev topSeedWins bottomSeedRank bottomSeedRankAbbrev
#> 1                 D1           4              4                  WC1
#> 2                 D2           4              3                   D3
#> 3                 D1           4              4                  WC2
#> 4                 D2           4              3                   D3
#> 5                 D1           4              4                  WC2
#> 6                 D2           1              3                   D3
#> 7                 D1           4              4                  WC1
#> 8                 D2           4              3                   D3
#> 9                 D1           4              2                   D2
#> 10                D1           4              2                   D2
#> 11                D1           4              3                   D3
#> 12                D1           3              2                   D2
#> 13                D1           2              1                   D1
#> 14                D1           2              2                   D2
#> 15                D1           4              2                   D2
#>    bottomSeedWins winningTeamId losingTeamId
#> 1               1            13           14
#> 2               3             6           10
#> 3               0             3           15
#> 4               1            12            2
#> 5               3            25           54
#> 6               4            21           52
#> 7               2            23           18
#> 8               1            22           26
#> 9               2            13            6
#> 10              2             3           12
#> 11              2            25           21
#> 12              4            22           23
#> 13              4            13            3
#> 14              4            22           25
#> 15              3            13           22
#>                                                                 seriesLogo
#> 1                                                                     <NA>
#> 2                                                                     <NA>
#> 3                                                                     <NA>
#> 4                                                                     <NA>
#> 5                                                                     <NA>
#> 6                                                                     <NA>
#> 7                                                                     <NA>
#> 8                                                                     <NA>
#> 9                                                                     <NA>
#> 10                                                                    <NA>
#> 11                                                                    <NA>
#> 12                                                                    <NA>
#> 13 https://assets.nhle.com/logos/playoffs/png/ecf-20232024-wordmark-en.png
#> 14 https://assets.nhle.com/logos/playoffs/png/wcf-20232024-wordmark-en.png
#> 15                https://assets.nhle.com/logos/playoffs/png/sc-banner.png
#>                                                               seriesLogoFr
#> 1                                                                     <NA>
#> 2                                                                     <NA>
#> 3                                                                     <NA>
#> 4                                                                     <NA>
#> 5                                                                     <NA>
#> 6                                                                     <NA>
#> 7                                                                     <NA>
#> 8                                                                     <NA>
#> 9                                                                     <NA>
#> 10                                                                    <NA>
#> 11                                                                    <NA>
#> 12                                                                    <NA>
#> 13 https://assets.nhle.com/logos/playoffs/png/ecf-20232024-wordmark-fr.png
#> 14 https://assets.nhle.com/logos/playoffs/png/wcf-20232024-wordmark-fr.png
#> 15                https://assets.nhle.com/logos/playoffs/png/sc-banner.png
#>    conferenceAbbrev conferenceName topSeedTeam.id topSeedTeam.abbrev
#> 1              <NA>           <NA>             13                FLA
#> 2              <NA>           <NA>              6                BOS
#> 3              <NA>           <NA>              3                NYR
#> 4              <NA>           <NA>             12                CAR
#> 5              <NA>           <NA>             25                DAL
#> 6              <NA>           <NA>             52                WPG
#> 7              <NA>           <NA>             23                VAN
#> 8              <NA>           <NA>             22                EDM
#> 9              <NA>           <NA>             13                FLA
#> 10             <NA>           <NA>              3                NYR
#> 11             <NA>           <NA>             25                DAL
#> 12             <NA>           <NA>             23                VAN
#> 13                E        Eastern              3                NYR
#> 14                W        Western             25                DAL
#> 15             <NA>           <NA>             13                FLA
#>                                                topSeedTeam.logo
#> 1           https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/BOS_20232024_light.svg
#> 3           https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 4           https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 5           https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 6           https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 7           https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 8           https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 9           https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 10          https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 11          https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 12          https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 13          https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 14          https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 15          https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#>                                           topSeedTeam.darkLogo
#> 1           https://assets.nhle.com/logos/nhl/svg/FLA_dark.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/BOS_20232024_dark.svg
#> 3           https://assets.nhle.com/logos/nhl/svg/NYR_dark.svg
#> 4           https://assets.nhle.com/logos/nhl/svg/CAR_dark.svg
#> 5           https://assets.nhle.com/logos/nhl/svg/DAL_dark.svg
#> 6           https://assets.nhle.com/logos/nhl/svg/WPG_dark.svg
#> 7           https://assets.nhle.com/logos/nhl/svg/VAN_dark.svg
#> 8           https://assets.nhle.com/logos/nhl/svg/EDM_dark.svg
#> 9           https://assets.nhle.com/logos/nhl/svg/FLA_dark.svg
#> 10          https://assets.nhle.com/logos/nhl/svg/NYR_dark.svg
#> 11          https://assets.nhle.com/logos/nhl/svg/DAL_dark.svg
#> 12          https://assets.nhle.com/logos/nhl/svg/VAN_dark.svg
#> 13          https://assets.nhle.com/logos/nhl/svg/NYR_dark.svg
#> 14          https://assets.nhle.com/logos/nhl/svg/DAL_dark.svg
#> 15          https://assets.nhle.com/logos/nhl/svg/FLA_dark.svg
#>    topSeedTeam.name.default       topSeedTeam.name.fr
#> 1          Florida Panthers    Panthers de la Floride
#> 2             Boston Bruins          Bruins de Boston
#> 3          New York Rangers       Rangers de New York
#> 4       Carolina Hurricanes Hurricanes de la Caroline
#> 5              Dallas Stars           Stars de Dallas
#> 6             Winnipeg Jets          Jets de Winnipeg
#> 7         Vancouver Canucks      Canucks de Vancouver
#> 8           Edmonton Oilers         Oilers d'Edmonton
#> 9          Florida Panthers    Panthers de la Floride
#> 10         New York Rangers       Rangers de New York
#> 11             Dallas Stars           Stars de Dallas
#> 12        Vancouver Canucks      Canucks de Vancouver
#> 13         New York Rangers       Rangers de New York
#> 14             Dallas Stars           Stars de Dallas
#> 15         Florida Panthers    Panthers de la Floride
#>    topSeedTeam.commonName.default topSeedTeam.placeNameWithPreposition.default
#> 1                        Panthers                                      Florida
#> 2                          Bruins                                       Boston
#> 3                         Rangers                                     New York
#> 4                      Hurricanes                                     Carolina
#> 5                           Stars                                       Dallas
#> 6                            Jets                                     Winnipeg
#> 7                         Canucks                                    Vancouver
#> 8                          Oilers                                     Edmonton
#> 9                        Panthers                                      Florida
#> 10                        Rangers                                     New York
#> 11                          Stars                                       Dallas
#> 12                        Canucks                                    Vancouver
#> 13                        Rangers                                     New York
#> 14                          Stars                                       Dallas
#> 15                       Panthers                                      Florida
#>    topSeedTeam.placeNameWithPreposition.fr bottomSeedTeam.id
#> 1                            de la Floride                14
#> 2                                de Boston                10
#> 3                              de New York                15
#> 4                           de la Caroline                 2
#> 5                                de Dallas                54
#> 6                              de Winnipeg                21
#> 7                             de Vancouver                18
#> 8                               d'Edmonton                26
#> 9                            de la Floride                 6
#> 10                             de New York                12
#> 11                               de Dallas                21
#> 12                            de Vancouver                22
#> 13                             de New York                13
#> 14                               de Dallas                22
#> 15                           de la Floride                22
#>    bottomSeedTeam.abbrev
#> 1                    TBL
#> 2                    TOR
#> 3                    WSH
#> 4                    NYI
#> 5                    VGK
#> 6                    COL
#> 7                    NSH
#> 8                    LAK
#> 9                    BOS
#> 10                   CAR
#> 11                   COL
#> 12                   EDM
#> 13                   FLA
#> 14                   EDM
#> 15                   EDM
#>                                                      bottomSeedTeam.logo
#> 1                    https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 2                    https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 3          https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 4                    https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 5                    https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 6                    https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 7                    https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/LAK_20192020-20232024_light.svg
#> 9           https://assets.nhle.com/logos/nhl/svg/BOS_20232024_light.svg
#> 10                   https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 11                   https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 12                   https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 13                   https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 14                   https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 15                   https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#>                                                 bottomSeedTeam.darkLogo
#> 1                    https://assets.nhle.com/logos/nhl/svg/TBL_dark.svg
#> 2                    https://assets.nhle.com/logos/nhl/svg/TOR_dark.svg
#> 3          https://assets.nhle.com/logos/nhl/svg/WSH_secondary_dark.svg
#> 4                    https://assets.nhle.com/logos/nhl/svg/NYI_dark.svg
#> 5                    https://assets.nhle.com/logos/nhl/svg/VGK_dark.svg
#> 6                    https://assets.nhle.com/logos/nhl/svg/COL_dark.svg
#> 7                    https://assets.nhle.com/logos/nhl/svg/NSH_dark.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/LAK_20192020-20232024_dark.svg
#> 9           https://assets.nhle.com/logos/nhl/svg/BOS_20232024_dark.svg
#> 10                   https://assets.nhle.com/logos/nhl/svg/CAR_dark.svg
#> 11                   https://assets.nhle.com/logos/nhl/svg/COL_dark.svg
#> 12                   https://assets.nhle.com/logos/nhl/svg/EDM_dark.svg
#> 13                   https://assets.nhle.com/logos/nhl/svg/FLA_dark.svg
#> 14                   https://assets.nhle.com/logos/nhl/svg/EDM_dark.svg
#> 15                   https://assets.nhle.com/logos/nhl/svg/EDM_dark.svg
#>    bottomSeedTeam.name.default    bottomSeedTeam.name.fr
#> 1          Tampa Bay Lightning    Lightning de Tampa Bay
#> 2          Toronto Maple Leafs    Maple Leafs de Toronto
#> 3          Washington Capitals    Capitals de Washington
#> 4           New York Islanders     Islanders de New York
#> 5         Vegas Golden Knights   Golden Knights de Vegas
#> 6           Colorado Avalanche     Avalanche du Colorado
#> 7          Nashville Predators    Predators de Nashville
#> 8            Los Angeles Kings      Kings de Los Angeles
#> 9                Boston Bruins          Bruins de Boston
#> 10         Carolina Hurricanes Hurricanes de la Caroline
#> 11          Colorado Avalanche     Avalanche du Colorado
#> 12             Edmonton Oilers         Oilers d'Edmonton
#> 13            Florida Panthers    Panthers de la Floride
#> 14             Edmonton Oilers         Oilers d'Edmonton
#> 15             Edmonton Oilers         Oilers d'Edmonton
#>    bottomSeedTeam.commonName.default
#> 1                          Lightning
#> 2                        Maple Leafs
#> 3                           Capitals
#> 4                          Islanders
#> 5                     Golden Knights
#> 6                          Avalanche
#> 7                          Predators
#> 8                              Kings
#> 9                             Bruins
#> 10                        Hurricanes
#> 11                         Avalanche
#> 12                            Oilers
#> 13                          Panthers
#> 14                            Oilers
#> 15                            Oilers
#>    bottomSeedTeam.placeNameWithPreposition.default
#> 1                                        Tampa Bay
#> 2                                          Toronto
#> 3                                       Washington
#> 4                                         New York
#> 5                                            Vegas
#> 6                                         Colorado
#> 7                                        Nashville
#> 8                                      Los Angeles
#> 9                                           Boston
#> 10                                        Carolina
#> 11                                        Colorado
#> 12                                        Edmonton
#> 13                                         Florida
#> 14                                        Edmonton
#> 15                                        Edmonton
#>    bottomSeedTeam.placeNameWithPreposition.fr
#> 1                                de Tampa Bay
#> 2                                  de Toronto
#> 3                               de Washington
#> 4                                 de New York
#> 5                                    de Vegas
#> 6                                 du Colorado
#> 7                                de Nashville
#> 8                              de Los Angeles
#> 9                                   de Boston
#> 10                             de la Caroline
#> 11                                du Colorado
#> 12                                 d'Edmonton
#> 13                              de la Floride
#> 14                                 d'Edmonton
#> 15                                 d'Edmonton
#> 
# }
```
