# **NHL Schedule Calendar**

Returns the schedule calendar showing which dates have games.

## Usage

``` r
nhl_schedule_calendar(date = NULL)
```

## Arguments

- date:

  Character date in "YYYY-MM-DD" format. If NULL, returns current.

## Value

Returns a list with calendar data.

## Examples

``` r
# \donttest{
  try(nhl_schedule_calendar())
#> $endDate
#> [1] "2026-04-12"
#> 
#> $nextStartDate
#> [1] "2026-04-13"
#> 
#> $previousStartDate
#> [1] "2026-03-30"
#> 
#> $startDate
#> [1] "2026-04-06"
#> 
#> $teams
#>    id seasonId abbrev
#> 1   1 20252026    NJD
#> 2   2 20252026    NYI
#> 3   3 20252026    NYR
#> 4   4 20252026    PHI
#> 5   5 20252026    PIT
#> 6   6 20252026    BOS
#> 7   7 20252026    BUF
#> 8   8 20252026    MTL
#> 9   9 20252026    OTT
#> 10 10 20252026    TOR
#> 11 12 20252026    CAR
#> 12 13 20252026    FLA
#> 13 14 20252026    TBL
#> 14 15 20252026    WSH
#> 15 16 20252026    CHI
#> 16 17 20252026    DET
#> 17 18 20252026    NSH
#> 18 19 20252026    STL
#> 19 20 20252026    CGY
#> 20 21 20252026    COL
#> 21 22 20252026    EDM
#> 22 23 20252026    VAN
#> 23 24 20252026    ANA
#> 24 25 20252026    DAL
#> 25 26 20252026    LAK
#> 26 28 20252026    SJS
#> 27 29 20252026    CBJ
#> 28 30 20252026    MIN
#> 29 52 20252026    WPG
#> 30 54 20252026    VGK
#> 31 55 20252026    SEA
#> 32 68 20252026    UTA
#>                                                                   logo
#> 1                  https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 2                  https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 3                  https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 4                  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 5                  https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/BOS_light.svg?season=20252026
#> 7                  https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 8                  https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 9                  https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 10                 https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 11                 https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 12                 https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 13                 https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 14       https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 15 https://assets.nhle.com/logos/nhl/svg/CHI_light.svg?season=20252026
#> 16 https://assets.nhle.com/logos/nhl/svg/DET_light.svg?season=20252026
#> 17                 https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 18 https://assets.nhle.com/logos/nhl/svg/STL_light.svg?season=20252026
#> 19                 https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#> 20                 https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 21                 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 22                 https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 23                 https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#> 24                 https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 25                 https://assets.nhle.com/logos/nhl/svg/LAK_light.svg
#> 26                 https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#> 27                 https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 28                 https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 29                 https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 30                 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 31                 https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#> 32 https://assets.nhle.com/logos/nhl/svg/UTA_light.svg?season=20252026
#>                                                              darkLogo french
#> 1                  https://assets.nhle.com/logos/nhl/svg/NJD_dark.svg  FALSE
#> 2                  https://assets.nhle.com/logos/nhl/svg/NYI_dark.svg  FALSE
#> 3                  https://assets.nhle.com/logos/nhl/svg/NYR_dark.svg  FALSE
#> 4                  https://assets.nhle.com/logos/nhl/svg/PHI_dark.svg  FALSE
#> 5                  https://assets.nhle.com/logos/nhl/svg/PIT_dark.svg  FALSE
#> 6  https://assets.nhle.com/logos/nhl/svg/BOS_dark.svg?season=20252026  FALSE
#> 7                  https://assets.nhle.com/logos/nhl/svg/BUF_dark.svg  FALSE
#> 8                  https://assets.nhle.com/logos/nhl/svg/MTL_dark.svg   TRUE
#> 9                  https://assets.nhle.com/logos/nhl/svg/OTT_dark.svg  FALSE
#> 10                 https://assets.nhle.com/logos/nhl/svg/TOR_dark.svg  FALSE
#> 11                 https://assets.nhle.com/logos/nhl/svg/CAR_dark.svg  FALSE
#> 12                 https://assets.nhle.com/logos/nhl/svg/FLA_dark.svg  FALSE
#> 13                 https://assets.nhle.com/logos/nhl/svg/TBL_dark.svg  FALSE
#> 14       https://assets.nhle.com/logos/nhl/svg/WSH_secondary_dark.svg  FALSE
#> 15 https://assets.nhle.com/logos/nhl/svg/CHI_dark.svg?season=20252026  FALSE
#> 16 https://assets.nhle.com/logos/nhl/svg/DET_dark.svg?season=20252026  FALSE
#> 17                 https://assets.nhle.com/logos/nhl/svg/NSH_dark.svg  FALSE
#> 18 https://assets.nhle.com/logos/nhl/svg/STL_dark.svg?season=20252026  FALSE
#> 19                 https://assets.nhle.com/logos/nhl/svg/CGY_dark.svg  FALSE
#> 20                 https://assets.nhle.com/logos/nhl/svg/COL_dark.svg  FALSE
#> 21                 https://assets.nhle.com/logos/nhl/svg/EDM_dark.svg  FALSE
#> 22                 https://assets.nhle.com/logos/nhl/svg/VAN_dark.svg  FALSE
#> 23                 https://assets.nhle.com/logos/nhl/svg/ANA_dark.svg  FALSE
#> 24                 https://assets.nhle.com/logos/nhl/svg/DAL_dark.svg  FALSE
#> 25                 https://assets.nhle.com/logos/nhl/svg/LAK_dark.svg  FALSE
#> 26                 https://assets.nhle.com/logos/nhl/svg/SJS_dark.svg  FALSE
#> 27                 https://assets.nhle.com/logos/nhl/svg/CBJ_dark.svg  FALSE
#> 28                 https://assets.nhle.com/logos/nhl/svg/MIN_dark.svg  FALSE
#> 29                 https://assets.nhle.com/logos/nhl/svg/WPG_dark.svg  FALSE
#> 30                 https://assets.nhle.com/logos/nhl/svg/VGK_dark.svg  FALSE
#> 31                 https://assets.nhle.com/logos/nhl/svg/SEA_dark.svg  FALSE
#> 32 https://assets.nhle.com/logos/nhl/svg/UTA_dark.svg?season=20252026  FALSE
#>    commonName.default commonName.fr          name.default
#> 1              Devils          <NA>     New Jersey Devils
#> 2           Islanders          <NA>    New York Islanders
#> 3             Rangers          <NA>      New York Rangers
#> 4              Flyers          <NA>   Philadelphia Flyers
#> 5            Penguins          <NA>   Pittsburgh Penguins
#> 6              Bruins          <NA>         Boston Bruins
#> 7              Sabres          <NA>        Buffalo Sabres
#> 8           Canadiens          <NA>    Montréal Canadiens
#> 9            Senators     Sénateurs       Ottawa Senators
#> 10        Maple Leafs          <NA>   Toronto Maple Leafs
#> 11         Hurricanes          <NA>   Carolina Hurricanes
#> 12           Panthers          <NA>      Florida Panthers
#> 13          Lightning          <NA>   Tampa Bay Lightning
#> 14           Capitals          <NA>   Washington Capitals
#> 15         Blackhawks          <NA>    Chicago Blackhawks
#> 16          Red Wings          <NA>     Detroit Red Wings
#> 17          Predators          <NA>   Nashville Predators
#> 18              Blues          <NA>       St. Louis Blues
#> 19             Flames          <NA>        Calgary Flames
#> 20          Avalanche          <NA>    Colorado Avalanche
#> 21             Oilers          <NA>       Edmonton Oilers
#> 22            Canucks          <NA>     Vancouver Canucks
#> 23              Ducks          <NA>         Anaheim Ducks
#> 24              Stars          <NA>          Dallas Stars
#> 25              Kings          <NA>     Los Angeles Kings
#> 26             Sharks          <NA>       San Jose Sharks
#> 27       Blue Jackets          <NA> Columbus Blue Jackets
#> 28               Wild          <NA>        Minnesota Wild
#> 29               Jets          <NA>         Winnipeg Jets
#> 30     Golden Knights          <NA>  Vegas Golden Knights
#> 31             Kraken          <NA>        Seattle Kraken
#> 32            Mammoth          <NA>          Utah Mammoth
#>                      name.fr placeNameWithPreposition.default
#> 1       Devils du New Jersey                       New Jersey
#> 2      Islanders de New York                         New York
#> 3        Rangers de New York                         New York
#> 4     Flyers de Philadelphie                     Philadelphia
#> 5     Penguins de Pittsburgh                       Pittsburgh
#> 6           Bruins de Boston                           Boston
#> 7          Sabres de Buffalo                          Buffalo
#> 8      Canadiens de Montréal                         Montréal
#> 9         Sénateurs d'Ottawa                           Ottawa
#> 10    Maple Leafs de Toronto                          Toronto
#> 11 Hurricanes de la Caroline                         Carolina
#> 12    Panthers de la Floride                          Florida
#> 13    Lightning de Tampa Bay                        Tampa Bay
#> 14    Capitals de Washington                       Washington
#> 15     Blackhawks de Chicago                          Chicago
#> 16      Red Wings de Detroit                          Detroit
#> 17    Predators de Nashville                        Nashville
#> 18        Blues de St. Louis                        St. Louis
#> 19         Flames de Calgary                          Calgary
#> 20     Avalanche du Colorado                         Colorado
#> 21         Oilers d'Edmonton                         Edmonton
#> 22      Canucks de Vancouver                        Vancouver
#> 23           Ducks d'Anaheim                          Anaheim
#> 24           Stars de Dallas                           Dallas
#> 25      Kings de Los Angeles                      Los Angeles
#> 26        Sharks de San Jose                         San Jose
#> 27  Blue Jackets de Columbus                         Columbus
#> 28         Wild du Minnesota                        Minnesota
#> 29          Jets de Winnipeg                         Winnipeg
#> 30   Golden Knights de Vegas                            Vegas
#> 31         Kraken de Seattle                          Seattle
#> 32         Mammoth de l'Utah                             Utah
#>    placeNameWithPreposition.fr placeName.default placeName.fr
#> 1                du New Jersey        New Jersey         <NA>
#> 2                  de New York      NY Islanders         <NA>
#> 3                  de New York        NY Rangers         <NA>
#> 4              de Philadelphie      Philadelphia Philadelphie
#> 5                de Pittsburgh        Pittsburgh         <NA>
#> 6                    de Boston            Boston         <NA>
#> 7                   de Buffalo           Buffalo         <NA>
#> 8                  de Montréal          Montréal         <NA>
#> 9                     d'Ottawa            Ottawa         <NA>
#> 10                  de Toronto           Toronto         <NA>
#> 11              de la Caroline          Carolina     Caroline
#> 12               de la Floride           Florida      Floride
#> 13                de Tampa Bay         Tampa Bay         <NA>
#> 14               de Washington        Washington         <NA>
#> 15                  de Chicago           Chicago         <NA>
#> 16                  de Detroit           Detroit         <NA>
#> 17                de Nashville         Nashville         <NA>
#> 18                de St. Louis         St. Louis         <NA>
#> 19                  de Calgary           Calgary         <NA>
#> 20                 du Colorado          Colorado         <NA>
#> 21                  d'Edmonton          Edmonton         <NA>
#> 22                de Vancouver         Vancouver         <NA>
#> 23                   d'Anaheim           Anaheim         <NA>
#> 24                   de Dallas            Dallas         <NA>
#> 25              de Los Angeles       Los Angeles         <NA>
#> 26                 de San Jose          San Jose         <NA>
#> 27                 de Columbus          Columbus         <NA>
#> 28                du Minnesota         Minnesota         <NA>
#> 29                 de Winnipeg          Winnipeg         <NA>
#> 30                    de Vegas             Vegas         <NA>
#> 31                  de Seattle           Seattle         <NA>
#> 32                   de l'Utah              Utah         <NA>
#> 
# }
```
