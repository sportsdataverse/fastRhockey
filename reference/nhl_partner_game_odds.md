# **NHL Partner Game Odds**

Returns partner game odds data for a country code.

## Usage

``` r
nhl_partner_game_odds(country_code = "US")
```

## Arguments

- country_code:

  Two-letter country code (e.g., "US", "CA"). Default "US".

## Value

Returns a list with game odds data.

## Examples

``` r
# \donttest{
try(nhl_partner_game_odds())
#> $currentOddsDate
#> [1] "2026-04-13"
#> 
#> $lastUpdatedUTC
#> [1] "2026-04-13T17:00:39Z"
#> 
#> $bettingPartner
#> $bettingPartner$partnerId
#> [1] 9
#> 
#> $bettingPartner$country
#> [1] "USA"
#> 
#> $bettingPartner$name
#> [1] "DraftKings"
#> 
#> $bettingPartner$imageUrl
#> [1] "https://assets.nhle.com/betting_partner/draftkings.svg"
#> 
#> $bettingPartner$siteUrl
#> [1] "https://dksb.sng.link/As9kz/3i4d?_dl=https%3A%2F%2Fsportsbook.draftkings.com%2Fgateway%3Fs%3D333653091&pcid=427326&psn=1320&pcn=NHL&pscn=OddsWidget&pcrn=NoOffer&pscid=SP&wpcid=427326&wpsrc=1320&wpcn=NHL&wpscn=OddsWidget&wpcrn=NoOffer&wpscid=SP&_forward_params=1"
#> 
#> $bettingPartner$bgColor
#> [1] "#000000"
#> 
#> $bettingPartner$textColor
#> [1] "#FFFFFF"
#> 
#> $bettingPartner$accentColor
#> [1] "#FFFFFF"
#> 
#> 
#> $games
#>        gameId gameType         startTimeUTC homeTeam.id homeTeam.abbrev
#> 1  2025021282        2 2026-04-13T23:00:00Z          14             TBL
#> 2  2025021283        2 2026-04-13T23:00:00Z          13             FLA
#> 3  2025021284        2 2026-04-13T23:00:00Z           4             PHI
#> 4  2025021285        2 2026-04-13T23:30:00Z          10             TOR
#> 5  2025021286        2 2026-04-14T00:00:00Z          19             STL
#> 6  2025021287        2 2026-04-14T00:00:00Z          18             NSH
#> 7  2025021288        2 2026-04-14T00:30:00Z          16             CHI
#> 8  2025021289        2 2026-04-14T01:30:00Z          22             EDM
#> 9  2025021290        2 2026-04-14T02:00:00Z          54             VGK
#> 10 2025021291        2 2026-04-14T01:30:00Z          55             SEA
#>                                                          homeTeam.logo
#> 1                  https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 2                  https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 3                  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 4                  https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/STL_light.svg?season=20252026
#> 6                  https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/CHI_light.svg?season=20252026
#> 8                  https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 9                  https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 10                 https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#>                                                                                                                    homeTeam.odds
#> 1   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 330, -125, 105, 124, -198, Draw, , O6.5, -1.5, 
#> 2    MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 310, -205, 124, 175, 110, Draw, +1.5, , , O6.5
#> 3   OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, -135, 110, 170, -155, 310, O5.5, , -1.5, , Draw
#> 4   OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, -105, 215, 340, 160, -155, O6.5, , Draw, , +1.5
#> 5   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, 100, 154, 310, -112, -170, , -1.5, Draw, O5.5, 
#> 6   MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 320, -112, 164, -155, 105, Draw, O6.5, -1.5, , 
#> 7   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, 340, 230, 170, -148, -102, Draw, , , +1.5, O6.5
#> 8   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 145, 330, -120, -250, 100, , Draw, O6.5, +1.5, 
#> 9  MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, -105, 330, 142, -130, -185, , Draw, -1.5, O5.5, 
#> 10  MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, 195, -125, 130, -198, 310, , O5.5, , +1.5, Draw
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1              Lightning          17             DET
#> 2               Panthers           3             NYR
#> 3                 Flyers          12             CAR
#> 4            Maple Leafs          25             DAL
#> 5                  Blues          30             MIN
#> 6              Predators          28             SJS
#> 7             Blackhawks           7             BUF
#> 8                 Oilers          21             COL
#> 9         Golden Knights          52             WPG
#> 10                Kraken          26             LAK
#>                                                          awayTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/DET_light.svg?season=20252026
#> 2                  https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 3                  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 4                  https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 5                  https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 6                  https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#> 7                  https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 8                  https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 9                  https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 10                 https://assets.nhle.com/logos/nhl/svg/LAK_light.svg
#>                                                                                                                    awayTeam.odds
#> 1   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 330, 240, -125, -148, 164, Draw, , U6.5, +1.5, 
#> 2   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 310, 170, -148, 110, -130, Draw, -1.5, , , U6.5
#> 3    OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 114, 180, -205, 130, 310, U5.5, , +1.5, , Draw
#> 4  OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, -115, -115, 340, -192, 130, U6.5, , Draw, , -1.5
#> 5   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, 195, -185, 310, -108, 142, , +1.5, Draw, U5.5, 
#> 6   MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 320, -108, -198, 130, 180, Draw, U6.5, +1.5, , 
#> 7  MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, 340, -125, -205, 124, -118, Draw, , , -1.5, U6.5
#> 8    MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 130, 330, 100, 205, -120, , Draw, U6.5, -1.5, 
#> 9    MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, 200, 330, -170, 110, 154, , Draw, +1.5, U5.5, 
#> 10   MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, 100, 105, -155, 164, 310, , U5.5, , -1.5, Draw
#>    awayTeam.name.default
#> 1              Red Wings
#> 2                Rangers
#> 3             Hurricanes
#> 4                  Stars
#> 5                   Wild
#> 6                 Sharks
#> 7                 Sabres
#> 8              Avalanche
#> 9                   Jets
#> 10                 Kings
#> 
# }
```
