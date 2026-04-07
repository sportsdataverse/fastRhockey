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
#> [1] "2026-04-07"
#> 
#> $lastUpdatedUTC
#> [1] "2026-04-07T16:00:38Z"
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
#> 1  2025021233        2 2026-04-07T23:00:00Z           8             MTL
#> 2  2025021234        2 2026-04-07T23:00:00Z           9             OTT
#> 3  2025021235        2 2026-04-07T23:00:00Z          17             DET
#> 4  2025021236        2 2026-04-07T23:00:00Z           1             NJD
#> 5  2025021237        2 2026-04-07T23:00:00Z          12             CAR
#> 6  2025021238        2 2026-04-08T00:00:00Z          19             STL
#> 7  2025021239        2 2026-04-08T00:00:00Z          25             DAL
#> 8  2025021240        2 2026-04-08T00:00:00Z          30             MIN
#> 9  2025021241        2 2026-04-08T01:30:00Z          68             UTA
#> 10 2025021242        2 2026-04-08T02:00:00Z          23             VAN
#> 11 2025021243        2 2026-04-08T02:00:00Z          24             ANA
#>                                                          homeTeam.logo
#> 1                  https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 2                  https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/DET_light.svg?season=20252026
#> 4                  https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 5                  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/STL_light.svg?season=20252026
#> 7                  https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 8                  https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/UTA_light.svg?season=20252026
#> 10                 https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 11                 https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#>                                                                                                                                                  homeTeam.odds
#> 1  MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -135, 340, 114, -115, -218, -260, , Draw, -1.5, O6.5, , 
#> 2   MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -130, -105, 120, 310, 185, -140, , O6.5, , Draw, -1.5, 
#> 3    MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 300, 205, 114, -115, 140, -115, Draw, -1.5, O6.5, , , 
#> 4   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 300, 200, -125, -135, 130, -120, Draw, -1.5, , O5.5, , 
#> 5   PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 140, 330, -185, 105, -110, -200, -1.5, Draw, , O6.5, , 
#> 6     MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 180, 130, -198, 110, 300, 120, , , +1.5, O6.5, Draw, 
#> 7  PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 110, -245, 330, -130, -140, -270, -1.5, , Draw, O5.5, , 
#> 8   PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 100, 350, -150, 114, -250, -295, -1.5, Draw, , O6.5, , 
#> 9   OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -110, -125, 310, 125, 190, -130, O6.5, , Draw, , -1.5, 
#> 10   MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 285, 210, -115, -105, 380, 215, , , +1.5, O6.5, Draw, 
#> 11  MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 320, -142, 110, -118, 170, -155, Draw, , , O6.5, -1.5, 
#>    homeTeam.name.default homeTeam.name.fr awayTeam.id awayTeam.abbrev
#> 1              Canadiens             <NA>          13             FLA
#> 2               Senators        Sénateurs          14             TBL
#> 3              Red Wings             <NA>          29             CBJ
#> 4                 Devils             <NA>           4             PHI
#> 5             Hurricanes             <NA>           6             BOS
#> 6                  Blues             <NA>          21             COL
#> 7                  Stars             <NA>          20             CGY
#> 8                   Wild             <NA>          55             SEA
#> 9                Mammoth             <NA>          22             EDM
#> 10               Canucks             <NA>          54             VGK
#> 11                 Ducks             <NA>          18             NSH
#>                                                          awayTeam.logo
#> 1                  https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 2                  https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 3                  https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 4                  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/BOS_light.svg?season=20252026
#> 6                  https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 7                  https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#> 8                  https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#> 9                  https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 10                 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 11                 https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#>                                                                                                                                                   awayTeam.odds
#> 1     MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 260, 340, -135, -105, 180, 190, , Draw, +1.5, U6.5, , 
#> 2     MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 110, -115, 160, 310, -225, 105, , U6.5, , Draw, +1.5, 
#> 3   MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 300, -250, -135, -105, 140, -115, Draw, +1.5, U6.5, , , 
#> 4     MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 300, -245, 105, 114, 150, -110, Draw, +1.5, , U5.5, , 
#> 5     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -166, 330, 154, -125, 210, 150, +1.5, Draw, , U6.5, , 
#> 6    MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 110, -155, 164, -130, 300, -160, , , -1.5, U6.5, Draw, 
#> 7      PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -130, 200, 330, 110, 270, 200, +1.5, , Draw, U5.5, , 
#> 8     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -120, 350, 285, -135, 205, 210, +1.5, Draw, , U6.5, , 
#> 9     OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -110, 105, 310, 155, -230, 100, U6.5, , Draw, , +1.5, 
#> 10 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -160, -258, -105, -115, 380, -300, , , -1.5, U6.5, Draw, 
#> 11    MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 320, 120, 175, -102, -205, 115, Draw, , , U6.5, +1.5, 
#>    awayTeam.name.default
#> 1               Panthers
#> 2              Lightning
#> 3           Blue Jackets
#> 4                 Flyers
#> 5                 Bruins
#> 6              Avalanche
#> 7                 Flames
#> 8                 Kraken
#> 9                 Oilers
#> 10        Golden Knights
#> 11             Predators
#> 
# }
```
