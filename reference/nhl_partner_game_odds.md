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
#> [1] "2026-11-03"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-04T00:30:38Z"
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
#> 1  2026020236        2 2026-11-04T00:00:00Z          10             TOR
#> 2  2026020237        2 2026-11-04T00:00:00Z           8             MTL
#> 3  2026020238        2 2026-11-04T00:00:00Z          14             TBL
#> 4  2026020239        2 2026-11-04T00:30:00Z           1             NJD
#> 5  2026020240        2 2026-11-04T00:00:00Z           3             NYR
#> 6  2026020241        2 2026-11-04T00:00:00Z           4             PHI
#> 7  2026020242        2 2026-11-04T00:00:00Z           5             PIT
#> 8  2026020243        2 2026-11-04T00:00:00Z          15             WSH
#> 9  2026020244        2 2026-11-04T00:00:00Z          29             CBJ
#> 10 2026020245        2 2026-11-04T01:00:00Z          18             NSH
#> 11 2026020246        2 2026-11-04T02:00:00Z          23             VAN
#>                                                    homeTeam.logo
#> 1            https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 2            https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 3            https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 4            https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 5            https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 6            https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 7            https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 9            https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 10           https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 11           https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#>                                                                                                                                                   homeTeam.odds
#> 1     PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -245, 100, 165, 285, -110, 100, +1.5, , , Draw, O5.5, 
#> 2   OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 114, -395, 380, -145, -225, -600, O6.5, , Draw, -1.5, , 
#> 3  MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 450, -135, -280, -105, -500, -725, Draw, O5.5, , -2.5, , 
#> 4                                   PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, 154, 105, 330, 114, -162, -1.5, , Draw, O6.5, 
#> 5     MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 145, 220, -120, 135, 275, -125, , -1.5, , O5.5, Draw, 
#> 6     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 124, 650, 550, 1100, -140, 800, +2.5, Draw, , , O6.5, 
#> 7    OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 114, 145, -298, -115, 300, -115, O5.5, , +1.5, , Draw, 
#> 8     MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 310, 180, -120, -140, 330, 230, Draw, , O5.5, +1.5, , 
#> 9   MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 300, -135, -115, 135, -180, -218, Draw, O7.5, , -1.5, , 
#> 10                                  OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, 105, 220, 340, -148, 164, O6.5, , Draw, +1.5, 
#> 11                                 OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, -142, 205, -162, 160, 350, O6.5, , +1.5, , Draw
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1            Maple Leafs          68             UTA
#> 2              Canadiens          52             WPG
#> 3              Lightning          17             DET
#> 4                 Devils          22             EDM
#> 5                Rangers          19             STL
#> 6                 Flyers          30             MIN
#> 7               Penguins          54             VGK
#> 8               Capitals          25             DAL
#> 9           Blue Jackets          21             COL
#> 10             Predators          12             CAR
#> 11               Canucks          24             ANA
#>                                          awayTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#>                                                                                                                                                   awayTeam.odds
#> 1    PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 185, -130, 130, 285, -120, -130, -1.5, , , Draw, U5.5, 
#> 2      OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -145, 280, 380, 114, 500, 390, U6.5, , Draw, +1.5, , 
#> 3      MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 450, 105, 600, -125, 340, 450, Draw, U5.5, , +2.5, , 
#> 4                                  PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, -185, 180, 330, -135, 136, +1.5, , Draw, U6.5, 
#> 5   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 155, -298, -110, -175, 275, -105, , +1.5, , U5.5, Draw, 
#> 6  PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -160, 650, -920, -500, 110, -1600, -2.5, Draw, , , U6.5, 
#> 7    OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -145, 145, 220, -115, 300, -115, U5.5, , -1.5, , Draw, 
#> 8   MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 310, -238, -110, 110, -145, -320, Draw, , U5.5, -1.5, , 
#> 9      MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 300, 105, 255, -175, 140, 165, Draw, U7.5, , +1.5, , 
#> 10                                OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, -125, -120, 340, 124, -198, U6.5, , Draw, -1.5, 
#> 11                                 OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 120, -110, 136, -192, 350, U6.5, , -1.5, , Draw
#>    awayTeam.name.default
#> 1                Mammoth
#> 2                   Jets
#> 3              Red Wings
#> 4                 Oilers
#> 5                  Blues
#> 6                   Wild
#> 7         Golden Knights
#> 8                  Stars
#> 9              Avalanche
#> 10            Hurricanes
#> 11                 Ducks
#> 
# }
```
