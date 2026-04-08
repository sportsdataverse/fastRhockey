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
#> [1] "2026-04-08T01:00:38Z"
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
#>                                                                                                                                                     homeTeam.odds
#> 1        MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 350, 195, -154, 110, 380, 200, , Draw, +1.5, O6.5, , 
#> 2    MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -1050, -130, 160, 160, -125, -148, , O6.5, , Draw, -1.5, 
#> 3      MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 150, 114, 150, -1300, 195, -115, Draw, -1.5, O6.5, , , 
#> 4   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 2000, -180, 2000, -154, 6500, 5000, Draw, +3.5, , O6.5, , 
#> 5  PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -125, 310, -810, -166, -265, -1300, -1.5, Draw, , O10.5, , 
#> 6      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 2000, 750, 100, -115, 650, 1500, , , +2.5, O6.5, Draw, 
#> 7      PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 280, -188, 115, -120, 150, -234, -1.5, , Draw, O7.5, , 
#> 8    PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -145, 330, -285, 120, -660, -1450, -1.5, Draw, , O7.5, , 
#> 9     OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -166, 350, 155, 170, -145, -142, O10.5, , Draw, , +1.5, 
#> 10     MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 400, 1200, -250, -110, 155, 220, , , +1.5, O4.5, Draw, 
#> 11   MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 1700, 1500, 5000, 105, -475, 3500, Draw, , , O4.5, +4.5, 
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
#>                                                                                                                                                       awayTeam.odds
#> 1        MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 100, 195, 120, -140, -580, -270, , Draw, -1.5, U6.5, , 
#> 2          MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 600, 100, 230, 160, -105, 115, , U6.5, , Draw, +1.5, 
#> 3        MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 150, -145, -195, 700, 205, -115, Draw, +1.5, U6.5, , , 
#> 4   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 2000, 140, -8500, 120, -3000, -2e+05, Draw, -3.5, , U6.5, , 
#> 5        PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -105, 310, 500, 130, 1000, 700, +1.5, Draw, , U10.5, , 
#> 6    MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -650, -1450, -130, -115, 650, -4500, , , -2.5, U6.5, Draw, 
#> 7         PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -395, 145, 115, -110, 380, 176, +1.5, , Draw, U7.5, , 
#> 8         PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 114, 330, 1100, -154, 420, 750, +1.5, Draw, , U7.5, , 
#> 9         OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 130, -520, 155, 225, 114, 110, U10.5, , Draw, , -1.5, 
#> 10      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 105, -2800, 190, -120, 155, -300, , , -1.5, U4.5, Draw, 
#> 11 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 1700, -4000, -2000, -135, 325, -80000, Draw, , , U4.5, -4.5, 
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
