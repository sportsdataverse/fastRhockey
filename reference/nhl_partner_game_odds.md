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
#> [1] "2026-11-05"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-06T05:00:38Z"
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
#> 1  2026020251        2 2026-11-06T00:00:00Z           8             MTL
#> 2  2026020252        2 2026-11-06T00:00:00Z          17             DET
#> 3  2026020253        2 2026-11-06T00:00:00Z          14             TBL
#> 4  2026020254        2 2026-11-06T00:00:00Z           1             NJD
#> 5  2026020255        2 2026-11-06T00:00:00Z           2             NYI
#> 6  2026020256        2 2026-11-06T00:00:00Z           3             NYR
#> 7  2026020257        2 2026-11-06T00:00:00Z           5             PIT
#> 8  2026020258        2 2026-11-06T00:00:00Z          29             CBJ
#> 9  2026020259        2 2026-11-06T00:00:00Z          52             WPG
#> 10 2026020260        2 2026-11-06T01:00:00Z          22             EDM
#> 11 2026020261        2 2026-11-06T02:30:00Z          21             COL
#>                                          homeTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#>                                                                                                                                                        homeTeam.odds
#> 1         PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -145, -210, 325, 245, 1200, 800, +1.5, O8.5, , Draw, , 
#> 2         MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -105, -1600, 325, 140, 225, 105, , +1.5, O3.5, Draw, , 
#> 3  MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 40000, -1600, -188, -6500, 140, -80000, Draw, , O4.5, , -4.5, 
#> 4         MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -270, -175, 750, 390, 390, -212, Draw, , -1.5, O9.5, , 
#> 5         MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -220, 110, 500, 300, -1300, 120, Draw, , , O7.5, +1.5, 
#> 6         MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 750, -3200, -200, 210, 220, 475, , , +1.5, Draw, O5.5, 
#> 7      OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -200, -175, 2000, 7000, 1800, 5000, O8.5, +3.5, , , Draw, 
#> 8        OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -188, 370, 650, -180, 5000, 3000, O1.5, Draw, , +1.5, , 
#> 9    MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -1300, 190, -10000, 8000, 450, -3000, , O8.5, , Draw, -4.5, 
#> 10      PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -360, 420, -140, 50000, 9000, 800, +3.5, , O7.5, , Draw, 
#> 11        PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 800, 400, -154, -255, 390, -168, -1.5, , , Draw, O5.5, 
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1              Canadiens          68             UTA
#> 2              Red Wings          54             VGK
#> 3              Lightning          16             CHI
#> 4                 Devils          19             STL
#> 5              Islanders          12             CAR
#> 6                Rangers           4             PHI
#> 7               Penguins           6             BOS
#> 8           Blue Jackets          25             DAL
#> 9                   Jets          23             VAN
#> 10                Oilers          24             ANA
#> 11             Avalanche          18             NSH
#>                                          awayTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#>                                                                                                                                                      awayTeam.odds
#> 1      PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 114, 160, -475, 245, -225, -1600, -1.5, U8.5, , Draw, , 
#> 2       MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -125, 800, -475, 140, 185, -135, , -1.5, U3.5, Draw, , 
#> 3    MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 40000, 5000, 145, 1800, -180, 3500, Draw, , U4.5, , +4.5, 
#> 4      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -270, 135, -1450, -600, 700, 160, Draw, , +1.5, U9.5, , 
#> 5      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -220, -140, 370, -425, 700, -154, Draw, , , U7.5, -1.5, 
#> 6      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -165, 1300, 154, 210, -298, -775, , , -1.5, Draw, U5.5, 
#> 7  OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 154, 135, -8500, -2500, 1800, -2e+05, U8.5, -3.5, , , Draw, 
#> 8    OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 145, 370, -1150, 140, -425, -50000, U1.5, Draw, , -1.5, , 
#> 9    MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 700, -250, 35000, 8000, -720, 1200, , U8.5, , Draw, +4.5, 
#> 10    PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 260, -660, 110, -360, 9000, -1600, -3.5, , U7.5, , Draw, 
#> 11     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -1600, 600, 120, -255, -600, 130, +1.5, , , Draw, U5.5, 
#>    awayTeam.name.default
#> 1                Mammoth
#> 2         Golden Knights
#> 3             Blackhawks
#> 4                  Blues
#> 5             Hurricanes
#> 6                 Flyers
#> 7                 Bruins
#> 8                  Stars
#> 9                Canucks
#> 10                 Ducks
#> 11             Predators
#> 
# }
```
