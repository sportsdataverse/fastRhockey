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
#> [1] "2026-11-04T04:00:38Z"
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
#>                                                                                                                                                       homeTeam.odds
#> 1       PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -125, 550, 3500, 320, -125, 2000, +1.5, , , Draw, O6.5, 
#> 2  OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -145, -130, 25000, 250, -3500, -1e+05, O4.5, , Draw, -4.5, , 
#> 3   MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 950, 154, -1100, -110, -1950, -20000, Draw, O6.5, , -1.5, , 
#> 4        PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 124, -155, 230, 110, -120, -525, -1.5, , Draw, O7.5, , 
#> 5    MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -2500, -145, -8500, 195, 1700, -675, , -2.5, , O3.5, Draw, 
#> 6       PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -110, 700, 1100, 2200, 105, 1600, +2.5, Draw, , , O6.5, 
#> 7          OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 340, 550, 700, -154, 235, 350, O5.5, , -1.5, , Draw, 
#> 8      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 550, 3000, -315, 110, 1700, 1300, Draw, , O10.5, +2.5, , 
#> 9    MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 3000, -188, -6000, -200, -160, -188, Draw, O9.5, , -3.5, , 
#> 10    OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -315, -125, 210, -110, -4800, -385, O4.5, , Draw, -2.5, , 
#> 11     OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -175, 2500, -115, 470, 650, 1800, O10.5, , +1.5, , Draw, 
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
#>                                                                                                                                                      awayTeam.odds
#> 1   PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -105, -920, -360, 320, -105, -10000, -1.5, , , Draw, U6.5, 
#> 2    OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 114, 100, 25000, -345, 60000, 4000, U4.5, , Draw, +4.5, , 
#> 3      MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 950, -200, 3500, -120, 950, 2500, Draw, U6.5, , +1.5, , 
#> 4       PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -160, 550, 230, -140, -110, 350, +1.5, , Draw, U7.5, , 
#> 5     MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 12000, 114, 2000, -260, 1700, 425, , +2.5, , U3.5, Draw, 
#> 6   PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -120, 700, -2500, -700, -135, -5000, -2.5, Draw, , , U6.5, 
#> 7     OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -500, -155, -1300, 120, 235, -525, U5.5, , +1.5, , Draw, 
#> 8  MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 550, -20000, 230, -140, -525, -3500, Draw, , U10.5, -2.5, , 
#> 9      MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 3000, 145, 13000, 154, 3500, 145, Draw, U9.5, , +3.5, , 
#> 10       OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 230, 450, 210, -120, 1600, 270, U4.5, , Draw, +2.5, , 
#> 11   OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 135, -700, -115, -750, 650, -7000, U10.5, , -1.5, , Draw, 
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
