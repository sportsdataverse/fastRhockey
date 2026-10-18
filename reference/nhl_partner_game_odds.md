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
#> [1] "2026-10-17"
#> 
#> $lastUpdatedUTC
#> [1] "2026-10-18T03:00:38Z"
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
#> 1  2026020121        2 2026-10-17T17:00:00Z          15             WSH
#> 2  2026020122        2 2026-10-17T19:30:00Z          52             WPG
#> 3  2026020123        2 2026-10-17T20:00:00Z          14             TBL
#> 4  2026020124        2 2026-10-17T22:00:00Z          13             FLA
#> 5  2026020125        2 2026-10-17T23:00:00Z          10             TOR
#> 6  2026020126        2 2026-10-17T23:00:00Z           8             MTL
#> 7  2026020127        2 2026-10-17T23:00:00Z          17             DET
#> 8  2026020128        2 2026-10-17T23:00:00Z           4             PHI
#> 9  2026020129        2 2026-10-17T23:00:00Z          29             CBJ
#> 10 2026020130        2 2026-10-17T23:00:00Z          16             CHI
#> 11 2026020131        2 2026-10-18T00:00:00Z          18             NSH
#> 12 2026020132        2 2026-10-18T01:00:00Z          26             LAK
#> 13 2026020133        2 2026-10-18T02:00:00Z          68             UTA
#> 14 2026020134        2 2026-10-18T02:00:00Z          54             VGK
#>                                                    homeTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 2            https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 3            https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 4            https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 5            https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 6            https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 7            https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 8            https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 9            https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 10           https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#> 11           https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 12           https://assets.nhle.com/logos/nhl/svg/LAK_light.svg
#> 13           https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 14           https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                                                                                                         homeTeam.odds
#> 1   MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -15000, 2200, -3500, -145, 135, -80000, , Draw, , O6.5, -3.5, 
#> 2          PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 100, 330, 500, -215, 1800, 1300, +1.5, Draw, , O6.5, , 
#> 3        MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -110, -2100, 190, 135, -125, -330, , , Draw, O6.5, -1.5, 
#> 4    MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -1150, 140, 350, -20000, 7000, -2500, , O11.5, -5.5, , Draw, 
#> 5        PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -140, 1400, -195, 3500, 450, 1000, +2.5, , O4.5, , Draw, 
#> 6          PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 650, 175, 320, -145, -120, -115, -1.5, O7.5, , Draw, , 
#> 7         MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 280, 450, -105, -160, 2800, 1800, Draw, , O6.5, +1.5, , 
#> 8        MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 235, 140, -130, -500, -230, -2000, Draw, -1.5, O4.5, , , 
#> 9          MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 370, -175, 210, -115, 750, -110, , Draw, O1.5, , -1.5, 
#> 10       OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 165, 4500, 1100, -540, 1400, 3000, O4.5, , Draw, +4.5, , 
#> 11         OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 145, 230, -160, 130, -330, -600, O7.5, Draw, , -1.5, , 
#> 12         MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -120, 550, -130, 280, 160, -130, , -1.5, Draw, , O3.5, 
#> 13     MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 30000, 4000, -154, 20000, 230, 4000, , , O8.5, Draw, +3.5, 
#> 14 MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -1600, -250, 10000, -500, -3500, -20000, , -3.5, Draw, O7.5, , 
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1               Capitals           1             NJD
#> 2                   Jets          12             CAR
#> 3              Lightning          23             VAN
#> 4               Panthers          55             SEA
#> 5            Maple Leafs           2             NYI
#> 6              Canadiens           7             BUF
#> 7              Red Wings          28             SJS
#> 8                 Flyers           9             OTT
#> 9           Blue Jackets           3             NYR
#> 10            Blackhawks          25             DAL
#> 11             Predators          19             STL
#> 12                 Kings           6             BOS
#> 13               Mammoth          22             EDM
#> 14        Golden Knights           5             PIT
#>                                          awayTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 12 https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 13 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 14 https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#>                                                                                                                                                        awayTeam.odds
#> 1      MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 2500, 2200, 10000, 114, -175, 3500, , Draw, , U6.5, +3.5, 
#> 2       PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -130, 330, -810, 165, -330, -3500, -1.5, Draw, , U6.5, , 
#> 3         MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 400, 1000, 190, -175, -105, 240, , , Draw, U6.5, +1.5, 
#> 4     MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 650, -180, -520, 25000, 7000, 1100, , U11.5, +5.5, , Draw, 
#> 5      PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 110, -400, 150, -50000, 450, -2200, -2.5, , U4.5, , Draw, 
#> 6      PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -1150, -230, 320, -145, -110, -115, +1.5, U7.5, , Draw, , 
#> 7       MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 280, -720, -125, 124, -300, -7000, Draw, , U6.5, -1.5, , 
#> 8          MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 235, -180, 100, 340, 1500, 950, Draw, +1.5, U4.5, , , 
#> 9      MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 350, -175, -280, -115, -1450, -120, , Draw, U1.5, , +1.5, 
#> 10  OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -215, -1400, 1100, 360, -3500, -50000, U4.5, , Draw, -4.5, , 
#> 11         OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -188, 230, 600, -166, 240, 390, U7.5, Draw, , +1.5, , 
#> 12       MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -110, -920, -130, 320, -210, 100, , +1.5, Draw, , U3.5, 
#> 13 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -3000, -6500, 120, 20000, -315, -1e+05, , , U8.5, Draw, -3.5, 
#> 14     MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 45000, 190, 10000, 340, 7000, 2500, , +3.5, Draw, U7.5, , 
#>    awayTeam.name.default awayTeam.name.fr
#> 1                 Devils             <NA>
#> 2             Hurricanes             <NA>
#> 3                Canucks             <NA>
#> 4                 Kraken             <NA>
#> 5              Islanders             <NA>
#> 6                 Sabres             <NA>
#> 7                 Sharks             <NA>
#> 8               Senators        Sénateurs
#> 9                Rangers             <NA>
#> 10                 Stars             <NA>
#> 11                 Blues             <NA>
#> 12                Bruins             <NA>
#> 13                Oilers             <NA>
#> 14              Penguins             <NA>
#> 
# }
```
