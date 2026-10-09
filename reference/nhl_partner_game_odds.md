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
#> [1] "2026-10-08"
#> 
#> $lastUpdatedUTC
#> [1] "2026-10-09T04:30:38Z"
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
#> 1  2026020056        2 2026-10-08T23:00:00Z           6             BOS
#> 2  2026020057        2 2026-10-08T23:00:00Z           7             BUF
#> 3  2026020058        2 2026-10-08T23:00:00Z           8             MTL
#> 4  2026020059        2 2026-10-08T23:00:00Z           9             OTT
#> 5  2026020060        2 2026-10-08T23:00:00Z          14             TBL
#> 6  2026020061        2 2026-10-08T23:00:00Z          12             CAR
#> 7  2026020062        2 2026-10-08T23:30:00Z           2             NYI
#> 8  2026020063        2 2026-10-09T00:00:00Z          19             STL
#> 9  2026020064        2 2026-10-09T01:00:00Z          20             CGY
#> 10 2026020065        2 2026-10-09T02:00:00Z          54             VGK
#>                                          homeTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                                                                                                         homeTeam.odds
#> 1    MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 35000, -1150, -700, -140, -345, -2500, Draw, , , O7.5, -4.5, 
#> 2        MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 5500, 470, 180, 4000, 20000, 3000, Draw, +3.5, O4.5, , , 
#> 3          MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -235, 390, 360, 850, -140, -154, Draw, , O5.5, -1.5, , 
#> 4      PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -105, 105, -1050, -390, 350, -15000, -1.5, O4.5, , , Draw, 
#> 5        OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 135, -2800, -220, -110, 275, -975, O7.5, , , -1.5, Draw, 
#> 6  OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -105, 30000, 114, -3000, -15000, -2e+05, O8.5, Draw, -5.5, , , 
#> 7   PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 100, 2800, -5000, -188, -50000, -2e+05, -3.5, Draw, , O4.5, , 
#> 8          OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 310, 380, -215, 700, -130, -142, O5.5, , Draw, -1.5, , 
#> 9     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -298, 50000, 3000, -130, 60000, 165, +4.5, Draw, , O10.5, , 
#> 10         PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 650, 340, -145, -210, 290, -168, -1.5, , , Draw, O7.5, 
#>    homeTeam.name.default homeTeam.name.fr awayTeam.id awayTeam.abbrev
#> 1                 Bruins             <NA>          68             UTA
#> 2                 Sabres             <NA>          25             DAL
#> 3              Canadiens             <NA>          18             NSH
#> 4               Senators        Sénateurs           4             PHI
#> 5              Lightning             <NA>          30             MIN
#> 6             Hurricanes             <NA>          23             VAN
#> 7              Islanders             <NA>          16             CHI
#> 8                  Blues             <NA>          28             SJS
#> 9                 Flames             <NA>          21             COL
#> 10        Golden Knights             <NA>          10             TOR
#>                                          awayTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#>                                                                                                                                                         awayTeam.odds
#> 1        MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 35000, 650, 25000, 110, 250, 1100, Draw, , , U7.5, +4.5, 
#> 2  MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 5500, -750, -238, -4000, -20000, -50000, Draw, -3.5, U4.5, , , 
#> 3         MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -235, 550, -540, -1750, 110, 120, Draw, , U5.5, +1.5, , 
#> 4         PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -125, -135, 600, 3500, 350, 2200, +1.5, U4.5, , , Draw, 
#> 5          OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -175, 1200, 850, -120, 275, 550, U7.5, , , +1.5, Draw, 
#> 6     OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -125, 30000, -145, 30000, 2500, 5000, U8.5, Draw, +5.5, , , 
#> 7       PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -130, 2800, 12000, 145, 3500, 5000, +3.5, Draw, , U4.5, , 
#> 8         OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -445, 500, -215, -1300, 100, 110, U5.5, , Draw, +1.5, , 
#> 9    PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 220, 50000, -20000, 100, -5000, -218, -4.5, Draw, , U10.5, , 
#> 10        PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -1150, 500, 114, -210, -410, 130, +1.5, , , Draw, U7.5, 
#>    awayTeam.name.default
#> 1                Mammoth
#> 2                  Stars
#> 3              Predators
#> 4                 Flyers
#> 5                   Wild
#> 6                Canucks
#> 7             Blackhawks
#> 8                 Sharks
#> 9              Avalanche
#> 10           Maple Leafs
#> 
# }
```
