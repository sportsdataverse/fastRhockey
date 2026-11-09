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
#> [1] "2026-11-08"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-09T03:00:38Z"
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
#>       gameId gameType         startTimeUTC homeTeam.id homeTeam.abbrev
#> 1 2026020277        2 2026-11-08T18:00:00Z           6             BOS
#> 2 2026020278        2 2026-11-08T20:30:00Z           7             BUF
#> 3 2026020279        2 2026-11-09T00:00:00Z          10             TOR
#> 4 2026020280        2 2026-11-08T23:00:00Z          52             WPG
#> 5 2026020281        2 2026-11-09T00:00:00Z          20             CGY
#> 6 2026020282        2 2026-11-09T01:00:00Z          24             ANA
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 5 https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#> 6 https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#>                                                                                                                                                       homeTeam.odds
#> 1  MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 2200, -175, -3500, 220, -15000, -4500, Draw, -3.5, , O6.5, , 
#> 2       MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 2200, -125, 114, 600, 8000, 1500, , +2.5, O4.5, Draw, , 
#> 3      MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 850, 35000, 185, 7000, -175, 1800, , , O4.5, Draw, +3.5, 
#> 4     MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 450, -175, -110, -140, -390, -2000, Draw, -1.5, O4.5, , , 
#> 5        MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -110, -185, 750, 390, 240, -110, , Draw, -1.5, , O7.5, 
#> 6 MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -4800, -154, -1600, -360, 1300, -80000, , -2.5, , O6.5, Draw, 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                Bruins          13             FLA
#> 2                Sabres          54             VGK
#> 3           Maple Leafs           1             NJD
#> 4                  Jets          14             TBL
#> 5                Flames           4             PHI
#> 6                 Ducks           5             PIT
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 5 https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 6 https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#>                                                                                                                                                    awayTeam.odds
#> 1   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 2200, 135, 9000, -298, 2500, 1500, Draw, +3.5, , U6.5, , 
#> 2 MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -600, -105, -145, 600, -1300, -4500, , -2.5, U4.5, Draw, , 
#> 3 MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -1750, -750, -245, 7000, 135, -7000, , , U4.5, Draw, -3.5, 
#> 4      MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 450, 135, -120, 110, 1300, 950, Draw, +1.5, U4.5, , , 
#> 5  MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -120, -185, -1450, 370, -330, -120, , Draw, +1.5, , U7.5, 
#> 6    MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 3000, 120, 5000, 260, 1300, 3500, , +2.5, , U6.5, Draw, 
#>   awayTeam.name.default
#> 1              Panthers
#> 2        Golden Knights
#> 3                Devils
#> 4             Lightning
#> 5                Flyers
#> 6              Penguins
#> 
# }
```
