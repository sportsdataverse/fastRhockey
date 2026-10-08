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
#> [1] "2026-10-07"
#> 
#> $lastUpdatedUTC
#> [1] "2026-10-08T04:00:38Z"
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
#> 1 2026020053        2 2026-10-07T23:30:00Z          15             WSH
#> 2 2026020054        2 2026-10-07T23:30:00Z          52             WPG
#> 3 2026020055        2 2026-10-08T02:00:00Z          24             ANA
#>                                                   homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 2           https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 3           https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#>                                                                                                                                                     homeTeam.odds
#> 1 MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -1400, 100, 1000, -4000, 124, -2e+05, , -2.5, Draw, , O9.5, 
#> 2     MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -155, 180, -298, -175, 180, -925, , Draw, , O4.5, -1.5, 
#> 3   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 4000, 19000, -180, 154, 1800, 4000, Draw, , +3.5, O8.5, , 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1              Capitals           5             PIT
#> 2                  Jets          21             COL
#> 3                 Ducks          22             EDM
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#>                                                                                                                                                       awayTeam.odds
#> 1     MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 7500, -130, 1000, 1500, -160, 5000, , +2.5, Draw, , U9.5, 
#> 2          MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 900, 180, 220, 135, -238, 525, , Draw, , U4.5, +1.5, 
#> 3 MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 4000, -10000, 140, -200, -6500, -1e+05, Draw, , -3.5, U8.5, , 
#>   awayTeam.name.default
#> 1              Penguins
#> 2             Avalanche
#> 3                Oilers
#> 
# }
```
