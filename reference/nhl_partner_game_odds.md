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
#> [1] "2026-09-25"
#> 
#> $lastUpdatedUTC
#> [1] "2026-09-26T00:30:38Z"
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
#> 1 2026010048        1 2026-09-26T00:30:00Z          21             COL
#> 2 2026010049        1 2026-09-25T23:00:00Z          15             WSH
#> 3 2026010050        1 2026-09-26T00:00:00Z          30             MIN
#> 4 2026010051        1 2026-09-25T23:30:00Z           2             NYI
#>                                                   homeTeam.logo
#> 1           https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 3           https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 4           https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#>                                                                                                                   homeTeam.odds
#> 1 MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, -580, -135, 124, -120, 320, , -1.5, O7.5, , Draw
#> 2 OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, -160, 310, -125, -130, 105, O3.5, Draw, -2.5, , 
#> 3  OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, 124, 115, 3500, -175, 310, O5.5, , , +1.5, Draw
#> 4  MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 700, -105, 260, 340, -166, , +5.5, , Draw, O6.5
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1             Avalanche          52             WPG
#> 2              Capitals           6             BOS
#> 3                  Wild          25             DAL
#> 4             Islanders           3             NYR
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#>                                                                                                                    awayTeam.odds
#> 1    MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, 380, 105, -160, 230, 320, , +1.5, U7.5, , Draw
#> 2    OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 124, 310, -105, 100, 180, U3.5, Draw, +2.5, , 
#> 3 OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, -160, 165, -50000, 135, 310, U5.5, , , -1.5, Draw
#> 4 MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, -1300, -125, -140, 340, 130, , -5.5, , Draw, U6.5
#>   awayTeam.name.default
#> 1                  Jets
#> 2                Bruins
#> 3                 Stars
#> 4               Rangers
#> 
# }
```
