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
#> [1] "2026-04-06"
#> 
#> $lastUpdatedUTC
#> [1] "2026-04-07T01:30:38Z"
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
#> 1 2025021229        2 2026-04-06T23:00:00Z           7             BUF
#> 2 2025021230        2 2026-04-06T23:30:00Z          52             WPG
#> 3 2025021231        2 2026-04-07T02:00:00Z          28             SJS
#> 4 2025021232        2 2026-04-07T02:30:00Z          26             LAK
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/LAK_light.svg
#>                                                                                                                                                     homeTeam.odds
#> 1     MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 235, -166, -540, -160, 110, -550, Draw, O6.5, , , -1.5, 
#> 2 MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -900, 154, -10000, 750, -140, -15000, , O7.5, , Draw, -2.5, 
#> 3    MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -220, -120, 285, 120, -1750, -925, , -1.5, Draw, O5.5, , 
#> 4      MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -110, 850, 340, -190, 250, -775, , -1.5, O5.5, , Draw, 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                Sabres          14             TBL
#> 2                  Jets          55             SEA
#> 3                Sharks          16             CHI
#> 4                 Kings          18             NSH
#>                                                         awayTeam.logo
#> 1                 https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 2                 https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/CHI_light.svg?season=20252026
#> 4                 https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#>                                                                                                                                                 awayTeam.odds
#> 1    MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 235, 130, 360, 550, -140, 360, Draw, U6.5, , , +1.5, 
#> 2 MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 3000, -200, 2200, 750, 110, 2200, , U7.5, , Draw, +2.5, 
#> 3   MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 800, -110, 285, -154, 850, 525, , +1.5, Draw, U5.5, , 
#> 4 MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -120, -1750, -500, 700, 250, 475, , +1.5, U5.5, , Draw, 
#>   awayTeam.name.default
#> 1             Lightning
#> 2                Kraken
#> 3            Blackhawks
#> 4             Predators
#> 
# }
```
