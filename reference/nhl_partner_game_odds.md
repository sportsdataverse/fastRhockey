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
#> [1] "2026-10-11"
#> 
#> $lastUpdatedUTC
#> [1] "2026-10-12T01:00:38Z"
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
#> 1 2026020084        2 2026-10-11T21:00:00Z          15             WSH
#> 2 2026020085        2 2026-10-11T22:00:00Z           3             NYR
#> 3 2026020086        2 2026-10-11T23:00:00Z           4             PHI
#>                                                   homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 2           https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 3           https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#>                                                                                                                                                     homeTeam.odds
#> 1      MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -205, 360, 300, -145, 750, -154, Draw, , O7.5, , -1.5, 
#> 2 OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 470, -4000, 1000, -1200, 650, -80000, O8.5, , Draw, , -4.5, 
#> 3     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 145, 5000, 950, 4000, -360, 3000, +2.5, , Draw, , O4.5, 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1              Capitals          55             SEA
#> 2               Rangers          23             VAN
#> 3                Flyers          12             CAR
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#>                                                                                                                                                      awayTeam.odds
#> 1      MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -205, 475, -425, 114, -1450, 120, Draw, , U7.5, , +1.5, 
#> 2   OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -750, 1500, 1000, 5000, -1150, 3500, U8.5, , Draw, , +4.5, 
#> 3 PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -188, -1200, 950, -1e+05, 260, -50000, -2.5, , Draw, , U4.5, 
#>   awayTeam.name.default
#> 1                Kraken
#> 2               Canucks
#> 3            Hurricanes
#> 
# }
```
