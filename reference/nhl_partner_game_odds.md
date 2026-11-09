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
#> [1] "2026-11-09"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-09T22:30:38Z"
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
#> 1 2026020283        2 2026-11-10T01:00:00Z          22             EDM
#> 2 2026020284        2 2026-11-10T02:30:00Z          23             VAN
#> 3 2026020285        2 2026-11-10T03:00:00Z          28             SJS
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#>                                                                                                                  homeTeam.odds
#> 1 MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 330, 154, -162, 100, -112, Draw, -1.5, , , O6.5
#> 2  OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, 110, -218, 165, 310, 114, O6.5, +1.5, , Draw, 
#> 3 PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, -205, 120, 310, 170, -102, +1.5, , Draw, , O6.5
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                Oilers           3             NYR
#> 2               Canucks           4             PHI
#> 3                Sharks           2             NYI
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#>                                                                                                                  awayTeam.odds
#> 1 MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 330, -185, 136, 190, -108, Draw, +1.5, , , U6.5
#> 2 OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, -130, 180, 115, 310, -135, U6.5, -1.5, , Draw, 
#> 3 PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 170, -142, 310, 115, -118, -1.5, , Draw, , U6.5
#>   awayTeam.name.default
#> 1               Rangers
#> 2                Flyers
#> 3             Islanders
#> 
# }
```
