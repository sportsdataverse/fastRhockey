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
#> [1] "2026-11-13"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-13T20:00:38Z"
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
#> 1 2026020310        2 2026-11-14T02:00:00Z          23             VAN
#> 2 2026020311        2 2026-11-14T03:00:00Z          54             VGK
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                                                                  homeTeam.odds
#> 1 PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, -135, 185, -105, 245, 340, +1.5, , O6.5, , Draw
#> 2  MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, -162, 150, 320, 100, 114, , -1.5, Draw, , O6.5
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1               Canucks           3             NYR
#> 2        Golden Knights          18             NSH
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#>                                                                                                                   awayTeam.odds
#> 1 PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, 114, -225, -115, -130, 340, -1.5, , U6.5, , Draw
#> 2  MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 136, -180, 320, 190, -135, , +1.5, Draw, , U6.5
#>   awayTeam.name.default
#> 1               Rangers
#> 2             Predators
#> 
# }
```
