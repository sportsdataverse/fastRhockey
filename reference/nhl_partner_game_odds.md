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
#> [1] "2026-06-14"
#> 
#> $lastUpdatedUTC
#> [1] "2026-06-15T02:30:38Z"
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
#> 1 2025030416        3 2026-06-15T00:00:00Z          54             VGK
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                                                                                                  homeTeam.odds
#> 1 PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, MONEY_LINE_3_WAY, OVER_UNDER, -110, 1000, 850, 2500, 3500, -140, +2.5, , Draw, , , O3.5
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1        Golden Knights          12             CAR
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#>                                                                                                                                                     awayTeam.odds
#> 1 PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, MONEY_LINE_3_WAY, OVER_UNDER, -120, -2100, 850, -20000, -1000, 110, -2.5, , Draw, , , U3.5
#>   awayTeam.name.default
#> 1            Hurricanes
#> 
# }
```
