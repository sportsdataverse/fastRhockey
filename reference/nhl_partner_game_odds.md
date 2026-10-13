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
#> [1] "2026-10-12"
#> 
#> $lastUpdatedUTC
#> [1] "2026-10-13T01:30:38Z"
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
#> 1 2026020087        2 2026-10-12T17:00:00Z           7             BUF
#> 2 2026020088        2 2026-10-12T23:00:00Z           1             NJD
#> 3 2026020089        2 2026-10-13T00:00:00Z          30             MIN
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#>                                                                                                                                                  homeTeam.odds
#> 1   PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 185, 195, 140, -120, -120, -120, -2.5, , Draw, , O8.5, 
#> 2  PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -115, 2500, 2200, 230, 600, 1500, +2.5, , , O5.5, Draw, 
#> 3 PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 110, -920, -450, -160, 500, -2500, -2.5, , , O6.5, Draw, 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                Sabres          13             FLA
#> 2                Devils           9             OTT
#> 3                  Wild          54             VGK
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                                                                                                     awayTeam.odds
#> 1     PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -245, 215, 140, -110, -110, -110, +2.5, , Draw, , U8.5, 
#> 2 PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -115, -15000, -650, -315, 600, -4500, -2.5, , , U5.5, Draw, 
#> 3      PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -140, 550, 1400, 124, 500, 1100, +2.5, , , U6.5, Draw, 
#>   awayTeam.name.default awayTeam.name.fr
#> 1              Panthers             <NA>
#> 2              Senators        Sénateurs
#> 3        Golden Knights             <NA>
#> 
# }
```
