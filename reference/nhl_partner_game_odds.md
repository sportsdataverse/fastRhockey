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
#> [1] "2026-11-02"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-03T04:00:38Z"
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
#> 1 2026020232        2 2026-11-03T00:00:00Z           6             BOS
#> 2 2026020233        2 2026-11-03T00:00:00Z          13             FLA
#> 3 2026020234        2 2026-11-03T01:00:00Z          16             CHI
#> 4 2026020235        2 2026-11-03T02:00:00Z          28             SJS
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#>                                                                                                                                                  homeTeam.odds
#> 1   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, -165, 225, 124, -115, 130, -625, , Draw, O5.5, , -1.5, 
#> 2  PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -160, 600, 550, -154, 2000, 1400, +2.5, , Draw, O7.5, , 
#> 3 MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 7000, 1100, 114, -120, 1600, 5000, , Draw, +2.5, O7.5, , 
#> 4     OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 105, 220, 550, -140, 470, 350, O4.5, Draw, , +1.5, , 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                Bruins          54             VGK
#> 2              Panthers          17             DET
#> 3            Blackhawks          21             COL
#> 4                Sharks          20             CGY
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#>                                                                                                                                                       awayTeam.odds
#> 1        MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 600, 225, -160, -115, -166, 400, , Draw, U5.5, , +1.5, 
#> 2      PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 124, -1050, 550, 120, -550, -4000, -2.5, , Draw, U7.5, , 
#> 3 MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -1400, 1100, -145, -110, -4800, -2e+05, , Draw, -2.5, U7.5, , 
#> 4       OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -135, 220, -150, 110, -750, -525, U4.5, Draw, , -1.5, , 
#>   awayTeam.name.default
#> 1        Golden Knights
#> 2             Red Wings
#> 3             Avalanche
#> 4                Flames
#> 
# }
```
