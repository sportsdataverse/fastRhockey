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
#> [1] "2026-11-01"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-02T03:00:38Z"
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
#> 1 2026020227        2 2026-11-01T18:00:00Z          12             CAR
#> 2 2026020228        2 2026-11-01T20:30:00Z          15             WSH
#> 3 2026020229        2 2026-11-01T21:00:00Z           4             PHI
#> 4 2026020230        2 2026-11-01T23:00:00Z           3             NYR
#> 5 2026020231        2 2026-11-02T01:00:00Z          24             ANA
#>                                                   homeTeam.logo
#> 1           https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 3           https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 4           https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 5           https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#>                                                                                                                                                      homeTeam.odds
#> 1     PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 290, -500, -265, 154, 35000, -775, -7.5, , , O7.5, Draw, 
#> 2      MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 12000, 145, -120, 1200, 700, 700, , O5.5, +1.5, , Draw, 
#> 3     MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -750, -425, 450, -425, 140, -2200, , , Draw, -2.5, O7.5, 
#> 4     PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -130, -110, 2000, 6000, 550, 1400, +2.5, O6.5, , , Draw, 
#> 5 MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -20000, -115, -215, -4000, 2200, -148, , O9.5, -2.5, , Draw, 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1            Hurricanes          52             WPG
#> 2              Capitals          30             MIN
#> 3                Flyers          68             UTA
#> 4               Rangers          22             EDM
#> 5                 Ducks           9             OTT
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 5 https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#>                                                                                                                                                    awayTeam.odds
#> 1  PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -410, 340, 60000, -200, 35000, 475, +7.5, , , U7.5, Draw, 
#> 2 MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -900, -188, -110, -2800, 700, -1300, , U5.5, -1.5, , Draw, 
#> 3     MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 470, 1400, 450, 300, -180, 1000, , , Draw, +2.5, U7.5, 
#> 4 PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 100, -120, -550, -1e+05, 550, -4000, -2.5, U6.5, , , Draw, 
#> 5   MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 3000, -115, 165, 16000, 2200, 115, , U9.5, +2.5, , Draw, 
#>   awayTeam.name.default awayTeam.name.fr
#> 1                  Jets             <NA>
#> 2                  Wild             <NA>
#> 3               Mammoth             <NA>
#> 4                Oilers             <NA>
#> 5              Senators        Sénateurs
#> 
# }
```
