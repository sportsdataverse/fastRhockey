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
#> [1] "2026-09-29"
#> 
#> $lastUpdatedUTC
#> [1] "2026-09-30T04:30:38Z"
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
#> 1 2026020001        2 2026-09-29T21:00:00Z          12             CAR
#> 2 2026020002        2 2026-09-29T23:00:00Z          10             TOR
#> 3 2026020003        2 2026-09-30T00:00:00Z           6             BOS
#> 4 2026020004        2 2026-09-30T02:00:00Z          22             EDM
#> 5 2026020005        2 2026-09-30T02:30:00Z          54             VGK
#>                                         homeTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/TOR_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 5 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                                                                                                      homeTeam.odds
#> 1       OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -125, 100, 360, -140, 200, -162, O1.5, Draw, -1.5, , , 
#> 2       MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 325, -140, 245, -270, 1200, 750, , +1.5, Draw, O4.5, , 
#> 3       MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 450, 950, 325, -125, -260, -135, , -1.5, O1.5, , Draw, 
#> 4      MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, 1200, 230, -270, 190, -154, 700, , , +1.5, Draw, O10.5, 
#> 5 MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -20000, -160, -1100, 160, 950, -50000, , -2.5, , O7.5, Draw, 
#>   homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1            Hurricanes          13             FLA
#> 2           Maple Leafs           8             MTL
#> 3                Bruins           3             NYR
#> 4                Oilers          23             VAN
#> 5        Golden Knights          16             CHI
#>                                         awayTeam.logo
#> 1 https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 2 https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 3 https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 4 https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 5 https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#>                                                                                                                                                  awayTeam.odds
#> 1    OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -105, 100, -540, 110, 320, 126, U1.5, Draw, +1.5, , , 
#> 2  MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -475, 110, 245, 200, -225, -1450, , -1.5, Draw, U4.5, , 
#> 3 MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 550, -1950, -475, -105, -260, 105, , +1.5, U1.5, , Draw, 
#> 4 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY_TNB, -175, -315, 200, 190, 120, -1300, , , -1.5, Draw, U10.5, 
#> 5  MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 3000, 124, 4500, -210, 950, 3000, , +2.5, , U7.5, Draw, 
#>   awayTeam.name.default
#> 1              Panthers
#> 2             Canadiens
#> 3               Rangers
#> 4               Canucks
#> 5            Blackhawks
#> 
# }
```
