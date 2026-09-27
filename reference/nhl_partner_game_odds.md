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
#> [1] "2026-09-26"
#> 
#> $lastUpdatedUTC
#> [1] "2026-09-27T04:00:38Z"
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
#>        gameId gameType         startTimeUTC homeTeam.id homeTeam.abbrev
#> 1  2026010052        1 2026-09-26T20:00:00Z          26             LAK
#> 2  2026010053        1 2026-09-26T19:00:00Z          18             NSH
#> 3  2026010055        1 2026-09-26T19:00:00Z           7             BUF
#> 4  2026010056        1 2026-09-26T23:00:00Z          23             VAN
#> 5  2026010057        1 2026-09-26T21:00:00Z           4             PHI
#> 6  2026010058        1 2026-09-26T23:00:00Z          16             CHI
#> 7  2026010059        1 2026-09-26T22:00:00Z          13             FLA
#> 8  2026010060        1 2026-09-27T01:00:00Z          22             EDM
#> 9  2026010061        1 2026-09-26T23:00:00Z          17             DET
#> 10 2026010062        1 2026-09-26T23:30:00Z           2             NYI
#> 11 2026010063        1 2026-09-27T02:00:00Z          54             VGK
#>                                          homeTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/LAK_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/EDM_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/VGK_light.svg
#>                                                                homeTeam.odds
#> 1     MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, -445, 280, -110, , -4.5, O8.5
#> 2     PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, 210, 1100, -115, +3.5, , O5.5
#> 3     MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, 650, -180, -175, , O3.5, +1.5
#> 4     OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 420, 1200, -105, O7.5, -1.5, 
#> 5      PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, 124, -175, 950, +2.5, O6.5, 
#> 6     OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 160, 124, -3500, O5.5, -2.5, 
#> 7      MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, -115, 130, 154, , -1.5, O5.5
#> 8  PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, -130, -115, -1e+05, -3.5, O4.5, 
#> 9    MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, 1200, -166, -160, , +3.5, O4.5
#> 10    MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, 470, -200, -345, , O4.5, +4.5
#> 11     OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, 124, -220, 180, O5.5, +1.5, 
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                  Kings          24             ANA
#> 2              Predators          12             CAR
#> 3                 Sabres           5             PIT
#> 4                Canucks          55             SEA
#> 5                 Flyers          15             WSH
#> 6             Blackhawks          19             STL
#> 7               Panthers          14             TBL
#> 8                 Oilers          20             CGY
#> 9              Red Wings          29             CBJ
#> 10             Islanders           1             NJD
#> 11        Golden Knights          28             SJS
#>                                                    awayTeam.logo
#> 1            https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#> 2            https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 3            https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 4            https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 6            https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 7            https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 8            https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#> 9            https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 10           https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 11           https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#>                                                               awayTeam.odds
#> 1    MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, 310, -395, -120, , +4.5, U8.5
#> 2  PUCK_LINE, MONEY_LINE_2_WAY, OVER_UNDER, -280, -2500, -115, -3.5, , U5.5
#> 3    MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, -1150, 140, 135, , U3.5, -1.5
#> 4  OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, -660, -2800, -125, U7.5, +1.5, 
#> 5   PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, -160, 135, -1950, -2.5, U6.5, 
#> 6   OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, -210, -160, 1400, U5.5, +2.5, 
#> 7   MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, -115, -166, -200, , +1.5, U5.5
#> 8    PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, 100, -115, 4000, +3.5, U4.5, 
#> 9    MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, -2800, 130, 124, , -3.5, U4.5
#> 10    MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, -750, 154, 250, , U4.5, -4.5
#> 11   OVER_UNDER, PUCK_LINE, MONEY_LINE_2_WAY, -160, 170, -238, U5.5, -1.5, 
#>    awayTeam.name.default
#> 1                  Ducks
#> 2             Hurricanes
#> 3               Penguins
#> 4                 Kraken
#> 5               Capitals
#> 6                  Blues
#> 7              Lightning
#> 8                 Flames
#> 9           Blue Jackets
#> 10                Devils
#> 11                Sharks
#> 
# }
```
