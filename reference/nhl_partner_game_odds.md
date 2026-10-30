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
#> [1] "2026-10-29"
#> 
#> $lastUpdatedUTC
#> [1] "2026-10-30T00:00:38Z"
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
#> 1  2026020203        2 2026-10-29T23:30:00Z          17             DET
#> 2  2026020204        2 2026-10-29T23:00:00Z          14             TBL
#> 3  2026020205        2 2026-10-29T23:00:00Z           5             PIT
#> 4  2026020206        2 2026-10-29T23:00:00Z          12             CAR
#> 5  2026020207        2 2026-10-30T00:00:00Z          19             STL
#> 6  2026020208        2 2026-10-30T00:00:00Z          18             NSH
#> 7  2026020209        2 2026-10-30T00:00:00Z          25             DAL
#> 8  2026020210        2 2026-10-30T00:00:00Z          30             MIN
#> 9  2026020211        2 2026-10-30T02:00:00Z          24             ANA
#> 10 2026020212        2 2026-10-30T02:00:00Z          26             LAK
#> 11 2026020213        2 2026-10-30T02:00:00Z          28             SJS
#>                                          homeTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/LAK_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#>                                                                                                                                                  homeTeam.odds
#> 1    PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, -195, 310, -115, 140, 240, 160, +1.5, Draw, O5.5, , , 
#> 2  PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 130, -110, 280, -115, -215, -240, -1.5, O6.5, Draw, , , 
#> 3    OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -140, -115, 500, 750, 380, 550, O6.5, +2.5, Draw, , , 
#> 4   MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 400, -275, 124, -540, 105, -850, Draw, , O5.5, , -2.5, 
#> 5                                 MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, -115, -125, 205, 130, 300, , O5.5, -1.5, , Draw
#> 6                                MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, -105, 320, -270, -105, 130, , Draw, +1.5, O6.5, 
#> 7                                MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, -105, -175, 320, -135, 145, , , Draw, O5.5, -1.5
#> 8                                PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, 124, 320, -218, -125, -125, -1.5, Draw, , O5.5, 
#> 9                                 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, 320, 120, 180, -125, -198, Draw, , , O6.5, +1.5
#> 10                                MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, 102, 145, -258, -125, 300, , , +1.5, O5.5, Draw
#> 11                               OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, -135, 340, 136, -105, -175, O6.5, Draw, -1.5, , 
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1              Red Wings          16             CHI
#> 2              Lightning          52             WPG
#> 3               Penguins          68             UTA
#> 4             Hurricanes          29             CBJ
#> 5                  Blues           6             BOS
#> 6              Predators          15             WSH
#> 7                  Stars           8             MTL
#> 8                   Wild           2             NYI
#> 9                  Ducks           7             BUF
#> 10                 Kings           9             OTT
#> 11                Sharks          23             VAN
#>                                                    awayTeam.logo
#> 1            https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#> 2            https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 3            https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 4            https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 5            https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 7            https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 8            https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 9            https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 10           https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 11           https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#>                                                                                                                                                  awayTeam.odds
#> 1  PUCK_LINE, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY_TNB, 150, 310, -115, -180, -110, -212, -1.5, Draw, U5.5, , , 
#> 2    PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, -166, -120, 280, 275, 165, 180, +1.5, U6.5, Draw, , , 
#> 3  OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_2_WAY_TNB, 110, -115, 500, -330, -580, -975, U6.5, -2.5, Draw, , , 
#> 4    MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_2_WAY_TNB, 400, 650, -160, 360, -135, 500, Draw, , U5.5, , +2.5, 
#> 5                                 MONEY_LINE_2_WAY, OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, -105, 105, -250, 150, 300, , U5.5, +1.5, , Draw
#> 6                                 MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, -115, 320, 220, -115, 145, , Draw, -1.5, U6.5, 
#> 7                                  MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, 205, 145, 320, 114, -175, , , Draw, U5.5, +1.5
#> 8                                  PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, -148, 320, 180, 105, 250, +1.5, Draw, , U5.5, 
#> 9                                  MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, PUCK_LINE, 320, -142, 105, 105, 164, Draw, , , U6.5, -1.5
#> 10                                 MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, -122, 135, 210, 105, 300, , , -1.5, U5.5, Draw
#> 11                                 OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, 114, 340, -162, 195, 145, U6.5, Draw, +1.5, , 
#>    awayTeam.name.default awayTeam.name.fr
#> 1             Blackhawks             <NA>
#> 2                   Jets             <NA>
#> 3                Mammoth             <NA>
#> 4           Blue Jackets             <NA>
#> 5                 Bruins             <NA>
#> 6               Capitals             <NA>
#> 7              Canadiens             <NA>
#> 8              Islanders             <NA>
#> 9                 Sabres             <NA>
#> 10              Senators        Sénateurs
#> 11               Canucks             <NA>
#> 
# }
```
