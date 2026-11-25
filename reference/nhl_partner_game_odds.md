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
#> [1] "2026-11-25"
#> 
#> $lastUpdatedUTC
#> [1] "2026-11-25T20:00:39Z"
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
#> 1  2026020386        2 2026-11-26T00:00:00Z           6             BOS
#> 2  2026020387        2 2026-11-26T00:00:00Z           7             BUF
#> 3  2026020388        2 2026-11-26T00:00:00Z          14             TBL
#> 4  2026020389        2 2026-11-26T00:00:00Z          13             FLA
#> 5  2026020390        2 2026-11-26T00:00:00Z           2             NYI
#> 6  2026020391        2 2026-11-26T00:00:00Z           5             PIT
#> 7  2026020392        2 2026-11-26T00:00:00Z          15             WSH
#> 8  2026020393        2 2026-11-26T00:00:00Z          29             CBJ
#> 9  2026020394        2 2026-11-26T00:30:00Z          17             DET
#> 10 2026020395        2 2026-11-26T01:00:00Z          25             DAL
#> 11 2026020396        2 2026-11-26T01:00:00Z          30             MIN
#> 12 2026020397        2 2026-11-26T02:30:00Z          21             COL
#> 13 2026020398        2 2026-11-26T03:00:00Z          24             ANA
#> 14 2026020399        2 2026-11-26T02:40:00Z          55             SEA
#>                                                    homeTeam.logo
#> 1            https://assets.nhle.com/logos/nhl/svg/BOS_light.svg
#> 2            https://assets.nhle.com/logos/nhl/svg/BUF_light.svg
#> 3            https://assets.nhle.com/logos/nhl/svg/TBL_light.svg
#> 4            https://assets.nhle.com/logos/nhl/svg/FLA_light.svg
#> 5            https://assets.nhle.com/logos/nhl/svg/NYI_light.svg
#> 6            https://assets.nhle.com/logos/nhl/svg/PIT_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/WSH_secondary_light.svg
#> 8            https://assets.nhle.com/logos/nhl/svg/CBJ_light.svg
#> 9            https://assets.nhle.com/logos/nhl/svg/DET_light.svg
#> 10           https://assets.nhle.com/logos/nhl/svg/DAL_light.svg
#> 11           https://assets.nhle.com/logos/nhl/svg/MIN_light.svg
#> 12           https://assets.nhle.com/logos/nhl/svg/COL_light.svg
#> 13           https://assets.nhle.com/logos/nhl/svg/ANA_light.svg
#> 14           https://assets.nhle.com/logos/nhl/svg/SEA_light.svg
#>                                                                                                                   homeTeam.odds
#> 1  OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, -105, 290, 105, -170, 150, O6.5, Draw, , , -1.5
#> 2  MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, 275, -135, 185, -102, 130, Draw, , -1.5, O6.5, 
#> 3  MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 145, -245, 105, -105, 280, , +1.5, O6.5, , Draw
#> 4  MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, 140, -120, 260, 205, -125, , O5.5, Draw, -1.5, 
#> 5  MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, 125, 195, -135, 270, -130, , -1.5, O5.5, Draw, 
#> 6  PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 154, -175, -105, 290, 114, -1.5, , , Draw, O6.5
#> 7  PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, 154, 110, -162, -102, 290, -1.5, , , O6.5, Draw
#> 8   PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, -205, 110, 124, 275, 180, +1.5, O6.5, , Draw, 
#> 9  OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, 105, 110, 300, -130, -230, O6.5, -1.5, Draw, , 
#> 10 MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 105, 164, -170, 280, -122, , -1.5, , Draw, O5.5
#> 11  MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, -148, 105, 115, 285, 170, , O6.5, , Draw, -1.5
#> 12 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, 285, -120, 185, -102, 140, Draw, , -1.5, O6.5, 
#> 13 OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, -122, -135, 120, 170, 280, O6.5, , , -1.5, Draw
#> 14 MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, 110, -102, -155, 160, 290, , O6.5, , -1.5, Draw
#>    homeTeam.name.default awayTeam.id awayTeam.abbrev
#> 1                 Bruins          52             WPG
#> 2                 Sabres           3             NYR
#> 3              Lightning          12             CAR
#> 4               Panthers           9             OTT
#> 5              Islanders          19             STL
#> 6               Penguins          20             CGY
#> 7               Capitals           4             PHI
#> 8           Blue Jackets           1             NJD
#> 9              Red Wings          23             VAN
#> 10                 Stars          18             NSH
#> 11                  Wild          68             UTA
#> 12             Avalanche           8             MTL
#> 13                 Ducks          28             SJS
#> 14                Kraken          16             CHI
#>                                          awayTeam.logo
#> 1  https://assets.nhle.com/logos/nhl/svg/WPG_light.svg
#> 2  https://assets.nhle.com/logos/nhl/svg/NYR_light.svg
#> 3  https://assets.nhle.com/logos/nhl/svg/CAR_light.svg
#> 4  https://assets.nhle.com/logos/nhl/svg/OTT_light.svg
#> 5  https://assets.nhle.com/logos/nhl/svg/STL_light.svg
#> 6  https://assets.nhle.com/logos/nhl/svg/CGY_light.svg
#> 7  https://assets.nhle.com/logos/nhl/svg/PHI_light.svg
#> 8  https://assets.nhle.com/logos/nhl/svg/NJD_light.svg
#> 9  https://assets.nhle.com/logos/nhl/svg/VAN_light.svg
#> 10 https://assets.nhle.com/logos/nhl/svg/NSH_light.svg
#> 11 https://assets.nhle.com/logos/nhl/svg/UTA_light.svg
#> 12 https://assets.nhle.com/logos/nhl/svg/MTL_light.svg
#> 13 https://assets.nhle.com/logos/nhl/svg/SJS_light.svg
#> 14 https://assets.nhle.com/logos/nhl/svg/CHI_light.svg
#>                                                                                                                   awayTeam.odds
#> 1  OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, -115, 290, 200, 142, -180, U6.5, Draw, , , +1.5
#> 2  MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, 275, 114, -225, -118, 165, Draw, , +1.5, U6.5, 
#> 3  MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, 145, 200, -125, -115, 280, , -1.5, U6.5, , Draw
#> 4   MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, 160, 100, 260, -250, 105, , U5.5, Draw, +1.5, 
#> 5   MONEY_LINE_3_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, 170, -238, 114, 270, 110, , +1.5, U5.5, Draw, 
#> 6  PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, OVER_UNDER, -185, 145, 215, 290, -135, +1.5, , , Draw, U6.5
#> 7  PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, -185, 185, 136, -118, 290, +1.5, , , U6.5, Draw
#> 8  PUCK_LINE, OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, 170, -130, -148, 275, 120, -1.5, U6.5, , Draw, 
#> 9  OVER_UNDER, PUCK_LINE, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, -125, -130, 300, 275, 190, U6.5, +1.5, Draw, , 
#> 10  MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, OVER_UNDER, 200, -198, 142, 280, 102, , +1.5, , Draw, U5.5
#> 11 MONEY_LINE_2_WAY, OVER_UNDER, MONEY_LINE_3_WAY, MONEY_LINE_3_WAY, PUCK_LINE, 124, -125, 175, 285, -205, , U6.5, , Draw, +1.5
#> 12 MONEY_LINE_3_WAY, MONEY_LINE_2_WAY, PUCK_LINE, OVER_UNDER, MONEY_LINE_3_WAY, 285, 100, -225, -118, 150, Draw, , +1.5, U6.5, 
#> 13  OVER_UNDER, MONEY_LINE_2_WAY, MONEY_LINE_3_WAY, PUCK_LINE, MONEY_LINE_3_WAY, 102, 114, 170, -205, 280, U6.5, , , +1.5, Draw
#> 14 MONEY_LINE_3_WAY, OVER_UNDER, MONEY_LINE_2_WAY, PUCK_LINE, MONEY_LINE_3_WAY, 185, -118, 130, -192, 290, , U6.5, , +1.5, Draw
#>    awayTeam.name.default awayTeam.name.fr
#> 1                   Jets             <NA>
#> 2                Rangers             <NA>
#> 3             Hurricanes             <NA>
#> 4               Senators        Sénateurs
#> 5                  Blues             <NA>
#> 6                 Flames             <NA>
#> 7                 Flyers             <NA>
#> 8                 Devils             <NA>
#> 9                Canucks             <NA>
#> 10             Predators             <NA>
#> 11               Mammoth             <NA>
#> 12             Canadiens             <NA>
#> 13                Sharks             <NA>
#> 14            Blackhawks             <NA>
#> 
# }
```
