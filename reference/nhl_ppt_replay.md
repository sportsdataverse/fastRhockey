# **NHL PPT Replay**

Returns event-level play-by-play replay metadata (including video clip
references) for a specific event in a given NHL game from the
`ppt-replay/{gameId}/{eventNumber}` endpoint.

## Usage

``` r
nhl_ppt_replay(game_id, event_number)
```

## Arguments

- game_id:

  Integer or character game ID (e.g., 2023020001).

- event_number:

  Integer event / play number within the game.

## Value

Returns a list with event replay metadata (clip IDs, video URLs, event
context).

## Examples

``` r
# \donttest{
  try(nhl_ppt_replay(game_id = 2023020001, event_number = 1))
#> $id
#> [1] 2023020001
#> 
#> $gameDate
#> [1] "2023-10-10"
#> 
#> $awayTeam
#> $awayTeam$id
#> [1] 18
#> 
#> $awayTeam$name
#> $awayTeam$name$default
#> [1] "Predators"
#> 
#> 
#> $awayTeam$abbrev
#> [1] "NSH"
#> 
#> $awayTeam$placeName
#> $awayTeam$placeName$default
#> [1] "Nashville"
#> 
#> 
#> $awayTeam$placeNameWithPreposition
#> $awayTeam$placeNameWithPreposition$default
#> [1] "Nashville"
#> 
#> $awayTeam$placeNameWithPreposition$fr
#> [1] "de Nashville"
#> 
#> 
#> $awayTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/NSH_light.svg"
#> 
#> $awayTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/NSH_dark.svg"
#> 
#> 
#> $homeTeam
#> $homeTeam$id
#> [1] 14
#> 
#> $homeTeam$name
#> $homeTeam$name$default
#> [1] "Lightning"
#> 
#> 
#> $homeTeam$abbrev
#> [1] "TBL"
#> 
#> $homeTeam$placeName
#> $homeTeam$placeName$default
#> [1] "Tampa Bay"
#> 
#> 
#> $homeTeam$placeNameWithPreposition
#> $homeTeam$placeNameWithPreposition$default
#> [1] "Tampa Bay"
#> 
#> $homeTeam$placeNameWithPreposition$fr
#> [1] "de Tampa Bay"
#> 
#> 
#> $homeTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/TBL_light.svg"
#> 
#> $homeTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/TBL_dark.svg"
#> 
#> 
#> $gameState
#> [1] "OFF"
#> 
#> $gameType
#> [1] 2
#> 
# }
```
