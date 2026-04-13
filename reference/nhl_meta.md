# **NHL Meta**

Returns NHL metadata (players, teams, season states, game types).

Three branches are supported:

- Both `year` and `series_letter` supplied — returns playoff-series
  metadata via `meta/playoff-series/{year}/{seriesLetter}`.

- Only `game_id` supplied — returns per-game metadata via
  `meta/game/{game_id}`.

- No arguments — returns general league meta via `meta`.

## Usage

``` r
nhl_meta(game_id = NULL, year = NULL, series_letter = NULL)
```

## Arguments

- game_id:

  Optional game ID. If provided (and `year` / `series_letter` are not),
  returns metadata for that game.

- year:

  Optional integer playoff year (e.g., `2024`). Must be supplied
  together with `series_letter` to fetch playoff-series metadata.

- series_letter:

  Optional single-letter playoff series identifier (e.g., `"a"`). Must
  be supplied together with `year`.

## Value

Returns a list with NHL metadata.

## Examples

``` r
# \donttest{
  try(nhl_meta())
#> $players
#> list()
#> 
#> $teams
#> list()
#> 
#> $seasonStates
#> list()
#> 
  try(nhl_meta(game_id = 2024020001))
#> $teams
#>   tricode teamId            teamSlug      name.default              name.fr
#> 1     NJD      1 new-jersey-devils-1 New Jersey Devils Devils du New Jersey
#> 2     BUF      7    buffalo-sabres-7    Buffalo Sabres    Sabres de Buffalo
#> 
#> $seasonStates
#> $seasonStates$date
#> [1] "2024-10-04"
#> 
#> $seasonStates$gameType
#> [1] 2
#> 
#> $seasonStates$season
#> [1] 20242025
#> 
#> 
#> $gameState
#> [1] "OFF"
#> 
  try(nhl_meta(year = 2024, series_letter = "a"))
#> $seriesTitle
#> [1] "1st Round"
#> 
#> $teams
#> $teams$topSeed
#> $teams$topSeed$name
#> $teams$topSeed$name$default
#> [1] "Florida Panthers"
#> 
#> $teams$topSeed$name$fr
#> [1] "Panthers de la Floride"
#> 
#> 
#> $teams$topSeed$commonName
#> $teams$topSeed$commonName$default
#> [1] "Panthers"
#> 
#> 
#> $teams$topSeed$tricode
#> [1] "FLA"
#> 
#> $teams$topSeed$teamSlug
#> [1] "florida-panthers-13"
#> 
#> 
#> $teams$bottomSeed
#> $teams$bottomSeed$name
#> $teams$bottomSeed$name$default
#> [1] "Tampa Bay Lightning"
#> 
#> $teams$bottomSeed$name$fr
#> [1] "Lightning de Tampa Bay"
#> 
#> 
#> $teams$bottomSeed$commonName
#> $teams$bottomSeed$commonName$default
#> [1] "Lightning"
#> 
#> 
#> $teams$bottomSeed$tricode
#> [1] "TBL"
#> 
#> $teams$bottomSeed$teamSlug
#> [1] "tampa-bay-lightning-14"
#> 
#> 
#> 
# }
```
