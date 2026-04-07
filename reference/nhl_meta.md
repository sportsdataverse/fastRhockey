# **NHL Meta**

Returns NHL metadata (players, teams, season states, game types).

## Usage

``` r
nhl_meta(game_id = NULL)
```

## Arguments

- game_id:

  Optional game ID. If provided, returns metadata for that game. If
  NULL, returns general league meta.

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
# }
```
