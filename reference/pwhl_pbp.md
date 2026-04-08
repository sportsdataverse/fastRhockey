# **PWHL Play-by-play**

PWHL Play-by-play

## Usage

``` r
pwhl_pbp(game_id)
```

## Arguments

- game_id:

  Game ID that you want play-by-play for

## Value

A data frame with play-by-play event data including columns: game_id,
event, team_id, period_of_game, time_of_period, player_id,
player_name_first, player_name_last, player_position, x_coord, y_coord,
and additional event-specific columns (shot_quality, goal,
penalty_length, home_win, etc.). Coordinate columns are provided in
multiple projections.

## Examples

``` r
# \donttest{
  try(pwhl_pbp(game_id = 27))
#> 2026-04-08 07:40:40.670869: Error encountered: In argument: `game_id = as.numeric(.data$game_id)`.. Please verify the game_id and ensure it corresponds to a valid game in the PWHL.
#> data frame with 0 columns and 0 rows
# }
```
