# **PWHL Player Game Log**

Retrieves game-by-game statistics for a PWHL player in a given season.

## Usage

``` r
pwhl_player_game_log(
  player_id,
  season = most_recent_pwhl_season(),
  game_type = "regular"
)
```

## Arguments

- player_id:

  Numeric player ID

- season:

  Season (YYYY) to pull the game log from, the concluding year in
  XXXX-YY format. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: "regular" (default), "preseason", or "playoffs".

## Value

A data frame with per-game statistics for the player, or NULL if
unavailable.

- `player_id` - Player ID.

- `game_id` - Game ID.

- `date` - Game date.

- `team` - Team abbreviation.

- `opponent` - Opponent abbreviation.

- `home_away` - Home or Away indicator.

- `goals` - Goals scored.

- `assists` - Assists.

- `points` - Total points.

- `plus_minus` - Plus/minus rating.

- `penalty_minutes` - Penalty minutes.

- `shots` - Shots on goal.

## Examples

``` r
# \donttest{
  try(pwhl_player_game_log(player_id = 28, season = 2025))
#> ── PWHL Player Game Log ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:34:04 UTC
#> # A tibble: 21 × 38
#>    g_month    id home_team visiting_team date_played  home goalie home_team_code
#>    <chr>   <dbl> <chr>     <chr>         <chr>       <dbl>  <dbl> <chr>         
#>  1 Novemb…   106 3         5             2024-11-30      1      1 MTL           
#>  2 Decemb…   110 3         4             2024-12-04      1      1 MTL           
#>  3 Decemb…   119 2         3             2024-12-28      0      1 MIN           
#>  4 Decemb…   121 3         1             2024-12-30      1      1 MTL           
#>  5 January   173 1         3             2025-01-05      0      1 BOS           
#>  6 January   174 6         3             2025-01-08      0      1 TOR           
#>  7 January   131 3         2             2025-01-17      1      1 MTL           
#>  8 January   176 3         5             2025-01-19      1      1 MTL           
#>  9 January   137 3         5             2025-01-29      1      1 MTL           
#> 10 Februa…   141 4         3             2025-02-02      0      1 NY            
#> # ℹ 11 more rows
#> # ℹ 30 more variables: home_team_name <chr>, home_division <chr>,
#> #   visiting_team_code <chr>, visiting_team_name <chr>,
#> #   visiting_division <chr>, goals_against <dbl>, seconds_played <chr>,
#> #   win <dbl>, tie <chr>, loss <dbl>, total_losses <chr>, shutout <chr>,
#> #   ot_loss <chr>, shootout_loss <chr>, saves <dbl>, shots_against <dbl>,
#> #   shootout_saves <chr>, shootout_goals_against <chr>, shootout_shots <chr>, …
# }
```
