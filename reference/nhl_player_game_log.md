# **NHL Player Game Log**

Returns game-by-game stats for an NHL player. Supports both current
season and historical season lookups.

## Usage

``` r
nhl_player_game_log(player_id, season = NULL, game_type = 2)
```

## Arguments

- player_id:

  Integer player ID (e.g., 8478402 for Connor McDavid)

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns the current season game log.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

## Value

Returns a data frame with game-by-game statistics.

## Examples

``` r
# \donttest{
  try(nhl_player_game_log(player_id = 8478402))
#> ── NHL Player Game Log ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:57:07 UTC
#> # A tibble: 78 × 23
#>     game_id team_abbrev home_road_flag game_date goals assists points plus_minus
#>       <int> <chr>       <chr>          <chr>     <int>   <int>  <int>      <int>
#>  1   2.03e9 EDM         R              2026-04-…     1       1      2         -1
#>  2   2.03e9 EDM         H              2026-04-…     0       0      0          1
#>  3   2.03e9 EDM         H              2026-04-…     0       1      1          0
#>  4   2.03e9 EDM         H              2026-03-…     1       0      1          1
#>  5   2.03e9 EDM         H              2026-03-…     1       2      3          2
#>  6   2.03e9 EDM         R              2026-03-…     1       2      3          1
#>  7   2.03e9 EDM         R              2026-03-…     2       0      2          2
#>  8   2.03e9 EDM         H              2026-03-…     1       0      1         -3
#>  9   2.03e9 EDM         H              2026-03-…     0       0      0         -1
#> 10   2.03e9 EDM         H              2026-03-…     0       1      1          0
#> # ℹ 68 more rows
#> # ℹ 15 more variables: power_play_goals <int>, power_play_points <int>,
#> #   game_winning_goals <int>, ot_goals <int>, shots <int>, shifts <int>,
#> #   shorthanded_goals <int>, shorthanded_points <int>, opponent_abbrev <chr>,
#> #   pim <int>, toi <chr>, common_name_default <chr>,
#> #   opponent_common_name_default <chr>, opponent_common_name_fr <chr>,
#> #   player_id <dbl>
# }
```
