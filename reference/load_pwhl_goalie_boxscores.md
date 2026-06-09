# **Load PWHL goalie box scores (alias)**

Alias of
[`load_pwhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_box.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_pwhl_goalie_boxscores(seasons = most_recent_pwhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments (currently unused; kept for API symmetry).

## Value

See
[`load_pwhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_box.md).

## See also

[`load_pwhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_box.md)

Other PWHL Loader Functions:
[`load_pwhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_boxscores.md),
[`load_pwhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedules.md),
[`load_pwhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_boxscores.md),
[`load_pwhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_pwhl_goalie_boxscores(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 340 × 21
#>    player_id first_name last_name  position team_id game_id league toi  
#>    <chr>     <chr>      <chr>      <chr>      <int>   <int> <chr>  <chr>
#>  1 70        Erica      Howe       G              6       2 pwhl   0    
#>  2 64        Kristen    Campbell   G              6       2 pwhl   59:59
#>  3 155       Corinne    Schroeder  G              4       2 pwhl   59:59
#>  4 41        Abbey      Levy       G              4       2 pwhl   0    
#>  5 48        Sandra     Abstreiter G              5       3 pwhl   0    
#>  6 59        Emerance   Maschmeyer G              5       3 pwhl   61:04
#>  7 85        Elaine     Chuli      G              3       3 pwhl   0    
#>  8 28        Ann-Renée  Desbiens   G              3       3 pwhl   61:04
#>  9 19        Emma       Söderberg  G              1       4 pwhl   0    
#> 10 6         Aerin      Frankel    G              1       4 pwhl   60:00
#> # ℹ 330 more rows
#> # ℹ 13 more variables: time_on_ice <dbl>, saves <int>, goals_against <int>,
#> #   shots_against <int>, goals <int>, assists <int>, points <int>,
#> #   penalty_minutes <int>, faceoff_attempts <int>, faceoff_wins <int>,
#> #   faceoff_losses <int>, faceoff_pct <lgl>, starting <int>
# }
```
