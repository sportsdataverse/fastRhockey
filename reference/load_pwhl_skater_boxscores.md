# **Load PWHL skater box scores (alias)**

Alias of
[`load_pwhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_box.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_pwhl_skater_boxscores(seasons = most_recent_pwhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments (currently unused; kept for API symmetry).

## Value

See
[`load_pwhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_box.md).

## See also

[`load_pwhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_box.md)

Other PWHL Loader Functions:
[`load_pwhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_boxscores.md),
[`load_pwhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_boxscores.md),
[`load_pwhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedules.md),
[`load_pwhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_pwhl_skater_boxscores(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 3,205 × 22
#>    player_id first_name last_name position team_id game_id league toi  
#>    <chr>     <chr>      <chr>     <chr>      <int>   <int> <chr>  <chr>
#>  1 71        Jocelyne   Larocque  LD             6       2 pwhl   26:41
#>  2 133       Lauriane   Rougeau   LD             6       2 pwhl   12:03
#>  3 68        Kali       Flanagan  RD             6       2 pwhl   21:39
#>  4 131       Olivia     Knowles   RD             6       2 pwhl   9:41 
#>  5 101       Alexa      Vasko     C              6       2 pwhl   10:30
#>  6 74        Allie      Munroe    LD             6       2 pwhl   12:31
#>  7 67        Renata     Fast      RD             6       2 pwhl   24:49
#>  8 126       Samantha   Cogan     RW             6       2 pwhl   14:21
#>  9 65        Jesse      Compher   C              6       2 pwhl   14:55
#> 10 72        Rebecca    Leslie    RW             6       2 pwhl   10:48
#> # ℹ 3,195 more rows
#> # ℹ 14 more variables: time_on_ice <dbl>, goals <int>, assists <int>,
#> #   points <int>, shots <int>, hits <int>, blocked_shots <int>,
#> #   penalty_minutes <int>, plus_minus <int>, faceoff_attempts <int>,
#> #   faceoff_wins <int>, faceoff_losses <int>, faceoff_pct <dbl>, starting <int>
# }
```
