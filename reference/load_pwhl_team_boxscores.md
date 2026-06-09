# **Load PWHL team box scores (alias)**

Alias of
[`load_pwhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_box.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_pwhl_team_boxscores(seasons = most_recent_pwhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments (currently unused; kept for API symmetry).

## Value

See
[`load_pwhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_box.md).

## See also

[`load_pwhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_box.md)

Other PWHL Loader Functions:
[`load_pwhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_boxscores.md),
[`load_pwhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_boxscores.md),
[`load_pwhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedules.md),
[`load_pwhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_pwhl_team_boxscores(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 170 × 23
#>    game_id team_id team           team_abbr team_side shots goals  hits pp_goals
#>      <int>   <int> <chr>          <chr>     <chr>     <int> <int> <int>    <int>
#>  1       2       6 PWHL Toronto   TOR       home         29     0     0        0
#>  2       2       4 PWHL New York  NY        away         28     4     0        0
#>  3       3       5 PWHL Ottawa    OTT       home         28     2     0        1
#>  4       3       3 PWHL Montreal  MTL       away         24     3     0        0
#>  5       4       1 PWHL Boston    BOS       home         35     2     0        1
#>  6       4       2 PWHL Minnesota MIN       away         16     3     0        0
#>  7       5       4 PWHL New York  NY        home         31     2     0        0
#>  8       5       6 PWHL Toronto   TOR       away         37     3     0        1
#>  9       6       2 PWHL Minnesota MIN       home         22     3     0        0
#> 10       6       3 PWHL Montreal  MTL       away         24     0     0        0
#> # ℹ 160 more rows
#> # ℹ 14 more variables: pp_opportunities <int>, goal_count <int>,
#> #   assist_count <int>, penalty_minutes <int>, infraction_count <int>,
#> #   faceoff_attempts <int>, faceoff_wins <int>, faceoff_win_pct <dbl>,
#> #   season_wins <int>, season_losses <int>, season_ot_wins <int>,
#> #   season_ot_losses <int>, season_so_losses <int>, season_record <chr>
# }
```
