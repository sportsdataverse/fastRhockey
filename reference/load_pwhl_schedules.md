# **Load PWHL schedules (alias)**

Alias of
[`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_pwhl_schedules(seasons = most_recent_pwhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database.

## Value

See
[`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md).

## See also

[`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md)

Other PWHL Loader Functions:
[`load_pwhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_boxscores.md),
[`load_pwhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_boxscores.md),
[`load_pwhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_boxscores.md),
[`load_pwhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_pwhl_schedules(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 85 × 29
#>    game_id season game_date   game_status home_team home_team_id away_team
#>    <chr>    <int> <chr>       <chr>       <chr>     <chr>        <chr>    
#>  1 84        2024 Wed, May 8  Final       Toronto   6            Minnesota
#>  2 98        2024 Wed, May 29 Final       Boston    1            Minnesota
#>  3 90        2024 Wed, May 15 Final OT2   Minnesota 2            Toronto  
#>  4 63        2024 Wed, May 1  Final       Toronto   6            Minnesota
#>  5 45        2024 Wed, Mar 6  Final       Toronto   6            Boston   
#>  6 46        2024 Wed, Mar 6  Final       New York  4            Montreal 
#>  7 52        2024 Wed, Mar 20 Final       Toronto   6            Boston   
#>  8 53        2024 Wed, Mar 20 Final       New York  4            Ottawa   
#>  9 50        2024 Wed, Mar 13 Final       Minnesota 2            Boston   
#> 10 4         2024 Wed, Jan 3  Final       Boston    1            Minnesota
#> # ℹ 75 more rows
#> # ℹ 22 more variables: away_team_id <chr>, home_score <chr>, away_score <chr>,
#> #   winner <chr>, venue <chr>, venue_url <chr>, game_type <chr>,
#> #   game_json <lgl>, game_json_url <glue>, PBP <lgl>, player_box <lgl>,
#> #   skater_box <lgl>, goalie_box <lgl>, team_box <lgl>, game_info <lgl>,
#> #   game_rosters <lgl>, scoring_summary <lgl>, penalty_summary <lgl>,
#> #   three_stars <lgl>, officials <lgl>, shots_by_period <lgl>, shootout <lgl>
# }
```
