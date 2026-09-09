# **Load PWHL expected-goals play-by-play from the SportsDataverse data repo**

Loads season-level PWHL play-by-play enriched with the fastRhockey
expected-goals (xG) model outputs – one row per event with the xG
probability columns appended. Published to the `pwhl_xg_pbp` release tag
on the sportsdataverse-data repo.

## Usage

``` r
load_pwhl_xg_pbp(
  seasons = most_recent_pwhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit season-ending years (2024 onward), or `TRUE` for
  every published season.

- ...:

  Additional arguments passed to the underlying database write.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A `fastRhockey_data` tibble with one row per play event.

## See also

Other PWHL Loader Functions:
[`load_pwhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_boxscores.md),
[`load_pwhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_boxscores.md),
[`load_pwhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedules.md),
[`load_pwhl_shifts()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shifts.md),
[`load_pwhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_boxscores.md),
[`load_pwhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_pwhl_xg_pbp(2024))
#> ── PWHL xG-enriched shots ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-02 19:01:46 UTC
#> # A tibble: 4,922 × 21
#>    game_id game_season game_date      team_id player_id goalie_id period_of_game
#>      <int>       <int> <chr>            <int>     <int>     <int> <chr>         
#>  1       2        2024 Monday, Janua…       4        44        64 1             
#>  2       2        2024 Monday, Janua…       4        36        64 1             
#>  3       2        2024 Monday, Janua…       4        46        64 1             
#>  4       2        2024 Monday, Janua…       4        86        64 1             
#>  5       2        2024 Monday, Janua…       4        38        64 1             
#>  6       2        2024 Monday, Janua…       6       128       155 1             
#>  7       2        2024 Monday, Janua…       6       126       155 1             
#>  8       2        2024 Monday, Janua…       6       101       155 1             
#>  9       2        2024 Monday, Janua…       6       100       155 1             
#> 10       2        2024 Monday, Janua…       4        34        64 1             
#> # ℹ 4,912 more rows
#> # ℹ 14 more variables: sec_from_start <int>, clock <chr>, x_coord <dbl>,
#> #   y_coord <dbl>, shot_distance <dbl>, shot_angle <dbl>, event_type <chr>,
#> #   shot_quality <chr>, power_play <int>, short_handed <chr>, empty_net <chr>,
#> #   penalty_shot <chr>, goal <lgl>, xg <dbl>
# }
```
