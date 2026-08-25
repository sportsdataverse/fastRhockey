# **Load PWHL shift charts from the SportsDataverse data repo**

Loads season-level PWHL shift data built from the HockeyTech game-center
feeds – one row per player-shift with on/off times. Published to the
`pwhl_shifts` release tag on the sportsdataverse-data repo.

## Usage

``` r
load_pwhl_shifts(
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

A `fastRhockey_data` tibble with one row per player-shift.

## See also

Other PWHL Loader Functions:
[`load_pwhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_boxscores.md),
[`load_pwhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_boxscores.md),
[`load_pwhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedules.md),
[`load_pwhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_boxscores.md),
[`load_pwhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_boxscores.md),
[`load_pwhl_xg_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_xg_pbp.md)

## Examples

``` r
# \donttest{
  try(load_pwhl_shifts(2024))
#> ── PWHL per-game player shifts (on-ice stints) ──────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-22 19:29:24 UTC
#> # A tibble: 66,752 × 14
#>    game_id player_id first_name last_name jersey_number  home period start_time
#>      <int>     <int> <chr>      <chr>     <chr>         <int>  <int> <chr>     
#>  1       2        64 Kristen    Campbell  50                1      1 20:00     
#>  2       2        64 Kristen    Campbell  50                1      2 20:00     
#>  3       2        64 Kristen    Campbell  50                1      3 20:00     
#>  4       2        65 Jesse      Compher   18                1      1 01:57     
#>  5       2        65 Jesse      Compher   18                1      1 03:44     
#>  6       2        65 Jesse      Compher   18                1      1 06:18     
#>  7       2        65 Jesse      Compher   18                1      1 06:42     
#>  8       2        65 Jesse      Compher   18                1      1 09:49     
#>  9       2        65 Jesse      Compher   18                1      1 12:49     
#> 10       2        65 Jesse      Compher   18                1      1 15:52     
#> # ℹ 66,742 more rows
#> # ℹ 6 more variables: end_time <chr>, length <chr>, start_s <int>, end_s <int>,
#> #   goal_on_shift <int>, penalty_on_shift <int>
# }
```
