# **NHL Records - Goalie Career Stats**

Returns career goalie statistics from the NHL Records API
(`https://records.nhl.com/site/api/goalie-career-stats`).

## Usage

``` r
nhl_records_goalie_career_stats(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string (e.g. `"playerId=8471679"`).

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A `fastRhockey_data` tibble of career goalie stats, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_goalie_career_stats(limit = 5))
#> ── NHL Records Goalie Career Stats ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:39 UTC
#> # A tibble: 5 × 37
#>      id active_player first_name first_season_for_game_type franchise_id
#>   <int> <lgl>         <chr>                           <int>        <int>
#> 1   151 FALSE         Hardy                        19771978           NA
#> 2  1421 FALSE         Hardy                        19771978           10
#> 3  2108 FALSE         Hardy                        19791980           23
#> 4   155 FALSE         Steve                        19791980           NA
#> 5  1443 FALSE         Steve                        19791980           10
#> # ℹ 32 more variables: game_seven_games_played <lgl>, game_seven_losses <lgl>,
#> #   game_seven_wins <lgl>, game_type_id <int>, games_played <int>,
#> #   goals_against <int>, goals_against_average <dbl>, last_name <chr>,
#> #   last_season_for_game_type <int>, losses <int>, overtime_games_played <int>,
#> #   overtime_goals_against <int>, overtime_goals_against_average <lgl>,
#> #   overtime_losses <lgl>, overtime_save_pctg <dbl>,
#> #   overtime_shots_against <int>, overtime_ties <int>, …
# }
```
