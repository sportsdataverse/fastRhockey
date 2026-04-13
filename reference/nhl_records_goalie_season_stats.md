# **NHL Records - Goalie Season Stats**

Returns season-by-season goalie statistics from the NHL Records API
(`https://records.nhl.com/site/api/goalie-season-stats`).

## Usage

``` r
nhl_records_goalie_season_stats(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A `fastRhockey_data` tibble of goalie season stats, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_goalie_season_stats(limit = 5))
#> ── NHL Records Goalie Season Stats ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:39 UTC
#> # A tibble: 5 × 34
#>      id active_player first_name franchise_id game_seven_games_played
#>   <int> <lgl>         <chr>             <int> <lgl>                  
#> 1  1485 FALSE         Hardy                NA NA                     
#> 2  7213 FALSE         Hardy                10 NA                     
#> 3  1884 FALSE         Hardy                NA NA                     
#> 4 10187 FALSE         Hardy                23 NA                     
#> 5  1816 FALSE         Hardy                NA NA                     
#> # ℹ 29 more variables: game_seven_losses <lgl>, game_seven_wins <lgl>,
#> #   game_type <int>, games_played <int>, games_started <int>,
#> #   goals_against <int>, goals_against_average <dbl>, last_name <chr>,
#> #   losses <int>, number_of_games_in_season <int>, overtime_games_played <int>,
#> #   overtime_goals_against <int>, overtime_losses <lgl>, overtime_ties <int>,
#> #   overtime_wins <int>, player_id <int>, position_code <chr>,
#> #   rookie_flag <lgl>, save_pctg <dbl>, saves <int>, season_id <int>, …
# }
```
