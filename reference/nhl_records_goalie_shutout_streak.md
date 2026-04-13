# **NHL Records - Goalie Shutout Streaks**

Returns goalie shutout streak records from the NHL Records API
(`https://records.nhl.com/site/api/goalie-shutout-streak`).

## Usage

``` r
nhl_records_goalie_shutout_streak(
  cayenne_exp = NULL,
  limit = NULL,
  start = NULL
)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A `fastRhockey_data` tibble of goalie shutout streaks, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_goalie_shutout_streak(limit = 5))
#> ── NHL Records Goalie Shutout Streak ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:39 UTC
#> # A tibble: 5 × 17
#>      id active_player active_streak duration_min_sec duration_seconds end_date  
#>   <int> <lgl>         <lgl>         <chr>                       <int> <chr>     
#> 1 13481 FALSE         FALSE         75:00                        4500 1996-02-17
#> 2 13447 FALSE         FALSE         75:00                        4500 1980-10-14
#> 3 13402 FALSE         FALSE         75:00                        4500 1942-12-17
#> 4 13438 FALSE         FALSE         75:00                        4500 1969-10-18
#> 5 13362 FALSE         FALSE         75:00                        4500 1926-12-30
#> # ℹ 11 more variables: first_name <chr>, franchise_id <int>,
#> #   game_type_id <int>, last_name <chr>, player_id <int>, saves <lgl>,
#> #   season_id <int>, start_date <chr>, team_abbrev <chr>, team_id <int>,
#> #   team_name <chr>
# }
```
