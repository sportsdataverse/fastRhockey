# **PWHL Player Season Stats**

Retrieves career and season-by-season statistics for a PWHL player.

## Usage

``` r
pwhl_player_stats(player_id)
```

## Arguments

- player_id:

  Numeric player ID

## Value

A data frame with per-season statistics for the player, or NULL if
unavailable.

- `player_id` - Player ID.

- `season_name` - Season name.

- `season_id` - Season ID.

- `team` - Team abbreviation.

- `league` - League name.

- `games_played` - Games played.

- `goals` - Goals scored.

- `assists` - Assists.

- `points` - Total points.

- `plus_minus` - Plus/minus rating.

- `penalty_minutes` - Penalty minutes.

## Examples

``` r
# \donttest{
  try(pwhl_player_stats(player_id = 28))
#> ── PWHL Player Season Stats ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:34:05 UTC
#> # A tibble: 7 × 42
#>   season_id season_name   shortname playoff career max_start_date veteran_status
#>       <dbl> <chr>         <chr>     <chr>   <chr>  <chr>          <chr>         
#> 1         8 2025-26 Regu… 2025-26 … 0       1      2025-11-21     2             
#> 2         5 2024-25 Regu… 2024-25 … 0       1      2024-11-25     2             
#> 3         1 2024 Regular… 2024 Reg  0       1      2023-11-01     2             
#> 4        14 Total         Total     0       3      6072           6             
#> 5         6 2025 Playoffs 2025 Pla… 1       1      2025-05-06     2             
#> 6         3 2024 Playoffs 2024 Pla… 1       1      2024-05-06     2             
#> 7         9 Total         Total     1       2      4049           4             
#> # ℹ 35 more variables: veteran <chr>, goals_against <dbl>,
#> #   shootout_goals_against <chr>, shootout_saves <chr>,
#> #   goals_against_average <dbl>, games_played <dbl>, minutes_played <dbl>,
#> #   seconds_played <chr>, shots_against <dbl>, wins <dbl>, losses <dbl>,
#> #   ties <chr>, ot_losses <chr>, total_losses <chr>, shootout_losses <chr>,
#> #   ot <chr>, sosavepct <chr>, shootout_shots <chr>, shutouts <dbl>,
#> #   saves <dbl>, savepct <dbl>, goals <dbl>, assists <dbl>, points <dbl>, …
# }
```
