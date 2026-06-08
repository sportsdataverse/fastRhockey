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

A data frame (`fastRhockey_data`) with the following columns:

|                        |           |                                        |
|------------------------|-----------|----------------------------------------|
| col_name               | types     | description                            |
| season_id              | numeric   | Season identifier.                     |
| season_name            | character | Season name.                           |
| shortname              | character | Short season name.                     |
| playoff                | character | Whether the row is playoff statistics. |
| career                 | character | Whether the row is career totals.      |
| max_start_date         | character | Latest game start date for the season. |
| veteran_status         | character | Player veteran status.                 |
| veteran                | character | Whether the player is a veteran.       |
| goals_against          | numeric   | Goals against (goalie).                |
| shootout_goals_against | character | Shootout goals against.                |
| shootout_saves         | character | Shootout saves made.                   |
| goals_against_average  | numeric   | Goals against average (goalie).        |
| games_played           | numeric   | Games played.                          |
| minutes_played         | numeric   | Minutes played.                        |
| seconds_played         | character | Seconds played.                        |
| shots_against          | numeric   | Shots faced (goalie).                  |
| wins                   | numeric   | Wins (goalie).                         |
| losses                 | numeric   | Losses (goalie).                       |
| ties                   | character | Ties (goalie).                         |
| ot_losses              | character | Overtime losses (goalie).              |
| total_losses           | character | Total losses (goalie).                 |
| shootout_losses        | character | Shootout losses (goalie).              |
| ot                     | character | Overtime results.                      |
| sosavepct              | character | Shootout save percentage.              |
| shootout_shots         | character | Shootout shots faced.                  |
| shutouts               | numeric   | Shutouts recorded (goalie).            |
| saves                  | numeric   | Saves made (goalie).                   |
| savepct                | numeric   | Save percentage (goalie).              |
| goals                  | numeric   | Goals scored.                          |
| assists                | numeric   | Assists.                               |
| points                 | numeric   | Total points (goals + assists).        |
| penalty_minutes        | numeric   | Penalty minutes.                       |
| team_name              | character | Team name.                             |
| team_code              | character | Team abbreviation.                     |
| team_city              | character | Team city.                             |
| team_nickname          | character | Team nickname.                         |
| team_id                | character | Unique team identifier.                |
| division               | character | Team division.                         |
| shotspct               | character | Shooting percentage.                   |
| gaa                    | character | Goals against average (goalie).        |
| player_id              | numeric   | Unique player identifier.              |
| stat_type              | character | Statistic type ("regular"/"playoff").  |

## Examples

``` r
# \donttest{
  try(pwhl_player_stats(player_id = 28))
#> ── PWHL Player Season Stats ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:45:19 UTC
#> # A tibble: 8 × 42
#>   season_id season_name   shortname playoff career max_start_date veteran_status
#>       <dbl> <chr>         <chr>     <chr>   <chr>  <chr>          <chr>         
#> 1         8 2025-26 Regu… 2025-26 … 0       1      2025-11-21     2             
#> 2         5 2024-25 Regu… 2024-25 … 0       1      2024-11-25     2             
#> 3         1 2024 Regular… 2024 Reg  0       1      2023-11-01     2             
#> 4        14 Total         Total     0       3      6072           6             
#> 5         9 2026 Playoffs 2026 Pla… 1       1      2026-04-28     2             
#> 6         6 2025 Playoffs 2025 Pla… 1       1      2025-05-06     2             
#> 7         3 2024 Playoffs 2024 Pla… 1       1      2024-05-06     2             
#> 8        18 Total         Total     1       3      6075           6             
#> # ℹ 35 more variables: veteran <chr>, goals_against <dbl>,
#> #   shootout_goals_against <chr>, shootout_saves <chr>,
#> #   goals_against_average <dbl>, games_played <dbl>, minutes_played <dbl>,
#> #   seconds_played <chr>, shots_against <dbl>, wins <dbl>, losses <dbl>,
#> #   ties <chr>, ot_losses <chr>, total_losses <chr>, shootout_losses <chr>,
#> #   ot <chr>, sosavepct <chr>, shootout_shots <chr>, shutouts <dbl>,
#> #   saves <dbl>, savepct <dbl>, goals <dbl>, assists <dbl>, points <dbl>, …
# }
```
