# **NHL Stats API — Goalie Stats**

Queries the NHL Stats REST API for goalie statistics.

## Usage

``` r
nhl_stats_goalies(
  report_type = "summary",
  season = NULL,
  game_type = 2,
  limit = 50,
  start = 0,
  sort = NULL,
  direction = "DESC",
  lang = "en"
)
```

## Arguments

- report_type:

  Character report type. Default "summary". Common types: "summary",
  "bios", "advanced", "daysrest", "penaltyShots", "savesByStrength",
  "shootout", "startedVsRelieved"

- season:

  Character season in "YYYYYYYY" format (e.g., "20242025"). If NULL,
  uses current season.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

- limit:

  Integer maximum number of results. Default 50.

- start:

  Integer start index for pagination. Default 0.

- sort:

  Character sort column. Default varies by report.

- direction:

  Character sort direction: "DESC" or "ASC". Default "DESC".

- lang:

  Character language code. Default "en".

## Value

Returns a data frame with goalie statistics.

## Examples

``` r
# \donttest{
  try(nhl_stats_goalies())
#> ── NHL Stats Goalies ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-30 14:45:29 UTC
#> # A tibble: 10 × 23
#>    assists games_played games_started goalie_full_name goals goals_against
#>      <int>        <int>         <int> <chr>            <int>         <int>
#>  1       0            1             1 Jacob Markstrom      0             0
#>  2       0            1             1 Carter Hart          0             2
#>  3       0            1             1 Jeremy Swayman       0             0
#>  4       0            1             1 Jakub Dobes          0             2
#>  5       0            1             1 Kevin Lankinen       0             5
#>  6       0            1             1 Sergei Bobrovsky     0             3
#>  7       0            1             1 Brandon Bussi        0             1
#>  8       0            1             1 Igor Shesterkin      0             2
#>  9       0            1             1 Spencer Knight       0             4
#> 10       0            1             1 Tristan Jarry        0             6
#> # ℹ 17 more variables: goals_against_average <dbl>, last_name <chr>,
#> #   losses <int>, ot_losses <int>, penalty_minutes <int>, player_id <int>,
#> #   points <int>, save_pct <dbl>, saves <int>, season_id <int>,
#> #   shoots_catches <chr>, shots_against <int>, shutouts <int>,
#> #   team_abbrevs <chr>, ties <lgl>, time_on_ice <int>, wins <int>
# }
```
