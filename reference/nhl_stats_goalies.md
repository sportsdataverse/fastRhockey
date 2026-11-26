# **NHL Stats API - Goalie Stats**

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
#> ── NHL Stats Goalies ────────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-26 22:31:31 UTC
#> # A tibble: 50 × 23
#>    assists games_played games_started goalie_full_name    goals goals_against
#>      <int>        <int>         <int> <chr>               <int>         <int>
#>  1       0           23            23 Jesper Wallstedt        0            48
#>  2       0           15            15 Pyotr Kochetkov         0            27
#>  3       0           19            19 Jake Oettinger          0            39
#>  4       0           14            14 Linus Ullmark           0            22
#>  5       0           18            17 Mackenzie Blackwood     0            42
#>  6       0           18            18 Andrei Vasilevskiy      0            46
#>  7       1           18            18 Jeremy Swayman          0            47
#>  8       1           14            14 Devon Levi              0            41
#>  9       0           16            16 Jacob Fowler            0            41
#> 10       0           17            17 Ilya Sorokin            0            41
#> # ℹ 40 more rows
#> # ℹ 17 more variables: goals_against_average <dbl>, last_name <chr>,
#> #   losses <int>, ot_losses <int>, penalty_minutes <int>, player_id <int>,
#> #   points <int>, save_pct <dbl>, saves <int>, season_id <int>,
#> #   shoots_catches <chr>, shots_against <int>, shutouts <int>,
#> #   team_abbrevs <chr>, ties <lgl>, time_on_ice <int>, wins <int>
# }
```
