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
#> ℹ Data updated: 2026-11-09 17:22:56 UTC
#> # A tibble: 50 × 23
#>    assists games_played games_started goalie_full_name    goals goals_against
#>      <int>        <int>         <int> <chr>               <int>         <int>
#>  1       0           12            12 Pyotr Kochetkov         0            20
#>  2       0           16            16 Jesper Wallstedt        0            32
#>  3       0           13            13 Jeremy Swayman          0            29
#>  4       0           13            13 Jake Oettinger          0            26
#>  5       0           14            13 Mackenzie Blackwood     0            35
#>  6       0           10            10 Jacob Fowler            0            22
#>  7       0           13            13 Andrei Vasilevskiy      0            37
#>  8       0            8             8 Devon Levi              0            24
#>  9       0            8             8 Linus Ullmark           0            10
#> 10       0            9             9 Adin Hill               0            23
#> # ℹ 40 more rows
#> # ℹ 17 more variables: goals_against_average <dbl>, last_name <chr>,
#> #   losses <int>, ot_losses <int>, penalty_minutes <int>, player_id <int>,
#> #   points <int>, save_pct <dbl>, saves <int>, season_id <int>,
#> #   shoots_catches <chr>, shots_against <int>, shutouts <int>,
#> #   team_abbrevs <chr>, ties <lgl>, time_on_ice <int>, wins <int>
# }
```
