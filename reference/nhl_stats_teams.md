# **NHL Stats API - Team Stats**

Queries the NHL Stats REST API for team-level statistics.

## Usage

``` r
nhl_stats_teams(
  report_type = "summary",
  season = NULL,
  game_type = 2,
  limit = 50,
  start = 0,
  sort = "points",
  direction = "DESC",
  lang = "en"
)
```

## Arguments

- report_type:

  Character report type. Default "summary". Common types: "summary",
  "penalties", "penaltykill", "penaltykilltime", "powerplay",
  "powerplaytime", "realtime", "faceoffpercentages", "faceoffwins",
  "goalsForAgainst", "goalsBy Period", "daysrest", "outshootoutshotby",
  "percentages", "scoretrailfirst", "shootout", "shottype"

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

  Character sort column. Default "points".

- direction:

  Character sort direction: "DESC" or "ASC". Default "DESC".

- lang:

  Character language code. Default "en".

## Value

Returns a data frame with team statistics.

## Examples

``` r
# \donttest{
  try(nhl_stats_teams())
#> ── NHL Stats Teams ──────────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:40:28 UTC
#> # A tibble: 32 × 25
#>    faceoff_win_pct games_played goals_against goals_against_per_game goals_for
#>              <dbl>        <int>         <int>                  <dbl>     <int>
#>  1           0.501           19            44                   2.32        78
#>  2           0.488           19            43                   2.26        63
#>  3           0.556           18            41                   2.28        67
#>  4           0.5             18            53                   2.94        74
#>  5           0.508           19            50                   2.63        63
#>  6           0.497           18            51                   2.83        65
#>  7           0.521           17            53                   3.12        59
#>  8           0.492           17            52                   3.06        63
#>  9           0.531           18            54                   3           56
#> 10           0.491           18            55                   3.06        72
#> # ℹ 22 more rows
#> # ℹ 20 more variables: goals_for_per_game <dbl>, losses <int>, ot_losses <int>,
#> #   penalty_kill_net_pct <dbl>, penalty_kill_pct <dbl>, point_pct <dbl>,
#> #   points <int>, power_play_net_pct <dbl>, power_play_pct <dbl>,
#> #   regulation_and_ot_wins <int>, season_id <int>,
#> #   shots_against_per_game <dbl>, shots_for_per_game <dbl>,
#> #   team_full_name <chr>, team_id <int>, team_shutouts <int>, ties <lgl>, …
# }
```
