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
#> ℹ Data updated: 2026-11-06 07:49:58 UTC
#> # A tibble: 32 × 25
#>    faceoff_win_pct games_played goals_against goals_against_per_game goals_for
#>              <dbl>        <int>         <int>                  <dbl>     <int>
#>  1           0.495           18            42                   2.33        69
#>  2           0.550           17            40                   2.35        63
#>  3           0.490           17            38                   2.24        56
#>  4           0.497           17            51                   3           70
#>  5           0.506           18            49                   2.72        59
#>  6           0.516           16            51                   3.19        55
#>  7           0.489           15            45                   3           56
#>  8           0.502           16            47                   2.94        56
#>  9           0.533           17            59                   3.47        61
#> 10           0.536           17            52                   3.06        52
#> # ℹ 22 more rows
#> # ℹ 20 more variables: goals_for_per_game <dbl>, losses <int>, ot_losses <int>,
#> #   penalty_kill_net_pct <dbl>, penalty_kill_pct <dbl>, point_pct <dbl>,
#> #   points <int>, power_play_net_pct <dbl>, power_play_pct <dbl>,
#> #   regulation_and_ot_wins <int>, season_id <int>,
#> #   shots_against_per_game <dbl>, shots_for_per_game <dbl>,
#> #   team_full_name <chr>, team_id <int>, team_shutouts <int>, ties <lgl>, …
# }
```
