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
#> ℹ Data updated: 2026-11-25 00:43:00 UTC
#> # A tibble: 32 × 25
#>    faceoff_win_pct games_played goals_against goals_against_per_game goals_for
#>              <dbl>        <int>         <int>                  <dbl>     <int>
#>  1           0.468           26            57                   2.19        95
#>  2           0.504           24            54                   2.25       100
#>  3           0.494           26            74                   2.85       100
#>  4           0.545           25            71                   2.84        79
#>  5           0.511           25            72                   2.88        83
#>  6           0.492           23            67                   2.91        84
#>  7           0.531           25            73                   2.92        76
#>  8           0.566           23            54                   2.35        63
#>  9           0.483           24            75                   3.12        90
#> 10           0.553           23            61                   2.65        82
#> # ℹ 22 more rows
#> # ℹ 20 more variables: goals_for_per_game <dbl>, losses <int>, ot_losses <int>,
#> #   penalty_kill_net_pct <dbl>, penalty_kill_pct <dbl>, point_pct <dbl>,
#> #   points <int>, power_play_net_pct <dbl>, power_play_pct <dbl>,
#> #   regulation_and_ot_wins <int>, season_id <int>,
#> #   shots_against_per_game <dbl>, shots_for_per_game <dbl>,
#> #   team_full_name <chr>, team_id <int>, team_shutouts <int>, ties <lgl>, …
# }
```
