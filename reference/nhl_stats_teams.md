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
#> ℹ Data updated: 2026-11-02 06:06:53 UTC
#> # A tibble: 32 × 25
#>    faceoff_win_pct games_played goals_against goals_against_per_game goals_for
#>              <dbl>        <int>         <int>                  <dbl>     <int>
#>  1           0.505           16            35                   2.19        63
#>  2           0.488           15            43                   2.87        63
#>  3           0.489           16            36                   2.25        52
#>  4           0.547           15            36                   2.4         55
#>  5           0.516           14            42                   3           46
#>  6           0.505           15            44                   2.93        52
#>  7           0.528           16            51                   3.19        50
#>  8           0.475           13            42                   3.23        48
#>  9           0.501           14            40                   2.86        48
#> 10           0.528           16            52                   3.25        60
#> # ℹ 22 more rows
#> # ℹ 20 more variables: goals_for_per_game <dbl>, losses <int>, ot_losses <int>,
#> #   penalty_kill_net_pct <dbl>, penalty_kill_pct <dbl>, point_pct <dbl>,
#> #   points <int>, power_play_net_pct <dbl>, power_play_pct <dbl>,
#> #   regulation_and_ot_wins <int>, season_id <int>,
#> #   shots_against_per_game <dbl>, shots_for_per_game <dbl>,
#> #   team_full_name <chr>, team_id <int>, team_shutouts <int>, ties <lgl>, …
# }
```
