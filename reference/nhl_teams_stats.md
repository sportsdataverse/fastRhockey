# **NHL Teams Stats**

Returns NHL team player-level stats (skaters and goalies) for a given
team abbreviation and season. Uses the new NHL API club-stats endpoint
(`api-web.nhle.com`).

**Breaking change:** The old `team_id` (integer) parameter has been
replaced by `team_abbr` (3-letter string). The `season` parameter now
accepts a 4-digit year (e.g., 2024 for the 2024-25 season).

## Usage

``` r
nhl_teams_stats(team_abbr, season = NULL, game_type = 2)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")

- season:

  Integer 4-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns current season stats.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

## Value

Returns a data frame with per-player stats for the team, including both
skaters and goalies identified by a `player_type` column.

## Examples

``` r
# \donttest{
  try(nhl_teams_stats(team_abbr = "TBL"))
#> ── NHL Teams Stats Information from NHL.com ─────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 16:15:53 UTC
#> # A tibble: 23 × 40
#>    player_id headshot position_code games_played goals assists points plus_minus
#>        <int> <chr>    <chr>                <int> <int>   <int>  <int>      <int>
#>  1   8470621 https:/… R                        7     0       0      0         -2
#>  2   8474151 https:/… D                        7     0       1      1          2
#>  3   8476453 https:/… R                        7     1       5      6          2
#>  4   8476826 https:/… C                        7     0       1      1          0
#>  5   8476878 https:/… C                        7     0       0      0         -2
#>  6   8477149 https:/… R                        2     0       0      0          0
#>  7   8477404 https:/… C                        7     2       6      8          3
#>  8   8477416 https:/… R                        4     0       0      0         -3
#>  9   8477426 https:/… L                        6     0       0      0         -2
#> 10   8478010 https:/… C                        7     1       0      1         -1
#> # ℹ 13 more rows
#> # ℹ 32 more variables: penalty_minutes <int>, power_play_goals <int>,
#> #   shorthanded_goals <int>, game_winning_goals <int>, overtime_goals <int>,
#> #   shots <int>, shooting_pctg <dbl>, avg_time_on_ice_per_game <dbl>,
#> #   avg_shifts_per_game <dbl>, faceoff_win_pctg <dbl>,
#> #   first_name_default <chr>, last_name_default <chr>, last_name_cs <chr>,
#> #   last_name_fi <chr>, last_name_sk <chr>, player_type <chr>, …
# }
```
