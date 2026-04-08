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
#> ℹ Data updated: 2026-04-08 07:40:29 UTC
#> # A tibble: 33 × 40
#>    player_id headshot position_code games_played goals assists points plus_minus
#>        <int> <chr>    <chr>                <int> <int>   <int>  <int>      <int>
#>  1   8470621 https:/… R                       68    16      20     36         -2
#>  2   8474151 https:/… D                       45     6      13     19         12
#>  3   8475167 https:/… D                       33     1      16     17         -1
#>  4   8476453 https:/… R                       72    43      84    127         44
#>  5   8476826 https:/… C                       78     9      19     28         -4
#>  6   8476878 https:/… C                       71     9       9     18         -1
#>  7   8477149 https:/… R                       23     1       3      4          0
#>  8   8477404 https:/… C                       78    37      49     86         13
#>  9   8477416 https:/… R                       76    11      19     30        -15
#> 10   8477426 https:/… L                       47     7       8     15        -14
#> # ℹ 23 more rows
#> # ℹ 32 more variables: penalty_minutes <int>, power_play_goals <int>,
#> #   shorthanded_goals <int>, game_winning_goals <int>, overtime_goals <int>,
#> #   shots <int>, shooting_pctg <dbl>, avg_time_on_ice_per_game <dbl>,
#> #   avg_shifts_per_game <dbl>, faceoff_win_pctg <dbl>,
#> #   first_name_default <chr>, last_name_default <chr>, last_name_cs <chr>,
#> #   last_name_fi <chr>, last_name_sk <chr>, player_type <chr>, …
# }
```
