# **NHL Team Summary (Season Range)**

Aggregator helper that calls
[`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md)
with `report_type = "summary"` for every season in
`[start_season, end_season]` and concatenates the results into a single
tidy frame. Mirrors the `Stats.team_summary` convenience helper from the
`nhl-api-py` Python client.

## Usage

``` r
nhl_team_summary_range(start_season, end_season, game_type = 2, limit = 50)
```

## Arguments

- start_season:

  Integer four-digit *end year* of the first season (e.g. `2022` for the
  2021-22 season).

- end_season:

  Integer four-digit *end year* of the final season (inclusive). Must be
  `>= start_season`.

- game_type:

  Integer game type: `2` = regular season (default), `3` = playoffs.

- limit:

  Integer maximum number of rows per season request. Defaults to `50` to
  match
  [`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md).

## Value

A `fastRhockey_data` / `data.frame` of concatenated team summary stats
with an added `season` column marking the season each row came from.
Returns `NULL` on outer failure.

## Examples

``` r
# \donttest{
  try(nhl_team_summary_range(start_season = 2023, end_season = 2024))
#> ── NHL Team Summary Range ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:06 UTC
#> # A tibble: 64 × 26
#>    faceoff_win_pct games_played goals_against goals_against_per_game goals_for
#>              <dbl>        <int>         <int>                  <dbl>     <int>
#>  1           0.545           82           174                   2.12       301
#>  2           0.529           82           210                   2.56       262
#>  3           0.513           82           222                   2.71       289
#>  4           0.531           82           220                   2.68       278
#>  5           0.523           82           225                   2.74       267
#>  6           0.508           82           256                   3.12       325
#>  7           0.467           82           223                   2.72       274
#>  8           0.548           82           215                   2.62       281
#>  9           0.491           82           216                   2.63       273
#> 10           0.533           82           254                   3.10       274
#> # ℹ 54 more rows
#> # ℹ 21 more variables: goals_for_per_game <dbl>, losses <int>, ot_losses <int>,
#> #   penalty_kill_net_pct <dbl>, penalty_kill_pct <dbl>, point_pct <dbl>,
#> #   points <int>, power_play_net_pct <dbl>, power_play_pct <dbl>,
#> #   regulation_and_ot_wins <int>, season_id <int>,
#> #   shots_against_per_game <dbl>, shots_for_per_game <dbl>,
#> #   team_full_name <chr>, team_id <int>, team_shutouts <int>, ties <lgl>, …
# }
```
