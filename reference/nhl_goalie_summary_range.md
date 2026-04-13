# **NHL Goalie Summary (Season Range)**

Aggregator helper that calls
[`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.md)
with `report_type = "summary"` for every season in
`[start_season, end_season]` and concatenates the results into a single
tidy frame. Mirrors the `Stats.goalie_stats_summary` convenience helper
from the `nhl-api-py` Python client.

## Usage

``` r
nhl_goalie_summary_range(start_season, end_season, game_type = 2, limit = 50)
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
  [`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.md).

## Value

A `fastRhockey_data` / `data.frame` of concatenated goalie summary stats
with an added `season` column marking the season each row came from.
Returns `NULL` on outer failure.

## Examples

``` r
# \donttest{
  try(nhl_goalie_summary_range(start_season = 2023, end_season = 2024))
#> ── NHL Goalie Summary Range ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:20 UTC
#> # A tibble: 100 × 24
#>    assists games_played games_started goalie_full_name   goals goals_against
#>      <int>        <int>         <int> <chr>              <int>         <int>
#>  1       0           49            48 Linus Ullmark          1            91
#>  2       1           62            62 Alexandar Georgiev     0           156
#>  3       1           58            58 Igor Shesterkin        0           144
#>  4       1           62            61 Jake Oettinger         0           144
#>  5       0           64            64 Connor Hellebuyck      0           157
#>  6       2           60            60 Andrei Vasilevskiy     0           159
#>  7       3           52            48 Vitek Vanecek          0           119
#>  8       1           64            63 Juuse Saros            0           171
#>  9       1           62            60 Ilya Sorokin           0           140
#> 10       2           50            48 Stuart Skinner         0           133
#> # ℹ 90 more rows
#> # ℹ 18 more variables: goals_against_average <dbl>, last_name <chr>,
#> #   losses <int>, ot_losses <int>, penalty_minutes <int>, player_id <int>,
#> #   points <int>, save_pct <dbl>, saves <int>, season_id <int>,
#> #   shoots_catches <chr>, shots_against <int>, shutouts <int>,
#> #   team_abbrevs <chr>, ties <lgl>, time_on_ice <int>, wins <int>, season <chr>
# }
```
