# **Get ESPN NHL Season Week Games (core-v2)**

**Get ESPN NHL Season Week Games (core-v2)**

## Usage

``` r
espn_nhl_season_week_games(
  season = most_recent_nhl_season(),
  season_type = 2,
  week,
  limit = 100,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season.
  Defaults to `2`.

- week:

  Week number (integer). Derive valid week numbers from
  [`espn_nhl_season_weeks()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_weeks.md).

- limit:

  Maximum number of events to return (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game (event) in the week,
or an empty tibble when the endpoint is unavailable. **Note**: NHL does
not use weeks in ESPN core-v2 - HTTP 404 is expected:

|             |           |                                           |
|-------------|-----------|-------------------------------------------|
| col_name    | types     | description                               |
| ref         | character | `$ref` URL for the event object.          |
| event_id    | character | ESPN event id parsed from the `$ref` URL. |
| season      | integer   | Season year (echoed from arg).            |
| season_type | integer   | Season type code (echoed from arg).       |
| week        | integer   | Week number (echoed from arg).            |
| count       | integer   | Total events in the collection.           |
| page_count  | integer   | Total pages in the collection.            |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
  try(espn_nhl_season_week_games(season = 2026, season_type = 2, week = 1))
#> ── NHL Season Week Games data from ESPN core-v2 ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:53 UTC
#> # A tibble: 48 × 7
#>    ref                        event_id season season_type  week count page_count
#>    <chr>                      <chr>     <int>       <int> <int> <int>      <int>
#>  1 http://sports.core.api.es… 4018017…   2026           2     1    48          1
#>  2 http://sports.core.api.es… 4018017…   2026           2     1    48          1
#>  3 http://sports.core.api.es… 4018018…   2026           2     1    48          1
#>  4 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#>  5 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#>  6 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#>  7 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#>  8 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#>  9 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#> 10 http://sports.core.api.es… 4018023…   2026           2     1    48          1
#> # ℹ 38 more rows
# }
```
