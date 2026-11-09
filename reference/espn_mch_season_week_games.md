# **Get ESPN MCH / WCH Season Week Games (core-v2)**

**Get ESPN MCH / WCH Season Week Games (core-v2)**

## Usage

``` r
espn_mch_season_week_games(
  season = most_recent_nhl_season(),
  season_type = 2,
  week,
  limit = 100,
  ...
)

espn_wch_season_week_games(
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
  [`espn_mch_season_weeks()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_weeks.md).

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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_week_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_week_games.md)
(columns inherited); a collection may be empty.

`espn_wch_season_week_games()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
  try(espn_mch_season_week_games(season = 2026, season_type = 2, week = 1))
#> ! ESPN MENS-COLLEGE-HOCKEY seasons/2026/types/2/weeks/1/events returned no items.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
  try(espn_wch_season_week_games(season = 2026, season_type = 2, week = 1))
#> ── WOMENS-COLLEGE-HOCKEY Season Week Games data from ESPN core-v2 ──────────────
#> ℹ Data updated: 2026-11-09 11:49:32 UTC
#> # A tibble: 8 × 7
#>   ref                         event_id season season_type  week count page_count
#>   <chr>                       <chr>     <int>       <int> <int> <int>      <int>
#> 1 http://sports.core.api.esp… 4018139…   2026           2     1     8          1
#> 2 http://sports.core.api.esp… 4018140…   2026           2     1     8          1
#> 3 http://sports.core.api.esp… 4018140…   2026           2     1     8          1
#> 4 http://sports.core.api.esp… 4018139…   2026           2     1     8          1
#> 5 http://sports.core.api.esp… 4018140…   2026           2     1     8          1
#> 6 http://sports.core.api.esp… 4018140…   2026           2     1     8          1
#> 7 http://sports.core.api.esp… 4018140…   2026           2     1     8          1
#> 8 http://sports.core.api.esp… 4018140…   2026           2     1     8          1
# }
```
