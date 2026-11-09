# **Get ESPN MCH / WCH Season Week (core-v2)**

**Get ESPN MCH / WCH Season Week (core-v2)**

## Usage

``` r
espn_mch_season_week(
  season = most_recent_nhl_season(),
  season_type = 2,
  week,
  ...
)

espn_wch_season_week(
  season = most_recent_nhl_season(),
  season_type = 2,
  week,
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

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with week metadata, or an empty
tibble when the endpoint is unavailable. **Note**: NHL does not use
weeks in ESPN core-v2 - HTTP 404 is the expected response:

|             |           |                                         |
|-------------|-----------|-----------------------------------------|
| col_name    | types     | description                             |
| season      | integer   | Season year (echoed from arg).          |
| season_type | integer   | Season type code (echoed from arg).     |
| week        | integer   | Week number (echoed from arg).          |
| number      | integer   | Week number as returned by the API.     |
| start_date  | character | Week start date (ISO 8601).             |
| end_date    | character | Week end date (ISO 8601).               |
| text        | character | Week display text.                      |
| week_ref    | character | `$ref` URL for this week object.        |
| events_ref  | character | `$ref` URL for the events in this week. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_week()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_week.md)
(columns inherited); a collection may be empty.

`espn_wch_season_week()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
  try(espn_mch_season_week(season = 2026, season_type = 2, week = 1))
#> ── MENS-COLLEGE-HOCKEY Season Week data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 22:50:46 UTC
#> # A tibble: 1 × 9
#>   season season_type  week number start_date  end_date text  week_ref events_ref
#>    <int>       <int> <int>  <int> <chr>       <chr>    <chr> <chr>    <chr>     
#> 1   2026           2     1      1 2025-09-01… 2025-10… Week… http://… NA        
# }
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN does not use weeks for hockey)
  try(espn_wch_season_week(season = 2026, season_type = 2, week = 1))
#> ── WOMENS-COLLEGE-HOCKEY Season Week data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-11-09 22:50:47 UTC
#> # A tibble: 1 × 9
#>   season season_type  week number start_date  end_date text  week_ref events_ref
#>    <int>       <int> <int>  <int> <chr>       <chr>    <chr> <chr>    <chr>     
#> 1   2026           2     1      1 2025-09-01… 2025-09… Week… http://… NA        
# }
```
