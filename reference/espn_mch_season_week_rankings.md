# **Get ESPN MCH / WCH Weekly Rankings (core-v2)**

**Get ESPN MCH / WCH Weekly Rankings (core-v2)**

## Usage

``` r
espn_mch_season_week_rankings(season, season_type, week, ...)

espn_wch_season_week_rankings(season, season_type, week, ...)
```

## Arguments

- season:

  Season END year (e.g. `2026`).

- season_type:

  Season type (1 = preseason, 2 = regular season, 3 = postseason).

- week:

  Week number within the season type.

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per ranking item:

|             |           |                                    |
|-------------|-----------|------------------------------------|
| col_name    | types     | description                        |
| ref         | character | `$ref` URL of the ranking object.  |
| ranking_id  | character | Trailing id of the ranking `$ref`. |
| season      | integer   | Season END year (echoed from arg). |
| season_type | integer   | Season type (echoed from arg).     |
| week        | integer   | Week (echoed from arg).            |

## Details

League slug `mens-college-hockey`. Core-v2 `weeks/{week}/rankings`
collection; `$ref`-only items become `ref` + `ranking_id`, inline items
are flattened. Mirrors sportsdataverse-py
`espn_mch_season_week_rankings()`.

`espn_wch_season_week_rankings()` is the same wrapper for women's
college hockey (league slug `womens-college-hockey`): same arguments,
same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_week_rankings(season = 2026, season_type = 2, week = 3))
#> ── MENS-COLLEGE-HOCKEY Week Rankings data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-11-13 21:26:37 UTC
#> # A tibble: 2 × 5
#>   ref                                        ranking_id season season_type  week
#>   <chr>                                      <chr>       <int>       <int> <int>
#> 1 http://sports.core.api.espn.com/v2/sports… 57           2026           2     3
#> 2 http://sports.core.api.espn.com/v2/sports… 10624        2026           2     3
# }
# \donttest{
  try(espn_wch_season_week_rankings(season = 2026, season_type = 2, week = 3))
#> ── WOMENS-COLLEGE-HOCKEY Week Rankings data from ESPN core-v2 ──────────────────
#> ℹ Data updated: 2026-11-13 21:26:37 UTC
#> # A tibble: 2 × 5
#>   ref                                        ranking_id season season_type  week
#>   <chr>                                      <chr>       <int>       <int> <int>
#> 1 http://sports.core.api.espn.com/v2/sports… 95           2026           2     3
#> 2 http://sports.core.api.espn.com/v2/sports… 10604        2026           2     3
# }
```
