# **Get ESPN MCH / WCH Weekly Power Index (core-v2)**

**Get ESPN MCH / WCH Weekly Power Index (core-v2)**

## Usage

``` r
espn_mch_season_week_powerindex(season, season_type, week, limit = NULL, ...)

espn_wch_season_week_powerindex(season, season_type, week, limit = NULL, ...)
```

## Arguments

- season:

  Season END year (e.g. `2026`).

- season_type:

  Season type (1 = preseason, 2 = regular season, 3 = postseason).

- week:

  Week number within the season type.

- limit:

  Page size; `NULL` (the default) leaves ESPN's default.

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per team:

|  |  |  |
|----|----|----|
| col_name | types | description |
| team_id | integer | ESPN team identifier (parsed from the team `$ref`). |
| season | integer | Season END year. |
| last_updated | character | When ESPN computed the snapshot. |
| run_date_time_key | character | ESPN run key for the snapshot. |
| ... | varies | One column per predictive / efficiency field. |

## Details

League slug `mens-college-hockey`. One row per team, `predictives` and
`efficiencies` flattened to one column per field (a name colliding
across the two blocks is suffixed). A zero-row `data.frame` when
unpublished. Mirrors sportsdataverse-py
`espn_mch_season_week_powerindex()`.

`espn_wch_season_week_powerindex()` is the same wrapper for women's
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
  try(espn_mch_season_week_powerindex(season = 2026, season_type = 2, week = 3))
#> ✖ 2026-11-09 11:49:32.706846: Invalid arguments or no ESPN mens-college-hockey power index available!
#> ✖ Args: league = "mens-college-hockey", kind = "weekly", path = "seasons/2026/types/2/weeks/3/powerindex", query = list()
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_season_week_powerindex(season = 2026, season_type = 2, week = 3))
#> ✖ 2026-11-09 11:49:32.728794: Invalid arguments or no ESPN womens-college-hockey power index available!
#> ✖ Args: league = "womens-college-hockey", kind = "weekly", path = "seasons/2026/types/2/weeks/3/powerindex", query = list()
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
