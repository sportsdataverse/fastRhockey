# **Get ESPN NHL Calendar Offseason Dates (core-v2)**

**Get ESPN NHL Calendar Offseason Dates (core-v2)**

## Usage

``` r
espn_nhl_calendar_offseason(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per offseason date. May return
zero rows when the offseason calendar is empty for the requested season.

|            |           |                                  |
|------------|-----------|----------------------------------|
| col_name   | types     | description                      |
| date       | character | Date (ISO 8601 datetime string). |
| date_type  | character | Calendar date type.              |
| start_date | character | Season start date.               |
| end_date   | character | Season end date.                 |
| season     | integer   | Season year (echoed from arg).   |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_calendar_offseason(season = 2026))
#> data frame with 0 columns and 0 rows
# }
```
