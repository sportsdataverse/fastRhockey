# **Get ESPN MCH / WCH Calendar**

**Get ESPN MCH / WCH Calendar**

## Usage

``` r
espn_mch_calendar(...)

espn_wch_calendar(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game date:

|  |  |  |
|----|----|----|
| col_name | type | description |
| date | character | Game date (ISO 8601 datetime string). |
| calendar_type | character | Calendar type (e.g. `"day"`). |
| calendar_start_date | character | Calendar season start date. |
| calendar_end_date | character | Calendar season end date. |
| calendar_is_whitelist | logical | Whether dates are a whitelist of game days. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_calendar()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_calendar.md);
columns verified against a captured MCH body.

`espn_wch_calendar()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_calendar())
#> ── MENS-COLLEGE-HOCKEY Calendar data from ESPN.com ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:15:45 UTC
#> # A tibble: 92 × 5
#>    date              calendar_type calendar_start_date calendar_end_date
#>    <chr>             <chr>         <chr>               <chr>            
#>  1 2026-10-02T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  2 2026-10-03T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  3 2026-10-04T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  4 2026-10-08T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  5 2026-10-09T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  6 2026-10-10T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  7 2026-10-11T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  8 2026-10-15T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#>  9 2026-10-16T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#> 10 2026-10-17T07:00Z day           2026-09-01T07:00Z   2027-05-01T06:59Z
#> # ℹ 82 more rows
#> # ℹ 1 more variable: calendar_is_whitelist <lgl>
# }
# \donttest{
  try(espn_wch_calendar())
#> ── WOMENS-COLLEGE-HOCKEY Calendar data from ESPN.com ────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:15:45 UTC
#> # A tibble: 84 × 5
#>    date              calendar_type calendar_start_date calendar_end_date
#>    <chr>             <chr>         <chr>               <chr>            
#>  1 2026-09-18T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  2 2026-09-19T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  3 2026-09-24T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  4 2026-09-25T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  5 2026-09-26T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  6 2026-10-01T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  7 2026-10-02T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  8 2026-10-03T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#>  9 2026-10-04T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#> 10 2026-10-06T07:00Z day           2026-09-01T07:00Z   2027-04-05T06:59Z
#> # ℹ 74 more rows
#> # ℹ 1 more variable: calendar_is_whitelist <lgl>
# }
```
