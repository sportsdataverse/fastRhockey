# **Get ESPN NHL Calendar**

**Get ESPN NHL Calendar**

## Usage

``` r
espn_nhl_calendar(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game date:

|  |  |  |
|----|----|----|
| col_name | types | description |
| date | character | Game date (ISO 8601 datetime string). |
| calendar_type | character | Calendar type (e.g. `"day"`). |
| calendar_start_date | character | Calendar season start date. |
| calendar_end_date | character | Calendar season end date. |
| calendar_is_whitelist | logical | Whether dates are a whitelist of game days. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_calendar())
#> ── NHL Calendar data from ESPN.com ──────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:48 UTC
#> # A tibble: 193 × 5
#>    date              calendar_type calendar_start_date calendar_end_date
#>    <chr>             <chr>         <chr>               <chr>            
#>  1 2026-09-19T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  2 2026-09-20T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  3 2026-09-21T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  4 2026-09-22T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  5 2026-09-23T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  6 2026-09-24T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  7 2026-09-25T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  8 2026-09-26T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#>  9 2026-09-29T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#> 10 2026-09-30T07:00Z day           2026-09-15T07:00Z   2027-07-01T06:59Z
#> # ℹ 183 more rows
#> # ℹ 1 more variable: calendar_is_whitelist <lgl>
# }
```
