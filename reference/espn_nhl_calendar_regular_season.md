# **Get ESPN NHL Calendar Regular-Season Dates (core-v2)**

**Get ESPN NHL Calendar Regular-Season Dates (core-v2)**

## Usage

``` r
espn_nhl_calendar_regular_season(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per regular-season game date.
Uses the core-v2 `calendar/whitelist` endpoint, which is the working
equivalent of the sportsdataverse-py `calendar/regular-season` path (the
`regular-season` path returns HTTP 400/503 for NHL; `whitelist` returns
identical data with HTTP 200).

|            |           |                                       |
|------------|-----------|---------------------------------------|
| col_name   | types     | description                           |
| date       | character | Game date (ISO 8601 datetime string). |
| date_type  | character | Calendar date type (e.g. `"day"`).    |
| start_date | character | Season start date.                    |
| end_date   | character | Season end date.                      |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_calendar_regular_season())
#> ── NHL Calendar Regular-Season data from ESPN core-v2 ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:59 UTC
#> # A tibble: 193 × 4
#>    date              date_type start_date        end_date         
#>    <chr>             <chr>     <chr>             <chr>            
#>  1 2026-09-19T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  2 2026-09-20T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  3 2026-09-21T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  4 2026-09-22T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  5 2026-09-23T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  6 2026-09-24T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  7 2026-09-25T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  8 2026-09-26T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#>  9 2026-09-29T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#> 10 2026-09-30T07:00Z whitelist 2026-09-15T07:00Z 2027-07-01T06:59Z
#> # ℹ 183 more rows
# }
```
