# **NHL Records - Season Attendance**

Returns historical NHL season attendance from the NHL Records API
(`https://records.nhl.com/site/api/attendance`).

## Usage

``` r
nhl_records_attendance(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of attendance records, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_attendance())
#> ── NHL Records Attendance ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:26 UTC
#> # A tibble: 79 × 5
#>       id playoff_attendance regular_attendance season_id total_attendance
#>    <int>              <int>              <int>     <int>            <int>
#>  1     1            1606364           21545024  20162017         23151388
#>  2     2            1684638           21615397  20152016         23300035
#>  3     3            1701336           21533419  20142015         23234755
#>  4     4            1775557           21758902  20132014         23534459
#>  5     5            1631683           12792723  20122013         14424406
#>  6     6            1592270           21468141  20112012         23060411
#>  7     7            1667624           21113926  20102011         22781550
#>  8     8            1702371           20996455  20092010         22698826
#>  9     9            1639602           21475223  20082009         23114825
#> 10    10            1587054           21236255  20072008         22823309
#> # ℹ 69 more rows
# }
```
