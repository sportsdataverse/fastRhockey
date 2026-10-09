# **Load NHL groups (conferences and divisions) from the SportsDataverse data repo**

One row per NHL group lineage (the league, its conferences and its
divisions), keyed by the SportsDataverse group id (e.g.
`nhl:metropolitan`). Published to the `nhl_groups` release tag on the
[sportsdataverse-data
releases](https://github.com/sportsdataverse/sportsdataverse-data/releases)
by
[sdv-reference-data](https://github.com/sportsdataverse/sdv-reference-data).
Seasons are keyed by their *end year* (2025 = the 2024-25 season).

## Usage

``` r
load_nhl_groups(..., dbConnection = NULL, tablename = NULL)
```

## Arguments

- ...:

  Additional arguments passed to an underlying function that writes the
  data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| league | character | League key. |
| group_id | character | SportsDataverse group id, `{league}:{slug}`; one id per lineage across renames. |
| level | character | Group level: `league`, `conference` or `division`. |
| first_season | integer | First season (end year) with at least one member. |
| last_season | integer | Last season (end year) with at least one member. |
| notes | character | Lineage decisions and source caveats. |

## Examples

``` r
# \donttest{
  try(load_nhl_groups())
#> ── NHL groups from the SportsDataverse data repo ────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-09 03:19:41 UTC
#> # A tibble: 21 × 6
#>    league group_id            level      first_season last_season notes         
#>    <chr>  <chr>               <chr>             <int>       <int> <chr>         
#>  1 nhl    nhl:adams-northeast division           1975        2013 lineage: Adam…
#>  2 nhl    nhl:american        division           1927        1938 1926-27 to 19…
#>  3 nhl    nhl:atlantic        division           2014        2026 lineage: the …
#>  4 nhl    nhl:canadian        division           1927        1938 the league's …
#>  5 nhl    nhl:central         division           2014        2026 new in 2013-1…
#>  6 nhl    nhl:central-2021    division           2021        2021 2020-21 only …
#>  7 nhl    nhl:east-1968       division           1968        1974 the 1967 expa…
#>  8 nhl    nhl:east-2021       division           2021        2021 2020-21 only:…
#>  9 nhl    nhl:eastern         conference         1975        2026 lineage: the …
#> 10 nhl    nhl:metropolitan    division           2014        2026 new in 2013-1…
#> # ℹ 11 more rows
# }
```
