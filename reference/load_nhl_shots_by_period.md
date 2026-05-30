# **Load fastRhockey NHL shots by period**

Helper that loads multiple seasons of NHL per-period shot totals (one
row per team per period per game) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_shots_by_period(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with one row per team per period per
game and the following columns:

|               |           |                                               |
|---------------|-----------|-----------------------------------------------|
| col_name      | types     | description                                   |
| game_id       | integer   | Unique game identifier.                       |
| season        | integer   | Season (concluding year, YYYY).               |
| game_date     | character | Game date.                                    |
| home_away     | character | Home or away indicator.                       |
| period_number | integer   | Period number (1-3 regulation, 4+ for OT/SO). |
| period_type   | character | Period type (REG/OT/SO).                      |
| shots         | integer   | Shots on goal in the period.                  |

## Examples

``` r
# \donttest{
  try(load_nhl_shots_by_period(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 4,629 × 9
#>     away  home    game_id   season game_date  periodDescriptor.number
#>    <int> <int>      <int>    <int> <chr>                        <int>
#>  1     3    17 2025020001 20252026 2025-10-07                       1
#>  2    11     9 2025020001 20252026 2025-10-07                       2
#>  3     5    11 2025020001 20252026 2025-10-07                       3
#>  4     8     7 2025020002 20252026 2025-10-07                       1
#>  5     8    13 2025020002 20252026 2025-10-07                       2
#>  6    15     5 2025020002 20252026 2025-10-07                       3
#>  7     7     6 2025020003 20252026 2025-10-07                       1
#>  8    11     5 2025020003 20252026 2025-10-07                       2
#>  9     5    14 2025020003 20252026 2025-10-07                       3
#> 10    11     8 2025020004 20252026 2025-10-08                       1
#> # ℹ 4,619 more rows
#> # ℹ 3 more variables: periodDescriptor.periodType <chr>,
#> #   periodDescriptor.maxRegulationPeriods <int>,
#> #   periodDescriptor.otPeriods <int>
# }
```
