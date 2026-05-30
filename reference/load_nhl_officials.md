# **Load fastRhockey NHL officials**

Helper that loads multiple seasons of NHL on-ice officials (referees +
linesmen) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases. One row per official per game.

## Usage

``` r
load_nhl_officials(
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

A data frame (`fastRhockey_data`) with one row per official per game and
the following columns:

|           |           |                                      |
|-----------|-----------|--------------------------------------|
| col_name  | types     | description                          |
| game_id   | integer   | Unique game identifier.              |
| season    | integer   | Season (concluding year, YYYY).      |
| game_date | character | Game date.                           |
| role      | character | Official role (referee or linesman). |
| name      | character | Full name of the official.           |

## Examples

``` r
# \donttest{
  try(load_nhl_officials(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 5,546 × 5
#>    role     name                   game_id   season game_date 
#>    <chr>    <chr>                    <int>    <int> <chr>     
#>  1 referee  Kelly Sutherland    2025020001 20252026 2025-10-07
#>  2 referee  Eric Furlatt        2025020001 20252026 2025-10-07
#>  3 linesman Scott Cherrey       2025020001 20252026 2025-10-07
#>  4 linesman Matt MacPherson     2025020001 20252026 2025-10-07
#>  5 referee  Wes McCauley        2025020002 20252026 2025-10-07
#>  6 referee  Jake Brenk          2025020002 20252026 2025-10-07
#>  7 linesman Libor Suchanek      2025020002 20252026 2025-10-07
#>  8 linesman Devin Berg          2025020002 20252026 2025-10-07
#>  9 referee  Frederick L'Ecuyer  2025020003 20252026 2025-10-07
#> 10 referee  Thomas John Luxmore 2025020003 20252026 2025-10-07
#> # ℹ 5,536 more rows
# }
```
