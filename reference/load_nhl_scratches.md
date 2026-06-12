# **Load fastRhockey NHL scratches**

Helper that loads multiple seasons of NHL healthy scratch data from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_scratches(
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

A data frame (`fastRhockey_data`) with one row per scratched player per
game and the following columns:

|           |           |                           |
|-----------|-----------|---------------------------|
| col_name  | types     | description               |
| id        | integer   | Unique player identifier. |
| firstName | character | Player first name.        |
| lastName  | character | Player last name.         |
| game_id   | integer   | Unique game identifier.   |

## Examples

``` r
# \donttest{
  try(load_nhl_scratches(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 9,395 × 4
#>         id firstName lastName      game_id
#>      <int> <chr>     <chr>           <int>
#>  1 8478881 Brett     Seney      2025020001
#>  2 8478981 Cavan     Fitzgerald 2025020001
#>  3 8481568 Alex      Vlasic     2025020001
#>  4 8481806 Louis     Crevier    2025020001
#>  5 8482117 Lukas     Reichel    2025020001
#>  6 8483611 Mitchell  Weeks      2025020001
#>  7 8479393 Noah      Gregor     2025020001
#>  8 8481655 Cole      Schwindt   2025020001
#>  9 8484304 Uvis      Balinskis  2025020001
#> 10 8476856 Mathew    Dumba      2025020002
#> # ℹ 9,385 more rows
# }
```
