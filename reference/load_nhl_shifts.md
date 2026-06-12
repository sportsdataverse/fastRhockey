# **Load fastRhockey NHL shifts**

Helper that loads multiple seasons of NHL shift-by-shift data from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_shifts(
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

A data frame (`fastRhockey_data`) with one row per shift change and the
following columns:

|                        |           |                                        |
|------------------------|-----------|----------------------------------------|
| col_name               | types     | description                            |
| event_team             | character | Team associated with the shift change. |
| period                 | integer   | Period number.                         |
| period_time            | character | Elapsed time in the period (MM:SS).    |
| period_seconds         | integer   | Elapsed seconds in the period.         |
| game_seconds           | integer   | Elapsed seconds in the game.           |
| num_on                 | integer   | Number of players coming on.           |
| players_on             | character | Names of players coming on.            |
| ids_on                 | character | Player ids coming on.                  |
| num_off                | integer   | Number of players going off.           |
| players_off            | character | Names of players going off.            |
| ids_off                | character | Player ids going off.                  |
| event                  | character | Event description label.               |
| event_type             | character | Standardized event type code.          |
| game_seconds_remaining | integer   | Seconds remaining in regulation.       |

## Examples

``` r
# \donttest{
  try(load_nhl_shifts(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 655,455 × 17
#>    event_team   period period_time period_seconds game_seconds num_on players_on
#>    <chr>         <int> <chr>                <int>        <int>  <int> <chr>     
#>  1 Chicago Bla…      1 00:00                    0            0      6 Connor Mu…
#>  2 Florida Pan…      1 00:00                    0            0      6 Brad Marc…
#>  3 Chicago Bla…      1 00:34                   34           34      4 Teuvo Ter…
#>  4 Florida Pan…      1 00:34                   34           34      5 Jeff Petr…
#>  5 Chicago Bla…      1 00:39                   39           39      1 Frank Naz…
#>  6 Chicago Bla…      1 00:57                   57           57      1 Matthew G…
#>  7 Florida Pan…      1 01:02                   62           62      2 Aaron Ekb…
#>  8 Florida Pan…      1 01:05                   65           65      1 Mackie Sa…
#>  9 Chicago Bla…      1 01:06                   66           66      1 Ryan Dona…
#> 10 Florida Pan…      1 01:06                   66           66      2 Evan Rodr…
#> # ℹ 655,445 more rows
#> # ℹ 10 more variables: ids_on <chr>, num_off <int>, players_off <chr>,
#> #   ids_off <chr>, event <chr>, event_type <chr>, game_seconds_remaining <int>,
#> #   game_id <int>, season <int>, game_date <chr>
# }
```
