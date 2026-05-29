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
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_shifts/shifts_2026.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_shifts/shifts_2026.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
