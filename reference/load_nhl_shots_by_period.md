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

A data frame of class `fastRhockey_data`, with one row per team per
period per game. Columns include:

|                 |                                         |
|-----------------|-----------------------------------------|
| column          | description                             |
| `game_id`       | NHL game id                             |
| `season`        | Season end year (e.g. 2026 for 2025-26) |
| `game_date`     | ISO date of the game                    |
| `home_away`     | `"home"` or `"away"`                    |
| `period_number` | 1-3 for regulation, 4+ for OT/SO        |
| `period_type`   | `"REG"`, `"OT"`, or `"SO"`              |
| `shots`         | Shots on goal in the period             |

## Examples

``` r
# \donttest{
  try(load_nhl_shots_by_period(2026))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_shots_by_period/shots_by_period_2026.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_shots_by_period/shots_by_period_2026.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
