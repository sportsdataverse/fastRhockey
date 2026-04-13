# **Load fastRhockey NHL shootout summary**

Helper that loads multiple seasons of NHL shootout-attempt data (one row
per shooter per shootout, with result + goalie) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases. Only games that ended in a shootout contribute rows.

## Usage

``` r
load_nhl_shootout(
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

A data frame of class `fastRhockey_data`, with one row per shootout
attempt. Only games that ended in a shootout contribute rows. Columns
include:

|               |                                                      |
|---------------|------------------------------------------------------|
| column        | description                                          |
| `game_id`     | NHL game id                                          |
| `season`      | Season end year (e.g. 2026 for 2025-26)              |
| `game_date`   | ISO date of the game                                 |
| `sequence`    | Order of the attempt within the shootout (1, 2, ...) |
| `team_abbrev` | Three-letter abbreviation of the shooting team       |
| `player_id`   | NHL player id of the shooter                         |
| `first_name`  | Shooter first name                                   |
| `last_name`   | Shooter last name                                    |
| `shot_type`   | e.g. `"wrist"`, `"snap"`, `"backhand"`               |
| `result`      | `"goal"`, `"save"`, or `"miss"`                      |
| `game_winner` | `TRUE` for the decisive attempt, `FALSE` otherwise   |

## Examples

``` r
# \donttest{
  try(load_nhl_shootout(2026))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_shootout/shootout_summary_2026.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_shootout/shootout_summary_2026.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
