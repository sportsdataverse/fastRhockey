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

A data frame (`fastRhockey_data`) with one row per shootout attempt
(only games that ended in a shootout contribute rows) and the following
columns:

|             |           |                                                  |
|-------------|-----------|--------------------------------------------------|
| col_name    | types     | description                                      |
| game_id     | integer   | Unique game identifier.                          |
| season      | integer   | Season (concluding year, YYYY).                  |
| game_date   | character | Game date.                                       |
| sequence    | integer   | Order of the attempt within the shootout.        |
| team_abbrev | character | Abbreviation of the shooting team.               |
| player_id   | integer   | Player id of the shooter.                        |
| first_name  | character | Shooter first name.                              |
| last_name   | character | Shooter last name.                               |
| shot_type   | character | Type of shot taken (e.g. wrist, snap, backhand). |
| result      | character | Attempt result (goal/save/miss).                 |
| game_winner | logical   | Whether this attempt was the decisive one.       |

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
