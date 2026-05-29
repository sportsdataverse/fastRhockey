# **Load fastRhockey NHL three stars / decisions**

Helper that loads multiple seasons of NHL three-star selections and game
decisions from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_three_stars(
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

A data frame (`fastRhockey_data`) with one row per star selection (plus
the game winner/loser goalie decision) and the following columns:

|                     |           |                                         |
|---------------------|-----------|-----------------------------------------|
| col_name            | types     | description                             |
| star                | integer   | Star ranking (1, 2, or 3).              |
| playerId            | integer   | Player id of the selected star.         |
| teamAbbrev          | character | Team abbreviation of the selected star. |
| headshot            | character | URL to the player headshot image.       |
| name                | list      | Player display name (localized list).   |
| sweaterNo           | integer   | Jersey number.                          |
| position            | character | Player position.                        |
| goals               | integer   | Goals scored in the game (skaters).     |
| assists             | integer   | Assists in the game (skaters).          |
| points              | integer   | Total points in the game (skaters).     |
| goalsAgainstAverage | numeric   | Goals-against average (goalies).        |
| savePctg            | numeric   | Save percentage (goalies).              |
| game_id             | integer   | Unique game identifier.                 |
| winner_id           | integer   | Player id of the winning goalie.        |
| winner_name         | character | Name of the winning goalie.             |
| loser_id            | integer   | Player id of the losing goalie.         |
| loser_name          | character | Name of the losing goalie.              |

## Examples

``` r
# \donttest{
  try(load_nhl_three_stars(2026))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_three_stars/three_stars_2026.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_three_stars/three_stars_2026.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
