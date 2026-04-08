# **Load fastRhockey PWHL play-by-play**

Helper that loads multiple seasons of pre-scraped PWHL play-by-play data
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_pbp(
  seasons = most_recent_pwhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the play-by-play data table within the database

## Value

A data frame of class `fastRhockey_data`

## Examples

``` r
# \donttest{
  try(load_pwhl_pbp(2024))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/pwhl_pbp/play_by_play_2024.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/pwhl_pbp/play_by_play_2024.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
