# **Load fastRhockey PWHL shootout summaries**

Helper that loads multiple seasons of PWHL shootout-attempt data from
the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_shootout`. File naming convention:
`shootout_summary_{end_year}.rds`. Note that some seasons (notably the
2024 inaugural season) do not contain any shootout games.

## Usage

``` r
load_pwhl_shootout(
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

  Additional arguments (currently unused; kept for API symmetry).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the shootout data table within the database

## Value

A data frame (`fastRhockey_data`) with one row per shootout attempt and
the following columns:

|               |           |                                     |
|---------------|-----------|-------------------------------------|
| col_name      | types     | description                         |
| game_id       | integer   | Unique game identifier.             |
| round         | integer   | Shootout round number.              |
| team_side     | character | Shooting team side ("home"/"away"). |
| shooter_id    | integer   | Shooter player identifier.          |
| shooter_first | character | Shooter first name.                 |
| shooter_last  | character | Shooter last name.                  |
| goalie_id     | integer   | Opposing goalie identifier.         |
| goalie_first  | character | Opposing goalie first name.         |
| goalie_last   | character | Opposing goalie last name.          |
| is_goal       | integer   | Whether the attempt scored (1/0).   |

## Examples

``` r
# \donttest{
  try(load_pwhl_shootout(2024))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/pwhl_shootout/shootout_summary_2024.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/pwhl_shootout/shootout_summary_2024.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
