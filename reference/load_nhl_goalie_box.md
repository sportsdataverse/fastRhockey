# **Load fastRhockey NHL goalie box scores**

Helper that loads multiple seasons of pre-scraped NHL goalie box scores
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_goalie_box(
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

A data frame (`fastRhockey_data`) with the following columns:

|                             |           |                                      |
|-----------------------------|-----------|--------------------------------------|
| col_name                    | types     | description                          |
| home_away                   | character | Home or away indicator.              |
| team_id                     | integer   | Unique team identifier.              |
| team_abbrev                 | character | Team abbreviation/code.              |
| player_id                   | integer   | Unique player identifier.            |
| player_name                 | character | Player name.                         |
| sweater_number              | integer   | Jersey number.                       |
| even_strength_shots_against | character | Even-strength shots against.         |
| power_play_shots_against    | character | Power play shots against.            |
| shorthanded_shots_against   | character | Shorthanded shots against.           |
| save_shots_against          | character | Saves / shots against.               |
| save_pctg                   | numeric   | Save percentage.                     |
| even_strength_goals_against | integer   | Even-strength goals against.         |
| power_play_goals_against    | integer   | Power play goals against.            |
| shorthanded_goals_against   | integer   | Shorthanded goals against.           |
| pim                         | integer   | Penalty minutes.                     |
| goals_against               | integer   | Goals against.                       |
| toi                         | character | Time on ice.                         |
| starter                     | logical   | Whether the goalie started the game. |
| decision                    | character | Goalie decision (W/L/O).             |
| shots_against               | integer   | Shots faced.                         |
| saves                       | integer   | Saves made.                          |

## Examples

``` r
# \donttest{
  try(load_nhl_goalie_box(2022))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_goalie_boxscores/goalie_box_2022.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_goalie_boxscores/goalie_box_2022.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
