# **Load fastRhockey NHL skater box scores**

Helper that loads multiple seasons of pre-scraped NHL skater box scores
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_skater_box(
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

|                      |           |                                 |
|----------------------|-----------|---------------------------------|
| col_name             | types     | description                     |
| home_away            | character | Home or away indicator.         |
| team_id              | integer   | Unique team identifier.         |
| team_abbrev          | character | Team abbreviation/code.         |
| player_id            | integer   | Unique player identifier.       |
| player_name          | character | Player name.                    |
| sweater_number       | integer   | Jersey number.                  |
| position             | character | Player position.                |
| goals                | integer   | Goals scored.                   |
| assists              | integer   | Assists.                        |
| points               | integer   | Total points (goals + assists). |
| plus_minus           | integer   | Plus/minus rating.              |
| pim                  | integer   | Penalty minutes.                |
| hits                 | integer   | Hits.                           |
| power_play_goals     | integer   | Power play goals.               |
| shots_on_goal        | integer   | Shots on goal.                  |
| faceoff_winning_pctg | numeric   | Faceoff win percentage.         |
| toi                  | character | Time on ice.                    |
| blocked_shots        | integer   | Blocked shots.                  |
| shifts               | integer   | Number of shifts.               |
| giveaways            | integer   | Giveaways.                      |
| takeaways            | integer   | Takeaways.                      |

## Examples

``` r
# \donttest{
  try(load_nhl_skater_box(2022))
#> Warning: cannot open URL 'https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_skater_boxscores/skater_box_2022.rds': HTTP status was '404 Not Found'
#> Warning: Failed to readRDS from <https://github.com/sportsdataverse/sportsdataverse-data/releases/download/nhl_skater_boxscores/skater_box_2022.rds>
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 0 × 0
# }
```
