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
#> ── NHL goalie boxscores ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-09 07:37:00 UTC
#> # A tibble: 5,605 × 24
#>    home_away team_id team_abbrev player_id player_name     sweater_number
#>    <chr>       <int> <chr>           <int> <chr>                    <int>
#>  1 away            5 PIT           8479193 C. DeSmith                   1
#>  2 away            5 PIT           8477465 T. Jarry                    35
#>  3 home           14 TBL           8470880 B. Elliott                   1
#>  4 home           14 TBL           8476883 A. Vasilevskiy              88
#>  5 away           55 SEA           8475831 P. Grubauer                 31
#>  6 away           55 SEA           8476904 C. Driedger                 60
#>  7 home           54 VGK           8476316 L. Brossoit                 39
#>  8 home           54 VGK           8475215 R. Lehner                   90
#>  9 away            8 MTL           8474596 J. Allen                    34
#> 10 away            8 MTL           8478470 S. Montembeault             35
#> # ℹ 5,595 more rows
#> # ℹ 18 more variables: even_strength_shots_against <chr>,
#> #   power_play_shots_against <chr>, shorthanded_shots_against <chr>,
#> #   save_shots_against <chr>, save_pctg <dbl>,
#> #   even_strength_goals_against <int>, power_play_goals_against <int>,
#> #   shorthanded_goals_against <int>, pim <int>, goals_against <int>, toi <chr>,
#> #   starter <lgl>, decision <chr>, shots_against <int>, saves <int>, …
# }
```
