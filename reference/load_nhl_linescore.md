# **Load fastRhockey NHL linescore**

Helper that loads multiple seasons of NHL linescore data (per-game
home/away goals, shots, shootout flag) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_linescore(
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

A data frame (`fastRhockey_data`) with one row per game and the
following columns:

|                |           |                                       |
|----------------|-----------|---------------------------------------|
| col_name       | types     | description                           |
| game_id        | integer   | Unique game identifier.               |
| home_team_id   | integer   | Unique home team identifier.          |
| home_team_abbr | character | Home team abbreviation.               |
| home_goals     | integer   | Home team goals.                      |
| home_shots     | integer   | Home team shots on goal.              |
| away_team_id   | integer   | Unique away team identifier.          |
| away_team_abbr | character | Away team abbreviation.               |
| away_goals     | integer   | Away team goals.                      |
| away_shots     | integer   | Away team shots on goal.              |
| has_shootout   | logical   | Whether the game ended in a shootout. |

## Examples

``` r
# \donttest{
  try(load_nhl_linescore(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 1,394 × 10
#>       game_id home_team_id home_team_abbr home_goals home_shots away_team_id
#>         <int>        <int> <chr>               <int>      <int>        <int>
#>  1 2025020001           13 FLA                     3         37           16
#>  2 2025020002            3 NYR                     0         25            5
#>  3 2025020003           26 LAK                     1         25           21
#>  4 2025020004           10 TOR                     5         27            8
#>  5 2025020005           15 WSH                     1         36            6
#>  6 2025020006           22 EDM                     3         35           20
#>  7 2025020007           54 VGK                     5         35           26
#>  8 2025020008            6 BOS                     4         33           16
#>  9 2025020009            7 BUF                     0         37            3
#> 10 2025020010           17 DET                     1         31            8
#> # ℹ 1,384 more rows
#> # ℹ 4 more variables: away_team_abbr <chr>, away_goals <int>, away_shots <int>,
#> #   has_shootout <lgl>
# }
```
