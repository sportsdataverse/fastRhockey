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
#> ── NHL three stars / decisions ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-22 23:49:22 UTC
#> # A tibble: 4,182 × 22
#>     star playerId teamAbbrev headshot       name.default name.cs name.de name.fi
#>    <int>    <int> <chr>      <chr>          <chr>        <chr>   <chr>   <chr>  
#>  1     1  8480003 FLA        https://asset… J. Boqvist   NA      NA      NA     
#>  2     2  8482713 FLA        https://asset… M. Samoskev… NA      NA      NA     
#>  3     3  8478421 FLA        https://asset… A. Greer     NA      NA      NA     
#>  4     1  8481668 PIT        https://asset… A. Silovs    A. Šil… NA      NA     
#>  5     2  8479638 PIT        https://asset… J. Brazeau   NA      NA      NA     
#>  6     3  8478048 NYR        https://asset… I. Shesterk… I. Šes… NA      I. She…
#>  7     1  8480039 COL        https://asset… M. Necas     M. Neč… NA      NA     
#>  8     2  8477492 COL        https://asset… N. MacKinnon NA      NA      NA     
#>  9     3  8477476 COL        https://asset… A. Lehkonen  NA      NA      NA     
#> 10     1  8477939 TOR        https://asset… W. Nylander  NA      NA      NA     
#> # ℹ 4,172 more rows
#> # ℹ 14 more variables: name.sk <chr>, name.sv <chr>, sweaterNo <int>,
#> #   position <chr>, goals <int>, assists <int>, points <int>, game_id <int>,
#> #   winner_id <int>, winner_name <chr>, loser_id <int>, loser_name <chr>,
#> #   goalsAgainstAverage <dbl>, savePctg <dbl>
# }
```
