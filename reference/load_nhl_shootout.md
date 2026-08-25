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
#> ── NHL shootout summary ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-22 23:49:22 UTC
#> # A tibble: 929 × 32
#>     home  away    game_id  season game_date sequence playerId teamAbbrev.default
#>    <int> <int>      <int>   <int> <chr>        <int>    <int> <chr>             
#>  1     1     2 2025020006  2.03e7 2025-10-…       NA       NA NA                
#>  2    NA    NA 2025020006  2.03e7 2025-10-…        1  8478402 EDM               
#>  3    NA    NA 2025020006  2.03e7 2025-10-…        2  8477993 CGY               
#>  4    NA    NA 2025020006  2.03e7 2025-10-…        3  8477934 EDM               
#>  5    NA    NA 2025020006  2.03e7 2025-10-…        4  8480028 CGY               
#>  6    NA    NA 2025020006  2.03e7 2025-10-…        5  8476454 EDM               
#>  7    NA    NA 2025020006  2.03e7 2025-10-…        6  8478397 CGY               
#>  8    NA    NA 2025020006  2.03e7 2025-10-…        7  8485493 EDM               
#>  9    NA    NA 2025020006  2.03e7 2025-10-…        8  8482679 CGY               
#> 10    NA    NA 2025020006  2.03e7 2025-10-…        9  8483455 EDM               
#> # ℹ 919 more rows
#> # ℹ 24 more variables: firstName.default <chr>, firstName.cs <chr>,
#> #   firstName.de <chr>, firstName.es <chr>, firstName.fi <chr>,
#> #   firstName.sk <chr>, firstName.sv <chr>, lastName.default <chr>,
#> #   lastName.cs <chr>, lastName.fi <chr>, lastName.sk <chr>, lastName.sv <chr>,
#> #   shotType <chr>, result <chr>, headshot <chr>, gameWinner <lgl>,
#> #   homeScore <int>, awayScore <int>, discreteClip <dbl>, …
# }
```
