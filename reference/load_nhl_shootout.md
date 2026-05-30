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
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 929 × 32
#>     home  away sequence playerId shotType result headshot   gameWinner homeScore
#>    <int> <int>    <int>    <int> <chr>    <chr>  <chr>      <lgl>          <int>
#>  1     1     2       NA       NA NA       NA     NA         NA                NA
#>  2    NA    NA        1  8478402 wrist    save   https://a… FALSE              0
#>  3    NA    NA        2  8477993 snap     save   https://a… FALSE              0
#>  4    NA    NA        3  8477934 wrist    goal   https://a… FALSE              1
#>  5    NA    NA        4  8480028 snap     goal   https://a… FALSE              1
#>  6    NA    NA        5  8476454 snap     save   https://a… FALSE              1
#>  7    NA    NA        6  8478397 wrist    save   https://a… FALSE              1
#>  8    NA    NA        7  8485493 backhand save   https://a… FALSE              1
#>  9    NA    NA        8  8482679 wrist    save   https://a… FALSE              1
#> 10    NA    NA        9  8483455 wrist    save   https://a… FALSE              1
#> # ℹ 919 more rows
#> # ℹ 23 more variables: awayScore <int>, game_id <int>, season <int>,
#> #   game_date <chr>, discreteClip <dbl>, discreteClipFr <dbl>,
#> #   highlightClipSharingUrl <chr>, highlightClipSharingUrlFr <chr>,
#> #   highlightClip <dbl>, highlightClipFr <dbl>, teamAbbrev.default <chr>,
#> #   firstName.default <chr>, firstName.cs <chr>, firstName.fi <chr>,
#> #   firstName.sk <chr>, firstName.de <chr>, firstName.es <chr>, …
# }
```
