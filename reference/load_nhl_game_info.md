# **Load fastRhockey NHL game info**

Helper that loads multiple seasons of NHL game-level metadata (venue,
attendance, officials, etc.) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_game_info(
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

|                |           |                                                  |
|----------------|-----------|--------------------------------------------------|
| col_name       | types     | description                                      |
| game_id        | integer   | Unique game identifier.                          |
| season         | integer   | Season (concluding year, YYYY).                  |
| game_type      | character | Game type the row belongs to (regular/playoffs). |
| game_date      | character | Game date.                                       |
| venue          | character | Venue where the game was played.                 |
| home_team_abbr | character | Home team abbreviation.                          |
| away_team_abbr | character | Away team abbreviation.                          |
| home_score     | integer   | Home team final score.                           |
| away_score     | integer   | Away team final score.                           |
| game_state     | character | Game status (e.g. FINAL/FUT).                    |

## Examples

``` r
# \donttest{
  try(load_nhl_game_info(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 1,390 × 10
#>       game_id   season game_type game_date  venue  home_team_abbr away_team_abbr
#>         <int>    <int> <chr>     <chr>      <chr>  <chr>          <chr>         
#>  1 2025020001 20252026 R         2025-10-07 Amera… FLA            CHI           
#>  2 2025020002 20252026 R         2025-10-07 Madis… NYR            PIT           
#>  3 2025020003 20252026 R         2025-10-07 Crypt… LAK            COL           
#>  4 2025020004 20252026 R         2025-10-08 Scoti… TOR            MTL           
#>  5 2025020005 20252026 R         2025-10-08 Capit… WSH            BOS           
#>  6 2025020006 20252026 R         2025-10-08 Roger… EDM            CGY           
#>  7 2025020007 20252026 R         2025-10-08 T-Mob… VGK            LAK           
#>  8 2025020008 20252026 R         2025-10-09 TD Ga… BOS            CHI           
#>  9 2025020009 20252026 R         2025-10-09 KeyBa… BUF            NYR           
#> 10 2025020010 20252026 R         2025-10-09 Littl… DET            MTL           
#> # ℹ 1,380 more rows
#> # ℹ 3 more variables: home_score <int>, away_score <int>, game_state <chr>
# }
```
