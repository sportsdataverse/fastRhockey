# **Load fastRhockey NHL per-game rosters**

Helper that loads multiple seasons of NHL per-game roster data (one row
per player per game) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_game_rosters(
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

A data frame (`fastRhockey_data`) with one row per player per game and
the following columns:

|                |           |                           |
|----------------|-----------|---------------------------|
| col_name       | types     | description               |
| player_id      | integer   | Unique player identifier. |
| full_name      | character | Player full name.         |
| first_name     | character | Player first name.        |
| last_name      | character | Player last name.         |
| team_abbr      | character | Team abbreviation/code.   |
| team_id        | integer   | Unique team identifier.   |
| position_code  | character | Player position code.     |
| sweater_number | integer   | Jersey number.            |

## Examples

``` r
# \donttest{
  try(load_nhl_game_rosters(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 55,758 × 11
#>    player_id full_name      first_name last_name team_abbr team_id position_code
#>        <int> <chr>          <chr>      <chr>     <chr>       <int> <chr>        
#>  1   8473419 Brad Marchand  Brad       Marchand  FLA            13 L            
#>  2   8473422 Nick Foligno   Nick       Foligno   CHI            16 L            
#>  3   8473507 Jeff Petry     Jeff       Petry     FLA            13 D            
#>  4   8475179 Dmitry Kulikov Dmitry     Kulikov   FLA            13 D            
#>  5   8475683 Sergei Bobrov… Sergei     Bobrovsky FLA            13 G            
#>  6   8476473 Connor Murphy  Connor     Murphy    CHI            16 D            
#>  7   8476882 Teuvo Teravai… Teuvo      Teravain… CHI            16 C            
#>  8   8476891 Matt Grzelcyk  Matt       Grzelcyk  CHI            16 D            
#>  9   8477409 Carter Verhae… Carter     Verhaeghe FLA            13 C            
#> 10   8477444 Andre Burakov… Andre      Burakovs… CHI            16 L            
#> # ℹ 55,748 more rows
#> # ℹ 4 more variables: sweater_number <int>, game_id <int>, season <int>,
#> #   game_date <chr>
# }
```
