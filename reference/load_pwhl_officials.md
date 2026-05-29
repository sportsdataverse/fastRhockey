# **Load fastRhockey PWHL officials**

Helper that loads multiple seasons of PWHL game-officials data
(referees, linespersons, scorekeepers) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_officials`. File naming convention:
`officials_{end_year}.rds`.

## Usage

``` r
load_pwhl_officials(
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

  The name of the officials data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|               |           |                                              |
|---------------|-----------|----------------------------------------------|
| col_name      | types     | description                                  |
| game_id       | integer   | Unique game identifier.                      |
| role          | character | Grouped official role (Referee/Linesperson). |
| first_name    | character | Official's first name.                       |
| last_name     | character | Official's last name.                        |
| jersey_number | integer   | Official's jersey number.                    |
| official_role | character | Official's specific role.                    |

## Examples

``` r
# \donttest{
  try(load_pwhl_officials(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 340 × 6
#>    game_id role        first_name last_name   jersey_number official_role
#>      <int> <chr>       <chr>      <chr>               <int> <chr>        
#>  1       2 Referee     Jake       Kamrass                 0 Referee      
#>  2       2 Referee     Lacey      Senuk                   0 Referee      
#>  3       2 Linesperson Antoine    Bujold-Roux            72 Linesperson  
#>  4       2 Linesperson Erin       Zach                   60 Linesperson  
#>  5       3 Referee     Elizabeth  Mantha                 15 Referee      
#>  6       3 Referee     Liam       Maaskant                0 Referee      
#>  7       3 Linesperson Jessica    Chartrand              26 Linesperson  
#>  8       3 Linesperson Anthony    Lapointe               78 Linesperson  
#>  9       4 Referee     Kelly      Cooke                  29 Referee      
#> 10       4 Referee     Jordan     Deckard                 0 Referee      
#> # ℹ 330 more rows
# }
```
