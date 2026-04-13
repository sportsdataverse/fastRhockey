# **Load fastRhockey PWHL schedules**

Helper that loads multiple seasons of pre-scraped PWHL schedule data
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_schedule(
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

  Additional arguments passed to an underlying function that writes the
  season data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the schedule data table within the database

## Value

A data frame of class `fastRhockey_data`

## Examples

``` r
# \donttest{
  try(load_pwhl_schedule(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 85 × 21
#>    game_id season game_date   game_status home_team home_team_id away_team
#>    <chr>    <int> <chr>       <chr>       <chr>     <chr>        <chr>    
#>  1 84        2024 Wed, May 8  Final       Toronto   6            Minnesota
#>  2 98        2024 Wed, May 29 Final       Boston    1            Minnesota
#>  3 90        2024 Wed, May 15 Final OT2   Minnesota 2            Toronto  
#>  4 63        2024 Wed, May 1  Final       Toronto   6            Minnesota
#>  5 46        2024 Wed, Mar 6  Final       New York  4            Montreal 
#>  6 45        2024 Wed, Mar 6  Final       Toronto   6            Boston   
#>  7 53        2024 Wed, Mar 20 Final       New York  4            Ottawa   
#>  8 52        2024 Wed, Mar 20 Final       Toronto   6            Boston   
#>  9 50        2024 Wed, Mar 13 Final       Minnesota 2            Boston   
#> 10 4         2024 Wed, Jan 3  Final       Boston    1            Minnesota
#> # ℹ 75 more rows
#> # ℹ 14 more variables: away_team_id <chr>, home_score <chr>, away_score <chr>,
#> #   winner <chr>, venue <chr>, venue_url <chr>, game_type <chr>,
#> #   game_json <lgl>, game_json_url <glue>, PBP <lgl>, team_box <lgl>,
#> #   player_box <lgl>, scoring_summary <lgl>, penalty_summary <lgl>
# }
```
