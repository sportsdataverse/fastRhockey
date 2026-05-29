# **Load fastRhockey PWHL three stars**

Helper that loads multiple seasons of PWHL three-stars-of-the- game data
(one to three rows per game) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_three_stars`. File naming convention:
`three_stars_{end_year}.rds`.

## Usage

``` r
load_pwhl_three_stars(
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

  The name of the three stars data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|               |           |                             |
|---------------|-----------|-----------------------------|
| col_name      | types     | description                 |
| game_id       | integer   | Unique game identifier.     |
| star          | integer   | Star rank (1-3).            |
| team_id       | integer   | Star's team identifier.     |
| team          | character | Star's team name.           |
| team_abbr     | character | Star's team abbreviation.   |
| player_id     | integer   | Star's player identifier.   |
| first_name    | character | Star's first name.          |
| last_name     | character | Star's last name.           |
| jersey_number | integer   | Jersey number.              |
| position      | character | Player position.            |
| is_goalie     | integer   | Goalie flag.                |
| is_home       | integer   | Home-team flag.             |
| goals         | integer   | Goals scored in this game.  |
| assists       | integer   | Assists in this game.       |
| points        | integer   | Points in this game.        |
| shots         | integer   | Shots on goal in this game. |
| saves         | integer   | Saves made (goalies).       |
| shots_against | integer   | Shots faced (goalies).      |
| goals_against | integer   | Goals against (goalies).    |
| time_on_ice   | character | Time on ice (MM:SS).        |

## Examples

``` r
# \donttest{
  try(load_pwhl_three_stars(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 255 × 20
#>    game_id  star team_id team           team_abbr player_id first_name last_name
#>      <int> <int>   <int> <chr>          <chr>         <int> <chr>      <chr>    
#>  1       2     1       4 PWHL New York  NY              155 Corinne    Schroeder
#>  2       2     2       4 PWHL New York  NY               34 Alex       Carpenter
#>  3       2     3       4 PWHL New York  NY               44 Ella       Shelton  
#>  4       3     1       3 PWHL Montreal  MTL              80 Ann-Sophie Bettez   
#>  5       3     2       3 PWHL Montreal  MTL              28 Ann-Renée  Desbiens 
#>  6       3     3       5 PWHL Ottawa    OTT              61 Hayley     Scamurra 
#>  7       4     1       2 PWHL Minnesota MIN              22 Nicole     Hensley  
#>  8       4     2       2 PWHL Minnesota MIN              21 Taylor     Heise    
#>  9       4     3       1 PWHL Boston    BOS              17 Theresa    Schafzahl
#> 10       5     1       6 PWHL Toronto   TOR              64 Kristen    Campbell 
#> # ℹ 245 more rows
#> # ℹ 12 more variables: jersey_number <int>, position <chr>, is_goalie <int>,
#> #   is_home <int>, goals <int>, assists <int>, points <int>, shots <int>,
#> #   saves <int>, shots_against <int>, goals_against <int>, time_on_ice <chr>
# }
```
