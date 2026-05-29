# **Load fastRhockey PWHL goalie box scores**

Helper that loads multiple seasons of pre-scraped PWHL goalie-only box
score data from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_goalie_boxscores`. File naming convention:
`goalie_box_{end_year}.rds`.

## Usage

``` r
load_pwhl_goalie_box(
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

  The name of the goalie box data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                  |           |                                      |
|------------------|-----------|--------------------------------------|
| col_name         | types     | description                          |
| player_id        | character | Unique player identifier.            |
| first_name       | character | Goalie first name.                   |
| last_name        | character | Goalie last name.                    |
| position         | character | Player position.                     |
| team_id          | integer   | Unique team identifier.              |
| game_id          | integer   | Unique game identifier.              |
| league           | character | League code.                         |
| toi              | character | Time on ice (MM:SS).                 |
| time_on_ice      | numeric   | Time on ice in seconds.              |
| saves            | integer   | Saves made.                          |
| goals_against    | integer   | Goals against.                       |
| shots_against    | integer   | Shots faced.                         |
| goals            | integer   | Goals scored.                        |
| assists          | integer   | Assists.                             |
| points           | integer   | Total points (goals + assists).      |
| penalty_minutes  | integer   | Penalty minutes.                     |
| faceoff_attempts | integer   | Faceoff attempts.                    |
| faceoff_wins     | integer   | Faceoff wins.                        |
| faceoff_losses   | integer   | Faceoff losses.                      |
| faceoff_pct      | logical   | Faceoff win percentage.              |
| starting         | integer   | Whether the goalie started the game. |

## Examples

``` r
# \donttest{
  try(load_pwhl_goalie_box(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 340 × 21
#>    player_id first_name last_name  position team_id game_id league toi  
#>    <chr>     <chr>      <chr>      <chr>      <int>   <int> <chr>  <chr>
#>  1 70        Erica      Howe       G              6       2 pwhl   0    
#>  2 64        Kristen    Campbell   G              6       2 pwhl   59:59
#>  3 155       Corinne    Schroeder  G              4       2 pwhl   59:59
#>  4 41        Abbey      Levy       G              4       2 pwhl   0    
#>  5 48        Sandra     Abstreiter G              5       3 pwhl   0    
#>  6 59        Emerance   Maschmeyer G              5       3 pwhl   61:04
#>  7 85        Elaine     Chuli      G              3       3 pwhl   0    
#>  8 28        Ann-Renée  Desbiens   G              3       3 pwhl   61:04
#>  9 19        Emma       Söderberg  G              1       4 pwhl   0    
#> 10 6         Aerin      Frankel    G              1       4 pwhl   60:00
#> # ℹ 330 more rows
#> # ℹ 13 more variables: time_on_ice <dbl>, saves <int>, goals_against <int>,
#> #   shots_against <int>, goals <int>, assists <int>, points <int>,
#> #   penalty_minutes <int>, faceoff_attempts <int>, faceoff_wins <int>,
#> #   faceoff_losses <int>, faceoff_pct <lgl>, starting <int>
# }
```
