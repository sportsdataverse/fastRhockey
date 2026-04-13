# **Load fastRhockey PWHL play-by-play**

Helper that loads multiple seasons of pre-scraped PWHL play-by-play data
from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_pbp(
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
  season data into a database (used by
  [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the play-by-play data table within the database

## Value

A data frame of class `fastRhockey_data`

## Examples

``` r
# \donttest{
  try(load_pwhl_pbp(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 10,456 × 95
#>    game_id event   team_id period_of_game time_of_period player_id
#>      <int> <chr>     <int> <chr>          <chr>              <int>
#>  1       2 faceoff      NA 1              0:00                  76
#>  2       2 shot          4 1              1:29                  44
#>  3       2 shot          4 1              3:30                  36
#>  4       2 shot          4 1              3:36                  46
#>  5       2 faceoff      NA 1              4:30                  73
#>  6       2 shot          4 1              4:57                  86
#>  7       2 faceoff      NA 1              5:18                  73
#>  8       2 shot          4 1              6:42                  38
#>  9       2 shot          6 1              7:05                 128
#> 10       2 shot          6 1              9:12                 126
#> # ℹ 10,446 more rows
#> # ℹ 89 more variables: player_name_first <chr>, player_name_last <chr>,
#> #   player_position <chr>, player_two_id <int>, player_two_name_first <chr>,
#> #   player_two_name_last <chr>, player_two_position <chr>, x_coord <dbl>,
#> #   y_coord <dbl>, home_win <int>, player_team_id <int>, event_type <chr>,
#> #   shot_quality <chr>, goal <lgl>, goalie_id <int>, goalie_first <chr>,
#> #   goalie_last <chr>, player_three_id <int>, player_three_name_first <chr>, …
# }
```
