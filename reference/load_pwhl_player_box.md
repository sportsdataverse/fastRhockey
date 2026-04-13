# **Load fastRhockey PWHL player box scores**

Helper that loads multiple seasons of pre-scraped PWHL player box score
data from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_player_box(
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

  The name of the player box data table within the database

## Value

A data frame of class `fastRhockey_data`

## Examples

``` r
# \donttest{
  try(load_pwhl_player_box(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 3,545 × 26
#>    player_id first_name last_name position team_id game_id league toi  
#>    <chr>     <chr>      <chr>     <chr>      <int>   <int> <chr>  <chr>
#>  1 71        Jocelyne   Larocque  LD             6       2 pwhl   26:41
#>  2 133       Lauriane   Rougeau   LD             6       2 pwhl   12:03
#>  3 68        Kali       Flanagan  RD             6       2 pwhl   21:39
#>  4 131       Olivia     Knowles   RD             6       2 pwhl   9:41 
#>  5 101       Alexa      Vasko     C              6       2 pwhl   10:30
#>  6 74        Allie      Munroe    LD             6       2 pwhl   12:31
#>  7 67        Renata     Fast      RD             6       2 pwhl   24:49
#>  8 126       Samantha   Cogan     RW             6       2 pwhl   14:21
#>  9 65        Jesse      Compher   C              6       2 pwhl   14:55
#> 10 72        Rebecca    Leslie    RW             6       2 pwhl   10:48
#> # ℹ 3,535 more rows
#> # ℹ 18 more variables: time_on_ice <dbl>, goals <int>, assists <int>,
#> #   points <int>, shots <int>, hits <int>, blocked_shots <int>,
#> #   penalty_minutes <int>, plus_minus <int>, faceoff_attempts <int>,
#> #   faceoff_wins <int>, faceoff_losses <int>, faceoff_pct <dbl>,
#> #   starting <chr>, player_type <chr>, saves <int>, goals_against <int>,
#> #   shots_against <int>
# }
```
