# **Load fastRhockey NHL player box scores**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_player_box(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2021.

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the player box data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_player_box(2022))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 55,980 × 34
#>    home_away team_id team_abbrev player_id player_name  sweater_number position
#>    <chr>       <int> <chr>           <int> <chr>                 <int> <chr>   
#>  1 away           28 SJS           8481477 N. Sturm                  7 C       
#>  2 away           28 SJS           8479316 L. Kunin                 11 C       
#>  3 away           28 SJS           8474009 N. Bonino                13 C       
#>  4 away           28 SJS           8478904 S. Lorentz               16 C       
#>  5 away           28 SJS           8478067 O. Lindblom              23 L       
#>  6 away           28 SJS           8478414 T. Meier                 28 R       
#>  7 away           28 SJS           8474053 L. Couture               39 C       
#>  8 away           28 SJS           8479981 J. Gadjovich             42 L       
#>  9 away           28 SJS           8476881 T. Hertl                 48 C       
#> 10 away           28 SJS           8478099 K. Labanc                62 R       
#> # ℹ 55,970 more rows
#> # ℹ 27 more variables: goals <int>, assists <int>, points <int>,
#> #   plus_minus <int>, pim <int>, hits <int>, power_play_goals <int>,
#> #   shots_on_goal <int>, faceoff_winning_pctg <dbl>, toi <chr>,
#> #   blocked_shots <int>, shifts <int>, giveaways <int>, takeaways <int>,
#> #   even_strength_shots_against <chr>, power_play_shots_against <chr>,
#> #   shorthanded_shots_against <chr>, save_shots_against <chr>, …
# }
```
