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
#> ── NHL Player Boxscores from fastRhockey data repository ── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:02:07 UTC
#> # A tibble: 56,020 × 34
#>    home_away team_id team_abbrev player_id player_name  sweater_number position
#>    <chr>       <int> <chr>           <int> <chr>                 <int> <chr>   
#>  1 away            5 PIT           8478542 E. Rodrigues              9 C       
#>  2 away            5 PIT           8482055 D. O'Connor              10 L       
#>  3 away            5 PIT           8470619 B. Boyle                 11 C       
#>  4 away            5 PIT           8475722 J. Zucker                16 L       
#>  5 away            5 PIT           8475810 B. Rust                  17 R       
#>  6 away            5 PIT           8478043 S. Lafferty              18 C       
#>  7 away            5 PIT           8476934 B. McGinn                23 L       
#>  8 away            5 PIT           8477953 K. Kapanen               42 R       
#>  9 away            5 PIT           8478046 D. Heinen                43 L       
#> 10 away            5 PIT           8478866 D. Simon                 49 C       
#> # ℹ 56,010 more rows
#> # ℹ 27 more variables: goals <int>, assists <int>, points <int>,
#> #   plus_minus <int>, pim <int>, hits <int>, power_play_goals <int>,
#> #   shots_on_goal <int>, faceoff_winning_pctg <dbl>, toi <chr>,
#> #   blocked_shots <int>, shifts <int>, giveaways <int>, takeaways <int>,
#> #   even_strength_shots_against <chr>, power_play_shots_against <chr>,
#> #   shorthanded_shots_against <chr>, save_shots_against <chr>, …
# }
```
