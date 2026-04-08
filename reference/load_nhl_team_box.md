# **Load fastRhockey NHL team box scores**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_team_box(
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

  The name of the team box data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_team_box(2022))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 2,800 × 16
#>    home_away team_id team_abbrev team_name      goals shots_on_goal   pim  hits
#>    <chr>       <int> <chr>       <chr>          <int>         <int> <int> <int>
#>  1 away           28 SJS         Sharks             1            31    13    22
#>  2 home           18 NSH         Predators          4            32    13    23
#>  3 away           18 NSH         Predators          3            18     8    33
#>  4 home           28 SJS         Sharks             2            33     6    23
#>  5 away           14 TBL         Lightning          1            26     8    19
#>  6 home            3 NYR         Rangers            3            39    12    24
#>  7 away           54 VGK         Golden Knights     4            51     6    25
#>  8 home           26 LAK         Kings              3            30    10    23
#>  9 away            6 BOS         Bruins             5            30     8    32
#> 10 home           15 WSH         Capitals           2            35     6    48
#> # ℹ 2,790 more rows
#> # ℹ 8 more variables: blocked_shots <int>, giveaways <int>, takeaways <int>,
#> #   power_play_goals <int>, faceoff_win_pctg <dbl>, saves <int>,
#> #   save_pctg <dbl>, goals_against <int>
# }
```
